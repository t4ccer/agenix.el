;;; agenix.el --- Decrypt and encrypt agenix secrets  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Tomasz Maciosowski (t4ccer)

;; Author: Tomasz Maciosowski <t4ccer@gmail.com>
;; Maintainer: Tomasz Maciosowski <t4ccer@gmail.com>
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/t4ccer/agenix.el
;; Version: 1.0

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Fully transparent editing of agenix secrets. Open a file, edit it, save it and it will be
;; encrypted automatically.

;;; Code:

(defcustom agenix-age-program "age"
  "The age program."
  :group 'agenix
  :type 'string)

(defcustom agenix-key-files '("~/.ssh/id_ed25519" "~/.ssh/id_rsa")
  "List of age key files."
  :group 'agenix
  :type '(repeat (choice (string :tag "Pathname to a key file")
                         (function :tag "Function returning the pathname to a key file"))))

(defcustom agenix-pre-mode-hook nil
  "Hook to run before entering `agenix-mode'.
Can be used to set up age binary path."
  :group 'agenix
  :type 'hook)

(defvar-local agenix--encrypted-fp nil)

(defvar-local agenix--keys nil)

(defvar-local agenix--undo-list nil)

(defvar-local agenix--point nil)

(define-derived-mode agenix-mode text-mode "agenix"
  "Major mode for agenix files.
Don't use directly, use `agenix-mode-if-with-secrets-nix' to ensure that
secrets.nix exists."
  (read-only-mode 1)

  (run-hooks 'agenix-pre-mode-hook)

  (agenix-decrypt-buffer)
  (goto-char (point-min))
  (setq buffer-undo-list nil)

  (setq require-final-newline nil)
  (setq buffer-auto-save-file-name nil)
  (setq write-contents-functions '(agenix-save-decrypted))

  ;; Reverting loads encrypted file back to the buffer, so we need to decrypt it
  (add-hook 'after-revert-hook
            (lambda () (when (eq major-mode 'agenix-mode) (agenix-decrypt-buffer)))))

(defun agenix--buffer-string* (buffer)
  "Like `buffer-string' but read from BUFFER parameter."
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun agenix--with-temp-buffer (func)
  "Like `with-temp-buffer' but doesn't actually switch the buffer.
FUNC takes a temporary buffer that will be disposed after the call."
  (let* ((age-buf (generate-new-buffer "*age-buf*"))
         (res (funcall func age-buf)))
    (kill-buffer age-buf)
    res))

(defun agenix--extract-identity-path (args)
  "Extract the path of the identity file from ARGS.
Check if the file exists and is readable."
  (let ((identity-index (or (cl-position "--identity" args :test 'string=)
                            (cl-position "-i" args :test 'string=))))
    (when identity-index
      (let ((identity-path (nth (1+ identity-index) args)))
        (if (and identity-path (file-readable-p identity-path))
            identity-path
          (error "Identity file %s does not exist or is not readable" identity-path))))))

(defun agenix--process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (let ((identity-path (agenix--extract-identity-path args)))
    (agenix--with-temp-buffer
     (lambda (buf) (list (apply #'call-process program nil buf nil args)
                         (agenix--buffer-string* buf))))))

;;;###autoload
(defun agenix-decrypt-buffer (&optional encrypted-buffer)
  "Decrypt ENCRYPTED-BUFFER in place.
If ENCRYPTED-BUFFER is unset or nil, decrypt the current buffer."
  (interactive
   (when current-prefix-arg
     (list (read-buffer "Encrypted buffer: " (current-buffer) t))))

  (with-current-buffer (or encrypted-buffer (current-buffer))
    (let* ((nix-res (apply #'agenix--process-exit-code-and-output "nix-instantiate"
                           (list "--strict" "--json" "--eval" "--expr"
                                 (format
                                  "(import \"%s\").\"%s\".publicKeys"
                                  (agenix-locate-secrets-nix buffer-file-name)
                                  (agenix-path-relative-to-secrets-nix (buffer-file-name))))))
           (nix-exit-code (car nix-res))
           (nix-output (car (cdr nix-res))))

      (if (/= nix-exit-code 0)
          (warn "Nix evaluation error.
Probably file %s is not declared as a secret in 'secrets.nix' file.
Error: %s" (buffer-file-name) nix-output)
        (let* ((keys (json-parse-string nix-output :array-type 'list))
               (age-flags (list "--decrypt")))

          ;; Add all user's keys to the age command
          (dolist (keyspec agenix-key-files)
            (let ((key-path (cond ((stringp keyspec) keyspec)
                                  (t (funcall keyspec)))))
              (when (and key-path (file-exists-p (expand-file-name key-path)))
                (setq age-flags
                      (nconc age-flags (list "--identity" (expand-file-name key-path)))))))

          ;; Add file-path to decrypt to the age command
          (setq age-flags (nconc age-flags (list (buffer-file-name))))
          (setq agenix--encrypted-fp (buffer-file-name))
          (setq agenix--keys keys)

          ;; Check if file already exists
          (if (not (file-exists-p (buffer-file-name)))
              (progn
                (message "Not decrypting. File %s does not exist and will be created when you \
will save this buffer." (buffer-file-name))
                (read-only-mode -1))
            (let*
                ((age-res
                  (apply #'agenix--process-exit-code-and-output agenix-age-program age-flags))
                 (age-exit-code (car age-res))
                 (age-output (car (cdr age-res))))

              (if (= 0 age-exit-code)
                  (progn
                    ;; Replace buffer with decrypted content
                    (read-only-mode -1)
                    (erase-buffer)
                    (insert age-output)

                    ;; Mark buffer as not modified
                    (set-buffer-modified-p nil)
                    (setq buffer-undo-list agenix--undo-list))
                (error age-output))))
          (when agenix--point
            (goto-char agenix--point)))))))

;;;###autoload
(defun agenix-save-decrypted (&optional unencrypted-buffer)
  "Encrypt UNENCRYPTED-BUFFER back to the original .age file.
If UNENCRYPTED-BUFFER is unset or nil, use the current buffer."
  (interactive
   (when current-prefix-arg
     (list (read-buffer "Unencrypted buffer: " (current-buffer) t))))
  (with-current-buffer (or unencrypted-buffer (current-buffer))
    (let* ((age-flags (list "--encrypt")))
      (progn
        (dolist (k agenix--keys)
          (setq age-flags (nconc age-flags (list "--recipient" k))))
        (setq age-flags (nconc age-flags (list "-o" agenix--encrypted-fp)))
        (let* ((decrypted-text (buffer-string))
               (age-res
                (agenix--with-temp-buffer
                 (lambda (buf)
                   (list
                    (apply #'call-process-region
                           decrypted-text nil
                           agenix-age-program
                           nil
                           buf
                           t
                           age-flags)
                    (agenix--buffer-string* buf))))))
          (when (/= 0 (car age-res))
            (error (car (cdr age-res))))
          (setq agenix--point (point))
          (setq agenix--undo-list buffer-undo-list)
          (revert-buffer :ignore-auto :noconfirm :preserve-modes)
          (set-buffer-modified-p nil)
          t)))))

(defun agenix-secrets-base-dir (pathname)
  "Return the directory above PATHNAME containing secrets.nix file, if one exists."
  (locate-dominating-file pathname "secrets.nix"))

(defun agenix-locate-secrets-nix (pathname)
  "Return the absolute path to secrets.nix in any directory containing PATHNAME."
  (when-let (dir (agenix-secrets-base-dir pathname))
    (expand-file-name "secrets.nix" dir)))

(defun agenix-path-relative-to-secrets-nix (pathname)
  "Convert absolute PATHNAME to a name relative to its secrets.nix."
  (when-let (dir (agenix-secrets-base-dir pathname))
    (file-relative-name pathname dir)))

;;;###autoload
(defun agenix-mode-if-with-secrets-nix ()
  "Enable `agenix-mode' if the current buffer is in a directory with secrets.nix."
  (interactive)
  (when (agenix-locate-secrets-nix buffer-file-name)
    (agenix-mode)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.age\\'" . agenix-mode-if-with-secrets-nix))

(provide 'agenix)
;;; agenix.el ends here
