;;; agenix.el --- Decrypt and encrypt agenix secrets  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Tomasz Maciosowski (t4ccer)

;; Author: Tomasz Maciosowski <t4ccer@gmail.com>
;; Maintainer: Tomasz Maciosowski <t4ccer@gmail.com>
;; Package-Requires: ((emacs "25.1"))
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

;; Fully transaprent editing of agenix secrets. Open a file, edit it, save it and it will be
;; encrypted automatically.

;;; Code:

(defcustom agenix-age-program "age"
  "The age program."
  :group 'agenix
  :type 'string)

(defcustom agenix-jq-program "jq"
  "The jq program."
  :group 'agenix
  :type 'string)

(defcustom agenix-key-files '("~/.ssh/id_ed25519" "~/.ssh/id_rsa")
  "List of age key files."
  :group 'agenix
  :type '(repeat string))

(defvar-local agenix--encrypted-fp nil)

(defvar-local agenix--keys nil)

(defvar-local agenix--undo-list nil)

(define-derived-mode agenix-mode text-mode "agenix"
  "Major mode for agenix files.
Don't use directly, use `agenix-mode-if-with-secrtes-nix' to ensure that
secres.nix exists."
  (read-only-mode 1)

  (agenix-decrypt-buffer)
  (goto-char (point-min))
  (setq buffer-undo-list nil)

  (setq require-final-newline nil)
  (setq buffer-auto-save-file-name nil)
  (setq write-contents-functions '(agenix-save-decrypted))

  ;; Reverting loads encrypted file back to the buffer, so we need to decrypt it
  (add-hook 'after-revert-hook 'agenix-decrypt-buffer))

(defun agenix--process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

;;;###autoload
(defun agenix-decrypt-buffer (&optional encrypted-buffer)
  "Decrypt ENCRYPTED-BUFFER in place.
If ENCRYPTED-BUFFER is unset or nil, decrypt the current buffer."
  (interactive
   (when current-prefix-arg
     (list (read-buffer "Encrypted buffer: " (current-buffer) t))))

  (with-current-buffer (or encrypted-buffer (current-buffer))
    (let* ((encrypted-fp (buffer-file-name))
           (raw-keys
            (shell-command-to-string
             (concat "nix-instantiate --strict --json --eval --expr "
                     "'(import ./secrets.nix).\""
                     (file-name-nondirectory encrypted-fp)
                     "\".publicKeys' | "
                     agenix-jq-program
                     " -r '.[]'")))
           (keys (butlast (split-string raw-keys "\n")))
           (age-flags (list "--decrypt")))

      ;; Add all user's keys to the age command
      (dolist (key-path agenix-key-files)
        (when (file-exists-p (expand-file-name key-path))
          (setq age-flags
                (nconc age-flags (list "--identity" (expand-file-name key-path))))))

      ;; Add filepath to decrypt to the age command
      (setq age-flags (nconc age-flags (list encrypted-fp)))
      (setq agenix--encrypted-fp encrypted-fp)
      (setq agenix--keys keys)

      ;; Check if file already exists
      (if (not (file-exists-p (buffer-file-name)))
          (progn
            (message "Not decrypting. File %s does not exist and will be created when you will \
save this buffer." (buffer-file-name))
            (read-only-mode -1))
        (progn
          ;; Call `age`, decrypt buffer and replace the content
          (let*
              ((age-res (apply 'agenix--process-exit-code-and-output agenix-age-program age-flags))
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
              (error age-output))))))))

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
                (with-temp-buffer
                  (list
                   (apply 'call-process-region
                          decrypted-text
                          nil
                          agenix-age-program
                          nil
                          (current-buffer)
                          t
                          age-flags)
                   (buffer-string)))))
          (when (/= 0 (car age-res))
            (error (car (cdr age-res))))
          (setq agenix--undo-list buffer-undo-list)
          (revert-buffer :ignore-auto :noconfirm :preserve-modes)
          (set-buffer-modified-p nil)
          t)))))

;;;###autoload
(defun agenix-mode-if-with-secrtes-nix ()
  "Enable `agenix-mode' if the current buffer is in a directory with secrets.nix."
  (interactive)
  (when (file-exists-p "secrets.nix")
    (agenix-mode)))

(add-to-list 'auto-mode-alist '("\\.age" . agenix-mode-if-with-secrtes-nix))

(provide 'agenix)
;;; agenix.el ends here
