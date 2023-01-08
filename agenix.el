;;; agenix.el --- Decrypt and encrypt agenix secrets  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tomasz Maciosowski (t4ccer)

;; Author: Tomasz Maciosowski <t4ccer@gmail.com>
;; Maintainer: Tomasz Maciosowski <t4ccer@gmail.com>
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/t4ccer/agenix.el
;; Version: 0.2.1

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

;; Decrypt a file

;; Inside buffer with encrypted file run =M-x agenix-decrypt-buffer=. It will open a new buffer
;; with decrypted content.

;; Encrypt a file

;; Inside buffer opened previously run =M-x agenix-encrypt-buffer=. It will encrypt content of
;; the buffer, override previously decrypted file and close a buffer.

;;; Code:

(defcustom agenix-age-program "age"
  "The age program."
  :group 'agenix
  :type 'string)

(defvar-local agenix--encrypted-fp nil)

(defvar-local agenix--keys nil)

(defvar-local agenix--encrypted-buf nil)

(defvar-local agenix--init nil)

(define-derived-mode agenix-encrypted-mode text-mode "Age[encrypted]"
  "Major mode for encrypted age files."
  (read-only-mode))

(define-derived-mode agenix-decrypted-mode text-mode "Age[decrypted]"
  "Major mode for decrypted age files.")

(defun agenix--process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(define-obsolete-function-alias 'agenix-decrypt-this-buffer 'agenix-decrypt-buffer "0.2.1"
  "Decrypt current buffer in a new buffer.")

;;;###autoload
(defun agenix-decrypt-buffer (&optional encrypted-buffer)
  "Decrypt ENCRYPTED-BUFFER in a new buffer.
If ENCRYPTED-BUFFER is unset or nil, decrypt the current buffer."
  (interactive
   (when current-prefix-arg
     (list (read-buffer "Encrypted buffer: " (current-buffer) t))))
  (with-current-buffer (or encrypted-buffer (current-buffer))
    (let* ((new-name (concat "*agenix[" (buffer-name) "]*"))
           (encrypted-fp (buffer-file-name))
           (raw-keys (shell-command-to-string
                      (concat "nix-instantiate --eval --expr "
                              "'(import ./secrets.nix).\""
                              (file-name-nondirectory encrypted-fp)
                              "\".publicKeys' | sed 's/\[ \"//; s/\" ]//'")))
           (keys (split-string raw-keys "\" \""))
           (age-flags (list "--decrypt")))

      (when (file-exists-p "~/.ssh/id_ed25519")
        (setq age-flags
              (nconc age-flags (list "--identity" (expand-file-name "~/.ssh/id_ed25519")))))

      (when (file-exists-p "~/.ssh/id_rsa")
        (setq age-flags (nconc age-flags (list "--identity" (expand-file-name "~/.ssh/id_rsa")))))

      (setq age-flags (nconc age-flags (list encrypted-fp)))

      (let* ((age-res (apply 'agenix--process-exit-code-and-output agenix-age-program age-flags)))
        (if (= 0 (car age-res))
            (progn
              (switch-to-buffer (generate-new-buffer new-name))
              (agenix-decrypted-mode)
              (setq buffer-auto-save-file-name nil)
              (insert (car (cdr age-res)))
              (setq agenix--encrypted-fp encrypted-fp)
              (setq agenix--keys keys)
              (setq agenix--encrypted-buf encrypted-buffer)
              (setq agenix--init (buffer-string))
              (goto-char (point-min)))
          (error (car (cdr age-res))))))))

(define-obsolete-function-alias 'agenix-encrypt-this-buffer 'agenix-encrypt-buffer "0.2.1"
  "Encrypt current buffer in a new buffer.")

;;;###autoload
(defun agenix-encrypt-buffer (&optional unencrypted-buffer)
  "Encrypt UNENCRYPTED-BUFFER in a new buffer.
If UNENCRYPTED-BUFFER is unset or nil, use the current buffer."
  (interactive
   (when current-prefix-arg
     (list (read-buffer "Unencrypted buffer: " (current-buffer) t))))
  (with-current-buffer (or unencrypted-buffer (current-buffer))
    (let* ((age-flags (list "--encrypt")))
      (if (string= (buffer-string) agenix--init)
          (progn
            (kill-buffer)
            (switch-to-buffer agenix--encrypted-buf))
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
            (if (= 0 (car age-res))
                (progn
                  (kill-buffer)
                  (switch-to-buffer agenix--encrypted-buf))
              (error (car (cdr age-res))))))))))

(provide 'agenix)
;;; agenix.el ends here
