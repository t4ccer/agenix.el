;;; agenix.el --- Decrypt and encrypt agenix secrets inside Emacs. -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tomasz Maciosowski (t4ccer)

;; Author: Tomasz Maciosowski <t4ccer@gmail.com>
;; Maintainer: Tomasz Maciosowski <t4ccer@gmail.com>
;; Version: 0.1

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

;;; Code:

(define-derived-mode agenix-encrypted-mode text-mode "Age[encrypted]"
  "Major mode for encrypted age files."
  (read-only-mode))

(define-derived-mode agenix-decrypted-mode text-mode "Age[decrypted]"
  "Major mode for decrypted age files.")

(defun process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer 
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun agenix-decrypt-this-buffer ()
  "Decrypt current buffer in a new buffer"
  (interactive)
  (let* ((new-name (concat "*agenix[" (buffer-name) "]*"))
         (raw-keys (shell-command-to-string "((nix-instantiate --eval -E \"(let rules = import ./secrets.nix; in builtins.concatStringsSep \\\"\n\\\" rules.\\\".netrc.age\\\".publicKeys)\" | sed 's/\"//g' | sed 's/\\\\n/\\n/g') | sed '/^$/d' || exit 1)"))
         (keys (seq-filter (lambda (s) (not (string= s ""))) (s-split "\n" raw-keys)))
         (encrypted-fp (buffer-file-name))
         (encypted-buf (current-buffer))
         (age-flags (list "--decrypt")))

    (when (file-exists-p "~/.ssh/id_ed25519")
      (setq age-flags (nconc age-flags (list "--identity" (expand-file-name "~/.ssh/id_ed25519")))))
    
    (when (file-exists-p "~/.ssh/id_rsa")
      (setq age-flags (nconc age-flags (list "--identity" (expand-file-name "~/.ssh/id_rsa")))))
    
    (setq age-flags (nconc age-flags (list encrypted-fp)))
    
    (let* ((age-res (apply 'process-exit-code-and-output "age" age-flags)))
      (if (= 0 (car age-res))
          (progn
            (switch-to-buffer (generate-new-buffer new-name))
            (agenix-decrypted-mode)
            (setq buffer-auto-save-file-name nil)
            (insert (car (cdr age-res)))
            (set (make-local-variable 'agenix-encyrpted-fp) encrypted-fp)
            (set (make-local-variable 'agenix-keys) keys)
            (set (make-local-variable 'agenix-encrypted-buf) encypted-buf)
            (set (make-local-variable 'agenix-init) (buffer-string))
            (goto-char (point-min)))
        (error (car (cdr age-res)))))))

(defun agenix-encrypt-this-buffer ()
  "Decrypt current buffer in a new buffer"
  (interactive)
  (let* ((age-flags (list "--encrypt")))
    (if (string= (buffer-string) agenix-init)
        (progn
          (kill-this-buffer)
          (switch-to-buffer agenix-encrypted-buf))
      (progn
        (dolist (k agenix-keys)
          (setq age-flags (nconc age-flags (list "--recipient" k))))
        (setq age-flags (nconc age-flags (list "-o" agenix-encyrpted-fp)))
        (let* ((decrypted-text (buffer-string))
               (age-res
                (with-temp-buffer
                  (list
                   (apply 'call-process-region decrypted-text nil "age" nil (current-buffer) t age-flags)
                   (buffer-string)))))
          (if (= 0 (car age-res))
              (progn
                (kill-this-buffer)
                (switch-to-buffer agenix-encrypted-buf))
            (error (car (cdr age-res)))))))))

(provide 'agenix.el)
;;; agenix.el ends here
