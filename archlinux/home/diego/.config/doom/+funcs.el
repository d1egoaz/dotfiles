;;; ~/dotfiles/archlinux/home/diego/.config/doom/+funcs.el -*- lexical-binding: t; -*-

;; shamelessly copied from Spacemacs

(defun spacemacs/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))
