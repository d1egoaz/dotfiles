;;; ~/dotfiles/archlinux/home/diego/.config/doom/+funcs.el -*- lexical-binding: t; -*-

;; shamelessly copied from Spacemacs

(defun diego/delete-last-character-end-of-line ()
  "Delete last character in line"
  (interactive)
  (save-excursion
    (move-end-of-line 1)
    (delete-backward-char 1)))

(defun diego/disapproval () (interactive) (insert "ಠ_ಠ"))
(defun diego/shrug () (interactive) (insert "¯\_(ツ)_/¯"))
(defun diego/table-flip () (interactive) (insert "(╯°□°）╯︵ ┻━┻"))
(defun diego/glasses () (interactive) (insert "(⌐■_■)"))
(defun diego/idonteven () (interactive) (insert "¯\(°_°)/¯"))

(defun diego/copy-buffer-name ()
  "copy buffer name"
  (interactive)
  (message (buffer-name))
  (kill-new (buffer-name)))

(defun diego/url-to-markdown-image ()
  "copy url from clipboard and creates an url src image to paste in a markdown document"
  (interactive)
  (kill-new
   (replace-regexp-in-string
    "url"
    (current-kill 0)
    "<img src=\"url\" width=\"50%\" />")))

(defun diego/iso-8601-date ()
  "copy the full UTC time to clipboard"
  (interactive)
  (kill-new (shell-command-to-string "date -n -u +'%Y-%m-%dT%H:%M:%SZ'")))

(defun diego--exec-command-replace-region (command)
  (interactive)
  (unless mark-active
    (mark-whole-buffer))
  (shell-command-on-region
   (region-beginning) (region-end)
   command
   (current-buffer) t "*diego/error-buffer*" t))

(defun diego/minify-json ()
  "minify json current region"
  (interactive)
  (diego--exec-command-replace-region "jq -ScM ."))

(defun diego/prettify-json ()
  "prettify json current region"
  (interactive)
  (diego--exec-command-replace-region "jq -SM ."))

(defun diego/resize-image ()
  (interactive)
  (let ((str (concat "convert \"" buffer-file-name "\" -geometry x300 \"" buffer-file-name "\"")))
    (message str)
    (shell-command-to-string str)))

;;;;;;;;;;;;;;;;;;;;;;
;; SPACEMACS FUNCS
;;;;;;;;;;;;;;;;;;;;;;

(defun spacemacs/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

;; http://stackoverflow.com/a/10216338/4869
(defun spacemacs/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun spacemacs/evil-insert-line-above (count)
  "Insert one or several lines above the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun spacemacs/evil-insert-line-below (count)
  "Insert one or several lines below the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(defun vterm-send-custom-return ()
  "Sends C-m to the libvterm."
  (interactive)
  (process-send-string vterm--process "\C-m"))

(defun save-ispell-word ()
  (interactive)
  (let ((word (read-from-minibuffer "A word you want to add to dictionary: " (word-at-point))))
    (ispell-send-string (concat "*" word "\n"))
    (setq ispell-pdict-modified-p '(t))))
