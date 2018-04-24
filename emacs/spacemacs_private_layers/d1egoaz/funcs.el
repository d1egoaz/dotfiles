
(defun diego/delete-last-character-end-of-line ()
  "Delete last character in line"
  (interactive)
  (save-excursion
    (move-end-of-line 1)
    (delete-backward-char 1)))

(with-eval-after-load 'hydra
  (defhydra diego/hydra-dumb-jump (:color pink)
    "Dumb Jump"
    ("g" dumb-jump-go "Go")
    ("b" dumb-jump-back "Back")
    ("f" dumb-jump-quick-look "Look")
    ("e" dumb-jump-go-prefer-external "External")
    ("o" dumb-jump-go-other-window "Other window")
    ("q" nil "Quit" :color blue)))

(defun diego/insert-ticket-prefix ()
  (interactive)
  "Inserts a prefix containing the number of the Jira ticket from the branch name"
  ;; (let* ((result  (re-search-forward "\\(pub\\|plat\\)-\\([0-9]+\\).*$" nil t))
  (let* ((result (re-search-forward "\\([a-zA-Z]+\\)-\\([0-9]+\\)\\(.*\\)$" nil t))
         (ticket (concat (upcase (concat (match-string 1) "-" (match-string 2)))
                         " -"
                         (capitalize (replace-regexp-in-string "_"
                                                               " "
                                                               (concat (match-string 3) ""))))))
    (goto-char (point-min))
    (if (and result
             (not (string-match (concat "\\[" ticket "\\]") (buffer-string))))
        (insert ticket)
      ;; enable to add a custom message when branch name it doesn't match a Jira ticket
      ;; (unless (string-match (concat "\\[.*\\]") (buffer-string))
      ;;   (insert (concat "my custom message")))
      )))

(defun diego/disapproval () (interactive) (insert "ಠ_ಠ"))
(defun diego/shrug () (interactive) (insert "¯\_(ツ)_/¯"))
(defun diego/table-flip () (interactive) (insert "(╯°□°）╯︵ ┻━┻"))
(defun diego/glasses () (interactive) (insert "(⌐■_■)"))
(defun diego/idonteven () (interactive) (insert "¯\(°_°)/¯"))

(defun diego/to-snake-case ()
  (interactive)
  (progn
    (replace-regexp
     "\\([A-Z]\\)" "_\\1" nil (region-beginning) (region-end))
    (downcase-region
     (region-beginning)
     (region-end))))
