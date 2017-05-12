
(defun diego/delete-last-character-end-of-line ()
  "Delete last character in line"
  (interactive)
  (save-excursion
    (move-end-of-line 1)
    (delete-backward-char 1)))

(with-eval-after-load 'hydra
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))

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
  (let* ((result  (re-search-forward "\\([a-zA-Z]+\\)-\\([0-9]+\\).*$" nil t))
          (s (concat (match-string 1) "-" (match-string 2))))
    (goto-char (point-min))
    (if (and result
              (not (string-match (concat "\\[" s "\\]") (buffer-string))))
        (insert (concat "[" (upcase s) "] "))
      ;; enable to insert another string when branch doesn't match a Jira ticket
      ;; (unless (string-match (concat "\\[.*\\]") (buffer-string))
      ;;   (insert (concat "my commit message")))
      )))
