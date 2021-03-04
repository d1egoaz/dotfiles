;;; ~/dotfiles/archlinux/home/diego/.config/doom/+funcs.el -*- lexical-binding: t; -*-

(require 'transient)
(require 'goto-addr)

(defun diego/delete-last-character-end-of-line ()
  "Delete last character in line"
  (interactive)
  (save-excursion
    (move-end-of-line 1)
    (delete-backward-char 1)))

(defun diego/disapproval () (interactive) (insert "ಠ_ಠ"))
(defun diego/shrug () (interactive) (insert "¯\\_(ツ)_/¯"))
(defun diego/table-flip () (interactive) (insert "(╯°□°）╯︵ ┻━┻"))
(defun diego/glasses () (interactive) (insert "(⌐■_■)"))
(defun diego/idonteven () (interactive) (insert "¯\(°_°)/¯"))

(defun diego/copy-buffer-name ()
  "copy buffer name"
  (interactive)
  (let ((path (buffer-name)))
    (message path
    (kill-new path))))

(defun diego/copy-buffer-dir-path ()
  "copy buffer name"
  (interactive)
  (let ((path (file-name-directory (buffer-file-name))))
    (message path)
    (kill-new path)))

(defun diego/url-to-markdown-image ()
  "copy url from clipboard and creates an url src image to paste in a markdown document"
  (interactive)
  (kill-new
   (format "<img src=\"%s\" width=\"50%%\" />" (current-kill 0))))

(defun diego/today-UTC-date ()
  "copy the full UTC time to clipboard"
  (interactive)
  "Inserts the current date in the buffer"
  ;; nil to use current date, t to use UTC
  (insert (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))

(defun diego/now ()
  "Inserts the current time in the buffer"
  (interactive)
  (insert (format-time-string "%H:%M:%S PT")))

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

(defun diego/fetch-and-rebase-onto-origin-master ()
  (interactive)
  (magit-fetch-branch "origin" "master" nil)
  (magit-git-rebase "origin/master" nil))

(defun diego/fetch-origin-master ()
  (interactive)
  (magit-fetch-branch "origin" "master" nil))

(defun diego/git-create-branch-from-origin-master ()
  "Creates a new branch starting from origin/master."
  (interactive)
  (diego/fetch-origin-master)
  (let ((new_branch_name (read-from-minibuffer "New branch name (from origin/master): " "diego_")))
    (magit-git-command-topdir
     (concat "git checkout -b " new_branch_name " origin/master"))))

(defun diego/review-pr (url)
  "creates a new worspace with a the PR to get reviewed"
  (interactive "sPR URL: ")
  (let* ((pr-alist (github-review-pr-from-url url))
         (repo (alist-get 'repo pr-alist))
         (num (alist-get 'num pr-alist))
         (dir (concat "~/src/github.com/Shopify/" repo "/")))
    ;; (+workspace-switch (concat (alist-get 'repo pr-alist) "-" (alist-get 'num pr-alist)) t)
    (+workspace-switch repo t)
    ;; (counsel-projectile-switch-project-by-name repo)
    (counsel-find-file (concat dir "README.md"))
    ;; (doom-project-browse dir)
    ;; (diego/fetch-origin-master);; needed to get the diff list
    (shell-command (concat "cd " dir "; gh pr checkout " num))
    ;;git diff --name-only remotes/origin/master..origin/new_business_platform_topic --diff-filter=d                                                                                                                                          <<<

;; (forge-checkout-pullreq (string-to-number (alist-get 'num pr-alist)))))
    ))

(defun diego/prettify-jsonv2 ()
  "prettify json current region"
  (interactive)
  (diego--exec-command-replace-region "prettier --parser json"))

(defun diego/prettify-markdown ()
  "prettify markdown current region"
  (interactive)
  (diego--exec-command-replace-region "prettier --parser markdown"))

(defun diego/prettify-yaml ()
  "prettify yaml current region"
  (interactive)
  (diego--exec-command-replace-region "prettier --parser yaml"))

(defun diego/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))

(defun diego/kill-close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;;;;;;;;;;;;;;;;;;;;;;
;; SPACEMACS FUNCS
;; shamelessly copied from Spacemacs
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

(defun diego/save-add-ispell-word ()
  (interactive)
  (let ((word (read-from-minibuffer "A word you want to add to dictionary: " (word-at-point))))
    (ispell-send-string (concat "*" word "\n"))
    (setq ispell-pdict-modified-p '(t))))

(defun diego/kubel-east-d ()
  (interactive)
  (setq
   kubel-resource "Pods"
   kubel-context "kafka-regional-us-east1-d"
   kubel-namespace "kafka-regional")
  (kubel-open))

(defun diego/magit-to-the-right (buffer)
  "Opens magit window on the right"
  (interactive)
  (delete-other-windows) ;; make space for other windows
  (split-window-right) ;; create a new window to host magit
  (evil-window-right 1) ;; move to that window
  (+magit-display-buffer-fn buffer))

(defun diego/elfeed-ivy-filter ()
  (interactive)
  (let ((filtered-tag (ivy-completing-read "Choose Tags: " (elfeed-db-get-all-tags))))
    (elfeed-search-set-filter (concat elfeed-search-filter " +" filtered-tag))
    (goto-char (point-min))))

(defun diego/make-new-scratch-buffer ()
  "New temporary scratch buffer with a random name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-"))
  (org-mode))

(defun diego/refill-paragraphs ()
  "fill individual paragraphs with large fill column"
  (interactive)
  (let ((fill-column 100000))
    (fill-individual-paragraphs (region-beginning) (region-end))))

(defun diego/rename-local-var (name)
  (interactive "sEnter new name: ")
  (let ((var (word-at-point)))
    (mark-defun)
    (replace-string var name nil (region-beginning) (region-end))))

(defun diego/eshell-here ()
  "Opens up a new shell in the directory associated with the
    current buffer's file. The eshell is renamed to match that
    directory to make multiple eshell windows easier."
  (interactive)
  (eshell "new"))

(defun diego/what-is-my-ip ()
  (interactive)
  (message "IP: %s"
           (with-current-buffer (url-retrieve-synchronously "https://ifconfig.me/ip")
             (buffer-substring (+ 1 url-http-end-of-headers) (point-max)))))


(defun diego/go-coverage-here ()
  "Show go coverage of current file."
  (interactive)
  (shell-command "go test . -coverprofile=cover.out")
  (go-coverage "cover.out"))

(defun diego/elfeed-v-mpv (url)
  "Watch a video from URL in MPV"
  ;; (async-shell-command (format "mpv %s" url)))
  (message url))

(defun diego/elfeed-view-mpv ()
  "Youtube-feed link"
  (interactive)
	(diego/elfeed-v-mpv (elfeed-entry-link (car (elfeed-search-selected))))      ;
  (unless (use-region-p) (forward-line)))

(defun diego/elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(defun diego/elfeed-open-with-eww ()
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (eww (elfeed-entry-link entry))
    (elfeed-untag entry 'unread)
    (add-hook 'eww-after-render-hook 'eww-readable nil t)
    (forward-line)))

;; based on https://github.com/eddsteel/df-emacs/blob/master/edd/edd-util.el
(defun diego/find-open-next-url ()
  (interactive)
  (search-forward-regexp goto-address-url-regexp)
  (backward-word)
  (browse-url-at-point))

    ;; (let ((rule (completing-read "style-rule "
		;; 		 '("file.size.limit" "line.size.limit")

(define-transient-command diego/elfeed-filter ()
  [["Arguments"
    ("a" "apple" "+apple")
    ("c" "Tech Crunch" "+techcrunch")
    ("e" "emacs" "+emacs")
    ("h" "Hacker News" "+hnews")
    ("l" "linux" "+linux")
    ("t" "top" "+top")
    ("s" "sre" "+sre")
    ("v" "verge" "+theverge")]
   ["Reddit"
    ("p" "r/Programming" "+programming")]
   ["Actions"
    ("f" "apply" diego/elfeed-filter-do)
    ("u" "update" elfeed-update)]])

(defun diego/elfeed-filter-do ()
  (interactive)
  (let ((tags (mapconcat 'identity (transient-args 'diego/elfeed-filter) " ")))
    (elfeed-search-set-filter (format "@2-weeks-ago +unread %s" tags))
    (goto-char (point-min))))

;; From https://www.reddit.com/r/emacs/comments/b058f8/weekly_tipstricketc_thread/eilbynr
;; https://github.com/xenodium/dotsies/blob/master/emacs/ar/ar-misc.el
(defun diego/diff-last-2-yanks ()
  "Run ediff on latest two entries in `kill-ring'."
  (interactive)
  ;; Implementation depends on `lexical-binding' being t, otherwise #'clean-up
  ;; will not be saved as closure to `ediff-cleanup-hook' and thus will lose
  ;; reference to itself.
  (let ((a (generate-new-buffer "*diff-yank a*"))
        (b (generate-new-buffer "*diff-yank b*")))
    (cl-labels ((clean-up ()
                          (kill-buffer a)
                          (kill-buffer b)
                          (remove-hook 'ediff-cleanup-hook #'clean-up)))
      (add-hook 'ediff-cleanup-hook #'clean-up)
      (with-current-buffer a
        (insert (elt kill-ring 0)))
      (with-current-buffer b
        (insert (elt kill-ring 1)))
      (ediff-buffers a b))))

(define-transient-command diego/engine-searches ()
  [
   ["Code"
    ("c g" "Github" engine/search-github)
    ("c s" "Stack Overflow" engine/search-stack-overflow)]
   ["Gifs"
    ("g t" "Tenor" engine/search-tenor)
    ("g g" "Giphy" engine/search-giphy)]
   ["Engines"
    ("o g" "Google" engine/search-google)
    ("o t" "Twitter" engine/search-twitter)
    ("o p" "Powerthesaurus" engine/search-powerthesaurus)]])

(defun diego/code-here ()
  "Opens current buffer in vs code"
  (interactive)
  (save-buffer)
  (shell-command (concat "code " (buffer-file-name))))

(defun diego/surround-org-src ()
  "surround a region in a org-mode src block"
  (interactive)
  (save-excursion
    (save-restriction ; to remove the narrowing
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (insert "#+BEGIN_SRC\n")
      (goto-char (point-max))
      (insert "\n#+END_SRC\n"))))

(defvar diego/kafka-clusters '(("kafka-regional-us-east1-b" "kafka-regional")
                     ("kafka-regional-us-central1-a" "kafka-regional")
                     ("kafka-aggregate-us-central1-a" "kafka-aggregate")
                     ("kafka-cdc-us-east1-d" "kafka-cdc")
                     ("" "")
                     ("" "")
                     ))

(defun diego/kafka-get-controller ()
  (interactive)
  (let* (
         (ctx (ivy-completing-read "Context: " diego/kafka-clusters))
         (ns (cadr (assoc ctx diego/kafka-clusters)))
         (str (format "kubectl --context %s --namespace %s exec -i kafka-0 -c kafka -- sh -c 'zookeeper-shell.sh $KAFKA_ZOOKEEPER_CONNECT get /controller' | tail -n 1"
                      ctx ns)))
    (with-output-to-temp-buffer "*kafka/get controller*"
      (print (format "%s/%s:" ctx ns))
      (print (shell-command-to-string str)))))

(defun diego/kafka-delete-controller ()
  (interactive)
  (let* (
         (ctx (ivy-completing-read "Context: " diego/kafka-clusters))
         (ns (cadr (assoc ctx diego/kafka-clusters)))
         (str (format "kubectl --context %s --namespace %s exec -i kafka-0 -c kafka -- sh -c 'zookeeper-shell.sh $KAFKA_ZOOKEEPER_CONNECT delete /controller'"
                      ctx ns)))
    (with-output-to-temp-buffer "*kafka/delete controller*"
      (print (format "%s/%s:" ctx ns))
      (when (yes-or-no-p (format "Sure to elect a new controller in %s/%s?" ctx ns))
        (print (shell-command-to-string str))))))

(defun diego/make-kafka-get-controller-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "alfredoc") (width . 80) (height . 32)
                (top . 400) (left . 300)))
  (select-frame-by-name "alfredoc")
  (switch-to-buffer "*kafka/get controller*")
  (read-only-mode -1)
  (erase-buffer)
  (local-set-key
   (kbd "C-c C-c") (lambda () (interactive)
                     (local-unset-key (kbd "C-c C-c"))
                     (delete-frame)))
  (diego/kafka-get-controller))

(add-hook 'org-capture-after-finalize-hook
          (lambda ()
            (when (equal
                   (cdr (assoc 'name (frame-parameters (selected-frame))))
                   "alfredoc")
              (delete-frame))))

(defun diego/make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "alfredoc") (width . 80) (height . 16)
                (top . 400) (left . 300)))
  (select-frame-by-name "alfredoc")
  (org-capture))
