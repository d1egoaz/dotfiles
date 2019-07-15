;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;;
(if (eq system-type 'darwin)
    (setq
     doom-font (font-spec :family "SF Mono" :size 14)
     ;; doom-variable-pitch-font (font-spec :family "Noto Sans" :size 14)
     projectile-project-search-path '("~/src/github.com")
     ))
(if (eq system-type 'gnu/linux)
    (setq
     doom-font (font-spec :family "SF Mono" :size 30)
     doom-variable-pitch-font (font-spec :family "Noto Sans" :size 30)
     projectile-project-search-path '("~/code")
     ))

(setq
 doom-localleader-key ","
 ;;display-line-numbers-type 'relative
 which-key-idle-delay 0.3
 ;; stay on the original character when leaving insert mode
 evil-move-cursor-back nil
 evil-shift-round nil

 evil-insert-state-cursor '((bar . 2) "#ff00ff")
 evil-normal-state-cursor '(box "#ff00ff")
 ;; disable .#file.ext creation
 create-lockfiles nil

 ;; deft
 deft-directory "~/gdrive/deft"
 deft-use-filename-as-title t
 deft-use-filename-as-title t

 ;; Scroll compilation output to first error
 compilation-scroll-output 'first-error

 ;; backups
 backup-directory-alist '((".*" . "~/emacs_backups/per-save"))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 backup-by-copying t
 version-control t
 delete-old-versions t
 delete-by-moving-to-trash t
 kept-new-versions 6
 kept-old-versions 2
 vc-make-backup-files t
 auto-save-default t
 auto-save-timeout 20
 auto-save-interval 200
 make-backup-files t ;; <- DISABLED

 ;; avy
 avy-all-windows 'all-frames
 avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?e ?i ?r ?u ?q ?p)
 ;; used for avy-goto-char-timer
 avy-timeout-seconds 0.3

 ;; magit hunk highlight whitespace, https://github.com/magit/magit/issues/1689
 smerge-refine-ignore-whitespace nil

 ea-paste nil
 git-link-open-in-browser t
 )

(set-foreground-color "#b2b2b2")
(set-background-color "#1e1e1e")

;; Disable arrows keys in evil mode
(define-key evil-insert-state-map [left] 'undefined)
(define-key evil-insert-state-map [right] 'undefined)
(define-key evil-insert-state-map [up] 'undefined)
(define-key evil-insert-state-map [down] 'undefined)
(define-key evil-motion-state-map [left] 'undefined)
(define-key evil-motion-state-map [right] 'undefined)
(define-key evil-motion-state-map [up] 'undefined)
(define-key evil-motion-state-map [down] 'undefined)

;; disable mouse
(defun nothing())
(define-key evil-normal-state-map (kbd "<down-mouse-1>") 'nothing)
(dolist (mouse '("<mouse-1>" "<mouse-2>" "<mouse-3>"
                 "<up-mouse-1>" "<up-mouse-2>" "<up-mouse-3>"
                 "<down-mouse-1>" "<down-mouse-2>" "<down-mouse-3>"
                 "<drag-mouse-1>" "<drag-mouse-2>" "<drag-mouse-3>"
                 "<mouse-4>" "<mouse-5>"
                 "<up-mouse-4>" "<up-mouse-5>"
                 "<down-mouse-4>" "<down-mouse-5>"
                 "<drag-mouse-4>" "<drag-mouse-5>"
                 "<wheel-up>" "<wheel-down>" "<wheel-left>" "<wheel-right>"))
  (global-unset-key (kbd mouse)))

(fset 'evil-visual-update-x-selection 'ignore)

;; ************** ORG-MODE **************
(after! org
  ;; fixes M-RET to create or elements
  (org-defkey org-mode-map [(meta return)] 'org-meta-return)
  (if (eq system-type 'darwin)
      (setq org-download-screenshot-method "screencapture -i %s"))
  (if (eq system-type 'gnu/linux)
      (setq org-download-screenshot-method "import  %s"))
  (setq
   org-directory "~/gdrive/deft"
   org-agenda-files (list "~/gdrive/deft/gtd-inbox.org" "~/gdrive/deft/gtd-personal.org" "~/gdrive/deft/gtd-work.org" )
   org-agenda-span 16
   org-agenda-start-day "-3d"
   org-blank-before-new-entry
   '((heading . always)
     (plain-list-item . nil))
   org-confirm-babel-evaluate nil
   org-default-notes-file "~/gdrive/deft/notes.org"
   org-download-heading-lvl nil
   org-download-image-dir "~/gdrive/deft/images"
   org-download-method 'directory
   org-log-into-drawer t
   org-refile-targets '(("~/gdrive/deft/gtd-inbox.org" :maxlevel . 1)
                        ("~/gdrive/deft/gtd-personal.org" :level . 1)
                        ("~/gdrive/deft/gtd-work.org" :maxlevel . 2))
   org-src-fontify-natively t
   org-startup-with-inline-images t
   org-todo-keywords '((sequence "TODO(t!)" "WAITING(w!)" "|" "DONE(d!)" "CANCELLED(c!)"))
   ;; ! is to log event on logbook drawer
   org-capture-templates
   '(
     ;; example:
     ;;   "t"                               = key
     ;;   "Todo"                            = description
     ;;   entry                             = type
     ;;   (file+headline "file" "tasks")    = target
     ;;   ""                                = template
     ;;   :prepend t                        = properties
     ;; https://orgmode.org/manual/Template-expansion.html
     ("t" "Todo" entry (file+headline "~/gdrive/deft/gtd-inbox.org" "Inbox")
      "* TODO %?\nCreated on on %U\n" :prepend t :empty-lines 1)
     ("l" "Link" entry (file* Links+headline "~/gdrive/deft/notes.org" "Links")
      "* %? %^L %^g \n%T" :prepend t)
     ("n" "Note" entry (file+headline "~/gdrive/deft/notes.org" "Notes")
      "* %^{title}%^g\n%T\n\n%?" :prepend t)
     ("j" "Journal" entry (file+datetree "~/gdrive/deft/journal.org")
      "* %?" :clock-in t :clock-resume t))
   )

  (add-hook! 'org-mode-hook #'visual-line-mode)) ;; http://superuser.com/questions/299886/linewrap-in-org-mode-of-emacs

;; loads ENV variables
(setq
 exec-path-from-shell-check-startup-files nil
 exec-path-from-shell-variables '("PATH" "GOPATH"))
(exec-path-from-shell-initialize)

;; whitespace mode
(global-whitespace-mode) ;; Toggle whitespace visualization globally.

;;(golden-ratio-mode)

(defun github-conversation-p (window-title)
  (or (string-match-p "Pull Request" window-title)
      (string-match-p "Issue" window-title)
      ;; ...
      ))

(defun popup-handler (app-name window-title x y w h)
  (unless (zerop w)
    (set-frame-size (selected-frame) 1000 500 t))
  ;; set major mode
  (cond
   ((github-conversation-p window-title) (gfm-mode))
   ;; ...
   (t (markdown-mode)) ; default major mode
   ))
(add-hook! 'ea-popup-hook 'popup-handler)

(add-hook! 'before-save-hook #'whitespace-cleanup)

(after! go-mode
  (add-hook! 'go-mode-hook
    (add-hook! 'before-save-hook 'gofmt-before-save))
  ;; (setq godoc-at-point-function 'godoc-gogetdoc)
  ;; (setq godoc-and-godef-command "gogetdoc")
  (setq
   godoc-and-godef-command "go doc"
   gofmt-command "goimports")
  (setq-default flycheck-disabled-checkers '(go-build go-errcheck)))

  ;; (set-lookup-handlers! 'go-mode
  ;;   :definition #'godef-jump
  ;;   :references #'go-guru-referrers
  ;;   :documentation #'godoc-at-point))

(setq highlight-thing-limit-to-region-in-large-buffers-p t)
(add-hook! 'prog-mode-hook 'highlight-thing-mode)
(add-hook! 'conf-mode 'highlight-thing-mode)
(add-hook! 'yaml-mode 'highlight-thing-mode)

(load! "+funcs")
(load! "+bindings")
(message ">>> done loading init file <<<")
