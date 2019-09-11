;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; currently using emacs from: https://github.com/railwaycat/homebrew-emacsmacport

;; Place your private configuration here
;;
(if (eq system-type 'darwin)
    (setq
     doom-font (font-spec :family "Iosevka SS04" :size 16 :weight 'light)
     ))
(if (eq system-type 'gnu/linux)
    (setq
     doom-font (font-spec :family "Iosevka SS04" :size 30)
     doom-variable-pitch-font (font-spec :family "Noto Sans" :size 30)
     ))

(setq +pretty-code-iosevka-font-name "Iosevka SS04")
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(setq
 doom-themes-enable-bold nil
 doom-localleader-key ","
 display-line-numbers-type 'relative
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
 ;; avy-all-windows nil
 avy-all-windows 'all-frames
 avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?e ?i ?r ?u ?q ?p)
 ;; used for avy-goto-char-timer
 avy-timeout-seconds 0.3

 ;; magit hunk highlight whitespace, https://github.com/magit/magit/issues/1689
 smerge-refine-ignore-whitespace nil

 ea-paste nil
 git-link-open-in-browser t
 )


;; loads the theme immediately, to modify faces afterwards
(load-theme 'doom-dracula t)

(set-foreground-color "#b2b2b2")
;;(set-background-color "#1e1e1e")

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
     ("l" "Link" entry (file+headline "~/gdrive/deft/notes.org" "Links")
      "* %? %^L %^g \n%T" :prepend t)
     ("n" "Note" entry (file+headline "~/gdrive/deft/notes.org" "Notes")
      "* %^{title}%^g\n%T\n\n%?" :prepend t)
     ("j" "Journal" entry (file+olp+datetree "~/gdrive/deft/journal.org")
      "* %?" :clock-in t :clock-resume t))
   )

  (add-hook! 'org-mode-hook #'visual-line-mode)) ;; http://superuser.com/questions/299886/linewrap-in-org-mode-of-emacs
;;
;; go get golang.org/x/tools/gopls@latest
(after! go-mode
  (add-hook! 'go-mode-hook
    (add-hook! 'before-save-hook 'gofmt-before-save))
  ;; (setq godoc-at-point-function 'godoc-gogetdoc)
  ;; (setq godoc-and-godef-command "gogetdoc")
  (setq
   ;; uses go provided tools
   godef-command "go doc"
   godoc-and-godef-command "go doc"
   gofmt-command "goimports")
  ;; (setq-default flycheck-disabled-checkers '(go-build go-errcheck))
  )

;; (set-lookup-handlers! 'go-mode
;;   :definition #'godef-jump
;;   :references #'go-guru-referrers
;;   :documentation #'godoc-at-point))

(after! magit
  (setq magit-refs-show-commit-count nil
        magit-diff-refine-hunk t ;; show whitespaces changes on the selected git diff hunks
        magit-revision-show-gravatars nil
        magit-process-popup-time 0
        magit-branch-rename-push-target nil
        magit-log-arguments '("-n50" "--decorate")  ;; was: '("-n256" "--graph" "--decorate")
        magit-log-section-arguments  '("-n50" "--decorate") ;; was: ("-n256" "--decorate")
        magit-log-select-arguments '("-n50" "--decorate")  ;; was: '("-n256" "--decorate")
        magit-refresh-status-buffer t ;;automatically refresh the current Magit status buffer
        magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        )
  (defun auto-display-magit-process-buffer (&rest args)
    "Automatically display the process buffer when it is updated."
    (let ((magit-display-buffer-noselect t))
      (magit-process-buffer)))
  (advice-add 'magit-process-insert-section :before #'auto-display-magit-process-buffer)
  (remove-hook! 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook! 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  (remove-hook! 'magit-refs-sections-hook 'magit-insert-tags) ;; remove tags from ref section
  (remove-hook! 'server-switch-hook 'magit-commit-diff)) ;; remove diff on commiting

(after! lsp-ui
  (setq ;;lsp-ui-sideline-enable nil
   ;; lsp-prefer-flymake t ;; t(flymake), nil(lsp-ui), or :none
   ;;
   ;; lsp-ui-doc
   lsp-ui-doc-enable t
   lsp-ui-doc-header t
   lsp-ui-doc-include-signature t
   lsp-ui-doc-position 'top ;; top, bottom, or at-point
   lsp-ui-doc-max-width 150
   lsp-ui-doc-max-height 30
   lsp-ui-doc-use-childframe t
   lsp-ui-doc-use-webkit t
   ;; ;; lsp-ui-flycheck
   ;; lsp-ui-flycheck-enable nil
   ;; ;; lsp-ui-sideline
   lsp-ui-sideline-enable nil
   ;; lsp-ui-sideline-ignore-duplicate t
   ;; lsp-ui-sideline-show-symbol t
   ;; lsp-ui-sideline-show-hover t
   ;; lsp-ui-sideline-show-diagnostics nil
   ;; lsp-ui-sideline-show-code-actions t
   ;; lsp-ui-sideline-code-actions-prefix ""
   ;; ;; lsp-ui-imenu
   ;; lsp-ui-imenu-enable t
   ;; lsp-ui-imenu-kind-position 'top
   ;; ;; lsp-ui-peek
   ;; lsp-ui-peek-enable t
   ;; lsp-ui-peek-peek-height 40
   ;; lsp-ui-peek-list-width 90
   ;; lsp-ui-peek-fontify 'on-demand
   ))

(after! tldr
  (setq tldr-directory-path (concat doom-etc-dir "tldr/"))
  ;; (set-popup-rule! "^\\*tldr\\*" :side 'right :select t :quit t))
  )

(setq google-translate-default-source-language "en"
        google-translate-default-target-language "sp")
;; loads ENV variables
(setq
 exec-path-from-shell-check-startup-files nil
 exec-path-from-shell-variables '("PATH" "GOPATH" "GOROOT"))
(exec-path-from-shell-initialize)

;; whitespace mode
;; (global-whitespace-mode) ;; Toggle whitespace visualization globally.

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

(after! highlight-thing-mode
  (setq
   highlight-thing-limit-to-region-in-large-buffers-p t
   highlight-thing-case-sensitive-p t
   highlight-thing-limit-to-defun t
   highlight-thing-exclude-thing-under-point t))

;; easy to the eyes
(doom-themes-set-faces nil
  '(default :foreground "#bbc2cf")
  '(highlight-thing :foreground "orange" :background "black"))

(custom-set-faces!
  `(font-lock-variable-name-face :foreground "#56b6c2"))

(solaire-global-mode nil)

(add-hook! 'prog-mode-hook 'highlight-thing-mode)
(add-hook! 'conf-mode 'highlight-thing-mode)
(add-hook! 'yaml-mode 'highlight-thing-mode)
(add-hook! 'emacs-lisp-mode 'highlight-thing-mode)

;; Include underscores and hyphen in word motions
(add-hook! 'emacs-lisp-mode-hook (modify-syntax-entry ?- "w"))
(add-hook! 'after-change-major-mode-hook (modify-syntax-entry ?_ "w"))

;; enables narrowing
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; recognize some files as scripts
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.aliases\\'" . sh-mode))


;; avoid file changed on disk checking message
;; (global-auto-revert-mode -1)
(setq revert-without-query '(".*"))
(add-hook! 'yaml-mode-hook 'prog-mode)

(set-popup-rule! "^\\*kubernetes" :ignore t :select t :quit t)
(set-popup-rule! "^\\*doom:vterm*" :ignore t :select t :quit t)

(load! "+funcs")
(load! "+bindings")
(message ">>> done loading init file <<<")
