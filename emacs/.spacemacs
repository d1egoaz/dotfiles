;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs-base
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     (auto-completion :variables
                      ;; auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-company-help-tooltip t
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)
     command-log
     deft
     emacs-lisp
     evil-commentary
     (git :packages not git-messenger magit-gitflow smeargle
          :variables
          magit-refs-show-commit-count 'all ;; See commit counts for all branches and tags
          magit-diff-refine-hunk 'all
          git-magit-status-fullscreen t)
     (github :packages not gist github-clone magithub)
     go
     ;; haskell
     helm
     ;; html
     ;; javascript
     (markdown :packages not emoji-cheat-sheet-plus vmd-mode)
     nlinum
     nginx
     (php :packages not php-extras)
     (plantuml :variables plantuml-jar-path "/usr/local/Cellar/plantuml/8046/plantuml.8046.jar")
     ;; php
     react
     (osx :packages not osx-dictionary)
     (org :packages not emoji-cheat-sheet-plus mu4e)
     (shell :variables shell-default-shell 'eshell)
     ;; ruby
     (shell-scripts :packages not fish-mode)
     spacemacs-completion
     spacemacs-editing
     spacemacs-editing-visual
     spacemacs-evil
     spacemacs-language
     ;; spacemacs-layouts
     spacemacs-misc ;; dumb-jump
     spacemacs-navigation
     spacemacs-org
     spacemacs-visual
     spell-checking
     (syntax-checking :variables
                      flycheck-scalastylerc "/usr/local/etc/scalastyle_config.xml"
                      flycheck-check-syntax-automatically '(save mode-enabled))
     (scala :variables
            scala-auto-insert-asterisk-in-comments t
            scala-auto-start-ensime nil
            scala-enable-eldoc-mode nil)
     ;; react
     restclient ;; https://github.com/pashky/restclient.el
     (version-control :variables
                      version-control-diff-side 'right
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)
     yaml
     ;; My personal layers
     d1egoaz
     d1egoaz-scala
   )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(persistent-scratch
                                      protobuf-mode
                                      all-the-icons
                                      tldr
                                      flycheck-vale
                                      evil-goggles)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(evil-lisp-state
                                    evil-mc
                                    evil-nerd-commenter
                                    evil-numbers
                                    rainbow-delimiters
                                    smooth-scrolling
                                    gtags
                                    )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 15)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         monokai
                         atom-one-dark
                         zenburn
                         spacemacs-light)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Hack"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands.
   dotspacemacs-auto-generate-layout-names nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil the paste micro-state is enabled. When enabled pressing `p'
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   dotspacemacs-frame-title-format "Mode: %m, Project: %t, Buffer: %a"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ))

(defun dotspacemacs/user-init ()
  (load "~/onedrive/deft/emacs-secrets.el" t)
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)

  ;; ensime stable
  ;; (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer--elpa-archives)
  ;; (push '(ensime . "melpa-stable") package-pinned-packages)
  (push '("melpa" . "melpa.org/packages/") configuration-layer--elpa-archives)
  (push '(ensime . "melpa") package-pinned-packages)
  (push '(sbt-mode . "melpa") package-pinned-packages)
  (push '(scala-mode . "melpa") package-pinned-packages)

  ;; monokai atom one dark colors
  ;; https://github.com/jonathanchu/atom-one-dark-theme/blob/master/atom-one-dark-theme.el
  (setq ;; atom onedark colors
        monokai-use-variable-pitch nil ;; org mode monospace font
        monokai-height-minus-1 0.8
        monokai-height-plus-1 1.1
        monokai-height-plus-2 1.15
        monokai-height-plus-3 1.2
        monokai-height-plus-4 1.3
        monokai-foreground     "#b2b2b2"
        monokai-background     "#282C34"

        ;; highlights and comments
        ;; monokai-comments       "#a5a5a5"
        ;; monokai-emphasis       "#282C34"
        ;; monokai-highlight      "#FFB269"
        monokai-highlight-alt  "#66D9EF"
        monokai-highlight-line "#1B1D1E"

        ;; colors
        monokai-blue           "#61AFEF"
        monokai-cyan           "#56B6C2"
        monokai-green          "#98C379"
        monokai-gray           "#3E4451"
        monokai-violet         "#a8a1de"
        monokai-red            "#E06C75"
        monokai-orange         "#D19A66"
        monokai-yellow         "#E5C07B"
        )

  (custom-set-faces
   '(font-lock-variable-name-face ((t (:foreground "#9acb9b"))))
   '(font-lock-function-name-face ((t (:foreground "#00FF00"))))
   '(font-lock-keyword-face ((t (:foreground "#CA79DA"))))
   '(font-lock-comment-face ((t (:foreground "#58626E"))))
   '(font-lock-comment-delimiter-face ((t (:foreground "#58626E"))))
   '(font-lock-type-face ((t (:foreground "#7dbaed"))))
   '(highlight-numbers-number ((t (:foreground "#BF7D56"))))
   '(font-lock-string-face ((t (:foreground "#e4e597")))))

  (if (eq system-type 'gnu/linux)
      (setq-default dotspacemacs-default-font '("Hack"
                                                :size 24
                                                :weight normal
                                                :width normal
                                                :powerline-scale 1.1)))

)

(defun dotspacemacs/user-config ()

  ;; For complex scala files
  (setq max-lisp-eval-depth 50000)
  (setq max-specpdl-size 5000)

  (setq powerline-default-separator 'arrow)

  ;; Evil
  ;; http://spacemacs.brianthicks.com/2015/12/01/stop-cursor-creep/
  (setq evil-move-cursor-back nil
        evil-shift-round nil)

  ;; whitespace mode
  (global-whitespace-mode)

  ;; deft
  (setq deft-directory "~/onedrive/deft")
  (setq create-lockfiles nil) ;; disable .#file.ext creation

  ;; Backups
  (setq
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
  )

  ;; Settings
  (setq-default
     vc-follow-symlinks t
     ;; Whitespace mode config
     whitespace-style '(face tabs tab-mark spaces trailing spaces)
     ;; tab width
     tab-width 2
     indent-tabs-mode nil
     ;; Avy
     avy-all-windows 'all-frames
     ;; column indicator
     fci-rule-column 120
     fill-column 120
  )

  ;; Enable auto-completion.
  (global-company-mode)

  ;; Disable arrows keys in evil mode
  (define-key evil-insert-state-map [left] 'undefined)
  (define-key evil-insert-state-map [right] 'undefined)
  (define-key evil-insert-state-map [up] 'undefined)
  (define-key evil-insert-state-map [down] 'undefined)
  (define-key evil-motion-state-map [left] 'undefined)
  (define-key evil-motion-state-map [right] 'undefined)
  (define-key evil-motion-state-map [up] 'undefined)
  (define-key evil-motion-state-map [down] 'undefined)

  ;; column indicator
  (add-hook 'scala-mode-hook #'fci-mode)
  ;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
  ;; (global-fci-mode 1)

  ;; Include underscores and hyphen in word motions
  (add-hook 'scala-mode-hook
            (lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (modify-syntax-entry ?- "w")))
  (add-hook 'ruby-mode-hook
            (lambda () (modify-syntax-entry ?_ "w")))

  ;; avy
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?e ?i ?r ?u ?q ?p))

  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#4f97d7"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#bc6ec5"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "#2d9574"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "#67b11d"))))
   )

  (custom-set-faces
   '(web-mode-html-tag-face ((t (:foreground "#a6e22e")))))

  (setq hl-paren-colors '("yellow" "green" "cyan" "white"))
  (setq hl-paren-background-colors '("black" "black" "black" "light pink"))

  ;; org-mode
  (with-eval-after-load 'org
    (if (eq system-type 'darwin)
        (setq org-download-screenshot-method "screencapture -i %s"
              org-plantuml-jar-path "/usr/local/Cellar/plantuml/8046/plantuml.8046.jar"))
    (if (eq system-type 'gnu/linux)
        (setq org-download-screenshot-method "import  %s"
              org-plantuml-jar-path "/opt/plantuml/plantuml.jar"))
    (setq
          org-agenda-files (list "~/onedrive/deft/new-todo.org" "~/onedrive/deft/schedule.org")
          org-agenda-span 10
          org-agenda-start-day "-3d"
          org-confirm-babel-evaluate nil
          org-default-notes-file "~/onedrive/deft/TODO.org"
          org-download-heading-lvl nil
          org-download-image-dir "~/onedrive/deft/images"
          org-download-method 'directory
          org-refile-targets (quote ((nil :maxlevel . 9)
                                     (org-agenda-files :maxlevel . 9)))
          org-src-fontify-natively t
          org-startup-with-inline-images t
          org-capture-templates '(("n" "Next task" entry (file+headline "~/onedrive/deft/new-todo.org" "Tasks")
                                   "** NEXT %? \nDEADLINE: %t")
                                  ("l" "Link" entry (file+headline "~/onedrive/deft/links.org" "Links")
                                   "* %? %^L %^g \n%T" :prepend t)
                                  ("t" "Note" entry (file+headline "~/onedrive/deft/notes.org" "Notes")
                                   "* %?\n%T" :prepend t)
                                  ("p" "Personal - Todo Item" entry (file+headline "~/onedrive/deft/new-todo.org" "Personal Todo Items")
                                   "* %?\n  CREATED: %T" :prepend t)
                                  ("w" "Work - Todo Item" entry (file+headline "~/onedrive/deft/new-todo.org" "Work Todo Items")
                                   "* %?\n  CREATED: %T" :prepend t)
                                  ("j" "Journal" entry (file+datetree "~/onedrive/deft/journal.org")
                                   "* %?\nEntered on %U\n  %i\n  %a" :clock-in t :clock-resume t))
          org-gcal-client-id secret-org-gcal-client-id
          org-gcal-client-secret secret-org-gcal-client-secret
          org-gcal-file-alist '(("diego.alvarez@hootsuite.com" . "~/onedrive/deft/schedule.org"))
          spaceline-org-clock-p t

    )
    (custom-set-faces
     '(org-block-background
       ((t (:background "#272822"))))
     '(org-block
       ((t (:background "#272822")))))

    (add-hook 'org-mode-hook #'visual-line-mode)) ;; http://superuser.com/questions/299886/linewrap-in-org-mode-of-emacs

  ;; Scroll compilation output to first error
  (setq compilation-scroll-output 'first-error)

  (add-to-list 'auto-mode-alist '("\\.proto\\'" . prog-mode))
  (add-to-list 'auto-mode-alist '("\\.apib\\'" . markdown-mode))

  (setq-default
   ;; js2-mode
   js2-basic-offset 2
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)

  ;; 2 space indent also for element's attributes, concatenations and contiguous function calls:
  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

  (setq large-file-warning-threshold nil) ;; ignore TAGS too file warning

  ;; github
  (setq git-link-default-branch "master")
  (eval-after-load "git-link"
    '(progn
       (add-to-list 'git-link-remote-alist
                    '("github.hootops.com" git-link-github))
       (add-to-list 'git-link-commit-remote-alist
                    '("github.hootops.com" git-link-commit-github))))

  ;; magit diff
  (setq smerge-refine-ignore-whitespace nil) ;; https://github.com/magit/magit/issues/1689

  ;; google translate
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "es")

  (spacemacs/toggle-truncate-lines-on)
  (spacemacs/toggle-automatic-symbol-highlight-on)

  ;; tramp
  (eval-after-load 'tramp
    '(progn
      (setenv "SHELL" "/bin/bash")
      ;; (tramp-default-method "ssh")
      (tramp-parse-sconfig "~/.ssh/config")
      (tramp-parse-shosts "~/.ssh/known_hosts")))

  (setq spacemacs-useful-buffers-regexp '("\\*\\(ansi-term\\|eshell\\|shell\\|terminal.+\\)\\*"
                                          "\\*scratch\\*"
                                          "\\*magit.*"
                                          "\\*sbt.*"
                                          "\\*deft\\*"
                                          "\\*ansi-term.*"
                                          ))

  ;; persistent-scratch
  (persistent-scratch-setup-default)

  ;; Remap paste key to be able to paste copied text multiple times
  ;; https://github.com/robbyoconnor/spacemacs/blob/develop/doc/FAQ.org#remap-paste-key-to-be-able-to-paste-copied-text-multiple-times
  (defun evil-paste-after-from-0 ()
    (interactive)
    (let ((evil-this-register ?0))
      (call-interactively 'evil-paste-after)))

  (define-key evil-visual-state-map "p" 'evil-paste-after-from-0)

  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (setq neo-theme 'icons)
  (setq mouse-yank-at-point t) ;; middle-clicking pastes at the current location instead of moving the mouse
  (global-set-key (kbd "<key-4660>") 'ignore) ;; http://emacsredux.com/blog/2013/11/12/a-crazy-productivity-boost-remap-return-to-control/

  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(concat spacemacs-cache-directory "undo"))))
  (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
    (make-directory (concat spacemacs-cache-directory "undo")))

  (defun nothing())
  (define-key evil-normal-state-map (kbd "<down-mouse-1>") 'nothing)
  (dolist (mouse '("<down-mouse-1>" "<mouse-1>"))
    (global-unset-key (kbd mouse)))

  (with-eval-after-load 'ensime
    (setq ensime-startup-snapshot-notification nil
          ensime-startup-notification nil))

  (add-hook 'git-commit-setup-hook 'diego/insert-ticket-prefix)

  ;; enable yasnippets on scala/ensime
  (defun unimacs-company-define-backends (modes-backends-cons)
    (let ((modes    (car modes-backends-cons))
          (backends (cdr modes-backends-cons)))
      (dolist (mode modes)
        (let* ((modename (symbol-name mode))
               (funcname (concat "company-backends-for-" modename))
               (func (intern funcname))
               (hook (intern (concat modename "-hook"))))
          (setf (symbol-function func)
                `(lambda ()
                   (set (make-local-variable 'company-backends)
                        ',backends)))
          (add-hook hook func)))))
  ;; company: If ensime is on, use ensime and yasnippet. Otherwise, use dabbrev and yasnippet.
  (unimacs-company-define-backends
   '((ensime-mode) . ((ensime-company :with company-yasnippet)
                      (company-dabbrev-code :with company-dabbrev company-yasnippet)
                      company-files)))
  (setq yas-triggers-in-field t)

  ;; vale
  (flycheck-vale-setup)
  ;; evil-goggles
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)
  (setq evil-goggles-enable-paste nil)

)

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
)
