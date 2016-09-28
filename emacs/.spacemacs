; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ansible
     (auto-completion :variables
                      ;; auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-company-help-tooltip t
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     ;; (colors :variables colors-enable-nyan-cat-progress-bar nil)
     command-log
     deft
     emacs-lisp
     evil-commentary
     emoji
     (git :variables
          ;; See commit counts for all branches and tags
          magit-refs-show-commit-count 'all
          magit-diff-refine-hunk 'all
          git-magit-status-fullscreen t)
     github
     gtags
     helm
     html
     javascript
     (markdown :variables markdown-live-preview-engine 'vmd)
     nlinum
     nginx
     plantuml
     php
     osx
     org
     (shell :variables
            shell-default-shell 'eshell
            shell-default-term-shell "/usr/local/bin/zsh")
     sql
     spacemacs-evil
     spacemacs-language
     spacemacs-ui-visual
     spell-checking
     (syntax-checking :variables
                      syntax-checking-enable-by-default t)
     (scala :variables
           scala-auto-insert-asterisk-in-comments t
           scala-auto-start-ensime nil
           scala-enable-eldoc-mode nil)
     react
     restclient ;; https://github.com/pashky/restclient.el
     (version-control :variables
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
   dotspacemacs-additional-packages '(beacon)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(rainbow-delimiters)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         monokai
                         atom-one-dark
                         zenburn
                         spacemacs-light)

   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Hack"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
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
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
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
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
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
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed
   ))

(defun dotspacemacs/user-init ()
  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer--elpa-archives)
  (push '(ensime . "melpa-stable") package-pinned-packages)
)

(defun dotspacemacs/user-config ()

  ;; For complex scala files
  (setq max-lisp-eval-depth 50000)
  (setq max-specpdl-size 5000)

  ;; Disable smartparens for most pairs, my editing style doesn't play well with it
  (eval-after-load 'smartparens
    '(progn
       (sp-pair "(" nil :actions :rem)
       (sp-pair "[" nil :actions :rem)
       (sp-pair "'" nil :actions :rem)
       (sp-pair "\"" nil :actions :rem)
     )
  )

  (setq powerline-default-separator 'arrow)

  ;; http://spacemacs.brianthicks.com/2015/12/01/stop-cursor-creep/
  (setq evil-move-cursor-back nil)

  ;; whitespace mode
  (global-whitespace-mode)

  ;; deft
  (setq deft-directory "~/OneDrive/deft")
  (setq create-lockfiles nil) ;; disable .#file.ext creation

  ;; Backups
  (setq
    make-backup-files nil ;; <- DISABLED
    backup-directory-alist '(("" . "~/emacs_backups/per-save"))
    auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
    backup-by-copying t
    version-control t
    delete-old-versions t
    delete-by-moving-to-trash t
    kept-new-versions 9
    kept-old-versions 6
    vc-make-backup-files t
    auto-save-default t
    auto-save-timeout 20
    auto-save-interval 200
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

  ;; indentguide indicator
  (spacemacs/toggle-indent-guide-globally-on)

  ;; column indicator
  (add-hook 'scala-mode-hook 'fci-mode)

  ;; Turn off smart-paren auto-highlighting
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  ;; avy
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?e ?i ?r ?u ?q ?p))

 ;; (set-background-color "#1d1f21")
  ;; (set-background-color "#282C34")
  ;; (set-foreground-color "#CED1CF")
  ;; (set-face-background 'hl-line "#333333") ;; highlight line
  ;; (set-face-attribute 'fringe nil :background "#272822" :foreground "green") ;; column before line numbers column
  ;; (set-face-attribute 'linum nil :background "#212026" :foreground "#44505c") ;; line numbers column
  ;; (set-face-attribute 'nlinum-relative-current-face nil :background "#212026" :foreground "#c56ec3") ;; current line number

  ;; (set-face-attribute 'font-lock-comment-face nil :foreground "#969896") ;; comments
  ;; (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "#969896") ;; comments
  ;; (set-face-attribute 'font-lock-string-face nil :foreground "#B5BD68") ;; string
  ;; (set-face-attribute 'font-lock-type-face nil :background "#292E34" :foreground "#28CCDE") ;; types
  ;; (set-face-attribute 'font-lock-function-name-face nil  :foreground "#A6E22E") ;; functions without background-color
  ;; ;; (set-face-attribute 'font-lock-variable-name-face nil :background "#362E26" :foreground "#FD971F") ;; variables
  ;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#f0c674") ;; variables without background-color
  ;; (set-face-attribute 'font-lock-keyword-face nil :foreground "#81a2be") ;; keyword
  ;; (set-face-attribute 'font-lock-builtin-face nil :foreground "#cc6666") ;; built in
  ;; (set-face-attribute 'region nil :background "#666") ;; selected region background color
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


  ;; ensime
  ;; https://github.com/ensime/ensime-server/issues/1036
  (setq ensime-sem-high-faces
        '(
          ;; (implicitConversion nil)
          ;; (var . (:foreground "#ff2222"))
          ;; (varField . (:foreground "#ff3333"))
          ;; (functionCall . (:foreground "#de935f"))
          ;; (object . (:foreground "#9f8cbb"))
          ;; (operator . (:foreground "yellow"))
          ;; (package . (:foreground "yellow"))
          ;; (deprecated . (:strike-through "#a9b7c6"))
          ;; (implicitConversion nil)
          ;; (implicitParams nil)
          )
        ;; ensime-completion-style 'company
        ;; ensime-sem-high-enabled-p nil ;; disable semantic highlighting
        ensime-tooltip-hints t ;; disable type-inspecting tooltips
        ensime-tooltip-type-hints t ;; disable typeinspecting tooltips
        ensime-graphical-tooltips t
        ensime-auto-generate-config t
        ensime-use-helm t
        )

  ;; the compiler sees duplicate symbol definitions
  (add-hook 'git-timemachine-mode-hook (lambda () (ensime-mode 0)))

  ;; org-mode
  (setq org-src-fontify-natively t
        org-startup-with-inline-images t)
  (add-hook 'org-mode-hook 'visual-line-mode) ;; http://superuser.com/questions/299886/linewrap-in-org-mode-of-emacs

  ;; Scroll compilation output to first error
  (setq compilation-scroll-output t)
  (setq compilation-scroll-output 'first-error)

  (add-hook 'conf-unix-mode-hook 'spacemacs/toggle-line-numbers-on)
  (add-hook 'restclient-mode-hook 'spacemacs/toggle-line-numbers-on)
  (add-to-list 'auto-mode-alist '("\\.proto\\'" . prog-mode))
  (add-to-list 'auto-mode-alist '("\\.apib\\'" . markdown-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))

  ;; js-mode
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

  (fringe-helper-define 'git-gutter-fr+-added nil
    ".XXXXXX."
    "XX....XX"
    "X......X"
    "X......X"
    "XXXXXXXX"
    "XXXXXXXX"
    "X......X"
    "X......X")

  (fringe-helper-define 'git-gutter-fr+-deleted nil
    "XXXXXX.."
    "XX....X."
    "XX.....X"
    "XX.....X"
    "XX.....X"
    "XX.....X"
    "XX....X."
    "XXXXXX..")

  (fringe-helper-define 'git-gutter-fr+-modified nil
    "XXXXXXXX"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X"
    "X..XX..X")

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
  ;; (setq smerge-refine-ignore-whitespace nil) ;; https://github.com/magit/magit/issues/1689

  ;; google translate
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "es")

  (spacemacs/toggle-truncate-lines-on)
  (spacemacs/toggle-automatic-symbol-highlight-on)

  (setq flycheck-scalastyle-jar "/usr/local/Cellar/scalastyle/0.8.0/libexec/scalastyle_2.11-0.8.0-batch.jar")
  (setq flycheck-scalastylerc "/usr/local/etc/scalastyle_config.xml")
)
