;;; init.el -*- lexical-binding: t; -*-

;; Copy this file to ~/.doom.d/init.el or ~/.config/doom/init.el ('doom
;; quickstart' will do this for you). The `doom!' block below controls what
;; modules are enabled and in what order they will be loaded. Remember to run
;; 'doom refresh' after modifying it.
;;
;; More information about these modules (and what flags they support) can be
;; found in modules/README.org.

(doom! :completion
       (company +childframe)
       ivy ;; +childframe

       :ui
       deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       ;; doom-dashboard    ; a nifty splash screen for Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       (modeline +light) ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +defaults        ; default popup rules
        +all)            ; catch all popups that start with an asterix
       ;; (pretty-code +iosevka)       ; replace bits of code with pretty symbols
       treemacs          ; a project drawer, like neotree but cooler
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       multiple-cursors  ; editing in many places at once
       ;; rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       format

       :emacs
       (dired +icons)
       (ibuffer +icons)
       electric          ; smarter, keyword-based electric-indent
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       vterm             ; another terminals in Emacs

       :tools
       docker
       (eval +overlay)   ; run code, run (also, repls)
       (lookup +dictionary) ; helps you navigate your code and documentation
       (lsp +peek)
       (magit +forge)    ; a git porcelain for Emacs
       ;; pdf               ; pdf enhancements

       :checkers
       (syntax +childframe) ; Live error/warning highlights
       (spell +everywhere) ; Spell checking
       grammar

       :lang
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       (go +lsp)
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       json              ; At least it ain't XML
       (markdown +grip)  ; writing docs for people to ignore
       nix
       plantuml
       (sh +lsp)
       (org              ; organize your plain life in plain text
        ;; +attach          ; custom attachment system
        ;; +babel           ; running code in org
        ;; +capture         ; org-capture in and outside of Emacs
        +dragndrop
        ;; +export          ; Exporting org to whatever you want
        +gnuplot
        ;; +habit           ; Keep track of your habits
        ;; +present         ; Emacs for presentations
        +protocol
        ;; :tools pdf
        )       ; Support for org-protocol:// links
       rest              ; Emacs as a REST client
       ;;ruby              ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp)
       (sh +lsp)         ; she sells {ba,z,fi}sh shells on the C xor
       web               ; the tubes
       (yaml +lsp)

       :app
       (rss +org)

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme and a smartparens
       ;; config. Use it as a reference for your own modules.
       (default +funcs +bindings +snippets +smartparens +evil-commands))
