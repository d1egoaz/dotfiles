;;; ~/dotfiles/archlinux/home/diego/.config/doom/+bindings.el -*- lexical-binding: t; -*-

;; Unbind keys
(map! :leader
      "A" nil
      "X" nil
      "w -" nil
      "h P" nil)

(map! :leader "`" #'evil-switch-to-windows-last-buffer)

;; Leader key
(map!
 ;; Text-scaling
 "M-+" (λ! (text-scale-set 0))
 "M-=" #'text-scale-increase
 "M--" #'text-scale-decrease

 (:when (featurep! :completion ivy)
  (:map ivy-minibuffer-map
   "C-j" 'ivy-next-line
   "C-k" 'ivy-previous-line
   "C-h" (kbd "DEL")))

 ;; https://github.com/suonlight/multi-libvterm
 (:when (featurep! :term vterm)
  (:map vterm-mode-map
   :ni "C-j"     'vterm--self-insert
   :ni "C-k"     'vterm--self-insert
   :ni "C-d"     'vterm--self-insert
   :ni "C-SPC"   'vterm--self-insert
   :i "<return>" (λ! (interactive) (process-send-string vterm--process "\C-m"))))

 (:leader
  :nv ";" nil ;; unbind eval
  :desc "Toggle last popup"  "~" #'+popup/toggle
  :desc "M-x"                    "SPC" #'execute-extended-command
  :desc "Find file in project"   "."   #'projectile-find-file
  :desc "Expand region"          ">"   #'er/expand-region
  (:desc "+apps" :prefix "a"
   :desc "undo tree"                   "u" #'undo-tree-visualize
   :desc "org capture"                 "o" #'counsel-org-capture
   :desc "List process"                "p" #'list-processes
   :desc "Kill process"                "P" #'counsel-list-processes
   :desc "align regexp"                "x" #'align-regexp)
  (:desc "+buffer" :prefix "b"
   :desc "safe erase buffer"           "e" #'spacemacs/safe-erase-buffer
   :desc "kill current buffer"         "d" #'kill-current-buffer
   :desc "Last buffer"                 "l" #'evil-switch-to-windows-last-buffer
   :desc "yank buffer name"            "y" #'diego/copy-buffer-name
   :desc "copy buffer to clipboard"    "Y" #'spacemacs/copy-whole-buffer-to-clipboard)
  (:desc "+error" :prefix "e"
   :desc "Flycheck list errors"        "l" #'flycheck-list-errors
   :desc "Disable flycheck"            "d" #'flycheck-disable-checker
   :desc "Enable flycheck"             "C" #'flycheck-buffer
   :desc "Flycheck next error"         "n" #'flycheck-next-error
   :desc "Flycheck previous error"     "p" #'flycheck-previous-error
   :desc "Flycheck clear errors"       "c" #'flycheck-clear
   :desc "Flycheck which checker"      "w" #'flycheck-select-checker)
  (:desc "+file" :prefix "f"
   :desc "jump to file"                "j" #'counsel-file-jump)
  (:desc "+git" :prefix "g"
   :desc "Magit status"                "s" #'magit-status)
  (:prefix ("P" . "Profiler")
   :desc "Profiler start"            "s" #'profiler-start
   :desc "Profiler stop"             "k" #'profiler-stop
   :desc "Profiler report"           "r" #'profiler-report)
  (:desc "+insert" :prefix "i"
   :desc "insert line above"           "k" #'spacemacs/evil-insert-line-above
   :desc "insert line below"           "j" #'spacemacs/evil-insert-line-below)
  (:desc "+jump" :prefix "j"
   :desc "Jump to recent file/buffer"  "f" #'counsel-buffer-or-recentf
   :desc "Jump to symbol"              "i" #'imenu
   :desc "Jump to line"                "l" #'avy-goto-line
   :desc "Avy jump work"               "j" #'avy-goto-char-timer)
  (:desc "+kubel" :prefix "k"
   :desc "set context"                 "c" #'kubel-set-context
   :desc "set namespace"               "n" #'kubel-set-namespace
   :desc "describe"                    "d" #'kubel-get-resource-details
   :desc "exec"                        "e" #'kubel-exec-pod
   :desc "filter"                      "f" #'kubel-set-filter
   :desc "help"                        "h" #'kubel-help-popup
   :desc "log"                         "l" #'kubel-log-popup
   :desc "port forward"                "p" #'kubel-port-forward-pod
   :desc "set resource"                "r" #'kubel-set-resource
   :desc "yank"                        "y" #'kubel-copy-popup)
  (:desc "+lang" :prefix "l"
   :desc "Describe thing at point"     "." #'lsp-describe-thing-at-point
   (:prefix ("g" . "Go to")
    :desc "Implementation"            "i" #'lsp-goto-implementation
    :desc "Definition"                "d" #'lsp-goto-type-definition)
   (:prefix ("f" . "Find")
    :desc "Definition"                "d" #'lsp-find-definition
    :desc "References"                "r" #'lsp-find-references)
   (:prefix ("l" . "Lens")
    :desc "Show"                      "l" #'lsp-lens-show
    :desc "Hide"                      "q" #'lsp-lens-hide)
   (:prefix ("m" . "menu")
    :desc "Show"                      "m" #'lsp-ui-imenu
    :desc "Hide"                      "q" #'lsp-ui-imenu--kill)
   (:prefix ("r" . "refactor")
    :desc "Rename"                    "r" #'lsp-rename)
   (:prefix ("s" . "show")
    :desc "show signature"            "s" #'lsp-signature-activate))
  (:desc "+narrow/notes" :prefix "n"
   :desc "narrow region"   "r" #'narrow-to-region
   :desc "narrow defun"   "f" #'narrow-to-defun
   :desc "narrow widen"   "w" #'widen)
  (:desc "+open" :prefix "o"
   :desc "delete last character eol"   "d" #'diego/delete-last-character-end-of-line
   :desc "highliht symbol at point"    "h" #'highlight-symbol-at-point
   :desc "remove highlight symbol"     "H" #'hi-lock-unface-buffer)
  (:desc "+project" :prefix "p"
   :desc "Find file in project"        "f" #'projectile-find-file)
  (:desc "+search" :prefix "s"
   :desc "iedit"                       "e" #'iedit-mode ;; next item [TAB]
   :desc "engine"                      "E" #'diego/engine-searches
   :desc "Search buffer"               "s" #'swiper-isearch
   :desc "Search project"              "p" #'+default/search-project
   :desc "Look up online"              "o" #'+lookup/online-select)
  (:desc "+toggle" :prefix "t"
   :desc "Toggle truncate lines"       "t" #'toggle-truncate-lines)
  (:desc "+window" :prefix "w"
   :desc "Split window right"        "/" #'split-window-right
   :desc "Split window below"        "-" #'split-window-below)))

(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup))

(after! lsp-ui-peek
  (map! :map lsp-ui-peek-mode-map
        "C-p" #'lsp-ui-peek--select-prev-file
        "C-j" #'lsp-ui-peek--select-next
        "C-k" #'lsp-ui-peek--select-prev
        "C-n" #'lsp-ui-peek--select-next-file))

(after! lsp-mode
  (map! :leader
        (:prefix "c"
         :desc "LSP" "l" lsp-command-map)))

(after! elfeed
  (map! :map elfeed-search-mode-map
        :n "r" #'elfeed-search-untag-all-unread
        :n "s" #'elfeed-search-live-filter
        :n "t" #'diego/elfeed-ivy-filter
        :n "u" #'elfeed-search-tag-all-unread
        :n "c" #'elfeed-search-clear-filter
        :n "e" #'diego/elfeed-open-with-eww
        :n "," #'diego/elfeed-filter)
  (map! :map elfeed-show-mode-map
        :n "n" #'diego/find-open-next-url))

(after! kubel
  (map! :map kubel-mode-map
        :localleader
        :desc "set context"                 "c" #'kubel-set-context
        :desc "set namespace"               "n" #'kubel-set-namespace
        :desc "describe"                    "d" #'kubel-get-resource-details
        :desc "describe popup"              "D" #'kubel-describe-popup
        :desc "exec"                        "e" #'kubel-exec-pod
        :desc "filter"                      "f" #'kubel-set-filter
        :desc "help"                        "h" #'kubel-help-popup
        :desc "log"                         "l" #'kubel-log-popup
        :desc "port forward"                "p" #'kubel-port-forward-pod
        :desc "set resource"                "r" #'kubel-set-resource
        :desc "yank"                        "y" #'kubel-copy-popup)
  )

;; (define-key map (kbd "F") 'kubel-set-output-format)
;; ;; (define-key map (kbd "k") 'kubel-delete-popup)
;; ;; based on view

(after! evil
  (map! :map evil-normal-state-map
        "C-r" #'undo-fu-only-redo))

(defhydra hydra-paste (:color red
                       :hint nil)
  "\n[%s(length kill-ring-yank-pointer)/%s(length kill-ring)] \
 [_C-j_/_C-k_] cycles through yanked text, [_p_/_P_] pastes the same text \
 above or below. Anything else exits."
  ("C-j" evil-paste-pop)
  ("C-k" evil-paste-pop-next)
  ("p" evil-paste-after)
  ("P" evil-paste-before))

(map! :nv "p" #'hydra-paste/evil-paste-after
      :nv "P" #'hydra-paste/evil-paste-before)
