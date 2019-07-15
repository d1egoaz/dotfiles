;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;; Unbind keys
(map! :leader
      "A" nil
      "X" nil
      "w -" nil
      )

;; Leader key
(map!
 ;; Text-scaling
 "M-+"    (λ! (text-scale-set 0))
 "M-="    #'text-scale-increase
 "M--" #'text-scale-decrease

 (:leader
   :nv ";" nil ;; unbind eval
   :desc "Toggle last popup" :n "~" #'+popup/toggle
   :desc "M-x"                    "SPC" #'execute-extended-command
   :desc "Find file in project"   "."   #'projectile-find-file
   :desc "Expand region"          ">"   #'er/expand-region
   (:desc "window" :prefix "w"
     :desc "Split window right" :n          "/" #'split-window-right
     :desc "Split window below" :n        "-" #'split-window-below)
   (:desc "buffer" :prefix               "b"
     :desc "safe erase vuffer"           "e" #'spacemacs/safe-erase-buffer
     :desc "kill current buffer"         "d" #'kill-current-buffer)
   (:desc "error" :prefix "e"
     :desc "Flycheck list errors"        "l" #'flycheck-list-errors
     :desc "Disable flycheck"            "d" #'flycheck-disable-checker
     :desc "Enable flycheck"             "C" #'flycheck-buffer
     :desc "Flycheck next error"         "n" #'flycheck-next-error
     :desc "Flycheck previous error"     "p" #'flycheck-previous-error
     :desc "Flycheck clear errors"       "c" #'flycheck-clear
     :desc "Flycheck which checker"      "w" #'flycheck-select-checker)
   (:desc "toggle" :prefix "t"
     :desc "Toggle truncate lines"       "t" #'toggle-truncate-lines)
   (:desc "jump" :prefix "j"
     :desc "Jump to symbol"              "i" #'imenu
     :desc "Jump to link"                "l" #'ace-link
     :desc "Avy jump work"               "j" #'avy-goto-char-timer)
   (:desc "search" :prefix "s"
     :desc "Search buffer"               "s" #'swiper
     :desc "Search project"              "p" #'+default/search-project
     :desc "Look up online"              "o" #'+lookup/online-select)
   (:desc "project" :prefix "p"
     :desc "Find file in project"        "f" #'projectile-find-file)
   (:desc "git" :prefix "g"
     (:when (featurep! :tools magit)
       :desc "Magit status"              "s" #'magit-status))
   (:prefix ("l" . "lang")
     :desc "Describe thing at point"      "." #'lsp-describe-thing-at-point
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
       :desc "Rename"                    "r" #'lsp-rename))))

(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup))
