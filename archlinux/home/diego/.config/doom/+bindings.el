;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-

;; :desc "Comment/Uncomment line" "; ;"  #'comment-line ;; unbind eval
;; Unbind keys
(map! :leader
      "A" nil
      "X" nil
      "w -" nil
      )

;; Leader key
(map!
 (:leader
   :nv ";" nil ;; unbind eval
   :desc "Toggle last popup" :n "~" #'+popup/toggle
   :desc "M-x"                    "SPC" #'execute-extended-command
   :desc "Find file in project"   "."   #'projectile-find-file
   :desc "Expand region"          ">"   #'er/expand-region
   (:desc "window" :prefix "w"
     :desc "Split window right" :n          "/" #'split-window-right
     :desc "Split window below" :n        "-" #'split-window-below
     )
   (:desc "error" :prefix "e"
     :desc "Flycheck list errors"        "l" #'flycheck-list-errors
     :desc "Disable flycheck"            "d" #'flycheck-disable-checker
     :desc "Enable flycheck"             "C" #'flycheck-buffer
     :desc "Flycheck next error"         "n" #'flycheck-next-error
     :desc "Flycheck previous error"     "p" #'flycheck-previous-error
     :desc "Flycheck clear errors"       "c" #'flycheck-clear
     :desc "Flycheck which checker"      "w" #'flycheck-select-checker
     )
   (:desc "toggle" :prefix "t"
     :desc "Toggle truncate lines"       "t" #'toggle-truncate-lines
     )
   (:desc "jump" :prefix "j"
     :desc "Jump to symbol"              "i" #'imenu
     :desc "Jump to link"                "l" #'ace-link
     :desc "Avy jump work"               "j" #'avy-goto-char-timer
     )
   (:desc "search" :prefix "s"
     :desc "Search buffer"               "s" #'swiper
     :desc "Search project"              "p" #'+default/search-project
     :desc "Look up online"              "o" #'+lookup/online-select
     )
   (:desc "project" :prefix "p"
     :desc "Find file in project"        "f" #'projectile-find-file
     )
   (:desc "git" :prefix "g"
     (:when (featurep! :tools magit)
       :desc "Magit status"              "s" #'magit-status
       )
     )
   )
 )
