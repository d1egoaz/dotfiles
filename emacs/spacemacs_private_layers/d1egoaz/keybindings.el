;;; packages.el --- d1egoaz layer packages file for Spacemacs.
;;
;; Copyright (c) 2016 Diego Alvarez
;; Author: Diego Alvarez <diego.alvarez.zuluaga@gmail.com>
;; This file is not part of GNU Emacs.
;;; License: GPLv3

(spacemacs/declare-prefix "o" "personal")

;; File bindings
(spacemacs/set-leader-keys
  ;; general
  "glh" 'git-link-homepage
  "oc" 'general-close
  "od" 'diego/delete-last-character-end-of-line
  "og" 'dumb-jump-go
  "oh" 'highlight-symbol-at-point
  ;; "oh" 'helm-eshell-history
  "opf" 'project-find-file
  "or" 'indent-region
  ;; scala
  "oss" 'sbt-command
  "osd" 'edd-scala/align-dependencies
  "osf" 'diego-scala/scalafmt-file
  "osg" 'edd-scala/ignore-style
  "osi" 'edd-scala/sort-imports
  "osl" 'sbt-run-previous-command
)

(define-key flyspell-mode-map (kbd "C-;") nil)
(global-set-key (kbd "C-;") 'general-close) ;; smart add close ]}

(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
