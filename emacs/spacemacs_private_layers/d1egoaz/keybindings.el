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
  ;; "jj"  'avy-goto-char-timer
  "jj"  'avy-goto-word-or-subword-1
  "od"  'diego/delete-last-character-end-of-line
  "of"  'diego-scala/scalafmt-file
  "og"  'dumb-jump-go
  "oh"  'highlight-symbol-at-point
  "oH"  'hi-lock-unface-buffer
  ;; "oo"  'sbt-hydra
  "or"  'indent-region
  "oi" 'yas-insert-snippet
  ;; "osd" 'edd-scala/align-dependencies
  ;; "osf" 'diego-scala/scalafmt-file
  "osg" 'edd-scala/ignore-style
  "osh" 'sbt-hydra
  "osl" 'sbt-run-previous-command
  "oss" 'sbt-command
  "ox"  'diego-scala/scalafix-file
  "aoL" 'diego/org-insert-last-stored-link
)

(with-eval-after-load 'flyspell
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (global-set-key (kbd "C-;") 'syntactic-close) ;; smart add close ]}
)

(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "C-SPC k") 'evil-window-up)
(global-set-key (kbd "C-SPC j") 'evil-window-down)

(with-eval-after-load 'evil
  (global-set-key (kbd "C-i") 'evil-jump-forward)
  (global-set-key (kbd "C-o") 'evil-jump-backward))
