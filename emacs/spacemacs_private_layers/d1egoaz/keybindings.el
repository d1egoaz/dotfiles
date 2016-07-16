;;; packages.el --- d1egoaz layer packages file for Spacemacs.
;;
;; Copyright (c) 2016 Diego Alvarez
;; Author: Diego Alvarez <diego.alvarez.zuluaga@gmail.com>
;; This file is not part of GNU Emacs.
;;; License: GPLv3

(spacemacs/declare-prefix "o" "personal")

;; File bindings
(spacemacs/set-leader-keys
  "og" 'd1egoaz-scala/ensime-edit-definition-with-fallback
  "osh" 'highlight-symbol-at-point
  "oc" 'general-close
  "oh" 'helm-eshell-history
  "or" 'indent-region
  "oad" 'd1egoaz-scala/align-dependencies
)

(define-key flyspell-mode-map (kbd "C-;") nil)
(global-set-key (kbd "C-;") 'general-close) ;; smart add close ]}
