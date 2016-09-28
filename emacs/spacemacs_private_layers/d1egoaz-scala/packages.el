;;; packages.el --- d1egoaz-scala layer packages file for Spacemacs.
;;
;; Copyright (c) 2016 Diego Alvarez
;; Author: Diego Alvarez <diego.alvarez.zuluaga@gmail.com>
;; This file is not part of GNU Emacs.
;;; License: GPLv3

(defconst d1egoaz-scala-packages
  '(
    ;; ensime
    play-routes-mode))

(defun d1egoaz-scala/init-play-routes-mode ()
  (use-package play-routes-mode
    :defer t))
