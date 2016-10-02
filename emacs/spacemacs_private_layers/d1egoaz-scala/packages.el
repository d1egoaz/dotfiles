;;; packages.el --- d1egoaz-scala layer packages file for Spacemacs.
;;
;; Copyright (c) 2016 Diego Alvarez
;; Author: Diego Alvarez <diego.alvarez.zuluaga@gmail.com>
;; This file is not part of GNU Emacs.
;;; License: GPLv3

(defconst d1egoaz-scala-packages
  '(
    sbt-mode
    scala-mode
    evil
    play-routes-mode))

(defun d1egoaz-scala/init-play-routes-mode ()
  (use-package scala-mode
    :defer t))

(defun d1egoaz-scala/init-play-routes-mode ()
  (use-package play-routes-mode
    :defer t))

(defun d1egoaz-scala/init-sbt-mode ()
  (use-package sbt-mode
    :defer t
    :init
    (setq
     sbt:scroll-to-bottom-on-output nil
     sbt:default-command ";compile ;test:compile ;it:compile")))


