;;; packages.el --- d1egoaz-scala layer packages file for Spacemacs.
;;
;; Copyright (c) 2016 Diego Alvarez
;; Author: Diego Alvarez <diego.alvarez.zuluaga@gmail.com>
;; This file is not part of GNU Emacs.
;;; License: GPLv3

(defconst d1egoaz-scala-packages
  '(
    flycheck
    flyspell
    scala-mode
    sbt-mode
    play-routes-mode
    ))


(defun d1egoaz-scala/post-init-flycheck ()
  (spacemacs/enable-flycheck 'scala-mode))

(defun d1egoaz-scala/post-init-flyspell ()
  (spell-checking/add-flyspell-hook 'scala-mode))

(defun d1egoaz-scala/init-play-routes-mode ()
  (use-package play-routes-mode
    :defer t))

(defun d1egoaz-scala/init-scala-mode ()
  (use-package scala-mode
    :defer t))


(defun d1egoaz-scala/init-sbt-mode ()
  (use-package sbt-mode
    :defer t
    :init
    (progn (spacemacs/set-leader-keys-for-major-mode 'scala-mode
            "b." 'sbt-hydra
            "bb" 'sbt-command)))
  (setq sbt:scroll-to-bottom-on-output nil
        sbt:default-command ";compile ;test:compile ;it:compile"))
