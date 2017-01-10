;;; packages.el --- d1egoaz layer packages file for Spacemacs.
;;
;; Copyright (c) 2016 Diego Alvarez
;; Author: Diego Alvarez <diego.alvarez.zuluaga@gmail.com>
;; This file is not part of GNU Emacs.
;;; License: GPLv3

(defconst d1egoaz-packages
  '(
    syntactic-close
    ))

(defun d1egoaz/init-syntactic-close ()
  (use-package syntactic-close
    :ensure t))

;;; packages.el ends here
