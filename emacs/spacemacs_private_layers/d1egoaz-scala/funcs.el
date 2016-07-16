;;; funcs.el --- d1egoaz-scala layer functions file for Spacemacs.
;;
;; Copyright (c) 2016 Diego Alvarez
;; Author: Diego Alvarez <diego.alvarez.zuluaga@gmail.com>
;; This file is not part of GNU Emacs.
;;; License: GPLv3

(defun d1egoaz-scala/ensime-edit-definition-with-fallback () ;; based on https://github.com/fommil/dotfiles/blob/master/.emacs.d/init.el#L620
  "Variant of `ensime-edit-definition' with ctags if ENSIME is not available."
  (interactive)
  (unless (and (ensime-connection-or-nil)
               (ensime-edit-definition))
    (projectile-find-tag)))

(defun d1egoaz-scala/align-dependencies ()
  (align-regexp (region-beginning) (region-end) "\\(\\s-+\\)\\(%%?\\|\"\\)" 1 1 't))

;;; funcs.el ends here
