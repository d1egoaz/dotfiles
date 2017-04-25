;;; funcs.el --- d1egoaz-scala layer functions file for Spacemacs.
;;
;; Copyright (c) 2016 Diego Alvarez
;; Author: Diego Alvarez <diego.alvarez.zuluaga@gmail.com>
;; This file is not part of GNU Emacs.
;;; License: GPLv3

(defun diego-scala/ensime-edit-definition-with-fallback () ;; based on https://github.com/fommil/dotfiles/blob/master/.emacs.d/init.el#L620
  "Variant of `ensime-edit-definition' with ctags if ENSIME is not available."
  (interactive "P")
  (unless (and (ensime-connection-or-nil)
               (ensime-edit-definition))
    (dumb-jump-go)))

(defun diego-scala/scalafmt-file ()
  (interactive)
  (let ((str (concat "scalafmt -f " buffer-file-name " --config=" user-home-directory ".scalafmt.conf -i --exclude ensime")))
    (message str)
    (shell-command-to-string str))
  (message "scalafmt done"))

(defun edd-scala/align-dependencies ()
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-+\\)\\(%%?\\|\"\\)" 1 1 't))

(defun edd-scala/ignore-style (start end)
  "Ignore a scalastyle rule. If region is active it will be
   wrapped in a scalastyle:off/scalastyle:on comment pair. If not, a
   scalastyle:ignore comment will be used."
  (interactive "r")
  (let ((rule (completing-read "style-rule "
                               '("file.size.limit" "line.size.limit" "line.contains.tab" "header.matches" "newline.at.eof" "no.newline.at.eof" "regex" "whitespace.end.of.line" "class.name" "covariant.equals" "cyclomatic.complexity" "equals.hash.code" "if.brace" "illegal.imports" "magic.number" "method.length" "method.name" "no.clone" "no.finalize" "no.whitespace.after.left.bracket" "no.whitespace.before.left.bracket" "null" "number.of.methods" "number.of.types" "object.name" "package.object.name" "parameter.number" "public.methods.have.type" "return" "simplify.boolean.expression" "spaces.after.plus" "spaces.before.plus" "structural.type" "uppercase.l" "var.field" "var.local" "while"))))
    (save-excursion
      (if (region-active-p)
          (progn
            (goto-char end)
            (end-of-line)
            (newline)
            (insert "// scalastyle:on " rule)
            (indent-according-to-mode)
            (goto-char start)
            (beginning-of-line)
            (open-line 1)
            (insert "// scalastyle:off " rule)
            (indent-according-to-mode))
        (progn
          (end-of-line)
          (insert " // scalastyle:ignore " rule))))))
