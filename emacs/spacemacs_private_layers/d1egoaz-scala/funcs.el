;;; funcs.el --- d1egoaz-scala layer functions file for Spacemacs.
;;
;; Copyright (c) 2016 Diego Alvarez
;; Author: Diego Alvarez <diego.alvarez.zuluaga@gmail.com>
;; This file is not part of GNU Emacs.
;;; License: GPLv3

(defun diego-scala/ensime-edit-definition-with-fallback () ;; based on https://github.com/fommil/dotfiles/blob/master/.emacs.d/init.el#L620
  "Variant of `ensime-edit-definition' with ctags if ENSIME is not available."
  (interactive)
  (unless (and (ensime-connection-or-nil)
               (ensime-edit-definition))
    (dumb-jump-go)
    (projectile-find-tag)))

(defun edd-scala/align-dependencies ()
  (align-regexp (region-beginning) (region-end) "\\(\\s-+\\)\\(%%?\\|\"\\)" 1 1 't))


(setq edd-scala-sort-imports-rules
      '(("^import com\\.hootsuite\\." . "1") ("^import scala\\." . "6") ("^import java\\." . "7") ("^import javax\\." . "8") ("^import " . "5")))


(defun edd-scala/sort-imports ()
  "Sorts imports according to rules, which are cons pairs of regexp to order"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (search-forward-regexp "^package\\b")
    (search-forward-regexp "^import\\b")
    (beginning-of-line)
    (let ((start (point)))
      (while (looking-at-p "^import\\b")
        (forward-line))
      (end-of-line)
      (let ((end (point)))
        (mapcar
         (lambda (pair)
           (progn
             (goto-char start)
             (while (< (point) end)
               (let ((rule (car pair))
                     (ord (cdr pair)))
                 (when
                     (search-forward-regexp rule end 't)
                   (replace-match (concat ord "\\&")))
                 (forward-line)))))
         edd-scala-sort-imports-rules)
        (sort-lines nil start end)
        (goto-char start)
        (while (search-forward-regexp "^\\([0-9]\\)import\\b" nil 't)
          (replace-match "import"))))))

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
