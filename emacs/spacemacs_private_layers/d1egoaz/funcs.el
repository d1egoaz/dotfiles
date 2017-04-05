
;; @fommil
(defun d1egoaz/indent-buffer ()
  "Indent the entire buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

(defun diego/delete-last-character-end-of-line ()
  "Delete last character in line"
  (interactive)
  (save-excursion
    (move-end-of-line 1)
    (delete-backward-char 1)))

(with-eval-after-load 'hydra
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))

  (defhydra diego/hydra-dumb-jump (:color pink)
    "Dumb Jump"
    ("g" dumb-jump-go "Go")
    ("b" dumb-jump-back "Back")
    ("f" dumb-jump-quick-look "Look")
    ("e" dumb-jump-go-prefer-external "External")
    ("o" dumb-jump-go-other-window "Other window")
    ("q" nil "Quit" :color blue))
)
