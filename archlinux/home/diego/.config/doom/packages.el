;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; fixes por ENV variables
(package! exec-path-from-shell)
(package! golden-ratio)
(package! highlight-thing)

(package! evil-snipe :disable t)
