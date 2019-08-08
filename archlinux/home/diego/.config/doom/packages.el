;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; fixes por ENV variables

(package! deadgrep)
(package! evil-snipe :disable t)
(package! exec-path-from-shell)
(package! highlight-thing)
(package! multi-term)
(package! prettier-js)
(package! tldr)
(package! multi-libvterm :recipe (:fetcher github :repo "d1egoaz/multi-libvterm"))
