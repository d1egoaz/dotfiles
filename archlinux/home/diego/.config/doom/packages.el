;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; fixes por ENV variables

(package! asx) ;; https://github.com/ragone/asx
(package! deadgrep)
(package! evil-snipe :disable t)
(package! exec-path-from-shell)
(package! google-translate)
(package! highlight-thing)
(package! multi-term)
(package! prettier-js)
(package! tldr)
(package! kubernetes)
(package! kubernetes-evil)
(package! multi-libvterm :recipe (:host github :repo "d1egoaz/multi-libvterm"))
