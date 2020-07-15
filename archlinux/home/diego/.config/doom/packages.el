;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; fixes por ENV variables

(package! atomic-chrome)
(package! asx) ;; https://github.com/ragone/asx
(package! base16-theme)
(package! command-log-mode)
(package! deadgrep)
(package! dockerfile-mode)
(package! emojify)
(package! engine-mode :recipe (:host github :repo "hrs/engine-mode" :branch "main"))
(package! exec-path-from-shell)
(package! google-translate)
(package! hackernews)
(package! highlight-indent-guides)
(package! highlight-thing)
(package! string-inflection)
(package! keycast)
(package! kubel :recipe (:host github :repo "d1egoaz/kubel"))
(package! markdown-changelog)
(package! multi-libvterm :recipe (:host github :repo "suonlight/multi-libvterm"))
(package! multi-term)
(package! powerthesaurus)
(package! prettier-js)
(package! tldr)

(unpin! lsp-mode)
(unpin! lsp-ui)
(unpin! lsp-ivy)
