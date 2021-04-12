;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; fixes por ENV variables

(package! asx) ;; https://github.com/ragone/asx
(package! atomic-chrome)
(package! base16-theme)
(package! command-log-mode)
(package! deadgrep)
(package! dockerfile-mode)
(package! engine-mode :recipe (:host github :repo "hrs/engine-mode" :branch "main"))
(package! exec-path-from-shell)
(package! google-translate)
(package! hackernews)
(package! highlight-thing)
(package! keycast)
(package! kubel :recipe (:host github :repo "d1egoaz/kubel"))
(package! markdown-changelog)
(package! mw-thesaurus)
;; (package! modus-themes :recipe (:host gitlab :repo "protesilaos/modus-themes" :branch "main"))
(package! olivetti)
(package! ox-gfm)
(package! powerthesaurus)
(package! prettier-js)
(package! string-inflection)
(package! tldr)

(unpin! lsp-mode)
(unpin! lsp-ui)
(unpin! lsp-ivy)
