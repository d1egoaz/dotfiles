;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq
 doom-font (font-spec :family "SF Mono" :size 30)
 doom-big-font (font-spec :family "SF Mono" :size 50)
 projectile-project-search-path '("~/code/")
 ;;display-line-numbers-type 'relative
 which-key-idle-delay 0.1
 ;; stay on the original character when leaving insert mode
 evil-move-cursor-back nil
 )

(load! "+bindings")
