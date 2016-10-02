;; print type just pressing `,,` or `,.`
(spacemacs/set-leader-keys-for-major-mode 'scala-mode "," 'ensime-type-at-point)
(spacemacs/set-leader-keys-for-major-mode 'scala-mode "." 'ensime-type-at-point-full-name)
