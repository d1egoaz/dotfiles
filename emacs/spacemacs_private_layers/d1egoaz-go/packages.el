
(defconst d1egoaz-go-packages
	'(
      ob-go
    ))


(defun d1egoaz-go/pre-init-ob-go ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-go
      :init (add-to-list 'org-babel-load-languages '(go . t)))))

(defun d1egoaz-go/init-ob-go ()
	(use-package ob-go
		:defer t
		:init))
