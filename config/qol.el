(use-package no-littering
  :ensure t
  :custom ((no-littering-etc-directory
	    (expand-file-name "A_temp/config/" user-emacs-directory))
	   (no-littering-var-directory
	    (expand-file-name "A_temp/data/" user-emacs-directory))))
