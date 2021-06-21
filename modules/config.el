(use-package no-littering
  :custom
  ((no-littering-etc-directory
    (concat user-emacs-directory
	    dk/user-emacs-etcdir
	    "etc/"))
   (no-littering-var-directory
    (concat user-emacs-directory
	    dk/user-emacs-etcdir
	    "var/"))))

(provide 'config.el)
