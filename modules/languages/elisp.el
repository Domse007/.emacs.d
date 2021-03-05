;;; custom-funcs.el - These are custom functions for my config

;; commentary:
;; - This code doesen't depend on any dependencies.

;; Package that provides syntax highlighting of known Elisp symbols.
(use-package highlight-defined
  :ensure t
  :hook (emacs-lisp-mode . highlight-defined-mode))

;; Package that is a learning a tool for visualizing Emacs Lisp lists.
(use-package pair-tree
  :if (window-system)
  :ensure t
  :quelpa (pair-tree
	   :fetcher github
	   :repo "zainab-ali/pair-tree.el"))

;; Package to make the buffers more colorful.
(use-package rainbow-mode
  :ensure t
  :diminish
  :hook (emacs-lisp-mode . rainbow-mode))

(provide 'elisp.el)
;;; elisp.el ends here
