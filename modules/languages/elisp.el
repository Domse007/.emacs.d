(use-package highlight-defined
  :ensure t
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package pair-tree
  :if (window-system)
  :ensure t
  :quelpa (pair-tree
	   :fetcher github
	   :repo "zainab-ali/pair-tree.el"))
