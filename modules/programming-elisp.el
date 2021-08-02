(use-package highlight-defined
  :straight t
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(use-package pair-tree
  :if (window-system)
  :straight
  (pair-tree
   :fetcher github
   :repo "zainab-ali/pair-tree.el"))

(use-package rainbow-mode
  :straight t
  :diminish t
  :hook
  (emacs-lisp-mode . rainbow-mode))

(use-package aggressive-indent
  :straight t
  :bind
  (:map emacs-lisp-mode-map
	("C-c C-f" . aggressive-indent-indent-defun))
  :hook
  ((emacs-lisp-mode . aggressive-indent-mode)))

(provide 'programming-elisp.el)
