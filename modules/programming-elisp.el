(use-package highlight-defined
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

;; Package that is a learning a tool for visualizing Emacs Lisp lists.
(use-package pair-tree
  :if (window-system)
  :quelpa
  (pair-tree
   :fetcher github
   :repo "zainab-ali/pair-tree.el"))

;; Package to make the buffers more colorful.
(use-package rainbow-mode
  :diminish
  :hook
  (emacs-lisp-mode . rainbow-mode))

(provide 'programming-elisp.el)
