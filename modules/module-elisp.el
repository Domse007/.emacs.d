(use-package highlight-defined
  :defer t
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

;; (dk/get-package!
;;     :user "zainab-ali"
;;     :repo "pair-tree.el"
;;     :force dk/get-package-override-git-availability)

;; (use-package pair-tree
;;   :if (window-system)
;;   :ensure (not dk/get-package-override-git-availability)
;;   :quelpa
;;   (pair-tree
;;    :fetcher github
;;    :repo "zainab-ali/pair-tree.el"))

(use-package rainbow-mode
  :diminish t
  :defer t
  :hook
  (emacs-lisp-mode . rainbow-mode))

(use-package aggressive-indent
  :defer t
  :bind
  (:map emacs-lisp-mode-map
	("C-c C-f" . aggressive-indent-indent-defun))
  :hook
  ((emacs-lisp-mode . aggressive-indent-mode)))

(provide 'module-elisp)
