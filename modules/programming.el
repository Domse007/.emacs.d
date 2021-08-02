(use-package lsp-mode
  :straight t
  :custom
  ((lsp-rust-analyzer-cargo-watch-command "clippy")
   (lsp-eldoc-render-all t)
   (lsp-idle-delay 0.6)
   (lsp-rust-analyzer-server-display-inlay-hints t)
   (lsp-session-file
    (concat user-emacs-directory
	    dk/user-emacs-etcdir
	    "lsp/lsp"))
   (lsp-server-install-dir
    (concat user-emacs-directory
	    dk/user-emacs-etcdir
	    "lsp-server/")))
  :hook
  ((lsp-mode-hook . lsp-ui-mode)))

(use-package lsp-ui
  :straight t
  :custom
  ((lsp-ui-peek-always-show t)
   (lsp-ui-sideline-show-hover t)
   (lsp-ui-doc-enable nil)))

(use-package company
  :bind
  (:map company-active-map
	("C-n". company-select-next)
	("C-p". company-select-previous)
	("M-<". company-select-first)
	("M->". company-select-last)))

(use-package company-box
  :straight t
  :hook
  (company-mode . company-box-mode))

(use-package flycheck
  :straight t)

(use-package flycheck-posframe
  :straight t
  :after flycheck)

(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :straight t)

(provide 'programming.el)
