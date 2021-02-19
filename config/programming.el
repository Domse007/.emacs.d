;; LSP configuration:
(use-package dash
  :ensure t)

(use-package lsp-mode
  :ensure t
  :custom ((lsp-rust-server 'rust-analyzer)
	   (lsp-enable-completion-at-point t))
  :hook ((lsp-after-open . lsp-enable-imenu)
	 (python-mode . lsp-deferred)
	 (rust-mode . lsp-deferred)
	 (csharp-mode . lsp-deferred)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom ((lsp-ui-sideline-ignore-duplicate t)
	   (lsp-ui-doc-max-height 30)
	   (lsp-ui-doc-max-width 30)
	   (lsp-ui-doc-position 'point)
	   (lsp-ui-doc-show-with-mouse t)
	   (lsp-ui-doc-show-with-cursor t))
  :hook (lsp-mode . lsp-ui)
  :config (lsp-ui-doc-enable t))

(use-package lsp-python-ms
  :ensure t
  :after lsp)

(use-package company
  :ensure t
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package company-posframe
  :ensure t
  :if window-system
  :config
  (company-posframe-mode t))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; visual packages
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package tree-sitter
  :ensure t
  :custom (global-tree-sitter-mode t)
  :hook (tree-sitter-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

;; git version control system
(use-package magit
  :ensure t)


(load-file (concat user-emacs-directory "/config/languages/web-dev.el"))
(load-file (concat user-emacs-directory "/config/languages/elisp.el"))
(load-file (concat user-emacs-directory "/config/languages/rust.el"))
(load-file (concat user-emacs-directory "/config/languages/c-sharp.el"))

