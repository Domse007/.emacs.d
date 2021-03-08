;;; programming.el - This is a configuration to enable a better programming experience.

;; commentary:
;; - The code depends on language specific language servers.
;; - There are sometimes weird buffers popping up..... sometimes.

;; Package that enables Emacs to behave as an LSP client.
(use-package lsp-mode
  :ensure t
  :custom ((lsp-rust-server 'rust-analyzer)
	   (lsp-enable-completion-at-point t)
	   (lsp-session-file "~/.emacs.d/var/lsp/lsp")
	   (lsp-server-install-dir "~/.emacs.d/var/lsp-server/")
	   (lsp-keep-workspace-alive nil)
	   (lsp-lens-enable t)
	   (lsp-diagnostics-provider :auto)
	   (lsp-modeline-diagnostics-enable t)
	   (lsp-auto-configure t)
	   (lsp-enable-snippet t))
  :hook ((lsp-after-open . lsp-enable-imenu)
	 (python-mode . lsp-deferred)
	 (rust-mode . lsp-deferred)
	 (csharp-mode . lsp-deferred))
  :config
  (lsp-headerline-breadcrumb-mode t))

;; Package that provides an ui to lsp-mode.
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom ((lsp-ui-sideline-ignore-duplicate t)
	   (lsp-ui-sideline-enable nil)
	   (lsp-ui-doc-max-height 30)
	   (lsp-ui-doc-max-width 30)
	   (lsp-ui-doc-position 'point)
	   (lsp-ui-doc-show-with-mouse t)
	   (lsp-ui-doc-show-with-cursor t))
  :hook (lsp-mode . lsp-ui)
  :config (lsp-ui-doc-enable t))

;; Package that provides the protocol to talk to the Microsoft Python LSP.
(use-package lsp-python-ms
  :ensure t
  :after lsp)

;; Package that provides ivy integration for lsp
(use-package lsp-ivy
  :ensure t)

;; Package that provides a completion framework.
(use-package company
  :ensure t
  :config (add-hook 'after-init-hook 'global-company-mode))

;; Package that provides posframe to company.
(use-package company-posframe
  :ensure t
  :if window-system
  :config
  (company-posframe-mode t))

;; Package that provides a better looking interface to comapany. 
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; Package that provides coloured delimiters.
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; This is an Emacs Lisp binding for tree-sitter.
(use-package tree-sitter
  :ensure t
  :custom (global-tree-sitter-mode t)
  :hook (tree-sitter-mode . tree-sitter-hl-mode))

;; Package that provides other languages to tree-sitter.
(use-package tree-sitter-langs
  :after tree-sitter)

;; Git version control system
(use-package magit
  :ensure t
  :defer t
  :commands magit)

;; Load other files.
(load-file (concat user-emacs-directory dk/user-emacs-subdir "/languages/web-dev.el"))
(load-file (concat user-emacs-directory dk/user-emacs-subdir "/languages/elisp.el"))
(load-file (concat user-emacs-directory dk/user-emacs-subdir "/languages/rust.el"))
(load-file (concat user-emacs-directory dk/user-emacs-subdir "/languages/c-sharp.el"))

(provide 'programming.el)
;;; programming.el ends here
