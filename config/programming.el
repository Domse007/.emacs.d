;; LSP configuration:
(use-package lsp-mode
  :ensure t
  :defer 3
  :commands lsp
  :config (setq lsp-rust-server 'rust-analyzer
		lsp-enable-completion-at-point t)
  :hook ((python-mode . lsp)
	 (rust-mode . lsp)))
   
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config (lsp-ui-doc-enable t))

(use-package lsp-python-ms
  :ensure t
  :demand t
  :after lsp)

;; universal stuff (including org-mode):
(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))

;; company stuff
(use-package company
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)))

(use-package company-posframe
  :ensure t
  :config
  (company-posframe-mode 0))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

;; git version control system
(use-package magit
  :ensure t)


(load-file (concat user-emacs-directory "/config/languages/web-dev.el"))
(load-file (concat user-emacs-directory "/config/languages/elisp.el"))
(load-file (concat user-emacs-directory "/config/languages/rust.el"))
(load-file (concat user-emacs-directory "/config/languages/c-sharp.el"))

