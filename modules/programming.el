(use-package lsp-mode
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
	    "lsp-server/"))
   (lsp-signature-auto-activate nil))
  :hook
  ((lsp-mode-hook . lsp-ui-mode)
   (lsp-mode-hook . linum-mode))
  :bind
  (:map lsp-mode-map
	("C-c C-f" . lsp-find-definition)))

(use-package lsp-ui
  :config
  (lsp-ui-doc-enable t)
  (lsp-ui-mode t)
  :custom
  ((lsp-ui-peek-always-show t)
   (lsp-ui-sideline-show-hover t)
   (lsp-ui-doc-position 'top)
   (lsp-ui-doc-position nil)
   (lsp-ui-doc-max-width 42)
   (lsp-ui-doc-max-height 30)
   (lsp-eldoc-hook nil)
   (lsp-ui-sideline-enable nil)))

(use-package company
  :bind
  (:map company-active-map
	("C-n". company-select-next)
	("C-p". company-select-previous)
	("M-<". company-select-first)
	("M->". company-select-last))
  :custom
  ((company-tooltip-maximum-width 60)
   (company-tooltip-width-grow-only t)
   (company-idle-delay 0)
   (company-tooltip-idle-delay 0))
  :hook
  ((emacs-lisp-mode . company-mode)
   (prog-mode . company-mode)))

(use-package company-box
  :hook
  (company-mode . company-box-mode))

(use-package flycheck)

(use-package flycheck-posframe
  :after flycheck)

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :custom
  ((transient-history-file
    (concat user-emacs-directory
	    dk/user-emacs-etcdir
	    "transient/history.el"))))

(use-package tree-sitter
  :config
  (tree-sitter-require 'rust)
  (tree-sitter-require 'python)
  (global-tree-sitter-mode)
  ;;(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  :hook
  ((rustic-mode . tree-sitter-hl-mode)
   (python-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package yasnippet
  :after yasnippet-snippets
  :custom
  ((yas-indent-line 'auto)
   (yas/snippet-dirs `(;; ,(concat user-emacs-directory
		       ;; 		dk/user-emacs-etcdir
		       ;; 		"snippets")
		       ,yasnippet-snippets-dir)))
  :config
  (yasnippet-snippets-initialize)
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package eshell
  :ensure nil
  :custom
  ((eshell-directory-name
    (concat user-emacs-directory
	    dk/user-emacs-etcdir
	    "eshell"))))

(provide 'dk/programming)
