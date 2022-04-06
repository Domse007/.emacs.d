(use-package lsp-mode
  :defer t
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
   (lsp-signature-auto-activate nil)
   ;; rust
   (lsp-rust-analyzer-display-closure-return-type-hints t)
   (lsp-rust-analyzer-display-parameter-hints t)
   (lsp-rust-analyzer-import-granularity "module"))
  :hook
  ((lsp-mode-hook . lsp-ui-mode)
   (lsp-mode-hook . linum-mode))
  :bind
  (:map lsp-mode-map
	("C-c C-f" . lsp-find-definition)))

(use-package lsp-ui
  :defer t
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
   (lsp-eldoc-enable-hover nil)
   (lsp-ui-sideline-enable nil)))

(use-package company
  :defer t
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
  :defer t
  :hook
  (company-mode . company-box-mode))

(use-package flycheck
  :defer t
  ;; :hook
  ;; ((emacs-lisp-mode . flycheck-mode))
  )

(use-package flycheck-posframe
  :defer t
  :after flycheck)

(use-package rainbow-delimiters
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :defer t
  :custom
  ((transient-history-file
    (concat user-emacs-directory
	    dk/user-emacs-etcdir
	    "transient/history.el"))))

(use-package tree-sitter
  :defer t
  :config
  (tree-sitter-require 'rust)
  (tree-sitter-require 'python)
  (global-tree-sitter-mode)
  ;;(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  :hook
  ((rustic-mode . tree-sitter-hl-mode)
   (python-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :defer t
  :after tree-sitter)

(use-package yasnippet
  :defer t
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

(use-package yasnippet-snippets
  :defer t)

(use-package eshell
  :ensure nil
  :custom
  ((eshell-directory-name
    (concat user-emacs-directory
	    dk/user-emacs-etcdir
	    "eshell"))))

(use-package treemacs-all-the-icons)

(use-package treemacs
  :custom
  ((treemacs-follow-after-init t)
   (treemacs-width 35)
   (treemacs-indentation 1)
   (treemacs-recenter-after-file-follow nil)
   (treemacs-silent-refresh t)
   (treemacs-silent-filewatch t)
   (treemacs-change-root-without-asking t)
   ;; (treemacs-sorting 'alphabetic-desc)
   (treemacs-show-hidden-files t)
   (treemacs-never-persist nil)
   (treemacs-is-never-other-window t))
  :config
  (setq treemacs-python-executable (executable-find "python"))
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-load-theme "all-the-icons")
  :bind
  (("C-x t" . treemacs-select-window)))

(provide 'programming-base)
