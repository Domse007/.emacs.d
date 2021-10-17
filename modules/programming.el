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
   (lsp-mode-hook . linum-mode)))

(use-package lsp-ui
  :config
  (lsp-ui-doc-enable t)
  (lsp-ui-mode t)
  :custom
  ((lsp-ui-peek-always-show t)
   (lsp-ui-sideline-show-hover t)
   (lsp-ui-doc-position 'top)
   (lsp-ui-doc-max-width 32)
   (lsp-ui-doc-max-height 128)
   (lsp-eldoc-hook nil)))

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

(use-package magit)

(use-package tree-sitter
  :config
  (tree-sitter-require 'rust)
  (tree-sitter-require 'python)
  (global-tree-sitter-mode)
  :hook
  ((tree-sitter-after-on-hook . tree-sitter-hl-mode)
   (tree-sitter-mode-hook . tree-sitter-hl-mode)))

(use-package tree-sitter-langs)

(provide 'dk/programming)
