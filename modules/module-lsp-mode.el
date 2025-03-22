(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook ((rust-mode . lsp)
	 (cobol-mode . lsp))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package dap-mode)
(use-package dap-rust)

(provide 'module-lsp-mode)
