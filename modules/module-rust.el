(new-external-dependency! 'cargo)
(new-external-dependency! 'rustc)

(use-package rustic
  :defer t
  :custom
  ((rustic-format-on-save t)
   (rustic-format-trigger 'on-save)
   (rustic-lsp-client 'eglot))
  :bind
  (:map rustic-mode-map
        ("M-j" . lsp-ui-imenu)
        ("M-?" . lsp-find-references)
        ("C-c C-c l" . flycheck-list-errors)
        ("C-c C-c a" . lsp-execute-code-action)
        ("C-c C-c r" . lsp-rename)
        ("C-c C-c q" . lsp-workspace-restart)
        ("C-c C-c Q" . lsp-workspace-shutdown)
        ("C-c C-c s" . lsp-rust-analyzer-status)))

(use-package rust-auto-use
  :defer t
  :hook
  ((rust-mode-hook . rust-auto-use)))

(use-package toml-mode)

(use-package cargo
  :defer t
  :hook
  (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :defer t
  :after flycheck)

(provide 'module-rust)
