(new-external-dependency! 'cargo)
(new-external-dependency! 'rustc)

(use-package rust-mode)

(use-package rust-auto-use
  :disabled t
  :hook
  ((rust-mode-hook . rust-auto-use)))

(use-package toml-mode)

(use-package cargo
  :hook
  (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :after flycheck)

(provide 'module-rust)
