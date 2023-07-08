(new-external-dependency! 'cargo)
(new-external-dependency! 'rustc)

;; (use-package rustic
;;   :defer t
;;   :custom
;;   ((rustic-format-on-save t)
;;    (rustic-format-trigger 'on-save)
;;    (rustic-lsp-client 'lspce)))

(use-package rust-mode)

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
