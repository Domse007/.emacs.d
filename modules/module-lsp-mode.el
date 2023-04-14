(use-package lsp-mode
  :defer t
  :custom
  ((lsp-rust-analyzer-cargo-watch-command "clippy")
   (lsp-eldoc-render-all t)
   (lsp-idle-delay 0.6)
   (lsp-rust-analyzer-server-display-inlay-hints t)
   (lsp-session-file (concat dk/user-emacs-cache-dir "lsp/lsp"))
   (lsp-server-install-dir (concat dk/user-emacs-cache-dir "lsp-server/"))
   (lsp-signature-auto-activate t)
   (lsp-signature-render-documentation nil)
   ;; rust
   (lsp-rust-analyzer-display-closure-return-type-hints t)
   (lsp-rust-analyzer-display-parameter-hints t)
   (lsp-rust-analyzer-import-granularity "module"))
  :hook
  ((rustic-mode . lsp)
   (rust-mode . lsp)
   (python-mode . lsp)
   (vue-mode . lsp)
   (dart-mode . lsp)
   (lsp-mode-hook . lsp-ui-mode)
   (lsp-mode-hook . linum-mode))
  :bind
  (:map lsp-mode-map ("C-c C-f" . lsp-find-definition)))

(use-package lsp-ui
  :defer t
  :config
  (lsp-ui-doc-enable t)
  (lsp-ui-mode t)
  :custom
  ((lsp-ui-peek-always-show t)
   (lsp-ui-sideline-show-hover nil)
   (lsp-ui-doc-position 'top)
   ;; (lsp-ui-doc-position nil)
   ;; (lsp-ui-doc-max-width 42)
   ;; (lsp-ui-doc-max-height 30)
   (lsp-eldoc-hook nil)
   (lsp-eldoc-enable-hover nil)
   (lsp-ui-sideline-enable nil)))

(new-external-dependency! 'gdb)

(use-package dap-mode)

(provide 'module-lsp-mode)

