(module! programming-eglot
  "Module that enables eglot - an lsp-client."
  :depends-on nil
  :conflicts-with (programming-lsp-mode programming-lsp-brigde)
  :dir dk/config-optional-path)

(use-package eglot
  :hook
  ((rustic-mode . eglot)
   (rust-mode . eglot)
   (python-mode . eglot)))

(provide 'programming-eglot)
