(new-external-dependency! 'rust-analyzer)

(use-package eglot
  :hook
  ((rustic-mode . eglot)
   (rust-mode . eglot)
   (python-mode . eglot)
   (vue-mode . eglot)
   (dart-mode . eglot)))

(provide 'programming-eglot)
