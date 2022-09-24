(use-package eglot
  :hook
  ((rustic-mode . eglot)
   (rust-mode . eglot)
   (python-mode . eglot)))

(provide 'programming-eglot)
