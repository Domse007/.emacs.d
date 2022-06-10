(module! programming-haskell
  "Module that enables haskell programming."
  :depends-on nil
  :conflicts-with nil
  :dir dk/config-optional-path)

(use-package haskell-mode
  :defer t)

(provide 'programming-haskell)
