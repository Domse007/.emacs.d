(new-external-dependency! 'flutter)

(use-package dart-mode)

(use-package flutter
  :after dart-mode
  :bind
  (:map dart-mode-map
	("C-M-x" . flutter-run-or-hot-reload))
  :hook
  ((dart-mode . flutter-test-mode)))

(provide 'programming-flutter)
