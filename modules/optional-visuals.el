(use-package which-key
  :config
  (which-key-setup-side-window-right)
  (which-key-mode))

(use-package which-key-posframe
  :custom
  ((which-key-posframe-poshandler 'posframe-poshandler-frame-top-center))
  :config
  (which-key-posframe-mode))

(provide 'optional-visuals)
