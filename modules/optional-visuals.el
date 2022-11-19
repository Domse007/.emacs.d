(use-package which-key
  :config
  (which-key-setup-side-window-right)
  (which-key-mode))

(use-package which-key-posframe
  :custom
  ((which-key-posframe-poshandler 'posframe-poshandler-frame-top-center))
  :config
  (which-key-posframe-mode))

;; better *help* buffer.
(use-package helpful
  :disabled t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))

(provide 'optional-visuals)
