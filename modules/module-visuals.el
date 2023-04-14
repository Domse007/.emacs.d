(use-package which-key
  :disabled t
  :config
  (which-key-setup-side-window-right)
  (which-key-mode))

(use-package which-key-posframe
  :disabled t
  :custom
  ((which-key-posframe-poshandler 'posframe-poshandler-frame-top-center))
  :config
  (which-key-posframe-mode))

;; better *help* buffer.
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))

(provide 'module-visuals)
