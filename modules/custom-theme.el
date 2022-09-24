(use-package doom-themes
  :defer t
  :custom
  ((doom-themes-enable-bold t)
   (doom-themes-enable-italic t))
  :config
  ;; Enable Flashing Mode-Line On Errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package humanoid-themes
  :custom
  ((humanoid-comment-italic t)
   (humanoid-org-highlight t)))

(use-package moe-theme
  :custom
  ((moe-theme-set-color 'magenta)
   (moe-theme-resize-org-title '(2.2 1.8 1.6 1.4 1.2 1.0 1.0 1.0 1.0)))
  :config
  ;;(moe-dark)
  )

(defvar dk/theme 'humanoid-dark ;;'doom-old-hope ;;'doom-1337 ;;'humanoid-dark
  ;; others: doom-monokai-spectrum
  "Main theme that is loaded by default.")

(defun dk/load-theme ()
  "Function that loads a theme."
  (let* ((theme dk/theme)
         (name (symbol-name theme)))
    (dk/log 'info "Loading theme " name)
    (load-theme theme t)
    (dk/log 'info "Loaded " name)))

(provide 'custom-theme)
