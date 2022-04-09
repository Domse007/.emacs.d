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

(defvar dk/theme 'doom-old-hope ;;'doom-1337 ;;'humanoid-dark
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
