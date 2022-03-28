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

(defconst dk/dark-theme 'doom-old-hope ;;'doom-1337 ;;'humanoid-dark
  ;; others: doom-monokai-spectrum
  "Main theme that is loaded by default.")

(defconst dk/light-theme 'modus-operandi
  "Main light theme.")

(defun dk/load-theme (theme)
  "Function that loads a theme."
  (dk/log (format "Loading theme %s" theme) 'info)
  (load-theme theme t)
  (dk/log (format "Loaded %s" theme) 'info))

(defun dk/load-light-theme ()
  "Load the light theme."
  (interactive)
  (dk/load-theme dk/light-theme))

(defun dk/load-dark-theme ()
  "Load the dark theme."
  (interactive)
  (dk/load-theme dk/dark-theme))

;; load the default theme.
(dk/load-dark-theme)

(provide 'custom-theme)
