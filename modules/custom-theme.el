(use-package doom-themes
  :custom
  ((doom-themes-enable-bold t)
   (doom-themes-enable-italic t))
  :config
  ;; Enable Flashing Mode-Line On Errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package melancholy-theme)

(use-package humanoid-themes
  :custom
  ((humanoid-comment-italic t)
   (humanoid-org-highlight t)))

(defconst dk/dark-theme 'humanoid-dark
  "Main theme that is loaded by default.")

(defconst dk/light-theme 'modus-operandi
  "Main light theme.")

(defun dk/load-light-theme ()
  "Load the light theme."
  (interactive)
  (load-theme dk/light-theme t))

(defun dk/load-dark-theme ()
  "Load the dark theme."
  (interactive)
  (load-theme dk/dark-theme t))

(dk/load-dark-theme)
