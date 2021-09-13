(use-package doom-themes
  :custom
  ((doom-themes-enable-bold t)
   (doom-themes-enable-italic t))
  :config
  (load-theme 'doom-molokai t)
  ;; Enable Flashing Mode-Line On Errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package melancholy-theme
  :disabled t
  :config
  (load-theme 'melancholy t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom    
  ((doom-modeline-height 25)
   (doom-modeline-bar-width 1)
   (doom-modeline-icon t)
   (doom-modeline-major-mode-icon t)
   (doom-modeline-major-mode-color-icon t)
   (doom-modeline-buffer-file-name-style 'truncate-upto-project)
   (doom-modeline-buffer-state-icon t)
   (doom-modeline-buffer-modification-icon t)
   (doom-modeline-minor-modes nil)
   (doom-modeline-enable-word-count nil)
   (doom-modeline-buffer-encoding t)
   (doom-modeline-indent-info nil)
   (doom-modeline-checker-simple-format t)
   (doom-modeline-vcs-max-length 12)
   (doom-modeline-env-version t)
   (doom-modeline-irc-stylize 'identity)
   (doom-modeline-github-timer nil)
   (doom-modeline-gnus-timer nil)))

(use-package dashboard
  :custom
  ((dashboard-startup-banner 'logo)
   (dashboard-show-shortcuts t)
   (dashboard-center-content t)
   (dashboard-set-file-icons t)
   (dashboard-set-heading-icons t)
   (dashboard-items '((recents  . 15))))
  :config
  (dashboard-setup-startup-hook))

(use-package all-the-icons
  :if (window-system))

(use-package beacon
  :custom
  ((beacon-color "#FFFFFF"))
  :hook
  ((after-init . beacon-mode)))

;; (use-package neotree
;;   :if (window-system)
;;   :bind
;;   (("C-c C-x n" . neotree-toggle))
;;   :custom
;;   ((neo-theme 'icons 'arrow)))

(use-package treemacs-all-the-icons)

(use-package treemacs
  :after treemacs-all-the-icons
  :config
  (treemacs-load-theme "all-the-icons")
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-project-follow-mode t)
  :custom
  ((treemacs-collapse-dirs 0)
   (treemacs-display-in-side-window t)
   (treemacs-eldoc-display t)
   (treemacs-follow-after-init t)
   (treemacs-expand-after-init t)
   (treemacs-show-hidden-files t)
   (treemacs-width 25)
   (treemacs-is-never-other-window t)))

(use-package dimmer
  :config
  (dimmer-configure-helm)
  (dimmer-configure-company-box)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-posframe)
  (dimmer-mode t)
  :custom
  ((dimmer-fraction 0.5)
   (dimmer-buffer-exclusion-regexps '("Treemacs" "\*[.]\*"))))

(provide 'design.el)
