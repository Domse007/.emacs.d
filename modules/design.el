(use-package doom-modeline
  :defer t
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
  :defer t
  :custom
  ((dashboard-startup-banner 'logo)
   (dashboard-show-shortcuts t)
   (dashboard-center-content t)
   (dashboard-set-file-icons t)
   (dashboard-set-heading-icons t)
   (dashboard-items '((recents  . 20))))
  :config
  (dashboard-setup-startup-hook))

(use-package all-the-icons
  :defer t
  :if (window-system))

(use-package beacon
  :defer t
  :custom
  ((beacon-color "#FFFFFF"))
  :hook
  ((after-init . beacon-mode)))

(use-package dimmer
  :defer t
  :disabled t
  :config
  (dimmer-configure-helm)
  (dimmer-configure-company-box)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-posframe)
  (dimmer-mode t)
  :custom
  ((dimmer-fraction 0.5)
   (dimmer-buffer-exclusion-regexps '("Treemacs" "\*[.]\*" "*.rs")))
  :hook
  ((prog-mode . (lambda () (dimmer-mode nil)))))

(use-package olivetti
  :defer t
  :custom
  ((olivetti-style 'fancy))
  :hook
  ((org-mode . olivetti-mode)))

(provide 'dk/design)
