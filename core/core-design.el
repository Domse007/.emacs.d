(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  ;; (defun my-doom-modeline--font-height ()
  ;;   "Calculate the actual char height of the mode-line."
  ;;   (+ (frame-char-height) 1))
  ;; (advice-add #'doom-modeline--font-height
  ;; 	      :override #'my-doom-modeline--font-height)
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
   (dashboard-items '((recents  . 15)
		      (projects . 5))))
  :bind
  (:map dashboard-mode-map ("q" . nil))
  :config
  (when (eq (length command-line-args) 1)
    (let ((home (getenv "HOME")))
      (if (getenv "HOME")
	  (cd home)
	(cd (concat user-emacs-directory "../"))))
    (dashboard-setup-startup-hook)))

(use-package all-the-icons
  :if (window-system))

(use-package beacon
  :custom
  ((beacon-color "#FFFFFF"))
  :hook
  ((after-init . beacon-mode)))

(use-package dimmer
  ;; :disabled t
  :config
  (dimmer-configure-helm)
  (dimmer-configure-company-box)
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-posframe)
  (dimmer-mode t)
  :custom
  ((dimmer-fraction 0.3)
   (dimmer-buffer-exclusion-regexps '("Treemacs" "\*[.]\*" "*.rs")))
  :hook
  ((prog-mode . (lambda () (dimmer-mode nil)))))

(use-package olivetti
  :custom
  ((olivetti-style nil))
  :hook
  ((org-mode . olivetti-mode)))

(use-package perfect-margin
  :disabled t
  :custom
  ((perfect-margin-visible-width 80))
  :hook
  ((org-mode . perfect-margin-mode)))

;; automatic window balancing.
(use-package zoom
  :disabled t
  :custom
  ((zoom-ignored-buffer-name-regexps '("\\`\\*helm" "\\`\\*org-roam")))
  :config
  (when (window-system)
    (zoom-mode t)))

(use-package humanoid-themes
  :custom
  ((humanoid-comment-italic t)
   (humanoid-org-highlight t))
  :config
  (load-theme 'humanoid-dark t))

(provide 'core-design)
