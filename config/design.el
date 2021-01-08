(use-package humanoid-themes
  :ensure t
  :config
  (load-theme 'humanoid-dark t))

(use-package all-the-icons
  :ensure t
  :if window-system)

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-startup-banner 'logo
	dashboard-show-shortcuts t
	dashboard-center-content t
	dashboard-set-file-icons t
	dashboard-set-heading-icons t
	dashboard-items '((recents  . 15)))
  :config
  (dashboard-setup-startup-hook))

(use-package beacon
  :ensure t
  :custom
  (beacon-color "#FFFFFF")
  :hook (after-init . beacon-mode))

(use-package rainbow-mode
  :ensure t
  :diminish
  :hook (emacs-lisp-mode . rainbow-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package zoom
  :ensure t
  :config
  (custom-set-variables
   '(zoom-mode t)
   '(zoom-size '(120 . 30))
   '(temp-buffer-resize-mode t))
  (zoom-mode nil)
  :bind
  ("C-c z" . zoom-mode))






