;; (use-package humanoid-themes
;;   :ensure t
;;   :config
;;   (load-theme 'humanoid-dark t))
(use-package doom-themes
  :ensure t
  :custom ((doom-themes-enable-bold t)
           (doom-themes-enable-italic t))
  :config
  (load-theme 'doom-molokai t)
  ;; Enable Flashing Mode-Line On Errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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

;; Margins
(defcustom perfect-margin-ignore-regexps
  '("^minibuf" "^[*]" "Minibuf" "[*]" "magit" "mu4e" "*Minibuf-1*")
  "List of strings to determine if window is ignored.
Each string is used as regular expression to match the window buffer name."
  :group 'perfect-margin)

(use-package perfect-margin
  :ensure t
  :config
  (perfect-margin-mode 1))

(defcustom perfect-margin-ignore-filters
  '(window-minibuffer-p)
  "List of functions to determine if window is ignored.
Each function is called with window as its sole arguemnt, returning a non-nil value indicate to ignore the window."
  :group 'perfect-margin)

(defun current-buffer ()
  (interactive)
  (message (buffer-name)))
;; end of perfect margin

(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))
