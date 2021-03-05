;;; design.el - This is a configuration to make Emacs look nicer

;; commentary:
;; - The code doesen't depend on any additional binaries.
;; - all-the-icons has to install a custom font
;;   - Has to be installed with `all-the-items-install-fonts'
;; - The packages get installed by use-package

;; disabled alternative dark theme
;; (use-package humanoid-themes
;;   :ensure t
;;   :config
;;   (load-theme 'humanoid-dark t))

;; Currently used dark theme. It is not loaded, if the light theme
;; is loaded.
(use-package doom-themes
  :ensure t
  :if (not dk/theme-light-choice)
  :custom ((doom-themes-enable-bold t)
           (doom-themes-enable-italic t))
  :config
  (load-theme 'doom-molokai t)
  ;; Enable Flashing Mode-Line On Errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Currently used light theme. It is loaded, when the "--light"
;; command line argument is set.
(use-package plan9-theme
  :ensure t
  :if dk/theme-light-choice
  :config (load-theme 'plan9 t))

;; Package to load a lot of nice looking icons. Depends on a custom
;; font package.
(use-package all-the-icons
  :ensure t
  :if window-system)

;; Package to have a nice dashboard on start.
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

;; Package to highlight the point if the point moves a big distance.
(use-package beacon
  :ensure t
  :custom
  (beacon-color "#FFFFFF")
  :hook (after-init . beacon-mode))

;; Package to provide a better looking, less cluttered modeline.
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Package to make the active buffer bigger in size
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

;; Package to center the content of a buffer. I couldn't get it work if I put
;; everything in the use-package command.
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

;; Package to replace ascii emojis with normal emojis. ( :) -> =) )
(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))

(provide 'design.el)
;;; design.el ends here
