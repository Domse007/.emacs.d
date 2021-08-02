(use-package doom-themes
  :straight t
  :custom
  ((doom-themes-enable-bold t)
   (doom-themes-enable-italic t))
  :config
  (load-theme 'doom-molokai t)
  ;; Enable Flashing Mode-Line On Errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
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
  :straight t
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
  :straight t
  :if (window-system))

(use-package beacon
  :straight t
  :custom
  ((beacon-color "#FFFFFF"))
  :hook
  ((after-init . beacon-mode)))

(use-package perfect-margin
  :straight t
  :custom
  ((perfect-margin-visible-width 90))
  :config
  (perfect-margin-mode 1)
  :init
  (defcustom perfect-margin-ignore-filters
    '(window-minibuffer-p)
    "List of functions to determine if window is ignored.
Each function is called with window as its sole arguemnt, 
returning a non-nil value indicate to ignore the window."
    :group 'perfect-margin)
  (defcustom perfect-margin-ignore-regexps
    '("^minibuf" "^[*]" "Minibuf" "[*]" "magit" "mu4e" "*Minibuf-1*")
    "List of strings to determine if window is ignored.
Each string is used as regular expression to match the window buffer name."
    :group 'perfect-margin))

(use-package neotree
  :straight t
  :if (window-system)
  :bind
  (("C-c C-x n" . neotree-toggle))
  :custom
  ((neo-theme 'icons 'arrow)))

(provide 'design.el)
