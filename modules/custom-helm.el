(module! custom-helm
  "Module that enables helm and presents it through posframe."
  :depends-on nil
  :conflicts-with (custom-ivy)
  :dir dk/config-optional-path)

(use-package helm
  :custom
  ((helm-move-to-line-cycle-in-source t)
   (helm-ff-search-library-in-sexp t)
   (helm-scroll-amount 8)
   (helm-ff-file-name-history-use-recentf t)
   (helm-echo-input-in-header-line t)
   (helm-split-window-inside-p t))
  :init
  (require 'helm-config)
  (helm-mode t)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-c v" . helm-apropos)
   ("C-c f" . helm-apropos)
   ("C-s" . helm-occur)
   ("C-x b" . helm-mini)
   ("M-y" . helm-show-kill-ring)
   :map helm-map
   ("<tab>" . helm-ff-RET)))

(use-package helm-posframe
  :after helm
  :if window-system
  :custom
  ((helm-posframe-width 120)
   (helm-posframe-border-width 2))
  :config
  (helm-posframe-enable))

(use-package helm-icons
  :after helm
  :if window-system
  :custom
  (helm-icons-provider 'all-the-icons)
  :config
  (helm-icons-enable))

(provide 'custom-helm)
