(use-package helm
  :custom
  ((helm-move-to-line-cycle-in-source t)
   (helm-ff-search-library-in-sexp t)
   (helm-scroll-amount 8)
   (helm-ff-file-name-history-use-recentf t)
   (helm-echo-input-in-header-line t)
   (helm-split-window-inside-p t)
   (helm-display-buffer-default-height 25)
   (helm-candidate-number-limit 50)
   (helm-ff-newfile-prompt-p nil)
   (helm-ff-skip-boring-files t))
  :init
  (require 'helm-config)
  (helm-mode t)
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*helm" (* not-newline) "*" eos)
		 (display-buffer-in-side-window)
		 (inhibit-same-window . t)
		 (window-height . 0.4)))
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

(use-package helm-flx
  :after helm
  :custom
  ((helm-flx-for-helm-find-files t)
   (helm-flx-for-helm-locate t))
  :config
  (helm-flx-mode t))

(use-package helm-posframe
  :disabled t
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
