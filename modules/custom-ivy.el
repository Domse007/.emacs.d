(use-package counsel
  :hook
  ((after-init . ivy-mode)
   (ivy-mode . counsel-mode))
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("M-y" . counsel-yank-pop)
   ("C-c l" . counsel-git-log))
  :custom
  ((counsel-yank-pop-height 15)
   (enable-recursive-minibuffers t)
   (ivy-use-selectable-prompt t)
   (ivy-use-virtual-buffers t)
   (ivy-on-del-error-function nil)
   (swiper-action-recenter t)
   (counsel-grep-base-command "ag -S --noheading --nocolor --nofilename --numbers '%s' %s")
   (enable-recursive-minibuffers t)
   (search-default-mode #'char-fold-to-regexp)
   (ivy-posframe-height-alist '((swiper . 20)
				(t      . 15)))))

(use-package consult)

(use-package ivy
  :bind
  (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode t))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode t))

(use-package amx
  :after ivy
  :custom
  ((amx-backend 'auto)
   (amx-save-file (concat dk/user-emacs-cache-dir "amx-items"))
   (amx-history-length 50)
   (amx-show-key-bindings nil))
  :config
  (amx-mode))

(use-package all-the-icons-ivy-rich
  :after ivy
  :custom
  (all-the-icons-ivy-rich-mode t))

(use-package swiper
  :bind
  (("C-s" . swiper)))

(use-package ivy-posframe
  :after ivy
  :hook
  ((ivy-mode . ivy-posframe-mode))
  :custom
  ((ivy-posframe-parameters '((left-fringe . 4)
			      (right-fringe . 4)))
   (ivy-posframe-display-functions-alist
    '((t . ivy-posframe-display-at-frame-center)))
   (ivy-posframe-border-width 2)
   (ivy-wrap t))
  :config
  (ivy-posframe-mode 1))

(provide 'custom-ivy)
