(new-external-dependency! 'grep)
(new-external-dependency! '(ripgrep . "cargo install ripgrep"))

(use-package projectile
  :bind
  (:map projectile-mode-map
	("C-c p" . 'projectile-command-map))
  :config
  (projectile-mode t))

(use-package company
  :defer t
  :bind
  (:map company-active-map
	("C-n". company-select-next)
	("C-p". company-select-previous)
	("M-<". company-select-first)
	("M->". company-select-last))
  :custom
  ((company-tooltip-maximum-width 60)
   (company-tooltip-width-grow-only t)
   (company-idle-delay 0)
   (company-tooltip-idle-delay 0))
  :hook
  ((emacs-lisp-mode . company-mode)
   (prog-mode . company-mode)))

(use-package company-box
  :defer t
  :hook
  (company-mode . company-box-mode))

(use-package prescient)

(use-package company-prescient
  :config
  (company-prescient-mode))

(use-package flycheck
  :defer t
  ;; :hook
  ;; ((emacs-lisp-mode . flycheck-mode))
  )

(use-package flycheck-posframe
  :defer t
  :after flycheck
  :config
  (flycheck-posframe-configure-pretty-defaults))

(use-package rainbow-delimiters
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :defer t
  :custom
  ((transient-history-file
    (expand-file-name dk/user-emacs-cache-dir "transient/history.el"))))

(use-package tree-sitter
  :defer t
  :config
  (tree-sitter-require 'rust)
  (tree-sitter-require 'python)
  (global-tree-sitter-mode)
  ;;(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  :hook
  ((rustic-mode . tree-sitter-hl-mode)
   (python-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :defer t
  :after tree-sitter)

(use-package yasnippet
  :defer t
  :after yasnippet-snippets
  :custom
  ((yas-indent-line 'auto)
   (yas/snippet-dirs `(,yasnippet-snippets-dir)))
  :config
  (yasnippet-snippets-initialize)
  (yas-global-mode 1)
  ;; (yas-reload-all)
  ;; (add-hook 'prog-mode-hook #'yas-minor-mode)
  ;; (yas-global-mode 1)
  ;; (setq yas-prompt-functions '(yas-dropdown-prompt
  ;;                              yas-ido-prompt
  ;;                              yas-completing-prompt))
  )

(use-package yasnippet-snippets
  :defer t)

(use-package eshell
  :ensure nil
  :custom
  ((eshell-directory-name (concat dk/user-emacs-cache-dir "eshell"))))

(use-package eshell-syntax-highlighting
  :after eshell-mode
  :config
  (eshell-syntax-highlighting-global-mode t))

(use-package treemacs-all-the-icons)

(use-package treemacs
  :custom
  ((treemacs-follow-after-init t)
   (treemacs-width 35)
   (treemacs-indentation 1)
   (treemacs-recenter-after-file-follow nil)
   (treemacs-silent-refresh t)
   (treemacs-silent-filewatch t)
   (treemacs-change-root-without-asking t)
   (treemacs-show-hidden-files t)
   (treemacs-never-persist nil)
   (treemacs-is-never-other-window t)
   (treemacs-sorting 'alphabetic-case-insensitive-asc)
   (treemacs-persist-file
    (concat user-emacs-directory dk/user-emacs-cache-dir "treemacs-persist"))
   (treemacs-last-error-persist-file
    (concat dk/user-emacs-cache-dir "treemacs-last-error-persist"))
   (treemacs-collapse-dirs 0))
  :bind
  (("C-x t" . treemacs-select-window)
   ("C-o" . treemacs))
  :config
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-load-theme "all-the-icons")
  (treemacs-git-mode 'simple)
  (treemacs-tag-follow-mode t)
  (remove-hook 'kill-emacs-hook #'treemacs--persist))

(use-package treemacs-projectile)

(use-package sublimity
  :disabled t
  :config
  (require 'sublimity)
  ;; minimap
  (require 'sublimity-map)
  (sublimity-map-set-delay nil)

  (when (boundp 'programming-lsp-mode)
    (add-hook 'lsp-after-open-hook 'sublimity-mode))
  (when (boundp 'programming-eglot)
    (add-hook 'eglot-connect-hook 'sublimity-mode)))

;; Code folding.
(use-package hideshow
  :ensure nil)

;; Org like TAB behaviour for hideshow.
(use-package hideshow-org
  :hook
  ((prog-mode . hs-org/minor-mode)))

(use-package shell-pop
  :custom
  ((shell-pop-full-span t)
   (shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell)))))
  :bind
  (("C-t" . shell-pop)))

(use-package highlight-numbers
  :hook
  ((prog-mode . highlight-numbers-mode)))

(use-package highlight-indent-guides
  :custom
  ((highlight-indent-guides-method 'bitmap))
  :hook
  ((prog-mode . highlight-indent-guides-mode)))

(provide 'programming-base)
