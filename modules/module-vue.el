;; External Dependencies
;;------------------------------------------------------------------------------

(new-external-dependency! '(vue-language-server . "npm install -g @vue/language-server"))
(new-external-dependency! '(vue-typescript-plugin . "npm install -g @vue/typescript-plugin"))
(new-external-dependency! '(typescript . "npm install -g typescript"))

;; Tree-sitter Language Sources
;;------------------------------------------------------------------------------

(use-package treesit-auto
  :hook (on-first-input . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; Configure language sources for tree-sitter grammars
  (add-to-list 'treesit-language-source-alist
	       '(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")))
  (add-to-list 'treesit-language-source-alist
	       '(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")))
  (add-to-list 'treesit-language-source-alist
	       '(vue . ("https://github.com/ikatyang/tree-sitter-vue")))
  (add-to-list 'treesit-language-source-alist
	       '(css . ("https://github.com/tree-sitter/tree-sitter-css")))
  (add-to-list 'treesit-language-source-alist
	       '(javascript . ("https://github.com/tree-sitter/tree-sitter-javascript")))
  (add-to-list 'treesit-language-source-alist
	       '(scss . ("https://github.com/serenadeai/tree-sitter-scss"))))

;; TypeScript Mode with Tree-sitter
;;------------------------------------------------------------------------------

(use-package typescript-mode
  :mode "\\.ts\\'"
  :custom
  (typescript-indent-level 2))

(use-package typescript-ts-mode
  :ensure nil  ; Built-in to Emacs 29+
  :mode "\\.ts\\'"
  :custom
  (typescript-ts-mode-indent-offset 2))

;; Vue Mode with Tree-sitter (Primary)
;;------------------------------------------------------------------------------

(use-package vue-ts-mode
  :quelpa
  (vue-ts-mode :fetcher github :repo "8uff3r/vue-ts-mode")
  :mode "\\.vue\\'"
  :hook
  (vue-ts-mode . (lambda ()
                   (setq-local tab-width 2)
                   (setq-local indent-tabs-mode nil))))

;; Web Mode (Fallback if tree-sitter-vue fails)
;;------------------------------------------------------------------------------

(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-script-padding 0)
  (web-mode-style-padding 0)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-current-element-highlight t)
  :config
  (add-to-list 'web-mode-engines-alist '("vue" . "\\.vue\\'")))

;; Legacy Vue Mode (Disabled - use web-mode as fallback instead)
;;------------------------------------------------------------------------------

(use-package vue-mode
  :disabled t
  :config
  (custom-set-faces '(mmm-default-submode-face ((t nil)))))

;; Emmet for Vue Templates
;;------------------------------------------------------------------------------

(use-package emmet-mode
  :hook
  ((vue-ts-mode . emmet-mode)
   (web-mode . emmet-mode))
  :custom
  (emmet-move-cursor-between-quotes t))

;; Prettier for Formatting
;;------------------------------------------------------------------------------

(use-package prettier-js
  :hook
  ((vue-ts-mode . prettier-js-mode)
   (web-mode . (lambda ()
                 (when (string-equal (file-name-extension buffer-file-name) "vue")
                   (prettier-js-mode))))
   (typescript-ts-mode . prettier-js-mode)
   (typescript-mode . prettier-js-mode))
  :custom
  (prettier-js-args '("--single-quote"
                     "--trailing-comma" "es5"
                     "--print-width" "100")))

;; Environment Setup
;;------------------------------------------------------------------------------

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(provide 'module-vue)
