(defvar dk/lsp-proxy-global-node-modules-path
  (if (executable-find "npm")
      (nth 0 (split-string (shell-command-to-string "npm list -g") "\n"))
    (expand-file-name ".nvm/versions/node/v22.15.0/lib" (getenv "HOME"))))

(defvar dk/lsp-proxy-typescript-plugin-path
  (expand-file-name "node_modules/@vue/typescript-plugin"
                    dk/lsp-proxy-global-node-modules-path))

(defvar dk/lsp-proxy-typescript-lib-path
  (expand-file-name "node_modules/typescript/lib"
                    dk/lsp-proxy-global-node-modules-path))

(defconst dk/lsp-proxy-config
  `(("[language-server.typescript-language-server]"
     ("config.plugins" . (("name" . "@vue/typescript-plugin")
			  ("location" . ,dk/lsp-proxy-typescript-plugin-path)
			  ("languages" . [ "vue" ]))))
    ("[language-server.vue-language-server]"
     ("command" . "vue-language-server")
     ("args" . [ "--stdio" ])
     ("config.typescript" . (("tsdk" .  ,dk/lsp-proxy-typescript-lib-path)))
     ("config.vue" . (("hybridMode" . false))))
    ("[[language]]"
     ("name" . "vue")
     ("roots" . [ "package.json" ])
     ("language-id" . "vue")
     ("file-types" . [ "vue" "ts" ])
     ("language-servers" .
      [ "vue-language-server" "typescript-language-server" ]))
    ("[[language]]"
     ("name" . "typescript")
     ("language-id" . "typescript")
     ("file-types" . [ "ts" "mts" "cts" ])
     ("roots" . [ "package.json" ])
     ("language-servers" .
      [(("name" . "typescript-language-server")
	("except-features" . [ "format" ]))
       (("name" . "eslint")
	("support-workspace" . true)
	("config-files" .
	 [ ".eslintrc.js" ".eslintrc.cjs" ".eslintrc.yaml" ".eslintrc.yml"
	   ".eslintrc" ".eslintrc.json" "eslint.config.js" "eslint.config.mjs"
	   "eslint.config.cjs" "eslint.config.ts" "eslint.config.mts"
	   "eslint.config.cts"]))]))))

(defun dk/to-toml--rhs (rhs)
  (cond ((symbolp rhs) (insert (symbol-name rhs)))
	((stringp rhs) (insert "\"" rhs "\""))
	((vectorp rhs)
	 (let ((flag nil))
	   (insert "[ ")
	   (dotimes (i (length rhs))
	     (when flag (insert ", "))
	     (setq flag t)
	     (dk/to-toml--rhs (aref rhs i)))
	   (insert " ]")))
	((proper-list-p rhs)
	 (let ((flag nil))
	   (insert "{ ")
	   (dolist (elem rhs)
	     (when flag (insert ", "))
	     (setq flag t)
	     (dk/to-toml--rhs elem))
	   (insert " }")))
	((consp rhs)
	 (insert (car rhs) " = ")
	 (dk/to-toml--rhs (cdr rhs)))
	(t (error "Unknown rhs type: %s :: %s" rhs (type-of rhs)))))

(defun dk/to-toml (inp file)
  (with-temp-file file
    (dolist (section inp)
      (dolist (entry section)
        (cond ((stringp entry) (insert entry "\n"))
	      ((consp entry)
	       (insert (car entry) " = ")
	       (dk/to-toml--rhs (cdr entry))
	       (insert "\n"))
	      (t (error "Unknown entry type: %s" entry))))
      (insert "\n"))))

(defconst dk/lsp-proxy-config-path
  (expand-file-name "lsp-proxy/languages.toml" dk/user-emacs-cache-dir))

(defun dk/write-lsp-proxy-config (force)
  (interactive (list (y-or-n-p "Force lsp-proxy config rebuild? ")))
  (if (file-exists-p dk/lsp-proxy-config-path)
      (when force
        (delete-file dk/lsp-proxy-config-path)
        (dk/to-toml dk/lsp-proxy-config
                    dk/lsp-proxy-config-path))
    (dk/to-toml dk/lsp-proxy-config dk/lsp-proxy-config-path)))

(use-package lsp-proxy
  :quelpa (lsp-proxy :fetcher github :repo "jadestrong/lsp-proxy" :files ("*"))
  :init
  (let ((default-directory (file-name-directory
			    (locate-library "lsp-proxy"))))
    (unless (file-exists-p (expand-file-name "lsp-proxy" default-directory))
      (shell-command "cargo build --release")
      (copy-file "./target/release/lsp-proxy" "./")))
  (let ((dir (file-name-directory dk/lsp-proxy-config-path)))
    (when (not (file-directory-p dir))
      (make-directory dir)))
  (dk/write-lsp-proxy-config nil)
  :custom
  ((lsp-proxy-user-languages-config dk/lsp-proxy-config-path))
  :hook
  ((rust-mode . lsp-proxy-mode)
   (vue-ts-mode . lsp-proxy-mode)
   (lsp-proxy-mode . (lambda ()
                       (local-set-key (kbd "C-c C-f")
                                      #'lsp-proxy-format-buffer)))))

(provide 'module-lsp-proxy)
