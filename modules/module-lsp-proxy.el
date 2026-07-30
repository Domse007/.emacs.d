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

(defconst dk/lsp-proxy-cobol-lsp
  (let ((file-base "var/lsp-proxy/cobol/cobol-server"))
    (expand-file-name (if (equal system-type 'windows-nt)
			  (concat file-base ".exe") file-base)
		      user-emacs-directory))
  "Path to the cobol lsp.")

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
	   "eslint.config.cts" ]))]))
    ;; testing for cobol.
    ("[language-server.cobol-language-server]"
     ("command" . ,dk/lsp-proxy-cobol-lsp)
     ("args" . [ "pipeEnabled" "-Dline.separator=\\r\\n"
		 "-Dlogback.statusListenerClass=ch.qos.logback.core.status.NopStatusListener"]))
    ("[[language]]"
     ("name" . "cobol")
     ("language-id" . "cobol")
     ("file-types" . [ "cbl" "cob" "cpy" ])
     ("roots" . [ "Makefile" "settings.json" ])
     ("language-servers" . [ "cobol-language-server" ]))
    ("[language-server.elixir-language-server]"
     ("command" . ,(expand-file-name "language_server.sh" elixir-ls-install-dir))
     ("args" . []))
    ("[language-server.basedpyright]"
     ("command" . "uv")
     ("args" . ["run" "basedpyright-langserver" "--stdio"]))
    ("[[language]]"
     ("name" . "elixir")
     ("language-id" . "elixir")
     ("file-types" . [ "ex" "exs" "eex" "leex" "heex" ])
     ("roots" . [ "mix.exs" ])
     ("language-servers" . [ "elixir-language-server" ]))
    ("[[language]]"
     ("name" . "python")
     ("file-types" . [ "py" "pyi" "py3" "pyw" "ptl" "rpy" "cpy" "ipy" "pyt"
		       (("glob" . ".python_history"))
		       (("glob" . ".pythonstartup"))
		       (("glob" . ".pythonrc"))
		       (("glob" . "SConstruct"))
		       (("glob" . "SConscript")) ])
     ("roots" . ["pyproject.toml" "setup.py" "poetry.lock" "pyrightconfig.json"])
     ("language-servers" . [ "basedpyright" ]))
    ;; JavaScript language server
    ("[language-server.vtsls.config.javascript]"
     ("suggest" . (("completeFunctionCalls" . true)))
     ("format" . (("enable" . false))))
    ("[[language]]"
     ("name" . "javascript")
     ("language-id" . "javascript")
     ("file-types" . ["js" "mjs" "cjs" "rules" "es6" "pac" "jakefile"])
     ("roots" . [ "package.json" ])
     ("language-servers" .
      [(("name" . "vtsls")
	("except-features" . [ "format" ])
	("support-workspace" . [ "package.json" ]))
       (("name" . "eslint")
	("support-workspace" . true)
	("config-files" . [".eslintrc.js" ".eslintrc.cjs" ".eslintrc.yaml"
			   ".eslintrc.yml" ".eslintrc" ".eslintrc.json"
			   "eslint.config.js" "eslint.config.mjs"
			   "eslint.config.cjs" "eslint.config.ts"
			   "eslint.config.mts" "eslint.config.cts"]))]))))



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

(defun dk/write-lsp-proxy-config (&optional auto-reload)
  "Write lsp-proxy TOML configuration file. Always regenerates.
If AUTO-RELOAD is non-nil, restart lsp-proxy after writing config."
  (interactive (list (y-or-n-p "Auto-reload lsp-proxy after rebuild? ")))
  (when (file-exists-p dk/lsp-proxy-config-path)
    (delete-file dk/lsp-proxy-config-path))
  (dk/to-toml dk/lsp-proxy-config dk/lsp-proxy-config-path)
  (message "[LSP-PROXY] Configuration file regenerated")
  (when (and auto-reload (fboundp 'lsp-proxy-restart))
    (lsp-proxy-restart)))

(use-package lsp-proxy
  :quelpa (lsp-proxy :fetcher github :repo "jadestrong/lsp-proxy" :files ("*"))
  :init
  (let ((default-directory (file-name-directory (locate-library "lsp-proxy"))))
    (unless (file-exists-p (expand-file-name "lsp-proxy" default-directory))
      (shell-command "cargo build --release")
      (copy-file "./target/release/emacs-lsp-proxy" "./")))
  (let ((dir (file-name-directory dk/lsp-proxy-config-path)))
    (when (not (file-directory-p dir))
      (make-directory dir)))
  (dk/write-lsp-proxy-config nil)
  :custom
  ((lsp-proxy-user-languages-config dk/lsp-proxy-config-path))
  :bind
  (("C-c C-l p" . lsp-proxy-mode)
   :map lsp-proxy-mode-map
   ("C-c C-f" . lsp-proxy-format-buffer))
  :hook
  ((rust-mode . lsp-proxy-mode)
   (vue-ts-mode . lsp-proxy-mode)
   (web-mode . lsp-proxy-mode)
   (typescript-ts-mode . lsp-proxy-mode)
   (typescript-mode . lsp-proxy-mode)
   (cobol-mode . lsp-proxy-mode)
   (python-mode . lsp-proxy-mode)
   (c++-mode . lsp-proxy-mode)
   (elixir-mode . lsp-proxy-mode)))

(provide 'module-lsp-proxy)
