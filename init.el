;; Macros
;;------------------------------------------------------------------------------

(defmacro dk/log (kind &rest msg)
  "Log a message. It will report to the minibuffer. The history is available in
the *Messages* buffer."
  `(cond ((eq ,kind 'info)
          (message (concat "[INFO] " ,@msg)))
         ((eq ,kind 'warning)
          (message (concat "[WARNING] " ,@msg)))
         ((eq ,kind 'error)
          (message (concat "[ERROR] " ,@msg)))))

;; Variables
;;------------------------------------------------------------------------------

(defgroup dk/config nil
  "Group for all custom config variables.")

(defconst dk/windows-flag
  (progn (dk/log 'info "Detected Windows. Setting variable...")
         (string-equal system-type "windows-nt"))
  "Flag that is set, if the host is a windows-nt kernel.")

(defconst dk/linux-flag
  (progn (dk/log 'info "Detected Linux. Setting variable...")
         (string-equal system-type "gnu/linux"))
  "Flag that is set, if the host has a linux kernel.")

(defcustom dk/user-system-base-path ""
  "Selected path at startup"
  :type 'string
  :group 'dk/config)

(defcustom dk/org-roam-dir ""
  "Default directory of org files that should be indexed by roam."
  :type 'string
  :group 'dk/config)

(defcustom dk/use-40-percent-keyboard nil
  "Flag that specifies if 40% keyboard specific keybindings should be enabled."
  :type 'bool
  :group 'dk/config)

(defconst dk/default-font "Source Code Pro"
  "The default font that will be used.")

(defconst dk/user-emacs-cache-dir (expand-file-name "var/" user-emacs-directory)
  "Default location for device specific files.")

;; Tracking of external dependencies
;;------------------------------------------------------------------------------

(defvar dk/external-dependencies nil
  "List of external programs that are used with this config.")

(defun new-external-dependency! (program)
  "Add a new external program to `dk/external-dependencies'. It is either a
symbol or a cons. If it is a symbol, it is just the name of the dependency. If
it is a cons cell, the car is the same symbol, but the cdr is a string with
installation instructions."
  (unless (member program dk/external-dependencies)
    (push program dk/external-dependencies)))

;; Modules
;;------------------------------------------------------------------------------

(defconst dk/config-core-path (expand-file-name "core/" user-emacs-directory)
  "Location where the core files are located.")

(defconst dk/config-optional-path (expand-file-name "modules/" user-emacs-directory)
  "Default location of config files.")

(add-to-list 'load-path dk/config-core-path)
(add-to-list 'load-path dk/config-optional-path)

(defconst dk/modules
  '((early-init             root   nil "The early-init file.")
    (init                   root   nil "The main init file.")
    (base-use-package       core   t   "Setup of use-package")
    (base-version           core   t   "Definitions of all version variables and functions") ; possibly useless
    (base-config            core   t   "Setup of invisible packages")
    (base-emacs             core   t   "Setup of built-in things.")
    (base-design            core   t   "Definitions of visible related packages.")
    (base-funcs             core   t   "General custom funcs.")
    (custom-search          module t   "Module that provides an interface to search through modules.")
    (custom-helm            module t   "Module that enables helm and presents it through posframe.")
    (custom-ivy             module nil "Module that enables ivy and presents it through posframe.")
    (custom-theme           module t   "Module that loads the desired theme.")
    (text-org-mode          module t   "Module that defines basic org setup.")
    (text-org-spell         module t   "Module that enables spell checking in org.")
    (text-org-roam          module t   "Module that enables org-roam.")
    (programming-base       module t   "Module that defines basics for programming.")
    (programming-lsp-mode   module t   "Module that simplifies elisp programming.")
    (programming-lsp-eglot  module nil "Module that enables eglot - an lsp-client.")
    (programming-lsp-bridge module nil "Wrapper around the package lsp-bridge")
    (programming-rust       module t   "Module that uses lsp to create a great rust environment.")
    (programming-elisp      module t   "Module that simplifies elisp programming.")
    (programming-python     module t   "Module that provides a better python workflow.")
    (programming-haskell    module t   "Module that enables haskell programming.")
    (optional-visuals       module t   "Module that enables more visual packages.")
    )
  "All modules that can be loaded. The first element is the module name. The
second element is the location of the module. The third element is the arg if
the module should be loaded. The fourth element is the description of the
module. `early-init' and `init' must be nil.")

(defun dk/load-modules ()
  (dolist (module-cons dk/modules)
    (let ((name (nth 0 module-cons))
          (location (nth 1 module-cons))
          (arg (nth 2 module-cons)))
      (when arg
        (dk/log 'info "Loading file " (symbol-name name) " from "
                (symbol-name location) ".")
        (require name)))))

;; init functions
;;------------------------------------------------------------------------------

(defun dk/load-config ()
  (when (daemonp)
    (require 'early-init))
  
  (dk/load-modules)

  (dk/load-theme)
  
  (dk/40-percent-keyboard-mode-maybe-enable)
  (dk/log 'info "Config loaded.")
  (setq gc-cons-threshold dk/original-gc-threshold)) ; old gc value

;; Call the entry point of the config.
(dk/load-config)

(provide 'init)
