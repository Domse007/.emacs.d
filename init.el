;; Logging
;;------------------------------------------------------------------------------

(defmacro dk/log (kind fmt &rest strings)
  "New version of logging macro. Log a message. It will report to the
minibuffer. The history is available in the *Messages* buffer."
  `(let ((pre (cond ((eq ,kind 'info) "[INFO]")
                    ((eq ,kind 'warning) "[WARNING]")
                    ((eq ,kind 'error) "[ERROR]"))))
     (message "%s %s" pre (format ,fmt ,@strings))))

;; Versioning
;;------------------------------------------------------------------------------

(defconst dk/config-version '(0 6 3)
  "The version of this config as a list.")

(defun dk/config-version-string ()
  "Get the config version as a string."
  (format "%i.%i.%i" (nth 0 dk/config-version)
          (nth 1 dk/config-version) (nth 2 dk/config-version)))

(defconst dk/minimal-emacs-version "27.1"
  "Minimal version of emacs to run this config.")

(defun dk/check-emacs-version ()
  "Check the emacs version if the config and emacs are compatible."
  (if (version<= dk/minimal-emacs-version emacs-version)
      (dk/log 'info "Emacs does satisfy version requirement.")
    (error "Emacs does not satisfy version requirement.")))

(dk/check-emacs-version)

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

(defconst dk/macos-flag
  (progn (dk/log 'info "Detected macos. Setting variable...")
         (string-equal system-type "darwin"))
  "Flag that is set, if the host is darwin like.")

(defcustom dk/user-system-base-path (expand-file-name "~/")
  "Selected path at startup"
  :type 'string
  :group 'dk/config)

(defcustom dk/org-roam-dir (expand-file-name "~/Notes/")
  "Default directory of org files that should be indexed by roam."
  :type 'string
  :group 'dk/config)

(defcustom dk/use-40-percent-keyboard nil
  "Flag that specifies if 40% keyboard specific keybindings should be enabled."
  :type 'bool
  :group 'dk/config)

(defconst dk/user-emacs-cache-dir
  (let ((dir (expand-file-name "var/" user-emacs-directory)))
    (unless (file-directory-p dir) (make-directory dir)) dir)
  "Default location for device specific files.")

(defconst dk/config-git-remote
  (let ((default-directory user-emacs-directory)
        (cmd "git config --get remote.origin.url"))
    (string-trim (shell-command-to-string cmd)))
  "Remote URL of the config.")

;; Font selecting
;;------------------------------------------------------------------------------

(defun dk/select-default-font ()
  "Select a font and return it."
  (let* ((fonts '("Source Code Pro" "Consolas" "DejaVu Sans Mono" "SF Mono"))
         (filtered (seq-filter (lambda (f) (if (find-font (font-spec :name f))
                                               f nil))
                               fonts)))
    (if (length> filtered 0) (car filtered) nil)))

(defvar dk/default-font (dk/select-default-font)
  "The default font that will be used.")

;; Tracking of external dependencies
;;------------------------------------------------------------------------------

(require 'cl-macs)

(defvar dk/external-dependencies nil
  "List of external programs that are used with this config.")

(defun dk/type-equal (obj type)
  "Assert that OBJ is of type TYPE."
  (cl-assert (eq (type-of obj) type)))

(defun new-external-dependency! (program &optional nocheck)
  "Add a new external program to `dk/external-dependencies'. It is either a
symbol or a cons. If it is a symbol, it is just the name of the dependency. If
it is a cons cell, the car is the same symbol, but the cdr is a string with
installation instructions."
  (if (consp program)
      (progn (dk/type-equal (car program) 'symbol)
             (dk/type-equal (cdr program) 'string))
    (dk/type-equal program 'symbol))
  (unless (member program dk/external-dependencies)
    (push (cons program nocheck) dk/external-dependencies)))

(when (and dk/windows-flag
           (executable-find "winget"))
  (new-external-dependency! 'winget) ;; by default included in W11
  (new-external-dependency! '(msys2 . "winget install msys2") t))

;; Modules
;;------------------------------------------------------------------------------

(defconst dk/config-core-path (expand-file-name "core/" user-emacs-directory)
  "Location where the core files are located.")

(defconst dk/config-optional-path (expand-file-name "modules/" user-emacs-directory)
  "Default location of config files.")

(add-to-list 'load-path dk/config-core-path)
(add-to-list 'load-path dk/config-optional-path)

(defconst dk/modules
  '((early-init             root    nil "The early-init file.")
    (init                   root    nil "The main init file.")
    (core-use-package       core    t   "Setup of use-package")
    (core-config            core    t   "Setup of invisible packages")
    (core-emacs             core    t   "Setup of built-in things.")
    (core-design            core    t   "Definitions of visible related packages.")
    (core-funcs             core    t   "General custom funcs.")
    (core-daemon            core    t   "Setup hooks for frame creation with daemon.")
    (custom-search          modules t   "Module that provides an interface to search through modules.")
    (custom-helm            modules t   "Module that enables helm and presents it through posframe.")
    (text-org-mode          modules t   "Module that defines basic org setup.")
    (text-org-spell         modules nil "Module that enables spell checking in org.")
    (text-org-roam          modules t   "Module that enables org-roam.")
    (module-spell           modules t   "Spell checking.")
    (programming-eglot      modules t   "Module that enables the eglot client for LSP.")
    (programming-lsp-mode   modules t   "Module that enables the lsp-mode client for LSP.")
    (programming-base       modules t   "Module that defines basics for programming.")
    (programming-rust       modules t   "Module that uses lsp to create a great rust environment.")
    (programming-elisp      modules t   "Module that simplifies elisp programming.")
    (programming-python     modules t   "Module that provides a better python workflow.")
    (programming-haskell    modules t   "Module that enables haskell programming.")
    (programming-vue        modules t   "Module for vue.js programming.")
    (programming-flutter    modules t   "Module for flutter / dart programming.")
    (optional-visuals       modules t   "Module that enables more visual packages.")
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
        (dk/log 'info "Loading file %s from %s."
                (symbol-name name) (symbol-name location))
        (require name)))))

;; machine specific settings
;;------------------------------------------------------------------------------

(defconst dk/custom-settings-file (expand-file-name "~/.emacs-customs.el")
  "File where the machine specific settings are stored.")

(defun dk/load-customs-file ()
  "Function that loads the machine specific settings."
  (if (file-exists-p dk/custom-settings-file)
      (progn (dk/log 'info "Loading customs file from %s."
                     dk/custom-settings-file)
             (load-file dk/custom-settings-file))
    (dk/log 'error "Customs file could not be loaded from %s."
            dk/custom-settings-file)))

(defun dk/install-customs-file ()
  (interactive)
  (let ((settings
         `(("dk/user-system-base-path" . ,dk/user-system-base-path)
           ("dk/org-roam-dir" . ,dk/org-roam-dir)
           ("user-full-name" . ,user-full-name)
           ("dk/use-40-percent-keyboard" . ,dk/use-40-percent-keyboard))))
    (with-temp-file dk/custom-settings-file
      (dolist (item settings)
        (let* ((var-name (car item))
               (var-val (cdr item))
               (val-string (cond ((stringp var-val)(concat "\"" var-val "\""))
                                 (t (if var-val "t" "nil")))))
          (dk/log 'info "Getting %s for %s with type %s."
                  val-string var-name (symbol-name (type-of var-val)))
          (insert "(setq " var-name " " val-string ")\n"))))
    (dk/log 'info "Settings file has been installed.")))

(defun dk/open-customs-file ()
  "Open the file defined in `dk/custom-settings-file'."
  (interactive)
  (if (file-exists-p dk/custom-settings-file)
      (progn (find-file dk/custom-settings-file)
             (dk/log 'info "Opened customs file."))
    "Consider calling `M-x dk/install-customs-file RET.'"))

(global-set-key (kbd "C-c RET") 'dk/open-customs-file)

;; init functions
;;------------------------------------------------------------------------------

(defun dk/load-config ()
  "Init function for the config."
  (when dk/windows-flag
    (setq no-native-compile t
          package-native-compile nil))
  (dk/load-customs-file)
  (dk/load-modules)
  (dk/40-percent-keyboard-mode-maybe-enable)
  (dk/log 'info "Config loaded.")  
  (when dk/original-gc-threshold
    (dk/log 'info "Restoring old gc values.")
    (setq gc-cons-threshold dk/original-gc-threshold))) ; old gc value

;; Call the entry point of the config.
(dk/load-config)

(provide 'init)
