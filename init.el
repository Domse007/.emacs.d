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

;; Modules
;;------------------------------------------------------------------------------

(defconst dk/config-core-path (expand-file-name "core/" user-emacs-directory)
  "Location where the core files are located.")

(defconst dk/config-optional-path (expand-file-name "modules/" user-emacs-directory)
  "Default location of config files.")

(add-to-list 'load-path dk/config-core-path)
(add-to-list 'load-path dk/config-optional-path)

(defconst dk/modules
  '((early-init       root    nil "The early-init file.")
    (init             root    nil "The main init file.")
    (core-use-package core    t   "Setup of use-package")
    (core-deps        core    t   "Setup of dependency management.")
    (core-config      core    t   "Setup of invisible packages")
    (core-emacs       core    t   "Setup of built-in things.")
    (core-design      core    t   "Definitions of visible related packages.")
    (core-funcs       core    t   "General custom funcs.")
    (core-daemon      core    t   "Setup hooks for frame creation with daemon.")
    (module-search    modules t   "Search through config modules.")
    (module-helm      modules t   "Configures helm and posframe.")
    (module-org-mode  modules t   "Definition of org setup.")
    (module-org-roam  modules t   "Definition of org-roam setup.")
    (module-spell     modules nil "Global spell checking.")
    (module-prog-base modules t   "Universal configs for programming.")
    (module-lspce     modules t   "A better lsp client.")
    (module-rust      modules t   "Configs for rust programming.")
    (module-elisp     modules t   "Configs for better elisp programming.")
    (module-haskell   modules t   "Basic setup for haskell programming.")
    (module-vue       modules t   "Very basic vue setup.")
    (module-flutter   modules t   "Flutter / dart setup.")
    (module-visuals   modules t   "More visual packages."))
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


;; Tracking of external dependencies
;;------------------------------------------------------------------------------

;; Should be declared in core-use-package but function is not yet defined.
(new-external-dependency! 'git)
(when dk/windows-flag
  (new-external-dependency! 'winget)
  (new-external-dependency! '(msys2 . "winget install msys2") t))

(provide 'init)
