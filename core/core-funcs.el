;; Personal additions to predefined functions
;;------------------------------------------------------------------------------

(defun dk/kill-word-at-point ()
  "Kill the word where the point is pointing at."
  (interactive)
  (let ((range (bounds-of-thing-at-point 'symbol)))
    (kill-region (- (car range) 1) (cdr range))))

(global-set-key (kbd "M-k") 'dk/kill-word-at-point)

;; Play a random game
;;------------------------------------------------------------------------------

(defconst dk/games '(gomoku tetris hanoi 5x5 blackbox bubbles dunnet life)
  "List of included games.")

(defun dk/play-a-game (&optional game)
  "Randomly play a game. The optional GAME argument specifies
a predefined game."
  (interactive)
  (if game
      (call-interactively game)
    (let* ((game-list-len (length dk/games))
	   (game-index (random game-list-len))
	   (game-to-be-played (nth game-index dk/games)))
      (call-interactively game-to-be-played))))

;; Aliasing for updating packages. It also checks that it has not been
;; introduced into package.el
;;------------------------------------------------------------------------------

(require 'auto-package-update)

(defun aliased-p (fn)
  "Return non-nil if function FN is aliased to a function symbol."
  (let ((val (symbol-function fn)))
    (and val                            ; `nil' means not aliased
         (symbolp val))))

(let ((fn 'package-update-packages))
  (when (and (fboundp fn)
             (not (aliased-p fn)))
    (error "This function has been introduced into package.el.")))

(defalias 'package-update-packages 'auto-package-update-now
  "Alias for me to remember how the command is actually called.")

;; Open working directory in file explorer
;;------------------------------------------------------------------------------

(defun dk/explorer ()
  "Open the current directory in the file explorer."
  (interactive)
  (let ((inhibit-message t))
    (cond (dk/windows-flag (shell-command "explorer ."))
	  (dk/linux-flag (shell-command "xdg-open ."))
          (dk/macos-flag (shell-command "open ."))
	  (t (dk/log 'error "This command is not supported on this platform.")))))

(unless (fboundp 'explorer)
  (defalias 'explorer 'dk/explorer "Make it look like it's built in."))

;; Org (roam) export helpers
;;------------------------------------------------------------------------------

(require 'org)

(defvar org-export-output-directory-prefix (expand-file-name "~/.export")
  "Prefix of directory used for org-mode export")

(defadvice org-export-output-file-name (before org-add-export-dir activate)
  "Modifies org-export to place exported files in a different directory"
  (when (not pub-dir)
    (setq pub-dir org-export-output-directory-prefix)
    (when (not (file-directory-p pub-dir))
      (make-directory pub-dir))))

(org-link-set-parameters "id" :export #'dk/org-id-link-export)

(defun dk/org-id-link-export (link description format _)
  "Custom formatting org org-roam links in export."
  description)

;; Support for 40% keyboards
;;------------------------------------------------------------------------------

(define-minor-mode dk/40-percent-keyboard-mode
  "Mode that redefines certain keybindings to support 40% keyboards."
  :group 'dk/config
  :global t
  :lighter " dk/40"
  :keymap `((,(kbd "C-x q") . delete-other-windows)
	    (,(kbd "C-x w") . split-window-below)
	    (,(kbd "C-x e") . split-window-right)
	    (,(kbd "C-x p") . dk/delete-window)))

(defun dk/40-percent-keyboard-mode-maybe-enable ()
  "Enable `dk/40-percent-keyboard-mode' when `dk/use-40-percent-keyboard'
is `t'"
  (when dk/use-40-percent-keyboard
    (progn (dk/log 'info "Enabling 40 percent keyboard mode.")
	   (dk/40-percent-keyboard-mode))))

;; Checking for external dependencies
;;------------------------------------------------------------------------------

(defun dk/check-external-deps ()
  "Check if external programs are available."
  (interactive)
  (let ((missing-alist nil))
    (dolist (program dk/external-dependencies)
      (let* ((program-cons? (car program))
             (program-str (if (consp program-cons?)
                              (symbol-name (car program-cons?))
                            (symbol-name program-cons?))))
        (when (not (cdr program)) ; do not check if collection of packages.
	  (progn (dk/log 'info (format "Checking %s..." program-str))
                 (unless (executable-find program-str)
	           (push missing-alist program))))))
    (if (not missing-alist)
	(dk/log 'info "No missing dependencies.")
      (dk/log 'error "Missing following dependencies: "
	      (substring (format "%s" missing-alist) 1 -1)))))

(defun dk/describe-external-dependency ()
  "Report the installation process for an external dependency."
  (interactive)
  (let* ((list-of-syms (mapcar (lambda (thing) (if (consp thing) (car thing) thing))
                               dk/external-dependencies))
         (user-input (completing-read "Dependency: " list-of-syms nil t)))
    (dolist (dep dk/external-dependencies)
      (let ((data (car dep)))
        (cond ((consp data)
               (let ((name (symbol-name (car data)))
                     (instr (cdr data)))
                 (when (string-equal user-input name)
                   (message "%s can be installed with the following command: %s"
                            name instr))))
              ((symbolp data)
               (when (string-equal user-input (symbol-name data))
                 (message "No instructions are available.")))
              (t (dk/log 'error (format "%s has a wrong type." data))))))))

;; Ask if prefer suspending instead of killing
;;------------------------------------------------------------------------------

(defun dk/maybe-suspend-else-kill (orig-fun &rest args)
  (cond ((display-graphic-p) (apply orig-fun args))
        (dk/windows-flag (apply orig-fun args))
        (t (if (y-or-n-p "Do you want to supend emacs?")
               (suspend-emacs)
             (let ((confirm-kill-emacs nil))
               (apply orig-fun args))))))

(advice-add 'save-buffers-kill-terminal :around #'dk/maybe-suspend-else-kill)

;; Do not ask if emacs should quit if restart-emacs is called.
;;------------------------------------------------------------------------------

(defun dk/restart-emacs-without-asking (orig-fun &rest args)
  (let ((confirm-kill-emacs nil))
    (apply orig-fun args)))

(advice-add 'restart-emacs :around #'dk/restart-emacs-without-asking)

;; Replace yes-or-no-p with y-or-n-p
;;------------------------------------------------------------------------------

(advice-add 'yes-or-no-p :around #'y-or-n-p)

;; Old version:
;; (defalias 'yes-or-no-p 'y-or-n-p)

;; Change the message in the minibuffer on startup.
;;------------------------------------------------------------------------------

(defun dk/startup-echo-area-message (_orig-fun &rest _args)
  (let ((not-installed
         (concat "Customs file not installed. Consider calling "
                 (substitute-command-keys "\\[dk/install-customs-file].")))
        (daemon "Starting Emacs daemon.")
        (normal (concat "Personal Emacs config version: "
                        (dk/config-version-string))))
    (if (not (file-exists-p dk/custom-settings-file))
        not-installed
      (if (daemonp) daemon normal))))

(advice-add 'startup-echo-area-message :around #'dk/startup-echo-area-message)

;; Balance windows after deleting windows.
;;------------------------------------------------------------------------------
(defun dk/delete-window (orig-fun &rest args)
  (interactive)
  (apply orig-fun args)
  (balance-windows))

(advice-add 'delete-window :around #'dk/delete-window)

(provide 'core-funcs)
