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

(defalias 'yes-or-no-p 'y-or-n-p)

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

;; Balance windows after deleting windows.
;;------------------------------------------------------------------------------

(require 'subr-x)

(defun dk/config-version ()
  "Report the current version and commit into the minibuffer."
  (interactive)
  (let* ((default-directory user-emacs-directory)
	 (git-found (executable-find "git"))
	 (version (dk/config-version-string))
	 (hash (when git-found
		 (string-trim (shell-command-to-string
			       "git rev-parse --short HEAD"))))
	 (branch (when git-found
		   (string-trim (shell-command-to-string
				 "git rev-parse --abbrev-ref HEAD")))))
    (message "Version: %s %s %s" version (if branch (concat "@" branch) "")
	     (if branch (concat "#" hash) ""))))

;; Interactively describe the major mode.
;;------------------------------------------------------------------------------

(defun dk/describe-major-mode ()
  "Interactively describe the current major-mode."
  (interactive)
  (let ((mode major-mode)
	(type (type-of major-mode)))
    (message "The current major mode is: %s. major-mode is of type %s"
	     mode type)))

(unless (fboundp major-mode) ; Prevents future incompatibility.
  (defalias 'major-mode 'dk/describe-major-mode))

;; Check for new commits on remote.
;;------------------------------------------------------------------------------

(defun dk/version-check-remote-ahead ()
  "Check if the local emacs config is up-to-date with remote."
  (interactive)
  (let* ((default-directory user-emacs-directory)
	 (_ (shell-command-to-string "git fetch"))
	 (local-hash (string-trim (shell-command-to-string
				   "git rev-parse HEAD")))
	 (remote-hash (string-trim (shell-command-to-string
				    "git rev-parse origin"))))
    (if (string-equal local-hash remote-hash)
	(dk/log 'info "Config is on latest commit.")
      (dk/log 'warning "Config on remote is ahead."))))

(run-with-idle-timer 0.3 nil #'dk/version-check-remote-ahead)

(provide 'core-funcs)
