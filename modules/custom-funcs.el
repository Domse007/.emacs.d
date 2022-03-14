;; Personal additions to predefined functions
;;------------------------------------------------------------------------------

(defun kill-word-at-point ()
  "Kill the word where the point is pointing at."
  (interactive)
  (let ((range (bounds-of-thing-at-point 'symbol)))
    (kill-region (- (car range) 1) (cdr range))))

(global-set-key (kbd "M-k") 'kill-word-at-point)

(defun dk/delete-window ()
  "Wrapper around `delete-window' to balance windows
after deleting the active window."
  (interactive)
  (delete-window)
  (balance-windows))

(global-set-key (kbd "C-x 0") 'dk/delete-window)

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

;; I'm forgetting a lot of stuff...
;;------------------------------------------------------------------------------

(require 'olivetti)

(defun dk/enable-centering ()
  "Disable the centering. This is more or less an alias for olivetti mode."
  (interactive)
  (olivetti-mode t))

(defun dk/disable-centering ()
  "Disable the centering. This is more or less an alias for olivetti mode."
  (interactive)
  (olivetti-mode nil))

;; See above...
;;------------------------------------------------------------------------------

(require 'auto-package-update)

(defun dk/package-update-packages ()
  "Update all packages. Alias for `auto-package-update-now'."
  (interactive)
  (dk/log "Updating packages..." t)
  (auto-package-update-now))

;; Config version stuff
;;------------------------------------------------------------------------------

(defun dk/config-version (&optional not-print only-version)
  "Return the major version of the config. If NOT-PRINT is given, the function
will not message the string. If ONLY-VERSION is given, only the version string
is returned otherwise the whole sentence is returned."
  (interactive)
  (let ((version-string
	 (concat
	  (number-to-string dk/config-major-version)
	  "."
	  (number-to-string dk/config-minor-version)))
	(explanation "Personal Emacs config version: "))
    (when (not not-print)
      (message (concat explanation version-string)))
    (if only-version
	version-string
      (concat explanation version-string))))

;; Open working directory in file explorer
;;------------------------------------------------------------------------------

(defun dk/explorer ()
  "Open the current directory in the file explorer."
  (interactive)
  (cond (dk/windows-flag (shell-command "explorer ."))
	(dk/linux-flag (shell-command "xdg-open ."))
	(t (dk/log "This command is not supported on this platform." t))))

;; Config error debug functions
;;------------------------------------------------------------------------------

(defun dk/count-loadable-files ()
  "Count the number of files in `dk/config-file-list' that can be loaded. This
is used by `display-startup-echo-area-message'."
  (let ((counter 0))
    (dolist (elem dk/config-file-list)
      (let ((file (car elem))
	    (arg (cdr elem)))
	(when arg
	  (setq counter (+ counter 1)))))
    counter))

(defun dk/locate-config-init-error (index)
  "Function that tries to locate the file where an error occured.
INDEX is the index of the list `dk/config-file-list' where the
error occured."
  (let ((error-file (nth (+ index 2) dk/config-file-list)))
    (if (not (equal error-file nil))
	(car error-file)
      "Could not locate error.")))

(defun display-startup-echo-area-message ()
  "Redefining the default startup message function. It appears to be very messi
internally. Because it's a redefine, it can't have the dk/ prefix."
  (let ((max-files (dk/count-loadable-files)))
    (progn
      (dk/log (concat "Info: Loaded "
		      (number-to-string dk/loaded-files-counter)
		      " files (out of "
		      (number-to-string max-files)
		      "). "
		      (number-to-string gcs-done)
		      " garbage collection runs. "
		      (if (equal max-files dk/loaded-files-counter)
			  "All loaded."
			(concat "Error in file "
				(dk/locate-config-init-error dk/loaded-files-counter)
				".")))
	      t)
      (when (equal max-files dk/loaded-files-counter)
	(run-with-timer 2 nil 'dk/config-version)))))

(defun dk/display-startup-message ()
  "Interactive wrapper around `display-startup-echo-area-message'."
  (interactive)
  (display-startup-echo-area-message))

;; Org (roam) export helpers
;;------------------------------------------------------------------------------

(require 'org)

(defvar org-export-output-directory-prefix "export"
  "Prefix of directory used for org-mode export")

(defadvice org-export-output-file-name (before org-add-export-dir activate)
  "Modifies org-export to place exported files in a different directory"
  (when (not pub-dir)
    (setq pub-dir org-export-output-directory-prefix)
    (when (not (file-directory-p pub-dir))
      (make-directory pub-dir))))

(org-link-set-parameters "id"  :export #'dk/org-id-link-export)

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

(when dk/use-40-percent-keyboard
  (progn (dk/log "Enabling 40 percent keyboard mode.")
	 (dk/40-percent-keyboard-mode)))

(provide 'custom-funcs)
