(defun kill-word-at-point ()
  "Kill the word where the point is pointing at."
  (interactive)
  (let ((range (bounds-of-thing-at-point 'symbol)))
    (kill-region (- (car range) 1) (cdr range))))

(global-set-key (kbd "M-k") 'kill-word-at-point)

;;------------------------------------------------------------------------------

(defconst games '(gomoku tetris hanoi 5x5 blackbox bubbles dunnet life)
  "List of included games.")

(defun play-a-game ()
  "randomly play a game."
  (interactive)
  (let* ((game-list-len (length games))
	 (game-index (random game-list-len))
	 (game-to-be-played (nth game-index games)))
    (call-interactively game-to-be-played)))

;;------------------------------------------------------------------------------

(require 'olivetti)

(defun disable-centering ()
  "Disable the centering. This is more or less an alias for 
olivetti mode."
  (interactive)
  (olivetti-mode nil))

;;------------------------------------------------------------------------------

(require 'auto-package-update)

(defun dk/package-update-packages ()
  "Update all packages. Alias for `auto-package-update-now'."
  (interactive)
  (message "Updating packages...")
  (auto-package-update-now))

;;------------------------------------------------------------------------------

(defun dk/config-version (&optional not-print only-version)
  "Return the major version of the config. If not-print
is given, the function will not message the string. If
only-version is given, only the version string is returned
otherwise the whole sentence is returned."
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

;;------------------------------------------------------------------------------

(defun dk/explorer ()
  "Open the current directory in the file explorer."
  (interactive)
  (cond (dk/windows-flag (shell-command "explorer ."))
	(dk/linux-flag (shell-command "xdg-open ."))
	(t (message "This command is not supported on this platform."))))

;;------------------------------------------------------------------------------

(defun dk/count-loadable-files ()
  "Count the number of files in `dk/config-file-list'
that can be loaded. This is used by
`display-startup-echo-area-message'."
  (let ((counter 0))
    (dolist (elem dk/config-file-list)
      (let ((file (car elem))
	    (arg (cdr elem)))
	(when arg
	  (setq counter (+ counter 1)))))
    counter))

(defun display-startup-echo-area-message ()
  "Redefining the default startup message function.
It appears to be very messi internally. Because
it's a redefine, it can't have the dk/ prefix."
  (let ((max-files (dk/count-loadable-files)))
    (message (concat "Info: Loaded "
		     (number-to-string dk/loaded-files-counter)
		     " files (out of "
		     (number-to-string max-files)
		     "). "
		     (if (equal max-files dk/loaded-files-counter)
			 "All loaded."
		       "An error occured while loading the files."))))
  (run-with-timer 2 nil 'dk/config-version))

(provide 'dk/custom-funcs)
