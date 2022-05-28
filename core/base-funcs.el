;; Personal additions to predefined functions
;;------------------------------------------------------------------------------

(defun dk/kill-word-at-point ()
  "Kill the word where the point is pointing at."
  (interactive)
  (let ((range (bounds-of-thing-at-point 'symbol)))
    (kill-region (- (car range) 1) (cdr range))))

(global-set-key (kbd "M-k") 'dk/kill-word-at-point)

(defun dk/delete-window ()
  "Wrapper around `delete-window' to balance windows after deleting the active
window."
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
  (dk/log 'info "Updating packages...")
  (auto-package-update-now)
  (quelpa-upgrade-all-maybe))

;; Config version stuff
;;------------------------------------------------------------------------------

(defun dk/config-version (&optional not-print only-version)
  "Return the major version of the config. If NOT-PRINT is given, the function
will not message the string. If ONLY-VERSION is given, only the version string
is returned otherwise the whole sentence is returned."
  (interactive)
  (let ((version-string (concat (number-to-string dk/config-major-version)
				"."
				(number-to-string dk/config-minor-version)))
	(explanation "Personal Emacs config version: "))
    (when (not not-print)
      (dk/log 'info explanation version-string))
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
	(t (dk/log 'error "This command is not supported on this platform."))))

;; Init functions
;;------------------------------------------------------------------------------

(defun display-startup-echo-area-message ()
  "Redefining the default startup message function. It appears to be very messi
internally. Because it's a redefine, it can't have the dk/ prefix."
  (dk/config-version))

;; Org (roam) export helpers
;;------------------------------------------------------------------------------

(require 'org)

(defvar org-export-output-directory-prefix ".export"
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
  (progn (dk/log 'info "Enabling 40 percent keyboard mode.")
	 (dk/40-percent-keyboard-mode)))

;; Checking for external dependencies
;;------------------------------------------------------------------------------

(defconst dk/system-dependencies
  '("gcc" "grep" "pdflatex" "git" "python" "cargo" "zip" "unzip")
  "List of external programs that are required to have a working config.")

(defun dk/check-external-deps ()
  "Check if external programs are available."
  (interactive)
  (let ((missing-alist))
    (dolist (program dk/system-dependencies)
      (when (equal (executable-find program) nil)
	(if (not missing-alist)
	    (setq missing-alist `(,program))
	  (add-to-list missing-alist program))))
    (if (equal missing-alist nil)
	(dk/log 'info "No missing dependencies.")
      (dk/log 'error "Missing following dependencies: "
	      (substring (format "%s" missing-alist) 1 -1)))))

(provide 'base-funcs)