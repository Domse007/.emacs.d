(defconst dk/cobol-download-info
  `(:version "2.4.0" :os ,(cond ((eq system-type 'gnu/linux) (cons "linux" ""))
                                ((eq system-type 'windows-nt) (cons "w32" "-signed"))))
  "Information on the version that should be downloaded")

(defconst dk/cobol-download-url-format
  "https://github.com/eclipse-che4z/che-che4z-lsp-for-cobol/releases/download/%s/cobol-language-support-%s-x64-%s%s.vsix")

(defconst dk/cobol-install-dir
  (expand-file-name "lsp-proxy/cobol/" dk/user-emacs-cache-dir))

(defun dk/cobol-download-url ()
  (let ((vers (plist-get dk/cobol-download-info :version))
        (os (plist-get dk/cobol-download-info :os)))
    (format dk/cobol-download-url-format vers (car os) vers (cdr os))))

(defun dk/cobol-lsp-install ()
  (interactive)
  (let ((url (dk/cobol-download-url))
        (file (expand-file-name "download.zip" dk/cobol-install-dir)))
    (make-directory dk/cobol-install-dir t)
    (url-copy-file url file)
    (let ((default-directory dk/cobol-install-dir))
      (shell-command-to-string (concat "unzip " file))
      (copy-file "extension/server/native/server-linux" "cobol-server")
      (if (file-exists-p "cobol-server")
          (message "Successfully installed server.")
        (user-error "Failed to install server.")))))

(defconst dk/cobol-server-file
  (expand-file-name (cond ((equal system-type 'windows-nt) "server-windows.exe")
                          ((equal system-type 'gnu/linux) "server-linux"))
                    dk/cobol-install-dir)
  "Location of the ")

(defconst dk/cobol-skeleton
  "       IDENTIFICATION DIVISION.
       PROGRAM-ID. SampleProgram.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION."
  "Default cobol skeleton.")

(defun dk/insert-default-cobol-skeleton ()
  (interactive)
  (with-current-buffer
      (if (equal major-mode 'cobol-mode)
          (progn 
            (insert dk/cobol-skeleton)
            (message "Successfully inserted skeleton"))
        (user-error "Not in a cobol-mode buffer"))))

(use-package cobol-mode
  :mode (("\\.cob\\'" . cobol-mode)
	 ("\\.cbl\\'" . cobol-mode)
	 ("\\.cpy\\'" . cobol-mode)))

;;; FIX: if the current line is empty, the default cobol-indent-line function is
;;;      throws an error.
(advice-add #'cobol-indent-line :around
	    (lambda (f &rest args)
	      (let ((current-line (buffer-substring-no-properties
				   (line-beginning-position)
				   (line-end-position))))
		(if (equal (length (string-trim current-line)) 0)
		    (insert "       ")
		  (condition-case err (apply f args)
		    (message "[ERROR] %s" err))))))

(defun cobol-smart-tab ()
  "COBOL-aware tab function.
  Tabs to column 8 (Area A) when in columns 1-7,
  then tabs to multiples of 4 from Area A start (columns 8, 12, 16, 20, etc.)."
  (interactive)
  (let* ((col (current-column))
         (spaces-to-insert
          (if (< col 7)
              ;; In sequence/indicator area - tab to Area A (column 7)
              (- 7 col)
            ;; In Area A or B - tab to next multiple of 4 from Area A start
            (let* ((offset-from-area-a (- col 7))
                   (next-tab-index (1+ (floor offset-from-area-a 4)))
                   (next-tab-position (+ 7 (* next-tab-index 4))))
              (- next-tab-position col)))))
    (insert (make-string spaces-to-insert ?\s))))

(defun cobol-mode-setup ()
  "Setup COBOL mode with custom tab behavior."
  (local-set-key (kbd "TAB") 'cobol-smart-tab))

(add-hook 'cobol-mode-hook 'cobol-mode-setup)

(use-package ob-cobol
  :quelpa (ob-cobol :fetcher github :repo "domse007/ob-cobol" :files ("ob-cobol.el"))
  :custom
  ((ob-cobol-compiler 'gcobol)))

;; (use-package jcl-mode
;;   :quelpa (jcl-mode :fetcher github :repo "lsiksous/jcl-mode.el" :files ("jcl-mode.el")))

(provide 'module-cobol)
