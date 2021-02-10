(defcustom dk/custom-org-std-user ""
  "Set the author for my personal org template"
  :type 'string)

(defun dk/custom-insert-equation (&optional dk/custom-insert-equation)
  "Insert custom equations. Either inline or centered"
  (let ((dk/custom-insert-equation-input  "")
	(dk/custom-insert-equation-input-response "")
	(dk/custom-insert-equation-parents '()))
    (progn (if (equal dk/custom-insert-equation t)
	       (progn (setq-local dk/custom-insert-equation-input-response "Insert INLINE equation: ")
		      (setq-local dk/custom-insert-equation-parents '("(" . ")")))
	     (progn (setq-local dk/custom-insert-equation-input-response "Insert CENTERED equation: ")
		    (setq-local dk/custom-insert-equation-parents '("[" . "]"))))
	   (setq-local dk/custom-insert-equation-input
		       (read-string dk/custom-insert-equation-input-response))
	   (if (string-equal major-mode "org-mode")
	       (progn (insert "\\"
			      (car dk/custom-insert-equation-parents)
			      dk/custom-insert-equation-input
			      "\\"
			      (cdr dk/custom-insert-equation-parents))
		      (message "Done."))
	     (message "Currently not using Org-mode!")))))

(defun dk/custom-org-header-template ()
  "Insert all my leading lines for any org file."
  (if (string-equal major-mode "org-mode")
      (let ((dk/custom-org-buffer-name (if (string-equal (substring (buffer-name) -4) ".org")
					   (concat (upcase (substring (buffer-name) 0 1))
						   (substring (buffer-name) 1 -4))
					 (buffer-name)))
	    (dk/custom-org-point-position-flag nil)
	    (dk/custom-org-point-position 0))
	(progn (save-excursion
		 (when (equal (point) 1)
		   (setq-local dk/custom-org-point-position-flag t))
		 (goto-char 0)
		 (insert "#+TITLE: " (read-string "Title: " dk/custom-org-buffer-name)
			 "\n#+AUTHOR: " dk/custom-org-std-user
			 "\n#+OPTIONS: toc:t date:nil title:t author:t num:t \\n:t"
			 "\n#+EXPORT_FILE_NAME: " dk/custom-org-buffer-name)
		 (when (char-equal (read-key "HTML theme? [t(rue), n(il)]: ") ?t)
		   (insert "\n#+SETUPFILE: https://fniessen.github.io/org-html-themes/org/theme-readtheorg.setup"))
		 (when (char-equal (read-key "Reveal presentation) [t(rue), n(il)]: ") ?t)
		   (insert "\n#+REVEAL_ROOT: https://cdn.jsdelivr.net/npm/reveal.js"))
		 (insert "\n#+LATEX_CLASS: "(read-string "LaTeX class: " nil nil "article")
			 "\n#+LANGUAGE: de"
			 "\n#+LATEX_HEADER: \\usepackage[AUTO]{babel}"
			 "\n#+LATEX: \\setlength\\parindent{0pt}"
			 "\n\n")
		 (setq-local dk/custom-org-point-position (point)))
	       (when (equal dk/custom-org-point-position-flag t)
		 (goto-char dk/custom-org-point-position))))
    (message "Currently not using Org-mode!")))

(defun dk/custom-org-general-inserter (dk/custom-insert-input)
  "Insert differnt kind of templates."
  (let ((dk/custom-org-template-list '(("frac" . "\\displaystyle\\frac{}{}")
				       ("vec" . "\\begin{pmatrix}  \\\\  \\\\  \\end{pmatrix}"))))
    (dotimes (dk/custom-org-i (length dk/custom-org-template-list))
      (when (string-equal dk/custom-insert-input (car (nth dk/custom-org-i dk/custom-org-template-list)))
	(progn (insert (cdr (nth dk/custom-org-i dk/custom-org-template-list)))
	       (message "Done!"))))))

(defun dk/custom-org-functions-selector (&optional selector)
  "Intial function to call helper functions to insert my org templates."
  (interactive)
  (let ((dk/custom-org-function-selector-input (read-string (concat "[[org, stdtemplate], "
								    "[eqi], [eq], "
								    "[frac], [vec]"
								    "]: "))))
    (cond ((or (string-equal dk/custom-org-function-selector-input "org")
	       (string-equal dk/custom-org-function-selector-input "stdtemplate")
	       (string-equal selector "org"))
	   (dk/custom-org-header-template))
	  ((or (string-equal dk/custom-org-function-selector-input "eqi")
	       (string-equal selector "eqi"))
	   (dk/custom-insert-equation t))
	  ((or (string-equal dk/custom-org-function-selector-input "eq")
	       (string-equal selector "eq"))
	   (dk/custom-insert-equation))
	  ((or (string-equal dk/custom-org-function-selector-input "frac")
	       (string-equal selector "frac")
	       (string-equal dk/custom-org-function-selector-input "vec")
	       (string-equal selector "vec"))
	   (dk/custom-org-general-inserter dk/custom-org-function-selector-input))
	  (t (message "Not a macro!")))))

(global-set-key (kbd "C-c m") 'dk/custom-org-functions-selector)
(global-set-key (kbd "C-x m") 'dk/custom-org-functions-selector)

;; (defcustom template-mode-insert-function-mode "math"
;;   "This variable is responsible to save the current subject."
;;   :type 'string)

;; (defconst template-mode-posframe-buffer-name "template-comp"
;;   :type 'string)

;; (defun math-mode ()
;;   (interactive)
;;   (setq insert-function-mode "math"))

;; (defun chem-mode ()
;;   (interactive)
;;   (setq insert-function-mode "chem"))

;; (defun template-mode-report-current-insert-mode ()
;;   "Returns the current active insert mode."
;;   (interactive)
;;   (message (concat "The current insert mode is: " insert-function-mode)))

;; (defun template-mode-insert-custom-template ()
;;   "Provides the front end to insert a template"
;;   (interactive)
;;   (message "This is in a test state")
;;   (when (posframe-workable-p)
;;     (posframe-show template-mode-posframe-buffer-name
;;                    :string (concat "(" template-mode-insert-function-mode "):")
;; 		   :position (point)
;; 		   :background-color "#FFFF00")))

;; (defun template-mode-delete-posframe ()
;;   (interactive)
;;   (posframe-delete template-mode-posframe-buffer-name))

(defun dk/open-config (&optional dk/open-config-file)
  "open the config.org file"
  (interactive)
  (if (string-equal dk/open-config-file "")
      (switch-to-buffer (find-file-noselect "~/.emacs.d/init.el"))
    (switch-to-buffer (find-file-noselect (concat "~/.emacs.d/config/" dk/open-config-file) nil nil t))))

(global-set-key (kbd "C-x RET RET") 'dk/open-config)
(global-set-key (kbd "C-x RET o") (lambda () (interactive) (dk/open-config "org-mode.el")))
(global-set-key (kbd "C-x RET c") (lambda () (interactive) (dk/open-config "custom-funcs.el")))

(defun explorer ()
  "Open the current directory in the file explorer."
  (interactive)
  (when (string-equal system-type "windows-nt")
    (shell-command "explorer .")))

(defun explorer ()
  "Open the current directory in the file explorer."
  (interactive)
  (when (string-equal system-type "windows-nt")
    (shell-command "explorer .")))

(provide 'custom-funcs.el)



