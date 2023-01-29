(new-external-dependency! 'gcc) ;; for compiling sqlite.

(defun dk/org-roam-get-template (file)
  "Get the string of a template file and modify it accordingly. Errors when
FILE does not exist."
  (if (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(replace-string "[NAME]" user-full-name)
	(buffer-string))
    (error "File %s does not exist." file)))

(defconst dk/org-roam-default-template
  (dk/org-roam-get-template (expand-file-name "templates/default.org"
					      user-emacs-directory))
  "Template string of the default org-roam template.")

(defconst dk/org-roam-program-template
  (dk/org-roam-get-template (expand-file-name "templates/program.org"
					      user-emacs-directory))
  "Tempate string for describing the functionalities of a program.")

(use-package org-roam
  :defer t
  :init
  (setq org-roam-v2-ack t)
  ;; currently this built-in package isn't loaded
  ;; can be removed in the future.
  (require 'ucs-normalize)
  :custom
  ((org-roam-directory dk/org-roam-dir)
   (org-roam-completion-everywhere t)
   (org-roam-db-location
    (concat dk/user-emacs-cache-dir "org/org-roam.db"))
   (org-roam-capture-templates
    `(("d" "default" plain "%?" :target
       (file+head "%<%Y%m%d>-${slug}.org" ,dk/org-roam-default-template)
       :unnarrowed t)
      ("p" "program" plain "%?" :target
       (file+head "program/%<%Y%m%d>-${slug}.org" ,dk/org-roam-program-template)
       :unnarrowed t))))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-M-i" . completion-at-point)
   ("<mouse-1>" . org-roam-visit-thing))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
		 (display-buffer-in-side-window)
		 (side . right)
		 (slot . 0)
		 (window-width . 0.25)
		 (preserve-size . (t nil))
		 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  (org-roam-db-autosync-enable))

(use-package org-roam-ui
  :defer t
  :after org-roam
  :hook
  (after-init . org-roam-ui-mode)
  :custom
  ((org-roam-ui-sync-theme t)
   (org-roam-ui-follow t)
   (org-roam-ui-update-on-save t)
   (org-roam-ui-open-on-start t)))

(use-package org-roam-peek
  :disabled t
  :after org-roam
  :quelpa
  (org-roam-peek :fetcher github :repo "domse007/org-roam-peek")
  :hook
  ((org-mode . org-roam-peek-mode)))

(use-package org-rainbow-tags
  :quelpa
  (org-rainbow-tags :fetcher github :repo "KaratasFurkan/org-rainbow-tags")
  :hook
  ((org-mode . org-rainbow-tags-mode)))

;; (dk/get-package!
;;     :user "publicimageltd"
;;     :repo "delve"
;;     :force dk/get-package-override-git-availability)
;;
;; (use-package delve
;;   :ensure (not dk/get-package-override-git-availability)
;;   :quelpa
;;   (delve :fetcher github :repo "publicimageltd/delve")
;;   :custom
;;   ((delve-store-directory (concat dk/user-emacs-cache-dir "delve-store")))
;;   :bind
;;   (("<f12>" . delve))
;;   :config
;;   (delve-global-minor-mode t))

;;; Custom Code:

(defun dk/org-roam-count-files ()
  "Approximate the number of files in the org database."
  (interactive)
  (let* ((ord org-roam-directory)
	 (directories `(,ord ,(expand-file-name "program" ord)))
	 (res 0))
    (dolist (dir directories)
      (setq res (+ res (- (length (directory-files dir))
			  2 ; subtract 2 for "." and ".."
			  ))))
    (when (called-interactively-p 'any)
      (message "There are %s files in the db." res))
    res))

(provide 'text-org-roam)
