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
    (concat user-emacs-directory
	    dk/user-emacs-etcdir
	    "org/org-roam.db")))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-M-i" . completion-at-point))
  :config
  (org-roam-db-autosync-enable))

(use-package org-roam-ui
  ;; :quelpa
  ;; (org-roam-ui :fetcher github
  ;; 	       :repo "org-roam/org-roam-ui"
  ;; 	       :files ("*"))
  :defer t
  :after org-roam
  :hook
  (after-init . org-roam-ui-mode)
  :custom
  ((org-roam-ui-sync-theme t)
   (org-roam-ui-follow t)
   (org-roam-ui-update-on-save t)
   (org-roam-ui-open-on-start t)))

(provide 'dk/org-roam)
