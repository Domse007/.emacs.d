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

(use-package delve
  :quelpa
  (delve :fetcher github :repo "publicimageltd/delve")
  :custom
  ((delve-store-directory (concat user-emacs-directory
				  dk/user-emacs-etcdir
				  "delve-store")))
  :bind
  (("<f12>" . delve))
  :config
  (delve-global-minor-mode))

(provide 'text-org-roam)
