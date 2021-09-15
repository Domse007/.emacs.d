;; (use-package org-roam
;;   :hook
;;   ((after-init . org-roam-mode))
;;   :init
;;   (setq org-roam-v2-ack t)
;;   :custom
;;   ((org-roam-directory dk/org-roam-dir)
;;    (org-roam-update-db-idle-seconds 60)
;;    (org-roam-db-update-idle-seconds 60))
;;   :config
;;   (org-roam-setup)
;;   :bind
;;   (("C-c C-SPC C-f" . org-roam-node-find)
;;    :map org-roam-mode-map
;;    ("C-c n g" . org-roam-graph)
;;    :map org-mode-map
;;    ("C-c n i" . org-roam-node-insert)))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory dk/org-roam-dir)
  (org-roam-completion-everywhere t)
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   :map org-mode-map
   ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

(use-package org-roam-ui
  :quelpa
  (org-roam-ui :fetcher github
	       :repo "org-roam/org-roam-ui"
	       :files ("*"))
  :after org-roam
  :hook
  (after-init . org-roam-ui-mode)
  :custom
  ((org-roam-ui-sync-theme t)
   (org-roam-ui-follow t)
   (org-roam-ui-update-on-save t)
   (org-roam-ui-open-on-start t)))

(provide 'org-roam.el)
