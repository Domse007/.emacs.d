(use-package org-roam
  :hook
  ((after-init . org-roam-mode))
  :custom
  ((org-roam-directory
    (concat dk/user-system-base-path "Schule/"))
   (org-roam-update-db-idle-seconds 60)
   (org-roam-db-update-idle-seconds 60))
  :bind
  (:map org-roam-mode-map
   ("C-c n l" . org-roam)
   ("C-c C-SPC C-f" . org-roam-find-file)
   ("C-c n g" . org-roam-graph)
   :map org-mode-map
   ("C-c n i" . org-roam-insert)
   ("C-c n I" . org-roam-insert-immediate)))

(use-package org-roam-server
  :custom
  ((org-roam-server-host "127.0.0.1")
   (org-roam-server-port 8080)
   (org-roam-server-authenticate nil)
   (org-roam-server-export-inline-images t)
   (org-roam-server-serve-files nil)
   (org-roam-server-served-file-extensions '("pdf" "mp4" "ogv"))
   (org-roam-server-network-poll t)
   (org-roam-server-network-arrows nil)
   (org-roam-server-network-label-truncate t)
   (org-roam-server-network-label-truncate-length 60)
   (org-roam-server-network-label-wrap-length 20)))

(provide 'org-roam.el)
