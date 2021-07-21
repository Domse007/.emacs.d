(use-package org-roam
  :hook
  ((after-init . org-roam-mode))
  :init
  (setq org-roam-v2-ack t)
  :custom
  ((org-roam-directory dk/org-roam-dir)
   (org-roam-update-db-idle-seconds 60)
   (org-roam-db-update-idle-seconds 60))
  :config
  (org-roam-setup)
  :bind
  (("C-c C-SPC C-f" . org-roam-node-find)
   :map org-roam-mode-map
   ("C-c n g" . org-roam-graph)
   :map org-mode-map
   ("C-c n i" . org-roam-node-insert)))

;; (use-package org-roam-server
;;   :custom
;;   ((org-roam-server-host "127.0.0.1")
;;    (org-roam-server-port 8080)
;;    (org-roam-server-authenticate nil)
;;    (org-roam-server-export-inline-images t)
;;    (org-roam-server-serve-files nil)
;;    (org-roam-server-served-file-extensions '("pdf" "mp4" "ogv"))
;;    (org-roam-server-network-poll t)
;;    (org-roam-server-network-arrows nil)
;;    (org-roam-server-network-label-truncate t)
;;    (org-roam-server-network-label-truncate-length 60)
;;    (org-roam-server-network-label-wrap-length 20)))

(provide 'org-roam.el)
