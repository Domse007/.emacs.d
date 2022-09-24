(new-external-dependency! 'gcc) ;; for compiling sqlite.

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
       (file+head "${slug}.org"
		  ,(concat "#+title: ${title}\n"
			   "#+author: " user-full-name "\n"
			   "#+options: toc:nil date:nil author:t\n\n"))
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
  :after org-roam
  :quelpa
  (org-roam-peek :fetcher github :repo "domse007/org-roam-peek")
  :hook
  ((org-mode . org-roam-peek-mode)))

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

(provide 'text-org-roam)
