(use-package org-journal
  :custom
  ((org-journal-file-type 'daily)
   (org-journal-dir (concat user-system-base-path "Personal/Journal/"))
   (org-journal-date-format "%A, %d. %B %Y")
   (org-journal-file-header "#+TITLE: Daily Journal from %d.%m.%Y")
   (org-journal-file-format "%Y%m%d")
   (org-journal-enable-agenda-integration t))
  :bind
  (("C-c C-SPC C-s" . org-journal-new-scheduled-entry))
  :config
  (add-to-list 'auto-mode-alist
               (cons (concat (file-truename org-journal-dir)
                             org-journal-file-pattern)
                     'org-journal-mode)))

(provide 'org-journal.el)
