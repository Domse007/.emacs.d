;;; vars.el - This holds a few variables.

;; commentary:
;; - The code doesen't depend on any additional binaries.


(defvar user-system-name (system-name)
;; This variable contains the name of the host computer
  :string)

(defvar user-system-base-path ""
;; This variable contains the used path in the current session
  :string)

;; (defconst user-ivy-posframe-background-color "#4C4;; C4C"
;; ;; This avariable describes the posframe background color.
;;   :string)

;; (defconst user-ivy-posframe-border-color "#FF9505"
;; ;; This avariable describes the posframe border color.
;;   :string)

;; (defconst user-ivy-posframe-cursor-color "#50C517"
;; ;; This avariable describes the posframe cursor color.
;;   :string)

(provide 'vars.el)
;;; vars.el ends here
