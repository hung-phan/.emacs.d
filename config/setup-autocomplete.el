(require 'yasnippet)
(require 'company)
(require 'color)

;;; custom color for company-mode
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;;; custom company mode
(setq company-idle-delay 0.1)

;;; init after load
(add-hook 'after-init-hook 'global-company-mode)

(add-to-list 'company-backends 'company-yasnippet t)
(add-to-list 'company-backends 'company-inf-ruby t)
(add-to-list 'company-backends 'company-robe t)

(defun company-yasnippet-or-completion ()
  (interactive)
  (if (first (yas--current-key))
    (progn
      (company-abort)
      (yas-expand))
    (company-complete-common)))

(define-key company-active-map (kbd "TAB") 'company-yasnippet-or-completion)
(define-key company-active-map (kbd "<tab>") 'company-yasnippet-or-completion)

(provide 'setup-autocomplete)
