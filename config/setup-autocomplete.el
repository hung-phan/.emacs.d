(require 'yasnippet)
(require 'company)
(require 'color)
(require 'company-tern)

;; custom color for company-mode
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; custom company mode
(setq company-idle-delay 0
      company-minimum-prefix-length 1
      company-require-match nil
      company-dabbrev-ignore-case t
      company-dabbrev-downcase nil
      company-frontends '(company-pseudo-tooltip-frontend))

;; enable yasnippet for all backends
(defvar company-mode/enable-yas t)

;; autocomplete list
(defvar colorvisa/removed-packages '(company-ropemacs
                                     company-bbdb
                                     company-eclim
                                     company-xcode
                                     company-clang
                                     company-cmake))

(dolist (p colorvisa/removed-packages)
  (setq company-backends (remove p company-backends)))

(add-to-list 'company-backends 'company-yasnippet t)
(add-to-list 'company-backends 'company-inf-ruby t)
(add-to-list 'company-backends 'company-robe t)
(add-to-list 'company-backends 'company-tern)

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append
     (if (consp backend) backend (list backend))
     '(:with company-yasnippet company-dabbrev-code))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(eval-after-load 'tern '(require 'company-tern))

;; init after load
(setq company-global-modes '(not eshell-mode comint-mode org-mode))
(add-hook 'after-init-hook 'global-company-mode)

(provide 'setup-autocomplete)
