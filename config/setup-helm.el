(require 'setup-util)

(require 'helm)
(require 'helm-descbinds)
(require 'helm-projectile)
(require 'helm-flycheck)
(require 'helm-ag)

;;; Code:
(colorvisa/in '(darwin)
  (setq helm-locate-command "mdfind %s %s"))

(projectile-global-mode)
(setq projectile-enable-caching t)
(helm-mode 1)
(helm-descbinds-install)
(custom-set-variables '(helm-ag-use-temp-buffer t))

;; set helm variables
(setq
 helm-mp-highlight-delay 0.7
 helm-mp-highlight-threshold 4
 helm-maybe-use-default-as-input nil
 helm-quick-update t
 helm-idle-delay 0.01
 helm-input-idle-delay 0.01
 helm-always-two-windows nil
 helm-split-window-default-side 'other
 helm-candidate-number-limit 200)

(defun helm-backspace ()
  "Forward to `backward-delete-char'. On error (read-only), quit without selecting."
  (interactive)
  (condition-case nil
      (backward-delete-char 1)
    (error
     (helm-keyboard-quit))))

(define-key helm-map (kbd "DEL") 'helm-backspace)
(define-key helm-map [escape] 'helm-keyboard-quit)

(provide 'setup-helm)
