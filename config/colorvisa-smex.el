(require 'smex)

;; -----------------------------------------------------------------------------
;; smex
;; -----------------------------------------------------------------------------
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; This is your old M-x.

(provide 'colorvisa-smex)
