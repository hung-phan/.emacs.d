;; -----------------------------------------------------------------------------
;; application alias
;; -----------------------------------------------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p) ;; y or n is enough
(defalias 'list-buffers 'ibuffer) ;; always use ibuffer

;; -----------------------------------------------------------------------------
;; Remove all backup files
;; -----------------------------------------------------------------------------
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

(provide 'setup-applications)
