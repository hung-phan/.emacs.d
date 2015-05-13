;; -----------------------------------------------------------------------------
;; Remove useless whitespace before saving a file
;; -----------------------------------------------------------------------------
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

(provide 'setup-editing)
