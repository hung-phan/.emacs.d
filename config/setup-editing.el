;; -----------------------------------------------------------------------------
;; Remove useless whitespace before saving a file
;; -----------------------------------------------------------------------------
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

;; indent level
(setq ruby-indent-level 2)
(setq enh-ruby-indent-level 2)
(setq css-indent-offset 2)

(provide 'setup-editing)
