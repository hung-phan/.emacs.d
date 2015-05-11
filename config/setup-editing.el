;; -----------------------------------------------------------------------------
;; Set locale to UTF8
;; -----------------------------------------------------------------------------
(set-terminal-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; -----------------------------------------------------------------------------
;; Remove useless whitespace before saving a file
;; -----------------------------------------------------------------------------
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

(provide 'setup-editing)
