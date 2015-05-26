;; -----------------------------------------------------------------------------
;; Remove useless whitespace before saving a file
;; -----------------------------------------------------------------------------
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))
(add-hook 'prog-mode-hook 'textmate-mode)

;; indent level
(setq ruby-indent-level 2)
(setq css-indent-offset 2)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq jsx-indent-level 2)

(provide 'setup-editing)
