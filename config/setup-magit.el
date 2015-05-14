;; Subtler highlight
(set-default 'magit-stage-all-confirm nil)
(set-default 'magit-unstage-all-confirm nil)

(add-hook 'magit-mode-hook 'magit-load-config-extensions)

(provide 'setup-magit)
