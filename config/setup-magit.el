(require 'magit)

;; Subtler highlight
(set-default 'magit-stage-all-confirm nil)
(set-default 'magit-unstage-all-confirm nil)

;; prevent warning to be shown
(setq magit-last-seen-setup-instructions "1.4.0")

(add-hook 'magit-mode-hook 'magit-load-config-extensions)

(provide 'setup-magit)
