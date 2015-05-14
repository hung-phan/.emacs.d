;; Subtler highlight
(set-default 'magit-stage-all-confirm nil)
(set-default 'magit-unstage-all-confirm nil)

(add-hook 'magit-mode-hook 'magit-load-config-extensions)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(provide 'setup-magit)
