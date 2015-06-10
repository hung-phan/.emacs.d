(require 'git-timemachine)
(require 'gist)

(colorvisa/set-up 'git-gutter
  ;; enable git-gutter as global mode
  (global-git-gutter-mode +1))

(colorvisa/set-up 'magit
  ;; no prompt for confirmation
  (set-default 'magit-stage-all-confirm nil)
  (set-default 'magit-unstage-all-confirm nil)

  ;; prevent warning to be shown
  (setq magit-last-seen-setup-instructions "1.4.0")
  (add-hook 'magit-mode-hook 'magit-load-config-extensions))

(provide 'setup-magit)
