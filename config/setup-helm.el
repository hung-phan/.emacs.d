(require 'setup-util)

(require 'helm)
(require 'helm-descbinds)
(require 'helm-projectile)
(require 'helm-flycheck)

(colorvisa/in '(darwin)
  (setq helm-locate-command "mdfind %s %s"))

(projectile-global-mode)
(setq projectile-enable-caching t)
(helm-mode 1)
(helm-descbinds-install)

(setq
 helm-mp-highlight-delay 0.7
 helm-mp-highlight-threshold 4
 helm-maybe-use-default-as-input nil
 helm-quick-update t
 helm-idle-delay 0.01
 helm-input-idle-delay 0.01
 helm-always-two-windows nil
 helm-split-window-default-side 'other
 helm-candidate-number-limit 200)

(provide 'setup-helm)
