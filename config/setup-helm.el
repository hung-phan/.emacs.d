(require 'setup-util)

(projectile-global-mode)
(setq projectile-enable-caching t)

(helm-mode 1)

(colorvisa/in '(darwin)
  (setq helm-locate-command "mdfind %s %s"))

(provide 'setup-helm)
