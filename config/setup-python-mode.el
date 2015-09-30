(colorvisa/set-up 'elpy
  (package-initialize)
  (setq elpy-modules (delq 'elpy-module-company elpy-modules))
  (setq elpy-rpc-backend "jedi")
  (elpy-enable))

(provide 'setup-python-mode)
