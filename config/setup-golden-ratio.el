(require 'golden-ratio)

(colorvisa/set-up 'golden-ratio
  (setq golden-ratio-exclude-modes '("ediff-mode"))
  (golden-ratio-mode 1))

(setq split-width-threshold nil)

(provide 'setup-golden-ratio)
