(require 'robe)
(require 'enh-ruby-mode)
(require 'inf-ruby)

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;; avoid ridiculous ruby indentation
(setq ruby-deep-indent-paren nil)

(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

;; easily switch from common Ruby compilation modes to interact with a debugger.
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(provide 'setup-ruby-mode)
