(require 'robe)
(require 'enh-ruby-mode)
(require 'inf-ruby)

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
(rvm-use-default)

;; avoid ridiculous ruby indentation
(setq ruby-deep-indent-paren nil)

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

(add-hook 'inf-ruby-mode-hook 'robe-start)

;; easily switch from common Ruby compilation modes to interact with a debugger.
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(add-hook 'enh-ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))
(add-hook 'ruby-mode-hook
          (lambda () (rvm-activate-corresponding-ruby)))

(provide 'setup-ruby-mode)
