(require 'ace-jump-mode)
(require 'evil)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

(define-key evil-normal-state-map (kbd "<f2>") 'ace-jump-mode)

(provide 'setup-ace-jump)
