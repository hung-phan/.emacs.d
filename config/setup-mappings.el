(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen)

(colorvisa/set-up 'which-key
  (define-key global-map (kbd "<f2>")  'which-key-show-top-level))

(colorvisa/set-up 'escreen
  (define-key global-map (kbd "<f5>")  'escreen-create-screen)
  (define-key global-map (kbd "<f6>")  'escreen-kill-screen)
  (define-key global-map (kbd "<f7>")  'escreen-goto-prev-screen)
  (define-key global-map (kbd "<f8>")  'escreen-goto-next-screen))

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(windmove-default-keybindings)

(provide 'setup-mappings)
