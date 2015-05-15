(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen)

(define-key global-map (kbd "<f5>") 'escreen-create-screen)
(define-key global-map (kbd "<f6>") 'escreen-kill-screen)
(define-key global-map (kbd "<f7>") 'escreen-goto-prev-screen)
(define-key global-map (kbd "<f8>") 'escreen-goto-next-screen)

(windmove-default-keybindings)

(provide 'setup-mappings)
