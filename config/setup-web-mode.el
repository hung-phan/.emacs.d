(require 'web-mode)
(require 'setup-tide)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.marko\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; make web-mode play nice with smartparens
(setq web-mode-enable-auto-pairing nil)

(defun custom-web-mode-hook ()
  "Hooks for Web mode."
  (setq fill-column 0) ;; never enter new line

  ;; auto insert and tag when typing </
  (setq nxml-slash-auto-complete-flag t))

(add-hook 'web-mode-hook 'custom-web-mode-hook)
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(provide 'setup-web-mode)
