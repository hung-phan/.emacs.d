(require 'nodejs-repl)
(require 'babel-repl)
(require 'setup-tide)

(defun send-region-to-nodejs-repl-process (start end)
  "Send region to `nodejs-repl' process."
  (interactive "r")
  (comint-send-region (get-process nodejs-repl-process-name)
                      start end))

(setq-default js2-basic-offset 2
              js2-bounce-indent-p nil)

;; Disable js2 mode's syntax error highlighting by default...
(setq-default js2-mode-show-parse-errors nil
              js2-mode-show-strict-warnings nil)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))

(add-hook 'js2-mode-hook 'setup-tide-mode)
(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook
          (lambda ()
            (push '("function" . ?λ) prettify-symbols-alist)
            (push '("yield" . ?⊢) prettify-symbols-alist)))

(provide 'setup-js-mode)
