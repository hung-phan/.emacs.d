(require 'nodejs-repl)
(require 'babel-repl)

(defun send-region-to-nodejs-repl-process (start end)
  "Send region to `nodejs-repl' process."
  (interactive "r")
  (comint-send-region (get-process nodejs-repl-process-name)
                      start end))

(js2r-add-keybindings-with-prefix "C-c C-m")
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . js2-mode))
(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'js2-mode-hook 'js2-refactor-mode)
(add-hook 'js2-mode-hook
          (lambda ()
            (push '("function" . ?λ) prettify-symbols-alist)
            (push '("yield" . ?⊢) prettify-symbols-alist)))

(provide 'setup-js-mode)
