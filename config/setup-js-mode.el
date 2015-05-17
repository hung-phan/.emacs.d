(require 'nodejs-repl)

(defun send-region-to-nodejs-repl-process (start end)
  "Send region to `nodejs-repl' process."
  (interactive "r")
  (save-selected-window
    (save-excursion (nodejs-repl)))
  (comint-send-region (get-process nodejs-repl-process-name)
                      start end))

(add-hook 'js-mode-hook 'tern-mode)
(add-hook 'js-mode-hook 'js2-mode)

(provide 'setup-js-mode)
