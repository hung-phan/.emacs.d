(require 'nodejs-repl)
(require 'babel-repl)

(setq nodejs-repl-arguments
      '("--use-strict"
        "--es_staging"
        "--harmony"
        "--harmony_shipping"
        "--harmony_modules"
        "--harmony_arrays"
        "--harmony_array_includes"
        "--harmony_regexps"
        "--harmony_arrow_functions"
        "--harmony_proxies"
        "--harmony_sloppy"
        "--harmony_unicode"
        "--harmony_tostring"
        "--harmony_classes"
        "--harmony_object_literals"
        "--harmony_numeric_literals"
        "--harmony_strings"
        "--harmony_scoping"
        "--harmony_templates"))

(defun send-region-to-nodejs-repl-process (start end)
  "Send region to `nodejs-repl' process."
  (interactive "r")
  (comint-send-region (get-process nodejs-repl-process-name)
                      start end))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . js2-mode))
(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'js2-mode-hook
          (lambda ()
            (push '("function" . ?λ) prettify-symbols-alist)
            (push '("yield" . ?γ) prettify-symbols-alist)))

(provide 'setup-js-mode)
