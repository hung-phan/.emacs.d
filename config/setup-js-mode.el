(require 'nodejs-repl)

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

(add-hook 'js-mode-hook 'tern-mode)
(add-hook 'js-mode-hook 'js2-mode)

(provide 'setup-js-mode)
