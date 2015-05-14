(require 'auto-complete-config)

(ac-config-default)

(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(setq ac-ignore-case nil)

;;; use quick help
(setq ac-use-quick-help t
      ac-quick-help-delay 0.5)

;;; complete in string
(delete 'font-lock-string-face ac-disable-faces)

(provide 'setup-autocomplete)
