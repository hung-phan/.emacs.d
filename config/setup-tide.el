(require 'tide)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1))

(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
;; see https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473 for the full list available options

(provide 'setup-tide)
