(require 'tide)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (company-mode +1))

(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
;; see https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473 for the full list available options

(add-hook 'typescript-mode-hook 'setup-tide-mode)

(setq typescript-indent-level 2
      typescript-expr-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))

(provide 'setup-tide)
