(require 'undo-tree)

;; Represent undo-history as an actual tree (visualize with C-x u)
(setq undo-tree-mode-lighter "")
(global-undo-tree-mode)

(provide 'setup-undo-tree)
