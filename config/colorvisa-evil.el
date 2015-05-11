;; -----------------------------------------------------------------------------
;; Use evil mode by default
;; -----------------------------------------------------------------------------
(defgroup colorvisa-evil nil
  "Configuration options for evil-mode."
  :group 'colorvisa
  :prefix 'colorvisa-evil)

(defcustom colorvisa-evil/evil-state-modes
  '(fundamental-mode
    text-mode
    prog-mode
    dired-mode
    comint-mode
    compilation-mode)
  "List of modes that should start up in Evil state."
  :type '(repeat (symbol))
  :group 'colorvisa-evil)

(defcustom colorvisa-evil/emacs-state-modes
  '(git-commit-mode
    git-rebase-mode)
  "List of modes that should start up in Evil Emacs state."
  :type '(repeat (symbol))
  :group 'colorvisa-evil)

(defcustom colorvisa-evil/emacs-cursor
  "red"
  "The color of the cursor when in Emacs state."
  :type 'color)

(evil-mode 1)
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))
(setq evil-search-module 'evil-search)
(setq evil-magic 'very-magic)
(unless (display-graphic-p) (evil-esc-mode))

(provide 'colorvisa-evil)
