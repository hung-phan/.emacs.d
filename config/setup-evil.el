(require 'evil)
(require 'evil-leader)
(require 'evil-surround)
(require 'evil-matchit)
(require 'expand-region)

(evil-mode 1)
(global-evil-leader-mode)
(global-evil-surround-mode 1)
(global-evil-matchit-mode 1)
(evil-leader/set-leader ",")
(setq evil-magic 'very-magic)

;;; default state
(dolist (mode '(org-mode))
  (add-to-list 'evil-emacs-state-modes mode))

;; -----------------------------------------------------------------------------
;; copy from http://rejeep.github.io/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
;; -----------------------------------------------------------------------------
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun evil-indent-left ()
  "Indent left the current line."
  (interactive)
  (indent-rigidly-left-to-tab-stop (line-beginning-position) (line-end-position)))

(defun evil-indent-right ()
  "Indent right the current line."
  (interactive)
  (indent-rigidly-right-to-tab-stop (line-beginning-position) (line-end-position)))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

;; -----------------------------------------------------------------------------
;; redefine key
;; -----------------------------------------------------------------------------
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map (kbd "<") 'indent-rigidly-left-to-tab-stop)
(define-key evil-visual-state-map (kbd ">") 'indent-rigidly-right-to-tab-stop)
(define-key evil-visual-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "J") 'move-region-down)
(define-key evil-visual-state-map (kbd "K") 'move-region-up)

(define-key evil-motion-state-map (kbd "RET") 'open-line-below)

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-normal-state-map (kbd "<") 'evil-indent-left)
(define-key evil-normal-state-map (kbd ">") 'evil-indent-right)
(define-key evil-normal-state-map (kbd "SPC b") 'ido-switch-buffer)
(define-key evil-normal-state-map (kbd "SPC B") 'ibuffer)
(define-key evil-normal-state-map (kbd "SPC d") 'duplicate-current-line-or-region)
(define-key evil-normal-state-map (kbd "SPC p") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC P") 'helm-projectile)
(define-key evil-normal-state-map (kbd "SPC g") 'magit-status)
(define-key evil-normal-state-map (kbd "SPC 0") 'delete-window)
(define-key evil-normal-state-map (kbd "SPC 1") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "SPC 2") 'split-window-below)
(define-key evil-normal-state-map (kbd "SPC 3") 'split-window-right)
(define-key evil-normal-state-map (kbd "SPC SPC") 'other-window)
(define-key evil-normal-state-map (kbd "SPC /") 'helm-ag-project-root)
(define-key evil-normal-state-map (kbd "SPC v s") 'git-gutter:stage-hunk)
(define-key evil-normal-state-map (kbd "SPC v r") 'git-gutter:revert-hunk)

(define-key evil-normal-state-map (kbd "J") 'move-line-down)
(define-key evil-normal-state-map (kbd "K") 'move-line-up)

;; esc to quit everything
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line)
(define-key evil-insert-state-map (kbd "C-n") 'company-select-next)
(define-key evil-insert-state-map (kbd "C-p") 'company-select-previous)

(evil-leader/set-key
  "w" 'save-buffer
  "q" 'evil-quit
  "s" 'projectile-run-async-shell-command-in-root
  "=" 'er/expand-region)

(setq evil-mode-line-format 'before
      evil-normal-state-tag (propertize "« N »" 'face 'tmtxt/evil-normal-tag)
      evil-motion-state-tag (propertize "« M »" 'face 'tmtxt/evil-normal-tag)
      evil-insert-state-tag (propertize "« I »" 'face 'tmtxt/evil-insert-tag)
      evil-emacs-state-tag (propertize "« E »" 'face 'tmtxt/evil-emacs-tag)
      evil-visual-state-tag (propertize "« ∞ »" 'face 'tmtxt/evil-visual-tag)
      evil-motion-state-cursor '(box "YellowGreen")
      evil-normal-state-cursor '(box "YellowGreen")
      evil-insert-state-cursor '(bar "White")
      evil-emacs-state-cursor '(bar "White")
      evil-visual-state-cursor '(box "#F86155"))

(provide 'setup-evil)
