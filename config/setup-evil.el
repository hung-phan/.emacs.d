(evil-mode 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

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

;; -----------------------------------------------------------------------------
;; redefine key
;; -----------------------------------------------------------------------------
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map "<" 'indent-rigidly-left-to-tab-stop)
(define-key evil-visual-state-map ">" 'indent-rigidly-right-to-tab-stop)
(define-key evil-visual-state-map "\C-g" 'evil-normal-state)

(define-key evil-motion-state-map (kbd "RET") 'open-line-below)

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-normal-state-map (kbd "<") 'evil-indent-left)
(define-key evil-normal-state-map (kbd ">") 'evil-indent-right)
(define-key evil-normal-state-map (kbd "SPC b") 'ido-switch-buffer)
(define-key evil-normal-state-map (kbd "SPC B") 'ibuffer)
(define-key evil-normal-state-map (kbd "SPC d") 'duplicate-current-line-or-region)
(define-key evil-normal-state-map (kbd "SPC p") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "SPC P") 'helm-projectile)
(define-key evil-normal-state-map (kbd "SPC g s") 'magit-status)
(define-key evil-normal-state-map (kbd "SPC 1") 'delete-other-windows)
(define-key evil-normal-state-map (kbd "SPC 2") 'split-window-below)
(define-key evil-normal-state-map (kbd "SPC 3") 'split-window-right)
(define-key evil-normal-state-map (kbd "SPC 0") 'split-window-right)

;; esc to quit everything
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(define-key evil-insert-state-map "\C-g" 'evil-normal-state)

(evil-leader/set-key
  "w" 'save-buffer
  "q" 'evil-quit
  "s" 'projectile-run-async-shell-command-in-root)

(provide 'setup-evil)
