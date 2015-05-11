(evil-mode 1)

;; not use evil insert state, prefer emacs state
(add-hook 'evil-insert-state-entry-hook (lambda ()
                                          (evil-exit-emacs-state)
                                          (evil-emacs-state 1)))

;; default state
(dolist (mode '(git-commit-mode
                 dired-mode
                 wdired-mode
                 twittering-mode
                 twittering-edit-mode
                 skewer-repl-mode
                 fundamental-mode
                 inferior-moz-mode
                 process-menu-mode
                 cider-repl-mode
                 sql-interactive-mode))
  (add-to-list 'evil-emacs-state-modes mode))

(dolist (mode '())
  (add-to-list 'evil-motion-state-modes mode))

(dolist (mode '(Info-mode))
  (add-to-list 'evil-normal-state-modes mode))

;; use jj to switch from insert state to normal or motion state
(colorvisa/set-up 'key-chord
                  (key-chord-mode 1)
                  (setq key-chord-one-key-delay 0.5)
                  (defun colorvisa/evil-exit-insert-state ()
                    "Exit evil insert state and change to normal or motion mode"
                    (interactive)
                    (cond                               ;decide which state to switch to
                      ;; switch these mode to motion state
                      ((member major-mode '(dired-mode)) (evil-motion-state 1))

                      ;; do nothing for these mode
                      ((member major-mode '(twittering-mode)))

                      ;; default switch to normal state
                      (t (evil-normal-state 1))))
                  (key-chord-define evil-emacs-state-map "jj" 'colorvisa/evil-exit-insert-state))

;; go to promt when enter insert state for repl mode
(defun colorvisa/repl-goto-prompt ()
  (when (member major-mode
                '(eshell-mode
                   sql-interactive-mode
                   inferior-moz-mode
                   inferior-emacs-lisp-mode
                   inferior-lisp-mode
                   cider-repl-mode
                   skewer-repl-mode))
    (goto-char (point-max))))
(add-hook 'evil-emacs-state-entry-hook 'colorvisa/repl-goto-prompt)

;; evil indicator
(defface colorvisa/evil-insert-tag
         `((t (:inherit font-lock-comment-delimiter-face :slant normal :weight bold)))
         "Evil insert mode indicator face")
(defface colorvisa/evil-normal-tag
         `((t (:inherit diredp-mode-line-flagged :weight bold :foreground "#C88BA7")))
         "Evil normal mode indicator face")
(defface colorvisa/evil-emacs-tag
         `((t (:inherit diredp-mode-line-marked :weight bold :foreground "#2E9B98")))
         "Evil emacs mode indicator face")
(defface colorvisa/evil-visual-tag
         `((t (:inherit font-lock-preprocessor-face)))
         "Evil visual mode indicator face")

(setq evil-mode-line-format 'before
      evil-normal-state-tag (propertize "« N »" 'face 'colorvisa/evil-normal-tag)
      evil-motion-state-tag (propertize "« M »" 'face 'colorvisa/evil-normal-tag)
      evil-insert-state-tag (propertize "« I »" 'face 'colorvisa/evil-insert-tag)
      evil-emacs-state-tag (propertize "« E »" 'face 'colorvisa/evil-emacs-tag)
      evil-visual-state-tag (propertize "« ∞ »" 'face 'colorvisa/evil-visual-tag)
      evil-motion-state-cursor '(box "YellowGreen")
      evil-normal-state-cursor '(box "YellowGreen")
      evil-insert-state-cursor '(bar "White")
      evil-emacs-state-cursor '(bar "White")
      evil-visual-state-cursor '(box "#F86155"))

(colorvisa/set-up 'evil-nerd-commenter
                  (evilnc-default-hotkeys))

(colorvisa/set-up 'git-messenger
                  (add-hook 'git-messenger:after-popup-hook
                            (lambda ()
                              (evil-exit-emacs-state))))

(colorvisa/set-up 'evil-matchit
                  (global-evil-matchit-mode 1))

(provide 'colorvisa-evil)
