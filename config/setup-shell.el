(require 'shell-command)
(require 'exec-path-from-shell)
(require 'multi-term)

(setq multi-term-program "/bin/zsh")
(shell-command-completion-mode)
(exec-path-from-shell-initialize)

(custom-set-variables
 '(term-default-bg-color "#000000")  ;; background color (black)
 '(term-default-fg-color "#dddd00")) ;; foreground color (yellow)

;; C-d to kill buffer if process is dead.
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 10000)
            (setq show-trailing-whitespace nil)
            (setq yas-dont-activate t)))

(defun create-shell ()
    "creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

(defun create-shell-in-new-tab ()
  (interactive)
  (multi-term))

(defun kill-escreen-and-buffer ()
  (interactive)
  (kill-buffer)
  (escreen-kill-screen))

(define-key global-map (kbd "<f1>") 'create-shell-in-new-tab)
(define-key global-map (kbd "<f12>") 'kill-escreen-and-buffer)

(provide 'setup-shell)
