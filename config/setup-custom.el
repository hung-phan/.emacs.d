(require 'comint)

;; load theme
(colorvisa/set-up 'dracula-theme
  (load-theme 'dracula t))

;; smart mode line
(colorvisa/set-up 'smart-mode-line
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'dark)
  (sml/setup))

;; nyan-mode
(colorvisa/set-up 'nyan-mode
  (nyan-mode 1)
  (setq nyan-bar-length 15))

;; which-key
(colorvisa/set-up 'which-key
  (which-key-mode))

;; switch-window
(colorvisa/set-up 'switch-window
  (global-set-key (kbd "C-x o") 'switch-window))

;; fix tramp error
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

;; undo tree
(colorvisa/set-up 'undo-tree
  (setq undo-tree-mode-lighter "")
  (global-undo-tree-mode))

(colorvisa/in '(darwin)
  (set-default-font "Hack"))

;; rainbow-delimiters mode
(colorvisa/set-up 'rainbow-delimiters
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Appearance config
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  ;; Turn off mouse interface early in startup to avoid momentary display
  (when (fboundp mode) (funcall mode -1)))

;; Show trailing white spaces
(setq-default show-trailing-whitespace t)

;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; minor config
(global-prettify-symbols-mode 1)
(setq visible-bell nil) ;; turn off visible bell
(setq ring-bell-function 'ignore) ;; also annoying square display
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2) ;; default tab width
(setq-default cursor-type 'bar)	;; set cursor to a thin vertical line instead of a little box
(blink-cursor-mode -1)
(global-hl-line-mode 1)	;; highlight current line
(setq-default highlight-tabs t) ;; highlight tabulations
(setq uniquify-buffer-name-style 'forward)
(show-paren-mode 1) ;; highlight matching paren
(setq-default show-trailing-whitespace t)
(setq ns-use-native-fullscreen nil) ;; full screen mode
(setq tab-always-indent 'complete)
(setq inhibit-startup-screen t)

;; solution for weird characters in comint-mode
(add-to-list 'comint-preoutput-filter-functions
             (lambda (output)
               (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#3d4053"))))
 '(company-scrollbar-fg ((t (:background "#323544"))))
 '(company-tooltip ((t (:inherit default :background "#2c2e3b"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face)))))

(provide 'setup-custom)
