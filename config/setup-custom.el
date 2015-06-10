(require 'smart-mode-line)
(require 'brin-theme)

;; smart mode line
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'dark)
(sml/setup)

;; Appearance config
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1))) ;; Turn off mouse interface early in startup to avoid momentary display

;; Show trailing white spaces
(setq-default show-trailing-whitespace t)

;; Smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; nyan-mode
(colorvisa/set-up 'nyan-mode (nyan-mode 1) (setq nyan-bar-length 15))

;; rainbow-delimiters mode
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; minor config
(setq inhibit-startup-message t) ;; No splash screen
(setq visible-bell nil) ;; turn off visible bell
(setq ring-bell-function 'ignore) ;; also annoying square display
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2) ;; default tab width
(setq-default cursor-type 'bar)	;; set cursor to a thin vertical line instead of a little box
(blink-cursor-mode -1)
(global-hl-line-mode 1)	;; highlight current line
(setq-default highlight-tabs t) ;; highlight tabulations
(setq inhibit-startup-message t) ;; not display welcome message
(setq uniquify-buffer-name-style 'forward)
(show-paren-mode 1) ;; highlight matching paren
(setq-default show-trailing-whitespace t)
(setq ns-use-native-fullscreen nil) ;; full screen mode
(setq tab-always-indent 'complete)
(setq inhibit-startup-screen t)

(provide 'setup-custom)
