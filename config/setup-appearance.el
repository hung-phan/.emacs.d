;; -----------------------------------------------------------------------------
;; Theme
;; -----------------------------------------------------------------------------
(require 'brin-theme)

;; -----------------------------------------------------------------------------
;; Appearance config
;; -----------------------------------------------------------------------------
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq frame-title-format '(buffer-file-name "%f" ("%b")))

;; -----------------------------------------------------------------------------
;; Show trailing white spaces
;; -----------------------------------------------------------------------------
(setq-default show-trailing-whitespace t)

;; -----------------------------------------------------------------------------
;; Smooth scrolling
;; -----------------------------------------------------------------------------
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1))) ;; Turn off mouse interface early in startup to avoid momentary display

;; -----------------------------------------------------------------------------
;; nyan-mode
;; -----------------------------------------------------------------------------
(colorvisa/set-up 'nyan-mode (nyan-mode 1) (setq nyan-bar-length 15))

;; -----------------------------------------------------------------------------
;; minor config
;; -----------------------------------------------------------------------------
(setq inhibit-startup-message t) ;; No splash screen
(setq visible-bell nil) ;; turn off visible bell
(setq ring-bell-function 'ignore) ;; also annoying square display

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2) ;; default tab width
(setq-default cursor-type 'bar)	;; set cursor to a thin vertical line instead of a little box
(global-hl-line-mode 1)	;; highlight current line
(setq-default highlight-tabs t) ;; highlight tabulations
(setq inhibit-startup-message t) ;; not display welcome message
(setq uniquify-buffer-name-style 'forward)
(show-paren-mode 1) ;; highlight matching paren
(setq-default show-trailing-whitespace t)
(setq ns-use-native-fullscreen nil) ;; full screen mode

(provide 'setup-appearance)
