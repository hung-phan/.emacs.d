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

;; (colorvisa/in '(darwin)
;;   (set-default-font "Hack"))
(colorvisa/in '(darwin)
  (set-default-font "Fira Code")
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                 )
               ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

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

(provide 'setup-custom)
