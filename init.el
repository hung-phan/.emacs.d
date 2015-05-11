(setq user-full-name "Hung Phan")
(setq user-mail-address "colorvisavn@gmail.com")

;; -----------------------------------------------------------------------------
;; define some load path here
;; -----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/config/")

;; -----------------------------------------------------------------------------
;; emacs's package manager
;; -----------------------------------------------------------------------------
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
			("org" . "http://orgmode.org/elpa/")
			("elpa" . "http://tromey.com/elpa/")
			("melpa-stable" . "http://stable.melpa.org/packages/")
			("melpa" . "http://melpa.org/packages/")
			("elpy" . "http://jorgenschaefer.github.io/packages/")))
	(add-to-list 'package-archives source t))
(package-initialize)

;; -----------------------------------------------------------------------------
;; Required packages
;; everytime emacs starts, it will automatically check if those packages are
;; missing, it will install them automatically
;; -----------------------------------------------------------------------------
(when (not package-archive-contents)
	(package-refresh-contents))
(defvar colorvisa/packages
	'(auto-complete       ;; auto complete package
		magit               ;; control git from emacs
		yasnippet           ;; code snipppets
		undo-tree           ;; undo tree visualization
		nyan-mode           ;; nyan nyan
		evil                ;; evil mode for vim
		flycheck            ;; on the fly syntax checking
		flycheck-pos-tip    ;; display fly check on tooltips

		sublime-themes      ;; theme
		flx                 ;; fuzzy matching
		flx-ido             ;; fuzzy matching for ido
		ido-ubiquitous      ;; use ido nearly everywhere
		ido-hacks           ;; ido hack for emacs 24
		smex                ;; M-x interface with ido style

		slim-mode           ;; slim support
		haml-mode           ;; haml support
		))
(dolist (p colorvisa/packages)
	(when (not (package-installed-p p))
		(package-install p)))

;; -----------------------------------------------------------------------------
;; Extra config
;; -----------------------------------------------------------------------------
(require 'colorvisa-util)

(require 'colorvisa-appearance)
(require 'colorvisa-applications)
(require 'colorvisa-editing)

(require 'colorvisa-smex)
(require 'colorvisa-ido)
(require 'colorvisa-flx)
(require 'colorvisa-evil)
(require 'colorvisa-flycheck)
(require 'colorvisa-dired)
