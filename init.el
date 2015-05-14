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
  '(s                   ;; string processor
    dash                ;; list collection processor

    smooth-scrolling    ;; smooth scrolling
    magit               ;; control git from emacs
    paredit             ;; minor mode for editing parentheses
    undo-tree           ;; undo tree visualization
    nyan-mode           ;; nyan nyan
    evil                ;; evil mode for vim
    evil-leader					;; evil leader
    flycheck            ;; on the fly syntax checking
    flycheck-pos-tip    ;; display flycheck on tooltips
    flycheck-clojure    ;; display flycheck for clojure

    yasnippet           ;; code snipppets
    auto-complete       ;; auto complete package

    sublime-themes      ;; theme
    flx                 ;; fuzzy matching
    flx-ido             ;; fuzzy matching for ido
    ido-ubiquitous      ;; use ido nearly everywhere
    ido-hacks           ;; ido hack for emacs 24
    smex                ;; M-x interface with ido style

    slim-mode           ;; slim support
    haml-mode           ;; haml support

    golden-ratio        ;; auto resize window by golden ratio

    helm                ;; incremental completion and selection
    helm-descbinds      ;; A helm frontend for describe-bindings.
    helm-projectile     ;; explore large projects
    helm-flycheck       ;; explore large projects
    ac-helm

    dired-details       ;; dired enhancement
    dired-details+
    dired+
    dired-rainbow
    ))
(dolist (p colorvisa/packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; -----------------------------------------------------------------------------
;; Extra config
;; -----------------------------------------------------------------------------
(require 'setup-util)
(require 'setup-defaults)

(require 'setup-appearance)
(require 'setup-editing)
(require 'setup-mappings)

(require 'setup-smex)
(require 'setup-ido)
(require 'setup-flx)
(require 'setup-flycheck)
(require 'setup-paredit)
(require 'setup-golden-ratio)
(require 'setup-helm)
(require 'setup-evil)
(require 'setup-dired)
(require 'setup-yasnippet)
(require 'setup-autocomplete)
