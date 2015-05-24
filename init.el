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
  '(s                    ;; string processing
    dash                 ;; list collection processor

    magit                ;; control git from emacs
    git-gutter           ;; git gutter

    windmove             ;; fast window jum
    escreen              ;; multiple tab like vim
    undo-tree            ;; undo tree visualization
    nyan-mode            ;; nyan nyan

    rainbow-mode         ;; color visualization
    rainbow-delimiters   ;; colorful bracket

    ace-jump-mode        ;; fast word jump
    multiple-cursors     ;; multiple cursor support
    move-text            ;; move text util
    expand-region        ;; fast select
    paredit              ;; minor mode for editing parentheses
    paredit-everywhere

    god-mode
    evil-god-state
    evil                 ;; evil mode for vim
    evil-leader          ;; evil leader
    evil-surround        ;; evil  surrond
    evil-matchit         ;; evil matchit

    flycheck             ;; on the fly syntax checking
    flycheck-pos-tip     ;; display flycheck on tooltips
    flycheck-clojure     ;; display flycheck for clojure

    yasnippet            ;; code snipppets
    company              ;; auto complete package

    sublime-themes       ;; theme
    flx                  ;; fuzzy matching
    flx-ido              ;; fuzzy matching for ido

    ido                  ;; use ido nearly everywhere
    ido-ubiquitous       ;; use ido nearly everywhere
    ido-hacks            ;; ido hack for emacs 24
    smex                 ;; M-x interface with ido style

    slim-mode            ;; slim support
    haml-mode            ;; haml support
    css-mode             ;; css
    less-css-mode        ;; less support
    scss-mode            ;; scss support
    web-mode

    markdown-mode        ;; markdown
    markdown-mode+

    org
    org-trello           ;; trello mode

    golden-ratio         ;; auto resize window by golden ratio

    helm                 ;; incremental completion and selection
    helm-descbinds       ;; A helm frontend for describe-bindings.
    helm-projectile      ;; explore large projects
    helm-flycheck        ;; explore large projects
    helm-ag              ;; silver search
    ac-helm

    dired-details        ;; dired enhancement
    dired-details+
    dired+
    dired-hacks-utils
    dired-rainbow

    tern
    company-tern
    js2-mode             ;; improve js editing
    nodejs-repl
    jsx-mode
    coffee-mode

    clojure-mode
    clojure-test-mode
    cider                ;; clojure repl

    robe                 ;; ruby
    rvm
    enh-ruby-mode
    inf-ruby
    yaml-mode

    shell-command        ;; shell
    exec-path-from-shell
    multi-term

    restclient))

(dolist (p colorvisa/packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; -----------------------------------------------------------------------------
;; Extra config
;; -----------------------------------------------------------------------------
(require 'setup-util)

(require 'setup-defaults)
(require 'setup-custom)

(require 'setup-smex)
(require 'setup-ido)
(require 'setup-flx)
(require 'setup-flycheck)
(require 'setup-paredit)
(require 'setup-golden-ratio)
(require 'setup-helm)
(require 'setup-dired)
(require 'setup-yasnippet)
(require 'setup-autocomplete)

(require 'setup-shell)
(require 'setup-ruby-mode)
(require 'setup-clojure-mode)
(require 'setup-js-mode)
(require 'setup-web-mode)
(require 'setup-magit)
(require 'setup-gitgutter)
(require 'setup-multiple-cursors)
(require 'setup-ace-jump)
(require 'setup-escreen)
(require 'setup-undo-tree)
(require 'setup-evil)
(require 'setup-god-mode)

(require 'setup-editing)
(require 'setup-mappings)

(provide 'init)
