;; -----------------------------------------------------------------------------
;; set default username and email
;; -----------------------------------------------------------------------------
(setq user-full-name "Hung Phan")
(setq user-mail-address "colorvisavn@gmail.com")

;; -----------------------------------------------------------------------------
;; define some load path here
;; -----------------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/theme/")
(add-to-list 'load-path "~/.emacs.d/libs/")
(add-to-list 'load-path "~/.emacs.d/config/")

;; -----------------------------------------------------------------------------
;; emacs's package manager
;; -----------------------------------------------------------------------------
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("org" . "http://orgmode.org/elpa/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("melpa-stable" . "http://stable.melpa.org/packages/")
                  ("melpa" . "http://melpa.org/packages/")))
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
    git-timemachine      ;; git history
    gist

    which-key
    windmove             ;; fast window jum
    switch-window
    escreen              ;; multiple tab like vim
    undo-tree            ;; undo tree visualization
    nyan-mode            ;; nyan nyan
    smart-mode-line

    rainbow-mode         ;; color visualization
    rainbow-delimiters   ;; colorful bracket

    ace-jump-mode        ;; fast word jump
    multiple-cursors     ;; multiple cursor support
    move-text            ;; move text util
    expand-region        ;; fast select
    paredit              ;; minor mode for editing parentheses
    paredit-everywhere
    textmate
    anzu

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

    dash-at-point        ;; dash-at-point

    org
    org-present

    helm                 ;; incremental completion and selection
    helm-swoop           ;; search functionality
    helm-descbinds       ;; A helm frontend for describe-bindings.
    helm-projectile      ;; explore large projects
    helm-flycheck        ;; explore large projects
    helm-ag              ;; silver search
    ac-helm

    dired-details        ;; dired enhancement
    dired-details+
    dired+
    dired-hacks-utils
    dired-sort-menu

    tern
    tide
    company-tern
    nodejs-repl
    json-mode
    js2-mode             ;; improve js editing
    indium

    company-ghc
    haskell-mode

    scala-mode
    ensime

    clojure-mode
    clojure-test-mode
    cider                ;; clojure repl
    clj-refactor

    dockerfile-mode

    jade-mode
    stylus-mode

    company-jedi         ;; python

    robe                 ;; ruby
    rvm
    enh-ruby-mode
    inf-ruby
    yaml-mode
    rspec-mode

    shell-command        ;; shell
    exec-path-from-shell
    multi-term

    restclient))

(dolist (p colorvisa/packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; -----------------------------------------------------------------------------
;; Custom dependencies
;; -----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(helm-ag-insert-at-point (quote symbol))
 '(helm-ag-use-temp-buffer t)
 '(package-selected-packages
   (quote
    (restclient multi-term exec-path-from-shell shell-command rspec-mode yaml-mode enh-ruby-mode rvm robe company-jedi stylus-mode jade-mode dockerfile-mode clj-refactor clojure-test-mode ensime scala-mode company-ghc js2-mode indium json-mode nodejs-repl company-tern tide tern dired-sort-menu dired-hacks-utils dired+ dired-details+ dired-details ac-helm helm-ag helm-flycheck helm-projectile helm-descbinds helm-swoop helm org-present dash-at-point markdown-mode+ markdown-mode web-mode scss-mode less-css-mode haml-mode slim-mode smex ido-hacks ido-ubiquitous flx-ido flx company yasnippet flycheck-clojure flycheck-pos-tip flycheck evil-matchit evil-surround evil-leader evil-god-state god-mode anzu textmate paredit-everywhere paredit expand-region move-text multiple-cursors ace-jump-mode rainbow-delimiters rainbow-mode smart-mode-line nyan-mode undo-tree escreen switch-window which-key gist git-timemachine git-gutter magit dash s)))
 '(term-default-bg-color "#000000")
 '(term-default-fg-color "#dddd00"))
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

;; -----------------------------------------------------------------------------
;; Extra config
;; -----------------------------------------------------------------------------
(require 'setup-util)

(require 'prettier-js)

(require 'setup-defaults)
(require 'setup-custom)
(require 'setup-tide)

(require 'setup-smex)
(require 'setup-ido)
(require 'setup-flx)
(require 'setup-flycheck)
(require 'setup-paredit)
(require 'setup-helm)
(require 'setup-dired)
(require 'setup-yasnippet)
(require 'setup-autocomplete)
(require 'setup-org)
(require 'setup-anzu)

(require 'setup-magit)
(require 'setup-gitgutter)
(require 'setup-multiple-cursors)
(require 'setup-ace-jump)
(require 'setup-escreen)
(require 'setup-evil)
(require 'setup-god-mode)
(require 'setup-shell)
(require 'setup-ruby-mode)
(require 'setup-python-mode)
(require 'setup-clojure-mode)
(require 'setup-js-mode)
(require 'setup-web-mode)

(require 'setup-editing)
(require 'setup-mappings)

(provide 'init)
