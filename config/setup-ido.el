(require 'ido)
(require 'ido-ubiquitous)
(require 'ido-hacks)

;; -----------------------------------------------------------------------------
;; ido configs
;; -----------------------------------------------------------------------------
(ido-mode t)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

;; -----------------------------------------------------------------------------
;; Set buffer separator in the mini buffer when press C-x b (ido-switch-buffer)
;; to new line instead of the character | so that it can be easy to read
;; -----------------------------------------------------------------------------
(setq ido-decorations
  '("\n=> " "" "\n" "" "[" "]" " [No match]" " [Matched]" " [Not readable]"
    " [Too big]" " [Confirm]"))
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(provide 'setup-ido)
