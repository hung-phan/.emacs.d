(colorvisa/set-up 'flx-ido (flx-ido-mode 1))

;; -----------------------------------------------------------------------------
;; sacrifices memory to make flx faster
;; -----------------------------------------------------------------------------
(setq gc-cons-threshold 100000000)

(provide 'setup-flx)
