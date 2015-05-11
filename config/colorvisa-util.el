;; -----------------------------------------------------------------------------
;; utility functions
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; add dir in "lib" folder to load path
;; -----------------------------------------------------------------------------
(defun colorvisa/add-lib (dir-name)
	"Append dir-name to ~/.emacs.d/lib/ and then add them to load-path"
	(add-to-list 'load-path (concat "~/.emacs.d/lib/" dir-name)))

;; -----------------------------------------------------------------------------
;; separate OS-specific stuff
;; Example:
;; (colorvisa/in '(darwin) something here) ;macos system
;; (colorvisa/in '(gnu/linux) something here) ;gnu linux system
;; -----------------------------------------------------------------------------
(defmacro colorvisa/in (systems &rest body)
	"Run BODY if `system-type' is in the list of SYSTEMS."
	(declare (indent 1))
	`(when (member system-type ,systems)
		 ,@body))

;; -----------------------------------------------------------------------------
;; a replacement for (require 'xxx)
;; https://github.com/ubolonton/.emacs.d/blob/master/config/ublt-editing.el
;; usage:
;; (colorvisa/set-up 'xxx
;;   body)
;; try to load (require the feature xxx) and then run the body (those commands
;; that go with xxx feature)
;; if can not load xxx feature, ignore it and continue running the rest of the
;; current file
;; -----------------------------------------------------------------------------
(defvar colorvisa/ok-features ())

(defvar colorvisa/error-features ())

(defun colorvisa/require (feature &optional filename noerror)
	(if noerror
		(condition-case err
										(progn
											(let ((name (require feature filename)))
												(add-to-list 'colorvisa/ok-features feature t)
												(message "Feature `%s' ok" feature)
												name))
										(error
											(setq colorvisa/error-features (plist-put colorvisa/error-features feature err))
											(message "Feature `%s' failed" feature)
											nil))
		(require feature filename)))

(defmacro colorvisa/set-up (feature &rest body)
	"Try loading the feature, running BODY afterward, notifying
	user if not found. This is mostly for my customizations, since I
	don't want a feature failing to load to affect other features in
	the same file. Splitting everything out would result in too many
	files."
	(declare (indent 1))
	`(let ((f (if (stringp ,feature) (intern ,feature) ,feature)))
		 (when (colorvisa/require f nil t)
			 ,@body)))

;; -----------------------------------------------------------------------------
;; enable all disabled functions in funcs list
;; -----------------------------------------------------------------------------
(defun colorvisa/enable (funcs)
	(dolist (f funcs)
		(put f 'disabled nil)))

;; -----------------------------------------------------------------------------
;; turn on and off function
;; -----------------------------------------------------------------------------
(defvar colorvisa/on-fns (make-hash-table))

(defun colorvisa/on-fn (minor-mode-fn)
	(let ((fn (gethash minor-mode-fn colorvisa/on-fns)))
		(if fn fn
			(puthash minor-mode-fn
							 `(lambda () (,minor-mode-fn +1))
							 colorvisa/on-fns))))

(defvar colorvisa/off-fns (make-hash-table))

(defun colorvisa/off-fn (minor-mode-fn)
	(let ((fn (gethash minor-mode-fn colorvisa/off-fns)))
		(if fn fn
			(puthash minor-mode-fn
							 `(lambda () (,minor-mode-fn -1))
							 colorvisa/off-fns))))

;; -----------------------------------------------------------------------------
;; show message in minibuffer
;; -----------------------------------------------------------------------------
(defun colorvisa/minibuffer-message (&rest args)
	"Show a message in the minibuffer without logging. Useful for
	transient messages like error messages when hovering over syntax
	errors."
	(let ((message-log-max nil))
		(apply #'message args)))

(provide 'colorvisa-util)
