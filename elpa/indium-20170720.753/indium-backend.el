;;; indium-backend.el --- Backend for indium.el          -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Generic backend implementation.

;; Backends should define a new backend symbol using `indium-register-backend'.
;; Once a connection to a JavaScript runtime is established by the backend, it
;; should set `indium-current-connection'.

;;; Code:

(require 'map)
(require 'seq)
(require 'indium-debugger-litable)
(eval-and-compile (require 'indium-structs))

(declare 'indium-debugger-unset-current-buffer)

(defgroup indium-backend nil
  "Indium backend."
  :prefix "indium-backend-"
  :group 'indium)

(defcustom indium-connection-open-hook nil
  "Hook called after a connection is open."
  :group 'indium-backend
  :type 'hook)

(defcustom indium-connection-closed-hook nil
  "Hook called after a connection is closed."
  :group 'indium-backend
  :type 'hook)

(defvar indium-backends nil "List of registered backends.")

(defun indium-register-backend (backend)
  "Register a new BACKEND.
BACKEND should be a symbol."
  (add-to-list 'indium-backends backend))

(defun indium-quit ()
  "Close the current connection and kill its REPL buffer if any.
When called interactively, prompt for a confirmation first."
  (interactive)
  (unless-indium-connected
    (user-error "No active connection to close"))
  (when (or (not (called-interactively-p 'interactive))
            (y-or-n-p (format "Do you really want to close the connection to %s ? "
                              (indium-current-connection-url))))
    (indium-backend-close-connection (indium-current-connection-backend))
    (indium-backend-cleanup-buffers)
    (setq indium-current-connection nil)))

(defun indium-reconnect ()
  "Try to re-establish a connection.
The new connection is based on the current (usually closed) one."
  (interactive)
  (unless-indium-connected
    (user-error "No Indium connection to reconnect to"))
  (indium-backend-reconnect (indium-current-connection-backend)))

(declare-function indium-repl-get-buffer "indium-repl.el")
(declare-function indium-debugger-unset-current-buffer "indium-debugger.el")

(defun indium-backend-cleanup-buffers ()
  "Cleanup all Indium buffers."
  (seq-map (lambda (buf)
             (with-current-buffer buf
               (when buffer-file-name
                 (indium-debugger-unset-current-buffer))))
           (buffer-list))
  (when-let ((buf (indium-repl-get-buffer)))
    (kill-buffer buf)))

(cl-defgeneric indium-backend-active-connection-p (_backend)
  "Return non-nil if the current connection is active."
  t)

(cl-defgeneric indium-backend-close-connection (_backend)
  "Close the current connection.")

(cl-defgeneric indium-backend-reconnect (_backend)
  "Try to re-establish a connection.
The new connection is created based on the current
`indium-current-connection'.")

(cl-defgeneric indium-backend-evaluate (backend string &optional callback)
  "Evaluate STRING then call CALLBACK.
CALLBACK is called with two arguments, the value returned by the
evaluation and non-nil if the evaluation threw an error.

The value should be an alist with a the following required keys:
`type', `value' and `description'.  If the value represents a
remote object that can be inspected, it should also have an
`objectid' key.")

(cl-defgeneric indium-backend-get-completions (backend expression prefix callback)
  "Get the completion for EXPRESSION that match PREFIX.
Evaluate CALLBACK on the filtered candidates.

EXPRESSION should be a valid JavaScript expression string.")

(cl-defgeneric indium-backend-add-breakpoint (backend location &optional callback condition)
  "Request the addition of a breakpoint.

The breakpoint is added at LOCATION.  When CALLBACK is
non-nil, evaluate it with the breakpoint's id.

Concrete implementations should call
`indium-backend-register-breakpoint' once the addition has been
performed.")

(cl-defgeneric indium-backend-remove-breakpoint (backend id)
  "Request the removal of the breakpoint with id ID.

Concrete implementations should call
`indium-backend-unregister-breakpoint' once the removal has been
performed.")

(defun indium-backend-remove-all-breakpoints-from-buffer (buffer)
  "Remove all breakpoints from BUFFER."
  (with-current-buffer buffer
    (seq-do (lambda (brk)
              (indium-backend-remove-breakpoint (indium-current-connection-backend)
                                                (map-elt brk 'id)))
            (indium-backend-get-breakpoints-in-file buffer-file-name))))

(defun indium-backend-register-breakpoint (id line file condition)
  "Register the breakpoint with ID at LINE in FILE and CONDITION.

Breakpoints are registered locally in the current connection so
that if a buffer later visits FILE with `indium-interaction-mode'
turned on, the breakpoint can be added back to the buffer."
  (let ((breakpoint `((line . ,line)
                      (file . ,file)
                      (condition . ,condition))))
    (map-put (indium-current-connection-breakpoints) id breakpoint)))

(defun indium-backend-unregister-breakpoint (id)
  "Remove the breakpoint with ID from the current connection."
  (map-delete (indium-current-connection-breakpoints) id))

(defun indium-backend-get-breakpoints ()
  "Return all breakpoints in the current connection.
A breakpoint is an alist with the keys `id', `file', `line' and
`condition'."
  (let ((breakpoints (indium-current-connection-breakpoints)))
    (map-keys-apply (lambda (key)
                      `((id . ,key)
                        (file . ,(map-nested-elt breakpoints `(,key file)))
                        (line . ,(map-nested-elt breakpoints `(,key line)))
                        (condition . ,(map-nested-elt breakpoints `(,key condition)))))
                    breakpoints)))

(cl-defgeneric indium-backend-deactivate-breakpoints (backend)
  "Deactivate all breakpoints.
The runtime will not pause on any breakpoint."
  )

(cl-defgeneric indium-backend-activate-breakpoints (backend)
  "Deactivate all breakpoints.
The runtime will not pause on any breakpoint."
  )

(defun indium-backend-get-breakpoints-in-file (file &optional line)
  "Return all breakpoints in FILE at LINE.
If LINE is not provided, return all breakpoints in FILE."
  (let ((breakpoints (indium-backend-get-breakpoints)))
    (seq-filter (lambda (brk)
                  (and (string= (map-elt brk 'file) file)
                       (or (not line)
                           (= (map-elt brk 'line) line))))
                breakpoints)))

(defun indium-backend-get-breakpoint (id)
  "Return the breakpoint with ID.
If not found, return nil."
  (let ((breakpoints (indium-backend-get-breakpoints)))
    (seq-find (lambda (brk)
                (eq (map-elt brk 'id) id))
              breakpoints)))

(cl-defgeneric indium-backend-set-script-source (backend url source &optional callback)
  "Update the contents of the script at URL to SOURCE.")

(cl-defgeneric indium-backend-get-properties (backend reference &optional callback all-properties)
  "Request the properties of the remote object represented by REFERENCE.
REFERENCE must be the id of a remote object.
CALLBACK is called with the fetched list of properties.

If ALL-PROPERTIES is non-nil, get all the properties from the
prototype chain of the remote object.")

(cl-defgeneric indium-backend-get-script-source (backend frame callback)
  "Get the source of the script for FRAME.
Evaluate CALLBACK with the result.")

(cl-defgeneric indium-backend-resume (backend &optional callback)
  "Resume the debugger and evaluate CALLBACK if non-nil.")

(cl-defgeneric indium-backend-step-into (backend &optional callback)
  "Step into the current stack frame and evaluate CALLBACK if non-nil.")

(cl-defgeneric indium-backend-step-out (backend &optional callback)
  "Step out the current stack frame and evaluate CALLBACK if non-nil.")

(cl-defgeneric indium-backend-step-over (backend &optional callback)
  "Step over the current stack frame and evaluate CALLBACK if non-nil.")

(cl-defgeneric indium-backend-continue-to-location (backend location &optional callback)
  "Continue to LOCATION and evaluate CALLBACK if non-nil.")

(defun indium-backend-object-reference-p (value)
  "Return non-nil if VALUE is a reference to a remote object."
  (map-elt value 'objectid))

(provide 'indium-backend)
;;; indium-backend.el ends here
