;;; subtle-mpv.el --- mpv process and JSON IPC -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <you@example.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of subtle; see subtle.el for the license notice.

;;; Commentary:

;; Starts mpv with an IPC socket, observes time-pos / pause / speed and
;; turns mpv events into messages: (time-pos . SECONDS), (pause . BOOL),
;; (speed . NUMBER).  Pure helpers are separate from the process code.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'subtle-custom)

(cl-defstruct (subtle-mpv (:constructor subtle-mpv--make))
  process connection socket on-message on-exit)

(defconst subtle-mpv--observed '("time-pos" "pause" "speed"))
(defconst subtle-mpv--connect-interval 0.05
  "Seconds between attempts to connect to the socket of mpv.")

;;; Pure helpers

(defun subtle-mpv--args (media socket)
  "Command-line arguments for playing MEDIA with IPC on SOCKET.
The user's `subtle-mpv-extra-args' come after the others, so they win."
  (append subtle-mpv-base-args
          (list (concat "--input-ipc-server=" socket))
          (and (eq subtle-video 'never) (list "--vid=no"))
          (and subtle-keep-focus (list "--focus-on=never"))
          subtle-mpv-extra-args
          (list "--" media)))

(defun subtle-mpv--attempts ()
  "Number of connection attempts that fit in `subtle-mpv-connect-timeout'."
  (ceiling subtle-mpv-connect-timeout subtle-mpv--connect-interval))

(defun subtle-mpv--socket-name ()
  "A fresh socket path under the variable `temporary-file-directory'.\nThe file itself is not made."
  (make-temp-name (expand-file-name "subtle-" temporary-file-directory)))

(defun subtle-mpv--command (args)
  "JSON line asking mpv to run the command ARGS."
  (concat (json-serialize `((command . ,(vconcat args)))) "\n"))

(defun subtle-mpv--split-lines (pending chunk)
  "Cons of complete lines and the unfinished rest of PENDING + CHUNK."
  (let ((parts (split-string (concat pending chunk) "\n")))
    (cons (butlast parts) (car (last parts)))))

(defun subtle-mpv--decode (line)
  "Alist parsed from JSON LINE, or nil if it is not valid JSON."
  (ignore-errors
    (json-parse-string line :object-type 'alist :null-object nil :false-object nil)))

(defun subtle-mpv--event->msg (object)
  "Message for the mpv event OBJECT, or nil if we do not care."
  (pcase (alist-get 'event object)
    ("property-change" (cons (intern (alist-get 'name object))
                             (alist-get 'data object)))
    (_ nil)))

;;; Process

(defun subtle-mpv-send (mpv &rest args)
  "Send the command ARGS to MPV; do nothing while it is not connected."
  (when-let* ((conn (subtle-mpv-connection mpv))
              ((process-live-p conn)))
    (process-send-string conn (subtle-mpv--command args))))

(defun subtle-mpv--filter (mpv conn chunk)
  "Handle CHUNK of output from CONN, forwarding messages for MPV."
  (pcase-let ((`(,lines . ,rest)
               (subtle-mpv--split-lines (or (process-get conn 'pending) "") chunk)))
    (process-put conn 'pending rest)
    (dolist (msg (delq nil (mapcar #'subtle-mpv--line->msg lines)))
      (funcall (subtle-mpv-on-message mpv) msg))))

(defun subtle-mpv--line->msg (line)
  "Message for one JSON LINE of mpv output, or nil."
  (thread-last line subtle-mpv--decode subtle-mpv--event->msg))

(defun subtle-mpv--open-connection (mpv)
  "Connect to the socket of MPV and start observing properties."
  (setf (subtle-mpv-connection mpv)
        (make-network-process :name "subtle-ipc" :family 'local :noquery t
                              :service (subtle-mpv-socket mpv) :coding 'utf-8
                              :filter (lambda (conn chunk)
                                        (subtle-mpv--filter mpv conn chunk))))
  (seq-do-indexed (lambda (name id) (subtle-mpv-send mpv "observe_property" id name))
                  subtle-mpv--observed))

(defun subtle-mpv--connect (mpv attempt)
  "Connect MPV once its socket exists.
Retry later; ATTEMPT counts the attempts so far."
  (cond ((not (process-live-p (subtle-mpv-process mpv))) nil)
        ((file-exists-p (subtle-mpv-socket mpv)) (subtle-mpv--open-connection mpv))
        ((>= attempt (subtle-mpv--attempts)) (funcall (subtle-mpv-on-exit mpv)))
        (t (run-at-time subtle-mpv--connect-interval nil
                        #'subtle-mpv--connect mpv (1+ attempt)))))

(defun subtle-mpv--spawn (mpv media)
  "Start the mpv process of MPV playing MEDIA."
  (make-process :name "subtle-mpv" :buffer nil :noquery t :connection-type 'pipe
                :command (cons subtle-mpv-executable
                               (subtle-mpv--args media (subtle-mpv-socket mpv)))
                :sentinel (lambda (process _event)
                            (unless (process-live-p process)
                              (funcall (subtle-mpv-on-exit mpv))))))

(defun subtle-mpv-start (media on-message on-exit)
  "Play MEDIA in a new mpv.
ON-MESSAGE gets each message; ON-EXIT is called when mpv goes away."
  (let ((mpv (subtle-mpv--make :socket (subtle-mpv--socket-name)
                                :on-message on-message :on-exit on-exit)))
    (setf (subtle-mpv-process mpv) (subtle-mpv--spawn mpv media))
    (subtle-mpv--connect mpv 0)
    mpv))

(defun subtle-mpv-stop (mpv)
  "Shut MPV down quietly and remove its socket."
  (when-let* ((process (subtle-mpv-process mpv)))
    (set-process-sentinel process #'ignore)
    (delete-process process))
  (when-let* ((conn (subtle-mpv-connection mpv)))
    (delete-process conn))
  (delete-file (subtle-mpv-socket mpv)))

(provide 'subtle-mpv)
;;; subtle-mpv.el ends here
