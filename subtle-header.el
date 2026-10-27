;;; subtle-header.el --- Header line of subtle buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <you@example.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of subtle; see subtle.el for the license notice.

;;; Commentary:

;; The header line shows the playback state, the file name and the
;; control strip.  With two files the first buffer shows state and name
;; and the second the control strip.  The pieces are pure functions.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'subtle-custom)
(require 'subtle-keys)
(require 'subtle-session)

(defconst subtle-header-format '(:eval (subtle-header-string))
  "Value of `header-line-format' in subtle buffers.")

(defun subtle-header--format-time (seconds)
  "SECONDS as MM:SS, or H:MM:SS past an hour."
  (let* ((s (floor seconds))
         (h (/ s 3600)))
    (if (> h 0)
        (format "%d:%02d:%02d" h (/ (% s 3600) 60) (% s 60))
      (format "%02d:%02d" (/ s 60) (% s 60)))))

(defun subtle-header--state (paused speed position)
  "Playback state text for the header line.
PAUSED, SPEED and POSITION describe the playback."
  (propertize (format "%s %gx %s" (if paused "[paused]" "[playing]") speed
                      (subtle-header--format-time position))
              'face 'subtle-header-state))

(defun subtle-header--parts (state label hints count index)
  "Header segments for buffer INDEX out of COUNT buffers.
STATE, LABEL and HINTS are the texts to arrange."
  (cond ((= count 1) (list state hints))
        ((= index 0) (list state label))
        (t (list hints))))

(defun subtle-header-string ()
  "Header line of the current subtle buffer."
  (if-let* ((session (subtle-current-session)))
      (let ((buffers (subtle-session-buffers session)))
        (string-join
         (subtle-header--parts
          (subtle-header--state (subtle-session-paused session)
                                (subtle-session-speed session)
                                (subtle-session-position session))
          (propertize subtle--label 'face 'subtle-header-label)
          (subtle-keys-hints)
          (length buffers)
          (or (seq-position buffers (current-buffer)) 0))
         "  "))
    ""))

(provide 'subtle-header)
;;; subtle-header.el ends here
