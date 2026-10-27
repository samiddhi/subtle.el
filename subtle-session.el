;;; subtle-session.el --- The session of subtle and its public queries -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <you@example.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of subtle; see subtle.el for the license notice.

;;; Commentary:

;; The state of the one open session, the state each subtitle buffer
;; keeps, and the small questions other files and other packages ask
;; about them.  Public API: `subtle-current-session', `subtle-active-p',
;; `subtle-session-buffers', `subtle-cue-span', `subtle-held-p',
;; `subtle-played-start-position' and `subtle-played-through-position'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subtle-custom)
(require 'subtle-track)

(cl-defstruct subtle-session
  "State of an open session.
`buffers' are the subtitle buffers, `tracks' their parsed tracks and
`indices' the spoken cue of each (nil in a gap).  `follow' says whether
the text follows the audio; `position' and `paused' mirror mpv.
`hold-pause' is nil, `holding' or `overridden' (see
`subtle-scroll-hold-pause-step')."
  mpv tracks buffers config timer shown-second
  (indices nil) (position 0.0) (paused nil) (speed 1.0) (follow t)
  (auto-resume subtle-auto-resume)
  pause-points pause-timer pausing send hold-timer hold-pause media subs)

(defvar subtle--session nil
  "The active `subtle-session', or nil.
Read it through `subtle-current-session'.")

(defvar-local subtle--track nil "Track shown by this buffer.")
(defvar-local subtle--spans nil "Vector of (BEG . END) per cue of this buffer.")
(defvar-local subtle--label "" "Name shown in the header line of this buffer.")
(defvar-local subtle--run nil
  "(START . END) buffer positions of the latest run of continuous playback.")

(defvar-local subtle-track-index nil
  "Position of the buffer's file among the session's files (0 for the first).")

(defvar-local subtle-language nil
  "Language of the buffer's subtitle file, from its name.
See `subtle-language-codes'.")

;;; The session

(defun subtle-current-session ()
  "The open `subtle-session', or nil."
  subtle--session)

(defun subtle-active-p ()
  "Non-nil while a subtle session is open."
  (and subtle--session t))

(defun subtle-require-session ()
  "The open session, or a `user-error'."
  (or subtle--session (user-error "No subtle session")))

(define-obsolete-function-alias 'subtle-ui-session
  #'subtle-require-session "0.1.0")

(defun subtle-held-p ()
  "Non-nil while something needs the text to stay where it is.
That is the minibuffer, or any of `subtle-hold-functions'."
  (or (minibuffer-window-active-p (minibuffer-window))
      (and (run-hook-with-args-until-success 'subtle-hold-functions) t)))

;;; Buffers and cues of a session

(defun subtle-session-pairs (session)
  "The (BUFFER . INDEX) of every live buffer of SESSION.
INDEX is the spoken cue of the buffer, nil in a gap."
  (seq-filter (lambda (pair) (buffer-live-p (car pair)))
              (seq-mapn #'cons (subtle-session-buffers session)
                        (subtle-session-indices session))))

(defun subtle-session-index-of (session buffer)
  "Spoken cue index of BUFFER in SESSION, or nil."
  (when-let* ((position (seq-position (subtle-session-buffers session) buffer)))
    (nth position (subtle-session-indices session))))

(defun subtle-cue-span (buffer index)
  "The (BEG . END) of cue INDEX in BUFFER, or nil when INDEX is not a cue."
  (and (integerp index)
       (aref (buffer-local-value 'subtle--spans buffer) index)))

(defun subtle-current-track (session)
  "Track of the current buffer, falling back to the first of SESSION."
  (or subtle--track (car (subtle-session-tracks session))))

(define-obsolete-function-alias 'subtle-ui-current-track
  #'subtle-current-track "0.1.0")

(defun subtle-cue-at-point ()
  "Cue under point in a subtle buffer, or a `user-error'."
  (let ((index (get-text-property (point) 'subtle-index)))
    (if index
        (aref (subtle-track-cues subtle--track) index)
      (user-error "No subtitle at point"))))

(define-obsolete-function-alias 'subtle-ui-cue-at-point
  #'subtle-cue-at-point "0.1.0")

;;; Reading credit

(defun subtle-played-through-position ()
  "End of the text of this buffer that the latest continuous playback reached.
Seeking, or pausing, does not move it; see `subtle-played-start-position'."
  (cdr subtle--run))

(defun subtle-played-start-position ()
  "Where the latest continuous playback run of this buffer began, or nil."
  (car subtle--run))

(provide 'subtle-session)
;;; subtle-session.el ends here
