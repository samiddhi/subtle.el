;;; subtle-scroll.el --- Pure scroll maths and follow state -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <you@example.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of subtle; see subtle.el for the license notice.

;;; Commentary:

;; No windows, no buffers: just the decisions.

;;; Code:

(defun subtle-scroll-offset (height ratio)
  "Distance from the window top to the active cue, in HEIGHT's unit.
HEIGHT is the window body height (pixels); RATIO the fraction to descend."
  (max 0 (min (1- height) (round (* height ratio)))))

(defun subtle-scroll-move-point-p (policy selected idle)
  "Non-nil if autoscroll should move point, under `subtle-move-point' POLICY.
SELECTED: the window is the selected one.  IDLE: its cursor is not in use."
  (pcase policy
    ('always t)
    ('never nil)
    (_ (or (not selected) idle))))

(defun subtle-scroll-page-needed-p (before-start used body margin
                                                 &optional top top-margin)
  "Non-nil if the active cue needs the page turned.
BEFORE-START: the cue begins above the window.  USED: pixels from the window
top to the cue's end.  BODY: window height.  MARGIN: pixels that must stay free
below the cue.  TOP, when given, is the pixels from the window top to the
cue's start, and TOP-MARGIN the pixels that must stay free above the cue.
This is the only test that lets autoscroll move the window."
  (or before-start
      (> used (- body margin))
      (and top top-margin (> top-margin 0) (< top top-margin))))

(defun subtle-scroll-far-p (lines threshold)
  "Non-nil if LINES lines is at least THRESHOLD, a number or nil (never)."
  (and threshold lines (>= lines threshold)))

(defun subtle-scroll-follow-next (follow event)
  "Return the new autoscroll state after EVENT, given the old state FOLLOW.
EVENT is `user-scroll' or `resume'; anything else keeps FOLLOW."
  (pcase event
    ('user-scroll nil)
    ('resume t)
    (_ follow)))

(define-obsolete-function-alias 'subtle-follow-next
  #'subtle-scroll-follow-next "0.1.0")

(defun subtle-scroll-hold-pause-step (far state paused)
  "Decide what a held position does to playback.
FAR: the held position is far above the spoken line.  STATE is `holding'
\(we paused playback), `overridden' (you resumed it by hand) or nil.
PAUSED: playback is paused.  Return (NEW-STATE . ACTION), ACTION being
`pause', `resume' or nil."
  (cond ((and far (not state) (not paused)) '(holding . pause))
        ((and (not far) state) (cons nil (and (eq state 'holding) 'resume)))
        (t (cons state nil))))

(provide 'subtle-scroll)
;;; subtle-scroll.el ends here
