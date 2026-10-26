;;; subtle-scroll.el --- Pure scroll maths and follow state -*- lexical-binding: t; -*-

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

(defun subtle-scroll-page-needed-p (before-start used body margin)
  "Non-nil if the active cue calls for turning the page.
BEFORE-START: the cue begins above the window.  USED: pixels from the window
top to the cue's end.  BODY: window height.  MARGIN: pixels that must stay free
below the cue."
  (or before-start (> used (- body margin))))

(defun subtle-scroll-far-p (lines threshold)
  "Non-nil if LINES lines is at least THRESHOLD, a number or nil (never)."
  (and threshold lines (>= lines threshold)))

(defun subtle-follow-next (follow event)
  "Return the new autoscroll state after EVENT, given the old state FOLLOW.
EVENT is `user-scroll' or `resume'; anything else keeps FOLLOW."
  (pcase event
    ('user-scroll nil)
    ('resume t)
    (_ follow)))

(provide 'subtle-scroll)
;;; subtle-scroll.el ends here
