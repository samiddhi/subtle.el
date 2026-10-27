;;; subtle-track.el --- Cues, tracks and time lookup -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <you@example.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of subtle; see subtle.el for the license notice.

;;; Commentary:

;; Pure data and functions.  A track is a vector of cues sorted by start
;; time, plus a parallel vector of start times for binary search.  Tracks
;; are independent: nothing here assumes two tracks line up.

;;; Code:

(require 'cl-lib)

(defconst subtle-max-subtitles 2
  "Most subtitle files subtle shows at once.")

(defconst subtle-track-seek-threshold 1.0
  "Seconds of media a position may advance by and still count as playing.
A bigger step is a seek, not playback.")

(cl-defstruct (subtle-cue (:constructor subtle-cue-create))
  start end text
  ;; Non-nil when the file itself marks a paragraph break after this cue.
  break-after)

(cl-defstruct (subtle-track (:constructor subtle-track-create))
  cues starts)

(defun subtle-track--before (starts time &optional lo hi)
  "Find the last index whose time is <= TIME, or -1.
STARTS is the sorted vector of times.  Optional LO and HI bound the search."
  (let ((lo (or lo 0))
        (hi (or hi (length starts))))
    (if (>= lo hi)
        (1- lo)
      (let ((mid (/ (+ lo hi) 2)))
        (if (<= (aref starts mid) time)
            (subtle-track--before starts time (1+ mid) hi)
          (subtle-track--before starts time lo mid))))))

(defun subtle-track--active-p (track index time)
  "Non-nil if cue INDEX of TRACK has not ended at TIME."
  (< time (subtle-cue-end (aref (subtle-track-cues track) index))))

(defun subtle-track-cue-index (track time &optional hold)
  "Index of the cue of TRACK speaking at TIME, or nil.
With HOLD, the latest started cue counts even after it ended."
  (let ((index (subtle-track--before (subtle-track-starts track) time)))
    (and (>= index 0)
         (or hold (subtle-track--active-p track index time))
         index)))

(defun subtle-track-next-start (track time)
  "Start of the first cue of TRACK beginning after TIME, or nil."
  (let* ((starts (subtle-track-starts track))
         (next (1+ (subtle-track--before starts time))))
    (and (< next (length starts))
         (aref starts next))))

(defun subtle-track-previous-start (track time &optional restart)
  "Start of the cue of TRACK to return to from TIME, or 0.0 when there is none.
A cue that began more than RESTART seconds ago (default 1.0) restarts;
otherwise the one before."
  (let* ((starts (subtle-track-starts track))
         (index (subtle-track--before starts (- time (or restart 1.0)))))
    (if (>= index 0)
        (aref starts index)
      0.0)))

(provide 'subtle-track)
;;; subtle-track.el ends here
