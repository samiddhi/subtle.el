;;; subtle-layout.el --- How cues are arranged in text -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <you@example.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of subtle; see subtle.el for the license notice.

;;; Commentary:

;; Pure functions: where sentences end, where paragraphs break, and what
;; text goes between two cues.  No buffers, no windows.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'subtle-custom)
(require 'subtle-track)

(defun subtle-layout--terminator-re ()
  "Regexp for a sentence terminator with its trailing closers."
  (concat (regexp-opt-charset (string-to-list subtle-sentence-terminators))
          "+" subtle-sentence-closers))

(defun subtle-layout--property (layout property)
  "PROPERTY of LAYOUT in the table `subtle-layouts'."
  (plist-get (cdr (assq layout subtle-layouts)) property))

(defun subtle-layout-inline-p (layout)
  "Non-nil if LAYOUT permits sharing a line between cues."
  (subtle-layout--property layout :inline))

(defun subtle-layout-paragraphs-p (layout)
  "Non-nil if paragraph breaks matter in LAYOUT."
  (subtle-layout--property layout :paragraphs))

(defun subtle-layout-pause-p (layout)
  "Non-nil if playback pauses at the paragraph ends of LAYOUT."
  (subtle-layout--property layout :pause))

(defun subtle-layout-sentence-end-p (text)
  "Non-nil if TEXT ends a sentence."
  (string-match-p (concat (subtle-layout--terminator-re) "[ \t]*\\'") text))

(defun subtle-layout-sentence-split (text)
  "TEXT cut into sentences; a break follows a terminator, its closers, and spaces."
  (let ((re (concat "\\(" (subtle-layout--terminator-re) "\\)[ \t]+"))
        (start 0) (sentences nil))
    (while (string-match re text start)
      (push (substring text start (match-end 1)) sentences)
      (setq start (match-end 0)))
    (nreverse (if (< start (length text))
                  (cons (substring text start) sentences)
                sentences))))

(defun subtle-layout-break-after-p (source cue next)
  "Non-nil if a paragraph ends after CUE, given the following cue NEXT.
SOURCE is `subtle-paragraph-source'."
  (or (subtle-cue-break-after cue)
      (and (eq source 'explicit-and-gap) next
           (subtle-layout-sentence-end-p (subtle-cue-text cue))
           (>= (- (subtle-cue-start next) (subtle-cue-end cue))
               subtle-paragraph-gap))))

(defun subtle-layout-fill (string width)
  "Return STRING with some spaces turned into newlines to fit lines in WIDTH.
The result has the length of STRING, so positions in it stay valid.  Existing
newlines start a new line; a word longer than WIDTH is left whole.  A WIDTH
that is not a positive integer (like -1) changes nothing."
  (if (not (and (integerp width) (> width 0)))
      string
    (let ((s (copy-sequence string)) (col 0) (last-space nil) (i 0))
      (while (< i (length s))
        (let ((c (aref s i)))
          (cond ((eq c ?\n) (setq col 0 last-space nil))
                ((memq c '(?\s ?\t)) (setq col (1+ col) last-space i))
                (t (setq col (1+ col))
                   (when (and (> col width) last-space)
                     (aset s last-space ?\n)
                     (setq col (- i last-space) last-space nil)))))
        (setq i (1+ i)))
      s)))

(defun subtle-layout-text (layout cue)
  "Text to show for CUE under LAYOUT, wrapped per `subtle-cue-fill-column'.
The paragraph layouts are filled as a whole instead; see
`subtle-paragraph-fill-column'."
  (let ((text (if (eq layout 'sentence)
                  (string-join (subtle-layout-sentence-split (subtle-cue-text cue)) "\n")
                (subtle-cue-text cue))))
    (if (subtle-layout-paragraphs-p layout)
        text
      (subtle-layout-fill text subtle-cue-fill-column))))

(defun subtle-layout-separator (layout source cue next)
  "Return the string between CUE and NEXT under LAYOUT.
SOURCE is `subtle-paragraph-source'."
  (cond ((subtle-layout-paragraphs-p layout)
         (if (subtle-layout-break-after-p source cue next) "\n\n" " "))
        ((eq layout 'sentence)
         (if (subtle-layout-sentence-end-p (subtle-cue-text cue)) "\n" " "))
        (t (or (subtle-layout--property layout :separator) "\n"))))

(defun subtle-layout-pause-points (source track)
  "Return the sorted vector of paragraph end times of TRACK.
SOURCE is `subtle-paragraph-source'."
  (let ((cues (subtle-track-cues track)))
    (vconcat
     (seq-sort #'<
               (delq nil
                     (seq-map-indexed
                      (lambda (cue i)
                        (let ((next (and (< (1+ i) (length cues)) (aref cues (1+ i)))))
                          (and next
                               (subtle-layout-break-after-p source cue next)
                               (subtle-cue-end cue))))
                      cues))))))

(defun subtle-layout-pause-crossed (points from to)
  "The element of the sorted vector POINTS in (FROM, TO], or nil.
A step beyond `subtle-track-seek-threshold' is a seek, not playback,
and finds nothing."
  (when (and (> to from) (<= (- to from) subtle-track-seek-threshold))
    (let ((i (subtle-track--before points to)))
      (and (>= i 0) (> (aref points i) from) (aref points i)))))

(provide 'subtle-layout)
;;; subtle-layout.el ends here
