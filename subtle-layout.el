;;; subtle-layout.el --- How cues are arranged in text -*- lexical-binding: t; -*-

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
  (concat "[.?!…]+" subtle-sentence-closers))

(defun subtle-layout-inline-p (layout)
  "Non-nil if LAYOUT lets several cues share a line."
  (memq layout '(sentence paragraph-flow paragraph-pause)))

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

(defun subtle-layout-text (layout cue)
  "Text to show for CUE under LAYOUT."
  (if (eq layout 'sentence)
      (string-join (subtle-layout-sentence-split (subtle-cue-text cue)) "\n")
    (subtle-cue-text cue)))

(defun subtle-layout-separator (layout source cue next)
  "String that goes between CUE and NEXT under LAYOUT."
  (pcase layout
    ('double-spaced "\n\n")
    ('sentence (if (subtle-layout-sentence-end-p (subtle-cue-text cue)) "\n" " "))
    ((or 'paragraph-flow 'paragraph-pause)
     (if (subtle-layout-break-after-p source cue next) "\n\n" " "))
    (_ "\n")))

(defun subtle-layout-pause-points (source track)
  "Sorted vector of the end times of the paragraphs of TRACK."
  (let* ((cues (subtle-track-cues track)))
    (vconcat
     (seq-sort #'<
      (delq nil
           (seq-map-indexed
            (lambda (cue i)
              (and (subtle-layout-break-after-p
                    source cue (and (< (1+ i) (length cues)) (aref cues (1+ i))))
                   (< (1+ i) (length cues))
                   (subtle-cue-end cue)))
            cues))))))

(defun subtle-layout-pause-crossed (points from to)
  "The element of the sorted vector POINTS in (FROM, TO], or nil.
A step of more than a second is a seek, not playback, and finds nothing."
  (when (and (> to from) (<= (- to from) 1.0))
    (let ((i (subtle-track--before points to)))
      (and (>= i 0) (> (aref points i) from) (aref points i)))))

(provide 'subtle-layout)
;;; subtle-layout.el ends here
