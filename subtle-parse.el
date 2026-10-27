;;; subtle-parse.el --- SRT and VTT parsing -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <you@example.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of subtle; see subtle.el for the license notice.

;;; Commentary:

;; text -> blocks -> cues -> track.  SRT and VTT share one pipeline: a
;; block is any chunk with a "-->" line; everything before it (indexes,
;; cue ids) and blocks without one (WEBVTT header, NOTE) are ignored.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'subtle-track)

(defconst subtle-parse--timestamp-re
  "\\(?:\\([0-9]+\\):\\)?\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)[.,]\\([0-9]+\\)"
  "Matches HH:MM:SS,mmm, HH:MM:SS.mmm and MM:SS.mmm.")

(defconst subtle-parse--markup-regexps '("<[^>]*>" "{\\\\[^}]*}")
  "Inline markup removed from cue text: HTML-ish tags and ASS overrides.")

(defconst subtle-parse--entities
  '(("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"") ("&apos;" . "'")
    ("&nbsp;" . " ") ("&amp;" . "&"))
  "Named HTML entities decoded in cue text; `&amp;' goes last.")

(defconst subtle-parse--numeric-entity-re "&#\\(x\\)?\\([0-9a-fA-F]+\\);"
  "Matches numeric entities such as &#39; and &#x27;.")

(defun subtle-parse--parse-timestamp (string)
  "Seconds denoted by the first timestamp in STRING, or nil."
  (when (string-match subtle-parse--timestamp-re string)
    (let ((hours (string-to-number (or (match-string 1 string) "0")))
          (minutes (string-to-number (match-string 2 string)))
          (seconds (string-to-number (match-string 3 string)))
          (fraction (match-string 4 string)))
      (+ (* 3600 hours) (* 60 minutes) seconds
         (/ (string-to-number fraction) (expt 10.0 (length fraction)))))))

(defun subtle-parse--time-line (line)
  "Cons of (START . END) seconds for a \"a --> b\" LINE, or nil."
  (let ((parts (split-string line "-->")))
    (when-let* ((start (subtle-parse--parse-timestamp (car parts)))
                (end (subtle-parse--parse-timestamp (or (cadr parts) ""))))
      (cons start end))))

(defun subtle-parse--replace-all (alist string)
  "Apply every (REGEXP . REPLACEMENT) of ALIST to STRING."
  (seq-reduce (lambda (text pair)
                (replace-regexp-in-string (car pair) (cdr pair) text t t))
              alist string))

(defun subtle-parse--numeric-entity (entity)
  "Character denoted by the numeric ENTITY, as a string."
  (string-match subtle-parse--numeric-entity-re entity)
  (string (string-to-number (match-string 2 entity)
                            (if (match-beginning 1) 16 10))))

(defun subtle-parse--decode-numeric (text)
  "TEXT with numeric entities replaced by their characters."
  (replace-regexp-in-string subtle-parse--numeric-entity-re
                            #'subtle-parse--numeric-entity text t t))

(defun subtle-parse--clean-text (lines)
  "Join LINES into one tag-free, entity-free, trimmed string."
  (thread-last (string-join lines " ")
               (subtle-parse--replace-all
                (mapcar (lambda (re) (cons re "")) subtle-parse--markup-regexps))
               subtle-parse--decode-numeric
               (subtle-parse--replace-all subtle-parse--entities)
               string-trim))

(defconst subtle-parse--non-text-re
  "\\`[ \t]*\\(?:WEBVTT\\|NOTE\\|STYLE\\|REGION\\|[0-9]+[ \t]*\\'\\)"
  "Blocks without a time line that are not cue text.")

(defun subtle-parse--block->cue (block)
  "Cue described by BLOCK, or nil when BLOCK is empty.
A cue with no text is returned with empty text; the caller treats it as a break."
  (let* ((lines (split-string block "\n" t))
         (rest (seq-drop-while (lambda (l) (not (string-match-p "-->" l))) lines))
         (times (and rest (subtle-parse--time-line (car rest)))))
    (when times
      (subtle-cue-create :start (car times) :end (cdr times)
                          :text (subtle-parse--clean-text (cdr rest))))))

(defun subtle-parse--continuation (block)
  "Cleaned text of BLOCK if it continues the previous cue, else nil."
  (unless (or (string-match-p "-->" block)
              (string-match-p subtle-parse--non-text-re block))
    (let ((text (subtle-parse--clean-text (split-string block "\n" t))))
      (and (not (string-empty-p text)) text))))

(defun subtle-parse--blocks->cues (blocks)
  "Cues of BLOCKS in file order, with explicit paragraph breaks recorded.
A blank line inside a cue (its remaining text forms its own block) or an
empty cue sets `break-after' on the cue before it."
  (let (cues)
    (dolist (block blocks)
      (let ((cue (subtle-parse--block->cue block))
            (more (and (not (string-match-p "-->" block))
                       (subtle-parse--continuation block))))
        (cond ((and cue (string-empty-p (subtle-cue-text cue)))
               (when cues (setf (subtle-cue-break-after (car cues)) t)))
              (cue (push cue cues))
              ((and more cues)
               (let ((previous (car cues)))
                 (setf (subtle-cue-text previous)
                       (concat (subtle-cue-text previous) " " more)
                       (subtle-cue-break-after previous) t))))))
    (nreverse cues)))

(defun subtle-parse--normalize (text)
  "TEXT with a BOM removed and every newline style turned into \\n."
  (replace-regexp-in-string "\r\n?\\|﻿" (lambda (m) (if (equal m "﻿") "" "\n")) text))

(defun subtle-parse--split-blocks (text)
  "Blank-line separated blocks of TEXT."
  (split-string text "\n\\(?:[ \t]*\n\\)+" t))

(defun subtle-parse-from-cues (cues)
  "Track holding CUES sorted by start."
  (let ((sorted (vconcat (seq-sort-by #'subtle-cue-start #'< cues))))
    (subtle-track-create :cues sorted
                          :starts (vconcat (mapcar #'subtle-cue-start sorted)))))

(defun subtle-parse-string (text)
  "Track parsed from subtitle TEXT."
  (thread-last (subtle-parse--normalize text)
               subtle-parse--split-blocks
               subtle-parse--blocks->cues
               subtle-parse-from-cues))

(defun subtle-parse-file (path)
  "Track parsed from the subtitle file PATH."
  (subtle-parse-string
   (with-temp-buffer
     (let ((coding-system-for-read 'undecided))
       (insert-file-contents path))
     (buffer-string))))

(provide 'subtle-parse)
;;; subtle-parse.el ends here
