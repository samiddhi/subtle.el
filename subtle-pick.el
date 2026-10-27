;;; subtle-pick.el --- Choosing media and subtitle files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <you@example.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of subtle; see subtle.el for the license notice.

;;; Commentary:

;; Interactive selection.  Ranking of candidates is a pure function of
;; file names; only the directory listing touches the disk.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'subtle-custom)
(require 'subtle-track)

(defun subtle-pick--extension-p (file extensions)
  "Non-nil if FILE has one of EXTENSIONS, ignoring case."
  (member (downcase (or (file-name-extension file) "")) extensions))

(defun subtle-pick--base-p (media-name name)
  "Non-nil if NAME begins with the base name of MEDIA-NAME."
  (string-prefix-p (file-name-sans-extension media-name) name))

(defun subtle-pick--subtitle-p (name)
  "Non-nil if NAME is a subtitle file name."
  (subtle-pick--extension-p name subtle-subtitle-extensions))

(defun subtle-pick--media-p (name)
  "Non-nil if NAME is a media file name."
  (subtle-pick--extension-p name subtle-media-extensions))

(defun subtle-pick--siblings (media-name names)
  "Subtitles among NAMES that belong to MEDIA-NAME, sorted."
  (sort (seq-filter (lambda (n) (and (subtle-pick--subtitle-p n)
                                     (subtle-pick--base-p media-name n)))
                    names)
        #'string<))

(defun subtle-pick--rank (media-name names)
  "NAMES with those sharing the base of MEDIA-NAME first."
  (let ((matching (seq-filter (lambda (n) (subtle-pick--base-p media-name n)) names)))
    (append matching (seq-difference names matching))))

(defun subtle-pick--candidates (media)
  "Subtitle file names beside MEDIA, best matches first."
  (subtle-pick--rank
   (file-name-nondirectory media)
   (sort (seq-filter #'subtle-pick--subtitle-p
                     (directory-files (file-name-directory media) nil nil t))
         #'string<)))

(defun subtle-pick--find-media (selected dir-names)
  "Media name among SELECTED, else one in DIR-NAMES matching a subtitle."
  (or (seq-find #'subtle-pick--media-p selected)
      (seq-find (lambda (n) (and (subtle-pick--media-p n)
                                 (subtle-pick--base-p n (car selected))))
                dir-names)))

(defun subtle-pick--resolve (selected dir-names)
  "Cons of (MEDIA . SUBTITLES) names deduced from SELECTED names.
DIR-NAMES are the other files in the same directory."
  (let ((media (subtle-pick--find-media selected dir-names)))
    (cons media
          (and media
               (or (seq-filter #'subtle-pick--subtitle-p selected)
                   (seq-take (subtle-pick--siblings media dir-names) subtle-max-subtitles))))))

(defun subtle-pick--media ()
  "Ask for a media file."
  (read-file-name "Media file: " nil nil t nil
                  (lambda (file)
                    (or (file-directory-p file)
                        (subtle-pick--media-p file)))))

(defun subtle-pick--read-subs (candidates &optional chosen)
  "Ask for subtitle names from CANDIDATES one by one, after CHOSEN."
  (let ((name (and (< (length chosen) subtle-max-subtitles)
                   (completing-read
                    (format "Subtitle %d (RET to finish): " (1+ (length chosen)))
                    (seq-difference candidates chosen)))))
    (if (member name '(nil ""))
        chosen
      (subtle-pick--read-subs candidates (append chosen (list name))))))

(defun subtle-pick--subs (media)
  "Ask for zero to two subtitle files beside MEDIA."
  (let ((dir (file-name-directory media)))
    (mapcar (lambda (name) (expand-file-name name dir))
            (subtle-pick--read-subs (subtle-pick--candidates media)))))

(defun subtle-pick-args ()
  "Interactive arguments of `subtle-open': media then subtitles."
  (let ((media (expand-file-name (subtle-pick--media))))
    (cons media (subtle-pick--subs media))))

(provide 'subtle-pick)
;;; subtle-pick.el ends here
