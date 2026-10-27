;;; subtle-doc.el --- Read the subtle manual and tutorial in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <you@example.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of subtle; see subtle.el for the license notice.

;;; Commentary:

;; The documents are Org files that mention keys as \[command]
;; placeholders, with \<subtle-mode-map> at the top.  Showing one runs
;; it through `substitute-command-keys', so you read the keys you
;; really have.  The Makefile renders the manual the same way, with the
;; default keys, before turning it into subtle.info.

;;; Code:

(require 'subtle-keys)

(defconst subtle-doc--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding the Org documents of subtle.")

(defun subtle-doc-render (text)
  "TEXT with its key placeholders replaced by the keys currently bound.
Quotes are left as typed, so code in the text is not curled."
  (let ((text-quoting-style 'grave))
    (substitute-command-keys text)))

(defun subtle-doc--read (name)
  "Contents of the document NAME (\"subtle-manual\" or \"subtle-tutorial\")."
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat name ".org")
                                            subtle-doc--directory))
    (buffer-string)))

(defun subtle-doc-render-file (name output)
  "Write the document NAME, with default keys shown, to the file OUTPUT."
  (with-temp-file output
    (insert (subtle-doc-render (subtle-doc--read name)))))

(defun subtle-doc--show (name title)
  "Show the document NAME in a read-only buffer called TITLE."
  (with-current-buffer (get-buffer-create title)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (subtle-doc-render (subtle-doc--read name)))
      (delay-mode-hooks (org-mode))
      (goto-char (point-min)))
    (view-mode 1)
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun subtle-manual ()
  "Show the manual of subtle, with the keys you have bound."
  (interactive)
  (subtle-doc--show "subtle-manual" "*subtle manual*"))

;;;###autoload
(defun subtle-tutorial ()
  "Show the tutorial of subtle, with the keys you have bound."
  (interactive)
  (subtle-doc--show "subtle-tutorial" "*subtle tutorial*"))

(provide 'subtle-doc)
;;; subtle-doc.el ends here
