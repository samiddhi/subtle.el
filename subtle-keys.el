;;; subtle-keys.el --- Keymaps of subtle and the hints derived from them -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <you@example.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of subtle; see subtle.el for the license notice.

;;; Commentary:

;; The keymaps are the single source of truth.  The control strip and
;; the help box are read from them with `where-is-internal', so a key
;; you bind yourself (`keymap-set' on `subtle-mode-map') shows up in
;; both with no further ado.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'subtle-custom)
(require 'subtle-popup)

;; Commands are quoted, not sharp-quoted: they live in files loaded later.
(defvar-keymap subtle-prefix-map
  :doc "Keys that follow the `s' prefix of `subtle-mode-map'."
  "t" 'subtle-toggle-auto-resume
  "s" 'subtle-swap-sides
  "v" 'subtle-set-layout
  "o" 'subtle-set-option)

(defvar-keymap subtle-mode-map
  :doc "Keymap of `subtle-mode'.
Single letters, so they work in a modal editor's motion state."
  "p" 'subtle-toggle-pause
  "f" 'subtle-seek-forward
  "b" 'subtle-seek-backward
  "F" 'subtle-seek-forward-big
  "B" 'subtle-seek-backward-big
  "}" 'subtle-faster
  "{" 'subtle-slower
  "=" 'subtle-reset-speed
  ")" 'subtle-next-cue
  "(" 'subtle-previous-cue
  "RET" 'subtle-seek-to-cue-at-point
  "r" 'subtle-resume-follow
  "m" 'subtle-point-track-toggle
  "s" subtle-prefix-map
  "h" 'subtle-help
  "?" 'subtle-help
  "q" 'subtle-quit)

;;; Reading the keymaps

(defun subtle-keys--key (command)
  "The key bound to COMMAND in `subtle-mode-map', as text, or nil."
  (when-let* ((key (where-is-internal command (list subtle-mode-map) t)))
    (key-description key)))

(defun subtle-keys--all (command)
  "Every key bound to COMMAND in `subtle-mode-map', as a list of texts."
  (mapcar #'key-description
          (where-is-internal command (list subtle-mode-map))))

(defun subtle-keys--commands (keymap)
  "Commands bound in KEYMAP and its prefix maps, in definition order.
Parent keymaps are left out: only the keys subtle itself offers."
  ;; The keymap is walked newest first and consed on, so the list ends up in
  ;; definition order.
  (let (commands)
    (map-keymap-internal
     (lambda (_event binding)
       (cond ((keymapp binding)
              (setq commands (append (subtle-keys--commands binding)
                                     commands)))
             ((commandp binding) (push binding commands))))
     keymap)
    (seq-uniq commands)))

(defun subtle-keys-help-commands ()
  "Commands listed by the help box, in definition order."
  (subtle-keys--commands subtle-mode-map))

;;; Control strip and notice

(defun subtle-keys-hints ()
  "Control strip text for the commands of `subtle-control-strip'."
  (mapconcat
   (lambda (entry)
     (format "%s %s"
             (propertize (or (subtle-keys--key (car entry))
                             (format "M-x %s" (car entry)))
                         'face 'subtle-key)
             (cdr entry)))
   subtle-control-strip "  "))

(defun subtle-keys--cap (command)
  "Key cap of COMMAND for a box, or a hint to use \\[execute-extended-command]."
  (subtle-popup-key (or (subtle-keys--key command) "M-x")))

(defun subtle-keys-hint-rows (auto-resume delay)
  "Rows of the \"autoscroll paused\" box.
AUTO-RESUME says whether it resumes by itself, after DELAY seconds."
  (list (subtle-popup-title "AUTOSCROLL PAUSED")
        (concat (subtle-keys--cap 'subtle-resume-follow) "  resume now")
        (concat (subtle-keys--cap 'subtle-toggle-auto-resume)
                "  auto-resume "
                (if auto-resume (format "on (%gs)" delay) "off"))
        (concat (subtle-keys--cap 'subtle-quit) "  hide this box")))

;;; Help box

(defconst subtle-keys--doc-width 44 "Longest description shown in the help box.")

(defun subtle-keys--first-line (command)
  "First line of the docstring of COMMAND, shortened to fit the help box."
  (truncate-string-to-width
   (car (split-string (or (documentation command) "") "\n"))
   subtle-keys--doc-width nil nil "…"))

(defun subtle-keys-help-rows ()
  "Rows of the help box describing every key of `subtle-mode-map'."
  (let* ((commands (subtle-keys-help-commands))
         (keys (mapcar (lambda (command)
                         (string-join (subtle-keys--all command) " / "))
                       commands))
         (width (apply #'max (mapcar #'string-width keys))))
    (cons (subtle-popup-title "SUBTLE KEYS")
          (seq-mapn (lambda (command key)
                      (concat (propertize (subtle-popup--pad key width)
                                          'face 'subtle-popup-key)
                              "  " (subtle-keys--first-line command)))
                    commands keys))))

(provide 'subtle-keys)
;;; subtle-keys.el ends here
