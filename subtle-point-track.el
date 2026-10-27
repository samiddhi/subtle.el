;;; subtle-point-track.el --- Keep point on the first unknown word of the spoken cue -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <you@example.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of subtle; see subtle.el for the license notice.

;;; Commentary:

;; Point tracking: point follows the playback.  Whenever a new cue becomes
;; the spoken one, point jumps to the place `subtle-cue-point-functions' pick
;; in it (bltzread registers the first word you do not know yet), or to the
;; start of the cue when none does.
;;
;; Three states per subtitle buffer:
;;   tracking  point jumps to the new cue at every cue change;
;;   released  you moved point off the cue; point is left alone until you put
;;             it back on the current cue, the playback reaches the cue point
;;             is on, or you press `subtle-point-track-toggle';
;;   off       tracking is switched off with `subtle-point-track-toggle' (or
;;             `subtle-point-track-initially' is nil).
;;
;; A cue change that comes less than `subtle-point-track-nav-grace' seconds
;; after you moved point releases tracking too, so it never snatches point
;; from a word you are just reading.  A card open at a change that moves point
;; is dismissed; `subtle-point-track-moved-functions' then lets bltzread open
;; the card of the word point landed on.
;;
;; Deciding is pure (`subtle-point-track-decide',
;; `subtle-point-track-sync-state'), so it can be tested without windows.
;;
;; Point tracking reaches the rest of subtle only through its hooks:
;; `subtle-before-cue-change-functions' and `subtle-cue-change-functions'
;; (a cue changed), `subtle-open-hook' and `subtle-close-hook' (the
;; indicator) and `subtle-mode-hook' (watching point).

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'subtle-custom)
(require 'subtle-session)
(require 'subtle-popup)

(defvar-local subtle-point-track--state nil
  "State of this buffer: `tracking', `released', `off', or nil if unset.")
(defvar-local subtle-point-track--span nil "(BEG . END) of the cue last seen here.")
(defvar-local subtle-point-track--nav-time nil
  "`float-time' when the user last moved point in this buffer, or nil.")
(defvar-local subtle-point-track--last-point nil "Point after the previous command.")
(defvar-local subtle-point-track--plan nil
  "Whether point moves with the cue change, decided ahead of it: `move' or nil.")
(defvar subtle-point-track--frame nil "Child frame of the corner indicator, or nil.")
(defvar subtle-point-track--text nil "Text shown in `subtle-point-track--frame'.")
(defconst subtle-point-track--buffer " *subtle-point-track*")

;;; Pure decisions

(defun subtle-point-track-decide (state in-old-cue recent-nav)
  "Decide what a cue change does to STATE.
IN-OLD-CUE is non-nil if point sits on the cue that just ended, or there is
none yet.  RECENT-NAV is non-nil if the user moved point just before the
change.  Return (NEW-STATE . ACTION), ACTION being `move' or nil."
  (pcase state
    ('tracking (if (and in-old-cue (not recent-nav)) '(tracking . move) '(released)))
    (_ (list state))))

(defun subtle-point-track-sync-state (state on-cue)
  "STATE after point was found ON-CUE (non-nil) or off it."
  (pcase state
    ('tracking (if on-cue state 'released))
    ('released (if on-cue 'tracking 'released))
    (_ state)))

(defun subtle-point-track--on-span-p (pos span)
  "Non-nil if POS lies within SPAN, a (BEG . END) cons."
  (and span (<= (car span) pos) (<= pos (cdr span))))

;;; State

(defun subtle-point-track--rest ()
  "State when point was neither released nor switched off."
  (if subtle-point-track-initially 'tracking 'off))

(defun subtle-point-track--state ()
  "State of the current buffer, set from the options when still unset."
  (or subtle-point-track--state (setq subtle-point-track--state (subtle-point-track--rest))))

(defun subtle-point-track--target (span)
  "Position point takes in SPAN: the registered functions' choice, or its start."
  (or (run-hook-with-args-until-success 'subtle-cue-point-functions
                                        (car span) (cdr span))
      (car span)))

(defun subtle-point-track--recent-nav-p ()
  "Non-nil if the user moved point less than `subtle-point-track-nav-grace' ago."
  (and subtle-point-track--nav-time
       (< (- (float-time) subtle-point-track--nav-time) subtle-point-track-nav-grace)))

(defun subtle-point-track--decide-plan (buffer)
  "Update the state of BUFFER ahead of a cue change; return `move' or nil."
  (with-current-buffer buffer
    (let* ((window (get-buffer-window buffer))
           (pos (if window (window-point window) (point)))
           (in-old-cue (or (null subtle-point-track--span)
                           (subtle-point-track--on-span-p pos subtle-point-track--span)))
           (decision (subtle-point-track-decide (subtle-point-track--state) in-old-cue
                                              (subtle-point-track--recent-nav-p))))
      (setq subtle-point-track--state (car decision))
      (cdr decision))))

(defun subtle-point-track--move (buffer span)
  "Take point in BUFFER to the target in SPAN, in its window too."
  (let ((target (subtle-point-track--target span))
        (window (get-buffer-window buffer)))
    (goto-char target)
    (setq subtle-point-track--last-point target)
    (when window
      (set-window-point window target)
      (set-window-parameter window 'subtle-point target))
    (run-with-idle-timer 0 nil #'subtle-point-track--announce buffer target)))

(defun subtle-point-track--announce (buffer pos)
  "Run `subtle-point-track-moved-functions' for BUFFER, if point is still at POS."
  (when (and (buffer-live-p buffer) (eq (buffer-local-value 'subtle-point-track--state buffer) 'tracking))
    (with-current-buffer buffer
      (when (eql (point) pos)
        (run-hook-with-args 'subtle-point-track-moved-functions buffer pos)))))

(defun subtle-point-track--apply (buffer span plan held)
  "After the cue of BUFFER became SPAN: carry out PLAN, `move' or nil.
While HELD (the minibuffer is in use) point stays, and tracking is released.
Without a move, point that sits on the new cue resumes released tracking:
the playback has reached the line you were reading."
  (with-current-buffer buffer
    (setq subtle-point-track--span span)
    (cond ((null span))
          ((and (eq plan 'move) held)
           (setq subtle-point-track--state 'released))
          ((eq plan 'move) (subtle-point-track--move buffer span))
          ((not held)
           (let ((window (get-buffer-window buffer)))
             (setq subtle-point-track--state
                   (subtle-point-track-sync-state
                    (subtle-point-track--state)
                    (subtle-point-track--on-span-p
                     (if window (window-point window) (point)) span))))))
    (subtle-point-track-sync-indicator)))

(defun subtle-point-track--before-cue-change (buffer _index)
  "Plan the fate of point in BUFFER ahead of a cue change and its scrolling.
When point is going to move, dismiss whatever card is open first, so it does
not hold the scrolling back.
Meant for `subtle-before-cue-change-functions'."
  (with-current-buffer buffer
    (setq subtle-point-track--plan (subtle-point-track--decide-plan buffer))
    (when (eq subtle-point-track--plan 'move)
      (run-hook-with-args-until-success 'subtle-dismiss-functions))))

(defun subtle-point-track--after-cue-change (buffer index)
  "Carry out the plan for BUFFER, whose spoken cue is now INDEX.
Meant for `subtle-cue-change-functions'."
  (subtle-point-track--apply buffer (subtle-cue-span buffer index)
                       (buffer-local-value 'subtle-point-track--plan buffer)
                       (minibuffer-window-active-p (minibuffer-window))))

(defun subtle-point-track--note-nav ()
  "Remember the time when a command moved point."
  (unless (eql (point) subtle-point-track--last-point)
    (setq subtle-point-track--nav-time (float-time)
          subtle-point-track--last-point (point))))

(defun subtle-point-track--post-command ()
  "Buffer-local `post-command-hook': release or resume tracking with point."
  (subtle-point-track--note-nav)
  (when subtle-point-track--span
    (let ((new (subtle-point-track-sync-state
                (subtle-point-track--state)
                (subtle-point-track--on-span-p (point) subtle-point-track--span))))
      (unless (eq new subtle-point-track--state)
        (setq subtle-point-track--state new)
        (subtle-point-track-sync-indicator)))))

;;; Command

(defun subtle-point-track-toggle ()
  "Switch point tracking the playback on or off.
Switching on takes point to the spoken cue at once."
  (interactive)
  (unless subtle-point-track--span (user-error "No cue yet"))
  (let ((tracking (eq (subtle-point-track--state) 'tracking)))
    (setq subtle-point-track--state (if tracking 'off 'tracking))
    (unless tracking
      (subtle-point-track--move (current-buffer) subtle-point-track--span))
    (message "Point tracking: %s" subtle-point-track--state)
    (subtle-point-track-sync-indicator)))

;;; Corner indicator

(defun subtle-point-track--text ()
  "Text for the indicator of the selected window's buffer, or nil for none."
  (and subtle-point-track-indicator (display-graphic-p) subtle-cue-point-functions
       (derived-mode-p 'subtle-mode)
       (and (eq (subtle-point-track--state) 'tracking) "● tracking point")))

(defun subtle-point-track--place (frame window)
  "Put FRAME in the bottom right corner of WINDOW."
  (let ((edges (window-pixel-edges window)))
    (set-frame-position
     frame
     (max 0 (- (nth 2 edges) (frame-pixel-width frame)))
     (max 0 (- (nth 3 edges) (window-mode-line-height window)
               (frame-pixel-height frame))))))

(defun subtle-point-track-hide-indicator ()
  "Remove the corner indicator."
  (when (frame-live-p subtle-point-track--frame)
    (delete-frame subtle-point-track--frame))
  (setq subtle-point-track--frame nil subtle-point-track--text nil)
  (when-let* ((buffer (get-buffer subtle-point-track--buffer)))
    (kill-buffer buffer)))

(defun subtle-point-track--show-indicator (text)
  "Show TEXT in a new indicator frame in the corner of the selected window."
  (let ((frame (subtle-popup-make-frame
                (subtle-popup-box
                 (list (propertize text 'face 'subtle-point-track-indicator)))
                subtle-point-track--buffer)))
    (subtle-point-track--place frame (selected-window))
    (make-frame-visible frame)
    (setq subtle-point-track--frame frame subtle-point-track--text text)))

(defun subtle-point-track-sync-indicator (&rest _)
  "Show, move or remove the corner indicator to match the selected buffer."
  (let ((text (subtle-point-track--text)))
    (cond
     ((null text) (subtle-point-track-hide-indicator))
     ((and (frame-live-p subtle-point-track--frame) (equal text subtle-point-track--text))
      (subtle-point-track--place subtle-point-track--frame (selected-window)))
     (t (subtle-point-track-hide-indicator)
        (subtle-point-track--show-indicator text)))))

;;; Joining a session

(defconst subtle-point-track--window-hooks
  '(window-selection-change-functions window-buffer-change-functions)
  "Global hooks that keep the indicator in step with the selected window.")

(defun subtle-point-track--install ()
  "Watch the windows while a session is open.
Meant for `subtle-open-hook'."
  (dolist (hook subtle-point-track--window-hooks)
    (add-hook hook #'subtle-point-track-sync-indicator)))

(defun subtle-point-track--remove ()
  "Stop watching the windows and remove the indicator.
Meant for `subtle-close-hook'."
  (dolist (hook subtle-point-track--window-hooks)
    (remove-hook hook #'subtle-point-track-sync-indicator))
  (subtle-point-track-hide-indicator))

(defun subtle-point-track--setup-buffer ()
  "Watch point in this subtitle buffer.  Meant for `subtle-mode-hook'."
  (add-hook 'post-command-hook #'subtle-point-track--post-command nil t))

(add-hook 'subtle-mode-hook #'subtle-point-track--setup-buffer)
(add-hook 'subtle-open-hook #'subtle-point-track--install)
(add-hook 'subtle-close-hook #'subtle-point-track--remove)
(add-hook 'subtle-before-cue-change-functions #'subtle-point-track--before-cue-change)
(add-hook 'subtle-cue-change-functions #'subtle-point-track--after-cue-change)

(provide 'subtle-point-track)
;;; subtle-point-track.el ends here
