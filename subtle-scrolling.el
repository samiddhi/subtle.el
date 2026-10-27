;;; subtle-scrolling.el --- Make the text follow the audio -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <you@example.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of subtle; see subtle.el for the license notice.

;;; Commentary:

;; Autoscroll: keeping the window at the spoken cue, turning the page
;; when the cue nears the bottom, holding still while something (a word
;; card, the minibuffer) asks for it, and stepping aside when the user
;; scrolls.  The decisions themselves are pure, in subtle-scroll.el.
;; Changes of the follow state are announced on
;; `subtle-follow-change-functions'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'subtle-custom)
(require 'subtle-session)
(require 'subtle-scroll)

;;; Where the window is

(defun subtle-scrolling--pos-above (window pos pixels)
  "Buffer position about PIXELS of text height above POS in WINDOW.
Pixels, not lines, so enlarged or proportional fonts land correctly."
  (with-selected-window window
    (save-excursion
      (goto-char pos)
      (while (and (> pixels (/ (line-pixel-height) 2))
                  (= -1 (vertical-motion -1)))
        (setq pixels (- pixels (line-pixel-height))))
      (point))))

(defun subtle-scrolling--record-window (window)
  "Remember the start and height of WINDOW, as subtle's own doing."
  (set-window-parameter window 'subtle-start (window-start window))
  (set-window-parameter window 'subtle-height (window-body-height window t)))

(defun subtle-scrolling--record-all (session)
  "Record every window showing a buffer of SESSION, see `--record-window'."
  (dolist (buffer (subtle-session-buffers session))
    (when-let* ((window (get-buffer-window buffer)))
      (subtle-scrolling--record-window window))))

(defun subtle-scrolling--forget-expected-starts (session)
  "Clear where subtle last scrolled each window of SESSION."
  (dolist (buffer (seq-filter #'buffer-live-p (subtle-session-buffers session)))
    (when-let* ((window (get-buffer-window buffer)))
      (set-window-parameter window 'subtle-start nil))))

(defun subtle-scrolling--resized-p (window)
  "Non-nil if WINDOW changed height since subtle last recorded it."
  (let ((height (window-parameter window 'subtle-height)))
    (and height (/= height (window-body-height window t)))))

(defun subtle-scrolling--drifted-p (window start)
  "Non-nil if START is not where subtle last put WINDOW: the user scrolled.
A resize moves the start by itself and does not count."
  (let ((expected (window-parameter window 'subtle-start)))
    (and expected (/= expected start)
         (not (subtle-scrolling--resized-p window)))))

(defun subtle-scrolling--user-command-p ()
  "Non-nil while a command is running, as opposed to a timer or filter."
  (null (current-idle-time)))

(defun subtle-ui-note-own-scroll ()
  "Record the selected window as placed, so a scroll made for it is not the user's.
Add this to the scroll hook of anything else that scrolls a subtle window.
The name is part of the public API."
  (when (derived-mode-p 'subtle-mode)
    (subtle-scrolling--record-window (selected-window))))

;;; Placing the window

(defun subtle-scrolling--cursor-idle-p (window)
  "Non-nil if the cursor of WINDOW seems unused.
That is: where subtle left it, on the first line, or out of sight."
  (and subtle-move-idle-cursor
       (let ((point (window-point window)))
         (or (eql point (window-parameter window 'subtle-point))
             (not (pos-visible-in-window-p point window))
             (with-current-buffer (window-buffer window)
               (save-excursion
                 (goto-char (window-start window))
                 (<= point (line-end-position))))))))

(defun subtle-scrolling--place (window pos &optional start keep-point)
  "Scroll WINDOW so POS sits where `subtle-scroll-style' wants it.
START, a position, is used as the window start instead.  KEEP-POINT leaves
the cursor alone whatever `subtle-move-point' says."
  (let ((pixels (if (eq subtle-scroll-style 'paged)
                    (* (max 0 subtle-page-lead) (default-line-height))
                  (subtle-scroll-offset (window-body-height window t)
                                        subtle-focus-ratio)))
        (move (and (not keep-point)
                   (subtle-scroll-move-point-p
                    subtle-move-point (eq window (selected-window))
                    (subtle-scrolling--cursor-idle-p window)))))
    (set-window-start window
                      (or start (subtle-scrolling--pos-above window pos pixels)))
    (subtle-scrolling--record-window window)
    (when move
      (set-window-point window pos)
      (set-window-parameter window 'subtle-point pos))))

(defun subtle-scrolling--scroll-to-index (buffer index)
  "Scroll the window of BUFFER to cue INDEX, if shown and valid."
  (when-let* ((window (get-buffer-window buffer))
              (span (subtle-cue-span buffer index)))
    (subtle-scrolling--place window (car span))))

(defun subtle-scrolling--reveal-all (session)
  "Scroll every buffer of SESSION to its current cue."
  (pcase-dolist (`(,buffer . ,index) (subtle-session-pairs session))
    (subtle-scrolling--scroll-to-index buffer index)))

;;; Does the page need to turn?

(defun subtle-scrolling--used-pixels (window start end)
  "Height of the text of WINDOW's buffer from START to END, as far as it matters."
  (cdr (window-text-pixel-size window start end nil
                               (1+ (window-body-height window t)))))

(defun subtle-scrolling--margin-pixels ()
  "Pixels that must stay free below the spoken line (`subtle-page-margin')."
  (* (max 0 subtle-page-margin) (default-line-height)))

(defun subtle-scrolling--page-needed-p (window index)
  "Non-nil if cue INDEX is too near the bottom of WINDOW, or not in it."
  (let* ((span (subtle-cue-span (window-buffer window) index))
         (start (window-start window))
         (before (< (car span) start))
         (top-lines (min (max 0 subtle-page-top-margin)
                         (max 0 (1- subtle-page-lead))))
         (top (and (not before) (> top-lines 0)
                   (cdr (posn-x-y (posn-at-point (car span) window))))))
    (subtle-scroll-page-needed-p
     before
     (if before 0 (subtle-scrolling--used-pixels window start (cdr span)))
     (window-body-height window t) (subtle-scrolling--margin-pixels)
     top (* top-lines (default-line-height)))))

(defun subtle-scrolling--line-start (window pos)
  "Start of the screen line of WINDOW holding POS."
  (with-selected-window window
    (save-excursion (goto-char pos) (vertical-motion 0) (point))))

(defun subtle-scrolling--fits-from-p (window start span)
  "Non-nil if SPAN would clear the page margin with START as the window top.
WINDOW is the window concerned."
  (not (subtle-scroll-page-needed-p
        nil (subtle-scrolling--used-pixels window start (cdr span))
        (window-body-height window t) (subtle-scrolling--margin-pixels))))

;;; Holding still

(defconst subtle-scrolling--hold-poll-interval 0.5
  "Seconds between looks at whether a hold on the text is over.")

(defun subtle-scrolling--stop-poll (session)
  "Stop waiting for the hold on SESSION to end."
  (when (timerp (subtle-session-hold-timer session))
    (cancel-timer (subtle-session-hold-timer session)))
  (setf (subtle-session-hold-timer session) nil))

(defun subtle-scrolling--defer (session)
  "Check regularly whether the hold on SESSION is over."
  (unless (timerp (subtle-session-hold-timer session))
    (setf (subtle-session-hold-timer session)
          (run-at-time subtle-scrolling--hold-poll-interval
                       subtle-scrolling--hold-poll-interval
                       #'subtle-scrolling--poll-hold session))))

(defun subtle-scrolling--poll-hold (session)
  "Catch up once SESSION's text is free to move."
  (cond ((not (eq session (subtle-current-session)))
         (subtle-scrolling--stop-poll session))
        ((not (subtle-held-p))
         (subtle-scrolling--stop-poll session)
         (when (subtle-session-follow session)
           (subtle-scrolling-follow-all session)))))

(defun subtle-scrolling--anchor-start (window span)
  "Window start that retains the held position, or nil if SPAN would not fit.
WINDOW shows the buffer; SPAN is the (BEG . END) of the cue to reach."
  (when-let* ((anchor (run-hook-with-args-until-success
                       'subtle-anchor-functions))
              ((< anchor (car span)))
              (start (subtle-scrolling--line-start window anchor))
              ((subtle-scrolling--fits-from-p window start span)))
    start))

(defun subtle-scrolling--turn-page (session window span)
  "Show the cue at SPAN on a new page in WINDOW of SESSION.
While something is held open, the page starts at the held position if the
cue still fits below it; if not, that thing is dismissed and the page turns
anyway."
  (let* ((held (subtle-held-p))
         (start (and held (subtle-scrolling--anchor-start window span))))
    (when (and held (not start))
      (run-hook-with-args-until-success 'subtle-dismiss-functions))
    (cond (start (subtle-scrolling--place window (car span) start t))
          ((subtle-held-p) (subtle-scrolling--defer session))
          (t (subtle-scrolling--place window (car span))))))

(defun subtle-scrolling--scroll-or-defer (session buffer index)
  "Scroll BUFFER to cue INDEX, or wait for the hold on SESSION to end."
  (if (subtle-held-p)
      (subtle-scrolling--defer session)
    (subtle-scrolling--scroll-to-index buffer index)))

(defun subtle-scrolling--catch-up (session buffer index)
  "Bring the window of BUFFER to cue INDEX of SESSION, but only if it must move.
The window moves only when the cue is off screen or inside a page margin
\(`subtle-scrolling--page-needed-p'), never merely because time passes,
playback is paused, or autoscroll resumes.  `continuous' playing keeps the
cue at its fixed height at every cue."
  (when-let* ((window (get-buffer-window buffer))
              (span (subtle-cue-span buffer index)))
    (let ((continuous (eq subtle-scroll-style 'continuous)))
      (cond ((and continuous (not (subtle-session-paused session)))
             (subtle-scrolling--scroll-or-defer session buffer index))
            ((not (subtle-scrolling--page-needed-p window index)) nil)
            (continuous
             (subtle-scrolling--scroll-or-defer session buffer index))
            (t (with-current-buffer buffer
                 (subtle-scrolling--turn-page session window span)))))))

;;; Following

(defun subtle-scrolling-follow-index (session buffer index)
  "Move BUFFER to cue INDEX of SESSION, unless the user scrolled.
Follows `subtle-scroll-style'."
  (when-let* ((window (get-buffer-window buffer))
              ((integerp index)))
    (if (subtle-scrolling--drifted-p window (window-start window))
        (subtle-scrolling--user-scrolled session)
      (subtle-scrolling--catch-up session buffer index))))

(defun subtle-scrolling-follow-all (session)
  "Follow the current cue in every buffer of SESSION."
  (pcase-dolist (`(,buffer . ,index) (subtle-session-pairs session))
    (subtle-scrolling-follow-index session buffer index)))

(defun subtle-scrolling--window-resized (session window)
  "WINDOW of SESSION changed height: take the new size, and follow again."
  (subtle-scrolling--record-window window)
  (when-let* (((subtle-session-follow session))
              (buffer (window-buffer window))
              (index (subtle-session-index-of session buffer)))
    (subtle-scrolling--catch-up session buffer index)
    (subtle-scrolling--record-window window)))

;;; The follow state

(defun subtle-scrolling--set-follow (session follow)
  "Set the follow state of SESSION to FOLLOW, announcing a change."
  (unless (eq (and follow t) (and (subtle-session-follow session) t))
    (setf (subtle-session-follow session) follow)
    (run-hook-with-args 'subtle-follow-change-functions session follow)))

(defun subtle-scrolling--cancel-timer (session)
  "Cancel the idle timer of SESSION."
  (when (timerp (subtle-session-timer session))
    (cancel-timer (subtle-session-timer session)))
  (setf (subtle-session-timer session) nil))

(defun subtle-scrolling--restart-idle-timer (session)
  "Debounce: start the countdown to resuming autoscroll in SESSION."
  (subtle-scrolling--cancel-timer session)
  (when (subtle-session-auto-resume session)
    (setf (subtle-session-timer session)
          (run-at-time subtle-resume-delay nil
                       #'subtle-scrolling--idle-elapsed session))))

(defun subtle-scrolling--idle-elapsed (session)
  "Resume autoscroll for SESSION if it is still the active one."
  (when (eq session (subtle-current-session))
    (if (subtle-held-p)
        (subtle-scrolling--restart-idle-timer session)
      (subtle-scrolling-resume session))))

(defun subtle-scrolling--user-scrolled (session)
  "The user took over scrolling in SESSION."
  (subtle-scrolling--set-follow
   session (subtle-scroll-follow-next (subtle-session-follow session)
                                      'user-scroll))
  (subtle-scrolling--restart-idle-timer session))

(defun subtle-scrolling--scrolled-p ()
  "Non-nil if the selected window really moved away from where subtle put it."
  (or (subtle-scrolling--drifted-p (selected-window) (window-start))
      (/= 0 (window-vscroll nil t))))

(defun subtle-scrolling-watch ()
  "Buffer-local `post-command-hook': detect the user scrolling."
  (when-let* ((session (subtle-current-session))
              ((memq this-command subtle-scroll-commands))
              ((subtle-scrolling--scrolled-p)))
    (subtle-scrolling--user-scrolled session)))

(defun subtle-scrolling-on-window-scroll (window start)
  "Buffer-local `window-scroll-functions': catch scrolling by any command.
WINDOW now starts at START."
  (when-let* ((session (subtle-current-session)))
    (cond ((subtle-scrolling--resized-p window)
           (subtle-scrolling--window-resized session window))
          ((and (subtle-scrolling--user-command-p)
                (subtle-scrolling--drifted-p window start))
           (subtle-scrolling--user-scrolled session)))))

(defun subtle-scrolling-resume (session &optional jump)
  "Resume autoscroll in SESSION.
With JUMP, move every window to its current cue.  Without it (the automatic
resume), a window moves only if its cue is off screen or inside a margin."
  (subtle-scrolling--cancel-timer session)
  (subtle-scrolling--set-follow
   session (subtle-scroll-follow-next (subtle-session-follow session) 'resume))
  (subtle-scrolling--forget-expected-starts session)
  (if jump
      (subtle-scrolling--reveal-all session)
    (pcase-dolist (`(,buffer . ,index) (subtle-session-pairs session))
      (subtle-scrolling--catch-up session buffer index)
      (when-let* ((window (get-buffer-window buffer)))
        (subtle-scrolling--record-window window)))))

(defun subtle-scrolling-stop (session)
  "Cancel every timer of the scrolling of SESSION."
  (subtle-scrolling--cancel-timer session)
  (subtle-scrolling--stop-poll session))

;;; Commands

;;;###autoload
(defun subtle-resume-follow ()
  "Resume autoscrolling now.
Every window moves to the spoken cue."
  (interactive)
  (subtle-scrolling-resume (subtle-require-session) t))

;;;###autoload
(defun subtle-toggle-auto-resume ()
  "Toggle automatic autoscroll resume."
  (interactive)
  (let ((session (subtle-require-session)))
    (setf (subtle-session-auto-resume session)
          (not (subtle-session-auto-resume session)))
    (unless (subtle-session-follow session)
      (subtle-scrolling--restart-idle-timer session)
      (run-hook-with-args 'subtle-follow-change-functions session nil))))

(provide 'subtle-scrolling)
;;; subtle-scrolling.el ends here
