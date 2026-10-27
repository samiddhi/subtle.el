;;; subtle.el --- Read subtitles in Emacs while mpv plays -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <you@example.com>
;; Maintainer: smdi <you@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: multimedia, convenience
;; URL: https://github.com/YOUR-NAME/subtle
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; subtle plays media in mpv and shows its subtitle files (SRT or VTT,
;; up to two) as read-only text in Emacs, with the spoken cue
;; highlighted.  It is made for studying a language from films,
;; podcasts and lectures.
;;
;; Start with `M-x subtle-open', or `subtle-dired-open' from Dired.
;; The text stays still until the spoken cue nears the bottom of the
;; window (or leaves the top), then the page turns.  Scrolling by hand
;; pauses the autoscroll; it resumes after `subtle-resume-delay'
;; seconds or when you press r.  Autoscroll never moves the window
;; while the cue is on screen and clear of the margins, whether playback
;; is paused or not.
;;
;; Keys are single letters (rarer ones follow the prefix s) so they work
;; in a modal editor's motion state; press h for the full list, q to
;; quit.  The keys are in `subtle-mode-map'; bind your own with
;; `keymap-set'.  Layouts, cue and paragraph filling, margins and
;; everything else are options of the `subtle' group; `M-x subtle-tutorial'
;; and `M-x subtle-manual' (also subtle.info) describe them.
;;
;; Requires mpv.  Other packages, such as bltzread (word annotation),
;; connect to subtle through its hooks; subtle never loads them.

;;; Code:

(require 'subtle-custom)
(require 'subtle-session)
(require 'subtle-track)
(require 'subtle-parse)
(require 'subtle-mpv)
(require 'subtle-layout)
(require 'subtle-keys)
(require 'subtle-ui)
(require 'subtle-point-track)
(require 'subtle-pick)
(require 'subtle-doc)
(require 'wid-edit)
(require 'cus-edit)

(declare-function dired-get-marked-files "dired")
(declare-function bookmark-prop-get "bookmark")

;;; Sending to mpv

(defun subtle--send (&rest args)
  "Send the mpv command ARGS to the active session."
  (apply #'subtle-mpv-send (subtle-session-mpv (subtle-require-session)) args))

(defun subtle--seek-relative (seconds)
  "Skip SECONDS (negative: backwards)."
  (subtle--send "seek" seconds "relative"))

(defun subtle--clamp-speed (speed)
  "SPEED limited to `subtle-speed-range' and rounded to two decimals."
  (let ((low (car subtle-speed-range))
        (high (cdr subtle-speed-range)))
    (/ (round (* 100 (max low (min high speed)))) 100.0)))

(defun subtle--set-speed (speed)
  "Play at SPEED."
  (subtle--send "set_property" "speed" (subtle--clamp-speed speed)))

(defun subtle--change-speed (delta)
  "Add DELTA to the playback speed."
  (subtle--set-speed (+ delta (subtle-session-speed (subtle-require-session)))))

(defun subtle--jump (finder)
  "Seek to the time that FINDER gives for the current track and position."
  (let* ((session (subtle-require-session))
         (target (funcall finder
                          (subtle-current-track session)
                          (subtle-session-position session))))
    (if target
        (subtle--send "seek" target "absolute")
      (user-error "No further subtitle"))))

;;; Commands

(defun subtle-toggle-pause ()
  "Pause or resume playback."
  (interactive)
  (subtle--send "cycle" "pause"))

(defun subtle-seek-forward ()
  "Skip forward a little."
  (interactive)
  (subtle--seek-relative subtle-seek-small))

(defun subtle-seek-backward ()
  "Skip back a little."
  (interactive)
  (subtle--seek-relative (- subtle-seek-small)))

(defun subtle-seek-forward-big ()
  "Skip forward far."
  (interactive)
  (subtle--seek-relative subtle-seek-big))

(defun subtle-seek-backward-big ()
  "Skip back far."
  (interactive)
  (subtle--seek-relative (- subtle-seek-big)))

(defun subtle-faster ()
  "Play faster."
  (interactive)
  (subtle--change-speed subtle-speed-step))

(defun subtle-slower ()
  "Play slower."
  (interactive)
  (subtle--change-speed (- subtle-speed-step)))

(defun subtle-reset-speed ()
  "Play at normal speed."
  (interactive)
  (subtle--set-speed 1.0))

(defun subtle-next-cue ()
  "Jump to the next subtitle."
  (interactive)
  (subtle--jump #'subtle-track-next-start))

(defun subtle-previous-cue ()
  "Jump to this or the previous subtitle.
Within `subtle-previous-cue-restart' seconds of a cue's start, go to the
one before it."
  (interactive)
  (subtle--jump (lambda (track time)
                  (subtle-track-previous-start
                   track time subtle-previous-cue-restart))))

(defun subtle-seek-to-cue-at-point ()
  "Play from the subtitle under point."
  (interactive)
  (subtle--send "seek" (subtle-cue-start (subtle-cue-at-point)) "absolute"))

(defun subtle-quit ()
  "Close what is on screen (a card, the notice box), else quit (asks y/n)."
  (interactive)
  (cond ((eq (subtle-popup-kind) 'help) (subtle-popup-hide 'help))
        ((run-hook-with-args-until-success 'subtle-dismiss-functions))
        ((subtle-popup-visible-p) (subtle-popup-hide))
        ((or (not subtle-confirm-quit) (y-or-n-p "Close subtle? "))
         (subtle-close))))

(defun subtle-close ()
  "End the session: stop mpv, kill the buffers, restore the windows."
  (interactive)
  (when-let* ((session subtle--session))
    (when (and (subtle-session-send session) (not (subtle-session-paused session)))
      (ignore-errors (funcall (subtle-session-send session) "set_property" "pause" t)))
    (run-hooks 'subtle-before-close-hook)
    (run-hooks 'subtle-close-hook)
    (setq subtle--session nil)
    (subtle-mpv-stop (subtle-session-mpv session))
    (subtle-ui-teardown session)))

;;; Setting options

(defun subtle--option-members (group)
  "Options of GROUP and of its sub-groups, except hooks and faces."
  (mapcan (lambda (member)
            (pcase member
              (`(,(or 'subtle-hooks 'subtle-faces) custom-group) nil)
              (`(,name custom-group) (subtle--option-members name))
              (`(,name custom-variable) (list name))))
          (custom-group-members group nil)))

(defun subtle--choices (option)
  "Values OPTION may take if its type is a `choice' of constants, else nil."
  (let ((type (get option 'custom-type)))
    (and (eq (car-safe type) 'choice)
         (seq-every-p (lambda (item) (eq (car-safe item) 'const)) (cdr type))
         (mapcar (lambda (item) (car (last item))) (cdr type)))))

(defun subtle--read-choice (prompt choices)
  "Ask with PROMPT for one of the CHOICES (any values), return it."
  (let* ((names (mapcar (lambda (choice) (format "%S" choice)) choices))
         (answer (completing-read prompt names nil t)))
    (nth (seq-position names answer) choices)))

(defun subtle--read-typed (prompt option)
  "Read a Lisp value for OPTION until it matches the type of OPTION.
PROMPT is shown, with the current value as the initial input."
  (let ((type (widget-convert (get option 'custom-type)))
        (value (read-minibuffer prompt (format "%S" (symbol-value option)))))
    (while (not (widget-apply type :match value))
      (setq value (read-minibuffer "Wrong type; again: " (format "%S" value))))
    value))

(defun subtle--read-value (option)
  "Ask for a new value of OPTION: a menu for choices, else by type."
  (let ((prompt (format "%s (now %S): " option (symbol-value option))))
    (cond ((subtle--choices option)
           (subtle--read-choice prompt (subtle--choices option)))
          ((eq (get option 'custom-type) 'boolean)
           (y-or-n-p (format "%s on? " option)))
          (t (subtle--read-typed prompt option)))))

(defun subtle-set-option (option value)
  "Set a subtle option, asking which one and its value.
Sets OPTION to VALUE.  Applies at once to an open session, through the
`:set' function of the option, and lasts for this Emacs session.  Use
\\[customize-option] to make a change permanent."
  (interactive
   (let ((option (intern (completing-read
                          "Subtle option: "
                          (mapcar #'symbol-name (subtle--option-members 'subtle))
                          nil t))))
     (list option (subtle--read-value option))))
  (customize-set-variable option value)
  (message "%s is now %S" option value))

(defun subtle-set-layout (layout)
  "Choose how subtitle text is laid out.
LAYOUT is a value of `subtle-layout': single-spaced, sentence, paragraphs..."
  (interactive (list (subtle--read-value 'subtle-layout)))
  (subtle-set-option 'subtle-layout layout))

;;; Opening

(defun subtle--handle (session message)
  "Apply the mpv MESSAGE to SESSION."
  (pcase message
    (`(time-pos . ,(and time (pred numberp))) (subtle-ui-tick session time))
    (`(pause . ,paused) (subtle-ui-set-paused session paused))
    (`(speed . ,(and speed (pred numberp))) (subtle-ui-set-speed session speed)))
  (run-hook-with-args 'subtle-mpv-message-functions session message))

(defun subtle--exited (session)
  "Close SESSION if it is still the active one, since mpv went away."
  (when (eq session subtle--session)
    (message "subtle: mpv exited")
    ;; not from the process sentinel: closing may ask questions
    (run-at-time 0 nil (lambda () (when (eq session subtle--session) (subtle-close))))))

(defun subtle--url-p (string)
  "Non-nil if STRING resembles a URL rather than a file."
  (string-match-p "\\`[a-z]+://" string))

(defun subtle--validate (media subs)
  "Signal a `user-error' unless MEDIA and SUBS can be played."
  (unless (executable-find subtle-mpv-executable)
    (user-error "Cannot find mpv (%s)" subtle-mpv-executable))
  (unless (or (subtle--url-p media) (file-exists-p media))
    (user-error "No such media: %s" media))
  (when (> (length subs) subtle-max-subtitles)
    (user-error "At most %d subtitle files, got %d"
                subtle-max-subtitles (length subs)))
  (dolist (sub subs)
    (unless (file-readable-p sub)
      (user-error "Cannot read subtitles: %s" sub))))

(defun subtle--tracks (subs)
  "One track per file of SUBS, or a single empty track."
  (if subs
      (mapcar #'subtle-parse-file subs)
    (list (subtle-parse-from-cues nil))))

(defun subtle--labels (media subs)
  "Display names for SUBS, or for MEDIA when there are none."
  (mapcar #'file-name-nondirectory (or subs (list media))))

;;;###autoload
(defun subtle-open (media &rest subs)
  "Play MEDIA with mpv, showing the subtitle files SUBS (at most two).
Runs `subtle-open-hook' once the session is up."
  (interactive (subtle-pick-args))
  (subtle--validate media subs)
  (subtle-close)
  (let ((session (subtle-ui-start (subtle--tracks subs) (subtle--labels media subs))))
    (setq subtle--session session)
    (setf (subtle-session-media session) media
          (subtle-session-subs session) subs)
    (setf (subtle-session-mpv session)
          (subtle-mpv-start media
                             (lambda (message) (subtle--handle session message))
                             (lambda () (subtle--exited session))))
    (setf (subtle-session-send session)
          (apply-partially #'subtle-mpv-send (subtle-session-mpv session)))
    (run-hooks 'subtle-open-hook)
    session))

;;; Bookmarks

(defvar bookmark-make-record-function)

(defun subtle-bookmark-make-record ()
  "Bookmark record for the open session: media, subtitles and playback time."
  (let ((session (subtle-require-session)))
    `(,(file-name-base (subtle-session-media session))
      (media . ,(subtle-session-media session))
      (subs . ,(subtle-session-subs session))
      (time . ,(subtle-session-position session))
      (handler . subtle-bookmark-jump))))

;;;###autoload
(defun subtle-bookmark-jump (bookmark)
  "Reopen the media of BOOKMARK, `subtle-bookmark-rewind' seconds earlier.
It starts paused unless `subtle-bookmark-autoplay' is non-nil."
  (require 'bookmark)
  (let* ((start (max 0 (- (or (bookmark-prop-get bookmark 'time) 0)
                    subtle-bookmark-rewind)))
         (subtle-mpv-extra-args
          (append subtle-mpv-extra-args
                  (list (format "--start=%s" start))
                  (unless subtle-bookmark-autoplay (list "--pause")))))
    (apply #'subtle-open (bookmark-prop-get bookmark 'media)
           (bookmark-prop-get bookmark 'subs))))

;;;###autoload
(defun subtle-dired-open ()
  "Play the marked files (or the file at point) from Dired.
The media file and its subtitles are told apart by extension; with no
subtitles selected, those beside the media sharing its name are used."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (dir (file-name-directory (car files)))
         (names (subtle-pick--resolve (mapcar #'file-name-nondirectory files)
                                       (directory-files dir nil nil t))))
    (unless (car names)
      (user-error "No media file found for the selection"))
    (apply #'subtle-open (mapcar (lambda (name) (expand-file-name name dir))
                                  (cons (car names) (cdr names))))))

;;; Info manual

(defconst subtle--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding subtle.info.")

(defvar Info-additional-directory-list)

(with-eval-after-load 'info
  (add-to-list 'Info-additional-directory-list subtle--directory))

(provide 'subtle)
;;; subtle.el ends here
