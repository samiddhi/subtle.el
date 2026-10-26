;;; subtle.el --- Read subtitles in Emacs while mpv plays -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: multimedia

;;; Commentary:

;; M-x subtle-open picks a media file and up to two subtitle files,
;; plays the media in mpv, and shows each subtitle track as read-only
;; text with the spoken cue highlighted and kept at 2/3 of the window.
;; Scrolling by hand pauses the autoscroll; it resumes after
;; `subtle-resume-delay' seconds or when you press r.
;;
;; Keys are single letters (rarer ones follow the prefix s) so they work
;; in meow's motion state; press h for the full list, q to quit.  See `subtle-controls'.

;;; Code:

(require 'subtle-custom)
(require 'subtle-track)
(require 'subtle-parse)
(require 'subtle-mpv)
(require 'subtle-layout)
(require 'subtle-ui)
(require 'subtle-pick)

(declare-function dired-get-marked-files "dired")
(declare-function bookmark-prop-get "bookmark")

;;; Sending to mpv

(declare-function bltzread-popup-visible-p "bltzread-popup")
(declare-function bltzread-popup-anchor "bltzread")
(declare-function bltzread-dismiss-popup "bltzread")

(declare-function bltzread-offer-record "bltzread")
(declare-function bltzread-popup-hide "bltzread-popup")
(defvar bltzread-mode)

(defun subtle--offer-reading-records ()
  "Let bltzread ask about every buffer of the closing session that has it on."
  (dolist (buffer (and subtle--session (subtle-session-buffers subtle--session)))
    (when (and (buffer-live-p buffer) (buffer-local-value 'bltzread-mode buffer))
      (with-current-buffer buffer (bltzread-offer-record)))))

(with-eval-after-load 'bltzread
  ;; bltzread's word card holds the text still, anchors the page, and closes on request.
  (add-hook 'subtle-hold-functions #'bltzread-popup-visible-p)
  (add-hook 'subtle-anchor-functions #'bltzread-popup-anchor)
  (add-hook 'subtle-dismiss-functions #'bltzread-dismiss-popup)
  (add-hook 'subtle-before-close-hook #'subtle--offer-reading-records)
  (add-hook 'subtle-close-hook #'bltzread-popup-hide))

(defun subtle--send (&rest args)
  "Send the mpv command ARGS to the active session."
  (apply #'subtle-mpv-send (subtle-session-mpv (subtle-ui-session)) args))

(defun subtle--seek-relative (seconds)
  "Skip SECONDS (negative goes back)."
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
  (subtle--set-speed (+ delta (subtle-session-speed (subtle-ui-session)))))

(defun subtle--jump (finder)
  "Seek to the time FINDER returns for the current track and position."
  (let* ((session (subtle-ui-session))
         (target (funcall finder
                          (subtle-ui-current-track session)
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
  "Jump to this or the previous subtitle."
  (interactive)
  (subtle--jump #'subtle-track-previous-start))

(defun subtle-seek-to-cue-at-point ()
  "Play from the subtitle under point."
  (interactive)
  (subtle--send "seek" (subtle-cue-start (subtle-ui-cue-at-point)) "absolute"))

(defun subtle-quit ()
  "Close what is on screen (a card, the notice box), else quit (asks y/n)."
  (interactive)
  (cond ((run-hook-with-args-until-success 'subtle-dismiss-functions))
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

(defconst subtle--options
  '(subtle-layout subtle-paragraph-source subtle-paragraph-gap
    subtle-paragraph-pause subtle-popup-style subtle-text-scale
    subtle-resume-delay subtle-auto-resume subtle-focus-ratio)
  "Options offered by `subtle-set-option'.")

(defun subtle--choices (option)
  "Symbols OPTION may take, from its `choice' type, or nil if it is not one."
  (let ((type (get option 'custom-type)))
    (and (eq (car-safe type) 'choice)
         (delq nil (mapcar (lambda (item) (and (eq (car-safe item) 'const) (nth 1 item)))
                           (cdr type))))))

(defconst subtle--prompts
  '((subtle-paragraph-gap . "Start a new paragraph after this much silence (s)")
    (subtle-paragraph-pause . "Pause the audio this long at a paragraph end (s)"))
  "Plain-language prompts for options whose names are easy to mix up.")

(defun subtle--prompt (option current)
  "Prompt text asking for OPTION, showing CURRENT."
  (format "%s [%s] (now %s): "
          (or (alist-get option subtle--prompts) option) option current))

(defun subtle--read-value (option)
  "Ask for a new value of OPTION: a menu for choices, else by type."
  (let ((current (symbol-value option)))
    (cond ((subtle--choices option)
           (intern (completing-read
                    (subtle--prompt option current)
                    (mapcar #'symbol-name (subtle--choices option)) nil t nil nil
                    (symbol-name current))))
          ((booleanp current)
           (intern (completing-read (subtle--prompt option current) '("t" "nil") nil t)))
          (t (read-number (subtle--prompt option current) current)))))

(defun subtle--apply-option (option value)
  "Set OPTION to VALUE for this Emacs session and refresh what is on screen."
  (customize-set-variable option value)
  (when-let* ((session subtle--session))
    (if (memq option '(subtle-layout subtle-paragraph-source subtle-paragraph-gap
                       subtle-text-scale))
        (subtle-ui-rebuild session)
      (subtle-ui-sync-popup)))
  (message "%s is now %s" option value))

(defun subtle-set-option (option)
  "Change one subtle OPTION, choosing its value from a menu or prompt.
Applies at once to an open session; lasts for this Emacs session.  Use
\[customize-option] to make a change permanent."
  (interactive
   (list (intern (completing-read "Subtle option: "
                                  (mapcar #'symbol-name subtle--options) nil t))))
  (subtle--apply-option option (subtle--read-value option)))

(defmacro subtle--define-setter (name option doc)
  "Define the command NAME that changes OPTION; DOC is its docstring."
  `(defun ,name ()
     ,doc
     (interactive)
     (subtle--apply-option ',option (subtle--read-value ',option))))

(subtle--define-setter subtle-set-layout subtle-layout
  "Choose how subtitle text is laid out (single-spaced, sentence, paragraphs...).")
(subtle--define-setter subtle-set-paragraph-source subtle-paragraph-source
  "Choose what counts as a paragraph break.")
(subtle--define-setter subtle-set-popup-style subtle-popup-style
  "Choose how the \"autoscroll paused\" notice is shown.")
(subtle--define-setter subtle-set-paragraph-pause subtle-paragraph-pause
  "Set how long the audio pauses at a paragraph end.
This does not change where paragraphs break; see `subtle-set-paragraph-gap'.")
(subtle--define-setter subtle-set-paragraph-gap subtle-paragraph-gap
  "Set how much silence between cues starts a new paragraph.
Smaller means shorter paragraphs.  Needs `subtle-paragraph-source' to be
`explicit-and-gap' and a paragraph layout.")
(subtle--define-setter subtle-set-text-scale subtle-text-scale
  "Set the size of subtitle text.")

;;; Opening

(defun subtle--handle (session message)
  "Apply the mpv MESSAGE to SESSION."
  (pcase message
    (`(time-pos . ,(and time (pred numberp))) (subtle-ui-tick session time))
    (`(pause . ,paused) (subtle-ui-set-paused session paused))
    (`(speed . ,(and speed (pred numberp))) (subtle-ui-set-speed session speed))))

(defun subtle--exited (session)
  "mpv went away: close SESSION if it is still the active one."
  (when (eq session subtle--session)
    (message "subtle: mpv exited")
    ;; not from the process sentinel: closing may ask questions
    (run-at-time 0 nil (lambda () (when (eq session subtle--session) (subtle-close))))))

(defun subtle--url-p (string)
  "Non-nil if STRING looks like a URL rather than a file."
  (string-match-p "\\`[a-z]+://" string))

(defun subtle--validate (media subs)
  "Signal a `user-error' unless MEDIA and SUBS can be played."
  (unless (executable-find subtle-mpv-executable)
    (user-error "Cannot find mpv (%s)" subtle-mpv-executable))
  (unless (or (subtle--url-p media) (file-exists-p media))
    (user-error "No such media: %s" media))
  (when (> (length subs) 2)
    (user-error "At most two subtitle files, got %d" (length subs)))
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
  "Play MEDIA with mpv, showing the subtitle files SUBS (at most two)."
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
    session))

;;; Bookmarks

(defvar bookmark-make-record-function)

(defun subtle-bookmark-make-record ()
  "Bookmark record for the open session: media, subtitles and playback time."
  (let ((session (subtle-ui-session)))
    `(,(file-name-base (subtle-session-media session))
      (media . ,(subtle-session-media session))
      (subs . ,(subtle-session-subs session))
      (time . ,(subtle-session-position session))
      (handler . subtle-bookmark-jump))))

;;;###autoload
(defun subtle-bookmark-jump (bookmark)
  "Reopen the media of BOOKMARK a few seconds before where it was left."
  (require 'bookmark)
  (let* ((start (max 0 (- (or (bookmark-prop-get bookmark 'time) 0) 3)))
         (subtle-mpv-extra-args
          (append subtle-mpv-extra-args (list (format "--start=%s" start)))))
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
