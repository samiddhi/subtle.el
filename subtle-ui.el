;;; subtle-ui.el --- Buffers, highlight, autoscroll and hint -*- lexical-binding: t; -*-

;;; Commentary:

;; Everything the user sees.  Text builders are pure; the rest moves
;; overlays and windows.  A single `subtle-session' holds the state.

;;; Code:

(require 'cl-lib)
(require 'face-remap)
(require 'seq)
(require 'subr-x)
(require 'subtle-custom)
(require 'subtle-track)
(require 'subtle-scroll)
(require 'subtle-popup)
(require 'subtle-layout)

(cl-defstruct subtle-session
  mpv tracks buffers config timer shown-second
  (indices nil) (position 0.0) (paused nil) (speed 1.0) (follow t)
  (auto-resume subtle-auto-resume)
  pause-points pause-timer pausing send hold-timer hold-paused media subs)

(declare-function subtle-bookmark-make-record "subtle")

(defvar subtle--session nil
  "The active `subtle-session', or nil.")

(defvar-local subtle-ui--track nil "Track shown by this buffer.")
(defvar-local subtle-ui--spans nil "Vector of (BEG . END) per cue.")
(defvar-local subtle-ui--overlay nil "Overlay marking the current cue.")
(defvar-local subtle-ui--label "" "Name shown in the header line.")
(defvar-local subtle-ui--font-cookies nil "Face remappings made by `subtle-ui--apply-font'.")

(defvar-local subtle-ui--run nil
  "(START . END) buffer positions of the latest run of continuous playback, or nil.")

(defconst subtle-ui--header '(:eval (subtle-ui--header-string)))

;;; Pure text

(defun subtle-ui--format-time (seconds)
  "SECONDS as MM:SS, or H:MM:SS past an hour."
  (let* ((s (floor seconds))
         (h (/ s 3600)))
    (if (> h 0)
        (format "%d:%02d:%02d" h (/ (% s 3600) 60) (% s 60))
      (format "%02d:%02d" (/ s 60) (% s 60)))))

(defun subtle-ui--key (key)
  "KEY as a propertized string."
  (propertize key 'face 'subtle-key))

(defun subtle-ui--default-keys ()
  "Keys of the table `subtle-controls' ships with."
  (mapcar #'car (eval (car (get 'subtle-controls 'standard-value)) t)))

(defun subtle-ui--help-groups (controls)
  "CONTROLS grouped by command: list of (COMMAND KEY...), in table order."
  (let (groups)
    (dolist (c controls)
      (if-let* ((group (assq (nth 1 c) groups)))
          (setcdr (last group) (list (nth 0 c)))
        (push (list (nth 1 c) (nth 0 c)) groups)))
    (nreverse groups)))

(defun subtle-ui--split-keys (keys defaults)
  "Cons of the KEYS the user chose and those that are among DEFAULTS."
  (cons (seq-remove (lambda (k) (member k defaults)) keys)
        (seq-filter (lambda (k) (member k defaults)) keys)))

(defun subtle-ui--group-hint (controls command)
  "Control-strip label of COMMAND in CONTROLS, or nil."
  (seq-some (lambda (c) (and (eq (nth 1 c) command) (nth 2 c))) controls))

(defun subtle-ui--shown-key (keys defaults)
  "The key to show for KEYS: the user's own if any, else the first."
  (let ((split (subtle-ui--split-keys keys defaults)))
    (or (car (car split)) (car keys))))

(defun subtle-ui--hints (controls &optional defaults)
  "Control strip text for the visible commands of CONTROLS.
A key the user added is shown instead of the default one."
  (let ((defaults (or defaults (subtle-ui--default-keys))))
    (mapconcat
     (lambda (group)
       (format "%s %s" (subtle-ui--key (subtle-ui--shown-key (cdr group) defaults))
               (subtle-ui--group-hint controls (car group))))
     (seq-filter (lambda (g) (subtle-ui--group-hint controls (car g)))
                 (subtle-ui--help-groups controls))
     "  ")))

(defun subtle-ui--state-string (paused speed position)
  "Playback state text for the header line."
  (format "%s %gx %s" (if paused "[paused]" "[playing]") speed
          (subtle-ui--format-time position)))

(defun subtle-ui--key-for (controls command)
  "Key of CONTROLS to show for COMMAND (the user's own first), or \"?\"."
  (if-let* ((group (assq command (subtle-ui--help-groups controls))))
      (subtle-ui--shown-key (cdr group) (subtle-ui--default-keys))
    "?"))

(defun subtle-ui--hint-rows (auto-resume delay &optional controls)
  "Rows of the \"autoscroll paused\" box; keys are looked up in CONTROLS."
  (let ((controls (or controls subtle-controls)))
    (list (subtle-popup-title "AUTOSCROLL PAUSED")
          (concat (subtle-popup-key (subtle-ui--key-for controls 'subtle-resume-follow))
                  "  resume now")
          (concat (subtle-popup-key (subtle-ui--key-for controls 'subtle-toggle-auto-resume))
                  "  auto-resume "
                  (if auto-resume (format "on (%gs)" delay) "off"))
          (concat (subtle-popup-key (subtle-ui--key-for controls 'subtle-quit))
                  "  hide this box"))))

(defconst subtle-ui--help-doc-width 44 "Longest description shown in the help box.")

(defun subtle-ui--first-line (command)
  "First line of the docstring of COMMAND."
  (truncate-string-to-width
   (car (split-string (or (documentation command) "") "\n"))
   subtle-ui--help-doc-width nil nil "…"))

(defun subtle-ui--keys-text (keys defaults)
  "KEYS as text: the user's own first, the default ones after in parentheses."
  (pcase-let ((`(,mine . ,theirs) (subtle-ui--split-keys keys defaults)))
    (cond ((null mine) (string-join theirs " / "))
          ((null theirs) (string-join mine " / "))
          (t (format "%s (%s)" (string-join mine " / ") (string-join theirs " / "))))))

(defun subtle-ui--help-rows (controls)
  "Rows of the help box describing every entry of CONTROLS."
  (let* ((groups (subtle-ui--help-groups controls))
         (defaults (subtle-ui--default-keys))
         (keys (mapcar (lambda (g) (subtle-ui--keys-text (cdr g) defaults)) groups))
         (width (apply #'max (mapcar #'string-width keys))))
    (cons (subtle-popup-title "SUBTLE KEYS")
          (cl-mapcar (lambda (g key)
                       (concat (propertize (subtle-popup--pad key width)
                                           'face 'subtle-popup-key)
                               "  " (subtle-ui--first-line (car g))))
                     groups keys))))

(defun subtle-ui--header-parts (state label hints count index)
  "Header segments for buffer INDEX out of COUNT buffers."
  (cond ((= count 1) (list state hints))
        ((= index 0) (list state label))
        (t (list hints))))

(defun subtle-ui--header-string ()
  "Header line of the current subtle buffer."
  (if-let* ((session subtle--session))
      (let ((buffers (subtle-session-buffers session)))
        (string-join
         (subtle-ui--header-parts
          (subtle-ui--state-string (subtle-session-paused session)
                                    (subtle-session-speed session)
                                    (subtle-session-position session))
          subtle-ui--label
          (subtle-ui--hints subtle-controls)
          (length buffers)
          (or (seq-position buffers (current-buffer)) 0))
         "  "))
    ""))

;;; Session access

(defun subtle-ui-session ()
  "The active session, or a `user-error'."
  (or subtle--session (user-error "No subtle session")))

(defun subtle-ui-current-track (session)
  "Track of the current buffer, falling back to the first of SESSION."
  (or subtle-ui--track (car (subtle-session-tracks session))))

(defun subtle-ui-cue-at-point ()
  "Cue under point in a subtle buffer, or a `user-error'."
  (let ((index (get-text-property (point) 'subtle-index)))
    (if index
        (aref (subtle-track-cues subtle-ui--track) index)
      (user-error "No subtitle at point"))))

;;; Buffers

(defun subtle-ui--insert-cue (cue index next)
  "Insert CUE tagged with INDEX, then what separates it from NEXT.
Return the (BEG . END) of the cue text."
  (let ((beg (point)))
    (insert (propertize (subtle-layout-text subtle-layout cue) 'subtle-index index))
    (prog1 (cons beg (point))
      (when next
        (insert (subtle-layout-separator subtle-layout subtle-paragraph-source
                                          cue next))))))

(defun subtle-ui--insert-cues (track)
  "Insert every cue of TRACK; return the vector of spans."
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        (cues (subtle-track-cues track)))
    (vconcat (seq-map-indexed
              (lambda (cue i)
                (subtle-ui--insert-cue
                 cue i (and (< (1+ i) (length cues)) (aref cues (1+ i)))))
              cues))))

(defun subtle-ui--new-overlay ()
  "Detached overlay carrying the `subtle-current' face."
  (let ((overlay (make-overlay (point-min) (point-min))))
    (overlay-put overlay 'face (if (subtle-layout-inline-p subtle-layout)
                                   'subtle-current-inline
                                 'subtle-current))
    (delete-overlay overlay)
    overlay))

(defun subtle-ui--font-specs ()
  "Face specs remapped onto `default' in subtle buffers."
  (append (and subtle-use-variable-pitch '(variable-pitch))
          (list (list :height subtle-text-scale))))

(defun subtle-ui--apply-font ()
  "Show this buffer in the configured font and size.
Safe to call again: the previous remappings are removed first, since
relative remappings would otherwise stack and keep enlarging the text."
  (mapc #'face-remap-remove-relative subtle-ui--font-cookies)
  (setq subtle-ui--font-cookies
        (list (apply #'face-remap-add-relative 'default (subtle-ui--font-specs))
              ;; Keep the control strip at normal size so the hints stay visible.
              (face-remap-add-relative 'header-line
                                       :height (face-attribute 'default :height)))))

(define-derived-mode subtle-mode special-mode "Subtle"
  "Read-only view of one subtitle track kept in sync with mpv."
  (setq-local header-line-format subtle-ui--header)
  (setq-local truncate-lines nil)
  (subtle-ui--apply-font)
  (visual-line-mode 1)
  (add-hook 'post-command-hook #'subtle-ui--watch-scroll nil t)
  (add-hook 'window-scroll-functions #'subtle-ui--on-window-scroll nil t))

(defun subtle-ui--install-controls (controls)
  "Bind every (KEY COMMAND . _) of CONTROLS in `subtle-mode-map'."
  (dolist (control controls)
    (let ((keys (kbd (nth 0 control))))
      ;; A key the parent mode binds to a command must become a prefix of our own.
      (dotimes (i (1- (length keys)))
        (let ((prefix (seq-subseq keys 0 (1+ i))))
          (unless (keymapp (lookup-key subtle-mode-map prefix))
            (define-key subtle-mode-map prefix (make-sparse-keymap)))))
      (define-key subtle-mode-map keys (nth 1 control)))))

(defun subtle-reload-controls ()
  "Rebind `subtle-mode-map' from `subtle-controls'."
  (interactive)
  (subtle-ui--install-controls subtle-controls))

(subtle-reload-controls)

(defvar-local subtle-track-index nil "Position of the buffer's file among the session's files.")
(defvar-local subtle-language nil
  "Language of the buffer's subtitle file, from its name.
See `subtle-language-codes'.")

(defun subtle-ui--language (label)
  "Language code in the file name LABEL (`movie.sl.srt' gives \"slv\"), or nil."
  (let ((parts (cdr (split-string (file-name-sans-extension label) "\\." t))))
    (when-let* ((part (car (last parts))))
      (or (cdr (assoc-string part subtle-language-codes t))
          (and (string-match-p "\\`[a-z]\\{2,3\\}\\'" part) part)))))

(defun subtle-ui--make-buffer (track label &optional index)
  "Buffer showing TRACK, named after LABEL; INDEX is its place among the files."
  (with-current-buffer (generate-new-buffer (format "*subtle: %s*" label))
    (subtle-mode)
    (setq-local bookmark-make-record-function #'subtle-bookmark-make-record)
    (setq subtle-ui--track track
          subtle-ui--label label
          subtle-ui--spans (subtle-ui--insert-cues track)
          subtle-ui--overlay (subtle-ui--new-overlay))
    (goto-char (point-min))
    (setq subtle-track-index (or index 0)
          subtle-language (subtle-ui--language label))
    (run-hooks 'subtle-buffer-setup-hook)
    (current-buffer)))

(defun subtle-ui--layout (buffers)
  "Show BUFFERS in the selected window, side by side when there are two."
  (switch-to-buffer (car buffers))
  (when (cdr buffers)
    (set-window-buffer (split-window-right) (cadr buffers))))

;;; Highlight and scroll

(defun subtle-ui--highlight (index)
  "Move the overlay of the current buffer onto cue INDEX, or hide it."
  (if-let* ((span (and index (aref subtle-ui--spans index))))
      (move-overlay subtle-ui--overlay (car span) (cdr span) (current-buffer))
    (delete-overlay subtle-ui--overlay)))

(defun subtle-ui--pos-above (window pos pixels)
  "Buffer position about PIXELS of text height above POS in WINDOW.
Pixels, not lines, so enlarged or proportional fonts land correctly."
  (with-selected-window window
    (save-excursion
      (goto-char pos)
      (while (and (> pixels (/ (line-pixel-height) 2)) (= -1 (vertical-motion -1)))
        (setq pixels (- pixels (line-pixel-height))))
      (point))))

(defun subtle-ui--record-window (window)
  "Remember where WINDOW starts and how tall it is, as subtle's own doing."
  (set-window-parameter window 'subtle-start (window-start window))
  (set-window-parameter window 'subtle-height (window-body-height window t)))

(defun subtle-ui--cursor-idle-p (window)
  "Non-nil if the cursor of WINDOW looks unused: where subtle left it, on the
first line, or out of sight."
  (and subtle-move-idle-cursor
       (let ((point (window-point window)))
         (or (eql point (window-parameter window 'subtle-point))
             (not (pos-visible-in-window-p point window))
             (with-current-buffer (window-buffer window)
               (save-excursion
                 (goto-char (window-start window))
                 (<= point (line-end-position))))))))

(defun subtle-ui--place (window pos &optional start keep-point)
  "Scroll WINDOW so POS sits where `subtle-scroll-style' wants it.
START, a position, is used as the window start instead.  KEEP-POINT leaves
the cursor alone whatever `subtle-move-point' says."
  (let ((pixels (if (eq subtle-scroll-style 'paged)
                    (* (max 0 subtle-page-lead) (default-line-height))
                  (subtle-scroll-offset (window-body-height window t)
                                        subtle-focus-ratio)))
        (move (and (not keep-point)
                   (subtle-scroll-move-point-p subtle-move-point
                                               (eq window (selected-window))
                                               (subtle-ui--cursor-idle-p window)))))
    (set-window-start window (or start (subtle-ui--pos-above window pos pixels)))
    (subtle-ui--record-window window)
    (when move
      (set-window-point window pos)
      (set-window-parameter window 'subtle-point pos))))

(defun subtle-ui--resized-p (window)
  "Non-nil if WINDOW changed height since subtle last recorded it."
  (let ((height (window-parameter window 'subtle-height)))
    (and height (/= height (window-body-height window t)))))

(defun subtle-ui--drifted-p (window start)
  "Non-nil if START is not where subtle last put WINDOW: the user scrolled.
A resize moves the start by itself and does not count."
  (let ((expected (window-parameter window 'subtle-start)))
    (and expected (/= expected start) (not (subtle-ui--resized-p window)))))

(defun subtle-ui--user-command-p ()
  "Non-nil while a command is running, as opposed to a timer or filter."
  (null (current-idle-time)))

(defun subtle-ui--scroll-to-index (buffer index)
  "Scroll the window of BUFFER to cue INDEX, if shown and valid."
  (when-let* ((window (get-buffer-window buffer))
              ((integerp index)))
    (subtle-ui--place
     window (car (aref (buffer-local-value 'subtle-ui--spans buffer) index)))))

(defun subtle-ui--hold-p ()
  "Non-nil while something needs the text to stay where it is."
  (or (minibuffer-window-active-p (minibuffer-window))
      (and (run-hook-with-args-until-success 'subtle-hold-functions) t)))

(defun subtle-ui--used-pixels (window start end)
  "Height of the text of WINDOW's buffer from START to END, as far as it matters."
  (cdr (window-text-pixel-size window start end nil
                                (1+ (window-body-height window t)))))

(defun subtle-ui--margin-pixels ()
  (* (max 0 subtle-page-margin) (default-line-height)))

(defun subtle-ui--page-needed-p (window index)
  "Non-nil if cue INDEX is too near the bottom of WINDOW, or not in it."
  (let* ((span (aref (buffer-local-value 'subtle-ui--spans (window-buffer window)) index))
         (start (window-start window))
         (before (< (car span) start)))
    (subtle-scroll-page-needed-p
     before (if before 0 (subtle-ui--used-pixels window start (cdr span)))
     (window-body-height window t) (subtle-ui--margin-pixels))))

(defun subtle-ui--line-start (window pos)
  "Start of the screen line of WINDOW holding POS."
  (with-selected-window window
    (save-excursion (goto-char pos) (vertical-motion 0) (point))))

(defun subtle-ui--fits-from-p (window start span)
  "Non-nil if SPAN would clear the page margin with START as the window top."
  (not (subtle-scroll-page-needed-p
        nil (subtle-ui--used-pixels window start (cdr span))
        (window-body-height window t) (subtle-ui--margin-pixels))))

(defun subtle-ui--defer (session)
  "Check every half second whether the hold on SESSION is over."
  (unless (timerp (subtle-session-hold-timer session))
    (setf (subtle-session-hold-timer session)
          (run-at-time 0.5 0.5 #'subtle-ui--poll-hold session))))

(defun subtle-ui--poll-hold (session)
  "Catch up once nothing holds SESSION's text any more."
  (cond ((not (eq session subtle--session))
         (cancel-timer (subtle-session-hold-timer session))
         (setf (subtle-session-hold-timer session) nil))
        ((not (subtle-ui--hold-p))
         (cancel-timer (subtle-session-hold-timer session))
         (setf (subtle-session-hold-timer session) nil)
         (when (subtle-session-follow session)
           (seq-mapn (lambda (buffer index)
                       (when (buffer-live-p buffer)
                         (subtle-ui--follow-index session buffer index)))
                     (subtle-session-buffers session)
                     (subtle-session-indices session))))))

(defun subtle-ui--turn-page (session window buffer index)
  "Show cue INDEX of BUFFER on a new page in WINDOW.
While something is held open, the page starts at the held position if the cue
still fits below it; if not, that thing is dismissed and the page turns anyway."
  (with-current-buffer buffer
    (let* ((span (aref subtle-ui--spans index))
           (hold (subtle-ui--hold-p))
           (anchor (and hold (run-hook-with-args-until-success 'subtle-anchor-functions)))
           (start (and anchor (< anchor (car span)) (subtle-ui--line-start window anchor))))
      (cond ((and start (subtle-ui--fits-from-p window start span))
             (subtle-ui--place window (car span) start t))
            (t (when hold (run-hook-with-args-until-success 'subtle-dismiss-functions))
               (if (subtle-ui--hold-p)
                   (subtle-ui--defer session)
                 (subtle-ui--place window (car span))))))))

(defun subtle-ui--follow-index (session buffer index)
  "Move BUFFER to cue INDEX per `subtle-scroll-style', unless the user scrolled."
  (when-let* ((window (get-buffer-window buffer))
              ((integerp index)))
    (cond ((subtle-ui--drifted-p window (window-start window))
           (subtle-ui--user-scrolled session))
          ((eq subtle-scroll-style 'continuous)
           (if (subtle-ui--hold-p)
               (subtle-ui--defer session)
             (subtle-ui--scroll-to-index buffer index)))
          ((subtle-ui--page-needed-p window index)
           (subtle-ui--turn-page session window buffer index)))))

(defun subtle-ui--focus-cue (session buffer index)
  "Highlight cue INDEX in BUFFER and, while following, scroll to it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (subtle-ui--highlight index))
    (when (subtle-session-follow session)
      (subtle-ui--follow-index session buffer index))))

(defun subtle-ui--window-resized (session window)
  "WINDOW of SESSION changed height: take the new size, and follow again."
  (subtle-ui--record-window window)
  (when (subtle-session-follow session)
    (when-let* ((position (seq-position (subtle-session-buffers session)
                                        (window-buffer window)))
                (index (nth position (subtle-session-indices session))))
      (subtle-ui--scroll-to-index (window-buffer window) index))))

(defun subtle-ui--forget-expected-starts (session)
  "Clear where subtle last scrolled each window of SESSION."
  (dolist (buffer (seq-filter #'buffer-live-p (subtle-session-buffers session)))
    (when-let* ((window (get-buffer-window buffer)))
      (set-window-parameter window 'subtle-start nil))))

(defun subtle-ui--reveal-all (session)
  "Scroll every buffer of SESSION to its current cue."
  (seq-mapn #'subtle-ui--scroll-to-index
            (seq-filter #'buffer-live-p (subtle-session-buffers session))
            (subtle-session-indices session)))

;;; Playback events

(defun subtle-ui--indices-at (session time)
  "Current cue index of every track of SESSION at TIME."
  (mapcar (lambda (track) (subtle-track-cue-index track time subtle-hold-last-cue))
          (subtle-session-tracks session)))

(defun subtle-ui--refresh-header (session time)
  "Redraw header lines when the displayed second of TIME changed."
  (let ((second (floor time)))
    (unless (eql second (subtle-session-shown-second session))
      (setf (subtle-session-shown-second session) second)
      (force-mode-line-update t))))

(defun subtle-ui--cancel-pause (session)
  "Forget a pending paragraph pause of SESSION."
  (when (timerp (subtle-session-pause-timer session))
    (cancel-timer (subtle-session-pause-timer session)))
  (setf (subtle-session-pause-timer session) nil
        (subtle-session-pausing session) nil))

(defun subtle-ui--pause-elapsed (session)
  "The paragraph pause of SESSION is over: let playback go on."
  (setf (subtle-session-pause-timer session) nil)
  (when (and (eq session subtle--session) (subtle-session-send session))
    (funcall (subtle-session-send session) "set_property" "pause" :false)))

(defun subtle-ui--paragraph-pause (session from to)
  "Pause SESSION's playback if it just played past a paragraph end (FROM to TO)."
  (when (and (subtle-session-pause-points session)
             (subtle-session-send session)
             (not (subtle-session-paused session))
             (not (subtle-session-pause-timer session))
             (subtle-layout-pause-crossed (subtle-session-pause-points session)
                                           from to))
    (setf (subtle-session-pausing session) t
          (subtle-session-pause-timer session)
          (run-at-time subtle-paragraph-pause nil
                       #'subtle-ui--pause-elapsed session))
    (funcall (subtle-session-send session) "set_property" "pause" t)))

(defun subtle-ui--hold-pause (session)
  "Pause playback while a held word is far above the spoken line.
Only if `subtle-hold-pause-lines' says so; playback resumes when it is released."
  (when (and subtle-hold-pause-lines (subtle-session-send session))
    (let* ((buffer (window-buffer (selected-window)))
           (position (seq-position (subtle-session-buffers session) buffer))
           (index (and position (nth position (subtle-session-indices session))))
           (far (and index
                     (with-current-buffer buffer
                       (when-let* ((anchor (run-hook-with-args-until-success
                                            'subtle-anchor-functions))
                                   (cue (car (aref subtle-ui--spans index)))
                                   ((< anchor cue)))
                         (subtle-scroll-far-p (count-lines anchor cue)
                                              subtle-hold-pause-lines)))))
           (state (subtle-session-hold-paused session)))
      (cond ((and far (not state) (not (subtle-session-paused session)))
             (setf (subtle-session-hold-paused session) t)
             (funcall (subtle-session-send session) "set_property" "pause" t))
            ((and (not far) state)
             (setf (subtle-session-hold-paused session) nil)
             (when (eq state t)
               (funcall (subtle-session-send session) "set_property" "pause" :false)))))))

(defconst subtle-ui--run-gap 1.0
  "Seconds of media a tick may advance by and still count as playing through.")

(defun subtle-ui--credited-end (track spans index time)
  "Buffer position up to which cue INDEX of TRACK has been played at TIME.
SPANS are the buffer spans of the cues."
  (cond ((>= time (subtle-cue-end (aref (subtle-track-cues track) index)))
         (cdr (aref spans index)))
        ((> index 0) (cdr (aref spans (1- index))))
        (t (car (aref spans 0)))))

(defun subtle-ui--credit-playback (session from time indices)
  "Extend each buffer's played run when SESSION moved from FROM to TIME unbroken.
A seek, or going backwards, starts a new run at the cue now playing.  Pausing
does not break a run, but does not extend it either."
  (let ((playing (and (not (subtle-session-paused session))
                      (> time from) (<= (- time from) subtle-ui--run-gap))))
    (seq-mapn
     (lambda (buffer index)
       (when (and (buffer-live-p buffer) index)
         (with-current-buffer buffer
           (let* ((spans subtle-ui--spans)
                  (start (car (aref spans index)))
                  (end (subtle-ui--credited-end subtle-ui--track spans index time)))
             (cond ((and subtle-ui--run (or playing (= time from)))
                    (when playing
                      (setcdr subtle-ui--run (max (cdr subtle-ui--run) end))))
                   (t (setq subtle-ui--run (cons start (max start end)))))))))
     (subtle-session-buffers session) indices)))

(defun subtle-played-through-position ()
  "End of the text of this buffer that the latest continuous playback reached.
Seeking, or pausing, does not move it; see `subtle-played-start-position'."
  (cdr subtle-ui--run))

(defun subtle-played-start-position ()
  "Where the latest continuous playback run of this buffer began, or nil."
  (car subtle-ui--run))

(defun subtle-ui-tick (session time)
  "Move SESSION to playback position TIME."
  (subtle-ui--hold-pause session)
  (let ((indices (subtle-ui--indices-at session time)))
    (subtle-ui--credit-playback session (subtle-session-position session) time indices)
    (subtle-ui--paragraph-pause session (subtle-session-position session) time)
    (setf (subtle-session-position session) time)
    (unless (equal indices (subtle-session-indices session))
      (setf (subtle-session-indices session) indices)
      (seq-mapn (lambda (buffer index) (subtle-ui--focus-cue session buffer index))
                (subtle-session-buffers session) indices))
    (subtle-ui--refresh-header session time)))

(defun subtle-ui--run-pause-hook (session)
  "Run `subtle-pause-hook' in the buffer of SESSION shown in the selected window."
  (let ((buffer (seq-find (lambda (b) (eq b (window-buffer))) (subtle-session-buffers session))))
    (with-current-buffer (or buffer (car (subtle-session-buffers session)))
      (run-hooks 'subtle-pause-hook))))

(defun subtle-ui-set-paused (session paused)
  "Record that SESSION is PAUSED or playing."
  (if (and paused (subtle-session-pausing session))
      (setf (subtle-session-pausing session) nil)
    ;; The user took over: their choice beats the paragraph pause.
    (subtle-ui--cancel-pause session)
    (when (and paused (not (subtle-session-paused session))
               (not (eq (subtle-session-hold-paused session) t)))
      (subtle-ui--run-pause-hook session)))
  (when (and (not paused) (eq (subtle-session-hold-paused session) t))
    ;; resumed by hand while a word is held: leave it going
    (setf (subtle-session-hold-paused session) 'overridden))
  (setf (subtle-session-paused session) paused)
  (force-mode-line-update t)
  (subtle-ui-sync-popup))

(defun subtle-ui-set-speed (session speed)
  "Record the playback SPEED of SESSION."
  (setf (subtle-session-speed session) speed)
  (force-mode-line-update t))

;;; Hint popup

(defun subtle-ui--popup-wanted-p (follow paused in-subtle minibuffer)
  "Non-nil if the \"autoscroll paused\" notice should be on screen.
It is only useful while audio plays, autoscroll is off, and the user is
looking at a subtle buffer."
  (and (not follow) (not paused) in-subtle (not minibuffer)))

(defun subtle-ui--in-subtle-p (session)
  "Non-nil if the selected window shows a buffer of SESSION."
  (and (memq (window-buffer (selected-window)) (subtle-session-buffers session)) t))

(defun subtle-ui--show-popup (session)
  "Tell the user how to resume autoscroll."
  (subtle-popup-show
   (subtle-ui--hint-rows (subtle-session-auto-resume session)
                          subtle-resume-delay)))

(defun subtle-ui-sync-popup (&rest _)
  "Show or hide the boxes so they match what the user is looking at."
  (when-let* ((session subtle--session))
    (let ((in-subtle (subtle-ui--in-subtle-p session))
          (kind (subtle-popup-kind)))
      (cond ((eq kind 'help)
             (unless in-subtle (subtle-popup-hide)))
            ((subtle-ui--popup-wanted-p
              (subtle-session-follow session) (subtle-session-paused session)
              in-subtle (minibuffer-window-active-p (minibuffer-window)))
             (unless kind (subtle-ui--show-popup session)))
            (t (subtle-popup-hide 'notice))))))

(defun subtle-ui--sync-popup-soon ()
  "Sync the popup once the current command has finished changing windows."
  (run-at-time 0 nil #'subtle-ui-sync-popup))

(defconst subtle-ui--window-hooks
  '((window-selection-change-functions . subtle-ui-sync-popup)
    (window-buffer-change-functions . subtle-ui-sync-popup)
    (minibuffer-setup-hook . subtle-ui-sync-popup)
    (minibuffer-exit-hook . subtle-ui--sync-popup-soon))
  "Global hooks that keep the popup in step with the selected window.")

(defun subtle-ui--install-window-hooks ()
  "Watch window changes while a session is open."
  (pcase-dolist (`(,hook . ,function) subtle-ui--window-hooks)
    (add-hook hook function)))

(defun subtle-ui--remove-window-hooks ()
  "Stop watching window changes."
  (pcase-dolist (`(,hook . ,function) subtle-ui--window-hooks)
    (remove-hook hook function)))

(defun subtle-help ()
  "Show this key list; q closes it."
  (interactive)
  (subtle-ui-session)
  (subtle-popup-show (subtle-ui--help-rows subtle-controls) 'help))

;;; Follow state

(defun subtle-ui--cancel-timer (session)
  "Cancel the idle timer of SESSION."
  (when (timerp (subtle-session-timer session))
    (cancel-timer (subtle-session-timer session)))
  (setf (subtle-session-timer session) nil))

(defun subtle-ui--restart-idle-timer (session)
  "Debounce: start the countdown to resuming autoscroll."
  (subtle-ui--cancel-timer session)
  (when (subtle-session-auto-resume session)
    (setf (subtle-session-timer session)
          (run-at-time subtle-resume-delay nil #'subtle-ui--idle-elapsed session))))

(defun subtle-ui--idle-elapsed (session)
  "Resume autoscroll for SESSION if it is still the active one."
  (when (eq session subtle--session)
    (if (subtle-ui--hold-p)
        (subtle-ui--restart-idle-timer session)
      (subtle-ui-resume session))))

(defun subtle-ui--user-scrolled (session)
  "The user took over scrolling in SESSION."
  (setf (subtle-session-follow session)
        (subtle-follow-next (subtle-session-follow session) 'user-scroll))
  (subtle-ui--restart-idle-timer session)
  (subtle-ui-sync-popup))

(defun subtle-ui--scrolled-p ()
  "Non-nil if the selected window really moved away from where subtle put it."
  (or (subtle-ui--drifted-p (selected-window) (window-start))
      (/= 0 (window-vscroll nil t))))

(defun subtle-ui--watch-scroll ()
  "Buffer-local `post-command-hook': detect the user scrolling."
  (when (and subtle--session (memq this-command subtle-scroll-commands)
             (subtle-ui--scrolled-p))
    (subtle-ui--user-scrolled subtle--session)))

(defun subtle-ui--on-window-scroll (window start)
  "Buffer-local `window-scroll-functions': catch scrolling by any command."
  (when subtle--session
    (cond ((subtle-ui--resized-p window)
           (subtle-ui--window-resized subtle--session window))
          ((and (subtle-ui--user-command-p) (subtle-ui--drifted-p window start))
           (subtle-ui--user-scrolled subtle--session)))))

(defun subtle-ui-resume (session)
  "Resume autoscroll in SESSION and jump to the current cues."
  (subtle-ui--cancel-timer session)
  (setf (subtle-session-follow session)
        (subtle-follow-next (subtle-session-follow session) 'resume))
  (subtle-popup-hide 'notice)
  (subtle-ui--forget-expected-starts session)
  (subtle-ui--reveal-all session))

(defun subtle-resume-follow ()
  "Resume autoscrolling now."
  (interactive)
  (subtle-ui-resume (subtle-ui-session)))

(defun subtle-toggle-auto-resume ()
  "Toggle automatic autoscroll resume."
  (interactive)
  (let ((session (subtle-ui-session)))
    (setf (subtle-session-auto-resume session)
          (not (subtle-session-auto-resume session)))
    (unless (subtle-session-follow session)
      (subtle-ui--restart-idle-timer session)
      (subtle-ui-sync-popup))))

;;; Rebuilding after an option changed

(defconst subtle-ui--space-re "[ \t\n]+")

(defun subtle-ui--space-runs (string)
  "Whitespace runs of STRING as (BEG . END) pairs, with empty ones at the ends.
There is exactly one more run than there are words, so two texts with the
same words have runs that pair up one to one."
  (let ((pos 0) (runs nil))
    (when (string-match (concat "\\`\\(?:" subtle-ui--space-re "\\)?") string)
      (push (cons 0 (match-end 0)) runs)
      (setq pos (match-end 0)))
    (while (string-match (concat "[^ \t\n]+\\(" subtle-ui--space-re "\\)?") string pos)
      (let ((end (match-end 0)))
        (push (if (match-beginning 1) (cons (match-beginning 1) end) (cons end end))
              runs)
        (setq pos end)))
    (nreverse runs)))

(defun subtle-ui--respace (string)
  "Make the buffer text equal STRING by replacing only whitespace.
Words, and anything attached to them such as overlays, stay untouched.
Return nil, having changed nothing, if the words differ."
  (let ((new (subtle-ui--space-runs string))
        (old (subtle-ui--space-runs (buffer-substring-no-properties (point-min) (point-max)))))
    (when (and (= (length new) (length old))
               (equal (split-string string subtle-ui--space-re t)
                      (split-string (buffer-substring-no-properties (point-min) (point-max))
                                    subtle-ui--space-re t)))
      (let ((offset (1- (point-min))))
        (cl-mapc
         (lambda (o n)
           (let ((new-run (substring-no-properties string (car n) (cdr n))))
             (unless (equal new-run (buffer-substring-no-properties
                                     (+ offset 1 (car o)) (+ offset 1 (cdr o))))
               ;; Insert before deleting: an overlay that starts at the end of
               ;; the old run must not swallow the new whitespace.
               (goto-char (+ offset 1 (car o)))
               (insert new-run)
               (delete-region (point) (+ (point) (- (cdr o) (car o)))))))
         (reverse old) (reverse new)))
      t)))

(defun subtle-ui--rebuild-text (track)
  "Bring the current buffer's text up to date with TRACK; return the spans.
Only whitespace is edited, so overlays that other packages put on the
words survive and nothing is re-analysed.  Falls back to rewriting it all."
  (let ((buffer-undo-list t)
        (inhibit-read-only t)
        (inhibit-modification-hooks t)
        spans string)
    (with-temp-buffer
      (setq spans (subtle-ui--insert-cues track)
            string (buffer-string)))
    (save-excursion
      (unless (subtle-ui--respace string)
        (erase-buffer)
        (insert string)))
    (remove-text-properties (point-min) (point-max) '(subtle-index nil))
    (seq-do-indexed (lambda (span i) (put-text-property (car span) (cdr span) 'subtle-index i))
                    spans)
    spans))

(defun subtle-ui--rebuild-buffer (buffer index)
  "Redraw BUFFER with the current options, highlighting cue INDEX."
  (with-current-buffer buffer
    (setq subtle-ui--spans (subtle-ui--rebuild-text subtle-ui--track))
    (unless (overlayp subtle-ui--overlay)
      (setq subtle-ui--overlay (subtle-ui--new-overlay)))
    (overlay-put subtle-ui--overlay 'face (if (subtle-layout-inline-p subtle-layout)
                                              'subtle-current-inline
                                            'subtle-current))
    (subtle-ui--apply-font)
    (goto-char (point-min))
    (subtle-ui--highlight index)))

(defun subtle-ui-rebuild (session)
  "Redraw every buffer of SESSION after a layout or font option changed."
  (subtle-ui--cancel-pause session)
  (setf (subtle-session-pause-points session)
        (and (eq subtle-layout 'paragraph-pause)
             (subtle-layout-pause-points subtle-paragraph-source
                                          (car (subtle-session-tracks session)))))
  (seq-mapn #'subtle-ui--rebuild-buffer
            (seq-filter #'buffer-live-p (subtle-session-buffers session))
            (subtle-session-indices session))
  (subtle-ui--forget-expected-starts session)
  (dolist (buffer (subtle-session-buffers session))
    (when-let* ((window (get-buffer-window buffer)))
      (subtle-ui--record-window window)))
  (subtle-ui--reveal-all session))

;;; Swapping sides

(defun subtle-swap-sides ()
  "Swap the left and right subtitle windows."
  (interactive)
  (let ((windows (mapcar #'get-buffer-window
                         (subtle-session-buffers (subtle-ui-session)))))
    (unless (and (= (length windows) 2) (seq-every-p #'identity windows))
      (user-error "Swapping needs two visible subtitle windows"))
    (apply #'window-swap-states windows)))

;;; Start and stop

(defvar subtle-warn-continuous-scroll)

(defun subtle-ui--warn-continuous ()
  "Warn that continuous scrolling and bltzread work badly together."
  (when (and (eq subtle-scroll-style 'continuous)
             (boundp 'subtle-warn-continuous-scroll) subtle-warn-continuous-scroll)
    (display-warning
     'subtle
     "`subtle-scroll-style' is `continuous' and bltzread is installed.
Continuous scrolling moves every word away as soon as the next cue starts,
so a word you are looking up is carried off the screen.  The default,
`paged', keeps the text still until the page turns.  If you want continuous
scrolling anyway, silence this warning with
  (setq subtle-warn-continuous-scroll nil)")))

(defun subtle-ui-start (tracks labels)
  "Create the session, buffers and layout for TRACKS named LABELS."
  (let ((session (make-subtle-session :tracks tracks
                                       :config (current-window-configuration)
                                       :indices (make-list (length tracks) nil))))
    (setf (subtle-session-buffers session)
          (seq-mapn #'subtle-ui--make-buffer tracks labels
                    (number-sequence 0 (1- (length tracks)))))
    (subtle-ui--layout (subtle-session-buffers session))
    (dolist (buffer (subtle-session-buffers session))
      (when-let* ((window (get-buffer-window buffer)))
        (subtle-ui--record-window window)))
    (when (eq subtle-layout 'paragraph-pause)
      (setf (subtle-session-pause-points session)
            (subtle-layout-pause-points subtle-paragraph-source (car tracks))))
    (subtle-ui--install-window-hooks)
    (subtle-ui--warn-continuous)
    session))

(defun subtle-ui-teardown (session)
  "Remove everything SESSION put on screen and restore the windows."
  (subtle-ui--cancel-timer session)
  (when (timerp (subtle-session-hold-timer session))
    (cancel-timer (subtle-session-hold-timer session)))
  (subtle-ui--cancel-pause session)
  (subtle-ui--remove-window-hooks)
  (subtle-popup-hide)
  (mapc #'kill-buffer (seq-filter #'buffer-live-p (subtle-session-buffers session)))
  (set-window-configuration (subtle-session-config session)))

(provide 'subtle-ui)
;;; subtle-ui.el ends here
