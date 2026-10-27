;;; subtle-ui.el --- Buffers, highlight, playback events and notice -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <you@example.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of subtle; see subtle.el for the license notice.

;;; Commentary:

;; Everything the user sees apart from scrolling (subtle-scrolling.el)
;; and the header line (subtle-header.el).  Text builders are pure; the
;; rest moves overlays.  The single `subtle-session' holds the state.
;; Other packages hook in through `subtle-before-cue-change-functions',
;; `subtle-cue-change-functions' and `subtle-follow-change-functions'
;; rather than being called from here.

;;; Code:

(require 'cl-lib)
(require 'face-remap)
(require 'seq)
(require 'subr-x)
(require 'subtle-custom)
(require 'subtle-session)
(require 'subtle-track)
(require 'subtle-scroll)
(require 'subtle-scrolling)
(require 'subtle-popup)
(require 'subtle-keys)
(require 'subtle-header)
(require 'subtle-layout)

(declare-function subtle-bookmark-make-record "subtle")

(defvar-local subtle-ui--overlay nil "Overlay marking the current cue.")
(defvar-local subtle-ui--font-cookies nil
  "Face remappings made by `subtle-ui--apply-font'.")

;;; Buffers

(defun subtle-ui--insert-cue (cue index next)
  "Insert CUE tagged with INDEX, then what separates it from NEXT.
Return the (BEG . END) of the cue text."
  (let ((beg (point)))
    (insert (propertize (subtle-layout-text subtle-layout cue)
                        'subtle-index index))
    (prog1 (cons beg (point))
      (when next
        (insert (subtle-layout-separator subtle-layout subtle-paragraph-source
                                         cue next))))))

(defun subtle-ui--insert-cues (track)
  "Insert every cue of TRACK; return the vector of spans."
  (let* ((inhibit-read-only t)
         (buffer-undo-list t)
         (cues (subtle-track-cues track))
         (origin (point))
         (spans (vconcat (seq-map-indexed
                          (lambda (cue i)
                            (subtle-ui--insert-cue
                             cue i (and (< (1+ i) (length cues))
                                        (aref cues (1+ i)))))
                          cues))))
    (when (subtle-layout-paragraphs-p subtle-layout)
      (subtle-ui--fill-paragraphs origin (point)))
    spans))

(defun subtle-ui--fill-paragraphs (beg end)
  "Break the lines of the paragraphs between BEG and END at the fill column.
Only spaces become newlines, so text properties and positions are kept."
  (when (natnump subtle-paragraph-fill-column)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let* ((from (point))
               (to (if (re-search-forward "\n\n+" end t)
                       (match-beginning 0)
                     (goto-char end)
                     end))
               (old (buffer-substring-no-properties from to))
               (new (subtle-layout-fill old subtle-paragraph-fill-column)))
          (dotimes (i (length old))
            (unless (eq (aref old i) (aref new i))
              (subst-char-in-region (+ from i) (+ from i 1) ?\s ?\n)))
          (goto-char (max (point) to)))))))

(defun subtle-ui--current-face ()
  "Face of the spoken cue under the current `subtle-layout'."
  (if (subtle-layout-inline-p subtle-layout)
      'subtle-current-inline
    'subtle-current))

(defun subtle-ui--new-overlay ()
  "Detached overlay carrying the face of the spoken cue."
  (let ((overlay (make-overlay (point-min) (point-min))))
    (overlay-put overlay 'face (subtle-ui--current-face))
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
  "Read-only view of one subtitle track kept in sync with mpv.
\\{subtle-mode-map}"
  (setq-local header-line-format subtle-header-format)
  (setq-local truncate-lines nil)
  (subtle-ui--apply-font)
  (visual-line-mode 1)
  (add-hook 'post-command-hook #'subtle-scrolling-watch nil t)
  (add-hook 'window-scroll-functions #'subtle-scrolling-on-window-scroll nil t))

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
    (setq subtle--track track
          subtle--label label
          subtle--spans (subtle-ui--insert-cues track)
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

;;; Highlight

(defun subtle-ui--highlight (index)
  "Move the overlay of the current buffer onto cue INDEX, or hide it."
  (if-let* ((span (subtle-cue-span (current-buffer) index)))
      (move-overlay subtle-ui--overlay (car span) (cdr span) (current-buffer))
    (delete-overlay subtle-ui--overlay)))

(defun subtle-ui--focus-cue (session buffer index)
  "Highlight cue INDEX in BUFFER and, while following, scroll to it.
Runs `subtle-before-cue-change-functions' first and
`subtle-cue-change-functions' last, both with BUFFER and INDEX.
SESSION says whether to follow."
  (when (buffer-live-p buffer)
    (run-hook-with-args 'subtle-before-cue-change-functions buffer index)
    (with-current-buffer buffer
      (subtle-ui--highlight index))
    (when (subtle-session-follow session)
      (subtle-scrolling-follow-index session buffer index))
    (run-hook-with-args 'subtle-cue-change-functions buffer index)))

;;; Playback events

(defun subtle-ui--indices-at (session time)
  "Current cue index of every track of SESSION at TIME."
  (mapcar (lambda (track) (subtle-track-cue-index track time subtle-hold-last-cue))
          (subtle-session-tracks session)))

(defun subtle-ui--refresh-header (session time)
  "Redraw the header lines of SESSION when the displayed second of TIME changed."
  (let ((second (floor time)))
    (unless (eql second (subtle-session-shown-second session))
      (setf (subtle-session-shown-second session) second)
      (force-mode-line-update t))))

(defun subtle-ui--set-mpv-pause (session paused)
  "Tell mpv to pause (PAUSED non-nil) or resume playback of SESSION."
  (when-let* ((send (subtle-session-send session)))
    (funcall send "set_property" "pause" (or paused :false))))

(defun subtle-ui--cancel-pause (session)
  "Forget a pending paragraph pause of SESSION."
  (when (timerp (subtle-session-pause-timer session))
    (cancel-timer (subtle-session-pause-timer session)))
  (setf (subtle-session-pause-timer session) nil
        (subtle-session-pausing session) nil))

(defun subtle-ui--pause-elapsed (session)
  "The paragraph pause of SESSION is over: let playback go on."
  (setf (subtle-session-pause-timer session) nil)
  (when (eq session (subtle-current-session))
    (subtle-ui--set-mpv-pause session nil)))

(defun subtle-ui--pause-due-p (session from to)
  "Non-nil if SESSION, playing from FROM to TO, passed a paragraph end."
  (and (subtle-session-pause-points session)
       (subtle-session-send session)
       (not (subtle-session-paused session))
       (not (subtle-session-pause-timer session))
       (subtle-layout-pause-crossed (subtle-session-pause-points session)
                                    from to)))

(defun subtle-ui--paragraph-pause (session from to)
  "Pause SESSION's playback if it just played past a paragraph end (FROM to TO)."
  (when (subtle-ui--pause-due-p session from to)
    (setf (subtle-session-pausing session) t
          (subtle-session-pause-timer session)
          (run-at-time subtle-paragraph-pause nil
                       #'subtle-ui--pause-elapsed session))
    (subtle-ui--set-mpv-pause session t)))

(defun subtle-ui--held-far-p (session)
  "Non-nil if a held position is far above the spoken line of SESSION.
Only the buffer in the selected window counts, and only when
`subtle-hold-pause-lines' says what far is."
  (let* ((buffer (window-buffer (selected-window)))
         (index (subtle-session-index-of session buffer))
         (span (subtle-cue-span buffer index)))
    (when span
      (with-current-buffer buffer
        (when-let* ((anchor (run-hook-with-args-until-success
                             'subtle-anchor-functions))
                    ((< anchor (car span))))
          (subtle-scroll-far-p (count-lines anchor (car span))
                               subtle-hold-pause-lines))))))

(defun subtle-ui--hold-pause (session)
  "Pause playback of SESSION while a held word is far above the spoken line.
Only if `subtle-hold-pause-lines' says so; playback resumes when it is released."
  (when (and subtle-hold-pause-lines (subtle-session-send session))
    (pcase-let ((`(,state . ,action)
                 (subtle-scroll-hold-pause-step
                  (subtle-ui--held-far-p session)
                  (subtle-session-hold-pause session)
                  (subtle-session-paused session))))
      (setf (subtle-session-hold-pause session) state)
      (pcase action
        ('pause (subtle-ui--set-mpv-pause session t))
        ('resume (subtle-ui--set-mpv-pause session nil))))))

(defun subtle-ui--credited-end (track spans index time)
  "Buffer position up to which cue INDEX of TRACK has been played at TIME.
SPANS are the buffer spans of the cues."
  (cond ((>= time (subtle-cue-end (aref (subtle-track-cues track) index)))
         (cdr (aref spans index)))
        ((> index 0) (cdr (aref spans (1- index))))
        (t (car (aref spans 0)))))

(defun subtle-ui--credit-buffer (buffer index time from playing)
  "Update the played run of BUFFER, at cue INDEX, now TIME after FROM.
PLAYING says that the move from FROM to TIME was unbroken playback."
  (with-current-buffer buffer
    (let* ((start (car (aref subtle--spans index)))
           (end (subtle-ui--credited-end subtle--track subtle--spans index time)))
      (cond ((and subtle--run playing)
             (setcdr subtle--run (max (cdr subtle--run) end)))
            ((and subtle--run (= time from)))
            (t (setq subtle--run (cons start (max start end))))))))

(defun subtle-ui--credit-playback (session from time indices)
  "Extend each buffer's played run when SESSION moved from FROM to TIME unbroken.
INDICES are the current cue indices.  A seek, or going backwards, starts a
new run at the cue now playing.  Pausing does not break a run, but does
not extend it either."
  (let ((playing (and (not (subtle-session-paused session))
                      (> time from)
                      (<= (- time from) subtle-track-seek-threshold))))
    (seq-mapn (lambda (buffer index)
                (when (and (buffer-live-p buffer) index)
                  (subtle-ui--credit-buffer buffer index time from playing)))
              (subtle-session-buffers session) indices)))

(defun subtle-ui-tick (session time)
  "Move SESSION to playback position TIME."
  (subtle-ui--hold-pause session)
  (let ((indices (subtle-ui--indices-at session time))
        (from (subtle-session-position session)))
    (subtle-ui--credit-playback session from time indices)
    (subtle-ui--paragraph-pause session from time)
    (setf (subtle-session-position session) time)
    (unless (equal indices (subtle-session-indices session))
      (setf (subtle-session-indices session) indices)
      (seq-mapn (lambda (buffer index) (subtle-ui--focus-cue session buffer index))
                (subtle-session-buffers session) indices))
    (subtle-ui--refresh-header session time)))

(defun subtle-ui--run-pause-hook (session)
  "Run `subtle-pause-hook' in the buffer of SESSION shown in the selected window."
  (let ((buffer (seq-find (lambda (b) (eq b (window-buffer)))
                          (subtle-session-buffers session))))
    (with-current-buffer (or buffer (car (subtle-session-buffers session)))
      (run-hooks 'subtle-pause-hook))))

(defun subtle-ui--user-paused-p (session paused)
  "Non-nil if SESSION just got PAUSED by the user, not by subtle."
  (and paused
       (not (subtle-session-paused session))
       (not (eq (subtle-session-hold-pause session) 'holding))))

(defun subtle-ui-set-paused (session paused)
  "Record that SESSION is PAUSED or playing."
  (cond ((and paused (subtle-session-pausing session))
         (setf (subtle-session-pausing session) nil))
        (t ;; The user took over: their choice beats the paragraph pause.
         (subtle-ui--cancel-pause session)
         (when (subtle-ui--user-paused-p session paused)
           (subtle-ui--run-pause-hook session))))
  (when (and (not paused) (eq (subtle-session-hold-pause session) 'holding))
    ;; resumed by hand while a word is held: leave it going
    (setf (subtle-session-hold-pause session) 'overridden))
  (setf (subtle-session-paused session) paused)
  (force-mode-line-update t)
  (subtle-ui-sync-popup))

(defun subtle-ui-set-speed (session speed)
  "Record the playback SPEED of SESSION."
  (setf (subtle-session-speed session) speed)
  (force-mode-line-update t))

;;; Notice and help box

(defun subtle-ui--popup-wanted-p (follow paused in-subtle minibuffer)
  "Non-nil if the \"autoscroll paused\" notice should be on screen.
It is only useful while audio plays (PAUSED nil), autoscroll is off
\(FOLLOW nil), and the user looks at a subtle buffer (IN-SUBTLE, not
MINIBUFFER)."
  (and (not follow) (not paused) in-subtle (not minibuffer)))

(defun subtle-ui--in-subtle-p (session)
  "Non-nil if the selected window displays a buffer of SESSION."
  (and (memq (window-buffer (selected-window)) (subtle-session-buffers session))
       t))

(defun subtle-ui--show-popup (session)
  "Tell the user how to resume autoscroll in SESSION."
  (subtle-popup-show
   (subtle-keys-hint-rows (subtle-session-auto-resume session)
                          subtle-resume-delay)))

(defun subtle-ui-sync-popup (&rest _)
  "Show or hide the boxes so they match what the user is looking at."
  (when-let* ((session (subtle-current-session)))
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
  "Watch the windows while a session is open."
  (pcase-dolist (`(,hook . ,function) subtle-ui--window-hooks)
    (add-hook hook function)))

(defun subtle-ui--remove-window-hooks ()
  "Stop watching the windows."
  (pcase-dolist (`(,hook . ,function) subtle-ui--window-hooks)
    (remove-hook hook function)))

(add-hook 'subtle-follow-change-functions #'subtle-ui-sync-popup)

;;;###autoload
(defun subtle-help ()
  "Show the key list; \\<subtle-mode-map>\\[subtle-help] or \\[subtle-quit] closes it."
  (interactive)
  (subtle-require-session)
  (if (eq (subtle-popup-kind) 'help)
      (subtle-popup-hide 'help)
    (subtle-popup-show (subtle-keys-help-rows) 'help)))

;;; Rebuilding after an option changed

(defconst subtle-ui--space-re "[ \t\n]+")

(defun subtle-ui--space-runs (string)
  "Whitespace stretches of STRING as (BEG . END) pairs, plus empty end ones.
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
    (setq subtle--spans (subtle-ui--rebuild-text subtle--track))
    (unless (overlayp subtle-ui--overlay)
      (setq subtle-ui--overlay (subtle-ui--new-overlay)))
    (overlay-put subtle-ui--overlay 'face (subtle-ui--current-face))
    (subtle-ui--apply-font)
    (goto-char (point-min))
    (subtle-ui--highlight index)))

(defun subtle-ui--pause-points (session)
  "Paragraph ends of SESSION's first track, or nil if the layout does not pause."
  (and (subtle-layout-pause-p subtle-layout)
       (subtle-layout-pause-points subtle-paragraph-source
                                   (car (subtle-session-tracks session)))))

(defun subtle-ui-rebuild (session)
  "Redraw every buffer of SESSION after a layout or font option changed.
Runs `subtle-rebuild-hook' at the end."
  (subtle-ui--cancel-pause session)
  (setf (subtle-session-pause-points session) (subtle-ui--pause-points session))
  (pcase-dolist (`(,buffer . ,index) (subtle-session-pairs session))
    (subtle-ui--rebuild-buffer buffer index))
  (subtle-scrolling--forget-expected-starts session)
  (subtle-scrolling--record-all session)
  (subtle-scrolling--reveal-all session)
  (run-hooks 'subtle-rebuild-hook))

;;; Swapping sides

;;;###autoload
(defun subtle-swap-sides ()
  "Swap the left and right subtitle windows."
  (interactive)
  (let ((windows (mapcar #'get-buffer-window
                         (subtle-session-buffers (subtle-require-session)))))
    (unless (and (= (length windows) 2) (seq-every-p #'identity windows))
      (user-error "Swapping needs two visible subtitle windows"))
    (apply #'window-swap-states windows)))

;;; Start and stop

(defun subtle-ui--warn-continuous ()
  "Warn that continuous scrolling and bltzread work badly together."
  (when (and (eq subtle-scroll-style 'continuous)
             subtle-warn-continuous-scroll
             (featurep 'bltzread))
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
  (let ((session (make-subtle-session
                  :tracks tracks
                  :config (current-window-configuration)
                  :indices (make-list (length tracks) nil))))
    (setf (subtle-session-buffers session)
          (seq-mapn #'subtle-ui--make-buffer tracks labels
                    (number-sequence 0 (1- (length tracks)))))
    (subtle-ui--layout (subtle-session-buffers session))
    (subtle-scrolling--record-all session)
    (setf (subtle-session-pause-points session) (subtle-ui--pause-points session))
    (subtle-ui--install-window-hooks)
    (subtle-ui--warn-continuous)
    session))

(defun subtle-ui-teardown (session)
  "Remove everything SESSION put on screen and restore the windows."
  (subtle-scrolling-stop session)
  (subtle-ui--cancel-pause session)
  (subtle-ui--remove-window-hooks)
  (subtle-popup-hide)
  (mapc #'kill-buffer (seq-filter #'buffer-live-p (subtle-session-buffers session)))
  (set-window-configuration (subtle-session-config session)))

(provide 'subtle-ui)
;;; subtle-ui.el ends here
