;;; subtle-custom.el --- Options, hooks and faces for subtle -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;; Author: smdi <you@example.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of subtle; see subtle.el for the license notice.

;;; Commentary:

;; Every user-facing knob of subtle lives here so the other files can
;; depend on it without depending on each other.  The options sit in
;; sub-groups of `subtle'.  The ones that matter to an open session
;; (layout, fill columns, text size, the notice) use a live setter:
;; `setopt', Customize and `subtle-set-option' all refresh the screen.

;;; Code:

(declare-function subtle-ui-rebuild "subtle-ui")
(declare-function subtle-ui-sync-popup "subtle-ui")

;;; Groups

(defgroup subtle nil
  "Play media through mpv with subtitles synced in Emacs buffers."
  :group 'multimedia
  :prefix "subtle-"
  :link '(info-link "(subtle)Top"))

(defgroup subtle-playback nil
  "How subtle starts mpv and steers playback."
  :group 'subtle
  :prefix "subtle-")

(defgroup subtle-scrolling nil
  "How the text follows the audio, and when it holds still."
  :group 'subtle
  :prefix "subtle-")

(defgroup subtle-text nil
  "Layout and look of the subtitle text."
  :group 'subtle
  :prefix "subtle-")

(defgroup subtle-point-track nil
  "Point tracking: point follows a chosen word of the spoken cue."
  :group 'subtle
  :prefix "subtle-point-track-")

(defgroup subtle-hooks nil
  "Hooks and function lists for extending subtle."
  :group 'subtle
  :prefix "subtle-")

(defgroup subtle-faces nil
  "Faces and boxes of subtle."
  :group 'subtle
  :prefix "subtle-")

;;; Live setters

(defun subtle--set-rebuild (option value)
  "Set OPTION to VALUE, and redraw the text of an open session."
  (set-default option value)
  (when (bound-and-true-p subtle--session)
    (subtle-ui-rebuild subtle--session)))

(defun subtle--set-notice (option value)
  "Set OPTION to VALUE, and refresh the notice of an open session."
  (set-default option value)
  (when (bound-and-true-p subtle--session)
    (subtle-ui-sync-popup)))

;;; Constants

(defconst subtle-layouts
  '((single-spaced :separator "\n")
    (double-spaced :separator "\n\n")
    (sentence :inline t)
    (paragraph-flow :inline t :paragraphs t)
    (paragraph-pause :inline t :paragraphs t :pause t))
  "Table of the values of `subtle-layout' and what each one is.
Each entry is (LAYOUT . PLIST).  `:inline' means cues share lines,
`:paragraphs' that paragraph breaks matter, `:pause' that playback
pauses at them, `:separator' is the text between two cues.")

;;; Playback

(defcustom subtle-mpv-executable "mpv"
  "Name or path of the mpv executable."
  :type 'string
  :safe #'stringp
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-mpv-base-args
  '("--no-terminal" "--sub-auto=no" "--sid=no" "--keep-open=yes"
    "--audio-display=no")
  "Command-line arguments subtle always gives mpv, before the others.
subtle adds the IPC socket, `--vid=no' for `subtle-video' and
`--focus-on=never' for `subtle-keep-focus' itself.  Arguments of
`subtle-mpv-extra-args' come after these; mpv lets the last one win."
  :type '(repeat string)
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-mpv-extra-args nil
  "Your own command-line arguments for mpv; they override the base ones.
For example (\"--volume=70\" \"--vf=hflip\")."
  :type '(repeat string)
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-mpv-connect-timeout 3.0
  "Seconds to wait for mpv to open its control socket before giving up."
  :type 'number
  :safe #'numberp
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-keep-focus t
  "Non-nil stops mpv from taking the keyboard focus when its window opens.
Passes `--focus-on=never', so Emacs keeps the keys (and bltzread its cards)."
  :type 'boolean
  :safe #'booleanp
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-video 'auto
  "Whether mpv may open a video window.
`auto' lets mpv decide; `never' plays audio only."
  :type '(choice (const auto) (const never))
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-seek-small 5
  "Seconds skipped by the small seek commands."
  :type 'number
  :safe #'numberp
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-seek-big 30
  "Seconds skipped by the big seek commands."
  :type 'number
  :safe #'numberp
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-speed-step 0.25
  "Amount added or removed by the speed commands."
  :type 'number
  :safe #'numberp
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-speed-range '(0.25 . 4.0)
  "Cons of the slowest and fastest allowed playback speed."
  :type '(cons (number :tag "Slowest") (number :tag "Fastest"))
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-previous-cue-restart 1.0
  "Seconds into a cue after which the previous-cue command restarts it.
Sooner than that it goes back one cue further."
  :type 'number
  :safe #'numberp
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-bookmark-rewind 3
  "Seconds before the saved time at which a bookmark reopens the media."
  :type 'number
  :safe #'numberp
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-bookmark-autoplay nil
  "Non-nil makes a bookmark start playing when it is opened.
Nil opens it paused, at the saved place; press the pause key to start."
  :type 'boolean
  :safe #'booleanp
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-hold-last-cue t
  "Non-nil keeps the last cue highlighted through gaps between cues."
  :type 'boolean
  :safe #'booleanp
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-confirm-quit t
  "Non-nil means the quit key asks y/n before ending the session."
  :type 'boolean
  :safe #'booleanp
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-media-extensions
  '("mp3" "m4a" "flac" "wav" "ogg" "opus" "aac" "wma"
    "mp4" "mkv" "webm" "mov" "avi" "m4v")
  "File extensions offered when picking a media file."
  :type '(repeat string)
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-subtitle-extensions '("srt" "vtt")
  "File extensions recognised as subtitles."
  :type '(repeat string)
  :group 'subtle-playback
  :package-version '(subtle . "0.1.0"))

;;; Scrolling

(defcustom subtle-scroll-style 'paged
  "How autoscroll moves the text.
`paged' leaves the text still until the spoken line nears the bottom of the
window, then turns the page (see `subtle-page-margin', `subtle-page-lead').
`continuous' keeps the spoken line at `subtle-focus-ratio' of the height at
every cue, which suits watching but is hard to study."
  :type '(choice (const paged) (const continuous))
  :group 'subtle-scrolling
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-focus-ratio (/ 2.0 3.0)
  "Fraction of the window height at which the active cue is kept.
Only the `continuous' scroll style uses it."
  :type 'float
  :safe #'numberp
  :group 'subtle-scrolling
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-page-margin 2
  "Lines that must stay free below the spoken line before the page turns."
  :type 'natnum
  :safe #'natnump
  :group 'subtle-scrolling
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-page-top-margin 0
  "Lines that must stay free above the spoken line before the page turns.
0 turns the page only when the line has scrolled off the top.  Values of
`subtle-page-lead' or more are cut down to one less, so a turned page does
not need turning again."
  :type 'natnum
  :safe #'natnump
  :group 'subtle-scrolling
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-page-lead 1
  "Lines shown above the spoken line after a page turn (1: it is the 2nd line)."
  :type 'natnum
  :safe #'natnump
  :group 'subtle-scrolling
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-hold-pause-lines nil
  "Pause playback while a held position is this many lines above the spoken one.
Nil never pauses.  A position is held when `subtle-anchor-functions' name
it.  Playback goes on by itself once it is released."
  :type '(choice (const :tag "Never" nil) (natnum :tag "Lines"))
  :group 'subtle-scrolling
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-move-point 'unless-selected
  "Whether autoscroll moves point to the start of the active cue.
`always' moves it every time.  `never' only scrolls, leaving the cursor where
you put it, which is what lets you study a word while the film plays.
`unless-selected' moves it in windows you are not working in, and in the
selected one only while the cursor is idle (see `subtle-move-idle-cursor')."
  :type '(choice (const always) (const unless-selected) (const never))
  :group 'subtle-scrolling
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-move-idle-cursor t
  "Non-nil means a cursor you have not moved keeps following the active cue.
It counts as not moved when it sits where subtle last put it, on the first
line of the window, or out of sight.  Only matters for `unless-selected'."
  :type 'boolean
  :safe #'booleanp
  :group 'subtle-scrolling
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-resume-delay 5
  "Seconds without scrolling before autoscroll resumes."
  :type 'number
  :safe #'numberp
  :set #'subtle--set-notice
  :group 'subtle-scrolling
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-auto-resume t
  "Non-nil means autoscroll resumes by itself after `subtle-resume-delay'."
  :type 'boolean
  :safe #'booleanp
  :set #'subtle--set-notice
  :group 'subtle-scrolling
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-scroll-commands
  '(mwheel-scroll pixel-scroll-precision
    scroll-up-command scroll-down-command
    scroll-other-window scroll-other-window-down
    pixel-scroll-up pixel-scroll-down recenter-top-bottom)
  "Commands that count as the user taking over scrolling.
Plain point motion (arrow keys) is not listed: it only takes over when it
actually pushes the window to scroll."
  :type '(repeat symbol)
  :group 'subtle-scrolling
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-popup-style 'box
  "How the \"autoscroll paused\" notice is shown.
`box' is a coloured box: a floating child frame in a graphical Emacs, a
bottom window in a terminal.  `echo-area' only messages; `none' is silent."
  :type '(choice (const box) (const echo-area) (const none))
  :set #'subtle--set-notice
  :group 'subtle-scrolling
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-warn-continuous-scroll t
  "Non-nil means warn about `continuous' scrolling with bltzread loaded.
Continuous scrolling moves words away while you look them up.  Set this to
nil once you have chosen continuous scrolling knowingly."
  :type 'boolean
  :safe #'booleanp
  :group 'subtle-scrolling
  :package-version '(subtle . "0.1.0"))

;;; Text

(defcustom subtle-layout 'single-spaced
  "How cues are arranged in a subtitle buffer.
`single-spaced': one cue per line.  `double-spaced': a blank line between
cues.  `sentence': a line break after each sentence.  `paragraph-flow':
running text, broken only at paragraph breaks (see
`subtle-paragraph-source').  `paragraph-pause': like `paragraph-flow', and
playback also pauses for `subtle-paragraph-pause' seconds at each break."
  :type `(choice ,@(mapcar (lambda (layout) `(const ,(car layout))) subtle-layouts))
  :set #'subtle--set-rebuild
  :group 'subtle-text
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-paragraph-source 'explicit-and-gap
  "What counts as a paragraph break in the paragraph layouts.
`explicit': only a blank line or empty cue inside the subtitle file.
`explicit-and-gap': also a silence of `subtle-paragraph-gap' seconds
between cues after a sentence end."
  :type '(choice (const explicit) (const explicit-and-gap))
  :set #'subtle--set-rebuild
  :group 'subtle-text
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-paragraph-gap 1.0
  "Seconds of silence between cues that start a new paragraph.
Smaller gives shorter paragraphs.  See also `subtle-paragraph-pause'."
  :type 'number
  :safe #'numberp
  :set #'subtle--set-rebuild
  :group 'subtle-text
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-paragraph-pause 1.75
  "Seconds playback pauses at a paragraph end in `paragraph-pause'.
This does not move the breaks; that is `subtle-paragraph-gap'."
  :type 'number
  :safe #'numberp
  :group 'subtle-text
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-cue-fill-column nil
  "Line length at which long cues are wrapped, like `fill-column'.
Nil never wraps.  Applies to the layouts that keep cues apart (not the
paragraph layouts, see `subtle-paragraph-fill-column').  The wrap is made
of real line breaks, so the cue keeps the same shape while it is
highlighted."
  :type '(choice (const :tag "Never" nil) (natnum :tag "Column"))
  :safe (lambda (value) (or (null value) (natnump value)))
  :set #'subtle--set-rebuild
  :group 'subtle-text
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-paragraph-fill-column nil
  "Line length paragraphs are filled to in the paragraph layouts.
Nil never fills.  Works like `fill-paragraph': the running text of a
paragraph is broken into lines of at most this many characters, so its
shape stays put."
  :type '(choice (const :tag "Never" nil) (natnum :tag "Column"))
  :safe (lambda (value) (or (null value) (natnump value)))
  :set #'subtle--set-rebuild
  :group 'subtle-text
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-sentence-terminators ".?!…。！？؟"
  "Characters that end a sentence, as one string.
Used by the `sentence' and paragraph layouts.  The default covers Latin,
Chinese, Japanese and Arabic punctuation."
  :type 'string
  :safe #'stringp
  :set #'subtle--set-rebuild
  :group 'subtle-text
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-sentence-closers "\\(?:[\"'”’)}»]\\|\\]\\)*"
  "Regexp for characters that may trail a sentence terminator.
A sentence break is placed after them."
  :type 'regexp
  :set #'subtle--set-rebuild
  :group 'subtle-text
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-text-scale 1.3
  "Size of subtitle text relative to the default face; 1.0 keeps it as is."
  :type 'number
  :safe #'numberp
  :set #'subtle--set-rebuild
  :group 'subtle-text
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-use-variable-pitch t
  "Non-nil shows subtitle text in your `variable-pitch' face."
  :type 'boolean
  :safe #'booleanp
  :set #'subtle--set-rebuild
  :group 'subtle-text
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-language-codes
  '(("sl" . "slv") ("hr" . "hrv") ("pl" . "pol") ("pi" . "pli") ("en" . "eng")
    ("de" . "deu") ("fr" . "fra") ("es" . "spa") ("it" . "ita") ("pt" . "por")
    ("ru" . "rus") ("sr" . "srp") ("cs" . "ces") ("sk" . "slk"))
  "Alist from the language part of a subtitle file name to the code you use.
`movie.sl.srt' gives `subtle-language' the value \"slv\" with the default.
A part that is not listed is used as it stands, if it is two or three letters."
  :type '(alist :key-type string :value-type string)
  :group 'subtle-text
  :package-version '(subtle . "0.1.0"))

;;; Point tracking

(defcustom subtle-point-track-initially t
  "Non-nil makes point jump to the chosen word of every new cue.
Moving point off the cue stops it until point is back on the cue, the playback
reaches the cue point is on, or you press the `subtle-point-track-toggle' key."
  :type 'boolean
  :safe #'booleanp
  :group 'subtle-point-track
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-point-track-nav-grace 0.25
  "Seconds before a cue change within which your moving point releases tracking.
Then the change leaves point where you put it."
  :type 'number
  :safe #'numberp
  :group 'subtle-point-track
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-point-track-indicator t
  "Non-nil shows a small box in the bottom right corner while point tracking is on."
  :type 'boolean
  :safe #'booleanp
  :group 'subtle-point-track
  :package-version '(subtle . "0.1.0"))

;;; Hooks

(defcustom subtle-open-hook nil
  "Hook run once a session is open: buffers shown, mpv started.
`subtle-current-session' is the new session."
  :type 'hook
  :group 'subtle-hooks
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-before-close-hook nil
  "Hook run when a session is closing, while its buffers and windows still exist.
Playback is paused first.  This is the place to ask questions about the session."
  :type 'hook
  :group 'subtle-hooks
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-close-hook nil
  "Hook run when a session closes, just before its buffers are killed."
  :type 'hook
  :options '(subtle-point-track--remove)
  :group 'subtle-hooks
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-rebuild-hook nil
  "Hook run after the text of the buffers was redrawn for a changed option."
  :type 'hook
  :group 'subtle-hooks
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-buffer-setup-hook nil
  "Hook run in each subtitle buffer once it is built, before playback starts.
`subtle-track-index' (0 for the first file) and `subtle-language' are set.
Use it to switch a minor mode on for the track you study and leave the
other one plain."
  :type 'hook
  :group 'subtle-hooks
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-pause-hook nil
  "Hook run, in a subtitle buffer, when you pause playback yourself.
It does not run for pauses subtle makes (paragraph ends, held words).
A good place to start a word hunt, such as `bltzread-avy-unknown'."
  :type 'hook
  :group 'subtle-hooks
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-before-cue-change-functions nil
  "Abnormal hook run before the spoken cue of a buffer changes.
Each function gets the BUFFER and the INDEX of the new cue (nil in a gap)."
  :type 'hook
  :options '(subtle-point-track--before-cue-change)
  :group 'subtle-hooks
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-cue-change-functions nil
  "Abnormal hook run after the spoken cue of a buffer changed.
Each function gets the BUFFER and the INDEX of the new cue (nil in a gap).
The cue is highlighted and the window has been scrolled by then."
  :type 'hook
  :options '(subtle-point-track--after-cue-change)
  :group 'subtle-hooks
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-follow-change-functions nil
  "Abnormal hook run when autoscroll stops or resumes.
Each function gets the SESSION and the new FOLLOW state (non-nil: the
text follows the audio)."
  :type 'hook
  :group 'subtle-hooks
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-mpv-message-functions nil
  "Abnormal hook run for every message from mpv, after subtle handled it.
Each function gets the SESSION and the MESSAGE: (time-pos . SECONDS),
\(pause . BOOL) or (speed . NUMBER).  Time messages arrive many times a
second, so keep the functions quick."
  :type 'hook
  :group 'subtle-hooks
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-hold-functions nil
  "Functions that hold autoscroll back while one returns non-nil.
They take no argument.  Use it for anything that needs the text to stay
put, like a dictionary card."
  :type 'hook
  :group 'subtle-hooks
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-anchor-functions nil
  "Functions returning a position to keep on screen, or nil.
They are called in a subtitle buffer.  When the page turns, the new page
starts at the first such position if the spoken line still fits below it;
otherwise `subtle-dismiss-functions' run."
  :type 'hook
  :group 'subtle-hooks
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-dismiss-functions nil
  "Functions that close something on screen, the first to return non-nil wins.
Run when subtle needs the text to move, and by `subtle-quit' before it asks
whether to close the session."
  :type 'hook
  :group 'subtle-hooks
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-point-track-moved-functions nil
  "Functions called after point tracking moved point, with the BUFFER and POS.
Meant for showing something about the word point is now on, such as a card.
They run once the cue change is over, not inside it."
  :type 'hook
  :group 'subtle-hooks
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-cue-point-functions nil
  "Functions choosing where point goes when a cue becomes the spoken one.
Each is called with the BEG and END of the cue in a subtitle buffer and
returns a position, or nil to pass.  The first answer wins.  Point tracking
\(`subtle-point-track-toggle') does nothing without one."
  :type 'hook
  :group 'subtle-hooks
  :package-version '(subtle . "0.1.0"))

;;; Keys

(defcustom subtle-control-strip
  '((subtle-toggle-pause . "pause")
    (subtle-seek-forward . "fwd")
    (subtle-seek-backward . "back")
    (subtle-help . "help")
    (subtle-quit . "quit"))
  "Commands shown in the control strip at the top, each with its label.
The key shown is whatever `subtle-mode-map' binds the command to.  Bind
keys with `keymap-set' on `subtle-mode-map'; this only chooses the hints."
  :type '(alist :key-type function :value-type string)
  :group 'subtle-faces
  :package-version '(subtle . "0.1.0"))

;;; Popup box

(defcustom subtle-popup-box-style 'unicode
  "How the borders of boxes are drawn.
`unicode' uses rounded line-drawing characters, `ascii' plain + - |
for fonts or terminals that lack them."
  :type '(choice (const unicode) (const ascii))
  :group 'subtle-faces
  :package-version '(subtle . "0.1.0"))

(defcustom subtle-popup-frame-parameters nil
  "Alist of frame parameters merged over those of subtle's child frames.
For example ((internal-border-width . 4)) adds padding.  Your entries
win over the defaults (see `subtle-popup--frame-parameters')."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'subtle-faces
  :package-version '(subtle . "0.1.0"))

;;; Faces

(defface subtle-current
  '((t :inherit highlight :extend t))
  "Face of the cue being spoken.
Keep it free of weight and size changes: they would rewrap the text."
  :group 'subtle-faces
  :package-version '(subtle . "0.1.0"))

(defface subtle-current-inline
  '((t :inherit highlight :extend nil))
  "Face of the spoken cue in layouts where cues share lines."
  :group 'subtle-faces
  :package-version '(subtle . "0.1.0"))

(defface subtle-key
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face of key names in the control strip and the hint."
  :group 'subtle-faces
  :package-version '(subtle . "0.1.0"))

(defface subtle-header-state
  '((t :inherit mode-line-emphasis))
  "Face of the playback state in the header line: [playing] 1x 00:12."
  :group 'subtle-faces
  :package-version '(subtle . "0.1.0"))

(defface subtle-header-label
  '((t :inherit shadow))
  "Face of the file name in the header line."
  :group 'subtle-faces
  :package-version '(subtle . "0.1.0"))

(defface subtle-point-track-indicator
  '((t :inherit bold))
  "Face of the text in the corner box shown while point tracking is on."
  :group 'subtle-faces
  :package-version '(subtle . "0.1.0"))

(defface subtle-popup
  '((((background dark)) :background "#2b2140" :foreground "#f3e9ff")
    (t :background "#fff1c9" :foreground "#3b2600"))
  "Body of the autoscroll notice box."
  :group 'subtle-faces
  :package-version '(subtle . "0.1.0"))

(defface subtle-popup-border
  '((((background dark)) :background "#2b2140" :foreground "#ffb454" :weight bold)
    (t :background "#fff1c9" :foreground "#d9480f" :weight bold))
  "Border of the autoscroll notice box."
  :group 'subtle-faces
  :package-version '(subtle . "0.1.0"))

(defface subtle-popup-title
  '((((background dark)) :background "#ffb454" :foreground "#1a1026" :weight bold)
    (t :background "#d9480f" :foreground "#ffffff" :weight bold))
  "Title of the autoscroll notice box."
  :group 'subtle-faces
  :package-version '(subtle . "0.1.0"))

(defface subtle-popup-key
  '((((background dark)) :background "#7dcfff" :foreground "#10202b" :weight bold)
    (t :background "#1d4ed8" :foreground "#ffffff" :weight bold))
  "Key caps in the autoscroll notice box."
  :group 'subtle-faces
  :package-version '(subtle . "0.1.0"))

(provide 'subtle-custom)
;;; subtle-custom.el ends here
