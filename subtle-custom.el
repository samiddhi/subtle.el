;;; subtle-custom.el --- Options and faces for subtle -*- lexical-binding: t; -*-

;; Copyright (C) 2026 smdi

;;; Commentary:

;; Every user-facing knob of subtle lives here so the other files can
;; depend on it without depending on each other.

;;; Code:

(defgroup subtle nil
  "Play media through mpv with subtitles synced in Emacs buffers."
  :group 'multimedia
  :prefix "subtle-")

(defcustom subtle-mpv-executable "mpv"
  "Name or path of the mpv executable."
  :type 'string)

(defcustom subtle-mpv-extra-args nil
  "Extra command-line arguments passed to mpv."
  :type '(repeat string))

(defcustom subtle-keep-focus t
  "Non-nil stops mpv from taking the keyboard focus when its window opens.
Passes `--focus-on=never', so Emacs keeps the keys (and bltzread its cards)."
  :type 'boolean)

(defcustom subtle-video 'auto
  "Whether mpv may open a video window.
`auto' lets mpv decide; `never' plays audio only."
  :type '(choice (const auto) (const never)))

(defcustom subtle-focus-ratio (/ 2.0 3.0)
  "Fraction of the window height at which the active cue is kept."
  :type 'float)

(defcustom subtle-scroll-style 'paged
  "How autoscroll moves the text.
`paged' leaves the text still until the spoken line nears the bottom of the
window, then turns the page (see `subtle-page-margin', `subtle-page-lead').
`continuous' keeps the spoken line at `subtle-focus-ratio' of the height at
every cue, which suits watching but is hard to study."
  :type '(choice (const paged) (const continuous)))

(defcustom subtle-page-margin 2
  "Lines that must stay free below the spoken line before the page turns."
  :type 'integer)

(defcustom subtle-page-lead 1
  "Lines shown above the spoken line after a page turn (1: it is the 2nd line)."
  :type 'integer)

(defcustom subtle-hold-pause-lines nil
  "Pause playback while a word held open is this many lines above the spoken one.
Nil never pauses.  A word is held open when `subtle-anchor-functions' name it.
Playback goes on by itself once the word is released."
  :type '(choice (const :tag "Never" nil) integer))

(defcustom subtle-hold-functions nil
  "Functions that hold autoscroll back while one returns non-nil.
They take no argument.  Use it for anything that needs the text to stay
put, like a dictionary card."
  :type 'hook)

(defcustom subtle-anchor-functions nil
  "Functions returning a position to keep on screen, or nil.
They are called in a subtitle buffer.  When the page turns, the new page
starts at the first such position if the spoken line still fits below it;
otherwise `subtle-dismiss-functions' run."
  :type 'hook)

(defcustom subtle-dismiss-functions nil
  "Functions that close something on screen, the first to return non-nil wins.
Run when subtle needs the text to move, and by `subtle-quit' before it asks
whether to close the session."
  :type 'hook)

(when (locate-library "bltzread")
  (defcustom subtle-warn-continuous-scroll t
    "Non-nil means warn about `continuous' scrolling with bltzread installed.
Continuous scrolling moves words away while you look them up.  Set this to
nil once you have chosen continuous scrolling knowingly."
    :type 'boolean))

(defcustom subtle-before-close-hook nil
  "Hook run when a session is closing, while its buffers and windows still exist.
Playback is paused first.  This is the place to ask questions about the session."
  :type 'hook)

(defcustom subtle-close-hook nil
  "Hook run when a session closes, just before its buffers are killed."
  :type 'hook)

(defcustom subtle-move-point 'unless-selected
  "Whether autoscroll moves point to the start of the active cue.
`always' moves it every time.  `never' only scrolls, leaving the cursor where
you put it, which is what lets you study a word while the film plays.
`unless-selected' moves it in windows you are not working in, and in the
selected one only while the cursor is idle (see `subtle-move-idle-cursor')."
  :type '(choice (const always) (const unless-selected) (const never)))

(defcustom subtle-move-idle-cursor t
  "Non-nil means a cursor you have not moved keeps following the active cue.
It counts as not moved when it sits where subtle last put it, on the first
line of the window, or out of sight.  Only matters for `unless-selected'."
  :type 'boolean)

(defcustom subtle-buffer-setup-hook nil
  "Hook run in each subtitle buffer once it is built, before playback starts.
`subtle-track-index' (0 for the first file) and `subtle-language' are set.
Use it to switch a minor mode on for the track you study and leave the
other one plain."
  :type 'hook)

(defcustom subtle-language-codes
  '(("sl" . "slv") ("hr" . "hrv") ("pl" . "pol") ("pi" . "pli") ("en" . "eng")
    ("de" . "deu") ("fr" . "fra") ("es" . "spa") ("it" . "ita") ("pt" . "por")
    ("ru" . "rus") ("sr" . "srp") ("cs" . "ces") ("sk" . "slk"))
  "Alist from the language part of a subtitle file name to the code you use.
`movie.sl.srt' gives `subtle-language' the value \"slv\" with the default.
A part that is not listed is used as it stands, if it is two or three letters."
  :type '(alist :key-type string :value-type string))

(defcustom subtle-pause-hook nil
  "Hook run, in a subtitle buffer, when you pause playback yourself.
It does not run for pauses subtle makes (paragraph ends, held words).
A good place to start a word hunt, such as `bltzread-avy-unknown'."
  :type 'hook)

(defcustom subtle-resume-delay 5
  "Seconds without scrolling before autoscroll resumes."
  :type 'number)

(defcustom subtle-auto-resume t
  "Non-nil means autoscroll resumes by itself after `subtle-resume-delay'."
  :type 'boolean)

(defcustom subtle-seek-small 5
  "Seconds skipped by the small seek commands."
  :type 'number)

(defcustom subtle-seek-big 30
  "Seconds skipped by the big seek commands."
  :type 'number)

(defcustom subtle-speed-step 0.25
  "Amount added or removed by the speed commands."
  :type 'number)

(defcustom subtle-speed-range '(0.25 . 4.0)
  "Cons of the slowest and fastest allowed playback speed."
  :type '(cons number number))

(defcustom subtle-hold-last-cue t
  "Non-nil keeps the last cue highlighted through gaps between cues."
  :type 'boolean)

(defcustom subtle-popup-style 'box
  "How the \"autoscroll paused\" notice is shown.
`box' is a coloured box: a floating child frame in a graphical Emacs, a
bottom window in a terminal.  `echo-area' only messages; `none' is silent."
  :type '(choice (const box) (const echo-area) (const none)))

(defcustom subtle-text-scale 1.3
  "Size of subtitle text relative to the default face; 1.0 keeps it as is."
  :type 'number)

(defcustom subtle-use-variable-pitch t
  "Non-nil shows subtitle text in your `variable-pitch' face."
  :type 'boolean)

(defcustom subtle-media-extensions
  '("mp3" "m4a" "flac" "wav" "ogg" "opus" "aac" "wma"
    "mp4" "mkv" "webm" "mov" "avi" "m4v")
  "File extensions offered when picking a media file."
  :type '(repeat string))

(defcustom subtle-subtitle-extensions '("srt" "vtt")
  "File extensions recognised as subtitles."
  :type '(repeat string))

(defcustom subtle-scroll-commands
  '(mwheel-scroll pixel-scroll-precision
    scroll-up-command scroll-down-command
    scroll-other-window scroll-other-window-down
    pixel-scroll-up pixel-scroll-down recenter-top-bottom)
  "Commands that count as the user taking over scrolling.
Plain point motion (arrow keys) is not listed: it only takes over when it
actually pushes the window to scroll."
  :type '(repeat symbol))

(defcustom subtle-layout 'single-spaced
  "How cues are arranged in a subtitle buffer.
`single-spaced': one cue per line.  `double-spaced': a blank line between
cues.  `sentence': a line break after each sentence.  `paragraph-flow':
running text, broken only at paragraph breaks (see
`subtle-paragraph-source').  `paragraph-pause': like `paragraph-flow', and
playback also pauses for `subtle-paragraph-pause' seconds at each break."
  :type '(choice (const single-spaced) (const double-spaced) (const sentence)
                 (const paragraph-flow) (const paragraph-pause)))

(defcustom subtle-paragraph-source 'explicit-and-gap
  "What counts as a paragraph break in the paragraph layouts.
`explicit': only a blank line or empty cue inside the subtitle file.
`explicit-and-gap': also a silence of `subtle-paragraph-gap' seconds
between cues after a sentence end."
  :type '(choice (const explicit) (const explicit-and-gap)))

(defcustom subtle-paragraph-gap 1.0
  "Seconds of silence between cues that start a new paragraph.
Smaller gives shorter paragraphs.  See also `subtle-paragraph-pause'."
  :type 'number)

(defcustom subtle-paragraph-pause 1.75
  "Seconds playback pauses at a paragraph end in `paragraph-pause'.
This does not move the breaks; that is `subtle-paragraph-gap'."
  :type 'number)

(defcustom subtle-sentence-closers "\\(?:[\"'”’)}»]\\|\\]\\)*"
  "Regexp for characters that may trail a sentence terminator (.?! or …).
A sentence break is placed after them."
  :type 'regexp)

(defcustom subtle-controls
  '(("p"   subtle-toggle-pause           "pause")
    ("f"   subtle-seek-forward           "fwd")
    ("b"   subtle-seek-backward          "back")
    ("F"   subtle-seek-forward-big       nil)
    ("B"   subtle-seek-backward-big      nil)
    ("]"   subtle-faster                 nil)
    ("["   subtle-slower                 nil)
    ("="   subtle-reset-speed            nil)
    (")"   subtle-next-cue               nil)
    ("("   subtle-previous-cue           nil)
    ("RET" subtle-seek-to-cue-at-point   nil)
    ("r"   subtle-resume-follow          nil)
    ("s t" subtle-toggle-auto-resume     nil)
    ("s s" subtle-swap-sides             nil)
    ("s v" subtle-set-layout             nil)
    ("s o" subtle-set-option             nil)
    ("h"   subtle-help                   "help")
    ("?"   subtle-help                   nil)
    ("q"   subtle-quit                   "quit"))
  "Key table: (KEY COMMAND HINT).
KEY is a `kbd' string.  HINT, when a string, puts the entry in the control
strip at the top; leave it nil to keep the strip short.  The help box
lists every entry, using the first line of each command's docstring.
The defaults are single keys, with `s' as the one prefix for the rarer
commands, and they do not clash with bltzread's keys: `{' `}' step through
unknown words and `(' `)' through cues.  Run `subtle-reload-controls' after
changing this."
  :type '(repeat (list string symbol (choice (const nil) string))))

(defcustom subtle-confirm-quit t
  "Non-nil means the quit key asks y/n before ending the session."
  :type 'boolean)

(defface subtle-current
  '((t :inherit highlight :weight bold :extend t))
  "Face of the cue being spoken.")

(defface subtle-current-inline
  '((t :inherit highlight :weight bold :extend nil))
  "Face of the spoken cue in layouts where cues share lines.")

(defface subtle-key
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face of key names in the control strip and the hint.")

(defface subtle-popup
  '((((background dark)) :background "#2b2140" :foreground "#f3e9ff")
    (t :background "#fff1c9" :foreground "#3b2600"))
  "Body of the autoscroll notice box.")

(defface subtle-popup-border
  '((((background dark)) :background "#2b2140" :foreground "#ffb454" :weight bold)
    (t :background "#fff1c9" :foreground "#d9480f" :weight bold))
  "Border of the autoscroll notice box.")

(defface subtle-popup-title
  '((((background dark)) :background "#ffb454" :foreground "#1a1026" :weight bold)
    (t :background "#d9480f" :foreground "#ffffff" :weight bold))
  "Title of the autoscroll notice box.")

(defface subtle-popup-key
  '((((background dark)) :background "#7dcfff" :foreground "#10202b" :weight bold)
    (t :background "#1d4ed8" :foreground "#ffffff" :weight bold))
  "Key caps in the autoscroll notice box.")

(provide 'subtle-custom)
;;; subtle-custom.el ends here
