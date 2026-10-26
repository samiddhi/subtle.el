;;; subtle-test.el --- Tests for subtle's pure core -*- lexical-binding: t; -*-

(require 'ert)
(require 'subtle-parse)
(require 'subtle-scroll)
(require 'subtle-layout)
(require 'subtle-ui)
(require 'subtle-mpv)
(require 'subtle-pick)
(require 'subtle)

(defconst subtle-test--srt
  "﻿1\r\n00:00:01,000 --> 00:00:03,500\r\n<i>Hello</i>\r\nworld &amp; co\r\n\r\n2\r\n00:00:05,000 --> 00:00:06,000\r\nSecond\r\n\r\nbroken block\r\n\r\n3\r\n00:01:00,250 --> 00:01:02,000\r\nLast\r\n")

(defconst subtle-test--vtt
  "WEBVTT\n\nNOTE a comment\n\ncue-id\n00:07.500 --> 00:09.000 align:start\n{\\an8}Up top\n")

(defun subtle-test--texts (track)
  (mapcar #'subtle-cue-text (subtle-track-cues track)))

;;; Parse

(ert-deftest subtle-parse-srt ()
  (let ((track (subtle-parse-string subtle-test--srt)))
    (should (equal (subtle-test--texts track)
                   '("Hello world & co" "Second broken block" "Last")))
    (should (equal (subtle-track-starts track) [1.0 5.0 60.25]))
    (should (= (subtle-cue-end (aref (subtle-track-cues track) 0)) 3.5))))

(ert-deftest subtle-parse-vtt ()
  (let ((track (subtle-parse-string subtle-test--vtt)))
    (should (equal (subtle-test--texts track) '("Up top")))
    (should (equal (subtle-track-starts track) [7.5]))))

(ert-deftest subtle-parse-sorts-and-drops-empty ()
  (let ((track (subtle-parse-string
                "00:00:09,000 --> 00:00:10,000\nb\n\n00:00:01,000 --> 00:00:02,000\na\n\n00:00:03,000 --> 00:00:04,000\n<i></i>\n")))
    (should (equal (subtle-test--texts track) '("a" "b")))))

(ert-deftest subtle-parse-empty ()
  (should (= 0 (length (subtle-track-cues (subtle-parse-string ""))))))

(ert-deftest subtle-parse-explicit-breaks ()
  (let* ((track (subtle-parse-string
                 (concat "00:00:01,000 --> 00:00:02,000\none\n\n"
                         "00:00:03,000 --> 00:00:04,000\ntwo\n\nmore of two\n\n"
                         "00:00:05,000 --> 00:00:06,000\nthree\n\n"
                         "00:00:07,000 --> 00:00:08,000\n\n"
                         "00:00:09,000 --> 00:00:10,000\nfour\n")))
         (cues (subtle-track-cues track)))
    (should (equal (subtle-test--texts track) '("one" "two more of two" "three" "four")))
    (should (equal (mapcar #'subtle-cue-break-after cues) '(nil t t nil)))))

(ert-deftest subtle-parse-ignores-header-blocks ()
  (let ((track (subtle-parse-string
                "00:00:01,000 --> 00:00:02,000\none\n\nNOTE x\n\n7\n\nSTYLE\n")))
    (should (equal (subtle-test--texts track) '("one")))
    (should-not (subtle-cue-break-after (aref (subtle-track-cues track) 0)))))

;;; Layout

(ert-deftest subtle-layout-sentence-split ()
  (should (equal '("Hi." "He said \"No!\"" "(Ok.) Really?" "Yes")
                 (let ((r (subtle-layout-sentence-split
                           "Hi. He said \"No!\" (Ok.) Really? Yes")))
                   ;; "(Ok.) Really?" splits after the closing paren
                   (list (nth 0 r) (nth 1 r) (concat (nth 2 r) " " (nth 3 r)) (nth 4 r)))))
  (should (equal '("3.5 is big") (subtle-layout-sentence-split "3.5 is big")))
  (should (equal '("Wait…" "go") (subtle-layout-sentence-split "Wait… go"))))

(ert-deftest subtle-layout-sentence-end ()
  (should (subtle-layout-sentence-end-p "Done.)\""))
  (should (subtle-layout-sentence-end-p "Really?!"))
  (should-not (subtle-layout-sentence-end-p "and then")))

(defun subtle-test--cue (start end text &optional brk)
  (subtle-cue-create :start start :end end :text text :break-after brk))

(ert-deftest subtle-layout-breaks ()
  (let ((subtle-paragraph-gap 2.0)
        (a (subtle-test--cue 0 1 "Done."))
        (b (subtle-test--cue 4 5 "Next"))
        (c (subtle-test--cue 1.5 2 "x" t))
        (d (subtle-test--cue 4 5 "No end")))
    (should (subtle-layout-break-after-p 'explicit-and-gap a b))
    (should-not (subtle-layout-break-after-p 'explicit a b))
    (should (subtle-layout-break-after-p 'explicit c nil))
    (should-not (subtle-layout-break-after-p 'explicit-and-gap d b))
    (should-not (subtle-layout-break-after-p 'explicit-and-gap a (subtle-test--cue 2 3 "y")))))

(ert-deftest subtle-layout-separators ()
  (let ((a (subtle-test--cue 0 1 "Done.")) (b (subtle-test--cue 1.2 2 "More"))
        (c (subtle-test--cue 0 1 "and")))
    (should (equal "\n" (subtle-layout-separator 'single-spaced 'explicit a b)))
    (should (equal "\n\n" (subtle-layout-separator 'double-spaced 'explicit a b)))
    (should (equal "\n" (subtle-layout-separator 'sentence 'explicit a b)))
    (should (equal " " (subtle-layout-separator 'sentence 'explicit c b)))
    (should (equal " " (subtle-layout-separator 'paragraph-flow 'explicit-and-gap a b)))
    (should (equal "\n\n" (subtle-layout-separator
                            'paragraph-pause 'explicit
                            (subtle-test--cue 0 1 "x" t) b)))))

(ert-deftest subtle-layout-pause-points ()
  (let* ((track (subtle-parse-from-cues
                 (list (subtle-test--cue 0 1 "A." t) (subtle-test--cue 1 3 "B")
                       (subtle-test--cue 3 4 "C." t))))
         (points (subtle-layout-pause-points 'explicit track)))
    (should (equal [1] points))            ; last cue has no successor
    (should (= 1 (subtle-layout-pause-crossed points 0.9 1.05)))
    (should-not (subtle-layout-pause-crossed points 1.05 1.1))
    (should-not (subtle-layout-pause-crossed points 0.0 1.5))   ; a seek
    (should-not (subtle-layout-pause-crossed points 1.5 1.2))))

(ert-deftest subtle-ui-user-keys-come-first ()
  (let* ((defaults '("p" "q"))
         (controls '(("p" pause "pause") ("q" quit "quit") ("s-d" pause nil))))
    (should (equal "s-d pause  q quit"
                   (substring-no-properties (subtle-ui--hints controls defaults))))
    (should (equal "s-d (p)" (subtle-ui--keys-text '("p" "s-d") defaults)))
    (should (equal "p / q" (subtle-ui--keys-text '("p" "q") defaults)))
    (should (equal "s-d" (subtle-ui--keys-text '("s-d") defaults)))))

(ert-deftest subtle-ui-hint-rows-use-user-key ()
  (let ((rows (subtle-ui--hint-rows
               t 5 '(("r" subtle-resume-follow nil) ("s-r" subtle-resume-follow nil)
                     ("s t" subtle-toggle-auto-resume nil) ("q" subtle-quit nil)))))
    (should (string-match-p " s-r " (substring-no-properties (nth 1 rows))))))

;;; Option commands

(ert-deftest subtle-option-choices ()
  (should (equal '(single-spaced double-spaced sentence paragraph-flow paragraph-pause)
                 (subtle--choices 'subtle-layout)))
  (should-not (subtle--choices 'subtle-text-scale)))

(ert-deftest subtle-set-layout-offers-menu ()
  (let ((subtle-layout 'single-spaced) offered)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p collection &rest _) (setq offered collection) "sentence"))
              ((symbol-function 'customize-set-variable)
               (lambda (o v) (set o v))))
      (subtle-set-layout)
      (should (member "paragraph-pause" offered))
      (should (eq 'sentence subtle-layout)))))

(ert-deftest subtle-rebuild-relays-out-buffers ()
  (let* ((track (subtle-parse-string
                 "00:00:01,000 --> 00:00:02,000\nA.\n\n00:00:03,000 --> 00:00:04,000\nB\n"))
         (subtle-layout 'single-spaced)
         (session (make-subtle-session :tracks (list track) :indices '(1)))
         (buffer (subtle-ui--make-buffer track "t")))
    (unwind-protect
        (progn (setf (subtle-session-buffers session) (list buffer))
               (let ((subtle-layout 'paragraph-flow) (subtle-paragraph-gap 2.0))
                 (subtle-ui-rebuild session))
               (with-current-buffer buffer
                 (should (equal "A. B" (buffer-substring-no-properties
                                        (point-min) (point-max))))
                 (should (overlay-buffer subtle-ui--overlay))))
      (kill-buffer buffer))))

(ert-deftest subtle-rebuild-does-not-grow-font ()
  (let* ((track (subtle-parse-string "00:00:01,000 --> 00:00:02,000\nA\n"))
         (buffer (subtle-ui--make-buffer track "t")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((before (copy-tree face-remapping-alist)))
            (dotimes (_ 3) (subtle-ui--rebuild-buffer buffer nil))
            (should (equal before face-remapping-alist))))
      (kill-buffer buffer))))

;;; Popup policy

(ert-deftest subtle-ui-popup-wanted ()
  (should (subtle-ui--popup-wanted-p nil nil t nil))
  (should-not (subtle-ui--popup-wanted-p t nil t nil))
  (should-not (subtle-ui--popup-wanted-p nil t t nil))
  (should-not (subtle-ui--popup-wanted-p nil nil nil nil))
  (should-not (subtle-ui--popup-wanted-p nil nil t t)))

(ert-deftest subtle-ui-buffer-layouts ()
  (let ((track (subtle-parse-string
                (concat "00:00:01,000 --> 00:00:02,000\nHello there.\n\n"
                        "00:00:02,100 --> 00:00:03,000\nHow are\n\n"
                        "00:00:03,100 --> 00:00:04,000\nyou?\n"))))
    (dolist (case '((single-spaced . "Hello there.\nHow are\nyou?")
                    (double-spaced . "Hello there.\n\nHow are\n\nyou?")
                    (sentence . "Hello there.\nHow are you?")
                    (paragraph-flow . "Hello there. How are you?")))
      (let ((subtle-layout (car case)) (spans nil))
        (with-temp-buffer
          (setq spans (subtle-ui--insert-cues track))
          (should (equal (cdr case) (buffer-substring-no-properties (point-min) (point-max))))
          (should (equal "How are"
                         (buffer-substring-no-properties
                          (car (aref spans 1)) (cdr (aref spans 1))))))))))

;;; Track lookup

(ert-deftest subtle-track-cue-index ()
  (let ((track (subtle-parse-string subtle-test--srt)))
    (should-not (subtle-track-cue-index track 0.5))
    (should (= 0 (subtle-track-cue-index track 1.0)))
    (should (= 0 (subtle-track-cue-index track 3.4)))
    (should-not (subtle-track-cue-index track 3.5))
    (should (= 0 (subtle-track-cue-index track 4.0 t)))
    (should (= 1 (subtle-track-cue-index track 5.5)))
    (should (= 2 (subtle-track-cue-index track 999 t)))))

(ert-deftest subtle-track-independent-tracks ()
  (let ((a (subtle-parse-string subtle-test--srt))
        (b (subtle-parse-string subtle-test--vtt)))
    (should (equal (list (subtle-track-cue-index a 8.0 t)
                         (subtle-track-cue-index b 8.0))
                   '(1 0)))))

(ert-deftest subtle-track-neighbors ()
  (let ((track (subtle-parse-string subtle-test--srt)))
    (should (= 1.0 (subtle-track-next-start track 0)))
    (should (= 5.0 (subtle-track-next-start track 1.0)))
    (should-not (subtle-track-next-start track 61))
    (should (= 5.0 (subtle-track-previous-start track 7.0)))
    (should (= 1.0 (subtle-track-previous-start track 5.5)))
    (should (= 0.0 (subtle-track-previous-start track 0.5)))))

;;; Scroll and follow

(ert-deftest subtle-scroll-offset ()
  (should (= 20 (subtle-scroll-offset 30 (/ 2.0 3.0))))
  (should (= 0 (subtle-scroll-offset 1 0.9)))
  (should (= 9 (subtle-scroll-offset 10 5.0))))

(ert-deftest subtle-follow-machine ()
  (should-not (subtle-follow-next t 'user-scroll))
  (should (subtle-follow-next nil 'resume))
  (should-not (subtle-follow-next nil 'other))
  (should (subtle-follow-next t 'other)))

;;; UI text

(ert-deftest subtle-ui-format-time ()
  (should (equal "00:07" (subtle-ui--format-time 7.9)))
  (should (equal "03:12" (subtle-ui--format-time 192)))
  (should (equal "1:01:01" (subtle-ui--format-time 3661))))

(ert-deftest subtle-ui-hints-only-labelled ()
  (let ((text (substring-no-properties
               (subtle-ui--hints '(("p" a "pause") ("x" b nil) ("q" c "close"))))))
    (should (equal text "p pause  q close"))))

(ert-deftest subtle-ui-header-parts ()
  (should (equal '("s" "h") (subtle-ui--header-parts "s" "l" "h" 1 0)))
  (should (equal '("s" "l") (subtle-ui--header-parts "s" "l" "h" 2 0)))
  (should (equal '("h") (subtle-ui--header-parts "s" "l" "h" 2 1))))

(ert-deftest subtle-ui-hint-rows ()
  (let ((on (mapconcat #'substring-no-properties (subtle-ui--hint-rows t 5) "|"))
        (off (mapconcat #'substring-no-properties (subtle-ui--hint-rows nil 5) "|")))
    (should (string-match-p "on (5s)" on))
    (should (string-match-p "auto-resume off" off))))

(ert-deftest subtle-popup-box-aligned ()
  (let ((lines (split-string (subtle-popup-box '("ab" "abcd")) "\n")))
    (should (= 4 (length lines)))
    (should (= 1 (length (seq-uniq (mapcar #'string-width lines)))))))

(ert-deftest subtle-ui-font-specs ()
  (let ((subtle-use-variable-pitch t) (subtle-text-scale 1.5))
    (should (equal '(variable-pitch (:height 1.5)) (subtle-ui--font-specs))))
  (let ((subtle-use-variable-pitch nil) (subtle-text-scale 1.0))
    (should (equal '((:height 1.0)) (subtle-ui--font-specs)))))

(ert-deftest subtle-controls-bound ()
  (should (eq 'subtle-toggle-pause (lookup-key subtle-mode-map (kbd "p"))))
  (should (eq 'subtle-help (lookup-key subtle-mode-map (kbd "h"))))
  (should (eq 'subtle-quit (lookup-key subtle-mode-map (kbd "q")))))

(ert-deftest subtle-ui-help-groups-aliases ()
  (let ((groups (subtle-ui--help-groups '(("a" f1 nil) ("b" f2 nil) ("c" f1 nil)))))
    (should (equal '((f1 "a" "c") (f2 "b")) groups))))

(ert-deftest subtle-quit-hides-popup-first ()
  (let ((closed nil) (asked nil))
    (cl-letf (((symbol-function 'subtle-close) (lambda () (setq closed t)))
              ((symbol-function 'y-or-n-p) (lambda (_) (setq asked t) t)))
      (subtle-popup-show '("x"))
      (subtle-quit)
      (should-not (or closed asked (subtle-popup-visible-p)))
      (subtle-quit)
      (should (and closed asked))
      (setq closed nil asked nil)
      (let ((subtle-confirm-quit nil))
        (subtle-quit))
      (should (and closed (not asked))))))

;;; mpv

(ert-deftest subtle-mpv-command ()
  (should (equal "{\"command\":[\"seek\",-5,\"relative\"]}\n"
                 (subtle-mpv--command '("seek" -5 "relative")))))

(ert-deftest subtle-mpv-split-lines ()
  (should (equal '(("a" "b") . "c") (subtle-mpv--split-lines "" "a\nb\nc")))
  (should (equal '(("abc") . "") (subtle-mpv--split-lines "a" "bc\n"))))

(ert-deftest subtle-mpv-messages ()
  (should (equal '(time-pos . 1.5)
                 (subtle-mpv--line->msg
                  "{\"event\":\"property-change\",\"id\":0,\"name\":\"time-pos\",\"data\":1.5}")))
  (should (equal '(pause) (subtle-mpv--line->msg
                           "{\"event\":\"property-change\",\"name\":\"pause\",\"data\":false}")))
  (should-not (subtle-mpv--line->msg "{\"error\":\"success\"}"))
  (should-not (subtle-mpv--line->msg "garbage")))

;;; Pick

(ert-deftest subtle-pick-rank ()
  (should (equal '("talk.en.srt" "talk.pt.srt" "other.srt")
                 (subtle-pick--rank "talk.mp3" '("other.srt" "talk.en.srt" "talk.pt.srt")))))

(ert-deftest subtle-pick-resolve ()
  (let ((dir '("t, x.mp3" "t, x.en.srt" "t, x.sl.srt" "t, x.txt" "z.srt" "z.mp4")))
    (should (equal '("t, x.mp3" "t, x.en.srt" "t, x.sl.srt")
                   (cons (car (subtle-pick--resolve '("t, x.mp3") dir))
                         (cdr (subtle-pick--resolve '("t, x.mp3") dir)))))
    (should (equal '("t, x.mp3" "t, x.sl.srt")
                   (subtle-pick--resolve '("t, x.sl.srt" "t, x.mp3") dir)))
    (should (equal '("t, x.mp3" "t, x.sl.srt")
                   (subtle-pick--resolve '("t, x.sl.srt") dir)))
    (should-not (car (subtle-pick--resolve '("z.srt" "t, x.txt") '("t, x.txt" "z.srt"))))))

(ert-deftest subtle-rebuild-keeps-word-overlays-and-undo-clean ()
  (let* ((track (subtle-parse-string
                 (concat "00:00:01,000 --> 00:00:02,000\nHello there.\n\n"
                         "00:00:02,100 --> 00:00:03,000\nHow are you?\n")))
         (buffer (subtle-ui--make-buffer track "t")))
    (unwind-protect
        (with-current-buffer buffer
          (should-not buffer-undo-list)
          (let ((words nil) (changes 0))
            (goto-char (point-min))
            (while (re-search-forward "[^ \t\n]+" nil t)
              (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                (overlay-put ov 'evaporate t)
                (push ov words)))
            (add-hook 'after-change-functions (lambda (&rest _) (cl-incf changes)) nil t)
            (dolist (layout '(sentence paragraph-flow double-spaced single-spaced))
              (let ((subtle-layout layout))
                (subtle-ui--rebuild-buffer buffer 1)))
            (should (= 0 changes))
            (should (cl-every #'overlay-buffer words))
            (should (equal '("Hello" "there." "How" "are" "you?")
                           (mapcar (lambda (o) (buffer-substring-no-properties
                                                (overlay-start o) (overlay-end o)))
                                   (reverse words))))
            (should (cl-every (lambda (o) (not (string-match-p "[ \n]" (buffer-substring-no-properties (overlay-start o) (overlay-end o))))) words))
            (should (= 1 (get-text-property (car (aref subtle-ui--spans 1)) 'subtle-index)))
            (should-not buffer-undo-list)))
      (kill-buffer buffer))))

(ert-deftest subtle-scroll-move-point-policy ()
  (should (subtle-scroll-move-point-p 'always t nil))
  (should-not (subtle-scroll-move-point-p 'never nil t))
  (should (subtle-scroll-move-point-p 'unless-selected nil nil))
  (should (subtle-scroll-move-point-p 'unless-selected t t))
  (should-not (subtle-scroll-move-point-p 'unless-selected t nil)))

(ert-deftest subtle-scroll-page-decisions ()
  (should (subtle-scroll-page-needed-p t 0 500 40))
  (should-not (subtle-scroll-page-needed-p nil 400 500 40))
  (should (subtle-scroll-page-needed-p nil 461 500 40))
  (should (subtle-scroll-far-p 12 10))
  (should-not (subtle-scroll-far-p 3 10))
  (should-not (subtle-scroll-far-p 30 nil)))

(ert-deftest subtle-hold-pause-pauses-and-releases ()
  (let* ((sent nil)
         (track (subtle-parse-string subtle-test--srt))
         (buffer (subtle-ui--make-buffer track "t"))
         (session (make-subtle-session :tracks (list track) :buffers (list buffer)
                                       :indices '(1)
                                       :send (lambda (&rest a) (push a sent))))
         (anchor nil) (subtle-hold-pause-lines 0)
         (subtle-anchor-functions (list (lambda () anchor))))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (subtle-ui--hold-pause session)
          (should-not sent)
          (setq anchor (point-min))
          (subtle-ui--hold-pause session)
          (should (equal '("set_property" "pause" t) (car sent)))
          (should (eq t (subtle-session-hold-paused session)))
          (setq anchor nil)
          (subtle-ui--hold-pause session)
          (should (equal '("set_property" "pause" :false) (car sent)))
          (should-not (subtle-session-hold-paused session)))
      (kill-buffer buffer))))

(ert-deftest subtle-played-run-ignores-seeks-and-pauses ()
  (let* ((track (subtle-parse-string subtle-test--srt))
         (buffer (subtle-ui--make-buffer track "t"))
         (session (make-subtle-session :tracks (list track) :buffers (list buffer)))
         (spans nil))
    (unwind-protect
        (with-current-buffer buffer
          (setq spans subtle-ui--spans)
          (cl-flet ((step (from to idx) (subtle-ui--credit-playback session from to (list idx))))
            ;; play from 1.0 to 3.6: cue 0 done
            (step 0.9 1.0 0) (step 1.0 1.5 0) (step 1.5 2.0 0) (step 3.0 3.6 0)
            (should (= (car (aref spans 0)) (subtle-played-start-position)))
            (should (= (cdr (aref spans 0)) (subtle-played-through-position)))
            ;; pausing: same time again changes nothing
            (setf (subtle-session-paused session) t)
            (step 3.6 3.6 0)
            (should (= (cdr (aref spans 0)) (subtle-played-through-position)))
            (setf (subtle-session-paused session) nil)
            ;; a seek to cue 2 starts a new run there and credits nothing before it
            (step 3.6 60.5 2)
            (should (= (car (aref spans 2)) (subtle-played-start-position)))
            (should (= (car (aref spans 2)) (subtle-played-through-position)))
            (step 60.5 61.0 2) (step 61.0 62.5 2)
            (should (= (cdr (aref spans 2)) (subtle-played-through-position)))))
      (kill-buffer buffer))))

(ert-deftest subtle-bookmark-record-holds-media-and-time ()
  (let* ((track (subtle-parse-string subtle-test--srt))
         (session (make-subtle-session :tracks (list track) :media "/a/film.mkv"
                                       :subs '("/a/film.sl.srt") :position 42.5))
         (subtle--session session))
    (let ((record (subtle-bookmark-make-record)))
      (should (equal "film" (car record)))
      (should (equal 42.5 (alist-get 'time (cdr record))))
      (should (equal '("/a/film.sl.srt") (alist-get 'subs (cdr record))))
      (should (eq 'subtle-bookmark-jump (alist-get 'handler (cdr record)))))))

(ert-deftest subtle-quit-dismisses-before-closing ()
  (let ((subtle-dismiss-functions (list (lambda () t)))
        (closed nil))
    (cl-letf (((symbol-function 'subtle-close) (lambda () (setq closed t)))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (subtle-quit)
      (should-not closed)
      (let ((subtle-dismiss-functions nil))
        (subtle-quit)
        (should closed)))))

(ert-deftest subtle-ui-language-from-file-name ()
  (should (equal "slv" (subtle-ui--language "film.sl.srt")))
  (should (equal "xyz" (subtle-ui--language "film.xyz.vtt")))
  (should-not (subtle-ui--language "film.srt"))
  (should-not (subtle-ui--language "film.2024.srt")))

(ert-deftest subtle-ui-setup-hook-sees-index-and-language ()
  (let* ((seen nil)
         (subtle-buffer-setup-hook
          (list (lambda () (push (list subtle-track-index subtle-language) seen))))
         (track (subtle-parse-from-cues nil))
         (b0 (subtle-ui--make-buffer track "film.sl.srt" 0))
         (b1 (subtle-ui--make-buffer track "film.en.srt" 1)))
    (unwind-protect
        (should (equal (nreverse seen) '((0 "slv") (1 "eng"))))
      (kill-buffer b0) (kill-buffer b1))))

(provide 'subtle-test)
;;; subtle-test.el ends here
