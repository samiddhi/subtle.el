;;; subtle-popup.el --- Boxed "autoscroll paused" notice -*- lexical-binding: t; -*-

;;; Commentary:

;; A bordered, coloured box.  In a graphical Emacs it floats in a child
;; frame near the top centre; in a terminal it fills a bottom side window.
;; The box itself is a pure string built from rows.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'subtle-custom)

(defconst subtle-popup--buffer " *subtle-popup*")
(defvar subtle-popup--frame nil "Child frame currently showing the box, or nil.")
(defvar subtle-popup--kind nil "Kind of notice on screen: notice, help, or nil.")

;;; Pure box building

(defun subtle-popup--pad (text width)
  "TEXT followed by spaces up to display WIDTH."
  (concat text (make-string (max 0 (- width (string-width text))) ?\s)))

;; Appended, so faces already on TEXT (title, key caps) win over the body.
(defun subtle-popup--body (text)
  "Copy of TEXT with the box body face added behind its own faces."
  (let ((copy (copy-sequence text)))
    (add-face-text-property 0 (length copy) 'subtle-popup t copy)
    copy))

(defun subtle-popup--row (text width)
  "One boxed row holding TEXT padded to WIDTH."
  (concat (propertize "│" 'face 'subtle-popup-border)
          (subtle-popup--body (concat " " (subtle-popup--pad text width) " "))
          (propertize "│" 'face 'subtle-popup-border)))

(defun subtle-popup--edge (left right width)
  "Top or bottom edge made of LEFT, a horizontal rule of WIDTH, and RIGHT."
  (propertize (concat left (make-string (+ width 2) ?─) right)
              'face 'subtle-popup-border))

(defun subtle-popup-box (rows)
  "ROWS (strings) drawn inside a box, as one multi-line string."
  (let ((width (apply #'max (mapcar #'string-width rows))))
    (string-join
     (append (list (subtle-popup--edge "╭" "╮" width))
             (mapcar (lambda (row) (subtle-popup--row row width)) rows)
             (list (subtle-popup--edge "╰" "╯" width)))
     "\n")))

(defun subtle-popup-title (text)
  "TEXT styled as the box title."
  (propertize (concat " " text " ") 'face 'subtle-popup-title))

(defun subtle-popup-key (key)
  "KEY styled as a key cap."
  (propertize (concat " " key " ") 'face 'subtle-popup-key))

;;; Showing

(defun subtle-popup--fill-buffer (box)
  "Put BOX in the popup buffer and return the buffer."
  (with-current-buffer (get-buffer-create subtle-popup--buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert box))
    (setq-local mode-line-format nil header-line-format nil cursor-type nil
                truncate-lines t)
    (current-buffer)))

(defun subtle-popup--frame-parameters (parent box)
  "Parameters of a borderless, unfocusable child frame of PARENT fitting BOX."
  `((parent-frame . ,parent)
    (width . ,(+ 4 (apply #'max (mapcar #'string-width (split-string box "\n")))))
    (height . ,(length (split-string box "\n"))) (name . "subtle") (minibuffer . nil)
    (undecorated . t) (no-accept-focus . t) (no-focus-on-map . t)
    (skip-taskbar . t) (unsplittable . t) (visibility . nil)
    (internal-border-width . 0) (child-frame-border-width . 0)
    (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
    (left-fringe . 0) (right-fringe . 0) (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil) (cursor-type . nil)))

(defun subtle-popup--place (frame parent)
  "Centre FRAME horizontally near the top of PARENT."
  (set-frame-position
   frame
   (max 0 (/ (- (frame-pixel-width parent) (frame-pixel-width frame)) 2))
   (round (* 0.06 (frame-pixel-height parent)))))

(defun subtle-popup--show-frame (buffer box)
  "Float BUFFER, showing BOX, in a child frame."
  (let* ((parent (selected-frame))
         (frame (make-frame (subtle-popup--frame-parameters parent box))))
    (set-window-buffer (frame-root-window frame) buffer)
    (set-window-dedicated-p (frame-root-window frame) t)
    (subtle-popup--place frame parent)
    (make-frame-visible frame)
    (setq subtle-popup--frame frame)))

(defun subtle-popup--show-side-window (buffer)
  "Show BUFFER in a bottom side window."
  (display-buffer-in-side-window
   buffer '((side . bottom) (slot . 0)
            (window-height . fit-window-to-buffer)
            (window-parameters . ((no-other-window . t))))))

(defun subtle-popup--style (kind)
  "Display style for a notice of KIND; help always uses the box."
  (if (eq kind 'help) 'box subtle-popup-style))

(defun subtle-popup-show (rows &optional kind)
  "Show ROWS of a notice of KIND (default `notice') per `subtle-popup-style'."
  (subtle-popup-hide)
  (setq subtle-popup--kind (or kind 'notice))
  (pcase (subtle-popup--style subtle-popup--kind)
    ('box (let* ((box (subtle-popup-box rows))
                (buffer (subtle-popup--fill-buffer box)))
            (if (display-graphic-p)
                (subtle-popup--show-frame buffer box)
              (subtle-popup--show-side-window buffer))))
    ('echo-area (message "%s" (string-join rows "   ")))))

(defun subtle-popup-kind ()
  "Kind of the notice on screen (`notice' or `help'), or nil."
  (and (subtle-popup-visible-p) subtle-popup--kind))

(defun subtle-popup-visible-p ()
  "Non-nil while a box is on screen."
  (buffer-live-p (get-buffer subtle-popup--buffer)))

(defun subtle-popup-hide (&optional kind)
  "Remove the box, wherever it is.  With KIND, only if that kind is shown."
  (when (or (null kind) (eq kind subtle-popup--kind))
    (when (frame-live-p subtle-popup--frame)
      (delete-frame subtle-popup--frame))
    (setq subtle-popup--frame nil subtle-popup--kind nil)
    (when-let* ((buffer (get-buffer subtle-popup--buffer)))
      (delete-windows-on buffer)
      (kill-buffer buffer))))

(provide 'subtle-popup)
;;; subtle-popup.el ends here
