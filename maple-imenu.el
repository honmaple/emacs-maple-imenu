;;; maple-imenu.el ---  maple imenu configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/dotfiles/tree/master/emacs.d

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; maple imenu configuration.
;;

;;; Code:

(require 'imenu)
(require 'cl-lib)
(require 'subr-x)


(defgroup maple-imenu nil
  "Variables for `maple-imenu' package."
  :group 'imenu)


(defvar maple-imenu-buffer "*maple-imenu*")
(defvar maple-imenu-displayed-buffer nil
  "The buffer who owns the saved imenu entries.")
(defvar maple-imenu-overlays nil)
(defvar maple-imenu-width 25)
(defvar maple-imenu-padding 2)
(defvar maple-imenu-position 'right)
(defvar maple-imenu-arrow '("▾" . "▸"))
(defvar maple-imenu-auto-update nil)

(defface maple-imenu-face
  '((t (:inherit font-lock-keyword-face)))
  "Default face for maple-imenu.")

(defface maple-imenu-item-face
  '((t (:inherit font-lock-keyword-face
                 :foreground "chocolate")))
  "Default item face for maple-imenu.")

(defmacro maple-imenu--with-buffer (&rest body)
  "Execute the forms in BODY with buffer."
  (declare (indent 0) (debug t))
  `(let ((buffer (get-buffer-create maple-imenu-buffer)))
     (with-current-buffer buffer
       ,@body)))

(defmacro maple-imenu--with-window (&rest body)
  "Execute the forms in BODY with window."
  (declare (indent 0) (debug t))
  `(let ((window (get-buffer-window maple-imenu-buffer t))
         (golden-ratio-mode-p (when (featurep 'golden-ratio) golden-ratio-mode)))
     (when golden-ratio-mode-p (golden-ratio-mode -1))
     (when window (with-selected-window window ,@body))
     (when golden-ratio-mode-p (golden-ratio-mode golden-ratio-mode-p))))

(defmacro maple-imenu--with-writable (&rest body)
  "Execute the forms in BODY with window."
  (declare (indent 0) (debug t))
  `(progn
     (read-only-mode -1)
     ,@body
     (read-only-mode 1)))

(defun maple-imenu--get-entries(&optional buffer)
  "Get imenu with &optional BUFFER."
  (with-current-buffer (or buffer maple-imenu-displayed-buffer (current-buffer))
    (let* ((imenu-max-item-length "Unlimited")
           (imenu-auto-rescan t)
           (imenu-auto-rescan-maxout (if current-prefix-arg
                                         (buffer-size)
                                       imenu-auto-rescan-maxout))
           (items (imenu--make-index-alist t)))
      (delete (assoc "*Rescan*" items) items))))

(defun maple-imenu--item-text(item &optional padding)
  "TEXT ITEM &OPTIONAL PADDING."
  (when (not (string= (car item) ""))
    (let* ((point (cdr item))
           (point (if (overlayp point) (overlay-start point) point))
           (text (replace-regexp-in-string "^dummy::" "" (car item)))
           (text  (format "%s%s" (make-string (or padding 0) ?\s) text)))
      (insert-button
       text
       'action
       `(lambda (_)
          (pop-to-buffer maple-imenu-displayed-buffer)
          (goto-char ,point))
       'follow-link t
       'face 'maple-imenu-item-face)
      (insert "\n"))))

(defun maple-imenu--text(text &optional padding)
  "TEXT &OPTIONAL PADDING."
  (insert-button
   (format "%s%s %s" (make-string (or padding 0) ?\s) (car maple-imenu-arrow) text)
   'action 'maple-imenu-toggle-entry
   'face 'maple-imenu-face
   'follow-link t)
  (insert "\n"))

(defun maple-imenu--handler(items &optional padding)
  "Handler ITEMS &OPTIONAL PADDING."
  (let ((keyword (car items))
        (item (cdr items))
        (padding (or padding 0)))
    (if (not (listp item))
        (maple-imenu--item-text items padding)
      (maple-imenu--text keyword padding)
      (setq padding (+ padding maple-imenu-padding))
      (dolist (arg item) (maple-imenu--handler arg padding)))))

(defun maple-imenu--set-buffer (buffer _alist)
  "Display BUFFER _ALIST."
  (let ((window-pos (if (eq maple-imenu-position 'left) 'left 'right)))
    (display-buffer-in-side-window buffer `((side . ,window-pos)))))

(defun maple-imenu--set-window ()
  "Reset window width."
  (let ((w (max maple-imenu-width window-min-width)))
    (if (> (window-width) w)
        (shrink-window-horizontally (- (window-width) w))
      (if (< (window-width) w)
          (enlarge-window-horizontally (- w (window-width)))))))

(defun maple-imenu--get-level()
  "Get current line level."
  (let ((text (thing-at-point 'line t)))
    (- (string-width text) (string-width (string-trim-left text)))))

(defun maple-imenu--get-point()
  "Get point."
  (let* ((level (maple-imenu--get-level))
         (point (line-end-position))
         stop)
    (save-excursion
      (while (not stop)
        (forward-line 1)
        (if (and (> (maple-imenu--get-level) level)
                 (< point (point-max)))
            (setq point (line-end-position))
          (setq stop t))))
    point))

(defun maple-imenu--exchange-arrow(&optional reverse)
  "Exchange arrow with &optional REVERSE."
  (maple-imenu--with-writable
    (save-excursion
      (beginning-of-line)
      (when (search-forward (if reverse (cdr maple-imenu-arrow) (car maple-imenu-arrow)) nil t)
        (replace-match (if reverse (car maple-imenu-arrow) (cdr maple-imenu-arrow)))))))

(defun maple-imenu-show-entry(&optional overlay)
  "Show entry with &optional OVERLAY."
  (interactive)
  (let ((overlay (or overlay (car maple-imenu-overlays))))
    (maple-imenu--exchange-arrow t)
    (delete-overlay (cdr overlay))
    (setq maple-imenu-overlays
          (cl-remove-if
           (lambda(x) (eq (car x) (car overlay)))
           maple-imenu-overlays))))

(defun maple-imenu-hide-entry()
  "Hide entry."
  (interactive)
  (let ((new-overlay (make-overlay (line-end-position) (maple-imenu--get-point))))
    (maple-imenu--exchange-arrow)
    (push (cons (line-number-at-pos) new-overlay) maple-imenu-overlays)
    (overlay-put new-overlay 'invisible t)))

(defun maple-imenu-toggle-entry(&optional _)
  "Toggle entry."
  (interactive)
  (let ((overlay (assq (line-number-at-pos) maple-imenu-overlays)))
    (if overlay
        (maple-imenu-show-entry overlay)
      (maple-imenu-hide-entry))))

(defun maple-imenu-update()
  "Update imenu."
  (interactive)
  (when (get-buffer-window maple-imenu-buffer)
    (setq maple-imenu-displayed-buffer (current-buffer))
    (maple-imenu--with-buffer (maple-imenu-refresh))))

(defun maple-imenu-refresh(&optional entries)
  "Refresh imenu buffer &optional ENTRIES."
  (interactive)
  (maple-imenu--with-writable
    (save-excursion
      (erase-buffer)
      (dolist (item (or entries (maple-imenu--get-entries)))
        (maple-imenu--handler item))))
  (maple-imenu--with-window
    (setq window-size-fixed nil)
    (maple-imenu--set-window)
    (setq window-size-fixed 'width)))

(defun maple-imenu-show ()
  "Show."
  (interactive)
  (setq maple-imenu-displayed-buffer (current-buffer))
  (let ((entries (maple-imenu--get-entries)))
    (if (car entries)
        (maple-imenu--with-buffer
          (maple-imenu-mode)
          (maple-imenu-refresh entries))
      (maple-imenu-hide)
      (message "no imenus found."))))

(defun maple-imenu-hide ()
  "Hide."
  (interactive)
  (let ((window (get-buffer-window maple-imenu-buffer t)))
    (when window (delete-window window)))
  (setq maple-imenu-displayed-buffer nil))

(defun maple-imenu ()
  "Toggle open and close."
  (interactive)
  (if (get-buffer-window maple-imenu-buffer t)
      (maple-imenu-hide)
    (maple-imenu-show)))

(defvar maple-imenu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] #'maple-imenu-toggle-entry)
    (define-key map (kbd "r") #'maple-imenu-refresh)
    (define-key map (kbd "q") #'maple-imenu-hide)
    (define-key map (kbd "h") #'backward-char)
    (define-key map (kbd "l") #'forward-char)
    map)
  "Maple-imenu-mode keymap.")

;;;###autoload
(define-derived-mode maple-imenu-mode special-mode "maple-imenu"
  "Maple-imenu-mode."
  (setq indent-tabs-mode nil
        buffer-read-only t
        truncate-lines -1
        cursor-in-non-selected-windows nil)
  (pop-to-buffer maple-imenu-buffer '(maple-imenu--set-buffer))
  (when maple-imenu-auto-update
    (add-hook 'after-save-hook 'maple-imenu-update)))

(provide 'maple-imenu)
;;; maple-imenu.el ends here
