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

(defconst maple-imenu-name "*maple-imenu*")
(defvar maple-imenu-buffer nil)
(defvar maple-imenu-overlays nil)

(defgroup maple-imenu nil
  "Display imenu in window side."
  :group 'imenu)

(defcustom maple-imenu-auto-update nil
  "Whether auto update imenu when file save."
  :type 'boolean
  :group 'maple-imenu)

(defcustom maple-imenu-width 25
  "Display window width."
  :type 'number
  :group 'maple-imenu)

(defcustom maple-imenu-indent 2
  "Display indent."
  :type 'number
  :group 'maple-imenu)

(defcustom maple-imenu-arrow '("▾" . "▸")
  "Display arrow when show or hide entry."
  :type 'cons
  :group 'maple-imenu)

(defcustom maple-imenu-display-action '(maple-imenu--set-buffer)
  "Display buffer func."
  :type 'list
  :group 'maple-imenu)

(defcustom maple-imenu-display-alist '((side . right) (slot . -1))
  "Used by `display-buffer-in-side-window`."
  :type 'alist
  :group 'maple-imenu)

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
  `(let ((buffer (get-buffer-create maple-imenu-name)))
     (with-current-buffer buffer
       ,@body)))

(defmacro maple-imenu--with-window (&rest body)
  "Execute the forms in BODY with window."
  (declare (indent 0) (debug t))
  `(let ((window (maple-imenu-window))
         (golden-ratio-mode-p (when (featurep 'golden-ratio) golden-ratio-mode)))
     (when golden-ratio-mode-p (golden-ratio-mode -1))
     (when window (with-selected-window window ,@body))
     (when golden-ratio-mode-p (golden-ratio-mode golden-ratio-mode-p))))

(defmacro maple-imenu--with-writable (&rest body)
  "Execute the forms in BODY with window."
  (declare (indent 0) (debug t))
  `(save-excursion
     (read-only-mode -1)
     ,@body
     (read-only-mode 1)))

(defun maple-imenu--entries(&optional buffer)
  "Get imenu with &optional BUFFER."
  (with-current-buffer (or buffer maple-imenu-buffer (current-buffer))
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
          (pop-to-buffer maple-imenu-buffer)
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
      (setq padding (+ padding maple-imenu-indent))
      (dolist (arg item) (maple-imenu--handler arg padding)))))

(defun maple-imenu--set-buffer (buffer _alist)
  "Display BUFFER _ALIST."
  (display-buffer-in-side-window buffer maple-imenu-display-alist))

(defun maple-imenu--set-window ()
  "Reset window width."
  (let ((w (max maple-imenu-width window-min-width)))
    (if (> (window-width) w)
        (shrink-window-horizontally (- (window-width) w))
      (if (< (window-width) w)
          (enlarge-window-horizontally (- w (window-width)))))))

(defun maple-imenu--level()
  "Get current line level."
  (let ((text (thing-at-point 'line t)))
    (- (string-width text) (string-width (string-trim-left text)))))

(defun maple-imenu--point()
  "Get point."
  (let* ((level (maple-imenu--level))
         (point (line-end-position))
         stop)
    (save-excursion
      (while (not stop)
        (forward-line 1)
        (if (and (> (maple-imenu--level) level)
                 (< point (point-max)))
            (setq point (line-end-position))
          (setq stop t))))
    point))

(defun maple-imenu--exchange-arrow(&optional reverse)
  "Exchange arrow with &optional REVERSE."
  (maple-imenu--with-writable
    (beginning-of-line)
    (when (search-forward (if reverse (cdr maple-imenu-arrow) (car maple-imenu-arrow)) nil t)
      (replace-match (if reverse (car maple-imenu-arrow) (cdr maple-imenu-arrow))))))

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
  (let ((new-overlay (make-overlay (line-end-position) (maple-imenu--point))))
    (maple-imenu--exchange-arrow)
    (push (cons (line-number-at-pos) new-overlay) maple-imenu-overlays)
    (overlay-put new-overlay 'invisible t)))

(defun maple-imenu-toggle-entry(&optional _)
  "Toggle entry."
  (interactive)
  (let ((overlay (assq (line-number-at-pos) maple-imenu-overlays)))
    (if overlay (maple-imenu-show-entry overlay)
      (maple-imenu-hide-entry))))

(defun maple-imenu-update()
  "Update imenu."
  (interactive)
  (when (maple-imenu-window)
    (setq maple-imenu-buffer (current-buffer))
    (maple-imenu--with-buffer (maple-imenu-refresh))))

(defun maple-imenu-refresh(&optional entries)
  "Refresh imenu buffer &optional ENTRIES."
  (interactive)
  (maple-imenu--with-writable
    (erase-buffer)
    (dolist (item (or entries (maple-imenu--entries)))
      (maple-imenu--handler item)))
  (maple-imenu--with-window
    (setq window-size-fixed nil)
    (maple-imenu--set-window)
    (setq window-size-fixed 'width)))

(defun maple-imenu-show ()
  "Show."
  (interactive)
  (setq maple-imenu-buffer (current-buffer))
  (let ((entries (maple-imenu--entries)))
    (if (car entries)
        (maple-imenu--with-buffer
          (maple-imenu-mode)
          (maple-imenu-refresh entries))
      (maple-imenu-hide)
      (message "no imenus found."))))

(defun maple-imenu-hide ()
  "Hide."
  (interactive)
  (when-let ((window (maple-imenu-window)))
    (delete-window window))
  (setq maple-imenu-buffer nil))

(defun maple-imenu-window ()
  "Whether show maple-imenu."
  (get-buffer-window maple-imenu-name t))

(defun maple-imenu ()
  "Toggle open and close."
  (interactive)
  (if (maple-imenu-window) (maple-imenu-hide) (maple-imenu-show)))

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
  (select-window (display-buffer maple-imenu-name maple-imenu-display-action))
  (when maple-imenu-auto-update
    (add-hook 'after-save-hook 'maple-imenu-update)))

(provide 'maple-imenu)
;;; maple-imenu.el ends here
