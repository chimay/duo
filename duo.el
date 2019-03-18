;;; duo.el --- In place list operations in Elisp     -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Chimay

;; Author : Chimay
;; Name: Duo
;; Package-Version: 1.0
;; Package-requires: ((emacs "24"))
;; Keywords: list, in-place, operation
;; URL: https://github.com/chimay/duo

;;; Commentary:

;;; Library of in place list operations in Emacs-Lisp.

;;; License:
;;; ----------------------------------------------------------------------

;; This file is not part of Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:
;;; ----------------------------------------------------------------------

;; Cons DUO = (CAR . CDR) can be used as pointer
;; with setcar and setcdr

;; ELEM = (car DUO)
;; DUO = (member ELEM LIST)

;;; References
;;; ------------------------------

(defun torus--set-deref (ptr object)
  "Change the content of the variable referenced by PTR to OBJECT.
OBJECT must be a cons or a list."
  (setcar ptr (car object))
  (setcdr ptr (cdr object))
  ptr)

;;; Find
;;; ------------------------------

(defun torus--member (elem list)
  "Return cons of ELEM in LIST or nil if ELEM is not in list."
  (let ((duo list))
    (while (and duo
                (not (equal (car duo) elem)))
      (setq duo (cdr duo)))
    duo))

(defun torus--last (list &optional num)
  "Return cons starting a sublist of NUM elements at the end of LIST.
NUM defaults to 1 : NUM nil means return cons of last element in LIST."
  (let ((num (if num
                 num
               1))
        (last list))
    (while (nthcdr num last)
      (setq last (cdr last)))
    last))

(defun torus--index (elem list)
  "Index of ELEM in LIST."
  (- (length list) (length (member elem list))))

;;; Next / Previous
;;; ------------------------------

(defun torus--previous (cons list)
  "Return cons before CONS in LIST. CONS must reference a cons in list.
Circular : if in beginning of list, go to the end.
Test with eq."
  (let ((duo list))
    (if (eq duo cons)
        (torus--last list)
      (while (and duo
                  (not (eq (cdr duo) cons)))
        (setq duo (cdr duo)))
      duo)))

(defun torus--next (cons list)
  "Return cons after CONS in LIST. CONS must reference a cons in LIST.
Circular : if in end of list, go to the beginning."
  (let ((duo (cdr cons)))
    (if duo
        (cdr cons)
      list)))

(defun torus--before (elem list)
  "Return cons before ELEM in LIST.
Circular : if in beginning of list, go to the end."
  (let ((duo list))
    (if (equal (car duo) elem)
        (torus--last list)
      (while (and duo
                  (not (equal (car (cdr duo)) elem)))
        (setq duo (cdr duo)))
      duo)))

(defun torus--after (elem list)
  "Return cons after ELEM in LIST.
Circular : if in end of list, go to the beginning."
  (torus--next (torus--member elem list) list))

;;; Add / Remove
;;; ------------------------------

;;; Beginning / End
;;; ---------------

(defun torus--add (elem list)
  "Add ELEM at the end of LIST. Return the new end cons."
  (let ((last (torus--last list))
        (duo (cons elem nil)))
    (setcdr last duo)
    duo))

(defun torus--add-new (elem list)
  "Add ELEM at the end of LIST if not already there. Return the new end cons."
  (unless (member elem list)
    (torus--add elem list)))

(defun torus--drop (list)
  "Remove last element of LIST. Return cons of removed element."
  (let* ((before-last (torus--last list 2))
         (last (cdr before-last)))
    (if last
        (setcdr before-last nil)
      ;; One element list
      (setq last (cons (car list) nil))
      (setcar list nil))
    last))

(defun torus--truncate (list &optional num)
  "Truncate LIST to its first NUM elements."
  (let* ((num (if num
                  num
                nil))
         (last)
         (tail))
    (when num
      (setq last (nthcdr (1- num) list))
      (setq tail (if last
                     (cdr last)
                   nil))
      (when last
        (setcdr last nil)))
    tail))

(defun torus--push (elem list)
  "Add ELEM at the beginning of LIST. Return LIST."
  (let* ((duo (cons (car list) (cdr list))))
    (setcar list elem)
    (setcdr list duo))
  list)

(defun torus--push-and-truncate (elem list &optional num)
  "Add ELEM at the beginning of LIST. Truncate LIST to NUM elements.
Return LIST."
  (torus--push elem list)
  (torus--truncate list num)
  list)

(defun torus--pop (list)
  "Remove first element of LIST. Return cons of removed element."
  (let ((value (car list))
        (next (cdr list)))
    (if next
        (progn
          (setcar list (car next))
          (setcdr list (cdr next))
          (setcar next value)
          (setcdr next nil))
      (setq next (cons (car list) nil))
      (setcar list nil))
    next))

(defun torus--update (old new list)
  "Replace OLD by NEW in LIST. Return cons of NEW."
  (let ((duo (torus--member old list)))
    (when duo
      (setcar duo new))
    duo))

;;; Anywhere
;;; ---------------

(defun torus--remove (cons list)
  "Delete CONS from LIST. Return cons of removed element."
  (if (equal cons list)
      (torus--pop list)
    (let* ((previous (torus--previous cons list))
           (duo (cdr previous)))
      (when previous
        (setcdr previous (cdr duo))
        (setcdr duo nil))
      duo)))

(defun torus--delete (elem list)
  "Delete ELEM from LIST. Return cons of removed element."
  (if (equal elem (car list))
      (torus--pop list)
    (let* ((previous (torus--before elem list))
           (duo (cdr previous)))
      (when previous
        (setcdr previous (cdr duo))
        (setcdr duo nil))
      duo)))

(defun torus--insert-after (elem new list)
  "Insert NEW after ELEM in LIST. Return cons of NEW."
  (let* ((member (torus--member elem list))
         (duo))
    (if member
        (progn
          (setq duo (cons new (cdr member)))
          (setcdr member duo)
          duo)
      nil)))

(defun torus--insert-before (elem new list)
  "Insert NEW before ELEM in LIST. Return cons of ELEM."
  (if (equal elem (car list))
      (torus--push new list)
    (let* ((previous (torus--before elem list))
           (duo))
      (if previous
          (progn
            (setq duo (cons new (cdr previous)))
            (setcdr previous duo)
            duo)
        nil))))

(defun torus--move-after (elem moved list)
  "Move MOVED after ELEM in LIST. Return cons of MOVED."
  (unless (equal moved elem)
    (when (torus--delete moved list)
      (torus--insert-after elem moved list))))

(defun torus--move-before (elem moved list)
  "Move MOVED before ELEM in LIST. Return cons of MOVED."
  (unless (equal moved elem)
    (when (torus--delete moved list)
      (torus--insert-before elem moved list))))

;;; Rotate <- ->
;;; ------------------------------

(defun torus--rotate-left (list)
  "Rotate LIST to the left.
Equivalent to pop first element and add it to the end."
  (let ((duo (torus--pop list)))
    (torus--add (car duo) list)))

(defun torus--rotate-right (list)
  "Rotate LIST to the right.
Equivalent to drop last element and push it at the beginning."
  (let ((duo (torus--drop list)))
    (torus--push (car duo) list)))

;;; Assoc
;;; ------------------------------

(defun torus--assoc (key list)
  "Return cons of first element in LIST whose car equals KEY.
Return nil if no matching element is found."
  (let ((duo list))
    (while (and duo
                (not (equal (car (car duo)) key)))
      (setq duo (cdr duo)))
    duo))

(defun torus--reverse-assoc (value list)
  "Return cons of first element in LIST whose cdr equals VALUE.
Return nil if no matching element is found."
  (let ((duo list))
    (while (and duo
                (not (equal (cdr (car duo)) value)))
      (setq duo (cdr duo)))
    duo))

;;; End
;;; ------------------------------------------------------------

(provide 'duo)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; torus.el ends here
