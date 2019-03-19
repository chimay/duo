;;; duo.el --- In place list operations in Elisp     -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Chimay

;; Author : Chimay
;; Name: Duo
;; Package-Version: 1.0
;; Package-requires: ((emacs "26"))
;; Keywords: list, in-place, operation
;; URL: https://github.com/chimay/duo

;;; Commentary:

;; Library of in place list operations in Emacs-Lisp.
;;
;; Cons DUO = (CAR . CDR) can be used as pointer
;; with setcar and setcdr
;;
;; ELEM = (car DUO)
;; DUO = (member ELEM LIST)
;;
;; Caution : apply these functions to circular lists
;; would produce infinite loops.
;;
;; However, some *-circ-* functions simulate circular lists by :
;;   - continuing at the beginning once arrived at the end
;;   - continuing at the end once arrived at the beginning

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

;;; References
;;; ------------------------------------------------------------

(defun torus--set-deref (ptr object)
  "Change the content of the variable referenced by PTR to OBJECT.
OBJECT must be a cons or a list."
  (setcar ptr (car object))
  (setcdr ptr (cdr object))
  ptr)

;;; Lists
;;; ------------------------------------------------------------

;;; Find
;;; ------------------------------

(defun torus--member (elem list &optional test-equal)
  "Return cons of ELEM in LIST or nil if ELEM is not in list.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (let ((duo list)
        (test-equal (if test-equal
                        test-equal
                      #'equal)))
    (while (and duo
                (not (funcall test-equal (car duo) elem)))
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
  "Return cons before CONS in LIST. CONS must reference a cons in list."
  (if (eq cons list)
      nil
    (let ((duo list))
      (while (and duo
                  (not (eq (cdr duo) cons)))
        (setq duo (cdr duo)))
      duo)))

(defun torus--next (cons)
  "Return cons after CONS in list. CONS must reference a cons in the list."
  (cdr cons))

(defun torus--before (elem list &optional test-equal)
  "Return cons before ELEM in LIST.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (let ((test-equal (if test-equal
                        test-equal
                      #'equal)))
    (if (funcall test-equal (car list) elem)
        nil
      (let ((duo list))
        (while (and duo
                    (not (funcall test-equal (car (cdr duo)) elem)))
          (setq duo (cdr duo)))
        duo))))

(defun torus--after (elem list &optional test-equal)
  "Return cons after ELEM in LIST.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (cdr (torus--member elem list test-equal)))

;;; Circular
;;; ---------------

(defun torus--circ-previous (cons list)
  "Return cons before CONS in LIST. CONS must reference a cons in list.
Circular : if in beginning of list, go to the end.
Test with eq."
  (if (eq cons list)
      (torus--last list)
    (let ((duo list))
      (while (and duo
                  (not (eq (cdr duo) cons)))
        (setq duo (cdr duo)))
      duo)))

(defun torus--circ-next (cons list)
  "Return cons after CONS in LIST. CONS must reference a cons in LIST.
Circular : if in end of list, go to the beginning."
  (let ((duo (cdr cons)))
    (if duo
        duo
      list)))

(defun torus--circ-before (elem list &optional test-equal)
  "Return cons before ELEM in LIST.
Circular : if in beginning of list, go to the end.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (let ((test-equal (if test-equal
                        test-equal
                      #'equal)))
    (if (funcall test-equal (car list) elem)
        (torus--last list)
      (let ((duo list))
        (while (and duo
                    (not (funcall test-equal (car (cdr duo)) elem)))
          (setq duo (cdr duo)))
        duo))))

(defun torus--circ-after (elem list &optional test-equal)
  "Return cons after ELEM in LIST.
Circular : if in end of list, go to the beginning.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (torus--circ-next (torus--member elem list test-equal) list))

;;; Add / Change / Remove
;;; ------------------------------

;;; Beginning / End
;;; ---------------

(defun torus--store-beg (cons list)
  "Store CONS at the beginning of LIST. Return LIST."
  (let* ((value (car list)))
    (setcar list (car cons))
    (setcar cons value)
    (setcdr cons (cdr list))
    (setcdr list cons)
    list))

(defun torus--store-end (cons list &optional last)
  "Store CONS at the end of LIST. Return CONS.
If non nil, LAST is used to speed up the process."
  (let ((last (if last
                  last
                (torus--last list))))
    (setcdr last cons)
    (setcdr cons nil)
    cons))

(defun torus--push (elem list)
  "Add ELEM at the beginning of LIST. Return LIST."
  (let* ((duo (cons (car list) (cdr list))))
    (setcar list elem)
    (setcdr list duo))
  list)

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

;;; Anywhere
;;; ---------------

(defun torus--update (old new list)
  "Replace OLD by NEW in LIST. Return cons of NEW."
  (let ((duo (torus--member old list)))
    (when duo
      (setcar duo new))
    duo))

(defun torus--remove (cons list)
  "Delete CONS from LIST. Return cons of removed element.
CONS must reference a cons in LIST."
  (if (eq cons list)
      (torus--pop list)
    (let* ((previous (torus--previous cons list))
           (duo (cdr previous)))
      (when previous
        (setcdr previous (cdr duo))
        (setcdr duo nil))
      duo)))

(defun torus--delete (elem list &optional test-equal)
  "Delete ELEM from LIST. Return cons of removed element.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (let ((test-equal (if test-equal
                        test-equal
                      #'equal)))
    (if (funcall test-equal (car list) elem)
        (torus--pop list)
      (let* ((previous (torus--before elem list test-equal))
             (duo (cdr previous)))
        (when previous
          (setcdr previous (cdr duo))
          (setcdr duo nil))
        duo))))

(defun torus--delete-all (elem list &optional test-equal)
  "Delete all elements equals to ELEM from LIST.
Return list of removed elements.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (let ((duo)
        (next)
        (removed)
        (last)
        (removed-list)
        (test-equal (if test-equal
                        test-equal
                      #'equal)))
    (while (funcall test-equal (car list) elem)
      ;; Pop case
      (setq removed (torus--remove list list))
      (if removed-list
          (setq last (torus--store-end removed removed-list last))
        (setq last removed)
        (setq removed-list removed)))
    (setq duo list)
    (while duo
      (setq next (cdr duo))
      (when (funcall test-equal (car duo) elem)
        (setq removed (torus--remove duo list))
        (if removed-list
            (setq last (torus--store-end removed removed-list last))
          (setq last removed)
          (setq removed-list removed)))
      (setq duo next))
    removed-list))

(defun torus--insert-next (cons new)
  "Insert NEW after CONS in LIST. Return cons of NEW.
CONS must reference a cons in LIST."
  (let ((duo (cons new (cdr cons))))
    (setcdr cons duo)
    duo))

(defun torus--insert-previous (cons new list)
  "Insert NEW before CONS in LIST. Return cons of NEW.
CONS must reference a cons in LIST."
  (if (eq cons list)
      (torus--push new list)
    (let* ((previous (torus--previous cons list))
           (duo))
      (if previous
          (progn
            (setq duo (cons new (cdr previous)))
            (setcdr previous duo)
            duo)
        nil))))

(defun torus--insert-after (elem new list &optional test-equal)
  "Insert NEW after ELEM in LIST. Return cons of NEW.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (let* ((member (torus--member elem list test-equal))
         (duo))
    (if member
        (progn
          (setq duo (cons new (cdr member)))
          (setcdr member duo)
          duo)
      nil)))

(defun torus--insert-before (elem new list &optional test-equal)
  "Insert NEW before ELEM in LIST. Return cons of NEW.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (let ((test-equal (if test-equal
                        test-equal
                      #'equal)))
    (if (funcall test-equal (car list) elem)
        (torus--push new list)
      (let* ((previous (torus--before elem list test-equal))
             (duo))
        (if previous
            (progn
              (setq duo (cons new (cdr previous)))
              (setcdr previous duo)
              duo)
          nil)))))

(defun torus--move-after (elem moved list &optional test-equal)
  "Move MOVED after ELEM in LIST. Return cons of MOVED.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (let ((test-equal (if test-equal
                        test-equal
                      #'equal)))
    (unless (funcall test-equal moved elem)
      (when (torus--delete moved list test-equal)
        (torus--insert-after elem moved list test-equal)))))

(defun torus--move-before (elem moved list &optional test-equal)
  "Move MOVED before ELEM in LIST. Return cons of MOVED.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (let ((test-equal (if test-equal
                        test-equal
                      #'equal)))
    (unless (funcall test-equal moved elem)
      (when (torus--delete moved list test-equal)
        (torus--insert-before elem moved list test-equal)))))

;;; Rotate <- ->
;;; ------------------------------

(defun torus--rotate-left (list)
  "Rotate LIST to the left.
Equivalent to pop first element and add it to the end."
  ;; Length list > 1
  (when (cdr list)
    (let ((duo (torus--pop list)))
      (torus--add (car duo) list))))

(defun torus--rotate-right (list)
  "Rotate LIST to the right.
Equivalent to drop last element and push it at the beginning."
  ;; Length list > 1
  (when (cdr list)
    (let ((duo (torus--drop list)))
      (torus--push (car duo) list))))

;;; Group
;;; ------------------------------

(defun torus--insert-at-group-beg (new list &optional test-group)
  "Insert NEW in LIST, at the beginning of a group determined by TEST-GROUP.
TEST-GROUP takes two arguments and returns t if they belongs to the same group.
TEST-GROUP defaults do `equal'."
  (torus--insert-before new new list test-group))

(defun torus--insert-at-group-end (new list &optional test-group)
  "Insert NEW in LIST, at the end of a group determined by TEST-GROUP.
TEST-GROUP takes two arguments and returns t if they belongs to the same group.
TEST-GROUP defaults do `equal'."
  (let ((previous (torus--member new list test-group)))
    (while (and previous
                (funcall test-group (car (cdr previous)) new))
      (setq previous (cdr previous)))
    (when previous
      (torus--insert-next previous new))))

;;; Filter
;;; ------------------------------

(defun torus--filter (test-filter list)
  "Return list of references to elements of LIST matching TEST-FILTER.
TEST-FILTER takes one argument and return t if it must belong
to the list of references."
  (let ((duo list)
        (new)
        (last)
        (filtered))
    (while duo
      (when (funcall test-filter (car duo))
        (setq new (cons (car duo) nil))
        (if filtered
            (setcdr last new)
          (setq filtered new))
        (setq last new))
      (setq duo (cdr duo)))
    filtered))

(defun torus--filter-previous (test-filter cons list)
  "Return reference of previous element of CONS in LIST matching TEST-FILTER."
  (let ((duo list)
        (found)
        (previous))
    (while (and duo
                (not found))
      (if (eq duo cons)
          (setq found t)
        (when (funcall test-filter (car duo))
          (setq previous duo))
        (setq duo (cdr duo))))
    (if found
        previous
      nil)))

(defun torus--filter-next (test-filter cons)
  "Return reference of next element of CONS in list matching TEST-FILTER."
  (let ((next (cdr cons))
        (found))
    (while (and next
                (not found))
      (if (funcall test-filter (car next))
          (setq found t)
        (setq next (cdr next))))
    next))

(defun torus--filter-before (test-filter elem list &optional test-equal)
  "Return reference of element before ELEM in LIST matching TEST-FILTER.
TEST-EQUAL tests equality of two elements, defaults to `equal'."
  (let ((duo list)
        (found)
        (previous)
        (test-equal (if test-equal
                        test-equal
                      #'equal)))
    (while (and duo
                (not found))
      (if (funcall test-equal (car duo) elem)
          (setq found t)
        (when (funcall test-filter (car duo))
          (setq previous duo))
        (setq duo (cdr duo))))
    (if found
        previous
      nil)))

(defun torus--filter-after (test-filter elem list &optional test-equal)
  "Return reference of element after ELEM in LIST matching TEST-FILTER.
TEST-EQUAL tests equality of two elements, defaults to `equal'."
  (let ((next (torus--after elem list test-equal))
        (found))
    (while (and next
                (not found))
      (if (funcall test-filter (car next))
          (setq found t)
        (setq next (cdr next))))
    next))

;;; Assoc
;;; ------------------------------------------------------------

;;; Find
;;; ------------------------------

(defun torus--assoc (key list &optional test-equal)
  "Return cons of first element in LIST whose car equals KEY.
TEST-EQUAL takes two arguments and return t if they are considered equals.
Return nil if no matching element is found."
  (let ((duo list)
        (test-equal (if test-equal
                        test-equal
                      #'equal)))
    (while (and duo
                (not (funcall test-equal (car (car duo)) key)))
      (setq duo (cdr duo)))
    duo))

(defun torus--reverse-assoc (value list &optional test-equal)
  "Return cons of first element in LIST whose cdr equals VALUE.
TEST-EQUAL takes two arguments and return t if they are considered equals.
Return nil if no matching element is found."
  (let ((duo list)
        (test-equal (if test-equal
                        test-equal
                      #'equal)))
    (while (and duo
                (not (funcall test-equal (cdr (car duo)) value)))
      (setq duo (cdr duo)))
    duo))

;;; End
;;; ------------------------------------------------------------

(provide 'duo)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; duo.el ends here
