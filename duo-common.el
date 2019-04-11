;;; duo-common.el --- Common part of duo             -*- lexical-binding: t; -*-

;;; Commentary:

;; Common part of duo

;; Copyright (C) 2019 Chimay

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

;;; Equality
;;; ------------------------------------------------------------

;;; Level 0
;;; ------------------------------

(defun duo-equal-p (one two)
  "Whether cons ONE and TWO are equal.
Return nil if ONE and TWO are distincts or not cons."
  (when (consp one)
    (equal one two)))

;;; Level 1
;;; ------------------------------

(defun duo-equal-car-p (one two)
  "Whether the car of ONE and TWO are equal."
  (when (and (consp one)
             (consp two))
    (equal (car one) (car two))))

(defun duo-equal-cdr-p (one two)
  "Whether the cdr of ONE and TWO are equal."
  (when (and (consp one)
             (consp two))
    (equal (cdr one) (cdr two))))

;;; Level 2
;;; ------------------------------

(defun duo-equal-caar-p (one two)
  "Whether the caar of ONE and TWO are equal."
  (when (and (consp one)
             (consp two))
    (equal (caar one) (caar two))))

(defun duo-equal-cdar-p (one two)
  "Whether the cdar of ONE and TWO are equal."
  (when (and (consp one)
             (consp two))
    (equal (cdar one) (cdar two))))

(defun duo-equal-cadr-p (one two)
  "Whether the cadr of ONE and TWO are equal."
  (when (and (consp one)
             (consp two))
    (equal (cadr one) (cadr two))))

(defun duo-equal-cddr-p (one two)
  "Whether the cddr of ONE and TWO are equal."
  (when (and (consp one)
             (consp two))
    (equal (cddr one) (cddr two))))

;;; Match
;;; ------------------------------------------------------------

;;; Level 1
;;; ------------------------------

(defun duo-car-match-x-p (one two)
  "Whether the car of ONE equals TWO."
  (equal (car one) two))

(defun duo-x-match-car-p (one two)
  "Whether ONE equals the car of TWO."
  (equal one (car two)))

(defun duo-cdr-match-x-p (one two)
  "Whether the cdr of ONE equals TWO."
  (equal (cdr one) two))

(defun duo-x-match-cdr-p (one two)
  "Whether ONE equals the cdr of TWO."
  (equal one (cdr two)))

;;; Level 2
;;; ------------------------------

(defun duo-caar-match-x-p (one two)
  "Whether the caar of ONE equals TWO."
  (equal (car (car one)) two))

(defun duo-x-match-caar-p (one two)
  "Whether ONE equals the caar of TWO."
  (equal one (car (car two))))

(defun duo-cdar-match-x-p (one two)
  "Whether the cdar of ONE equals TWO."
  (equal (cdr (car one)) two))

(defun duo-x-match-cdar-p (one two)
  "Whether ONE equals the cdar of TWO."
  (equal one (cdr (car two))))

(defun duo-cadr-match-x-p (one two)
  "Whether the cadr of ONE equals TWO."
  (equal (car (cdr one)) two))

(defun duo-x-match-cadr-p (one two)
  "Whether ONE equals the cadr of TWO."
  (equal one (car (cdr two))))

(defun duo-cddr-match-x-p (one two)
  "Whether the cddr of ONE equals TWO."
  (equal (cdr (cdr one)) two))

(defun duo-x-match-cddr-p (one two)
  "Whether ONE equals the cddr of TWO."
  (equal one (cdr (cdr two))))

;;; Order
;;; ------------------------------------------------------------

(defun duo-< (one two)
  "Return t if ONE is less than TWO."
  (cond ((and (number-or-marker-p one)
              (number-or-marker-p two))
         (< one two))
        ((and (or (stringp one) (symbolp one))
              (or (stringp two) (symbolp two)))
         (string< one two))
        ((and (consp one)
              (consp two))
         (let ((car-one (car one))
               (car-two (car two))
               (cdr-one (cdr one))
               (cdr-two (cdr two)))
           (cond ((and (null cdr-one)
                       (null cdr-two))
                  (duo-< car-one car-two))
                 ((null cdr-one) t)
                 ((null cdr-two) nil)
                 (t (or (duo-< car-one car-two)
                        (and (equal car-one car-two)
                             (duo-< cdr-one cdr-two)))))))
        (t (error "Function duo-< : wrong type argument"))))

(defun duo-<= (one two)
  "Return t if ONE is less or equal to TWO."
  (cond ((and (number-or-marker-p one)
              (number-or-marker-p two))
         (<= one two))
        ((and (or (stringp one) (symbolp one))
              (or (stringp two) (symbolp two)))
         (or (equal one two)
             (string< one two)))
        ((and (consp one)
              (consp two))
         (let ((car-one (car one))
               (car-two (car two))
               (cdr-one (cdr one))
               (cdr-two (cdr two)))
           (cond ((and (null cdr-one)
                       (null cdr-two))
                  (duo-<= car-one car-two))
                 ((null cdr-one) t)
                 ((null cdr-two) nil)
                 (t (or (duo-< car-one car-two)
                        (and (equal car-one car-two)
                             (duo-<= cdr-one cdr-two)))))))
        (t (error "Function duo-<= : wrong type argument"))))

;;; First & Last
;;; ------------------------------------------------------------

;; Keep this in mind

(defun duo-first (list)
  "Return first cons of LIST, ie the list itself."
  list)

;; Just for fun

(defun duo-last (list &optional num)
  "Return cons starting a sublist of NUM elements at the end of LIST.
If NUM exceeds the length of LIST, return LIST.
NUM defaults to 1 : NUM nil means return cons of last element in LIST."
  (let ((num (or num 1))
        (last list))
    (while (nthcdr num last)
      (setq last (cdr last)))
    last))

;;; Element & Cons
;;; ------------------------------------------------------------

(defun duo-at-index (index list)
  "Cons at INDEX in LIST."
  (let ((position (if (> index 0)
                      index
                    (+ (length list) index))))
    (nthcdr position list)))

(defun duo-inside (cons list)
  "Return CONS if CONS is in LIST or nil otherwise."
  (let ((duo list))
    (while (and duo
                (not (eq duo cons)))
      (setq duo (cdr duo)))
    duo))

(defun duo-member (elem list &optional fn-equal)
  "Return cons of ELEM in LIST or nil if ELEM is not in list.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let ((duo list)
        (fn-equal (or fn-equal #'equal)))
    (while (and duo
                (not (funcall fn-equal (car duo) elem)))
      (setq duo (cdr duo)))
    duo))

;;; Range
;;; ------------------------------------------------------------

(defun duo-range (list index-one &optional index-two)
  "Return start and end of range from INDEX-ONE to INDEX-TWO in LIST.
Cons of INDEX-ONE and INDEX-TWO are included in the range.
Return first and last cons of range."
  (let* ((length (length list))
         (index-two (or index-two length))
         (pos-one (if (> index-one 0)
                      index-one
                    (+ length index-one)))
         (pos-two (if (> index-two 0)
                      index-two
                    (+ length index-two)))
         (min (min pos-one pos-two))
         (max (max pos-one pos-two))
         (delta (- max min))
         (duo-one (nthcdr min list))
         (duo-two (nthcdr delta duo-one)))
    (cons duo-one duo-two)))

(defun duo-slice (list index-one &optional index-two)
  "Return new list formed with range from INDEX-ONE to INDEX-TWO in LIST.
INDEX-ONE is included in the range, whereas INDEX-TWO is excluded."
  (let* ((pair (duo-range list index-one index-two))
         (duo (car pair))
         (end (cdr pair))
         (new (cons (car duo) nil))
         (newlist new)
         (last new))
    (unless (eq duo end)
      (while (not (eq (cdr duo) end))
        (setq duo (cdr duo))
        (setq new (cons (car duo) nil))
        (setcdr last new)
        (setq last new)))
    newlist))

;;; Position
;;; ------------------------------------------------------------

(defun duo-index-of-cons (cons list)
  "Return index of CONS in LIST or nil if not present."
  (let ((duo list)
        (index 0))
    (while (and duo
                (not (eq duo cons)))
      (setq duo (cdr duo))
      (setq index (1+ index)))
    (if duo
        index
      nil)))

(defun duo-index-of (elem list &optional fn-equal)
  "Return index of ELEM in LIST or nil if not present.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let ((duo list)
        (fn-equal (or fn-equal #'equal))
        (index 0))
    (while (and duo
                (not (funcall fn-equal (car duo) elem)))
      (setq duo (cdr duo))
      (setq index (1+ index)))
    (if duo
        index
      nil)))

(defun duo-index-member (elem list &optional fn-equal)
  "Return (index . cons) of ELEM in LIST or nil if not present.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let ((duo list)
        (fn-equal (or fn-equal #'equal))
        (index 0))
    (while (and duo
                (not (funcall fn-equal (car duo) elem)))
      (setq duo (cdr duo))
      (setq index (1+ index)))
    (if duo
        (cons index duo)
      nil)))

;;; Assoc
;;; ------------------------------------------------------------

(defun duo-assoc (key list &optional fn-equal)
  "Return cons of first element in LIST whose car equals KEY.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Return nil if no matching element is found."
  (let ((duo list)
        (fn-equal (or fn-equal #'equal)))
    (while (and duo
                (not (funcall fn-equal (car (car duo)) key)))
      (setq duo (cdr duo)))
    duo))

(defun duo-reverse-assoc (value list &optional fn-equal)
  "Return cons of first element in LIST whose cdr equals VALUE.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Return nil if no matching element is found."
  (let ((duo list)
        (fn-equal (or fn-equal #'equal)))
    (while (and duo
                (not (funcall fn-equal (cdr (car duo)) value)))
      (setq duo (cdr duo)))
    duo))

;;; Next / Previous
;;; ------------------------------------------------------------

(defun duo-previous (cons list &optional num)
  "Return cons of NUM elements before CONS in LIST.
NUM defaults to 1.
CONS must be a cons in LIST."
  (let* ((num (or num 1))
         (duo list)
         (scout (nthcdr num duo)))
    (while (and scout
                (not (eq scout cons))
                (not (eq duo cons)))
      (setq duo (cdr duo))
      (setq scout (cdr scout)))
    (if (eq scout cons)
        duo
      nil)))

(defun duo-next (cons &optional num)
  "Return cons of NUM elements after CONS in list.
NUM defaults to 1.
CONS must be a cons in the list."
  (let ((num (or num 1)))
    (nthcdr num cons)))

(defun duo-before (elem list &optional num fn-equal)
  "Return cons of NUM elements before ELEM in LIST.
NUM defaults to 1.
ELEM must be present in list.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let* ((num (or num 1))
         (fn-equal (or fn-equal #'equal))
         (duo list)
         (scout duo)
         (iter 0))
    ;; We want the cons before the _first_ occurrence
    (while (< iter num)
      (if (funcall fn-equal (car scout) elem)
          (progn
            (setq duo nil)
            (setq scout nil)
            (setq iter num))
        (setq scout (cdr scout))
        (setq iter (1+ iter))))
    ;; If it’ok, we go on
    (while (and scout
                (not (funcall fn-equal (car scout) elem))
                (not (funcall fn-equal (car duo) elem)))
      (setq duo (cdr duo))
      (setq scout (cdr scout)))
    (if (and scout
             (funcall fn-equal (car scout) elem))
        duo
      nil)))

(defun duo-after (elem list &optional num fn-equal)
  "Return cons of NUM elements after ELEM in LIST.
NUM defaults to 1.
ELEM must be present in list.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let ((num (or num 1)))
    (nthcdr num (duo-member elem list fn-equal))))

;;; Circular
;;; ------------------------------

(defun duo-circ-previous (cons list &optional num)
  "Return cons of NUM elements before CONS in LIST.
Circular : if in beginning of list, go to the end.
NUM defaults to 1.
CONS must be a cons in LIST.
Test with eq."
  (let* ((num (or num 1))
         (duo list)
         (scout (nthcdr num duo))
         (iter 0))
    (unless scout
      (setq num (mod num (length list)))
      (setq scout (nthcdr num duo)))
    (while (and duo
                (not (eq scout cons))
                (not (eq duo cons)))
      (setq duo (cdr duo))
      (setq scout (cdr scout))
      (setq iter (1+ iter)))
    (unless (eq scout cons)
      (setq duo list)
      (setq scout (nthcdr (- num iter) duo))
      (while (and duo
                  scout)
        (setq duo (cdr duo))
        (setq scout (cdr scout))))
    duo))

(defun duo-circ-next (cons list &optional num)
  "Return cons of NUM elements after CONS in LIST.
Circular : if in end of list, go to the beginning.
NUM defaults to 1.
CONS must be a cons in LIST."
  (let ((num (or num 1))
        (duo cons)
        (iter 0))
    (while (and duo
                (< iter num))
      (setq duo (cdr duo))
      (setq iter (1+ iter)))
    (unless duo
      (setq duo (nthcdr (- num iter) list)))
    (unless duo
      (setq num (mod (- num iter) (length list)))
      (setq duo (nthcdr num list)))
    duo))

(defun duo-circ-before (elem list &optional num fn-equal)
  "Return cons of NUM elements before ELEM in LIST.
Circular : if in beginning of list, go to the end.
NUM defaults to 1.
ELEM must be present in list.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let* ((num (or num 1))
         (fn-equal (or fn-equal #'equal))
         (duo list)
         (scout duo)
         (iter 0))
    ;; We want the cons before the _first_ occurrence
    (while (and scout
                (< iter num))
      (if (funcall fn-equal (car scout) elem)
          (progn
            (setq duo nil)
            (setq scout nil))
        (setq scout (cdr scout))
        (setq iter (1+ iter))))
    ;; If it’ok, we go on
    (when duo
      (setq iter 0)
      (when (not scout)
        (setq num (mod num (length list)))
        (setq scout (nthcdr num duo)))
      (while (and duo
                  (not (funcall fn-equal (car scout) elem))
                  (not (funcall fn-equal (car duo) elem)))
        (setq duo (cdr duo))
        (setq scout (cdr scout))
        (setq iter (1+ iter))))
    (unless (and scout
                 (funcall fn-equal (car scout) elem))
      (setq duo list)
      (setq scout (nthcdr (- num iter) duo))
      (while (and duo
                  scout)
        (setq duo (cdr duo))
        (setq scout (cdr scout))))
    duo))

(defun duo-circ-after (elem list &optional num fn-equal)
  "Return cons of NUM elements after ELEM in LIST.
Circular : if in end of list, go to the beginning.
NUM defaults to 1.
ELEM must be present in list.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (duo-circ-next (duo-member elem list fn-equal) list num))

;;; Replace
;;; ------------------------------------------------------------

(defun duo-replace (old new list &optional fn-equal)
  "Replace OLD by NEW in LIST. Return cons of NEW.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((duo (duo-member old list fn-equal)))
    (when duo
      (setcar duo new))
    duo))

;;; Car & Cdr
;;; ------------------------------

;;; Level 1
;;; ---------------

(defun duo-replace-car (old new list &optional fn-equal)
  "Replace OLD car by NEW in LIST. Return cons of NEW.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((duo list)
        (fn-equal (or fn-equal #'equal)))
    (while (and duo
                (not (funcall fn-equal (car (car duo)) old)))
      (setq duo (cdr duo)))
    (when duo
      (setcar (car duo) new))
    duo))

(defun duo-replace-cdr (old new list &optional fn-equal)
  "Replace OLD cdr by NEW in LIST. Return cons of NEW.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((duo list)
        (fn-equal (or fn-equal #'equal)))
    (while (and duo
                (not (funcall fn-equal (cdr (car duo)) old)))
      (setq duo (cdr duo)))
    (when duo
      (setcdr (car duo) new))
    duo))

;;; Level 2
;;; ---------------

(defun duo-replace-caar (old new list &optional fn-equal)
  "Replace OLD caar by NEW in LIST. Return cons of NEW.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((duo list)
        (fn-equal (or fn-equal #'equal)))
    (while (and duo
                (not (funcall fn-equal (car (car (car duo))) old)))
      (setq duo (cdr duo)))
    (when duo
      (setcar (car (car duo)) new))
    duo))

(defun duo-replace-cdar (old new list &optional fn-equal)
  "Replace OLD cdar by NEW in LIST. Return cons of NEW.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((duo list)
        (fn-equal (or fn-equal #'equal)))
    (while (and duo
                (not (funcall fn-equal (cdr (car (car duo))) old)))
      (setq duo (cdr duo)))
    (when duo
      (setcdr (car (car duo)) new))
    duo))

(defun duo-replace-cadr (old new list &optional fn-equal)
  "Replace OLD cadr by NEW in LIST. Return cons of NEW.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((duo list)
        (fn-equal (or fn-equal #'equal)))
    (while (and duo
                (not (funcall fn-equal (car (cdr (car duo))) old)))
      (setq duo (cdr duo)))
    (when duo
      (setcar (cdr (car duo)) new))
    duo))

(defun duo-replace-cddr (old new list &optional fn-equal)
  "Replace OLD cddr by NEW in LIST. Return cons of NEW.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((duo list)
        (fn-equal (or fn-equal #'equal)))
    (while (and duo
                (not (funcall fn-equal (cdr (cdr (car duo))) old)))
      (setq duo (cdr duo)))
    (when duo
      (setcdr (cdr (car duo)) new))
    duo))

;;; All
;;; ------------------------------

(defun duo-replace-all (old new list &optional fn-equal)
  "Replace all occurences of OLD by NEW in LIST.
Return number of elements replaced.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((fn-equal (or fn-equal #'equal))
        (duo list)
        (num 0))
    (while duo
      (when (funcall fn-equal (car duo) old)
        (setcar duo new)
        (setq num (1+ num)))
      (setq duo (cdr duo)))
    num))

;;; Level 1
;;; ---------------

(defun duo-replace-all-car (old new list &optional fn-equal)
  "Replace all occurences of OLD car by NEW in LIST.
Return number of elements replaced.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((fn-equal (or fn-equal #'equal))
        (duo list)
        (num 0))
    (while duo
      (when (funcall fn-equal (car (car duo)) old)
        (setcar (car duo) new)
        (setq num (1+ num)))
      (setq duo (cdr duo)))
    num))

(defun duo-replace-all-cdr (old new list &optional fn-equal)
  "Replace all occurences of OLD cdr by NEW in LIST.
Return number of elements replaced.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((fn-equal (or fn-equal #'equal))
        (duo list)
        (num 0))
    (while duo
      (when (funcall fn-equal (cdr (car duo)) old)
        (setcdr (car duo) new)
        (setq num (1+ num)))
      (setq duo (cdr duo)))
    num))

;;; Level 2
;;; ---------------

(defun duo-replace-all-caar (old new list &optional fn-equal)
  "Replace all occurences of OLD caar by NEW in LIST.
Return number of elements replaced.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((fn-equal (or fn-equal #'equal))
        (duo list)
        (num 0))
    (while duo
      (when (funcall fn-equal (car (car (car duo))) old)
        (setcar (car (car duo)) new)
        (setq num (1+ num)))
      (setq duo (cdr duo)))
    num))

(defun duo-replace-all-cdar (old new list &optional fn-equal)
  "Replace all occurences of OLD cdar by NEW in LIST.
Return number of elements replaced.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((fn-equal (or fn-equal #'equal))
        (duo list)
        (num 0))
    (while duo
      (when (funcall fn-equal (cdr (car (car duo))) old)
        (setcdr (car (car duo)) new)
        (setq num (1+ num)))
      (setq duo (cdr duo)))
    num))

(defun duo-replace-all-cadr (old new list &optional fn-equal)
  "Replace all occurences of OLD cadr by NEW in LIST.
Return number of elements replaced.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((fn-equal (or fn-equal #'equal))
        (duo list)
        (num 0))
    (while duo
      (when (funcall fn-equal (car (cdr (car duo))) old)
        (setcar (cdr (car duo)) new)
        (setq num (1+ num)))
      (setq duo (cdr duo)))
    num))

(defun duo-replace-all-cddr (old new list &optional fn-equal)
  "Replace all occurences of OLD cddr by NEW in LIST.
Return number of elements replaced.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((fn-equal (or fn-equal #'equal))
        (duo list)
        (num 0))
    (while duo
      (when (funcall fn-equal (cdr (cdr (car duo))) old)
        (setcdr (cdr (car duo)) new)
        (setq num (1+ num)))
      (setq duo (cdr duo)))
    num))

;;; Map
;;; ------------------------------------------------------------

(defun duo-map (list fn-map)
  "Replace each element of LIST by (FN-MAP element). Return LIST.
Destructive."
  (let ((duo list))
    (while duo
      (setcar duo (funcall fn-map (car duo)))
      (setq duo (cdr duo))))
  list)

;;; Assoc
;;; ------------------------------

(defun duo-assoc-map (list fn-map)
  "Replace each element of LIST by (element . (FN-MAP element)).
Return assoc LIST.
Destructive."
  (let ((duo list)
        (elem))
    (while duo
      (setq elem (car duo))
      (setcar duo (cons elem (funcall fn-map elem)))
      (setq duo (cdr duo))))
  list)

;;; Join
;;; ------------------------------------------------------------

(defun duo-join (&rest arguments)
  "Join lists given in ARGUMENTS.
Destructive."
  (let* ((arg arguments)
         (list (car arg))
         (last (duo-last list))
         (next))
    (while arg
      (setq next (car (cdr arg)))
      (unless (duo-inside next list)
        (setcdr last next)
        (while (cdr last)
          (setq last (cdr last))))
      (setq arg (cdr arg)))
    list))

;;; Truncate
;;; ------------------------------------------------------------

(defun duo-truncate (list &optional num)
  "Truncate LIST to its first NUM elements. Return removed part.
If NUM is nil, do nothing.
Destructive."
  (let* ((num (or num nil))
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

;;; Group
;;; ------------------------------------------------------------

;;; Next / Previous
;;; ------------------------------

(defun duo-previous-in-group (cons list &optional fn-group)
  "Return cons of previous element of CONS in LIST matching FN-GROUP.
The result and CONS are in the same group : (FN-GROUP CONS result) = t.
FN-GROUP takes two arguments and returns t if they belongs to the same group.
FN-GROUP defaults to `equal'."
  (let ((duo list)
        (previous))
    (while (and duo
                (not (eq duo cons)))
      (when (funcall fn-group (car duo) (car cons))
        (setq previous duo))
      (setq duo (cdr duo)))
    previous))

(defun duo-next-in-group (cons &optional fn-group)
  "Return cons of next element of CONS in list matching FN-GROUP.
The result and CONS are in the same group : (FN-GROUP CONS result) = t.
FN-GROUP takes two arguments and returns t if they belongs to the same group.
FN-GROUP defaults to `equal'."
  (let ((next (cdr cons)))
    (while (and next
                (not (funcall fn-group (car next) (car cons))))
      (setq next (cdr next)))
    next))

(defun duo-before-in-group (elem list &optional fn-group fn-equal)
  "Return cons of element before ELEM in LIST matching FN-GROUP.
The result and CONS are in the same group : (FN-GROUP CONS result) = t.
FN-GROUP takes two arguments and returns t if they belongs to the same group.
FN-GROUP defaults to `equal'.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let ((duo (duo-member elem list fn-equal)))
    (duo-previous-in-group duo list fn-group)))

(defun duo-after-in-group (elem list &optional fn-group fn-equal)
  "Return cons of element after ELEM in LIST matching FN-GROUP.
The result and CONS are in the same group : (FN-GROUP CONS result) = t.
FN-GROUP takes two arguments and returns t if they belongs to the same group.
FN-GROUP defaults to `equal'.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let ((duo (duo-member elem list fn-equal)))
    (duo-next-in-group duo fn-group)))

;;; Circular
;;; ------------------------------

(defun duo-circ-previous-in-group (cons list &optional fn-group)
  "Return cons of previous element of CONS in LIST matching FN-GROUP.
The result and CONS are in the same group : (FN-GROUP CONS result) = t.
FN-GROUP takes two arguments and returns t if they belongs to the same group.
FN-GROUP defaults to `equal'."
  (let ((duo list)
        (previous))
    (while (and duo
                (not (eq duo cons)))
      (when (funcall fn-group (car duo) (car cons))
        (setq previous duo))
      (setq duo (cdr duo)))
    (unless previous
      (setq duo (cdr duo))
      (while duo
        (when (funcall fn-group (car duo) (car cons))
          (setq previous duo))
        (setq duo (cdr duo))))
    previous))

(defun duo-circ-next-in-group (cons list &optional fn-group)
  "Return cons of next element of CONS in LIST matching FN-GROUP.
The result and CONS are in the same group : (FN-GROUP CONS result) = t.
FN-GROUP takes two arguments and returns t if they belongs to the same group.
FN-GROUP defaults to `equal'."
  (let ((next (cdr cons)))
    (while (and next
                (not (funcall fn-group (car next) (car cons))))
      (setq next (cdr next)))
    (unless next
      (setq next list)
      (while (and next
                  (not (eq next cons))
                  (not (funcall fn-group (car next) (car cons))))
        (setq next (cdr next))))
    next))

(defun duo-circ-before-in-group (elem list &optional fn-group fn-equal)
  "Return cons of element before ELEM in LIST matching FN-GROUP.
The result and CONS are in the same group : (FN-GROUP CONS result) = t.
FN-GROUP takes two arguments and returns t if they belongs to the same group.
FN-GROUP defaults to `equal'.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let ((duo (duo-member elem list fn-equal)))
    (duo-circ-previous-in-group duo list fn-group)))

(defun duo-circ-after-in-group (elem list &optional fn-group fn-equal)
  "Return cons of element after ELEM in LIST matching FN-GROUP.
The result and CONS are in the same group : (FN-GROUP CONS result) = t.
FN-GROUP takes two arguments and returns t if they belongs to the same group.
FN-GROUP defaults to `equal'.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let ((duo (duo-member elem list fn-equal)))
    (duo-circ-next-in-group duo list fn-group)))

;;; Filter
;;; ------------------------------------------------------------

(defun duo-filter (list &optional fn-filter)
  "Return list of elements in LIST matching FN-FILTER.
FN-FILTER takes one argument and return t if the element passes the filter.
FN-FILTER defaults to `identity'.
LIST is not modified."
  (let ((fn-filter (or fn-filter #'identity))
        (duo list)
        (new)
        (last)
        (filtered))
    (while duo
      (when (funcall fn-filter (car duo))
        (setq new (cons (car duo) nil))
        (if filtered
            (setcdr last new)
          (setq filtered new))
        (setq last new))
      (setq duo (cdr duo)))
    filtered))

;;; Next / Previous
;;; ------------------------------

(defun duo-filter-previous (cons list &optional fn-filter)
  "Return cons of previous element of CONS in LIST matching FN-FILTER.
FN-FILTER takes one argument and return t if the element passes the filter."
  (let ((fn-filter (or fn-filter #'identity))
        (duo list)
        (previous))
    (while (and duo
                (not (eq duo cons)))
      (when (funcall fn-filter (car duo))
        (setq previous duo))
      (setq duo (cdr duo)))
    previous))

(defun duo-filter-next (cons &optional fn-filter)
  "Return cons of next element of CONS in list matching FN-FILTER.
FN-FILTER takes one argument and return t if the element passes the filter."
  (let ((fn-filter (or fn-filter #'identity))
        (next (cdr cons)))
    (while (and next
                (not (funcall fn-filter (car next))))
      (setq next (cdr next)))
    next))

(defun duo-filter-before (elem list &optional fn-filter fn-equal)
  "Return cons of element before ELEM in LIST matching FN-FILTER.
FN-FILTER takes one argument and return t if the element passes the filter.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let ((fn-filter (or fn-filter #'identity))
        (duo (duo-member elem list fn-equal)))
    (duo-filter-previous duo list fn-filter)))

(defun duo-filter-after (elem list &optional fn-filter fn-equal)
  "Return cons of element after ELEM in LIST matching FN-FILTER.
FN-FILTER takes one argument and return t if the element passes the filter.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let ((fn-filter (or fn-filter #'identity))
        (duo (duo-member elem list fn-equal)))
    (duo-filter-next duo fn-filter)))

;;; Circular
;;; ------------------------------

(defun duo-circ-filter-previous (cons list &optional fn-filter)
  "Return cons of previous element of CONS in LIST matching FN-FILTER.
FN-FILTER takes one argument and return t if the element passes the filter."
  (let ((fn-filter (or fn-filter #'identity))
        (duo list)
        (previous))
    (while (and duo
                (not (eq duo cons)))
      (when (funcall fn-filter (car duo))
        (setq previous duo))
      (setq duo (cdr duo)))
    (unless previous
      (setq duo (cdr duo))
      (while duo
        (when (funcall fn-filter (car duo))
          (setq previous duo))
        (setq duo (cdr duo))))
    previous))

(defun duo-circ-filter-next (cons list &optional fn-filter)
  "Return cons of next element of CONS in LIST matching FN-FILTER.
FN-FILTER takes one argument and return t if the element passes the filter."
  (let ((fn-filter (or fn-filter #'identity))
        (next (cdr cons)))
    (while (and next
                (not (funcall fn-filter (car next))))
      (setq next (cdr next)))
    (unless next
      (setq next list)
      (while (and next
                  (not (eq next cons))
                  (not (funcall fn-filter (car next))))
        (setq next (cdr next))))
    next))

(defun duo-circ-filter-before (elem list &optional fn-filter fn-equal)
  "Return cons of element before ELEM in LIST matching FN-FILTER.
FN-FILTER takes one argument and return t if the element passes the filter.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let ((fn-filter (or fn-filter #'identity))
        (duo (duo-member elem list fn-equal)))
    (duo-circ-filter-previous duo list fn-filter)))

(defun duo-circ-filter-after (elem list &optional fn-filter fn-equal)
  "Return cons of element after ELEM in LIST matching FN-FILTER.
FN-FILTER takes one argument and return t if the element passes the filter.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let ((fn-filter (or fn-filter #'identity))
        (duo (duo-member elem list fn-equal)))
    (duo-circ-filter-next duo list fn-filter)))

;;; Stack & Queue
;;; ------------------------------------------------------------

(defun duo-naive-push ()
  "Do not use it on any LIST !
Adding a cons before the first one without recovering the updated list
does not work.
The calling scope list var holds the address of the first cons of the list.
The argument list var holds a copy of this address.
Using (setq list ...) inside the defun changes the argument list reference,
not the calling scope one. So, the calling scope address remains the same,
which becomes the address of the second cons of the list.
Some problem may also arise when you push the first element to an emtpy list.
There are two solutions :
- Recover the list in the returned structure
- Pass a one element list containing the list as argument (*-ref-* functions)"
;; (let* ((newlist))
;;     (setcdr cons list)
;;     (setq newlist cons)
;;     nil)
  )

(defun duo-naive-pop ()
  "Do not use it on any LIST !
Removing the first cons without returning the updated list does not work.
The calling scope list var holds the address of the first cons of the list.
The argument list var holds a copy of this address.
Using (setq list ...) inside the defun changes the argument list reference,
not the calling scope one. So, the calling scope address remains the same,
which becomes the address of the removed cons.
Some problem may also arise when you pop the last element from a list.
There are two solutions :
- Recover the list in the returned structure
- Pass a one element list containing the list as argument (*-ref-* functions)"
  ;; (let ((popped list))
  ;;   (setq list (cdr list))
  ;;   (setcdr popped nil)
  ;;   popped)
  )

;;; End
;;; ------------------------------------------------------------

(provide 'duo-common)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; duo-common.el ends here
