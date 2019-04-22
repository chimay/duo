;;; duo-referen.el --- Reference part of duo         -*- lexical-binding: t; -*-

;;; Commentary:

;; Use reference to alter list

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

;;; Require
;;; ------------------------------------------------------------

(eval-when-compile
  (require 'duo-common))

(declare-function duo-< "duo-common")
(declare-function duo-last "duo-common")
(declare-function duo-inside "duo-common")
(declare-function duo-member "duo-common")
(declare-function duo-truncate "duo-common")
(declare-function duo-previous "duo-common")
(declare-function duo-before "duo-common")
(declare-function duo-circ-previous "duo-common")
(declare-function duo-circ-next "duo-common")
(declare-function duo-circ-before "duo-common")
(declare-function duo-assoc "duo-common")

;;; Pointers
;;; ------------------------------------------------------------

(defun duo-deref (reflist)
  "Return list referenced by REFLIST.
REFLIST must be one of the forms :
- (cons my-list nil) = (list my-list)
- (cons my-list whatever-you-want)
- (cons not-a-cons . my-list)
That’s all folks."
  (let ((one (car reflist))
        (two (cdr reflist)))
    (cond ((and (not (consp one))
                (not (consp two)))
           nil)
          ((and (consp two)
                (not (consp one)))
           two)
          (t one))))

(defun duo-ref-set (reflist list)
  "Change the reference of REFLIST to LIST.
See `duo-deref' for the format of REFLIST.
LIST must be a cons."
  (when (or (consp list) (null list))
    (let ((oldlist (duo-deref reflist)))
      (cond ((eq oldlist (car reflist)) (setcar reflist list))
            ((eq oldlist (cdr reflist)) (setcdr reflist list))))
    reflist))

(defun duo-ref-frame (reflist &optional list last)
  "Set car of REFLIST to LIST and cdr of REFLIST to LAST in LIST."
  (when list
    (setcar reflist list))
  (when last
    (setcdr reflist last)))

;;; Stack & Queue
;;; ------------------------------------------------------------

(defun duo-ref-push-cons (cons reflist)
  "Add CONS at the beginning of the list referenced by REFLIST.
Return list referenced by REFLIST.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-push-cons cons reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (setcdr cons (duo-deref reflist))
  (duo-ref-set reflist cons)
  (duo-deref reflist))

(defun duo-ref-add-cons (cons reflist &optional last)
  "Store CONS at the end of list referenced by REFLIST. Return CONS.
If non nil, LAST is used to speed up the process.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-add-cons cons reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (last (if last
                   last
                 (duo-last list))))
    (when last
      (setcdr last cons))
    (setcdr cons nil)
    (unless list
      (duo-ref-set reflist cons))
    cons))

(defun duo-ref-push (elem reflist)
  "Add ELEM at the beginning of the list referenced by REFLIST.
Return list referenced by REFLIST.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-push elem reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let ((duo (cons elem (duo-deref reflist))))
    (duo-ref-set reflist duo)
    (duo-deref reflist)))

(defun duo-ref-add (elem reflist &optional last)
  "Add ELEM at the end of list referenced by REFLIST. Return the new LAST.
If non nil, LAST is used to speed up the process.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-add elem reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (last (if last
                   last
                 (duo-last list)))
         (duo (cons elem nil)))
    (when last
      (setcdr last duo))
    (unless list
      (duo-ref-set reflist duo))
    duo))

(defun duo-ref-push-new-cons (cons reflist)
  "Add CONS at the beginning of list referenced by REFLIST if not already there.
Return list referenced by REFLIST.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-push-new-cons cons reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let ((list (duo-deref reflist)))
    (if (duo-inside cons list)
        list
      (duo-ref-push-cons cons reflist))))

(defun duo-ref-add-new-cons (cons reflist &optional last)
  "Add CONS at the end of list referenced by REFLIST if not already there.
Return the new LAST.
If non nil, LAST is used to speed up the process.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-add-new-cons cons reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (unless (duo-inside cons (duo-deref reflist))
    (duo-ref-add-cons cons reflist last)))

(defun duo-ref-push-new (elem reflist &optional fn-equal)
  "Add ELEM at the beginning of list referenced by REFLIST if not already there.
Return list referenced by REFLIST.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-push-new elem reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let ((list (duo-deref reflist)))
    (if (duo-member elem list fn-equal)
        list
      (duo-ref-push elem reflist))))

(defun duo-ref-add-new (elem reflist &optional last fn-equal)
  "Add ELEM at the end of the list referenced by REFLIST.
Do nothing if ELEM is already present.
Return the new LAST.
If non nil, LAST is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-add-new elem reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (unless (duo-member elem (duo-deref reflist) fn-equal)
    (duo-ref-add elem reflist last)))

(defun duo-ref-pop (reflist)
  "Remove first element in the list referenced by REFLIST. Return popped cons.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-pop' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(setq popped (duo-ref-pop reflist))
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (popped list))
    (duo-ref-set reflist (cdr list))
    (setcdr popped nil)
    popped))

(defun duo-ref-drop (reflist)
  "Remove last element of list referenced by REFLIST.
Return cons of removed element.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-drop reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (before-last (duo-last list 2))
         (last (cdr before-last)))
    (if last
        (setcdr before-last nil)
      ;; One element list
      (setq last list)
      (duo-ref-set reflist nil))
    last))

(defun duo-ref-push-and-truncate (elem reflist &optional num)
  "Add ELEM at the beginning of list referenced by REFLIST.
Truncate list referenced by REFLIST to its first NUM elements.
If NUM is nil, do nothing.
Return list referenced by REFLIST.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-push-and-truncate elem reflist num)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (duo-ref-push elem reflist)
  (duo-truncate (duo-deref reflist) num)
  (duo-deref reflist))

(defun duo-ref-add-and-clip (elem reflist &optional num length last)
  "Add ELEM at the end of list referenced by REFLIST.
Truncate list referenced by REFLIST to its last NUM elements.
If NUM is nil, do nothing.
Return the new LAST.
If non nil, LENGTH is used to speed up the process.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-add-and-clip elem reflist num)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (length (if length
                     length
                   (length list)))
         (added (duo-ref-add elem reflist last)))
    (when added
      (setq length (1+ length)))
    (while (> length num)
      (duo-ref-pop reflist)
      (setq length (1- length)))
    added))

(defun duo-ref-push-new-and-truncate (elem reflist &optional num fn-equal)
  "Push ELEM to list referenced by REFLIST if not there and truncate.
The first NUM elements are kept.
If NUM is nil, do nothing.
Return list referenced by REFLIST.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-push-new-and-truncate elem reflist num)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let ((list (duo-deref reflist)))
    (if (duo-member elem list fn-equal)
        list
      (duo-ref-push-and-truncate elem reflist num))))

(defun duo-ref-add-new-and-clip (elem reflist &optional num length last fn-equal)
  "Add ELEM at the end of list referenced by REFLIST if not there.
If NUM is nil, do nothing.
Return the new LAST.
If non nil, LENGTH and LAST are used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-add-new-and-clip elem reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (unless (duo-member elem (duo-deref reflist) fn-equal)
    (duo-ref-add-and-clip elem reflist num length last)))

;;; Rotate <- ->
;;; ------------------------------

(defun duo-ref-rotate-left (reflist)
  "Rotate list referenced by REFLIST to the left.
Return list referenced by REFLIST.
Equivalent to pop first element and add it to the end.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-pop' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-rotate-left reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  ;; Length list > 1
  (when (cdr (duo-deref reflist))
    (let ((popped (duo-ref-pop reflist)))
      (duo-ref-add-cons popped reflist)))
  (duo-deref reflist))

(defun duo-ref-rotate-right (reflist)
  "Rotate list referenced by REFLIST to the right.
Return list referenced by REFLIST.
Equivalent to drop last element and push it at the beginning.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-rotate-right reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  ;; Length list > 1
  (when (cdr (duo-deref reflist))
    (let ((dropped (duo-ref-drop reflist)))
      (duo-ref-push-cons dropped reflist)))
  (duo-deref reflist))

;;; Roll
;;; ------------------------------

(defun duo-ref-roll-cons-to-beg (cons reflist &optional previous)
  "Roll list referenced by REFLIST to the left until CONS is at the beginning.
Return list referenced by REFLIST.
CONS must be a cons in list referenced by REFLIST.
If non nil, PREVIOUS is used to speed up the process.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-roll-cons-to-beg cons reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (previous (if previous
                       previous
                     (duo-previous cons list)))
         (last previous))
    (when (and (cdr list)
               previous)
      (while (cdr last)
        (setq last (cdr last)))
      (setcdr previous nil)
      (setcdr last list)
      (duo-ref-set reflist cons)))
  (duo-deref reflist))

(defun duo-ref-roll-cons-to-end (cons reflist)
  "Roll list referenced by REFLIST to the right until CONS is at the end.
Return list referenced by REFLIST.
CONS must be a cons in list referenced by REFLIST.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-roll-cons-to-end cons reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (next (cdr cons))
         (last next))
    (when (and (cdr list)
               next)
      (while (cdr last)
        (setq last (cdr last)))
      (setcdr cons nil)
      (setcdr last list)
      (duo-ref-set reflist next)))
  (duo-deref reflist))

(defun duo-ref-roll-to-beg (elem reflist &rest restargs)
  "Roll list referenced by REFLIST to the left until ELEM is at the beginning.
Return list referenced by REFLIST.
ELEM must be present in list referenced by REFLIST.
If non nil in RESTARGS :
- PREVIOUS is used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-roll-to-beg elem reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (previous (or (car (cdr (car (duo-assoc "cons" argassoc))))
                       (duo-before elem list 1 fn-equal)))
         (duo (if previous
                  (cdr previous)
                list)))
    (duo-ref-roll-cons-to-beg duo reflist previous)))

(defun duo-ref-roll-to-end (elem reflist &optional fn-equal)
  "Roll list referenced by REFLIST to the right until ELEM is at the end.
Return list referenced by REFLIST.
ELEM must be present in list referenced by REFLIST.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-roll-to-end elem reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (duo (duo-member elem list fn-equal)))
    (duo-ref-roll-cons-to-end duo reflist)))

;;; Reverse
;;; ------------------------------

(defun duo-ref-reverse (reflist)
  "Reverse list referenced by REFLIST. Return list referenced by REFLIST.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-reverse reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (newlist (duo-last list))
         (current newlist)
         (previous (duo-previous current list)))
    (while previous
      (setcdr current previous)
      (setq current previous)
      (setq previous (duo-previous current list)))
    (setcdr current nil)
    (duo-ref-set reflist newlist)
    newlist))

(defun duo-ref-reverse-previous (cons reflist)
  "Reverse first part of list referenced by REFLIST.
The fist part goes from beginning to CONS included.
Return list referenced by REFLIST.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-reverse-previous cons reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let ((next (cdr cons)))
    (setcdr cons nil)
    (duo-ref-reverse reflist)
    (setcdr (duo-last (duo-deref reflist)) next)
    (duo-deref reflist)))

(defun duo-ref-reverse-next (cons reflist)
  "Reverse second part of list referenced by REFLIST, from after CONS to end.
Return list referenced by REFLIST.
Destructive."
  (let ((refnext (list (cdr cons))))
    (setcdr cons nil)
    (duo-ref-reverse refnext)
    (setcdr cons (duo-deref refnext))
    (duo-deref reflist)))

(defun duo-ref-reverse-before (elem reflist &optional fn-equal)
  "Reverse first part of list referenced by REFLIST.
The first part goes from beginning to ELEM included.
Return list referenced by REFLIST.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-reverse-before elem reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let ((duo (duo-member elem (duo-deref reflist) fn-equal)))
    (duo-ref-reverse-previous duo reflist)))

(defun duo-ref-reverse-after (elem reflist &optional fn-equal)
  "Reverse second part of list referenced by REFLIST, from after ELEM to end.
Return list referenced by REFLIST.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((duo (duo-member elem (duo-deref reflist) fn-equal)))
    (duo-ref-reverse-next duo reflist)))

;;; Insert
;;; ------------------------------------------------------------

;;; Cons Cons
;;; ------------------------------

(defun duo-ref-insert-cons-previous (cons new reflist &optional previous)
  "Insert NEW before CONS in list referenced by REFLIST. Return NEW.
CONS must be a cons in list referenced by REFLIST.
NEW is the cons inserted.
If non nil, PREVIOUS inserted is used to speed up the process.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-insert-cons-previous cons new reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let ((list (duo-deref reflist)))
    (if (eq cons list)
        (duo-ref-push-cons new reflist)
      (let ((previous (if (and previous (not (eq previous new)))
                          previous
                        (duo-previous cons list))))
        (if previous
            (progn
              (setcdr new (cdr previous))
              (setcdr previous new)
              new)
          nil)))))

(defun duo-ref-insert-cons-next (cons new)
  "Insert NEW after CONS in list. Return NEW.
CONS must be a cons in list.
NEW is the cons inserted.
Destructive."
    (setcdr new (cdr cons))
    (setcdr cons new)
    new)

;;; Cons Elem
;;; ------------------------------

(defun duo-ref-insert-previous (cons new reflist &optional previous)
  "Insert NEW before CONS in list referenced by REFLIST. Return cons of NEW.
CONS must be a cons in list referenced by REFLIST.
NEW is the value of the element inserted.
If non nil, PREVIOUS inserted is used to speed up the process.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-insert-previous cons new reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let ((duo (list new)))
    (duo-ref-insert-cons-previous cons duo reflist previous)))

(defun duo-ref-insert-next (cons new)
  "Insert NEW after CONS in list. Return cons of NEW.
CONS must be a cons in list.
NEW is the value of the element inserted.
Destructive."
  (let ((duo (list new)))
    (duo-ref-insert-cons-next cons duo)))

;;; Elem Cons
;;; ------------------------------

(defun duo-ref-insert-cons-before (elem new reflist &rest restargs)
  "Insert NEW before ELEM in list referenced by REFLIST. Return NEW.
ELEM must be present in list.
NEW is the cons inserted.
If non nil in RESTARGS :
- PREVIOUS inserted is used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-insert-cons-before elem new reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (previous (or (car (cdr (car (duo-assoc "cons" argassoc))))
                       (duo-before elem list 1 fn-equal)))
         (duo (if (funcall fn-equal elem (car list))
                  list
                (cdr previous))))
    (duo-ref-insert-cons-previous duo new reflist previous)))

(defun duo-ref-insert-cons-after (elem new reflist &optional fn-equal)
  "Insert NEW after ELEM in list referenced in REFLIST. Return NEW.
ELEM must be present in list.
NEW is the cons inserted.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((duo (duo-member elem (duo-deref reflist) fn-equal)))
    (duo-ref-insert-cons-next duo new)))

;;; Elem Elem
;;; ------------------------------

(defun duo-ref-insert-before (elem new reflist &rest restargs)
  "Insert NEW before ELEM in list referenced by REFLIST. Return cons of NEW.
ELEM must be present in list.
NEW is the value of the element inserted.
If non nil in RESTARGS :
- PREVIOUS inserted is used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-insert-before elem new reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (previous (or (car (cdr (car (duo-assoc "cons" argassoc))))
                       (duo-before elem list 1 fn-equal)))
         (cons-elem (if (funcall fn-equal elem (car list))
                        list
                      (cdr previous)))
         (cons-new (list new)))
    (duo-ref-insert-cons-previous cons-elem cons-new reflist previous)))

(defun duo-ref-insert-after (elem new reflist &optional fn-equal)
  "Insert NEW after ELEM referenced in REFLIST. Return cons of NEW.
ELEM must be present in list.
NEW is the value of the element inserted.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let* ((list (duo-deref reflist))
         (cons-elem (duo-member elem list fn-equal))
         (cons-new (list new)))
    (duo-ref-insert-cons-next cons-elem cons-new)))

;;; Remove
;;; ------------------------------------------------------------

(defun duo-ref-remove (cons reflist &optional previous)
  "Remove CONS from list referenced by REFLIST. Return CONS.
CONS must be a cons in list referenced by REFLIST.
If non nil, PREVIOUS removed is used to speed up the process.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-pop' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-remove cons reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let ((list (duo-deref reflist)))
    (if (eq cons list)
        (duo-ref-pop reflist)
      (let ((previous (if (and previous
                               (not (eq previous cons)))
                          previous
                        (duo-previous cons list))))
        (when previous
          (setcdr previous (cdr cons))
          (setcdr cons nil))
        cons))))

(defun duo-ref-delete (elem reflist &rest restargs)
  "Delete ELEM from list referenced by REFLIST. Return removed cons.
If non nil in RESTARGS :
- PREVIOUS deleted is used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-pop' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-delete elem reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (previous (or (car (cdr (car (duo-assoc "cons" argassoc))))
                       (duo-before elem list 1 fn-equal)))
         (duo (if (funcall fn-equal elem (car list))
                  list
                (cdr previous))))
    (if (and duo list)
        (duo-ref-remove duo reflist previous)
      nil)))

(defun duo-ref-delete-all (elem reflist &optional fn-equal)
  "Delete all elements equals to ELEM from list referenced by REFLIST.
Return list of removed cons.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-pop' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-delete-all elem reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let ((removed)
        (removed-list (list nil))
        (last)
        (list)
        (duo)
        (pre)
        (next)
        (fn-equal (or fn-equal #'equal)))
    (while (funcall fn-equal elem (car (duo-deref reflist)))
      (setq removed (duo-ref-pop reflist))
      (setq last (duo-ref-add-cons removed removed-list last)))
    (setq list (duo-deref reflist))
    (setq duo list)
    (setq pre nil)
    (while duo
      (setq next (cdr duo))
      (if (funcall fn-equal elem (car duo))
          (progn
            (duo-ref-remove duo reflist pre)
            (setq removed duo)
            (setq last (duo-ref-add-cons removed removed-list last))
            (setq pre nil))
        (setq pre duo))
      (setq duo next))
    (duo-deref removed-list)))

;;; Teleport
;;; ------------------------------------------------------------

;;; Cons Cons
;;; ------------------------------

(defun duo-ref-teleport-cons-previous (cons moved reflist &optional
                                            previous-removed previous-inserted)
  "Move MOVED before CONS in list referenced by REFLIST. Return MOVED.
CONS must be a cons in list referenced by REFLIST.
MOVED is the cons of the moved element.
If non nil, PREVIOUS-REMOVED and PREVIOUS-INSERTED
are used to speed up the process.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-cons-previous cons moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (unless (eq cons moved)
    (duo-ref-remove moved reflist previous-removed)
    (duo-ref-insert-cons-previous cons moved reflist previous-inserted))
  moved)

(defun duo-ref-teleport-cons-next (cons moved reflist &optional previous)
  "Move MOVED after CONS in list referenced by REFLIST. Return MOVED.
CONS must be a cons in list referenced by REFLIST.
MOVED is the cons of the moved element.
If non nil, PREVIOUS removed is used to speed up the process.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-cons-next cons moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (unless (eq cons moved)
    (duo-ref-remove moved reflist previous)
    (duo-ref-insert-cons-next cons moved))
  moved)

;;; Cons Elem
;;; ------------------------------

(defun duo-ref-teleport-previous (cons moved reflist &rest restargs)
  "Move MOVED before CONS in list referenced by REFLIST. Return MOVED.
CONS must be a cons in list referenced by REFLIST.
MOVED is the value of the moved element.
If non nil in RESTARGS :
- PRE-REMOVED and PRE-INSERTED are used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-previous cons moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (pre-removed (or (car (cdr (car (duo-assoc "cons" argassoc))))
                          (duo-before moved list 1 fn-equal)))
         (pre-inserted (car (nthcdr 2 (car (duo-assoc "cons" argassoc)))))
         (duo (if pre-removed
                  (cdr pre-removed)
                (duo-member moved list fn-equal))))
    (duo-ref-teleport-cons-previous cons duo reflist pre-removed pre-inserted)))

(defun duo-ref-teleport-next (cons moved reflist &rest restargs)
  "Move MOVED after CONS in list referenced by REFLIST. Return MOVED.
CONS must be a cons in list referenced by REFLIST.
MOVED is the value of the moved element.
If non nil in RESTARGS :
- PREVIOUS removed is used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-next cons moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (previous (or (car (cdr (car (duo-assoc "cons" argassoc))))
                       (duo-before moved list 1 fn-equal)))
         (duo (if previous
                  (cdr previous)
                (duo-member moved list fn-equal))))
    (duo-ref-teleport-cons-next cons duo reflist previous)))

;;; Elem Cons
;;; ------------------------------

(defun duo-ref-teleport-cons-before (elem moved reflist &rest restargs)
  "Move MOVED before ELEM in list referenced by REFLIST. Return MOVED.
ELEM must be present in list.
MOVED is the cons of the moved element.
If non nil in RESTARGS :
- PRE-REMOVED and PRE-INSERTED are used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-cons-before cons moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (pre-removed (car (cdr (car (duo-assoc "cons" argassoc)))))
         (pre-inserted (or (car (nthcdr 2 (car (duo-assoc "cons" argassoc))))
                           (duo-before elem list 1 fn-equal)))
         (duo (if pre-inserted
                  (cdr pre-inserted)
                (duo-member elem list fn-equal))))
    (duo-ref-teleport-cons-previous duo moved reflist
                                    pre-removed pre-inserted)))

(defun duo-ref-teleport-cons-after (elem moved reflist &rest restargs)
  "Move MOVED after ELEM in list referenced by REFLIST. Return MOVED.
ELEM must be present in list.
MOVED is the cons of the moved element.
If non nil in RESTARGS :
- PREVIOUS removed is used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-cons-after cons moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (previous (car (cdr (car (duo-assoc "cons" argassoc)))))
         (duo (duo-member elem list fn-equal)))
    (duo-ref-teleport-cons-next duo moved reflist previous)))

;;; Elem Elem
;;; ------------------------------

(defun duo-ref-teleport-before (elem moved reflist &rest restargs)
  "Move MOVED before ELEM in list referenced by REFLIST. Return MOVED.
ELEM must be present in list.
MOVED is the value of the moved element.
If non nil in RESTARGS :
- PRE-REMOVED and PRE-INSERTED are used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-before cons moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (pre-removed (or (car (cdr (car (duo-assoc "cons" argassoc))))
                          (duo-before moved list 1 fn-equal)))
         (pre-inserted (or (car (nthcdr 2 (car (duo-assoc "cons" argassoc))))
                           (duo-before elem list 1 fn-equal)))
         (elem-cons (if pre-inserted
                        (cdr pre-inserted)
                      (duo-member elem list fn-equal)))
         (moved-cons (if pre-removed
                         (cdr pre-removed)
                       (duo-member moved list fn-equal))))
    (duo-ref-teleport-cons-previous elem-cons moved-cons reflist
                                    pre-removed pre-inserted)))

(defun duo-ref-teleport-after (elem moved reflist &rest restargs)
  "Move MOVED after ELEM in LIST. Return (cons of MOVED . LIST).
ELEM must be present in list.
MOVED is the value of the moved element.
If non nil in RESTARGS :
- PREVIOUS removed is used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-after elem moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (previous (or (car (cdr (car (duo-assoc "cons" argassoc))))
                       (duo-before moved list 1 fn-equal)))
         (elem-cons (duo-member elem list fn-equal))
         (moved-cons (if previous
                         (cdr previous)
                       (duo-member moved list fn-equal))))
    (duo-ref-teleport-cons-next elem-cons moved-cons reflist previous)))

;;; Move
;;; ------------------------------------------------------------

;;; Linear
;;; ------------------------------

(defun duo-ref-move-previous (moved reflist &optional num)
  "Move MOVED to NUM previous place in list referenced by REFLIST. Return MOVED.
If range is exceeded, move MOVED at the beginning of the list.
MOVED must be a cons in list referenced by REFLIST.
NUM defaults to 1.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-move-previous moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (num (or num 1))
         (pre-ins (duo-previous moved list (1+ num)))
         (landmark (if pre-ins
                       (cdr pre-ins)
                     list))
         (pre-rem (if pre-ins
                      (nthcdr num pre-ins)
                    (duo-previous moved list))))
    (duo-ref-teleport-cons-previous landmark moved reflist pre-rem pre-ins)
    moved))

(defun duo-ref-move-next (moved reflist &optional num)
  "Move MOVED to NUM next place in list referenced by REFLIST. Return MOVED.
If range is exceeded, move MOVED at the end of the list.
MOVED must be a cons in list referenced by REFLIST.
NUM defaults to 1.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-move-next moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (num (or num 1))
         (landmark (nthcdr num moved)))
    (unless landmark
      (setq landmark (duo-last list)))
    (duo-ref-teleport-cons-next landmark moved reflist)
    moved))

(defun duo-ref-move-before (elem reflist &optional num fn-equal)
  "Move ELEM to NUM previous place in list referenced by REFLIST. Return MOVED.
If range is exceeded, move ELEM at the beginning of the list.
MOVED is the moved value.
NUM defaults to 1.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-move-before moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (num (or num 1))
         (pre-ins (duo-before elem list (1+ num) fn-equal))
         (landmark (if pre-ins
                       (cdr pre-ins)
                     list))
         (pre-rem (if pre-ins
                      (nthcdr num pre-ins)
                    (duo-before elem list 1 fn-equal)))
         (moved (if pre-rem
                    (cdr pre-rem)
                  (duo-member elem list fn-equal))))
    (duo-ref-teleport-cons-previous landmark moved reflist pre-rem pre-ins)
    moved))

(defun duo-ref-move-after (elem reflist &optional num fn-equal)
  "Move ELEM to NUM next place in list referenced by REFLIST. Return MOVED.
If range is exceeded, move MOVED at the end of the list.
MOVED is the moved value.
NUM defaults to 1.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-move-after moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (num (or num 1))
         (moved (duo-member elem list fn-equal))
         (landmark (nthcdr num moved)))
    (unless landmark
      (setq landmark (duo-last list)))
    (duo-ref-teleport-cons-next landmark moved reflist)
    moved))

;;; Circular
;;; ------------------------------

(defun duo-ref-circ-move-previous (moved reflist &optional num)
  "Move MOVED to NUM previous place in list referenced by REFLIST. Return MOVED.
Circular : if in beginning of list, go to the end.
MOVED must be a cons in list referenced by REFLIST.
NUM defaults to 1.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-circ-move-previous moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (num (or num 1))
         (pre-ins (duo-circ-previous moved list (1+ num)))
         (landmark (duo-circ-next pre-ins list))
         (pre-rem (duo-circ-next pre-ins list num)))
    (when (eq landmark list)
      (setq pre-ins nil))
    (when (eq moved pre-ins)
      (setq pre-ins nil))
    (duo-ref-teleport-cons-previous landmark moved reflist pre-rem pre-ins)))

(defun duo-ref-circ-move-next (moved reflist &optional num)
  "Move MOVED to NUM next place in list referenced by REFLIST. Return MOVED.
Circular : if in end of list, go to the beginning.
MOVED must be a cons in list referenced by REFLIST.
NUM defaults to 1.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-circ-move-next moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (num (or num 1))
         (landmark (duo-circ-next moved list num)))
    (duo-ref-teleport-cons-next landmark moved reflist)))

(defun duo-ref-circ-move-before (elem reflist &optional num fn-equal)
  "Move ELEM to NUM previous place in LIST. Return MOVED.
Circular : if in beginning of list, go to the end.
MOVED is the moved value.
NUM defaults to 1.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-circ-move-before moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (num (or num 1))
         (pre-ins (duo-circ-before elem list (1+ num) fn-equal))
         (landmark (duo-circ-next pre-ins list))
         (pre-rem (duo-circ-next pre-ins list num))
         (moved (cdr pre-rem)))
    (unless moved
      (setq moved (duo-member elem list fn-equal))
      (setq pre-rem nil))
    (when (eq landmark list)
      (setq pre-ins nil))
    (when (eq moved pre-ins)
      (setq pre-ins nil))
    (duo-ref-teleport-cons-previous landmark moved reflist pre-rem pre-ins)))

(defun duo-ref-circ-move-after (elem reflist &optional num fn-equal)
  "Move ELEM to NUM next place in LIST. Return MOVED.
Circular : if in end of list, go to the beginning.
MOVED is the moved value.
NUM defaults to 1.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-circ-move-after moved reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (num (or num 1))
         (moved (duo-member elem list fn-equal))
         (landmark (duo-circ-next moved list num)))
    (duo-ref-teleport-cons-next landmark moved reflist)))

;;; Exchange
;;; ------------------------------------------------------------

(defun duo-ref-exchange-cons (one two reflist &optional pre-one pre-two)
  "Exchange cons ONE and TWO in list referenced by REFLIST. Return (ONE . TWO).
ONE and TWO must be cons in list referenced by REFLIST.
If non nil, PRE-ONE and PRE-TWO are used to speed up the process.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-exchange-cons one two reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (unless (eq one two)
    (if (eq two (duo-deref reflist))
        (let ((return)
              (swap))
          (setq return (duo-ref-exchange-cons two one reflist))
          (setq swap (car return))
          (setcar return (cdr return))
          (setcdr return swap)
          return)
      (let* ((list (duo-deref reflist))
             (pre-one (if pre-one
                          pre-one
                        (duo-previous one list)))
             (pre-two (if pre-two
                          pre-two
                        (duo-previous two list))))
        (cond ((eq (cdr one) two)
               (duo-ref-teleport-cons-next two one reflist pre-one))
              ((eq (cdr two) one)
               (duo-ref-teleport-cons-next one two reflist pre-two))
              (t
               (duo-ref-teleport-cons-next one two reflist pre-two)
               (duo-ref-teleport-cons-next pre-two one reflist pre-one)))
        (cons one two)))))

(defun duo-ref-exchange (one two reflist &optional pre-one pre-two fn-equal)
  "Exchange elements ONE and TWO in list referenced by REFLIST.
Return (ONE . TWO).
ONE and TWO must be present in list referenced by REFLIST.
If non nil, PRE-ONE and PRE-TWO are used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned list.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-exchange one two reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (cons-one (duo-member one list fn-equal))
         (cons-two (duo-member two list fn-equal)))
    (duo-ref-exchange-cons cons-one cons-two reflist pre-one pre-two)))

;;; Sorted
;;; ------------------------------------------------------------

(defun duo-ref-insert-in-sorted-list (new reflist &optional fn-less)
  "Insert NEW at the right place in list referenced by REFLIST.
The list must be sorted in ascending order.
Return cons of NEW.
FN-LESS takes two arguments and return t if the first is less than the second.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-insert-in-sorted-list new reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let ((fn-less (or fn-less #'duo-<))
        (list (duo-deref reflist)))
    (cond ((not list)
           (duo-ref-set reflist (cons new nil))
           (duo-deref reflist))
          ((funcall fn-less new (car list)) (duo-ref-push new reflist))
          (t (let ((duo list)
                   (next (cdr list)))
               (while (and duo
                           next
                           (funcall fn-less (car next) new))
                 (setq duo (cdr duo))
                 (setq next (cdr next)))
               (duo-ref-insert-next duo new))))))

;;; Group
;;; ------------------------------

(defun duo-ref-insert-at-group-beg (new reflist &optional fn-group)
  "Insert NEW in list referenced by REFLIST at the beginning of its group.
The group is determined by FN-GROUP.
If the group is not found, insert at the beginning of list
referenced by REFLIST.
Return cons of NEW.
NEW is the value of the element inserted.
FN-GROUP takes two arguments and returns t if they belongs to the same group.
FN-GROUP defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-insert-at-group-beg new reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (previous (duo-before new list 1 fn-group))
         (duo))
    (if previous
        (progn
          (duo-ref-insert-next previous new)
          (setq duo (cdr previous)))
      (duo-ref-push new reflist)
      (setq duo (duo-deref reflist)))
    duo))

(defun duo-ref-insert-at-group-end (new reflist &optional fn-group)
  "Insert NEW in list referenced by REFLIST, at the end of its group.
The group is determined by FN-GROUP.
If the group is not found, insert at the end of list referenced by REFLIST.
Return cons of NEW.
NEW is the value of the element inserted.
FN-GROUP takes two arguments and returns t if they belongs to the same group.
FN-GROUP defaults to `equal'.
See `duo-deref' for the format of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-insert-at-group-end new reflist)
;; Update list
\(setq mylist (duo-deref reflist))
Destructive."
  (let* ((list (duo-deref reflist))
         (fn-group (or fn-group #'equal))
         (previous (duo-member new list fn-group))
         (duo))
    (while (and previous
                (funcall fn-group (car (cdr previous)) new))
      (setq previous (cdr previous)))
    (if previous
        (progn
          (duo-ref-insert-next previous new)
          (setq duo (cdr previous)))
      (setq duo (duo-ref-add new reflist)))
    duo))

;;; End
;;; ------------------------------------------------------------

(provide 'duo-referen)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; duo-referen.el ends here
