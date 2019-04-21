;;; duo-symbol.el --- Symbol part of duo             -*- lexical-binding: t; -*-

;;; Commentary:

;; Use symbol to alter list

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

;;; Stack & Queue
;;; ------------------------------------------------------------

(defun duo-sym-push-cons (cons symlist)
  "Add CONS at the beginning of SYMLIST. Return list in SYMLIST.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-push-cons cons 'list)
Destructive."
  (setcdr cons (symbol-value symlist))
  (set symlist cons)
  (symbol-value symlist))

(defun duo-sym-add-cons (cons symlist &optional last)
  "Store CONS at the end of SYMLIST. Return CONS.
If non nil, LAST is used to speed up the process.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-add-cons cons 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (last (if last
                   last
                 (duo-last list))))
    (when last
      (setcdr last cons))
    (setcdr cons nil)
    (unless list
      (set symlist cons))
    cons))

(defun duo-sym-push (elem symlist)
  "Add ELEM at the beginning of SYMLIST.
Return list in SYMLIST.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-push elem 'list)
Destructive."
  (let ((duo (cons elem (symbol-value symlist))))
    (set symlist duo)
    (symbol-value symlist)))

(defun duo-sym-add (elem symlist &optional last)
  "Add ELEM at the end of SYMLIST. Return the new LAST.
If non nil, LAST is used to speed up the process.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-add elem 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (last (if last
                   last
                 (duo-last list)))
         (duo (cons elem nil)))
    (when last
      (setcdr last duo))
    (unless list
      (set symlist duo))
    duo))

(defun duo-sym-push-new-cons (cons symlist)
  "Add CONS at the beginning of SYMLIST if not already there.
Return list in SYMLIST.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-push-new-cons cons 'list)
Destructive."
  (let ((list (symbol-value symlist)))
    (if (duo-inside cons list)
        list
      (duo-sym-push-cons cons symlist))))

(defun duo-sym-add-new-cons (cons symlist &optional last)
  "Add CONS at the end of SYMLIST if not already there.
Return the new LAST.
If non nil, LAST is used to speed up the process.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-add-new-cons cons 'list)
Destructive."
  (unless (duo-inside cons (symbol-value symlist))
    (duo-sym-add-cons cons symlist last)))

(defun duo-sym-push-new (elem symlist &optional fn-equal)
  "Add ELEM at the beginning of SYMLIST if not already there.
Return list in SYMLIST.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-push-new elem 'list)
Destructive."
  (let ((list (symbol-value symlist)))
    (if (duo-member elem list fn-equal)
        list
      (duo-sym-push elem symlist))))

(defun duo-sym-add-new (elem symlist &optional last fn-equal)
  "Add ELEM at the end of SYMLIST.
Do nothing if ELEM is already present.
Return the new LAST.
If non nil, LAST is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-add-new elem 'list)
Destructive."
  (unless (duo-member elem (symbol-value symlist) fn-equal)
    (duo-sym-add elem symlist last)))

(defun duo-sym-pop (symlist)
  "Remove first element in SYMLIST. Return popped cons.
See the docstring of `duo-naive-pop' to know why it
uses the list symbol as argument.
Common usage :
\(setq popped (duo-sym-pop 'list))
Destructive."
  (let* ((list (symbol-value symlist))
         (popped list))
    (set symlist (cdr list))
    (setcdr popped nil)
    popped))

(defun duo-sym-drop (symlist)
  "Remove last element of SYMLIST.
Return cons of removed element.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-drop 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (before-last (duo-last list 2))
         (last (cdr before-last)))
    (if last
        (setcdr before-last nil)
      ;; One element list
      (setq last list)
      (set symlist nil))
    last))

(defun duo-sym-push-and-truncate (elem symlist &optional num)
  "Add ELEM at the beginning of SYMLIST.
Truncate SYMLIST to its first NUM elements.
If NUM is nil, do nothing.
Return list in SYMLIST.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-push-and-truncate elem 'list num)
Destructive."
  (duo-sym-push elem symlist)
  (duo-truncate (symbol-value symlist) num)
  (symbol-value symlist))

(defun duo-sym-add-and-clip (elem symlist &optional num length last)
  "Add ELEM at the end of SYMLIST.
Truncate SYMLIST to its last NUM elements.
If NUM is nil, do nothing.
Return the new LAST.
If non nil, LENGTH is used to speed up the process.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-add-and-clip elem 'list num)
Destructive."
  (let* ((list (symbol-value symlist))
         (length (if length
                     length
                   (length list)))
         (added (duo-sym-add elem symlist last)))
    (when added
      (setq length (1+ length)))
    (while (> length num)
      (duo-sym-pop symlist)
      (setq length (1- length)))
    added))

(defun duo-sym-push-new-and-truncate (elem symlist &optional num fn-equal)
  "Push ELEM to SYMLIST if not there and truncate.
The first NUM elements are kept.
If NUM is nil, do nothing.
Return list in SYMLIST.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-push-new-and-truncate elem 'list num)
Destructive."
  (let ((list (symbol-value symlist)))
    (if (duo-member elem list fn-equal)
        list
      (duo-sym-push-and-truncate elem symlist num))))

(defun duo-sym-add-new-and-clip (elem symlist &optional num length last fn-equal)
  "Add ELEM at the end of SYMLIST if not there.
If NUM is nil, do nothing.
Return the new LAST.
If non nil, LENGTH and LAST are used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-add-new-and-clip elem 'list)
Destructive."
  (unless (duo-member elem (symbol-value symlist) fn-equal)
    (duo-sym-add-and-clip elem symlist num length last)))

;;; Rotate <- ->
;;; ------------------------------

(defun duo-sym-rotate-left (symlist)
  "Rotate SYMLIST to the left.
Return list in SYMLIST.
Equivalent to pop first element and add it to the end.
See the docstring of `duo-naive-pop' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-rotate-left 'list)
Destructive."
  ;; Length list > 1
  (when (cdr (symbol-value symlist))
    (let ((popped (duo-sym-pop symlist)))
      (duo-sym-add-cons popped symlist)))
  (symbol-value symlist))

(defun duo-sym-rotate-right (symlist)
  "Rotate SYMLIST to the right.
Return list in SYMLIST.
Equivalent to drop last element and push it at the beginning.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-rotate-right 'list)
Destructive."
  ;; Length list > 1
  (when (cdr (symbol-value symlist))
    (let ((dropped (duo-sym-drop symlist)))
      (duo-sym-push-cons dropped symlist)))
  (symbol-value symlist))

;;; Roll
;;; ------------------------------

(defun duo-sym-roll-cons-to-beg (cons symlist &optional previous)
  "Roll SYMLIST to the left until CONS is at the beginning.
Return list in SYMLIST.
CONS must be a cons in SYMLIST.
If non nil, PREVIOUS is used to speed up the process.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-roll-cons-to-beg cons 'list)
Destructive."
  (let* ((list (symbol-value symlist))
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
      (set symlist cons)))
  (symbol-value symlist))

(defun duo-sym-roll-cons-to-end (cons symlist)
  "Roll SYMLIST to the right until CONS is at the end.
Return list in SYMLIST.
CONS must be a cons in SYMLIST.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-roll-cons-to-end cons 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (next (cdr cons))
         (last next))
    (when (and (cdr list)
               next)
      (while (cdr last)
        (setq last (cdr last)))
      (setcdr cons nil)
      (setcdr last list)
      (set symlist next)))
  (symbol-value symlist))

(defun duo-sym-roll-to-beg (elem symlist &rest restargs)
  "Roll SYMLIST to the left until ELEM is at the beginning.
Return list in SYMLIST.
ELEM must be present in SYMLIST.
If non nil in RESTARGS :
- PREVIOUS is used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-roll-to-beg elem 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (previous (or (car (cdr (car (duo-assoc "cons" argassoc))))
                       (duo-before elem list 1 fn-equal)))
         (duo (if previous
                  (cdr previous)
                list)))
    (duo-sym-roll-cons-to-beg duo symlist previous)))

(defun duo-sym-roll-to-end (elem symlist &optional fn-equal)
  "Roll SYMLIST to the right until ELEM is at the end.
Return list in SYMLIST.
ELEM must be present in SYMLIST.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-roll-to-end elem 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (duo (duo-member elem list fn-equal)))
    (duo-sym-roll-cons-to-end duo symlist)))

;;; Reverse
;;; ------------------------------

(defun duo-sym-reverse (symlist)
  "Reverse SYMLIST. Return list in SYMLIST.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-reverse 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (newlist (duo-last list))
         (current newlist)
         (previous (duo-previous current list)))
    (while previous
      (setcdr current previous)
      (setq current previous)
      (setq previous (duo-previous current list)))
    (setcdr current nil)
    (set symlist newlist)
    newlist))

(defun duo-sym-reverse-previous (cons symlist)
  "Reverse first part of SYMLIST.
The fist part goes from beginning to CONS included.
Return list in SYMLIST.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-reverse-previous cons 'list)
Destructive."
  (let ((next (cdr cons)))
    (setcdr cons nil)
    (duo-sym-reverse symlist)
    (setcdr (duo-last (symbol-value symlist)) next)
    (symbol-value symlist)))

(defun duo-sym-reverse-next (cons symlist)
  "Reverse second part of SYMLIST, from after CONS to end.
Return list in SYMLIST.
Destructive."
  (let ((symnext (make-symbol "next")))
    (set symnext (cdr cons))
    (setcdr cons nil)
    (duo-sym-reverse symnext)
    (setcdr cons (symbol-value symnext))
    (symbol-value symlist)))

(defun duo-sym-reverse-before (elem symlist &optional fn-equal)
  "Reverse first part of SYMLIST.
The first part goes from beginning to ELEM included.
Return list in SYMLIST.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-reverse-before elem 'list)
Destructive."
  (let ((duo (duo-member elem (symbol-value symlist) fn-equal)))
    (duo-sym-reverse-previous duo symlist)))

(defun duo-sym-reverse-after (elem symlist &optional fn-equal)
  "Reverse second part of SYMLIST, from after ELEM to end.
Return list in SYMLIST.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((duo (duo-member elem (symbol-value symlist) fn-equal)))
    (duo-sym-reverse-next duo symlist)))

;;; Insert
;;; ------------------------------------------------------------

;;; Cons Cons
;;; ------------------------------

(defun duo-sym-insert-cons-previous (cons new symlist &optional previous)
  "Insert NEW before CONS in SYMLIST. Return NEW.
CONS must be a cons in SYMLIST.
NEW is the cons inserted.
If non nil, PREVIOUS inserted is used to speed up the process.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-insert-cons-previous cons new 'list)
Destructive."
  (let ((list (symbol-value symlist)))
    (if (eq cons list)
        (duo-sym-push-cons new symlist)
      (let ((previous (if (and previous
                               (not (eq previous new)))
                          previous
                        (duo-previous cons list))))
        (if previous
            (progn
              (setcdr new (cdr previous))
              (setcdr previous new)
              new)
          nil)))))

(defun duo-sym-insert-cons-next (cons new)
  "Insert NEW after CONS in list. Return NEW.
CONS must be a cons in list.
NEW is the cons inserted.
Destructive."
    (setcdr new (cdr cons))
    (setcdr cons new)
    new)

;;; Cons Elem
;;; ------------------------------

(defun duo-sym-insert-previous (cons new symlist &optional previous)
  "Insert NEW before CONS in SYMLIST. Return cons of NEW.
CONS must be a cons in SYMLIST.
NEW is the value of the element inserted.
If non nil, PREVIOUS inserted is used to speed up the process.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-insert-previous cons new 'list)
Destructive."
  (let ((duo (list new)))
    (duo-sym-insert-cons-previous cons duo symlist previous)))

(defun duo-sym-insert-next (cons new)
  "Insert NEW after CONS in list. Return cons of NEW.
CONS must be a cons in list.
NEW is the value of the element inserted.
Destructive."
  (let ((duo (list new)))
    (duo-sym-insert-cons-next cons duo)))

;;; Elem Cons
;;; ------------------------------

(defun duo-sym-insert-cons-before (elem new symlist &rest restargs)
  "Insert NEW before ELEM in SYMLIST. Return NEW.
ELEM must be present in list.
NEW is the cons inserted.
If non nil in RESTARGS :
- PREVIOUS inserted is used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-insert-cons-before elem new 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (previous (or (car (cdr (car (duo-assoc "cons" argassoc))))
                       (duo-before elem list 1 fn-equal)))
         (duo (if (funcall fn-equal elem (car list))
                  list
                (cdr previous))))
    (duo-sym-insert-cons-previous duo new symlist previous)))

(defun duo-sym-insert-cons-after (elem new symlist &optional fn-equal)
  "Insert NEW after ELEM in SYMLIST. Return NEW.
ELEM must be present in list.
NEW is the cons inserted.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((duo (duo-member elem (symbol-value symlist) fn-equal)))
    (duo-sym-insert-cons-next duo new)))

;;; Elem Elem
;;; ------------------------------

(defun duo-sym-insert-before (elem new symlist &rest restargs)
  "Insert NEW before ELEM in SYMLIST. Return cons of NEW.
ELEM must be present in list.
NEW is the value of the element inserted.
If non nil in RESTARGS :
- PREVIOUS inserted is used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-insert-before elem new 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (previous (or (car (cdr (car (duo-assoc "cons" argassoc))))
                       (duo-before elem list 1 fn-equal)))
         (cons-elem (if (funcall fn-equal elem (car list))
                        list
                      (cdr previous)))
         (cons-new (list new)))
    (duo-sym-insert-cons-previous cons-elem cons-new symlist previous)))

(defun duo-sym-insert-after (elem new symlist &optional fn-equal)
  "Insert NEW after ELEM in SYMLIST. Return cons of NEW.
ELEM must be present in list.
NEW is the value of the element inserted.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let* ((list (symbol-value symlist))
         (cons-elem (duo-member elem list fn-equal))
         (cons-new (list new)))
    (duo-sym-insert-cons-next cons-elem cons-new)))

;;; Remove
;;; ------------------------------------------------------------

(defun duo-sym-remove (cons symlist &optional previous)
  "Remove CONS from SYMLIST. Return CONS.
CONS must be a cons in SYMLIST.
If non nil, PREVIOUS removed is used to speed up the process.
See the docstring of `duo-naive-pop' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-remove cons 'list)
Destructive."
  (let ((list (symbol-value symlist)))
    (if (eq cons list)
        (duo-sym-pop symlist)
      (let ((previous (if (and previous
                               (not (eq previous cons)))
                          previous
                        (duo-previous cons list))))
        (when previous
          (setcdr previous (cdr cons))
          (setcdr cons nil))
        cons))))

(defun duo-sym-delete (elem symlist &rest restargs)
  "Delete ELEM from SYMLIST. Return removed cons.
If non nil in RESTARGS :
- PREVIOUS deleted is used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-pop' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-delete elem 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (previous (or (car (cdr (car (duo-assoc "cons" argassoc))))
                       (duo-before elem list 1 fn-equal)))
         (duo (if (funcall fn-equal elem (car list))
                  list
                (cdr previous))))
    (if (and duo list)
        (duo-sym-remove duo symlist previous)
      nil)))

(defun duo-sym-delete-all (elem symlist &optional fn-equal)
  "Delete all elements equals to ELEM from SYMLIST.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-pop' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-delete-all elem 'list)
Destructive."
  (let ((removed)
        (sym-removed-list (make-symbol "removed-list"))
        (last)
        (list)
        (duo)
        (pre)
        (next)
        (fn-equal (or fn-equal #'equal)))
    (set sym-removed-list nil)
    (while (funcall fn-equal elem (car (symbol-value symlist)))
      (setq removed (duo-sym-pop symlist))
      (setq last (duo-sym-add-cons removed sym-removed-list last)))
    (setq list (symbol-value symlist))
    (setq duo list)
    (setq pre nil)
    (while duo
      (setq next (cdr duo))
      (if (funcall fn-equal elem (car duo))
          (progn
            (duo-sym-remove duo symlist pre)
            (setq removed duo)
            (setq last (duo-sym-add-cons removed sym-removed-list last))
            (setq pre nil))
        (setq pre duo))
      (setq duo next))
    (symbol-value sym-removed-list)))

;;; Teleport
;;; ------------------------------------------------------------

;;; Cons Cons
;;; ------------------------------

(defun duo-sym-teleport-cons-previous (cons moved symlist
                                            &optional pre-removed pre-inserted)
  "Move MOVED before CONS in SYMLIST. Return MOVED.
CONS must be a cons in SYMLIST.
MOVED is the cons of the moved element.
If non nil, PRE-REMOVED and PRE-INSERTED
are used to speed up the process.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-teleport-cons-previous cons moved 'list)
Destructive."
  (unless (eq cons moved)
    (duo-sym-remove moved symlist pre-removed)
    (duo-sym-insert-cons-previous cons moved symlist pre-inserted))
  moved)

(defun duo-sym-teleport-cons-next (cons moved symlist &optional previous)
  "Move MOVED after CONS in SYMLIST. Return MOVED.
CONS must be a cons in SYMLIST.
MOVED is the cons of the moved element.
If non nil, PREVIOUS removed is used to speed up the process.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-teleport-cons-next cons moved 'list)
Destructive."
  (unless (eq cons moved)
    (duo-sym-remove moved symlist previous)
    (duo-sym-insert-cons-next cons moved))
  moved)

;;; Cons Elem
;;; ------------------------------

(defun duo-sym-teleport-previous (cons moved symlist &rest restargs)
  "Move MOVED before CONS in SYMLIST. Return MOVED.
CONS must be a cons in SYMLIST.
MOVED is the value of the moved element.
If non nil in RESTARGS :
- PRE-REMOVED and PRE-INSERTED (in that order) are used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-teleport-previous cons moved 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (pre-removed (or (car (cdr (car (duo-assoc "cons" argassoc))))
                          (duo-before moved list 1 fn-equal)))
         (pre-inserted (car (nthcdr 2 (car (duo-assoc "cons" argassoc)))))
         (duo (if pre-removed
                  (cdr pre-removed)
                (duo-member moved list fn-equal))))
    (duo-sym-teleport-cons-previous cons duo symlist pre-removed pre-inserted)))

(defun duo-sym-teleport-next (cons moved symlist &rest restargs)
  "Move MOVED after CONS in SYMLIST. Return MOVED.
CONS must be a cons in SYMLIST.
MOVED is the value of the moved element.
If non nil in RESTARGS :
- PREVIOUS removed is used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-teleport-next cons moved 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (previous (or (car (cdr (car (duo-assoc "cons" argassoc))))
                       (duo-before moved list 1 fn-equal)))
         (duo (if previous
                  (cdr previous)
                (duo-member moved list fn-equal))))
    (duo-sym-teleport-cons-next cons duo symlist previous)))

;;; Elem Cons
;;; ------------------------------

(defun duo-sym-teleport-cons-before (elem moved symlist &rest restargs)
  "Move MOVED before ELEM in SYMLIST. Return MOVED.
ELEM must be present in list.
MOVED is the cons of the moved element.
If non nil in RESTARGS :
- PRE-REMOVED and PRE-INSERTED (in that order) are used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-teleport-cons-before cons moved 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (pre-removed (car (cdr (car (duo-assoc "cons" argassoc)))))
         (pre-inserted (or (car (nthcdr 2 (car (duo-assoc "cons" argassoc))))
                           (duo-before elem list 1 fn-equal)))
         (duo (if pre-inserted
                  (cdr pre-inserted)
                (duo-member elem list fn-equal))))
    (duo-sym-teleport-cons-previous duo
                                    moved symlist pre-removed pre-inserted)))

(defun duo-sym-teleport-cons-after (elem moved symlist &rest restargs)
  "Move MOVED after ELEM in SYMLIST. Return MOVED.
ELEM must be present in list.
MOVED is the cons of the moved element.
If non nil in RESTARGS :
- PREVIOUS removed is used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-teleport-cons-after cons moved 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (previous (or (car (cdr (car (duo-assoc "cons" argassoc))))
                       (duo-before elem list 1 fn-equal)))
         (duo (duo-member elem list fn-equal)))
    (duo-sym-teleport-cons-next duo moved symlist previous)))

;;; Elem Elem
;;; ------------------------------

(defun duo-sym-teleport-before (elem moved symlist &rest restargs)
  "Move MOVED before ELEM in SYMLIST. Return MOVED.
ELEM must be present in list.
MOVED is the value of the moved element.
If non nil in RESTARGS ;
- PRE-REMOVED and PRE-INSERTED (in that order) are used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-teleport-before cons moved 'list)
Destructive."
  (let* ((list (symbol-value symlist))
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
    (duo-sym-teleport-cons-previous elem-cons
                                    moved-cons symlist pre-removed pre-inserted)))

(defun duo-sym-teleport-after (elem moved symlist &rest restargs)
  "Move MOVED after ELEM in LIST. Return (cons of MOVED . LIST).
ELEM must be present in list.
MOVED is the value of the moved element.
If non nil in RESTARGS :
- PREVIOUS removed is used to speed up the process.
- FN-EQUAL takes two arguments and return t if they are considered equals.
- FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-teleport-after elem moved 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (argassoc (duo-partition restargs #'duo-type-of))
         (fn-equal (or (car (cdr (car (duo-assoc "function" argassoc))))
                       #'equal))
         (previous (or (car (cdr (car (duo-assoc "cons" argassoc))))
                       (duo-before moved list 1 fn-equal)))
         (elem-cons (duo-member elem list fn-equal))
         (moved-cons (if previous
                         (cdr previous)
                       (duo-member moved list fn-equal))))
    (duo-sym-teleport-cons-next elem-cons moved-cons symlist previous)))

;;; Move
;;; ------------------------------------------------------------

;;; Linear
;;; ------------------------------

(defun duo-sym-move-previous (moved symlist &optional num)
  "Move MOVED to NUM previous place in SYMLIST. Return MOVED.
If range is exceeded, move MOVED at the beginning of the list.
MOVED must be a cons in SYMLIST.
NUM defaults to 1.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-move-previous moved 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (num (or num 1))
         (pre-ins (duo-previous moved list (1+ num)))
         (landmark (if pre-ins
                       (cdr pre-ins)
                     list))
         (pre-rem (if pre-ins
                      (nthcdr num pre-ins)
                    (duo-previous moved list))))
    (duo-sym-teleport-cons-previous landmark moved symlist pre-rem pre-ins)
    moved))

(defun duo-sym-move-next (moved symlist &optional num)
  "Move MOVED to NUM next place in SYMLIST. Return MOVED.
If range is exceeded, move MOVED at the end of the list.
MOVED must be a cons in SYMLIST.
NUM defaults to 1.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-move-next moved 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (num (or num 1))
         (landmark (nthcdr num moved)))
    (unless landmark
      (setq landmark (duo-last list)))
    (duo-sym-teleport-cons-next landmark moved symlist)
    moved))

(defun duo-sym-move-before (elem symlist &optional num fn-equal)
  "Move ELEM to NUM previous place in SYMLIST. Return MOVED.
If range is exceeded, move ELEM at the beginning of the list.
MOVED is the moved value.
NUM defaults to 1.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-move-before moved 'list)
Destructive."
  (let* ((list (symbol-value symlist))
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
    (duo-sym-teleport-cons-previous landmark moved symlist pre-rem pre-ins)
    moved))

(defun duo-sym-move-after (elem symlist &optional num fn-equal)
  "Move ELEM to NUM next place in SYMLIST. Return MOVED.
If range is exceeded, move MOVED at the end of the list.
MOVED is the moved value.
NUM defaults to 1.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-move-after moved 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (num (or num 1))
         (moved (duo-member elem list fn-equal))
         (landmark (nthcdr num moved)))
    (unless landmark
      (setq landmark (duo-last list)))
    (duo-sym-teleport-cons-next landmark moved symlist)
    moved))

;;; Circular
;;; ------------------------------

(defun duo-sym-circ-move-previous (moved symlist &optional num)
  "Move MOVED to NUM previous place in SYMLIST. Return MOVED.
Circular : if in beginning of list, go to the end.
MOVED must be a cons in SYMLIST.
NUM defaults to 1.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-circ-move-previous moved 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (num (or num 1))
         (pre-ins (duo-circ-previous moved list (1+ num)))
         (landmark (duo-circ-next pre-ins list))
         (pre-rem (duo-circ-next pre-ins list num)))
    (when (eq landmark list)
      (setq pre-ins nil))
    (when (eq moved pre-ins)
      (setq pre-ins nil))
    (duo-sym-teleport-cons-previous landmark moved symlist pre-rem pre-ins)))

(defun duo-sym-circ-move-next (moved symlist &optional num)
  "Move MOVED to NUM next place in SYMLIST. Return MOVED.
Circular : if in end of list, go to the beginning.
MOVED must be a cons in SYMLIST.
NUM defaults to 1.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-circ-move-next moved 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (num (or num 1))
         (landmark (duo-circ-next moved list num)))
    (duo-sym-teleport-cons-next landmark moved symlist)))

(defun duo-sym-circ-move-before (elem symlist &optional num fn-equal)
  "Move ELEM to NUM previous place in SYMLIST. Return MOVED.
Circular : if in beginning of list, go to the end.
MOVED is the moved value.
NUM defaults to 1.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-circ-move-before moved 'list)
Destructive."
  (let* ((list (symbol-value symlist))
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
    (duo-sym-teleport-cons-previous landmark moved symlist pre-rem pre-ins)))

(defun duo-sym-circ-move-after (elem symlist &optional num fn-equal)
  "Move ELEM to NUM next place in SYMLIST. Return MOVED.
Circular : if in end of list, go to the beginning.
MOVED is the moved value.
NUM defaults to 1.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-circ-move-after moved 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (num (or num 1))
         (moved (duo-member elem list fn-equal))
         (landmark (duo-circ-next moved list num)))
    (duo-sym-teleport-cons-next landmark moved symlist)))

;;; Exchange
;;; ------------------------------------------------------------

(defun duo-sym-exchange-cons (one two symlist &optional pre-one pre-two)
  "Exchange cons ONE and TWO in SYMLIST. Return (ONE . TWO).
ONE and TWO must be cons in SYMLIST.
If non nil, PRE-ONE and PRE-TWO are used to speed up the process.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-exchange-cons one two 'list)
Destructive."
  (unless (eq one two)
    (if (eq two (symbol-value symlist))
        (let ((return)
              (swap))
          (setq return (duo-sym-exchange-cons two one symlist))
          (setq swap (car return))
          (setcar return (cdr return))
          (setcdr return swap)
          return)
      (let* ((list (symbol-value symlist))
             (pre-one (if pre-one
                          pre-one
                        (duo-previous one list)))
             (pre-two (if pre-two
                          pre-two
                        (duo-previous two list))))
        (cond ((eq (cdr one) two)
               (duo-sym-teleport-cons-next two one symlist pre-one))
              ((eq (cdr two) one)
               (duo-sym-teleport-cons-next one two symlist pre-two))
              (t
               (duo-sym-teleport-cons-next one two symlist pre-two)
               (duo-sym-teleport-cons-next pre-two one symlist pre-one)))
        (cons one two)))))

(defun duo-sym-exchange (one two symlist &optional pre-one pre-two fn-equal)
  "Exchange elements ONE and TWO in SYMLIST.
Return (ONE . TWO).
ONE and TWO must be present in SYMLIST.
If non nil, PRE-ONE and PRE-TWO are used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(duo-sym-exchange one two 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (cons-one (duo-member one list fn-equal))
         (cons-two (duo-member two list fn-equal)))
    (duo-sym-exchange-cons cons-one cons-two symlist pre-one pre-two)))

;;; Sorted
;;; ------------------------------------------------------------

(defun duo-sym-insert-in-sorted-list (new symlist &optional fn-less)
  "Insert NEW at the right place in SYMLIST.
The list must be sorted in ascending order.
Return cons of NEW.
FN-LESS takes two arguments and return t if the first is less than the second.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-insert-in-sorted-list new 'list)
Destructive."
  (let ((fn-less (or fn-less #'duo-<))
        (list (symbol-value symlist)))
    (cond ((not list) (set symlist (cons new nil)))
          ((funcall fn-less new (car list)) (duo-sym-push new symlist))
          (t (let ((duo list)
                   (next (cdr list)))
               (while (and duo
                           next
                           (funcall fn-less (car next) new))
                 (setq duo (cdr duo))
                 (setq next (cdr next)))
               (duo-sym-insert-next duo new))))))

;;; Group
;;; ------------------------------

(defun duo-sym-insert-at-group-beg (new symlist &optional fn-group)
  "Insert NEW in SYMLIST at the beginning of its group.
The group is determined by FN-GROUP.
If the group is not found, insert at the beginning of SYMLIST.
Return cons of NEW.
NEW is the value of the element inserted.
FN-GROUP takes two arguments and returns t if they belongs to the same group.
FN-GROUP defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-insert-at-group-beg new 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (previous (duo-before new list 1 fn-group))
         (duo))
    (if previous
        (progn
          (duo-sym-insert-next previous new)
          (setq duo (cdr previous)))
      (duo-sym-push new symlist)
      (setq duo (symbol-value symlist)))
    duo))

(defun duo-sym-insert-at-group-end (new symlist &optional fn-group)
  "Insert NEW in SYMLIST, at the end of its group.
The group is determined by FN-GROUP.
If the group is not found, insert at the end of SYMLIST.
Return cons of NEW.
NEW is the value of the element inserted.
FN-GROUP takes two arguments and returns t if they belongs to the same group.
FN-GROUP defaults to `equal'.
See the docstring of `duo-naive-push' to know why it
uses the list symbol as argument.
Common usage :
\(duo-sym-insert-at-group-end new 'list)
Destructive."
  (let* ((list (symbol-value symlist))
         (fn-group (or fn-group #'equal))
         (previous (duo-member new list fn-group))
         (duo))
    (while (and previous
                (funcall fn-group (car (cdr previous)) new))
      (setq previous (cdr previous)))
    (if previous
        (progn
          (duo-sym-insert-next previous new)
          (setq duo (cdr previous)))
      (setq duo (duo-sym-add new symlist)))
    duo))

;;; End
;;; ------------------------------------------------------------

(provide 'duo-symbol)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; duo-symbol.el ends here
