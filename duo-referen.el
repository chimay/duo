;;; duo-referen.el --- Reference part of duo         -*- lexical-binding: t; -*-

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
  (when (consp list)
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
  "Add ELEM at the end of car of REFlist referenced by REFLIST.
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
  (let ((list (duo-deref reflist)))
    ;; Length list > 1
    (when (cdr list)
      (let ((popped (duo-ref-pop reflist)))
        (setq list (duo-deref reflist))
        (duo-add-cons popped list))))
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

(defun duo-ref-roll-to-beg (elem reflist &optional previous fn-equal)
  "Roll list referenced by REFLIST to the left until ELEM is at the beginning.
Return list referenced by REFLIST.
ELEM must be present in list referenced by REFLIST.
If non nil, PREVIOUS is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
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
         (previous (if previous
                       previous
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

;;; End
;;; ------------------------------------------------------------

(provide 'duo-referen)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; duo-referen.el ends here
