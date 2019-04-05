;;; duo-return.el --- Return part of duo             -*- lexical-binding: t; -*-

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

;;; Stack & Queue
;;; ------------------------------------------------------------

(defun duo-push-cons (cons list)
  "Add CONS at the beginning of LIST. Return LIST.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-push-cons cons list))
Destructive."
  (setcdr cons list)
  cons)

(defun duo-add-cons (cons list &optional last)
  "Store CONS at the end of LIST. Return CONS.
If non nil, LAST is used to speed up the process.
Destructive."
  (let ((last (if last
                  last
                (duo-last list))))
    (when last
      (setcdr last cons))
    (setcdr cons nil)
    cons))

(defun duo-push (elem list)
  "Add ELEM at the beginning of LIST. Return LIST.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-push elem list))
Destructive."
  (cons elem list))

(defun duo-add (elem list &optional last)
  "Add ELEM at the end of LIST. Return the new LAST.
If non nil, LAST is used to speed up the process.
Destructive."
  (let ((last (if last
                  last
                (duo-last list)))
        (duo (cons elem nil)))
    (when last
      (setcdr last duo))
    duo))

(defun duo-push-new-cons (cons list)
  "Add CONS at the beginning of LIST if not already there. Return LIST.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-push-new elem list))
Destructive."
  (if (duo-inside cons list)
      list
    (duo-push-cons cons list)))

(defun duo-add-new-cons (cons list &optional last)
  "Add CONS at the end of LIST if not already there. Return the new LAST.
If non nil, LAST is used to speed up the process.
Destructive."
  (unless (duo-inside cons list)
    (duo-add-cons cons list last)))

(defun duo-push-new (elem list &optional fn-equal)
  "Add ELEM at the beginning of LIST if not already there. Return LIST.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-push-new elem list))
Destructive."
  (if (duo-member elem list fn-equal)
      list
    (duo-push elem list)))

(defun duo-add-new (elem list &optional last fn-equal)
  "Add ELEM at the end of LIST if not already there.
Return the new LAST.
If non nil, LAST is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (unless (duo-member elem list fn-equal)
    (duo-add elem list last)))

(defun duo-pop (list)
  "Remove the first element of LIST. Return (popped-cons . new-list)
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-pop list))
\(setq popped (car pair))
\(setq list (cdr pair))"
  (let ((popped list)
        (newlist (cdr list)))
    (setcdr popped nil)
    (cons popped newlist)))

(defun duo-drop (list)
  "Remove last element of LIST. Return cons of removed element.
If the list had only one element before the operation,
it must be manually set to nil after that.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq dropped (duo-drop list))
\(when (eq dropped list)
  (setq list nil))
Destructive."
  (let* ((before-last (duo-last list 2))
         (last (cdr before-last)))
    (if last
        (setcdr before-last nil)
      (setq last list))
    last))

(defun duo-push-and-truncate (elem list &optional num)
  "Add ELEM at the beginning of LIST. Truncate LIST to its first NUM elements.
If NUM is nil, do nothing.
Return LIST.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-push-and-truncate elem list))
Destructive."
  (let ((newlist list))
    (setq newlist (duo-push elem list))
    (duo-truncate newlist num)
    newlist))

(defun duo-add-and-clip (elem list &optional num length last)
  "Add ELEM at the end of LIST. Truncate LIST to its last NUM elements.
If NUM is nil, do nothing.
Return (new LAST . LIST).
If non nil, LENGTH and LAST are used to speed up the process.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-add-and-clip elem list num))
\(setq added (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((length (if length
                    length
                  (length list)))
        (added (duo-add elem list last)))
    (when added
      (setq length (1+ length)))
    (while (> length num)
      (setq list (cdr (duo-pop list)))
      (setq length (1- length)))
    (cons added list)))

(defun duo-push-new-and-truncate (elem list &optional num fn-equal)
  "Push ELEM to LIST if not there and truncate to NUM elements.
If NUM is nil, do nothing.
Return LIST.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-push-new-and-truncate elem list))
Destructive."
  (if (duo-member elem list fn-equal)
      list
    (duo-push-and-truncate elem list num)))

(defun duo-add-new-and-clip (elem list &optional num length last fn-equal)
  "Add ELEM to LIST if not there and truncate to NUM elements.
If NUM is nil, do nothing.
Return (new LAST . LIST).
If non nil, LENGTH and LAST are used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (unless (duo-member elem list fn-equal)
    (duo-add-and-clip elem list num length last)))

;;; Rotate <- ->
;;; ------------------------------------------------------------

(defun duo-rotate-left (list)
  "Rotate LIST to the left. Return LIST.
Equivalent to pop first element and add it to the end.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq list (duo-rotate-left list))
Destructive."
  ;; Length list > 1
  (if (cdr list)
      (let* ((pair (duo-pop list))
             (duo (car pair))
             (newlist (cdr pair)))
        (duo-add-cons duo newlist)
        newlist)
    list))

(defun duo-rotate-right (list)
  "Rotate LIST to the right. Return LIST.
Equivalent to drop last element and push it at the beginning.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-rotate-right list))
Destructive."
  ;; Length list > 1
  (if (cdr list)
      (let ((duo (duo-drop list)))
        (duo-push-cons duo list))
    list))

;;; Roll
;;; ------------------------------------------------------------

(defun duo-roll-cons-to-beg (cons list &optional previous)
  "Roll LIST to the left until CONS is at the beginning. Return LIST.
CONS must be a cons in LIST.
If non nil, PREVIOUS is used to speed up the process.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-roll-cons-to-beg cons list))
Destructive."
  (let* ((previous (if previous
                       previous
                     (duo-previous cons list)))
         (last previous))
    (if (and (cdr list)
             previous)
        (progn
          (while (cdr last)
            (setq last (cdr last)))
          (setcdr previous nil)
          (setcdr last list)
          cons)
      list)))

(defun duo-roll-cons-to-end (cons list)
  "Roll LIST to the right until CONS is at the end. Return LIST.
CONS must be a cons in LIST.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-roll-cons-to-end cons list))
Destructive."
  (let* ((next (cdr cons))
         (last next))
    (if (and (cdr list)
             next)
        (progn
          (while (cdr last)
            (setq last (cdr last)))
          (setcdr cons nil)
          (setcdr last list)
          next)
      list)))

(defun duo-roll-to-beg (elem list &optional previous fn-equal)
  "Roll LIST to the left until ELEM is at the beginning. Return LIST.
ELEM must be present in LIST.
If non nil, PREVIOUS is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-roll-to-beg elem list))
Destructive."
  (let* ((previous (if previous
                       previous
                     (duo-before elem list 1 fn-equal)))
         (duo (if previous
                  (cdr previous)
                list)))
    (duo-roll-cons-to-beg duo list previous)))

(defun duo-roll-to-end (elem list &optional fn-equal)
  "Roll LIST to the right until ELEM is at the end. Return LIST.
ELEM must be present in LIST.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-roll-to-end elem list))
Destructive."
  (let ((duo (duo-member elem list fn-equal)))
    (duo-roll-cons-to-end duo list)))

;;; Reverse
;;; ------------------------------------------------------------

(defun duo-reverse (list)
  "Reverse LIST. Return LIST.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-reverse list))
Destructive."
  (let* ((newlist (duo-last list))
         (current newlist)
         (previous (duo-previous current list)))
    (while previous
      (setcdr current previous)
      (setq current previous)
      (setq previous (duo-previous current list)))
    (setcdr current nil)
    newlist))

(defun duo-reverse-previous (cons list)
  "Reverse first part of LIST, from beginning to CONS included.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-reverse-previous cons list))
Destructive."
  (let ((next (cdr cons))
        (newlist))
    (setcdr cons nil)
    (setq newlist (duo-reverse list))
    (setcdr (duo-last newlist) next)
    newlist))

(defun duo-reverse-next (cons list)
  "Reverse second part of LIST, starting just after CONS to end.
Destructive."
  (let ((next (cdr cons))
        (reversed))
    (setcdr cons nil)
    (setq reversed (duo-reverse next))
    (setcdr cons reversed)
    list))

(defun duo-reverse-before (elem list &optional fn-equal)
  "Reverse first part of LIST, from beginning to ELEM included.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-reverse-before elem list))
Destructive."
  (let ((duo (duo-member elem list fn-equal)))
    (duo-reverse-previous duo list)))

(defun duo-reverse-after (elem list &optional fn-equal)
  "Reverse second part of LIST, starting just after ELEM to end.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((duo (duo-member elem list fn-equal)))
    (duo-reverse-next duo list)))

;;; Insert
;;; ------------------------------------------------------------

;;; Cons Cons
;;; ------------------------------

(defun duo-insert-cons-previous (cons new list &optional previous)
  "Insert NEW before CONS in LIST. Return NEW.
CONS must be a cons in LIST.
NEW is the cons inserted.
If non nil, PREVIOUS inserted is used to speed up the process.
If the new cons is inserted at the beginning of the list,
the actual new list must be recovered using new LIST = NEW.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq return (duo-insert-cons-previous cons new list))
\(when (eq (cdr return) list)
  (setq list return))
Destructive."
  (if (eq cons list)
      (duo-push-cons new list)
    (let ((previous (if (and previous
                             (not (eq previous new)))
                        previous
                      (duo-previous cons list))))
      (if previous
          (progn
            (setcdr new (cdr previous))
            (setcdr previous new)
            new)
        nil))))

(defun duo-insert-cons-next (cons new)
  "Insert NEW after CONS in list. Return NEW.
CONS must be a cons in LIST.
NEW is the cons inserted.
Destructive."
    (setcdr new (cdr cons))
    (setcdr cons new)
    new)

;;; Cons Elem
;;; ------------------------------

(defun duo-insert-previous (cons new list &optional previous)
  "Insert NEW before CONS in LIST. Return cons of NEW.
CONS must be a cons in LIST.
NEW is the value of the element inserted.
If non nil, PREVIOUS inserted is used to speed up the process.
If the new cons is inserted at the beginning of the list,
the actual new list must be recovered using new LIST = NEW.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq return (duo-insert-previous cons new list))
\(when (eq (cdr return) list)
  (setq list return))
Destructive."
  (let ((duo (list new)))
    (duo-insert-cons-previous cons duo list previous)))

(defun duo-insert-next (cons new)
  "Insert NEW after CONS in list. Return cons of NEW.
CONS must be a cons in LIST.
NEW is the value of the element inserted.
Destructive."
  (let ((duo (list new)))
    (duo-insert-cons-next cons duo)))

;;; Elem Cons
;;; ------------------------------

(defun duo-insert-cons-before (elem new list &optional previous fn-equal)
  "Insert NEW before ELEM in LIST. Return NEW.
ELEM must be present in list.
NEW is the cons inserted.
If non nil, PREVIOUS inserted is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
If the new cons is inserted at the beginning of the list,
the actual new list must be recovered using new LIST = NEW.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq return (duo-insert-cons-before cons new list))
\(when (eq (cdr return) list)
  (setq list return))
Destructive."
  (let* ((fn-equal (if fn-equal
                         fn-equal
                       #'equal))
         (previous (if previous
                       previous
                     (duo-before elem list 1 fn-equal)))
         (duo (if (funcall fn-equal (car list) elem)
                  list
                (cdr previous))))
    (duo-insert-cons-previous duo new list previous)))

(defun duo-insert-cons-after (elem new list &optional fn-equal)
  "Insert NEW after ELEM in LIST. Return NEW.
ELEM must be present in list.
NEW is the cons inserted.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((duo (duo-member elem list fn-equal)))
    (duo-insert-cons-next duo new)))

;;; Elem Elem
;;; ------------------------------

(defun duo-insert-before (elem new list &optional previous fn-equal)
  "Insert NEW before ELEM in LIST. Return cons of NEW.
ELEM must be present in list.
NEW is the value of the element inserted.
If non nil, PREVIOUS inserted is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
If the new cons is inserted at the beginning of the list,
the actual new list must be recovered using new LIST = NEW.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq return (duo-insert-before cons new list))
\(when (eq (cdr return) list)
  (setq list return))
Destructive."
  (let* ((fn-equal (if fn-equal
                         fn-equal
                       #'equal))
         (previous (if previous
                       previous
                     (duo-before elem list 1 fn-equal)))
         (cons-elem (if (funcall fn-equal (car list) elem)
                        list
                      (cdr previous)))
         (cons-new (list new)))
    (duo-insert-cons-previous cons-elem cons-new list previous)))

(defun duo-insert-after (elem new list &optional fn-equal)
  "Insert NEW after ELEM in LIST. Return cons of NEW.
ELEM must be present in list.
NEW is the value of the element inserted.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Destructive."
  (let ((cons-elem (duo-member elem list fn-equal))
        (cons-new (list new)))
    (duo-insert-cons-next cons-elem cons-new)))

;;; Remove
;;; ------------------------------------------------------------

(defun duo-remove (cons list &optional previous)
  "Remove CONS from LIST. Return (CONS . LIST).
CONS must be a cons in LIST.
If non nil, PREVIOUS removed is used to speed up the process.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-remove cons list))
\(setq removed (car pair))
\(setq list (cdr pair))
Destructive."
  (if (eq cons list)
      (duo-pop list)
    (let ((previous (if (and previous
                             (not (eq previous cons)))
                        previous
                      (duo-previous cons list))))
      (when previous
        (setcdr previous (cdr cons))
        (setcdr cons nil))
      (cons cons list))))

(defun duo-delete (elem list &optional previous fn-equal)
  "Delete ELEM from LIST. Return (removed-cons . LIST).
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
If non nil, PREVIOUS removed is used to speed up the process.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-delete elem list))
\(setq removed (car pair))
\(setq list (cdr pair))
Destructive."
  (let* ((fn-equal (if fn-equal
                         fn-equal
                       #'equal))
         (previous (if previous
                       previous
                     (duo-before elem list 1 fn-equal)))
         (duo (if (funcall fn-equal (car list) elem)
                  list
                (cdr previous))))
    (if (and duo
             list)
        (duo-remove duo list previous)
      (cons nil list))))

(defun duo-delete-all (elem list &optional fn-equal)
  "Delete all elements equals to ELEM from LIST.
Return (list-of-removed-cons . LIST).
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-delete-all elem list))
\(setq removed-list (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((newlist list)
        (pair)
        (removed)
        (removed-list)
        (last)
        (duo)
        (next)
        (fn-equal (if fn-equal
                        fn-equal
                      #'equal)))
    (while (funcall fn-equal (car newlist) elem)
      (setq pair (duo-pop newlist))
      (setq removed (car pair))
      (setq newlist (cdr pair))
      (if removed-list
          (setq last (duo-add-cons removed removed-list last))
        (setq removed-list removed)
        (setq last removed)))
    (setq duo newlist)
    (while duo
      (setq next (cdr duo))
      (when (funcall fn-equal (car duo) elem)
        (setq newlist (cdr (duo-remove duo newlist)))
        (setq removed duo)
        (if removed-list
            (setq last (duo-add-cons removed removed-list last))
          (setq removed-list removed)
          (setq last removed)))
      (setq duo next))
    (cons removed-list newlist)))

;;; Teleport
;;; ------------------------------------------------------------

;;; Cons Cons
;;; ------------------------------

(defun duo-teleport-cons-previous (cons moved list &optional
                                        previous-removed previous-inserted)
  "Move MOVED before CONS in LIST. Return (MOVED . LIST).
CONS must be a cons in LIST.
MOVED is the cons of the moved element.
If non nil, PREVIOUS-REMOVED and PREVIOUS-INSERTED
are used to speed up the process.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-teleport-cons-previous cons moved list))
\(setq moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((newlist list)
        (return))
    (unless (eq cons moved)
      (setq newlist (cdr (duo-remove moved list previous-removed)))
      (setq return (duo-insert-cons-previous cons moved newlist
                                             previous-inserted))
      (when (eq (cdr return) newlist)
        (setq newlist return)))
    (cons moved newlist)))

(defun duo-teleport-cons-next (cons moved list &optional previous)
  "Move MOVED after CONS in LIST. Return (MOVED . LIST).
CONS must be a cons in LIST.
MOVED is the cons of the moved element.
If non nil, PREVIOUS removed is used to speed up the process.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-teleport-cons-next cons moved list))
\(setq moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((newlist list))
    (unless (eq cons moved)
      (setq newlist (cdr (duo-remove moved list previous)))
      (duo-insert-cons-next cons moved))
    (cons moved newlist)))

;;; Cons Elem
;;; ------------------------------

(defun duo-teleport-previous (cons moved list &optional
                                   previous-removed previous-inserted
                                   fn-equal)
  "Move MOVED before CONS in LIST. Return (cons of MOVED . LIST).
CONS must be a cons in LIST.
MOVED is the value of the moved element.
If non nil, PREVIOUS-REMOVED and PREVIOUS-INSERTED
are used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-teleport-previous cons moved list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((duo (duo-member moved list fn-equal)))
    (duo-teleport-cons-previous cons duo list
                                previous-removed previous-inserted)))

(defun duo-teleport-next (cons moved list &optional previous fn-equal)
  "Move MOVED after CONS in LIST. Return (cons of MOVED . LIST).
CONS must be a cons in LIST.
MOVED is the value of the moved element.
If non nil, PREVIOUS removed is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-teleport-next cons moved list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((duo (duo-member moved list fn-equal)))
    (duo-teleport-cons-next cons duo list previous)))

;;; Elem Cons
;;; ------------------------------

(defun duo-teleport-cons-before (elem moved list &optional
                                   previous-removed previous-inserted
                                   fn-equal)
  "Move MOVED before ELEM in LIST. Return (MOVED . LIST).
ELEM must be present in list.
MOVED is the cons of the moved element.
If non nil, PREVIOUS-REMOVED and PREVIOUS-INSERTED
are used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-teleport-before elem moved list))
\(setq moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((duo (duo-member elem list fn-equal)))
    (duo-teleport-cons-previous duo moved list
                                previous-removed previous-inserted)))

(defun duo-teleport-cons-after (elem moved list &optional previous fn-equal)
  "Move MOVED after ELEM in LIST. Return (MOVED . LIST).
ELEM must be present in list.
MOVED is the cons of the moved element.
If non nil, PREVIOUS removed is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-teleport-after elem moved list))
\(setq moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((duo (duo-member elem list fn-equal)))
    (duo-teleport-cons-next duo moved list previous)))

;;; Elem Elem
;;; ------------------------------

(defun duo-teleport-before (elem moved list &optional
                                 previous-removed previous-inserted
                                 fn-equal)
  "Move MOVED before ELEM in LIST. Return (cons of MOVED . LIST).
ELEM must be present in list.
MOVED is the value of the moved element.
If non nil, PREVIOUS-REMOVED and PREVIOUS-INSERTED
are used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-teleport-before elem moved list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((elem-cons (duo-member elem list fn-equal))
        (moved-cons (duo-member moved list fn-equal)))
    (duo-teleport-cons-previous elem-cons moved-cons list
                                previous-removed previous-inserted)))

(defun duo-teleport-after (elem moved list &optional previous fn-equal)
  "Move MOVED after ELEM in LIST. Return (cons of MOVED . LIST).
ELEM must be present in list.
MOVED is the value of the moved element.
If non nil, PREVIOUS removed is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-teleport-after elem moved list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((elem-cons (duo-member elem list fn-equal))
        (moved-cons (duo-member moved list fn-equal)))
    (duo-teleport-cons-next elem-cons moved-cons list previous)))

;;; Move
;;; ------------------------------------------------------------

;;; Step
;;; ------------------------------

(defun duo-move-previous (moved list &optional num)
  "Move MOVED to NUM previous place in LIST. Return (MOVED . LIST).
If range is exceeded, move MOVED at the beginning of the list.
MOVED must be a cons in LIST.
NUM defaults to 1.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-move-previous moved list))
\(setq moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let* ((num (if num
                  num
                1))
         (pre-ins (duo-previous moved list (1+ num)))
         (landmark (if pre-ins
                       (cdr pre-ins)
                     list))
         (pre-rem (if pre-ins
                      (nthcdr num pre-ins)
                    (duo-previous moved list))))
    (duo-teleport-cons-previous landmark moved list pre-rem pre-ins)))

(defun duo-move-next (moved list &optional num)
  "Move MOVED to NUM next place in LIST. Return (MOVED . LIST).
If range is exceeded, move MOVED at the end of the list.
MOVED must be a cons in LIST.
NUM defaults to 1.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-move-next moved list))
\(setq moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let* ((num (if num
                  num
                1))
         (landmark (nthcdr num moved)))
    (unless landmark
      (setq landmark (duo-last list)))
    (duo-teleport-cons-next landmark moved list)))

(defun duo-move-before (elem list &optional num fn-equal)
  "Move ELEM to NUM previous place in LIST. Return (MOVED . LIST).
If range is exceeded, move ELEM at the beginning of the list.
MOVED is the moved value.
NUM defaults to 1.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-move-before elem list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let* ((num (if num
                  num
                1))
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
    (duo-teleport-cons-previous landmark moved list pre-rem pre-ins)))

(defun duo-move-after (elem list &optional num fn-equal)
  "Move ELEM to NUM next place in LIST. Return (MOVED . LIST).
If range is exceeded, move MOVED at the end of the list.
MOVED is the moved value.
NUM defaults to 1.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-move-after elem list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let* ((num (if num
                  num
                1))
         (moved (duo-member elem list fn-equal))
         (landmark (nthcdr num moved)))
    (unless landmark
      (setq landmark (duo-last list)))
    (duo-teleport-cons-next landmark moved list)))

;;; Circular
;;; ------------------------------

(defun duo-circ-move-previous (moved list &optional num)
  "Move MOVED to NUM previous place in LIST. Return (MOVED . LIST).
Circular : if in beginning of list, go to the end.
MOVED must be a cons in LIST.
NUM defaults to 1.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-circ-move-previous moved list))
\(setq moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let* ((num (if num
                  num
                1))
         (pre-ins (duo-circ-previous moved list (1+ num)))
         (landmark (duo-circ-next pre-ins list))
         (pre-rem (duo-circ-next pre-ins list num)))
    (when (eq landmark list)
      (setq pre-ins nil))
    (when (eq moved pre-ins)
      (setq pre-ins nil))
    (duo-teleport-cons-previous landmark moved list pre-rem pre-ins)))

(defun duo-circ-move-next (moved list &optional num)
  "Move MOVED to NUM next place in LIST. Return (MOVED . LIST).
Circular : if in end of list, go to the beginning.
MOVED must be a cons in LIST.
NUM defaults to 1.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-circ-move-next moved list))
\(setq moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let* ((num (if num
                  num
                1))
         (landmark (duo-circ-next moved list num)))
    (duo-teleport-cons-next landmark moved list)))

(defun duo-circ-move-before (elem list &optional num fn-equal)
  "Move ELEM to NUM previous place in LIST. Return (MOVED . LIST).
Circular : if in beginning of list, go to the end.
MOVED is the moved value.
NUM defaults to 1.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-move-before elem list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let* ((num (if num
                  num
                1))
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
    (duo-teleport-cons-previous landmark moved list pre-rem pre-ins)))

(defun duo-circ-move-after (elem list &optional num fn-equal)
  "Move ELEM to NUM next place in LIST. Return (MOVED . LIST).
Circular : if in end of list, go to the beginning.
MOVED is the moved value.
NUM defaults to 1.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-move-after elem list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let* ((num (if num
                  num
                1))
         (moved (duo-member elem list fn-equal))
         (landmark (duo-circ-next moved list num)))
    (duo-teleport-cons-next landmark moved list)))

;;; Exchange
;;; ------------------------------------------------------------

(defun duo-exchange-cons (one two list &optional pre-one pre-two)
  "Exchange cons ONE and TWO in LIST. Return ((ONE . TWO) . LIST).
ONE and TWO must be cons in LIST.
If non nil, PRE-ONE and PRE-TWO are used to speed up the process.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq structure (duo-exchange-cons one two list))
\(setq one (car (car structure)))
\(setq two (cdr (car structure)))
\(setq list (cdr structure))
Destructive."
  (unless (eq one two)
    (if (eq two list)
        (let ((return)
              (swap))
          (setq return (duo-exchange-cons two one list))
          (setq swap (car (car return)))
          (setcar (car return) (cdr (car return)))
          (setcdr (car return) swap)
          return)
      (let ((newlist list)
            (pre-one (if pre-one
                         pre-one
                       (duo-previous one list)))
            (pre-two (if pre-two
                         pre-two
                       (duo-previous two list)))
            (pair))
        (cond ((eq (cdr one) two)
               (setq pair (duo-teleport-cons-next two one newlist pre-one)))
              ((eq (cdr two) one)
               (setq pair (duo-teleport-cons-next one two newlist pre-two)))
              (t
               (setq pair (duo-teleport-cons-next one two newlist pre-two))
               (setq newlist (cdr pair))
               (setq pair (duo-teleport-cons-next pre-two one newlist pre-one))))
        (setq newlist (cdr pair))
        (cons (cons one two) newlist)))))

(defun duo-exchange (one two list &optional pre-one pre-two fn-equal)
  "Exchange elements ONE and TWO in LIST. Return ((ONE . TWO) . LIST).
ONE and TWO must be present in LIST.
If non nil, PRE-ONE and PRE-TWO are used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq structure (duo-exchange-cons one two list))
\(setq one (car (car structure)))
\(setq two (cdr (car structure)))
\(setq list (cdr structure))
Destructive."
  (let ((cons-one (duo-member one list fn-equal))
        (cons-two (duo-member two list fn-equal)))
    (duo-exchange-cons cons-one cons-two list pre-one pre-two)))

;;; Sorted
;;; ------------------------------------------------------------

(defun duo-insert-in-sorted-list (new list &optional fn-less)
  "Insert NEW at the right place in LIST.
LIST must be sorted in ascending order.
Return cons of NEW.
FN-LESS takes two arguments and return t if the first is less than the second.
FN-LESS defaults to <
If the new cons is inserted at the beginning of the list,
the actual new list must be recovered using new LIST = NEW.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq return (duo-insert-in-sorted-list cons new list))
\(when (eq (cdr return) list)
  (setq list return))
Destructive."
  (let ((fn-less (if fn-less
                     fn-less
                   #'<)))
    (cond ((not list) (cons new nil))
          ((funcall fn-less new (car list)) (duo-push new list))
          (t (let ((duo list)
                   (next (cdr list)))
               (while (and duo
                           next
                           (funcall fn-less (car next) new))
                 (setq duo (cdr duo))
                 (setq next (cdr next)))
               (duo-insert-next duo new))))))

;;; Group
;;; ------------------------------

(defun duo-insert-at-group-beg (new list &optional fn-group)
  "Insert NEW in LIST, at the beginning of a group determined by FN-GROUP.
If the group is not found, insert at the beginning of LIST.
Return (cons of NEW . LIST).
NEW is the value of the element inserted.
FN-GROUP takes two arguments and returns t if they belongs to the same group.
FN-GROUP defaults to `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-insert-at-group-beg new list))
\(setq cons-inserted (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((newlist list)
        (previous (duo-before new list 1 fn-group))
        (duo))
    (if previous
        (progn
          (duo-insert-next previous new)
          (setq duo (cdr previous)))
      (setq newlist (duo-push new list))
      (setq duo newlist))
    (cons duo newlist)))

(defun duo-insert-at-group-end (new list &optional fn-group)
  "Insert NEW in LIST, at the end of a group determined by FN-GROUP.
If the group is not found, insert at the end of LIST.
Return (cons of NEW . LIST).
NEW is the value of the element inserted.
FN-GROUP takes two arguments and returns t if they belongs to the same group.
FN-GROUP defaults to `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-insert-at-group-end new list))
\(setq cons-inserted (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((fn-group (if fn-group
                        fn-group
                      #'equal))
        (newlist list)
        (previous (duo-member new list fn-group))
        (duo))
    (while (and previous
                (funcall fn-group (car (cdr previous)) new))
      (setq previous (cdr previous)))
    (if previous
        (progn
          (duo-insert-next previous new)
          (setq duo (cdr previous)))
      (setq duo (duo-add new newlist)))
    (unless newlist
      (setq newlist duo))
    (cons duo newlist)))

;;; End
;;; ------------------------------------------------------------

(provide 'duo-return)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; duo-return.el ends here