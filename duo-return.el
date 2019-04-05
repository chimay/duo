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

;;; End
;;; ------------------------------------------------------------

(provide 'duo-return)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; duo-return.el ends here
