;;; duo.el --- In place list operations in Elisp     -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Chimay

;; Author : Chimay
;; Name: Duo
;; Package-Version: 1.2
;; Package-requires: ((emacs "26"))
;; Keywords: lisp, extensions, list, in-place, operation
;; URL: https://github.com/chimay/duo

;;; Commentary:

;; Library of in place list operations in Emacs-Lisp.
;;
;; Cons DUO = (CAR . CDR) can be used as double pointer
;; with setcar and setcdr
;;
;; ELEM = (car DUO)          -> straightforward
;; DUO  = (member ELEM LIST) -> needs loop
;;
;; Duo is a library of in place list operations in Emacs-Lisp.
;; Its functions modify the original list when :
;;
;;   - It’s easy to get back : rotate, reverse, etc
;;   - The name is clear : push, pop, add, drop, insert, remove, etc
;;     + When an element is removed, a reference to it is often returned
;;
;; However, when it’s difficult or impossible to reverse the operation, a
;; new list is created, with references to the elements of the original
;; list when possible. For instance :
;;
;;   - filter
;;   - partition
;;
;; If in doubt, check their doc.
;;
;; In fact, the functions use and return references whenever possible.
;; It’s implemented with (CAR . CDR) cons, which are called duo,
;; hence the name of the library. These cons are everywhere in Elisp,
;; either explicitely created or in lists. A list variable <list> is
;; itself the cons at the beginning of the list. (cdr list) is the
;; second cons in the list. And so one with (cddr list), (nthcdr N list).
;; Generally speaking, a member of a list is a cons
;; (value . next-member-in-list). Most of Elisp is built around these
;; (CAR . CDR) double pointers. You can even construct binary trees with it.
;;
;; When a =duo= has to be modified, it’s generally by =setcar= and
;; =setcdr= built-in.
;;
;; Caution : apply some of these functions to circular lists would
;; produce infinite loops.
;;
;; However, some functions, like =*-circ-*= or =*-rotate-*=, simulate
;; circular lists by :
;;
;;   - Continuing at the beginning once arrived at the end
;;   - Continuing at the end once arrived at the beginning

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

;;; Pointers
;;; ------------------------------------------------------------

(defun duo-set-deref (ptr object)
  "Change the content of the variable referenced by PTR to OBJECT.
OBJECT must be a cons or a list."
  (setcar ptr (car object))
  (setcdr ptr (cdr object))
  ptr)

;;; Predicates
;;; ------------------------------------------------------------

;;; Level 1
;;; ------------------------------

(defun duo-equal-car-p (one two)
  "Whether the car of ONE and TWO are equal."
  (equal (car one) (car two)))

(defun duo-equal-cdr-p (one two)
  "Whether the cdr of ONE and TWO are equal."
  (equal (cdr one) (cdr two)))

;;; Level 2
;;; ------------------------------

(defun duo-equal-caar-p (one two)
  "Whether the caar of ONE and TWO are equal."
  (equal (caar one) (caar two)))

(defun duo-equal-cdar-p (one two)
  "Whether the cdar of ONE and TWO are equal."
  (equal (cdar one) (cdar two)))

(defun duo-equal-cadr-p (one two)
  "Whether the cadr of ONE and TWO are equal."
  (equal (cadr one) (cadr two)))

(defun duo-equal-cddr-p (one two)
  "Whether the cddr of ONE and TWO are equal."
  (equal (cddr one) (cddr two)))

;;; Find
;;; ------------------------------------------------------------

(defun duo-at-index (index list)
  "Element at INDEX in LIST."
  (nthcdr index list))

(defun duo-inside (cons list)
  "Return CONS if CONS is in LIST or nil otherwise."
  (let ((duo list))
    (while (and duo
                (not (eq duo cons)))
      (setq duo (cdr duo)))
    duo))

(defun duo-member (elem list &optional test-equal)
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

(defun duo-index-member (elem list &optional test-equal)
  "Return (index . cons) of ELEM in LIST.
ELEM must be present in list.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (let ((duo list)
        (test-equal (if test-equal
                        test-equal
                      #'equal))
        (index 0))
    (while (and duo
                (not (funcall test-equal (car duo) elem)))
      (setq duo (cdr duo))
      (setq index (1+ index)))
    (if duo
        (cons index duo)
      nil)))

(defun duo-last (list &optional num)
  "Return cons starting a sublist of NUM elements at the end of LIST.
NUM defaults to 1 : NUM nil means return cons of last element in LIST."
  (let ((num (if num
                 num
               1))
        (last list))
    (while (nthcdr num last)
      (setq last (cdr last)))
    last))

;;; Assoc
;;; ------------------------------

(defun duo-assoc (key list &optional test-equal)
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

(defun duo-reverse-assoc (value list &optional test-equal)
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

;;; Next / Previous
;;; ------------------------------------------------------------

(defun duo-previous (cons list &optional num)
  "Return cons of NUM elements before CONS in LIST.
NUM defaults to 1.
CONS must be a cons in LIST."
  (let* ((num (if num
                  num
                1))
         (duo list)
         (scout (nthcdr num duo)))
    (while (and duo
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
  (let ((num (if num
                 num
               1)))
    (nthcdr num cons)))

(defun duo-before (elem list &optional num test-equal)
  "Return cons of NUM elements before ELEM in LIST.
NUM defaults to 1.
ELEM must be present in list.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (let ((test-equal (if test-equal
                        test-equal
                      #'equal)))
    (let* ((num (if num
                    num
                  1))
           (duo list)
           (scout (nthcdr num duo)))
      (while (and duo
                  (not (funcall test-equal (car scout) elem))
                  (not (funcall test-equal (car duo) elem)))
        (setq duo (cdr duo))
        (setq scout (cdr scout)))
      (if (funcall test-equal (car scout) elem)
          duo
        nil))))

(defun duo-after (elem list &optional num test-equal)
  "Return cons of NUM elements after ELEM in LIST.
NUM defaults to 1.
ELEM must be present in list.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (let ((num (if num
                 num
               1)))
    (nthcdr num (duo-member elem list test-equal))))

;;; Circular
;;; ------------------------------

(defun duo-circ-previous (cons list &optional num)
  "Return cons of NUM elements before CONS in LIST.
Circular : if in beginning of list, go to the end.
NUM defaults to 1.
CONS must be a cons in LIST.
Test with eq."
  (let* ((num (if num
                  num
                1))
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
    (if (eq scout cons)
        duo
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
  (let ((num (if num
                 num
               1))
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

(defun duo-circ-before (elem list &optional num test-equal)
  "Return cons of NUM elements before ELEM in LIST.
Circular : if in beginning of list, go to the end.
NUM defaults to 1.
ELEM must be present in list.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (let* ((num (if num
                  num
                1))
         (duo list)
         (scout (nthcdr num duo))
         (test-equal (if test-equal
                         test-equal
                       #'equal))
         (iter 0))
    (unless scout
      (setq num (mod num (length list)))
      (setq scout (nthcdr num duo)))
    (while (and duo
                (not (funcall test-equal (car scout) elem))
                (not (funcall test-equal (car duo) elem)))
      (setq duo (cdr duo))
      (setq scout (cdr scout))
      (setq iter (1+ iter)))
    (unless (funcall test-equal (car scout) elem)
      (setq duo list)
      (setq scout (nthcdr (- num iter) duo))
      (while (and duo
                  scout)
        (setq duo (cdr duo))
        (setq scout (cdr scout))))
    duo))

(defun duo-circ-after (elem list &optional num test-equal)
  "Return cons of NUM elements after ELEM in LIST.
Circular : if in end of list, go to the beginning.
NUM defaults to 1.
ELEM must be present in list.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (duo-circ-next (duo-member elem list test-equal) list num))

;;; Change
;;; ------------------------------------------------------------

(defun duo-update (old new list &optional test-equal)
  "Replace OLD by NEW in LIST. Return cons of NEW.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
Destructive."
  (let ((duo (duo-member old list test-equal)))
    (when duo
      (setcar duo new))
    duo))

;;; Add / Remove at Beg / End
;;; ------------------------------------------------------------

(defun duo-naive-push ()
  "Do not use it on any LIST !
Adding a cons before the first one without returning the updated list
does not work.
The calling scope list var holds the address of the first cons of the list.
The argument list var holds a copy of this address.
Using (setq list ...) inside the defun changes the argument list reference,
not the calling scope one. So, the calling scope address remains the same,
which becomes the address of the second cons of the list."
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
which becomes the address of the removed cons."
  ;; (let ((popped list))
  ;;   (setq list (cdr list))
  ;;   (setcdr popped nil)
  ;;   popped)
  )

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
    (setcdr last cons)
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
    (setcdr last duo)
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

(defun duo-push-new (elem list &optional test-equal)
  "Add ELEM at the beginning of LIST if not already there. Return LIST.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-push-new elem list))
Destructive."
  (if (duo-member elem list test-equal)
      list
    (duo-push elem list)))

(defun duo-add-new (elem list &optional last test-equal)
  "Add ELEM at the end of LIST if not already there.
Return the new LAST or the member found.
If non nil, LAST is used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
Destructive."
  (unless (duo-member elem list test-equal)
    (duo-add elem list last)))

(defun duo-pop (list)
  "Remove the first element of LIST. Return (popped-cons . new-list)
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-pop list))
\(setq popped (car pair))
\(setq list (cdr pair))
That’s all folks."
  (let ((popped list)
        (newlist (cdr list)))
    (setcdr popped nil)
    (cons popped newlist)))

(defun duo-drop (list)
  "Remove last element of LIST. Return cons of removed element.
Destructive."
  (let* ((before-last (duo-last list 2))
         (last (cdr before-last)))
    (if last
        (setcdr before-last nil)
      ;; One element list
      (setq last (cons (car list) nil))
      (setcar list nil))
    last))

(defun duo-truncate (list &optional num)
  "Truncate LIST to its first NUM elements. Return removed part.
Destructive."
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

(defun duo-push-and-truncate (elem list &optional num)
  "Add ELEM at the beginning of LIST. Truncate LIST to NUM elements.
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

;;; Reference
;;; ------------------------------

(defun duo-ref-push-cons (cons reflist)
  "Add CONS at the beginning of the car of REFLIST. Return car of REFLIST.
REFLIST must be a cons (list . whatever-you-want)
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))          ; this
\(setq reflist (cons mylist nil))      ; or that
\(setq reflist (cons mylist whatever)) ; or that
;; Modify
\(duo-ref-push-cons reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (setcdr cons (car reflist))
  (setcar reflist cons)
  (car reflist))

(defun duo-ref-push (elem reflist)
  "Add ELEM at the beginning of the car of REFLIST. Return car of REFLIST.
REFLIST must be a cons (list . whatever-you-want)
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-push reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let ((duo (cons elem (car reflist))))
    (setcar reflist duo)
    (car reflist)))

(defun duo-ref-pop (reflist)
  "Remove first element in the car of REFLIST. Return popped cons.
REFLIST must be a cons (list . whatever-you-want)
See the docstring of `duo-naive-pop' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))          ; this
\(setq reflist (cons mylist nil))      ; or that
\(setq reflist (cons mylist whatever)) ; or that
;; Modify
\(setq popped (duo-ref-pop reflist))
;; Update list
\(setq mylist (car reflist))
That’s all folks."
  (let* ((list (car reflist))
         (popped list))
    (setcar reflist (cdr list))
    (setcdr popped nil)
    popped))

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

;;; Reference
;;; ------------------------------

(defun duo-ref-rotate-left (reflist)
  "Rotate car of REFLIST to the left. Return car of REFLIST.
REFLIST must be a cons (list . whatever-you-want)
Equivalent to pop first element and add it to the end.
See the docstring of `duo-naive-pop' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-rotate-left reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let ((list (car reflist)))
    ;; Length list > 1
    (when (cdr list)
      (let ((popped (duo-ref-pop reflist)))
        (setq list (car reflist))
        (duo-add-cons popped list))))
  (car reflist))

(defun duo-ref-rotate-right (reflist)
  "Rotate car of REFLIST to the right. Return car of REFLIST.
REFLIST must be a cons (list . whatever-you-want)
Equivalent to drop last element and push it at the beginning.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-rotate-right reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let ((list (car reflist)))
    ;; Length list > 1
    (when (cdr list)
      (let ((dropped (duo-drop list)))
        (duo-ref-push-cons dropped reflist))))
  (car reflist))

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

(defun duo-roll-to-beg (elem list &optional previous test-equal)
  "Roll LIST to the left until ELEM is at the beginning. Return LIST.
ELEM must be present in LIST.
If non nil, PREVIOUS is used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-roll-to-beg elem list))
Destructive."
  (let* ((previous (if previous
                       previous
                     (duo-before elem list 1 test-equal)))
         (duo (if previous
                  (cdr previous)
                list)))
    (duo-roll-cons-to-beg duo list previous)))

(defun duo-roll-to-end (elem list &optional test-equal)
  "Roll LIST to the right until ELEM is at the end. Return LIST.
ELEM must be present in LIST.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-roll-to-end elem list))
Destructive."
  (let ((duo (duo-member elem list test-equal)))
    (duo-roll-cons-to-end duo list)))

;;; Reference
;;; ------------------------------

(defun duo-ref-roll-cons-to-beg (cons reflist &optional previous)
  "Roll car of REFLIST to the left until CONS is at the beginning.
Return car of REFLIST.
CONS must be a cons in car of REFLIST.
If non nil, PREVIOUS is used to speed up the process.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-roll-cons-to-beg cons reflist)
;; Update list
\(setq mylist (car reflist))
Modifies car of REFLIST."
  (let* ((list (car reflist))
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
      (setcar reflist cons)))
  (car reflist))

(defun duo-ref-roll-cons-to-end (cons reflist)
  "Roll car of REFLIST to the right until CONS is at the end.
Return car of REFLIST.
CONS must be a cons in car of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-roll-cons-to-end cons reflist)
;; Update list
\(setq mylist (car reflist))
Modifies car of REFLIST."
  (let* ((list (car reflist))
         (next (cdr cons))
         (last next))
    (when (and (cdr list)
               next)
      (while (cdr last)
        (setq last (cdr last)))
      (setcdr cons nil)
      (setcdr last list)
      (setcar reflist next)))
  (car reflist))

(defun duo-ref-roll-to-beg (elem reflist &optional previous test-equal)
  "Roll car of REFLIST to the left until ELEM is at the beginning.
Return car of REFLIST.
ELEM must be present in car of REFLIST.
If non nil, PREVIOUS is used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-roll-to-beg elem reflist)
;; Update list
\(setq mylist (car reflist))
Modifies car of REFLIST."
  (let* ((list (car reflist))
         (previous (if previous
                       previous
                     (duo-before elem list 1 test-equal)))
         (duo (if previous
                  (cdr previous)
                list)))
    (duo-ref-roll-cons-to-beg duo reflist previous)))

(defun duo-ref-roll-to-end (elem reflist &optional test-equal)
  "Roll car of REFLIST to the right until ELEM is at the end.
Return car of REFLIST.
ELEM must be present in car of REFLIST.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-roll-to-end elem reflist)
;; Update list
\(setq mylist (car reflist))
Modifies car of REFLIST."
  (let* ((list (car reflist))
         (duo (duo-member elem list test-equal)))
    (duo-ref-roll-cons-to-end duo reflist)))

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

;;; Reference
;;; ------------------------------

(defun duo-ref-reverse (reflist)
  "Reverse car of REFLIST. Return car of REFLIST.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-reverse reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (newlist (duo-last list))
         (current newlist)
         (previous (duo-previous current list)))
    (while previous
      (setcdr current previous)
      (setq current previous)
      (setq previous (duo-previous current list)))
    (setcdr current nil)
    (setcar reflist newlist)
    newlist))

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

(defun duo-insert-cons-before (elem new list &optional previous test-equal)
  "Insert NEW before ELEM in LIST. Return NEW.
ELEM must be present in list.
NEW is the cons inserted.
If non nil, PREVIOUS inserted is used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
If the new cons is inserted at the beginning of the list,
the actual new list must be recovered using new LIST = NEW.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq return (duo-insert-cons-before cons new list))
\(when (eq (cdr return) list)
  (setq list return))
Destructive."
  (let* ((test-equal (if test-equal
                         test-equal
                       #'equal))
         (previous (if previous
                       previous
                     (duo-before elem list 1 test-equal)))
         (duo (if (funcall test-equal (car list) elem)
                  list
                (cdr previous))))
    (duo-insert-cons-previous duo new list previous)))

(defun duo-insert-cons-after (elem new list &optional test-equal)
  "Insert NEW after ELEM in LIST. Return NEW.
ELEM must be present in list.
NEW is the cons inserted.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
Destructive."
  (let ((duo (duo-member elem list test-equal)))
    (duo-insert-cons-next duo new)))

;;; Elem Elem
;;; ------------------------------

(defun duo-insert-before (elem new list &optional previous test-equal)
  "Insert NEW before ELEM in LIST. Return cons of NEW.
ELEM must be present in list.
NEW is the value of the element inserted.
If non nil, PREVIOUS inserted is used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
If the new cons is inserted at the beginning of the list,
the actual new list must be recovered using new LIST = NEW.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq return (duo-insert-before cons new list))
\(when (eq (cdr return) list)
  (setq list return))
Destructive."
  (let* ((test-equal (if test-equal
                         test-equal
                       #'equal))
         (previous (if previous
                       previous
                     (duo-before elem list 1 test-equal)))
         (cons-elem (if (funcall test-equal (car list) elem)
                        list
                      (cdr previous)))
         (cons-new (list new)))
    (duo-insert-cons-previous cons-elem cons-new list previous)))

(defun duo-insert-after (elem new list &optional test-equal)
  "Insert NEW after ELEM in LIST. Return cons of NEW.
ELEM must be present in list.
NEW is the value of the element inserted.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
Destructive."
  (let ((cons-elem (duo-member elem list test-equal))
        (cons-new (list new)))
    (duo-insert-cons-next cons-elem cons-new)))

;;; Reference
;;; ------------------------------

;;; Cons Cons
;;; ---------------

(defun duo-ref-insert-cons-previous (cons new reflist &optional previous)
  "Insert NEW before CONS in car of REFLIST. Return NEW.
CONS must be a cons in car of REFLIST.
NEW is the cons inserted.
REFLIST must be a cons (list . whatever-you-want)
If non nil, PREVIOUS inserted is used to speed up the process.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-insert-cons-previous cons new reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let ((list (car reflist)))
    (if (eq cons list)
        (duo-ref-push-cons new reflist)
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

;;; Cons Elem
;;; ---------------

(defun duo-ref-insert-previous (cons new reflist &optional previous)
  "Insert NEW before CONS in car of REFLIST. Return cons of NEW.
CONS must be a cons in car of REFLIST.
NEW is the value of the element inserted.
REFLIST must be a cons (list . whatever-you-want)
If non nil, PREVIOUS inserted is used to speed up the process.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-insert-previous cons new reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let ((duo (list new)))
    (duo-ref-insert-cons-previous cons duo reflist previous)))

;;; Elem Cons
;;; ---------------

(defun duo-ref-insert-cons-before (elem new reflist &optional previous test-equal)
  "Insert NEW before ELEM in car of REFLIST. Return NEW.
ELEM must be present in list.
NEW is the cons inserted.
REFLIST must be a cons (list . whatever-you-want)
If non nil, PREVIOUS inserted is used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-insert-cons-before elem new reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((test-equal (if test-equal
                         test-equal
                       #'equal))
         (list (car reflist))
         (previous (if previous
                       previous
                     (duo-before elem list 1 test-equal)))
         (duo (if (funcall test-equal (car list) elem)
                  list
                (cdr previous))))
    (duo-ref-insert-cons-previous duo new reflist previous)))

;;; Elem Elem
;;; ---------------

(defun duo-ref-insert-before (elem new reflist &optional previous test-equal)
  "Insert NEW before ELEM in car of REFLIST. Return cons of NEW.
ELEM must be present in list.
NEW is the value of the element inserted.
REFLIST must be a cons (list . whatever-you-want)
If non nil, PREVIOUS inserted is used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-insert-before elem new reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((test-equal (if test-equal
                         test-equal
                       #'equal))
         (list (car reflist))
         (previous (if previous
                       previous
                     (duo-before elem list 1 test-equal)))
         (cons-elem (if (funcall test-equal (car list) elem)
                        list
                      (cdr previous)))
         (cons-new (list new)))
    (duo-ref-insert-cons-previous cons-elem cons-new reflist previous)))

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

(defun duo-delete (elem list &optional previous test-equal)
  "Delete ELEM from LIST. Return (removed-cons . LIST).
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
If non nil, PREVIOUS removed is used to speed up the process.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-delete elem list))
\(setq removed (car pair))
\(setq list (cdr pair))
Destructive."
  (let* ((test-equal (if test-equal
                         test-equal
                       #'equal))
         (previous (if previous
                       previous
                     (duo-before elem list 1 test-equal)))
         (duo (if (funcall test-equal (car list) elem)
                  list
                (cdr previous))))
    (duo-remove duo list previous)))

(defun duo-delete-all (elem list &optional test-equal)
  "Delete all elements equals to ELEM from LIST.
Return (list-of-removed-cons . LIST).
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
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
        (test-equal (if test-equal
                        test-equal
                      #'equal)))
    (while (funcall test-equal (car newlist) elem)
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
      (when (funcall test-equal (car duo) elem)
        (setq newlist (cdr (duo-remove duo newlist)))
        (setq removed duo)
        (if removed-list
            (setq last (duo-add-cons removed removed-list last))
          (setq removed-list removed)
          (setq last removed)))
      (setq duo next))
    (cons removed-list newlist)))

;;; Reference
;;; ------------------------------

(defun duo-ref-remove (cons reflist &optional previous)
  "Remove CONS from car of REFLIST. Return CONS.
CONS must be a cons in car of REFLIST.
REFLIST must be a cons (list . whatever-you-want)
If non nil, PREVIOUS removed is used to speed up the process.
See the docstring of `duo-naive-pop' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-remove cons reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let ((list (car reflist)))
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

(defun duo-ref-delete (elem reflist &optional previous test-equal)
  "Delete ELEM from car of REFLIST. Return removed cons.
REFLIST must be a cons (list . whatever-you-want)
If non nil, PREVIOUS deleted is used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-pop' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-delete elem reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (test-equal (if test-equal
                         test-equal
                       #'equal))
         (previous (if previous
                       previous
                     (duo-before elem list 1 test-equal)))
         (duo (if (funcall test-equal (car list) elem)
                  list
                (cdr previous))))
    (duo-ref-remove duo reflist previous)))

(defun duo-ref-delete-all (elem reflist &optional test-equal)
  "Delete all elements equals to ELEM from car of REFLIST.
Return list of removed cons.
REFLIST must be a cons (list . whatever-you-want)
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-pop' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-delete-all elem reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let ((removed)
        (removed-list)
        (last)
        (list)
        (duo)
        (next)
        (test-equal (if test-equal
                        test-equal
                      #'equal)))
    (while (funcall test-equal (car (car reflist)) elem)
      (setq removed (duo-ref-pop reflist))
      (if removed-list
          (setq last (duo-add-cons removed removed-list last))
        (setq removed-list removed)
        (setq last removed)))
    (setq list (car reflist))
    (setq duo list)
    (while duo
      (setq next (cdr duo))
      (when (funcall test-equal (car duo) elem)
        (setq list (car (duo-ref-remove duo reflist)))
        (setq removed duo)
        (if removed-list
            (setq last (duo-add-cons removed removed-list last))
          (setq removed-list removed)
          (setq last removed)))
      (setq duo next))
    removed-list))

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
The actual new list must be recovered using the returned list.
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
The actual new list must be recovered using the returned list.
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
                                   test-equal)
  "Move MOVED before CONS in LIST. Return (cons of MOVED . LIST).
CONS must be a cons in LIST.
MOVED is the value of the moved element.
If non nil, PREVIOUS-REMOVED and PREVIOUS-INSERTED
are used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-teleport-previous cons moved list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((duo (duo-member moved list test-equal)))
    (duo-teleport-cons-previous cons duo list
                                previous-removed previous-inserted)))

(defun duo-teleport-next (cons moved list &optional previous test-equal)
  "Move MOVED after CONS in LIST. Return (cons of MOVED . LIST).
CONS must be a cons in LIST.
MOVED is the value of the moved element.
If non nil, PREVIOUS removed is used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-teleport-next cons moved list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((duo (duo-member moved list test-equal)))
    (duo-teleport-cons-next cons duo list previous)))

;;; Elem Cons
;;; ------------------------------

(defun duo-teleport-cons-before (elem moved list &optional
                                   previous-removed previous-inserted
                                   test-equal)
  "Move MOVED before ELEM in LIST. Return (MOVED . LIST).
ELEM must be present in list.
MOVED is the cons of the moved element.
If non nil, PREVIOUS-REMOVED and PREVIOUS-INSERTED
are used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-teleport-before elem moved list))
\(setq moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((duo (duo-member elem list test-equal)))
    (duo-teleport-cons-previous duo moved list
                                previous-removed previous-inserted)))

(defun duo-teleport-cons-after (elem moved list &optional previous test-equal)
  "Move MOVED after ELEM in LIST. Return (MOVED . LIST).
ELEM must be present in list.
MOVED is the cons of the moved element.
If non nil, PREVIOUS removed is used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-teleport-after elem moved list))
\(setq moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((duo (duo-member elem list test-equal)))
    (duo-teleport-cons-next duo moved list previous)))

;;; Elem Elem
;;; ------------------------------

(defun duo-teleport-before (elem moved list &optional
                                 previous-removed previous-inserted
                                 test-equal)
  "Move MOVED before ELEM in LIST. Return (cons of MOVED . LIST).
ELEM must be present in list.
MOVED is the value of the moved element.
If non nil, PREVIOUS-REMOVED and PREVIOUS-INSERTED
are used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-teleport-before elem moved list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((elem-cons (duo-member elem list test-equal))
        (moved-cons (duo-member moved list test-equal)))
    (duo-teleport-cons-previous elem-cons moved-cons list
                                previous-removed previous-inserted)))

(defun duo-teleport-after (elem moved list &optional previous test-equal)
  "Move MOVED after ELEM in LIST. Return (cons of MOVED . LIST).
ELEM must be present in list.
MOVED is the value of the moved element.
If non nil, PREVIOUS removed is used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-teleport-after elem moved list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((elem-cons (duo-member elem list test-equal))
        (moved-cons (duo-member moved list test-equal)))
    (duo-teleport-cons-next elem-cons moved-cons list previous)))

;;; Reference
;;; ------------------------------

;;; Cons Cons
;;; ---------------

(defun duo-ref-teleport-cons-previous (cons moved reflist &optional
                                            previous-removed previous-inserted)
  "Move MOVED before CONS in car of REFLIST. Return MOVED.
CONS must be a cons in car of REFLIST.
MOVED is the cons of the moved element.
REFLIST must be a cons (list . whatever-you-want)
If non nil, PREVIOUS-REMOVED and PREVIOUS-INSERTED
are used to speed up the process.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-cons-previous cons moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (unless (eq cons moved)
    (duo-ref-remove moved reflist previous-removed)
    (duo-ref-insert-cons-previous cons moved reflist previous-inserted))
  moved)

(defun duo-ref-teleport-cons-next (cons moved reflist &optional previous)
  "Move MOVED after CONS in car of REFLIST. Return MOVED.
CONS must be a cons in car of REFLIST.
MOVED is the cons of the moved element.
REFLIST must be a cons (list . whatever-you-want)
If non nil, PREVIOUS removed is used to speed up the process.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-cons-next cons moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (unless (eq cons moved)
    (duo-ref-remove moved reflist previous)
    (duo-insert-cons-next cons moved))
  moved)

;;; Cons Elem
;;; ---------------

(defun duo-ref-teleport-previous (cons moved reflist &optional
                                       previous-removed previous-inserted
                                       test-equal)
  "Move MOVED before CONS in car of REFLIST. Return MOVED.
CONS must be a cons in car of REFLIST.
MOVED is the value of the moved element.
REFLIST must be a cons (list . whatever-you-want)
If non nil, PREVIOUS-REMOVED and PREVIOUS-INSERTED
are used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-previous cons moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (duo (duo-member moved list test-equal)))
    (duo-ref-teleport-cons-previous cons duo reflist
                                    previous-removed previous-inserted)))

(defun duo-ref-teleport-next (cons moved reflist &optional previous test-equal)
  "Move MOVED after CONS in car of REFLIST. Return MOVED.
CONS must be a cons in car of REFLIST.
MOVED is the value of the moved element.
REFLIST must be a cons (list . whatever-you-want)
If non nil, PREVIOUS removed is used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-next cons moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (duo (duo-member moved list test-equal)))
    (duo-ref-teleport-cons-next cons duo reflist previous)))

;;; Elem Cons
;;; ---------------

(defun duo-ref-teleport-cons-before (elem moved reflist &optional
                                          previous-removed previous-inserted
                                          test-equal)
  "Move MOVED before ELEM in car of REFLIST. Return MOVED.
ELEM must be present in list.
MOVED is the cons of the moved element.
REFLIST must be a cons (list . whatever-you-want)
If non nil, PREVIOUS-REMOVED and PREVIOUS-INSERTED
are used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-cons-before cons moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (duo (duo-member elem list test-equal)))
    (duo-ref-teleport-cons-previous duo moved reflist
                                    previous-removed previous-inserted)))

(defun duo-ref-teleport-cons-after (elem moved reflist &optional previous test-equal)
  "Move MOVED after ELEM in car of REFLIST. Return MOVED.
ELEM must be present in list.
MOVED is the cons of the moved element.
REFLIST must be a cons (list . whatever-you-want)
If non nil, PREVIOUS removed is used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-cons-after cons moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (duo (duo-member elem list test-equal)))
    (duo-ref-teleport-cons-next duo moved reflist previous)))

;;; Elem Elem
;;; ---------------

(defun duo-ref-teleport-before (elem moved reflist &optional
                                     previous-removed previous-inserted
                                     test-equal)
  "Move MOVED before ELEM in car of REFLIST. Return MOVED.
ELEM must be present in list.
MOVED is the value of the moved element.
REFLIST must be a cons (list . whatever-you-want)
If non nil, PREVIOUS-REMOVED and PREVIOUS-INSERTED
are used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-before cons moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (elem-cons (duo-member elem list test-equal))
         (moved-cons (duo-member moved list test-equal)))
    (duo-ref-teleport-cons-previous elem-cons moved-cons reflist
                                    previous-removed previous-inserted)))

(defun duo-ref-teleport-after (elem moved reflist &optional previous test-equal)
  "Move MOVED after ELEM in LIST. Return (cons of MOVED . LIST).
ELEM must be present in list.
MOVED is the value of the moved element.
REFLIST must be a cons (list . whatever-you-want)
If non nil, PREVIOUS removed is used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-teleport-after elem moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (elem-cons (duo-member elem list test-equal))
         (moved-cons (duo-member moved list test-equal)))
    (duo-ref-teleport-cons-next elem-cons moved-cons reflist previous)))

;;; Move
;;; ------------------------------------------------------------

;;; Step
;;; ------------------------------

(defun duo-move-previous (moved list &optional num)
  "Move MOVED to NUM previous place in LIST. Return (MOVED . LIST).
If range is exceeded, move MOVED at the beginning of the list.
MOVED must be a cons in LIST.
NUM defaults to 1.
The actual new list must be recovered using the returned list.
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
The actual new list must be recovered using the returned list.
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

(defun duo-move-before (elem list &optional num test-equal)
  "Move ELEM to NUM previous place in LIST. Return (MOVED . LIST).
If range is exceeded, move ELEM at the beginning of the list.
MOVED is the moved value.
NUM defaults to 1.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-move-before elem list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let* ((num (if num
                  num
                1))
         (pre-ins (duo-before elem list (1+ num) test-equal))
         (landmark (if pre-ins
                       (cdr pre-ins)
                     list))
         (pre-rem (if pre-ins
                      (nthcdr num pre-ins)
                    (duo-before elem list 1 test-equal)))
         (moved (if pre-rem
                    (cdr pre-rem)
                  (duo-member elem list test-equal))))
    (duo-teleport-cons-previous landmark moved list pre-rem pre-ins)))

(defun duo-move-after (elem list &optional num test-equal)
  "Move ELEM to NUM next place in LIST. Return (MOVED . LIST).
If range is exceeded, move MOVED at the end of the list.
MOVED is the moved value.
NUM defaults to 1.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-move-after elem list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let* ((num (if num
                  num
                1))
         (moved (duo-member elem list test-equal))
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
The actual new list must be recovered using the returned list.
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
The actual new list must be recovered using the returned list.
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

(defun duo-circ-move-before (elem list &optional num test-equal)
  "Move ELEM to NUM previous place in LIST. Return (MOVED . LIST).
Circular : if in beginning of list, go to the end.
MOVED is the moved value.
NUM defaults to 1.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-move-before elem list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let* ((num (if num
                  num
                1))
         (pre-ins (duo-circ-before elem list (1+ num) test-equal))
         (landmark (duo-circ-next pre-ins list))
         (pre-rem (duo-circ-next pre-ins list num))
         (moved (cdr pre-rem)))
    (unless moved
      (setq moved (duo-member elem list test-equal))
      (setq pre-rem nil))
    (when (eq landmark list)
      (setq pre-ins nil))
    (when (eq moved pre-ins)
      (setq pre-ins nil))
    (duo-teleport-cons-previous landmark moved list pre-rem pre-ins)))

(defun duo-circ-move-after (elem list &optional num test-equal)
  "Move ELEM to NUM next place in LIST. Return (MOVED . LIST).
Circular : if in end of list, go to the beginning.
MOVED is the moved value.
NUM defaults to 1.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-move-after elem list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Destructive."
  (let* ((num (if num
                  num
                1))
         (moved (duo-member elem list test-equal))
         (landmark (duo-circ-next moved list num)))
    (duo-teleport-cons-next landmark moved list)))

;;; Reference
;;; ------------------------------

;;; Linear
;;; ---------------

(defun duo-ref-move-previous (moved reflist &optional num)
  "Move MOVED to NUM previous place in car of REFLIST. Return MOVED.
If range is exceeded, move MOVED at the beginning of the list.
MOVED must be a cons in car of REFLIST.
REFLIST must be a cons (list . whatever-you-want)
NUM defaults to 1.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-move-previous moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (num (if num
                  num
                1))
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
  "Move MOVED to NUM next place in car of REFLIST. Return MOVED.
If range is exceeded, move MOVED at the end of the list.
MOVED must be a cons in car of REFLIST.
REFLIST must be a cons (list . whatever-you-want)
NUM defaults to 1.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-move-next moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (num (if num
                  num
                1))
         (landmark (nthcdr num moved)))
    (unless landmark
      (setq landmark (duo-last list)))
    (duo-ref-teleport-cons-next landmark moved reflist)
    moved))

(defun duo-ref-move-before (elem reflist &optional num test-equal)
  "Move ELEM to NUM previous place in car of REFLIST. Return MOVED.
If range is exceeded, move ELEM at the beginning of the list.
MOVED is the moved value.
REFLIST must be a cons (list . whatever-you-want)
NUM defaults to 1.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-move-before moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (num (if num
                  num
                1))
         (pre-ins (duo-before elem list (1+ num) test-equal))
         (landmark (if pre-ins
                       (cdr pre-ins)
                     list))
         (pre-rem (if pre-ins
                      (nthcdr num pre-ins)
                    (duo-before elem list 1 test-equal)))
         (moved (if pre-rem
                    (cdr pre-rem)
                  (duo-member elem list test-equal))))
    (duo-ref-teleport-cons-previous landmark moved reflist pre-rem pre-ins)
    moved))

(defun duo-ref-move-after (elem reflist &optional num test-equal)
  "Move ELEM to NUM next place in car of REFLIST. Return MOVED.
If range is exceeded, move MOVED at the end of the list.
MOVED is the moved value.
REFLIST must be a cons (list . whatever-you-want)
NUM defaults to 1.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-move-after moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (num (if num
                  num
                1))
         (moved (duo-member elem list test-equal))
         (landmark (nthcdr num moved)))
    (unless landmark
      (setq landmark (duo-last list)))
    (duo-ref-teleport-cons-next landmark moved reflist)
    moved))

;;; Circular
;;; ---------------

(defun duo-ref-circ-move-previous (moved reflist &optional num)
  "Move MOVED to NUM previous place in car of REFLIST. Return MOVED.
Circular : if in beginning of list, go to the end.
MOVED must be a cons in car of REFLIST.
REFLIST must be a cons (list . whatever-you-want)
NUM defaults to 1.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-circ-move-previous moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (num (if num
                  num
                1))
         (pre-ins (duo-circ-previous moved list (1+ num)))
         (landmark (duo-circ-next pre-ins list))
         (pre-rem (duo-circ-next pre-ins list num)))
    (when (eq landmark list)
      (setq pre-ins nil))
    (when (eq moved pre-ins)
      (setq pre-ins nil))
    (duo-ref-teleport-cons-previous landmark moved reflist pre-rem pre-ins)))

(defun duo-ref-circ-move-next (moved reflist &optional num)
  "Move MOVED to NUM next place in car of REFLIST. Return MOVED.
Circular : if in end of list, go to the beginning.
MOVED must be a cons in car of REFLIST.
REFLIST must be a cons (list . whatever-you-want)
NUM defaults to 1.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-circ-move-next moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (num (if num
                  num
                1))
         (landmark (duo-circ-next moved list num)))
    (duo-ref-teleport-cons-next landmark moved reflist)))

(defun duo-ref-circ-move-before (elem reflist &optional num test-equal)
  "Move ELEM to NUM previous place in LIST. Return MOVED.
Circular : if in beginning of list, go to the end.
MOVED is the moved value.
REFLIST must be a cons (list . whatever-you-want)
NUM defaults to 1.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-circ-move-before moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (num (if num
                  num
                1))
         (pre-ins (duo-circ-before elem list (1+ num) test-equal))
         (landmark (duo-circ-next pre-ins list))
         (pre-rem (duo-circ-next pre-ins list num))
         (moved (cdr pre-rem)))
    (unless moved
      (setq moved (duo-member elem list test-equal))
      (setq pre-rem nil))
    (when (eq landmark list)
      (setq pre-ins nil))
    (when (eq moved pre-ins)
      (setq pre-ins nil))
    (duo-ref-teleport-cons-previous landmark moved reflist pre-rem pre-ins)))

(defun duo-ref-circ-move-after (elem reflist &optional num test-equal)
  "Move ELEM to NUM next place in LIST. Return MOVED.
Circular : if in end of list, go to the beginning.
MOVED is the moved value.
REFLIST must be a cons (list . whatever-you-want)
NUM defaults to 1.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-circ-move-after moved reflist)
;; Update list
\(setq mylist (car reflist))
Destructive."
  (let* ((list (car reflist))
         (num (if num
                  num
                1))
         (moved (duo-member elem list test-equal))
         (landmark (duo-circ-next moved list num)))
    (duo-ref-teleport-cons-next landmark moved reflist)))

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

(defun duo-exchange (one two list &optional pre-one pre-two test-equal)
  "Exchange elements ONE and TWO in LIST. Return ((ONE . TWO) . LIST).
ONE and TWO must be present in LIST.
If non nil, PRE-ONE and PRE-TWO are used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq structure (duo-exchange-cons one two list))
\(setq one (car (car structure)))
\(setq two (cdr (car structure)))
\(setq list (cdr structure))
Destructive."
  (let ((cons-one (duo-member one list test-equal))
        (cons-two (duo-member two list test-equal)))
    (duo-exchange-cons cons-one cons-two list pre-one pre-two)))

;;; Reference
;;; ------------------------------

(defun duo-ref-exchange-cons (one two reflist &optional pre-one pre-two)
  "Exchange cons ONE and TWO in car of REFLIST. Return (ONE . TWO).
ONE and TWO must be cons in car of REFLIST.
If non nil, PRE-ONE and PRE-TWO are used to speed up the process.
See the docstring of `duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-exchange-cons one two reflist)
;; Update list
\(setq mylist (car reflist))
Modifies car of REFLIST."
  (unless (eq one two)
    (if (eq two (car reflist))
        (let ((return)
              (swap))
          (setq return (duo-ref-exchange-cons two one reflist))
          (setq swap (car return))
          (setcar return (cdr return))
          (setcdr return swap)
          return)
      (let* ((list (car reflist))
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

(defun duo-ref-exchange (one two reflist &optional pre-one pre-two test-equal)
  "Exchange elements ONE and TWO in car of REFLIST. Return (ONE . TWO).
ONE and TWO must be present in car of REFLIST.
If non nil, PRE-ONE and PRE-TWO are used to speed up the process.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-exchange one two reflist)
;; Update list
\(setq mylist (car reflist))
Modifies car of REFLIST."
  (let* ((list (car reflist))
         (cons-one (duo-member one list test-equal))
         (cons-two (duo-member two list test-equal)))
    (duo-ref-exchange-cons cons-one cons-two reflist pre-one pre-two)))

;;; Group
;;; ------------------------------------------------------------

(defun duo-insert-at-group-beg (new list &optional test-group)
  "Insert NEW in LIST, at the beginning of a group determined by TEST-GROUP.
If the group is not found, insert at the beginning of LIST.
Return (cons of NEW . LIST).
NEW is the value of the element inserted.
TEST-GROUP takes two arguments and returns t if they belongs to the same group.
TEST-GROUP defaults do `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-insert-at-group-beg new list))
\(setq cons-new (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((newlist list)
        (previous (duo-before new list 1 test-group))
        (duo))
    (if previous
        (progn
          (duo-insert-next previous new)
          (setq duo (cdr previous)))
      (setq newlist (duo-push new list))
      (setq duo newlist))
    (cons duo newlist)))

(defun duo-insert-at-group-end (new list &optional test-group)
  "Insert NEW in LIST, at the end of a group determined by TEST-GROUP.
If the group is not found, insert at the end of LIST.
Return (cons of NEW. LIST).
NEW is the value of the element inserted.
TEST-GROUP takes two arguments and returns t if they belongs to the same group.
TEST-GROUP defaults do `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-insert-at-group-end new list))
\(setq cons-new (car pair))
\(setq list (cdr pair))
Destructive."
  (let ((newlist list)
        (previous (duo-member new list test-group))
        (duo))
    (while (and previous
                (funcall test-group (car (cdr previous)) new))
      (setq previous (cdr previous)))
    (if previous
        (progn
          (duo-insert-next previous new)
          (setq duo (cdr previous)))
      (setq duo (duo-add new newlist)))
    (cons duo newlist)))

;;; Reference
;;; ------------------------------

;;; Filter
;;; ------------------------------------------------------------

(defun duo-filter (test-filter list)
  "Return list of references to elements of LIST matching TEST-FILTER.
LIST is not modified.
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

;;; Next / Previous
;;; ------------------------------

(defun duo-filter-previous (test-filter cons list)
  "Return reference of previous element of CONS in LIST matching TEST-FILTER."
  (let ((duo list)
        (previous))
    (while (and duo
                (not (eq duo cons)))
      (when (funcall test-filter (car duo))
        (setq previous duo))
      (setq duo (cdr duo)))
    previous))

(defun duo-filter-next (test-filter cons)
  "Return reference of next element of CONS in list matching TEST-FILTER."
  (let ((next (cdr cons)))
    (while (and next
                (not (funcall test-filter (car next))))
      (setq next (cdr next)))
    next))

(defun duo-filter-before (test-filter elem list &optional test-equal)
  "Return reference of element before ELEM in LIST matching TEST-FILTER.
TEST-EQUAL tests equality of two elements, defaults to `equal'."
  (let ((duo (duo-member elem list test-equal)))
    (duo-filter-previous test-filter duo list)))

(defun duo-filter-after (test-filter elem list &optional test-equal)
  "Return reference of element after ELEM in LIST matching TEST-FILTER.
TEST-EQUAL tests equality of two elements, defaults to `equal'."
  (let ((duo (duo-member elem list test-equal)))
    (duo-filter-next test-filter duo)))

;;; Circular
;;; ------------------------------

(defun duo-circ-filter-previous (test-filter cons list)
  "Return reference of previous element of CONS in LIST matching TEST-FILTER."
  (if (eq cons list)
      (duo-filter-previous test-filter
                              (duo-last list)
                              list)
    (let ((duo list)
          (previous))
      (while (and duo
                  (not (eq duo cons)))
        (when (funcall test-filter (car duo))
          (setq previous duo))
        (setq duo (cdr duo)))
      (unless previous
        (setq duo (cdr duo))
        (while duo
          (when (funcall test-filter (car duo))
            (setq previous duo))
          (setq duo (cdr duo))))
      previous)))

(defun duo-circ-filter-next (test-filter cons list)
  "Return reference of next element of CONS in LIST matching TEST-FILTER."
  (let ((next (cdr cons)))
    (while (and next
                (not (funcall test-filter (car next))))
      (setq next (cdr next)))
    (if next
        next
      (duo-filter-next test-filter list))))

(defun duo-circ-filter-before (test-filter elem list &optional test-equal)
  "Return reference of element before ELEM in LIST matching TEST-FILTER.
TEST-EQUAL tests equality of two elements, defaults to `equal'."
  (let ((duo (duo-member elem list test-equal)))
    (duo-circ-filter-previous test-filter duo list)))

(defun duo-circ-filter-after (test-filter elem list &optional test-equal)
  "Return reference of element after ELEM in LIST matching TEST-FILTER.
TEST-EQUAL tests equality of two elements, defaults to `equal'."
  (let ((duo (duo-member elem list test-equal)))
    (duo-circ-filter-next test-filter duo list)))

;;; Partition
;;; ------------------------------------------------------------

(defun duo-partition (funkey list)
  "Partition LIST using FUNKEY.
The resut is an alist whose keys are given by the values of FUNKEY
applied to the elements of LIST.
Each element of the alist is of the form :
\(key elem-1 elem-2 ... elem-N)
where all the elem-* verify (FUNKEY elem-?) = key."
  (let ((duo list)
        (assoc-list)
        (key)
        (key-list))
    (while duo
      (setq key (funcall funkey (car duo)))
      (setq key-list (duo-assoc key assoc-list))
      (if key-list
          (duo-add (car duo) (car key-list))
        (if assoc-list
            (duo-add (list key (car duo)) assoc-list)
          (setq assoc-list (list (list key (car duo))))))
      (setq duo (cdr duo)))
    assoc-list))

;;; End
;;; ------------------------------------------------------------

(provide 'duo)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; duo.el ends here
