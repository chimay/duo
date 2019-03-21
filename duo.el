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
;;   - filter-{previous,next,before,after}
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

(defun torus--duo-set-deref (ptr object)
  "Change the content of the variable referenced by PTR to OBJECT.
OBJECT must be a cons or a list."
  (setcar ptr (car object))
  (setcdr ptr (cdr object))
  ptr)

;;; Lists
;;; ------------------------------------------------------------

;;; Find
;;; ------------------------------

(defun torus--duo-member (elem list &optional test-equal)
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

(defun torus--duo-last (list &optional num)
  "Return cons starting a sublist of NUM elements at the end of LIST.
NUM defaults to 1 : NUM nil means return cons of last element in LIST."
  (let ((num (if num
                 num
               1))
        (last list))
    (while (nthcdr num last)
      (setq last (cdr last)))
    last))

(defun torus--duo-at-index (index list)
  "Element at INDEX in LIST."
  (nthcdr index list))

(defun torus--duo-index (elem list &optional test-equal)
  "Index of ELEM in LIST.
ELEM must be present in list.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (- (length list) (length (torus--duo-member elem list test-equal))))

;;; Next / Previous
;;; ------------------------------

(defun torus--duo-previous (cons list &optional num)
  "Return cons of NUM elements before CONS in LIST.
NUM defaults to 1.
CONS must reference a cons in list."
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

(defun torus--duo-next (cons &optional num)
  "Return cons of NUM elements after CONS in list.
NUM defaults to 1.
CONS must reference a cons in the list."
  (let ((num (if num
                 num
               1)))
    (nthcdr num cons)))

(defun torus--duo-before (elem list &optional num test-equal)
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

(defun torus--duo-after (elem list &optional num test-equal)
  "Return cons of NUM elements after ELEM in LIST.
NUM defaults to 1.
ELEM must be present in list.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (let ((num (if num
                 num
               1)))
    (nthcdr num (torus--duo-member elem list test-equal))))

;;; Circular
;;; ---------------

(defun torus--duo-circ-previous (cons list &optional num)
  "Return cons of NUM elements before CONS in LIST.
Circular : if in beginning of list, go to the end.
NUM defaults to 1.
CONS must reference a cons in list.
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

(defun torus--duo-circ-next (cons list &optional num)
  "Return cons of NUM elements after CONS in LIST.
Circular : if in end of list, go to the beginning.
NUM defaults to 1.
CONS must reference a cons in LIST."
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

(defun torus--duo-circ-before (elem list &optional num test-equal)
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

(defun torus--duo-circ-after (elem list &optional num test-equal)
  "Return cons of NUM elements after ELEM in LIST.
Circular : if in end of list, go to the beginning.
NUM defaults to 1.
ELEM must be present in list.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (torus--duo-circ-next (torus--duo-member elem list test-equal) list num))

;;; Add / Remove at Beg / End
;;; ------------------------------

(defun torus--duo-naive-push (list)
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

(defun torus--duo-push-cons (cons list)
  "Add CONS at the beginning of LIST. Return LIST.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-push' to know why.
Common usage :
\(setq list (torus--duo-push-cons cons list))
Modifies LIST."
  (let* ((newlist))
    (setcdr cons list)
    (setq newlist cons)
    newlist))

(defun torus--duo-add-cons (cons list &optional last)
  "Store CONS at the end of LIST. Return CONS.
If non nil, LAST is used to speed up the process.
Modifies LIST."
  (let ((last (if last
                  last
                (torus--duo-last list))))
    (setcdr last cons)
    (setcdr cons nil)
    cons))

(defun torus--duo-push (elem list)
  "Add ELEM at the beginning of LIST. Return LIST.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-push' to know why.
Common usage :
\(setq list (torus--duo-push elem list))
Modifies LIST."
  (let* ((duo (cons elem list))
         (newlist duo))
    newlist))

(defun torus--duo-add (elem list)
  "Add ELEM at the end of LIST. Return the new end cons.
Modifies LIST."
  (let ((last (torus--duo-last list))
        (duo (cons elem nil)))
    (setcdr last duo)
    duo))

(defun torus--duo-push-new (elem list)
  "Add ELEM at the beginning of LIST if not already there. Return LIST.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-push' to know why.
Common usage :
\(setq list (torus--duo-push-new elem list))
Modifies LIST."
  (if (member elem list)
      list
    (torus--duo-push elem list)))

(defun torus--duo-add-new (elem list)
  "Add ELEM at the end of LIST if not already there. Return the new end cons.
Modifies LIST."
  (unless (member elem list)
    (torus--duo-add elem list)))

(defun torus--duo-naive-pop (list)
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

(defun torus--duo-pop (list)
  "Remove the first element of LIST. Return (popped-cons . new-list)
The actual new list must be recovered using the returned structure.
See the docstring of `torus--duo-naive-pop' to know why.
Common usage :
\(setq pair (torus--duo-pop list))
\(setq popped (car pair))
\(setq list (cdr pair))
That’s all folks."
  (let ((popped list)
        (newlist (cdr list)))
    (setcdr popped nil)
    (cons popped newlist)))

(defun torus--duo-drop (list)
  "Remove last element of LIST. Return cons of removed element.
Modifies LIST."
  (let* ((before-last (torus--duo-last list 2))
         (last (cdr before-last)))
    (if last
        (setcdr before-last nil)
      ;; One element list
      (setq last (cons (car list) nil))
      (setcar list nil))
    last))

(defun torus--duo-truncate (list &optional num)
  "Truncate LIST to its first NUM elements.
Modifies LIST."
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

(defun torus--duo-push-and-truncate (elem list &optional num)
  "Add ELEM at the beginning of LIST. Truncate LIST to NUM elements.
Return LIST.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-push' to know why.
Common usage :
\(setq list (torus--duo-push-and-truncate elem list))
Modifies LIST."
  (let ((newlist list))
    (setq newlist (torus--duo-push elem list))
    (torus--duo-truncate newlist num)
    newlist))

;;; Reference
;;; ---------------

(defun torus--duo-ref-push-cons (cons reflist)
  "Add CONS at the beginning of the car of REFLIST. Return REFLIST.
REFLIST must be a cons (list . whatever-you-want)
See the docstring of `torus--duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))          ; this
\(setq reflist (cons mylist nil))      ; or that
\(setq reflist (cons mylist whatever)) ; or that
;; Push
\(torus--duo-ref-push-cons reflist)
;; Update list
\(setq mylist (car reflist))
Modifies LIST."
  (setcdr cons (car reflist))
  (setcar reflist cons)
  reflist)

(defun torus--duo-ref-push (elem reflist)
  "Add ELEM at the beginning of the car of REFLIST. Return REFLIST.
REFLIST must be a cons (list . whatever-you-want)
See the docstring of `torus--duo-naive-push' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))          ; this
\(setq reflist (cons mylist nil))      ; or that
\(setq reflist (cons mylist whatever)) ; or that
;; Push
\(torus--duo-ref-push-cons reflist)
;; Update list
\(setq mylist (car reflist))
Modifies LIST."
  (let ((duo (cons elem (car reflist))))
    (setcar reflist duo)
    reflist))

(defun torus--duo-ref-pop (reflist)
  "Remove first element in the car of REFLIST. Return popped cons.
REFLIST must be a cons (list . whatever-you-want)
See the docstring of `torus--duo-naive-pop' to know why it doesn’t
use the list itself as argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))          ; this
\(setq reflist (cons mylist nil))      ; or that
\(setq reflist (cons mylist whatever)) ; or that
;; Pop
\(setq popped (torus--duo-ref-pop reflist))
;; Update list
\(setq mylist (car reflist))
That’s all folks."
  (let* ((list (car reflist))
         (popped list))
    (setcar reflist (cdr list))
    (setcdr popped nil)
    popped))

;;; Insert
;;; ------------------------------

(defun torus--duo-insert-cons-previous (cons new list)
  "Insert NEW before CONS in LIST. Return NEW.
CONS must reference a cons in LIST.
NEW is the cons inserted.
If the new cons is inserted at the beginning of the list,
the actual new list must be recovered using new LIST = NEW.
See the docstring of `torus--duo-naive-push' to know why.
Common usage :
\(setq return (torus--duo-insert-cons-previous cons new list))
\(when (eq (cdr return) list)
  (setq list return))
Modifies LIST."
  (if (eq cons list)
      (torus--duo-push-cons new list)
    (let* ((previous (torus--duo-previous cons list)))
      (if previous
          (progn
            (setcdr new (cdr previous))
            (setcdr previous new)
            new)
        nil))))

(defun torus--duo-insert-cons-next (cons new)
  "Insert NEW after CONS in list. Return NEW.
CONS must reference a cons in LIST.
NEW is the cons inserted.
Modifies LIST."
    (setcdr new (cdr cons))
    (setcdr cons new)
    new)

(defun torus--duo-ref-insert-cons-previous (cons new reflist)
  "Insert NEW before CONS in car of REFLIST. Return NEW.
CONS must reference a cons in LIST.
NEW is the cons inserted.
REFLIST must be a cons (list . whatever-you-want)
See the docstring of `torus--duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Insert
\(torus--duo-ref-insert-cons-previous cons new reflist)
;; Update list
\(setq mylist (car reflist))
Modifies LIST."
  (let ((list (car reflist)))
    (if (eq cons list)
        (torus--duo-ref-push-cons new reflist)
      (let ((previous (torus--duo-previous cons list)))
        (if previous
            (progn
              (setcdr new (cdr previous))
              (setcdr previous new)
              new)
          nil)))))

(defun torus--duo-insert-previous (cons new list)
  "Insert NEW before CONS in LIST. Return cons of NEW.
CONS must reference a cons in LIST.
NEW is the value of the element inserted.
If the new cons is inserted at the beginning of the list,
the actual new list must be recovered using new LIST = NEW.
See the docstring of `torus--duo-naive-push' to know why.
Common usage :
\(setq return (torus--duo-insert-previous cons new list))
\(when (eq (cdr return) list)
  (setq list return))
Modifies LIST."
  (if (eq cons list)
      (torus--duo-push new list)
    (let* ((previous (torus--duo-previous cons list))
           (duo))
      (if previous
          (progn
            (setq duo (cons new (cdr previous)))
            (setcdr previous duo)
            duo)
        nil))))

(defun torus--duo-insert-next (cons new)
  "Insert NEW after CONS in list. Return cons of NEW.
CONS must reference a cons in LIST.
NEW is the value of the element inserted.
Modifies LIST."
  (let ((duo (cons new (cdr cons))))
    (setcdr cons duo)
    duo))

(defun torus--duo-ref-insert-previous (cons new reflist)
  "Insert NEW before CONS in car of REFLIST. Return cons of NEW.
CONS must reference a cons in LIST.
NEW is the value of the element inserted.
REFLIST must be a cons (list . whatever-you-want)
See the docstring of `torus--duo-naive-push' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Insert
\(torus--duo-ref-insert-previous cons new reflist)
;; Update list
\(setq mylist (car reflist))
Modifies LIST."
  ;; TODO
  (if (eq cons list)
      (torus--duo-push new list)
    (let* ((previous (torus--duo-previous cons list))
           (duo))
      (if previous
          (progn
            (setq duo (cons new (cdr previous)))
            (setcdr previous duo)
            duo)
        nil))))

(defun torus--duo-insert-before (elem new list &optional test-equal)
  "Insert NEW before ELEM in LIST. Return cons of NEW.
ELEM must be present in list.
NEW is the value of the element inserted.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
If the new cons is inserted at the beginning of the list,
the actual new list must be recovered using new LIST = NEW.
See the docstring of `torus--duo-naive-push' to know why.
Common usage :
\(setq return (torus--duo-insert-before cons new list))
\(when (eq (cdr return) list)
  (setq list return))
Modifies LIST."
  (let ((test-equal (if test-equal
                        test-equal
                      #'equal)))
    (if (funcall test-equal (car list) elem)
        (torus--duo-push new list)
      (let* ((previous (torus--duo-before elem list 1 test-equal))
             (duo))
        (if previous
            (progn
              (setq duo (cons new (cdr previous)))
              (setcdr previous duo)
              duo)
          nil)))))

(defun torus--duo-insert-after (elem new list &optional test-equal)
  "Insert NEW after ELEM in LIST. Return cons of NEW.
ELEM must be present in list.
NEW is the value of the element inserted.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
Modifies LIST."
  (let* ((member (torus--duo-member elem list test-equal))
         (duo))
    (if member
        (progn
          (setq duo (cons new (cdr member)))
          (setcdr member duo)
          duo)
      nil)))

;;; Remove
;;; ---------------

(defun torus--duo-remove (cons list)
  "Remove CONS from LIST. Return LIST.
CONS must reference a cons in LIST.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-pop' to know why.
Common usage :
\(setq list (torus--duo-remove cons list))
Modifies LIST."
  (if (eq cons list)
      (cdr (torus--duo-pop list))
    (let* ((previous (torus--duo-previous cons list))
           (duo cons))
      (when previous
        (setcdr previous (cdr duo))
        (setcdr duo nil))
      list)))

(defun torus--duo-delete (elem list &optional test-equal)
  "Delete ELEM from LIST. Return (removed-cons . LIST).
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `torus--duo-naive-pop' to know why.
Common usage :
\(setq pair (torus--duo-delete elem list))
\(setq removed (car pair))
\(setq list (cdr pair))
Modifies LIST."
  (let ((test-equal (if test-equal
                        test-equal
                      #'equal)))
    (if (funcall test-equal (car list) elem)
        (torus--duo-pop list)
      (let* ((previous (torus--duo-before elem list 1 test-equal))
             (duo (cdr previous)))
        (when previous
          (setcdr previous (cdr duo))
          (setcdr duo nil))
        (cons duo list)))))

(defun torus--duo-delete-all (elem list &optional test-equal)
  "Delete all elements equals to ELEM from LIST.
Return (list-of-removed-elements . LIST).
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned structure.
See the docstring of `torus--duo-naive-pop' to know why.
Common usage :
\(setq pair (torus--duo-delete-all elem list))
\(setq removed-list (car pair))
\(setq list (cdr pair))
Modifies LIST."
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
      (setq pair (torus--duo-pop newlist))
      (setq removed (car pair))
      (setq newlist (cdr pair))
      (if removed-list
          (setq last (torus--duo-add-cons removed removed-list last))
        (setq removed-list removed)
        (setq last removed)))
    (setq duo newlist)
    (while duo
      (setq next (cdr duo))
      (when (funcall test-equal (car duo) elem)
        (setq newlist (torus--duo-remove duo newlist))
        (setq removed duo)
        (if removed-list
            (setq last (torus--duo-add-cons removed removed-list last))
          (setq removed-list removed)
          (setq last removed)))
      (setq duo next))
    (cons removed-list newlist)))

;;; Change
;;; ------------------------------

(defun torus--duo-update (old new list)
  "Replace OLD by NEW in LIST. Return cons of NEW.
Modifies LIST."
  (let ((duo (torus--duo-member old list)))
    (when duo
      (setcar duo new))
    duo))

;;; Move
;;; ------------------------------

;;; Step
;;; ---------------

(defun torus--duo-move-previous (cons list)
  "Move CONS to previous place in LIST. Return LIST.
CONS must reference a cons in LIST.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-push' to know why.
Common usage :
\(setq list (torus--duo-move-previous cons list))
Modifies LIST."
  (if (eq cons (cdr list))
      (let ((newlist (cdr list)))
        (setcdr list (cdr newlist))
        (setcdr newlist list)
        newlist)
    (let* ((before (torus--duo-previous cons list 2))
           (after (cdr before)))
      (when before
        (setcdr after (cdr cons))
        (setcdr cons after)
        (setcdr before cons))
      list)))

(defun torus--duo-move-next (cons list)
  "Move CONS to next place in LIST.")

(defun torus--duo-move-before (elem list)
  "Move ELEM to next place in LIST.")

(defun torus--duo-move-after (elem list)
  "Move ELEM to next place in LIST.")

;;; Circular
;;; ---------------

(defun torus--duo-move-circ-previous (cons list)
  "Move CONS to previous place in LIST.
Circular : if in beginning of list, go to the end."
  )

(defun torus--duo-move-circ-next (cons list)
  "Move CONS to next place in LIST.
Circular : if in end of list, go to the beginning."
  )

(defun torus--duo-move-circ-before (elem list)
  "Move ELEM to next place in LIST.
Circular : if in beginning of list, go to the end."
  )

(defun torus--duo-move-circ-after (elem list)
  "Move ELEM to next place in LIST.
Circular : if in end of list, go to the beginning."
  )

;;; Jump
;;; ---------------

(defun torus--duo-teleport-cons-previous (cons moved list)
  "Move MOVED before CONS in LIST. Return LIST.
CONS must reference a cons in LIST.
MOVED is the cons of the moved element.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-push' to know why.
Common usage :
\(setq list (torus--duo-teleport-cons-previous cons moved list))
Modifies LIST."
  (let ((newlist list)
        (return))
    (unless (eq cons moved)
      (setq newlist (torus--duo-remove moved list))
      (setq return (torus--duo-insert-cons-previous cons moved newlist))
      (when (eq (cdr return) newlist)
        (setq newlist return)))
    newlist))

(defun torus--duo-teleport-cons-next (cons moved list)
  "Move MOVED after CONS in LIST. Return LIST.
CONS must reference a cons in LIST.
MOVED is the cons of the moved element.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-pop' to know why.
Common usage :
\(setq list (torus--duo-teleport-cons-next cons moved list))
Modifies LIST."
  (let ((newlist list)
        (return))
    (unless (eq cons moved)
      (setq newlist (torus--duo-remove moved list))
      (torus--duo-insert-cons-next cons moved))
    newlist))

(defun torus--duo-teleport-previous (cons moved list &optional test-equal)
  "Move MOVED before CONS in LIST. Return LIST.
CONS must reference a cons in LIST.
MOVED is the value of the moved element.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-push' to know why.
Common usage :
\(setq list (torus--duo-teleport-previous cons moved list))
Modifies LIST."
  (let ((duo (torus--duo-member moved list test-equal)))
    (torus--duo-teleport-cons-previous cons duo list)))

(defun torus--duo-teleport-next (cons moved list &optional test-equal)
  "Move MOVED after CONS in LIST. Return LIST.
CONS must reference a cons in LIST.
MOVED is the value of the moved element.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-pop' to know why.
Common usage :
\(setq list (torus--duo-teleport-next cons moved list))
Modifies LIST."
  (let ((duo (torus--duo-member moved list test-equal)))
    (torus--duo-teleport-cons-next cons duo list)))

(defun torus--duo-teleport-before (elem moved list &optional test-equal)
  "Move MOVED before ELEM in LIST. Return LIST.
ELEM must be present in list.
MOVED is the value of the moved element.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-push' to know why.
Common usage :
\(setq list (torus--duo-teleport-before cons moved list))
Modifies LIST."
  (let ((newlist list)
        (test-equal (if test-equal
                        test-equal
                      #'equal)))
    (unless (funcall test-equal moved elem)
      (let ((pair (torus--duo-delete moved newlist test-equal))
            (member)
            (duo)
            (return))
        (setq duo (car pair))
        (setq newlist (cdr pair))
        (setq member (torus--duo-member elem newlist test-equal))
        (setq return (torus--duo-insert-cons-previous member duo newlist))
        (when (eq (cdr return) newlist)
          (setq newlist return))))
    newlist))

(defun torus--duo-teleport-after (elem moved list &optional test-equal)
  "Move MOVED after ELEM in LIST. Return cons of MOVED.
ELEM must be present in list.
MOVED is the value of the moved element.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-pop' to know why.
Common usage :
\(setq list (torus--duo-teleport-after cons moved list))
Modifies LIST."
  (let ((newlist list)
        (test-equal (if test-equal
                        test-equal
                      #'equal)))
    (unless (funcall test-equal moved elem)
      (let ((pair (torus--duo-delete moved newlist test-equal))
            (member)
            (duo)
            (return))
        (setq duo (car pair))
        (setq newlist (cdr pair))
        (setq member (torus--duo-member elem newlist test-equal))
        (torus--duo-insert-cons-next member duo)))
    newlist))

;;; Rotate <- ->
;;; ------------------------------

(defun torus--duo-rotate-left (list)
  "Rotate LIST to the left.
Equivalent to pop first element and add it to the end.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-pop' to know why.
Common usage :
\(setq list (torus--duo-rotate-left list))
Modifies LIST."
  ;; Length list > 1
  (if (cdr list)
      (let* ((pair (torus--duo-pop list))
             (duo (car pair))
             (newlist (cdr pair)))
        (torus--duo-add-cons duo newlist)
        newlist)
    list))

(defun torus--duo-rotate-right (list)
  "Rotate LIST to the right.
Equivalent to drop last element and push it at the beginning.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-push' to know why.
Common usage :
\(setq list (torus--duo-rotate-right list))
Modifies LIST."
  ;; Length list > 1
  (if (cdr list)
      (let ((duo (torus--duo-drop list)))
        (torus--duo-push-cons duo list))
    list))

;;; Reverse
;;; ------------------------------

(defun torus--duo-reverse (list)
  "Reverse LIST. Return LIST.
Modifies LIST."
  (let* ((begin list)
         (end (torus--duo-last list))
         (value)
         (middle))
    (while (not middle)
      (if (or (eq begin end)
              (eq begin (cdr end)))
          (setq middle t)
        (setq value (car begin))
        (setcar begin (car end))
        (setcar end value)
        (setq begin (cdr begin))
        (setq end (torus--duo-previous end list)))))
  list)

;;; Group
;;; ------------------------------

(defun torus--duo-insert-at-group-beg (new list &optional test-group)
  "Insert NEW in LIST, at the beginning of a group determined by TEST-GROUP.
Return LIST.
NEW is the value of the element inserted.
TEST-GROUP takes two arguments and returns t if they belongs to the same group.
TEST-GROUP defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-pop' to know why.
Common usage :
\(setq list (torus--duo-insert-at-group-beg new list))
Modifies LIST."
  (let ((newlist list)
        (return))
    (setq return (torus--duo-insert-before new new list test-group))
    (when (eq (cdr return) newlist)
      (setq newlist return))
    newlist))

(defun torus--duo-insert-at-group-end (new list &optional test-group)
  "Insert NEW in LIST, at the end of a group determined by TEST-GROUP.
Return LIST.
NEW is the value of the element inserted.
TEST-GROUP takes two arguments and returns t if they belongs to the same group.
TEST-GROUP defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `torus--duo-naive-pop' to know why.
Common usage :
\(setq list (torus--duo-insert-at-group-end new list))
Modifies LIST."
  (let ((newlist list)
        (previous (torus--duo-member new list test-group))
        (return))
    (while (and previous
                (funcall test-group (car (cdr previous)) new))
      (setq previous (cdr previous)))
    (when previous
      (torus--duo-insert-next previous new))
    newlist))

;;; Filter
;;; ------------------------------

(defun torus--duo-filter (test-filter list)
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
;;; ---------------

(defun torus--duo-filter-previous (test-filter cons list)
  "Return reference of previous element of CONS in LIST matching TEST-FILTER."
  (let ((duo list)
        (previous))
    (while (and duo
                (not (eq duo cons)))
      (when (funcall test-filter (car duo))
        (setq previous duo))
      (setq duo (cdr duo)))
    previous))

(defun torus--duo-filter-next (test-filter cons)
  "Return reference of next element of CONS in list matching TEST-FILTER."
  (let ((next (cdr cons)))
    (while (and next
                (not (funcall test-filter (car next))))
      (setq next (cdr next)))
    next))

(defun torus--duo-filter-before (test-filter elem list &optional test-equal)
  "Return reference of element before ELEM in LIST matching TEST-FILTER.
TEST-EQUAL tests equality of two elements, defaults to `equal'."
  (let ((duo (torus--duo-member elem list test-equal)))
    (torus--duo-filter-previous test-filter duo list)))

(defun torus--duo-filter-after (test-filter elem list &optional test-equal)
  "Return reference of element after ELEM in LIST matching TEST-FILTER.
TEST-EQUAL tests equality of two elements, defaults to `equal'."
  (let ((duo (torus--duo-member elem list test-equal)))
    (torus--duo-filter-next test-filter duo)))

;;; Circular
;;; ---------------

(defun torus--duo-circ-filter-previous (test-filter cons list)
  "Return reference of previous element of CONS in LIST matching TEST-FILTER."
  (if (eq cons list)
      (torus--duo-filter-previous test-filter
                              (torus--duo-last list)
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

(defun torus--duo-circ-filter-next (test-filter cons list)
  "Return reference of next element of CONS in LIST matching TEST-FILTER."
  (let ((next (cdr cons)))
    (while (and next
                (not (funcall test-filter (car next))))
      (setq next (cdr next)))
    (if next
        next
      (torus--duo-filter-next test-filter list))))

(defun torus--duo-circ-filter-before (test-filter elem list &optional test-equal)
  "Return reference of element before ELEM in LIST matching TEST-FILTER.
TEST-EQUAL tests equality of two elements, defaults to `equal'."
  (let ((duo (torus--duo-member elem list test-equal)))
    (torus--duo-circ-filter-previous test-filter duo list)))

(defun torus--duo-circ-filter-after (test-filter elem list &optional test-equal)
  "Return reference of element after ELEM in LIST matching TEST-FILTER.
TEST-EQUAL tests equality of two elements, defaults to `equal'."
  (let ((duo (torus--duo-member elem list test-equal)))
    (torus--duo-circ-filter-next test-filter duo list)))

;;; Alists
;;; ------------------------------------------------------------

;;; Find
;;; ------------------------------

(defun torus--duo-assoc (key list &optional test-equal)
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

(defun torus--duo-reverse-assoc (value list &optional test-equal)
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

;;; Group
;;; ------------------------------

(defun torus--duo-partition (funkey list)
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
      (setq key-list (torus--duo-assoc key assoc-list))
      (if key-list
          (torus--duo-add (car duo) (car key-list))
        (if assoc-list
            (torus--duo-add (list key (car duo)) assoc-list)
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
