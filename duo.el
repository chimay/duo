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

(defun duo-set-deref (ptr object)
  "Change the content of the variable referenced by PTR to OBJECT.
OBJECT must be a cons or a list."
  (setcar ptr (car object))
  (setcdr ptr (cdr object))
  ptr)

;;; Lists
;;; ------------------------------------------------------------

;;; Find
;;; ------------------------------

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

(defun duo-at-index (index list)
  "Element at INDEX in LIST."
  (nthcdr index list))

(defun duo-index (elem list &optional test-equal)
  "Index of ELEM in LIST.
ELEM must be present in list.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'."
  (- (length list) (length (duo-member elem list test-equal))))

;;; Next / Previous
;;; ------------------------------

(defun duo-previous (cons list &optional num)
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

(defun duo-next (cons &optional num)
  "Return cons of NUM elements after CONS in list.
NUM defaults to 1.
CONS must reference a cons in the list."
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
;;; ---------------

(defun duo-circ-previous (cons list &optional num)
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

(defun duo-circ-next (cons list &optional num)
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
;;; ------------------------------

(defun duo-update (old new list &optional test-equal)
  "Replace OLD by NEW in LIST. Return cons of NEW.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
Modifies LIST."
  (let ((duo (duo-member old list test-equal)))
    (when duo
      (setcar duo new))
    duo))

;;; Add / Remove at Beg / End
;;; ------------------------------

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
Modifies LIST."
  (let* ((newlist))
    (setcdr cons list)
    (setq newlist cons)
    newlist))

(defun duo-add-cons (cons list &optional last)
  "Store CONS at the end of LIST. Return CONS.
If non nil, LAST is used to speed up the process.
Modifies LIST."
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
Modifies LIST."
  (let* ((duo (cons elem list))
         (newlist duo))
    newlist))

(defun duo-add (elem list &optional last)
  "Add ELEM at the end of LIST. Return the new LAST.
If non nil, LAST is used to speed up the process.
Modifies LIST."
  (let ((last (if last
                  last
                (duo-last list)))
        (duo (cons elem nil)))
    (setcdr last duo)
    duo))

(defun duo-push-new (elem list)
  "Add ELEM at the beginning of LIST if not already there. Return LIST.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-push-new elem list))
Modifies LIST."
  (if (member elem list)
      list
    (duo-push elem list)))

(defun duo-add-new (elem list &optional last)
  "Add ELEM at the end of LIST if not already there. Return the new LAST.
If non nil, LAST is used to speed up the process.
Modifies LIST."
  (unless (member elem list)
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
Modifies LIST."
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

(defun duo-push-and-truncate (elem list &optional num)
  "Add ELEM at the beginning of LIST. Truncate LIST to NUM elements.
Return LIST.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-push-and-truncate elem list))
Modifies LIST."
  (let ((newlist list))
    (setq newlist (duo-push elem list))
    (duo-truncate newlist num)
    newlist))

;;; Reference
;;; ---------------

(defun duo-ref-push-cons (cons reflist)
  "Add CONS at the beginning of the car of REFLIST. Return REFLIST.
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
Modifies LIST."
  (setcdr cons (car reflist))
  (setcar reflist cons)
  reflist)

(defun duo-ref-push (elem reflist)
  "Add ELEM at the beginning of the car of REFLIST. Return REFLIST.
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
Modifies LIST."
  (let ((duo (cons elem (car reflist))))
    (setcar reflist duo)
    reflist))

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
;;; ------------------------------

(defun duo-rotate-left (list)
  "Rotate LIST to the left. Return LIST.
Equivalent to pop first element and add it to the end.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq list (duo-rotate-left list))
Modifies LIST."
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
Modifies LIST."
  ;; Length list > 1
  (if (cdr list)
      (let ((duo (duo-drop list)))
        (duo-push-cons duo list))
    list))

;;; Reference
;;; ---------------

(defun duo-ref-rotate-left (reflist)
  "Rotate car of REFLIST to the left. Return REFLIST.
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
Modifies LIST."
  (let ((list (car reflist)))
    ;; Length list > 1
    (if (cdr list)
        (let ((popped (duo-ref-pop reflist)))
          (setq list (car reflist))
          (duo-add-cons popped list)
          reflist)
      reflist)))

(defun duo-ref-rotate-right (reflist)
  "Rotate car of REFLIST to the right. Return REFLIST.
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
Modifies LIST."
  (let ((list (car reflist)))
    ;; Length list > 1
    (if (cdr list)
        (let ((dropped (duo-drop list)))
          (duo-ref-push-cons dropped reflist))
      reflist)))

;;; Reverse
;;; ------------------------------

(defun duo-reverse (list)
  "Reverse LIST. Return LIST.
Modifies LIST."
  (let* ((begin list)
         (end (duo-last list))
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
        (setq end (duo-previous end list)))))
  list)

;;; Insert
;;; ------------------------------

;;; Cons Cons
;;; ---------------

(defun duo-insert-cons-previous (cons new list &optional previous)
  "Insert NEW before CONS in LIST. Return NEW.
CONS must reference a cons in LIST.
NEW is the cons inserted.
If non nil, PREVIOUS inserted is used to speed up the process.
If the new cons is inserted at the beginning of the list,
the actual new list must be recovered using new LIST = NEW.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq return (duo-insert-cons-previous cons new list))
\(when (eq (cdr return) list)
  (setq list return))
Modifies LIST."
  (if (eq cons list)
      (duo-push-cons new list)
    (let ((previous (if previous
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
CONS must reference a cons in LIST.
NEW is the cons inserted.
Modifies LIST."
    (setcdr new (cdr cons))
    (setcdr cons new)
    new)

;;; Cons Elem
;;; ---------------

(defun duo-insert-previous (cons new list &optional previous)
  "Insert NEW before CONS in LIST. Return cons of NEW.
CONS must reference a cons in LIST.
NEW is the value of the element inserted.
If non nil, PREVIOUS inserted is used to speed up the process.
If the new cons is inserted at the beginning of the list,
the actual new list must be recovered using new LIST = NEW.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq return (duo-insert-previous cons new list))
\(when (eq (cdr return) list)
  (setq list return))
Modifies LIST."
  (let ((duo (list new)))
    (duo-insert-cons-previous cons duo list previous)))

(defun duo-insert-next (cons new)
  "Insert NEW after CONS in list. Return cons of NEW.
CONS must reference a cons in LIST.
NEW is the value of the element inserted.
Modifies LIST."
  (let ((duo (list new)))
    (duo-insert-cons-next cons duo)))

;;; Elem Cons
;;; ---------------

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
Modifies LIST."
  (let ((previous (if previous
                      previous
                    (duo-before elem list 1 test-equal)))
        (duo (if (eq elem (car list))
                 list
               (cdr previous))))
    (duo-insert-cons-previous duo new list previous)))

(defun duo-insert-cons-after (elem new list &optional test-equal)
  "Insert NEW after ELEM in LIST. Return NEW.
ELEM must be present in list.
NEW is the cons inserted.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
Modifies LIST."
  (let ((duo (duo-member elem list test-equal)))
    (duo-insert-cons-next duo new)))

;;; Elem Elem
;;; ---------------

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
Modifies LIST."
  (let ((previous (if previous
                      previous
                    (duo-before elem list 1 test-equal)))
        (cons-elem (if (eq elem (car list))
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
Modifies LIST."
  (let ((cons-elem (duo-member elem list test-equal))
        (cons-new (list new)))
    (duo-insert-cons-next cons-elem cons-new)))

;;; Reference
;;; ---------------

;;; Cons Cons
;;; ----------

(defun duo-ref-insert-cons-previous (cons new reflist &optional previous)
  "Insert NEW before CONS in car of REFLIST. Return NEW.
CONS must reference a cons in LIST.
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
Modifies LIST."
  (let ((list (car reflist)))
    (if (eq cons list)
        (car (duo-ref-push-cons new reflist))
      (let ((previous (if previous
                        previous
                      (duo-previous cons list))))
        (if previous
            (progn
              (setcdr new (cdr previous))
              (setcdr previous new)
              new)
          nil)))))

;;; Cons Elem
;;; ----------

(defun duo-ref-insert-previous (cons new reflist &optional previous)
  "Insert NEW before CONS in car of REFLIST. Return cons of NEW.
CONS must reference a cons in LIST.
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
Modifies LIST."
  (let ((duo (list new)))
    (duo-ref-insert-cons-previous cons duo reflist previous)))

;;; Elem Cons
;;; ----------

(defun duo-ref-insert-cons-before (elem new reflist &optional previous test-equal)
  "Insert NEW before ELEM in car of REFLIST. Return NEW.
ELEM must be present in list.
NEW is the cons inserted.
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
Modifies LIST."
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
;;; ----------

(defun duo-ref-insert-before (elem new reflist &optional previous test-equal)
  "Insert NEW before ELEM in car of REFLIST. Return cons of NEW.
ELEM must be present in list.
NEW is the value of the element inserted.
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
Modifies LIST."
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
;;; ------------------------------

(defun duo-remove (cons list &optional previous)
  "Remove CONS from LIST. Return (CONS . LIST).
CONS must reference a cons in LIST.
If non nil, PREVIOUS removed is used to speed up the process.
The actual new list must be recovered using the returned structure.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-remove cons list))
\(setq removed (car pair))
\(setq list (cdr pair))
Modifies LIST."
  (if (eq cons list)
      (duo-pop list)
    (let ((previous (if previous
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
Modifies LIST."
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
;;; ---------------

(defun duo-ref-remove (cons reflist)
  "Remove CONS from car of REFLIST. Return CONS.
CONS must reference a cons in LIST.
REFLIST must be a cons (list . whatever-you-want)
See the docstring of `duo-naive-pop' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-remove cons reflist)
;; Update list
\(setq mylist (car reflist))
Modifies LIST."
  (let ((list (car reflist)))
    (if (eq cons list)
        (duo-ref-pop reflist)
      (let* ((previous (duo-previous cons list)))
        (when previous
          (setcdr previous (cdr cons))
          (setcdr cons nil))
        cons))))

(defun duo-ref-delete (elem reflist &optional test-equal)
  "Delete ELEM from car of REFLIST. Return removed cons.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
REFLIST must be a cons (list . whatever-you-want)
See the docstring of `duo-naive-pop' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-delete elem reflist)
;; Update list
\(setq mylist (car reflist))
Modifies LIST."
  (let ((list (car reflist))
        (test-equal (if test-equal
                        test-equal
                      #'equal)))
    (if (funcall test-equal (car list) elem)
        (duo-ref-pop reflist)
      (let* ((previous (duo-before elem list 1 test-equal))
             (duo (cdr previous)))
        (when previous
          (setcdr previous (cdr duo))
          (setcdr duo nil))
        duo))))

(defun duo-ref-delete-all (elem reflist &optional test-equal)
  "Delete all elements equals to ELEM from car of REFLIST.
Return list of removed cons.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
REFLIST must be a cons (list . whatever-you-want)
See the docstring of `duo-naive-pop' to know why it doesn’t
use the list itself in argument.
Common usage :
;; Create reflist
\(setq reflist (list mylist))
;; Modify
\(duo-ref-delete-all elem reflist)
;; Update list
\(setq mylist (car reflist))
Modifies LIST."
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

;;; Move
;;; ------------------------------

;;; Step
;;; ---------------

(defun duo-move-previous (cons list)
  "Move CONS to previous place in LIST. Return (CONS . LIST).
CONS must reference a cons in LIST.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq list (duo-move-previous cons list))
Modifies LIST."
  (if (eq cons (cdr list))
      (let ((newlist cons))
        (setcdr list (cdr newlist))
        (setcdr newlist list)
        (cons cons newlist))
    (let* ((before (duo-previous cons list 2))
           (after (cdr before)))
      (when before
        (setcdr after (cdr cons))
        (setcdr cons after)
        (setcdr before cons))
      (cons cons list))))

(defun duo-move-next (cons list)
  "Move CONS to next place in LIST.")

(defun duo-move-before (elem list)
  "Move ELEM to next place in LIST.")

(defun duo-move-after (elem list)
  "Move ELEM to next place in LIST.")

;;; Circular
;;; ---------------

(defun duo-move-circ-previous (cons list)
  "Move CONS to previous place in LIST.
Circular : if in beginning of list, go to the end."
  )

(defun duo-move-circ-next (cons list)
  "Move CONS to next place in LIST.
Circular : if in end of list, go to the beginning."
  )

(defun duo-move-circ-before (elem list)
  "Move ELEM to next place in LIST.
Circular : if in beginning of list, go to the end."
  )

(defun duo-move-circ-after (elem list)
  "Move ELEM to next place in LIST.
Circular : if in end of list, go to the beginning."
  )

;;; Reference
;;; ---------------

;;; Teleport
;;; ------------------------------

;;; Cons Cons
;;; ---------------

(defun duo-teleport-cons-previous (cons moved list)
  "Move MOVED before CONS in LIST. Return (MOVED . LIST).
CONS must reference a cons in LIST.
MOVED is the cons of the moved element.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-teleport-cons-previous cons moved list))
\(setq moved (car pair))
\(setq list (cdr pair))
Modifies LIST."
  (let ((newlist list)
        (return))
    (unless (eq cons moved)
      (setq newlist (cdr (duo-remove moved list)))
      (setq return (duo-insert-cons-previous cons moved newlist))
      (when (eq (cdr return) newlist)
        (setq newlist return)))
    (cons moved newlist)))

(defun duo-teleport-cons-next (cons moved list)
  "Move MOVED after CONS in LIST. Return (MOVED . LIST).
CONS must reference a cons in LIST.
MOVED is the cons of the moved element.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-teleport-cons-next cons moved list))
\(setq moved (car pair))
\(setq list (cdr pair))
Modifies LIST."
  (let ((newlist list)
        (return))
    (unless (eq cons moved)
      (setq newlist (cdr (duo-remove moved list)))
      (duo-insert-cons-next cons moved))
    (cons moved newlist)))

;;; Cons Elem
;;; ---------------

(defun duo-teleport-previous (cons moved list &optional test-equal)
  "Move MOVED before CONS in LIST. Return (cons of MOVED . LIST).
CONS must reference a cons in LIST.
MOVED is the value of the moved element.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-teleport-previous cons moved list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Modifies LIST."
  (let ((duo (duo-member moved list test-equal)))
    (duo-teleport-cons-previous cons duo list)))

(defun duo-teleport-next (cons moved list &optional test-equal)
  "Move MOVED after CONS in LIST. Return (cons of MOVED . LIST).
CONS must reference a cons in LIST.
MOVED is the value of the moved element.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-teleport-next cons moved list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Modifies LIST."
  (let ((duo (duo-member moved list test-equal)))
    (duo-teleport-cons-next cons duo list)))

;;; Elem Cons
;;; ---------------

(defun duo-teleport-cons-before (elem moved list &optional test-equal)
  "Move MOVED before ELEM in LIST. Return (MOVED . LIST).
ELEM must be present in list.
MOVED is the cons of the moved element.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-teleport-before elem moved list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Modifies LIST."
  (let ((duo (duo-member elem list test-equal)))
    (duo-teleport-cons-previous duo moved list)))

(defun duo-teleport-cons-after (elem moved list &optional test-equal)
  "Move MOVED after ELEM in LIST. Return (MOVED . LIST).
ELEM must be present in list.
MOVED is the cons of the moved element.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-teleport-after elem moved list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Modifies LIST."
  (let ((duo (duo-member elem list test-equal)))
    (duo-teleport-cons-next duo moved list)))

;;; Elem Elem
;;; ---------------

(defun duo-teleport-before (elem moved list &optional test-equal)
  "Move MOVED before ELEM in LIST. Return (cons of MOVED . LIST).
ELEM must be present in list.
MOVED is the value of the moved element.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-push' to know why.
Common usage :
\(setq pair (duo-teleport-before elem moved list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Modifies LIST."
  (let ((elem-cons (duo-member elem list test-equal))
        (moved-cons (duo-member moved list test-equal)))
    (duo-teleport-cons-previous elem-cons moved-cons list)))

(defun duo-teleport-after (elem moved list &optional test-equal)
  "Move MOVED after ELEM in LIST. Return (cons of MOVED . LIST).
ELEM must be present in list.
MOVED is the value of the moved element.
TEST-EQUAL takes two arguments and return t if they are considered equals.
TEST-EQUAL defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq pair (duo-teleport-after elem moved list))
\(setq cons-moved (car pair))
\(setq list (cdr pair))
Modifies LIST."
  (let ((elem-cons (duo-member elem list test-equal))
        (moved-cons (duo-member moved list test-equal)))
    (duo-teleport-cons-next elem-cons moved-cons list)))

;;; Reference
;;; ---------------

;;; Cons Cons
;;; ----------

;;; Cons Elem
;;; ----------

;;; Elem Cons
;;; ----------

;;; Elem Elem
;;; ----------

;;; Group
;;; ------------------------------

(defun duo-insert-at-group-beg (new list &optional test-group)
  "Insert NEW in LIST, at the beginning of a group determined by TEST-GROUP.
Return LIST.
NEW is the value of the element inserted.
TEST-GROUP takes two arguments and returns t if they belongs to the same group.
TEST-GROUP defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq list (duo-insert-at-group-beg new list))
Modifies LIST."
  (let ((newlist list)
        (return))
    (setq return (duo-insert-before new new list test-group))
    (when (eq (cdr return) newlist)
      (setq newlist return))
    newlist))

(defun duo-insert-at-group-end (new list &optional test-group)
  "Insert NEW in LIST, at the end of a group determined by TEST-GROUP.
Return LIST.
NEW is the value of the element inserted.
TEST-GROUP takes two arguments and returns t if they belongs to the same group.
TEST-GROUP defaults do `equal'.
The actual new list must be recovered using the returned list.
See the docstring of `duo-naive-pop' to know why.
Common usage :
\(setq list (duo-insert-at-group-end new list))
Modifies LIST."
  (let ((newlist list)
        (previous (duo-member new list test-group))
        (return))
    (while (and previous
                (funcall test-group (car (cdr previous)) new))
      (setq previous (cdr previous)))
    (when previous
      (duo-insert-next previous new))
    newlist))

;;; Filter
;;; ------------------------------

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
;;; ---------------

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
;;; ---------------

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

;;; Alists
;;; ------------------------------------------------------------

;;; Find
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

;;; Partition
;;; ------------------------------

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
