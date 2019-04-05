;;; duo.el --- In place list operations in Elisp     -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Chimay

;; Author : Chimay
;; Name: Duo
;; Package-Version: 1.3
;; Package-requires: ((emacs "26"))
;; Keywords: lisp, extensions, list, in-place, operation
;; URL: https://github.com/chimay/duo

;;; Commentary:

;; Library of in place list operations in Emacs-Lisp.
;;
;; See also https://github.com/chimay/duo/blob/master/README.org
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
;;
;; There is a slight difference between next/previous and after/before
;; functions :
;;
;;   - Next / Previous use a cons as main argument
;;   - After / Before use the value of an element of the list as main argument
;;
;; There is a slight difference between remove and delete functions :
;;
;;   - Remove removes a cons given as argument
;;   - Delete remove the first cons whose car matches an element given as argument
;;
;; The fn-* accepting two arguments are called like this :
;;
;; (funcall fn-* cons-from-loop elem-or-cons-from-argument)

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
  (require 'duo-common)
  (require 'duo-return)
  (require 'duo-referen)
  (require 'duo-symbol))

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

;;; Reference
;;; ------------------------------

;;; Cons Cons
;;; ---------------

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

;;; Elem Cons
;;; ---------------

(defun duo-ref-insert-cons-before (elem new reflist &optional previous fn-equal)
  "Insert NEW before ELEM in list referenced by REFLIST. Return NEW.
ELEM must be present in list.
NEW is the cons inserted.
If non nil, PREVIOUS inserted is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
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
  (let* ((fn-equal (if fn-equal
                         fn-equal
                       #'equal))
         (list (duo-deref reflist))
         (previous (if previous
                       previous
                     (duo-before elem list 1 fn-equal)))
         (duo (if (funcall fn-equal (car list) elem)
                  list
                (cdr previous))))
    (duo-ref-insert-cons-previous duo new reflist previous)))

;;; Elem Elem
;;; ---------------

(defun duo-ref-insert-before (elem new reflist &optional previous fn-equal)
  "Insert NEW before ELEM in list referenced by REFLIST. Return cons of NEW.
ELEM must be present in list.
NEW is the value of the element inserted.
If non nil, PREVIOUS inserted is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
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
  (let* ((fn-equal (if fn-equal
                         fn-equal
                       #'equal))
         (list (duo-deref reflist))
         (previous (if previous
                       previous
                     (duo-before elem list 1 fn-equal)))
         (cons-elem (if (funcall fn-equal (car list) elem)
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

;;; Reference
;;; ------------------------------

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

(defun duo-ref-delete (elem reflist &optional previous fn-equal)
  "Delete ELEM from list referenced by REFLIST. Return removed cons.
If non nil, PREVIOUS deleted is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
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
         (fn-equal (if fn-equal
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
        (removed-list)
        (last)
        (list)
        (duo)
        (next)
        (fn-equal (if fn-equal
                      fn-equal
                    #'equal)))
    (while (funcall fn-equal (car (duo-deref reflist)) elem)
      (setq removed (duo-ref-pop reflist))
      (if removed-list
          (setq last (duo-add-cons removed removed-list last))
        (setq removed-list removed)
        (setq last removed)))
    (setq list (duo-deref reflist))
    (setq duo list)
    (while duo
      (setq next (cdr duo))
      (when (funcall fn-equal (car duo) elem)
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

;;; Reference
;;; ------------------------------

;;; Cons Cons
;;; ---------------

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
    (duo-insert-cons-next cons moved))
  moved)

;;; Cons Elem
;;; ---------------

(defun duo-ref-teleport-previous (cons moved reflist &optional
                                       previous-removed previous-inserted
                                       fn-equal)
  "Move MOVED before CONS in list referenced by REFLIST. Return MOVED.
CONS must be a cons in list referenced by REFLIST.
MOVED is the value of the moved element.
If non nil, PREVIOUS-REMOVED and PREVIOUS-INSERTED
are used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
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
         (duo (duo-member moved list fn-equal)))
    (duo-ref-teleport-cons-previous cons duo reflist
                                    previous-removed previous-inserted)))

(defun duo-ref-teleport-next (cons moved reflist &optional previous fn-equal)
  "Move MOVED after CONS in list referenced by REFLIST. Return MOVED.
CONS must be a cons in list referenced by REFLIST.
MOVED is the value of the moved element.
If non nil, PREVIOUS removed is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
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
         (duo (duo-member moved list fn-equal)))
    (duo-ref-teleport-cons-next cons duo reflist previous)))

;;; Elem Cons
;;; ---------------

(defun duo-ref-teleport-cons-before (elem moved reflist &optional
                                          previous-removed previous-inserted
                                          fn-equal)
  "Move MOVED before ELEM in list referenced by REFLIST. Return MOVED.
ELEM must be present in list.
MOVED is the cons of the moved element.
If non nil, PREVIOUS-REMOVED and PREVIOUS-INSERTED
are used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
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
         (duo (duo-member elem list fn-equal)))
    (duo-ref-teleport-cons-previous duo moved reflist
                                    previous-removed previous-inserted)))

(defun duo-ref-teleport-cons-after (elem moved reflist &optional previous fn-equal)
  "Move MOVED after ELEM in list referenced by REFLIST. Return MOVED.
ELEM must be present in list.
MOVED is the cons of the moved element.
If non nil, PREVIOUS removed is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
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
         (duo (duo-member elem list fn-equal)))
    (duo-ref-teleport-cons-next duo moved reflist previous)))

;;; Elem Elem
;;; ---------------

(defun duo-ref-teleport-before (elem moved reflist &optional
                                     previous-removed previous-inserted
                                     fn-equal)
  "Move MOVED before ELEM in list referenced by REFLIST. Return MOVED.
ELEM must be present in list.
MOVED is the value of the moved element.
If non nil, PREVIOUS-REMOVED and PREVIOUS-INSERTED
are used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
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
         (elem-cons (duo-member elem list fn-equal))
         (moved-cons (duo-member moved list fn-equal)))
    (duo-ref-teleport-cons-previous elem-cons moved-cons reflist
                                    previous-removed previous-inserted)))

(defun duo-ref-teleport-after (elem moved reflist &optional previous fn-equal)
  "Move MOVED after ELEM in LIST. Return (cons of MOVED . LIST).
ELEM must be present in list.
MOVED is the value of the moved element.
If non nil, PREVIOUS removed is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
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
         (elem-cons (duo-member elem list fn-equal))
         (moved-cons (duo-member moved list fn-equal)))
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

;;; Reference
;;; ------------------------------

;;; Linear
;;; ---------------

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
         (num (if num
                  num
                1))
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
         (num (if num
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
         (num (if num
                  num
                1))
         (moved (duo-member elem list fn-equal))
         (landmark (nthcdr num moved)))
    (unless landmark
      (setq landmark (duo-last list)))
    (duo-ref-teleport-cons-next landmark moved reflist)
    moved))

;;; Circular
;;; ---------------

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
         (num (if num
                  num
                1))
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
         (num (if num
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
         (num (if num
                  num
                1))
         (moved (duo-member elem list fn-equal))
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

;;; Reference
;;; ------------------------------

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

;;; Reference
;;; ------------------------------

(defun duo-ref-insert-in-sorted-list (new reflist &optional fn-less)
  "Insert NEW at the right place in list referecend by REFLIST.
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
  (let ((fn-less (if fn-less
                     fn-less
                   #'<))
        (list (duo-deref reflist)))
    (cond ((not list) (duo-ref-set reflist (cons new nil)))
          ((funcall fn-less new (car list)) (duo-ref-push new reflist))
          (t (let ((duo list)
                   (next (cdr list)))
               (while (and duo
                           next
                           (funcall fn-less (car next) new))
                 (setq duo (cdr duo))
                 (setq next (cdr next)))
               (duo-insert-next duo new))))))

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

;;; Insert
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

;;; Reference
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
          (duo-insert-next previous new)
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
         (fn-group (if fn-group
                         fn-group
                       #'equal))
         (previous (duo-member new list fn-group))
         (duo))
    (while (and previous
                (funcall fn-group (car (cdr previous)) new))
      (setq previous (cdr previous)))
    (if previous
        (progn
          (duo-insert-next previous new)
          (setq duo (cdr previous)))
      (setq duo (duo-ref-add new reflist)))
    duo))

;;; Filter
;;; ------------------------------------------------------------

(defun duo-filter (list &optional fn-filter)
  "Return list of elements in LIST matching FN-FILTER.
FN-FILTER takes one argument and return t if the element passes the filter.
FN-FILTER defaults to `identity'.
LIST is not modified."
  (let ((fn-filter (if fn-filter
                       fn-filter
                     #'identity))
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
  (let ((fn-filter (if fn-filter
                       fn-filter
                     #'identity))
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
  (let ((fn-filter (if fn-filter
                       fn-filter
                     #'identity))
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
  (let ((fn-filter (if fn-filter
                       fn-filter
                     #'identity))
        (duo (duo-member elem list fn-equal)))
    (duo-filter-previous duo list fn-filter)))

(defun duo-filter-after (elem list &optional fn-filter fn-equal)
  "Return cons of element after ELEM in LIST matching FN-FILTER.
FN-FILTER takes one argument and return t if the element passes the filter.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let ((fn-filter (if fn-filter
                       fn-filter
                     #'identity))
        (duo (duo-member elem list fn-equal)))
    (duo-filter-next duo fn-filter)))

;;; Circular
;;; ------------------------------

(defun duo-circ-filter-previous (cons list &optional fn-filter)
  "Return cons of previous element of CONS in LIST matching FN-FILTER.
FN-FILTER takes one argument and return t if the element passes the filter."
  (let ((fn-filter (if fn-filter
                       fn-filter
                     #'identity))
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
  (let ((fn-filter (if fn-filter
                       fn-filter
                     #'identity))
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
  (let ((fn-filter (if fn-filter
                       fn-filter
                     #'identity))
        (duo (duo-member elem list fn-equal)))
    (duo-circ-filter-previous duo list fn-filter)))

(defun duo-circ-filter-after (elem list &optional fn-filter fn-equal)
  "Return cons of element after ELEM in LIST matching FN-FILTER.
FN-FILTER takes one argument and return t if the element passes the filter.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'."
  (let ((fn-filter (if fn-filter
                       fn-filter
                     #'identity))
        (duo (duo-member elem list fn-equal)))
    (duo-circ-filter-next duo list fn-filter)))

;;; Partition
;;; ------------------------------------------------------------

(defun duo-partition (list &optional fn-key)
  "Partition LIST using FN-KEY.
The result is an alist whose keys are given by the values of FN-KEY
applied to the elements of LIST.
Each element of the alist is of the form :
\(key elem-1 elem-2 ... elem-N)
where all the elem-* verify (FN-KEY elem-?) = key.
FN-KEY defaults to `identity'."
  (let ((fn-key (if fn-key
                    fn-key
                  #'identity))
        (duo list)
        (assoc-list)
        (key)
        (key-list))
    (while duo
      (setq key (funcall fn-key (car duo)))
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
