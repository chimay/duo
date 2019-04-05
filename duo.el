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

;;; End
;;; ------------------------------------------------------------

(provide 'duo)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; duo.el ends here
