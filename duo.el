;;; duo.el --- In place list operations in Elisp     -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Chimay

;; Author : Chimay
;; Name: Duo
;; Package-Version: 1.3
;; Package-requires: ((emacs "26"))
;; Keywords: lisp, extensions, list, in-place, operation
;; URL: https://github.com/chimay/duo

;;; Commentary:

;; Duo is a library of in place list operations in Emacs-Lisp.
;;
;; See also https://github.com/chimay/duo/blob/master/README.org
;;
;; This library is implemented with (CAR . CDR) cons, which are
;; called duo, hence the name. These cons are everywhere in Elisp,
;; either explicitely created or as brick components in lists. A list
;; variable list is itself the cons at the beginning of the list.
;; (cdr list) is the second cons in the list. And so one with
;; (cddr list), (nthcdr N list). Generally speaking, a member of
;; a list is a cons (value . next-member-in-list). Most of Elisp
;; is built around these (CAR . CDR) double pointers. You can even
;; construct binary trees with it.
;;
;; Cons DUO = (CAR . CDR) can be used as double pointer
;; with setcar and setcdr
;;
;; ELEM = (car DUO)          -> straightforward
;; DUO  = (member ELEM LIST) -> needs loop
;;
;; Modify the argument list
;; ------------------------------
;;
;; When you pass a list as argument to a function, the calling scope
;; list-var holds the address of the first cons of the list. The
;; argument arg-list-var holds a copy of it. Using (setq list ...)
;; inside the definition of the function changes the argument list
;; reference, not the calling scope one. So, the calling scope address is
;; not updated. As a result, you need either to :
;;
;;   - use the list symbol in argument (*-sym-* functions)
;;     + (function ... 'list ...)
;;   - pass a reference to the list (*-ref-* functions)
;;     + (setq reflist (cons list nil))
;;     + (function ... reflist ...)
;;   - recover the modified list in the returned value (*-return-* functions)
;;     + (setq list (function ... list ...))
;;
;; A common case of this situation is with functions which modify the
;; first cons of the list : push, pop, etc.
;;
;; Check their doc to know how to recover the updated list.
;;
;; Files
;; ------------------------------
;;
;; - duo-common.el holds the functions which don’t modify the list
;; - duo-symbol.el holds the *-sym-* functions
;; - duo-referen.el holds the *-ref-* functions
;; - duo-return.el holds the *-return-* functions

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

;; (eval-when-compile
;;   (require 'duo-common)
;;   (require 'duo-symbol)
;;   (require 'duo-referen)
;;   (require 'duo-return))

;;; Init
;;; ------------------------------------------------------------

(defun duo-init (&rest file-list)
  "Load files in LIST."
  (let ((file-list (or file-list (list "duo-common"
                                       "duo-symbol"
                                       "duo-referen"
                                       "duo-return"))))
    (dolist (file file-list)
      (load file))))

;;; End
;;; ------------------------------------------------------------

(provide 'duo)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; duo.el ends here
