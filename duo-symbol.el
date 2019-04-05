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

;;; Stack & Queue
;;; ------------------------------------------------------------

(defun duo-sym-push-cons (cons symlist)
  "Add CONS at the beginning of SYMLIST. Return list.
Common usage :
\(duo-sym-push-cons cons 'list)
Destructive."
  (setcdr cons (symbol-value symlist))
  (set symlist cons)
  cons)

(defun duo-sym-add-cons (cons symlist &optional last)
  "Store CONS at the end of SYMLIST. Return CONS.
If non nil, LAST is used to speed up the process.
Common usage :
\(duo-sym-add-cons cons 'list)
Destructive."
  (let ((last (if last
                  last
                (duo-last (symbol-value symlist)))))
    (if last
        (setcdr last cons)
      (set symlist cons))
    (setcdr cons nil)
    cons))

(defun duo-sym-push (elem symlist)
  "Add ELEM at the beginning of SYMLIST. Return list.
Common usage :
\(duo-sym-push elem 'list)
Destructive."
  (set symlist (cons elem (symbol-value symlist)))
  (symbol-value symlist))

(defun duo-sym-add (elem symlist &optional last)
  "Add ELEM at the end of SYMLIST. Return the new LAST.
If non nil, LAST is used to speed up the process.
Common usage :
\(duo-sym-add elem 'list)
Destructive."
  (let ((last (if last
                  last
                (duo-last (symbol-value symlist))))
        (duo (cons elem nil)))
    (if last
        (setcdr last duo)
      (set symlist duo))
    duo))

(defun duo-sym-push-new-cons (cons symlist)
  "Add CONS at the beginning of SYMLIST if not already there. Return list.
Common usage :
\(duo-sym-push-new-cons cons 'list)
Destructive."
  (let ((list (symbol-value symlist)))
    (if (duo-inside cons list)
        list
      (duo-sym-push-cons cons symlist))))

(defun duo-sym-add-new-cons (cons symlist &optional last)
  "Add CONS at the end of SYMLIST if not already there. Return the new LAST.
If non nil, LAST is used to speed up the process.
Common usage :
\(duo-sym-add-new-cons cons 'list)
Destructive."
  (let ((list (symbol-value symlist)))
    (unless (duo-inside cons list)
      (duo-sym-add-cons cons symlist last))))

(defun duo-sym-push-new (elem symlist &optional fn-equal)
  "Add ELEM at the beginning of SYMLIST if not already there. Return list.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Common usage :
\(duo-sym-push-new elem 'list)
Destructive."
  (let ((list (symbol-value symlist)))
    (if (duo-member elem list fn-equal)
        list
      (duo-sym-push elem symlist))))

(defun duo-sym-add-new (elem symlist &optional last fn-equal)
  "Add ELEM at the end of SYMLIST if not already there.
Return the new LAST.
If non nil, LAST is used to speed up the process.
FN-EQUAL takes two arguments and return t if they are considered equals.
FN-EQUAL defaults to `equal'.
Common usage :
\(duo-sym-add-new elem 'list)
Destructive."
  (let ((list (symbol-value symlist)))
    (unless (duo-member elem list fn-equal)
      (duo-sym-add elem symlist last))))

;;; End
;;; ------------------------------------------------------------

(provide 'duo-symbol)

;; Local Variables:
;; mode: emacs-lisp
;; indent-tabs-mode: nil
;; End:

;;; duo-symbol.el ends here
