;;;; By Nikodemus Siivola <nikodemus@random-state.net>, 2009.
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :sb-cga)

;;; Similar to FUNCTION, but the result type is "exactly" specified: if it is
;;; an object type, then the function returns exactly one value, if it is a
;;; short form of VALUES, then this short form specifies the exact number of
;;; values.
(deftype sfunction (args &optional result)
  (let ((result (cond ((eq result '*) '*)
                      ((or (atom result)
                           (not (eq (car result) 'values)))
                       `(values ,result &optional))
                      ((intersection (cdr result) lambda-list-keywords)
                       result)
                      (t `(values ,@(cdr result) &optional)))))
    `(function ,args ,result)))

;;; VECTOR TYPE

(deftype vec ()
  "A 3D vector of single floats."
  `(simple-array single-float (3)))

;;; MATRIX TYPE

(deftype matrix ()
  "4x4 matrix of single floats, represented as a one-dimensional vector stored
in column-major order."
  `(simple-array single-float (16)))

;;; Miscellany -- should really be somewhere else.

(defconstant +default-epsilon+ 1.e-7
  "Used as a liminal value to work around floating point inaccuracy.")

(defconstant +pi+ (coerce pi 'single-float)
  "Single-float PI.")

(declaim (inline ~))
(defun ~ (a b &optional (epsilon +default-epsilon+))
  "Return true if A and B are within EPSILON of each other. EPSILON
defaults to +DEFAULT-EPSILON+."
  (< (- epsilon) (- a b) epsilon))

;;; Open code comparisons to constants: no substraction needed at runtime.
(define-compiler-macro ~ (&whole form a b &optional (epsilon +default-epsilon+))
  (if (constantp epsilon)
      (flet ((open-code (x constant)
               (let ((c (eval constant))
                     (e (eval epsilon)))
                 `(< ,(- c e) ,x ,(+ c e)))))
        (cond ((constantp a)
               (open-code b a))
              ((constantp b)
               (open-code a b))
              (t
               form)))
      form))
