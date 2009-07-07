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

;;;; ACCESSORS

(declaim (ftype (sfunction (matrix (integer 0 3) (integer 0 3)) single-float) mref)
         (inline mref))
(defun mref (matrix row column)
  "Return value in the specificed ROW and COLUMN in MATRIX."
  (aref matrix (+ row (* column 4))))

(declaim (inline %set-mref))
(defun %set-mref (matrix row column value)
  (setf (aref matrix (+ row (* column 4))) value))

;;; PRETTY-PRINTING

(defun pprint-matrix (stream matrix)
  (pprint-logical-block (stream nil)
    (print-unreadable-object (matrix stream :type nil :identity nil)
      (dotimes (i 4)
        (format stream "[~s, ~s, ~s, ~s]"
                (mref matrix i 0)
                (mref matrix i 1)
                (mref matrix i 2)
                (mref matrix i 3))
        (unless (= 3 i)
          (pprint-newline :mandatory stream))))))
(set-pprint-dispatch 'matrix 'pprint-matrix)

;;;; PREDICATES

(declaim (ftype (sfunction (t) boolean))
         (inline matrixp))
(defun matrixp (object)
  "Return true of OBJECT is a matrix."
  (typep object 'matrix))

;;;; COMPARISON

(declaim (ftype (sfunction (matrix matrix) boolean) matrix=))
(defun matrix= (a b)
  "Return true if MATRIX A is elementwise equal to MATRIX B."
  (dotimes (i 16 t)
    (unless (= (aref a i) (aref b i))
      (return nil))))

;;;; CONSTRUCTORS

(declaim (ftype (sfunction (single-float single-float single-float single-float
                            single-float single-float single-float single-float
                            single-float single-float single-float single-float
                            single-float single-float single-float single-float)
                           matrix)
                matrix)
         (inline matrix))
(defun matrix (m11 m12 m13 m14
               m21 m22 m23 m24
               m31 m32 m33 m34
               m41 m42 m43 m44)
  "Construct MATRIX with the given elements (arguments are provided in row
major order.)"
  (make-array 16
              :element-type 'single-float
              :initial-contents (list m11 m21 m31 m41
                                      m12 m22 m32 m42
                                      m13 m23 m33 m43
                                      m14 m24 m34 m44)))

(declaim (ftype (sfunction () matrix) zero-matrix)
         (inline zero-matrix))
(defun zero-matrix ()
  "Construct a zero matrix."
  (make-array 16 :element-type 'single-float))

(declaim (ftype (sfunction () matrix) identity-matrix)
         (inline identity-matrix))
(defun identity-matrix ()
  "Construct an identity matrix."
  (matrix 1.0 0.0 0.0 0.0
          0.0 1.0 0.0 0.0
          0.0 0.0 1.0 0.0
          0.0 0.0 0.0 1.0))
