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
  "Accessor for value in the specificed ROW and COLUMN in MATRIX."
  (aref matrix (+ row (* column 4))))

(declaim (inline (setf mref)))
(defun (setf mref) (value matrix row column)
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

(declaim (ftype (sfunction (matrix matrix &optional single-float) boolean) matrix~))
(defun matrix~ (a b &optional (epsilon +default-epsilon+))
  "Return true if MATRIX A and MATRIX B are elementwise within EPSILON of each other.
EPSILON defaults to +DEFAULT-EPSILON+"
  (let ((-e (- epsilon)))
    (dotimes (i 16 t)
      (unless (<= -e (- (aref a i) (aref b i)) epsilon)
        (return nil)))))

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

(declaim (ftype (sfunction () matrix) identity-matrix))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun identity-matrix ()
    "Construct an identity matrix."
    (matrix 1.0 0.0 0.0 0.0
            0.0 1.0 0.0 0.0
            0.0 0.0 1.0 0.0
            0.0 0.0 0.0 1.0)))

(defconstant +identity-matrix+
  (if (boundp '+identity-matrix+)
      (symbol-value '+identity-matrix+)
      (identity-matrix))
  "Constant identity matrix.")

;;;; MATRIX MULTIPLICATION

(declaim (ftype (sfunction (&rest matrix) matrix) matrix*))
(defun matrix* (&rest matrices)
  "Multiply MATRICES. The result might not be freshly allocated if all,
or all but one multiplicant is an identity matrix."
  (labels ((mul (left more)
             (if more
                 (if (eq left +identity-matrix+)
                     (mul (pop more) more)
                     (let ((right (pop more)))
                       (if (eq +identity-matrix+ right)
                           (mul left more)
                           (let ((result (zero-matrix)))
                             (dotimes (i 4)
                               (dotimes (j 4)
                                 (setf (mref result i j)
                                       (loop for k below 4
                                             summing (* (mref left i k) (mref right k j))))))
                             (mul result more)))))
                 left)))
    (cond ((not matrices)
           +identity-matrix+)
          ((cdr matrices)
           (mul (car matrices) (cdr matrices)))
          (t
           (car matrices)))))

;;;; TRANSFORMATIONS

(declaim (ftype (sfunction (single-float single-float single-float) matrix) translate*))
(defun translate* (x y z)
  "Construct a translation matrix from translation factors X, Y and Z."
  (matrix 1.0 0.0 0.0 x
          0.0 1.0 0.0 y
          0.0 0.0 1.0 z
          0.0 0.0 0.0 1.0))

(declaim (ftype (sfunction (vec) matrix) translate))
(defun translate (vec)
  "Construct a translation matrix using first three elements of VEC as the
translation factors."
  (translate* (aref vec 0) (aref vec 1) (aref vec 2)))

(declaim (ftype (sfunction (single-float single-float single-float) matrix) scale*))
(defun scale* (x y z)
  "Construct a scaling matrix from scaling factors X, Y, and Z."
  (matrix x    0.0  0.0  0.0
          0.0  y    0.0  0.0
          0.0  0.0  z    0.0
          0.0  0.0  0.0  1.0))

(declaim (ftype (sfunction (vec) matrix) scale))
(defun scale (vec)
  "Construct a scaling matrix using first threee elements of VEC as the
scaling factors."
  (scale* (aref vec 0) (aref vec 1) (aref vec 2)))

(declaim (ftype (sfunction (single-float single-float single-float) matrix) rotate*))
(defun rotate* (x y z)
  "Construct a rotation matrix from rotation factors X, Y, Z."
  (let ((rotate (identity-matrix)))
    (unless (= 0.0 z)
      (let ((c (cos z))
            (s (sin z)))
        (setf rotate (matrix* rotate
                              (matrix c     (- s) 0.0    0.0
                                      s     c     0.0    0.0
                                      0.0   0.0   1.0    0.0
                                      0.0   0.0   0.0    1.0)))))
    (unless (= 0.0 y)
      (let ((c (cos y))
            (s (sin y)))
        (setf rotate (matrix* rotate
                              (matrix c     0.0   s      0.0
                                      0.0   1.0   0.0    0.0
                                      (- s) 0.0   c      0.0
                                      0.0   0.0   0.0    1.0)))))
    (unless (= 0.0 x)
      (let ((c (cos x))
            (s (sin x)))
        (setf rotate (matrix* rotate
                              (matrix 1.0   0.0   0.0    0.0
                                      0.0   c     (- s)  0.0
                                      0.0   s     c      0.0
                                      0.0   0.0   0.0    1.0)))))
    rotate))

(declaim (ftype (sfunction (vec) matrix) rotate))
(defun rotate (vec)
  "Construct a rotation matrix using first three elements of VEC as the
rotation factors."
  (rotate* (aref vec 0) (aref vec 1) (aref vec 2)))

(declaim (ftype (sfunction (vec single-float) matrix) rotate-around))
(defun rotate-around (a radians)
  "Construct a rotation matrix that rotates by RADIANS around VEC A. 4th
element of A is ignored."
  (cond ((vec= a (vec 1.0 0.0 0.0))
         (rotate* radians 0.0 0.0))
        ((vec= a (vec 0.0 1.0 0.0))
         (rotate* 0.0 radians 0.0))
        ((vec= a (vec 0.0 0.0 1.0))
         (rotate* 0.0 0.0 radians))
        (t
         (let ((c (cos radians))
               (s (sin radians))
               (g (- 1.0 (cos radians))))
           (let* ((x (aref a 0))
                  (y (aref a 1))
                  (z (aref a 2))
                  (gxx (* g x x)) (gxy (* g x y)) (gxz (* g x z))
                  (gyy (* g y y)) (gyz (* g y z)) (gzz (* g z z)))
             (matrix
              (+ gxx c)        (- gxy (* s z))  (+ gxz (* s y)) 0.0
              (+ gxy (* s z))  (+ gyy c)        (- gyz (* s x)) 0.0
              (- gxz (* s y))  (+ gyz (* s x))  (+ gzz c)       0.0
              0.0              0.0              0.0             1.0))))))

(declaim (ftype (sfunction (vec vec) matrix) reorient))
(defun reorient (a b)
  "Construct a transformation matrix to reorient A with B."
  (let ((na (normalize a))
	(nb (normalize b)))
    (if (vec~ na nb)
	(identity-matrix)
	(rotate-around (normalize (cross-product na nb))
		       (acos (dot-product na nb))))))

(declaim (ftype (sfunction (matrix) matrix) transpose-matrix))
(defun transpose-matrix (matrix)
  "Transpose of MATRIX."
  (let ((transpose (zero-matrix)))
    (dotimes (i 4)
      (dotimes (j 4)
	(setf (mref transpose i j) (mref matrix j i))))
    transpose))

(declaim (ftype (sfunction (matrix) matrix) inverse-matrix))
(defun inverse-matrix (matrix)
  "Inverse of MATRIX. Signals an error if there is no inverse."
  (declare (type matrix matrix))
  (if (eq matrix +identity-matrix+)
      +identity-matrix+
      (if (and (= 0.0 (mref matrix 3 0) (mref matrix 3 1) (mref matrix 3 2))
               (= 1.0 (mref matrix 3 3)))
          ;; Affine matrix, fast track (and less loss of accuracy from multiplications)
          (let ((inverse (zero-matrix)))
            ;; Inverse of the upper left 3x3
            (let ((det (submatrix-determinant matrix)))
              (if (zerop det)
                  ;; If the 3x3 is zero, we're fine -- otherwise punt to the complete
                  ;; implementation.
                  (dotimes (i 3)
                    (dotimes (j 3)
                      (unless (= 0.0 (mref matrix i j))
                        (return-from inverse-matrix (generic-inverse-matrix matrix)))))
                  (macrolet ((inv ((i j) (ai aj bi bj) (ci cj di dj))
                             `(setf (mref inverse ,(1- i) ,(1- j))
                                    (/ (- (* (mref matrix ,(1- ai) ,(1- aj))
                                             (mref matrix ,(1- bi) ,(1- bj)))
                                          (* (mref matrix ,(1- ci) ,(1- cj))
                                             (mref matrix ,(1- di) ,(1- dj))))
                                       det))))
                  (inv (1 1) (2 2 3 3) (2 3 3 2))
                  (inv (1 2) (1 3 3 2) (1 2 3 3))
                  (inv (1 3) (1 2 2 3) (1 3 2 2))
                  (inv (2 1) (2 3 3 1) (2 1 3 3))
                  (inv (2 2) (1 1 3 3) (1 3 3 1))
                  (inv (2 3) (1 3 2 1) (1 1 2 3))
                  (inv (3 1) (2 1 3 2) (2 2 3 1))
                  (inv (3 2) (1 2 3 1) (1 1 3 2))
                  (inv (3 3) (1 1 2 2) (1 2 2 1)))))
            ;; translation: negation after dotting with upper rows
            (let ((x (mref matrix 0 3))
                  (y (mref matrix 1 3))
                  (z (mref matrix 2 3)))
              (dotimes (i 3)
                (setf (mref inverse i 3) (- (+ (* x (mref inverse i 0))
                                               (* y (mref inverse i 1))
                                               (* z (mref inverse i 2)))))))
            ;; affine bottom row (0 0 0 1)
            (setf (mref inverse 3 3) 1.0)
            inverse)
          (generic-inverse-matrix matrix))))

(defun submatrix-determinant (matrix)
  "Determinant of the upper left 3x3 submatrix of MATRIX."
  (macrolet ((a (i j)
               `(mref matrix ,(- i 1) ,(- j 1))))
    (- (+ (* (a 1 1) (a 2 2) (a 3 3))
          (* (a 2 1) (a 3 2) (a 1 3))
          (* (a 3 1) (a 1 2) (a 2 3)))
       (* (a 1 1) (a 3 2) (a 2 3))
       (* (a 3 1) (a 2 2) (a 1 3))
       (* (a 2 1) (a 1 2) (a 3 3)))))

(defun matrix-determinant (matrix)
  "Determinant of MATRIX."
  (macrolet ((a (i j)
               `(mref matrix (- ,i 1) (- ,j 1))))
    (- (+ (* (a 1 1) (a 2 2) (a 3 3) (a 4 4))
          (* (a 1 1) (a 2 3) (a 3 4) (a 4 2))
          (* (a 1 1) (a 2 4) (a 3 2) (a 4 3))

          (* (a 1 2) (a 2 1) (a 3 4) (a 4 3))
          (* (a 1 2) (a 2 3) (a 3 1) (a 4 4))
          (* (a 1 2) (a 2 4) (a 3 3) (a 4 1))

          (* (a 1 3) (a 2 1) (a 3 2) (a 4 4))
          (* (a 1 3) (a 2 2) (a 3 4) (a 4 1))
          (* (a 1 3) (a 2 4) (a 3 1) (a 4 2))

          (* (a 1 4) (a 2 1) (a 3 3) (a 4 2))
          (* (a 1 4) (a 2 2) (a 3 1) (a 4 3))
          (* (a 1 4) (a 2 3) (a 3 2) (a 4 1)))

       (* (a 1 1) (a 2 2) (a 3 4) (a 4 3))
       (* (a 1 1) (a 2 3) (a 3 2) (a 4 4))
       (* (a 1 1) (a 2 4) (a 3 3) (a 4 2))

       (* (a 1 2) (a 2 1) (a 3 3) (a 4 4))
       (* (a 1 2) (a 2 3) (a 3 4) (a 4 1))
       (* (a 1 2) (a 2 4) (a 3 1) (a 4 3))

       (* (a 1 3) (a 2 1) (a 3 4) (a 4 2))
       (* (a 1 3) (a 2 2) (a 3 1) (a 4 4))
       (* (a 1 3) (a 2 4) (a 3 2) (a 4 1))

       (* (a 1 4) (a 2 1) (a 3 2) (a 4 3))
       (* (a 1 4) (a 2 2) (a 3 3) (a 4 1))
       (* (a 1 4) (a 2 3) (a 3 1) (a 4 2)))))

(eval-when (:compile-toplevel)
  ;; The next baby wants this
  (setf sb-ext:*inline-expansion-limit* 1000))

(defun generic-inverse-matrix (matrix)
  (let ((det (matrix-determinant matrix)))
    (if (< (abs det) +default-epsilon+)
        (error "Cannot invert matrix with zero determinant:~%  ~S"
               matrix)
        (macrolet ((a (x y z)
                     (multiple-value-bind (r1 c1) (truncate (- x 11) 10)
                       (multiple-value-bind (r2 c2) (truncate (- y 11) 10)
                         (multiple-value-bind (r3 c3) (truncate (- z 11) 10)
                           `(* (mref matrix ,r1 ,c1)
                               (mref matrix ,r2 ,c2)
                               (mref matrix ,r3 ,c3)))))))
          (let ((m
                 (matrix
                  ;; row 1
                  (- (+ (a 22 33 44) (a 23 34 42) (a 24 32 43))
                     (a 22 34 43) (a 23 32 44) (a 24 33 42))
                  (- (+ (a 12 34 43) (a 13 32 44) (a 14 33 42))
                     (a 12 33 44) (a 13 34 42) (a 14 32 43))
                  (- (+ (a 12 23 44) (a 13 24 42) (a 14 22 43))
                     (a 12 24 43) (a 13 22 44) (a 14 23 42))
                  (- (+ (a 12 24 33) (a 13 22 34) (a 14 23 32))
                     (a 12 23 34) (a 13 24 32) (a 14 22 33))
                  ;; row 2
                  (- (+ (a 21 34 43) (a 23 31 44) (a 24 33 41))
                     (a 21 33 44) (a 23 34 41) (a 24 31 43))
                  (- (+ (a 11 33 44) (a 13 34 41) (a 14 31 43))
                     (a 11 34 43) (a 13 31 44) (a 14 33 41))
                  (- (+ (a 11 24 43) (a 13 21 44) (a 14 23 41))
                     (a 11 23 44) (a 13 24 41) (a 14 21 43))
                  (- (+ (a 11 23 34) (a 13 24 31) (a 14 21 33))
                     (a 11 24 33) (a 13 21 34) (a 14 23 31))
                  ;; row 3
                  (- (+ (a 21 32 44) (a 22 34 41) (a 24 31 42))
                     (a 21 34 42) (a 22 31 44) (a 24 32 41))
                  (- (+ (a 11 34 42) (a 12 31 44) (a 14 32 41))
                     (a 11 32 44) (a 12 34 41) (a 14 31 42))
                  (- (+ (a 11 22 44) (a 12 24 41) (a 14 21 42))
                     (a 11 24 42) (a 12 21 44) (a 14 22 41))
                  (- (+ (a 11 24 32) (a 12 21 34) (a 14 22 31))
                     (a 11 22 34) (a 12 24 31) (a 14 21 32))
                  ;; row 4
                  (- (+ (a 21 33 42) (a 22 31 43) (a 23 32 41))
                     (a 21 32 43) (a 22 33 41) (a 23 31 42))
                  (- (+ (a 11 32 43) (a 12 33 41) (a 13 31 42))
                     (a 11 33 42) (a 12 31 43) (a 13 32 41))
                  (- (+ (a 11 23 42) (a 12 21 43) (a 13 22 41))
                     (a 11 22 43) (a 12 23 41) (a 13 21 42))
                  (- (+ (a 11 22 33) (a 12 23 31) (a 13 21 32))
                     (a 11 23 32) (a 12 21 33) (a 13 22 31)))))
            (dotimes (i 4)
              (dotimes (j 4)
                (setf (mref m i j) (/ (mref m i j) det))))
            m)))))