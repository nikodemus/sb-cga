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

;;;; CONSTRUCTORS

(declaim (ftype (sfunction () vec) alloc-vec)
         (inline alloc-vec))
(defun alloc-vec ()
  "Allocate a zero-initialized VEC."
  (make-array 3 :element-type 'single-float))

(declaim (ftype (sfunction (single-float single-float single-float) vec) vec)
         (inline vec))
(defun vec (a b c)
  "Allocate 3D vector [A, B, C]."
  (make-array 3 :element-type 'single-float :initial-contents (list a b c)))

;;;; COPYING

(declaim (ftype (sfunction (vec) vec) copy-vec)
         (inline copy-vec))
(defun copy-vec (vec)
  "Allocate a fresh copy of VEC."
  (%copy-vec (alloc-vec) vec))

;;;; ARITHMETIC

(declaim (ftype (sfunction (vec vec) vec) vec+)
         (inline vec+))
(defun vec+ (a b)
  "Add VEC A and VEC B, return result as a freshly allocated VEC."
  (%vec+ (alloc-vec) a b))

(declaim (ftype (sfunction (vec vec) vec) vec-)
         (inline vec-))
(defun vec- (a b)
  "Substract VEC B from VEC A, return result as a freshly allocated VEC."
  (%vec- (alloc-vec) a b))

(declaim (ftype (sfunction (vec single-float)) vec*)
         (inline vec*))
(defun vec* (a f)
  "Multiply VEC A with single-float F, return result as a freshly allocated
VEC."
  (%vec* (alloc-vec) a f))

(declaim (ftype (sfunction (vec single-float) vec) vec/)
         (inline vec/))
(defun vec/ (a f)
  "Divide VEC A by single-float F, return result as a freshly allocated VEC."
  (%vec/ (alloc-vec) a f))

;;; FIXME: Unless this is inline SBCL doesn't seem to trust
;;; the declared type!
(declaim (ftype (sfunction (vec vec) single-float) dot-product)
         (inline dot-product))
(defun dot-product (a b)
  "Compute dot product VEC A and VEC B."
  (sb-cga-vm:%dot-product a b))

(declaim (ftype (sfunction (vec vec) vec) hadamard-product)
         (inline hadamard-product))
(defun hadamard-product (a b)
  "Compute hadamard product (elementwise product) of VEC A and VEC B,
return result as a freshly allocated VEC."
  (%hadamard-product (alloc-vec) a b))

(declaim (ftype (sfunction (vec) single-float) vec-length))
(defun vec-length (a)
  "Length of VEC A."
  (sb-cga-vm:%vec-length a))

(declaim (ftype (sfunction (vec) vec))
         (inline normalize))
(defun normalize (a)
  "Normalize VEC A, return result as a freshly allocated VEC."
  (%normalize (alloc-vec) a))

(declaim (ftype (sfunction (vec vec single-float) vec) vec-lerp)
         (inline vec-lerp))
(defun vec-lerp (a b f)
  "Linear interpolate VEC A and VEC B using single-float F as the
interpolation factor, return result as a freshly allocated VEC."
  (%vec-lerp (alloc-vec) a b f))

(declaim (ftype (sfunction (vec &rest vec) vec) vec-min))
(defun vec-min (vec &rest vecs)
  "Elementwise minimum of VEC and VECS, return result as a freshly allocated
VEC."
  (declare (dynamic-extent vecs))
  (let ((result (copy-vec vec)))
    (dolist (vec vecs)
      (%vec-min result result vec))
    result))

(declaim (ftype (sfunction (vec &rest vec) vec) vec-max))
(defun vec-max (vec &rest vecs)
  "Elementwise maximum of VEC and VECS, return result as a freshly allocated
VEC."
  (declare (dynamic-extent vecs))
  (let ((result (copy-vec vec)))
    (dolist (vec vecs)
      (%vec-max result result vec))
    result))

(declaim (ftype (sfunction (vec vec) vec) cross-product))
(defun cross-product (a b)
  "Cross product of 3D vector A and 3D vector B, return result as a freshly
allocated VEC."
  (declare (optimize speed))
  (let ((a1 (aref a 0))
        (a2 (aref a 1))
        (a3 (aref a 2))
        (b1 (aref b 0))
        (b2 (aref b 1))
        (b3 (aref b 2)))
    (vec (- (* a2 b3) (* a3 b2))
         (- (* a3 b1) (* a1 b3))
         (- (* a1 b2) (* a2 b1)))))

;;;; COMPARISON

(declaim (ftype (sfunction (vec vec) boolean) vec=))
(defun vec= (a b)
  "Return true if VEC A and VEC B are elementwise identical."
  (sb-cga-vm:%vec= a b))

(declaim (ftype (sfunction (vec vec &optional single-float) boolean) vec~))
(defun vec~ (a b &optional (epsilon +default-epsilon+))
  "Return true if VEC A and VEC B are elementwise within EPSILON of each other.
EPSILON defaults to +DEFAULT-EPSILON+."
  (let ((-e (- epsilon))
        (d (vec- a b)))
    (declare (dynamic-extent d))
    (macrolet ((dim (n)
                 `(<= -e (aref d ,n) epsilon)))
      (and (dim 0) (dim 1) (dim 2)))))

;;;; ADJUSTING A VECTOR

(declaim (ftype (sfunction (vec vec single-float) vec) adjust-vec)
         (inline adjust-vec))
(defun adjust-vec (point direction distance)
  "Multiply VEC DIRECTION by single-float DISTANCE adding the result to VEC POINT.
Return result as a freshly allocated VEC."
  (%adjust-vec (alloc-vec) point direction distance))

