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

(in-package :sb-cga-vm)

;;;; VECTOR COPYING

#-sb-cga-sse2
(declaim (inline %copy-vec))
(defun %copy-vec (result vec)
  "Copy contents of VEC into RESULT, return RESULT. Unsafe."
  (declare (optimize (speed 3) (safety 0) (debug 0) (sb-c::recognize-self-calls 0)))
  #+sb-cga-sse2
  (%copy-vec result vec)
  #-sb-cga-sse2
  (macrolet ((dim (n)
               `(setf (aref result ,n) (aref vec ,n))))
    (dim 0)
    (dim 1)
    (dim 2)
    (dim 3)
    result))

;;;; VECTOR ADDITION

#-sb-cga-sse2
(declaim (inline %vec+))
(defun %vec+ (result a b)
  "Add VEC A and B, store result in VEC RESULT. Return RESULT. Unsafe"
  (declare (optimize (speed 3) (safety 0) (debug 0) (sb-c::recognize-self-calls 0)))
  #+sb-cga-sse2
  (%vec+ result a b)
  #-sb-cga-sse2
  (macrolet ((dim (n)
               `(setf (aref result ,n) (+ (aref a ,n) (aref b ,n)))))
    (dim 0)
    (dim 1)
    (dim 2)
    (dim 3)
    result))

;;;; VECTOR SUBSTRACTION

#-sb-cga-sse2
(declaim (inline %vec-))
(defun %vec- (result a b)
  "Substract VEC B from VEC A, store result in VEC RESULT. Return RESULT.
Unsafe."
  (declare (optimize (speed 3) (safety 0) (debug 0) (sb-c::recognize-self-calls 0)))
  #+sb-cga-sse2
  (%vec- result a b)
  #-sb-cga-sse2
  (macrolet ((dim (n)
               `(setf (aref result ,n) (- (aref a ,n) (aref b ,n)))))
    (dim 0)
    (dim 1)
    (dim 2)
    (dim 3)
    result))

;;;; VECTOR/SCALAR MULTIPLICATION

#-sb-cga-sse2
(declaim (inline %vec*))
(defun %vec* (result a f)
  "Multiply VEC A with single-float F, store result in VEC RESULT. Return
RESULT. Unsafe."
  (declare (optimize (speed 3) (safety 0) (debug 0) (sb-c::recognize-self-calls 0)))
  #+sb-cga-sse2
  (%vec* result a f)
  #-sb-cga-sse2
  (macrolet ((dim (n)
               `(setf (aref result ,n) (* (aref a ,n) f))))
    (dim 0)
    (dim 1)
    (dim 2)
    (dim 3)
    result))

;;;; VECTOR/SCALAR DIVISION

#-sb-cga-sse2
(declaim (inline %vec/))
(defun %vec/ (result a f)
  "Divide VEC A by single-float F, store result in VEC RESULT. Return RESULT.
Unsafe."
  (declare (optimize (speed 3) (safety 0) (debug 0) (sb-c::recognize-self-calls 0)))
  #+sb-cga-sse2
  (%vec/ result a f)
  #-sb-cga-sse2
  (macrolet ((dim (n)
               `(setf (aref result ,n) (/ (aref a ,n) f))))
    (dim 0)
    (dim 1)
    (dim 2)
    (dim 3)
    result))

;;;; HADAMARD PRODUCT

#-sb-cga-sse2
(declaim (inline %hadamard-product))
(defun %hadamard-product (result a b)
  "Compute hadamard product (elementwise product) of VEC A and VEC B, store
result in VEC RESULT. Return RESULT. Unsafe."
  (declare (optimize (speed 3) (safety 0) (debug 0) (sb-c::recognize-self-calls 0)))
  #+sb-cga-sse2
  (%hadamard-product result a b)
  #-sb-cga-sse2
  (macrolet ((dim (n)
               `(setf (aref result ,n) (* (aref a ,n) (aref b ,n)))))
    (dim 0)
    (dim 1)
    (dim 2)
    (dim 3)
    result))

;;;; NORMALIZATION

#-sb-cga-sse2
(declaim (inline %normalize))
(defun %normalize (result a)
  "Normalize VEC A, store result into VEC RESULT. Return RESULT. Unsafe."
  (declare (optimize (speed 3) (safety 0) (debug 0) (sb-c::recognize-self-calls 0)))
  #+sb-cga-sse2
  (%normalize result a)
  #-sb-cga-sse2
  (let* ((va (aref a 0))
         (vb (aref a 1))
         (vc (aref a 2))
         (vd (aref a 3))
         (len (sqrt (+ (* va va) (* vb vb) (* vc vc) (* vd vd)))))
    (setf (aref result 0) (/ va len)
          (aref result 1) (/ vb len)
          (aref result 2) (/ vc len)
          (aref result 3) (/ vd len))
    result))

;;;; LINEAR INTERPOLATION

#-sb-cga-sse2
(declaim (inline %vec-lerp))
(defun %vec-lerp (result a b f)
  "Linear interpolate VEC A and VEC B using single-float F as the
interpolation factor, store result in VEC RESULT. Return RESULT. Unsafe."
  (declare (optimize (speed 3) (safety 0) (debug 0) (sb-c::recognize-self-calls 0)))
  #+sb-cga-sse2
  (%vec-lerp result a b f)
  #-sb-cga-sse2
  (let ((f2 (- 1.0 f)))
    (macrolet ((dim (n)
                 `(setf (aref result ,n) (+ (* f2 (aref a ,n)) (* f (aref b .b))))))
      (dim 0)
      (dim 1)
      (dim 2)
      (dim 3))
    result))

;;;; TRANSFORMING A VECTOR

#-sb-cga-sse2
(declaim (inline %transform-vec))
(defun %transform-vec (result vec matrix)
  "Apply transformation MATRIX to VEC, store result in RESULT. Return RESULT. Unsafe."
  (declare (optimize (speed 3) (safety 0) (debug 0) (sb-c::recognize-self-calls 0)))
  #+sb-cga-sse2
  (%transform-vec result vec matrix)
  #-sb-cga-sse2
  (let ((a (aref vec 0))
        (b (aref vec 1))
        (c (aref vec 2))
        (d (aref vec 3)))
    (macrolet ((dim (n)
                 `(setf (aref result ,n)
                        (+ (* a (mref matrix ,n 0))
                           (* b (mref matrix ,n 1))
                           (* c (mref matrix ,n 2))
                           (* d (mref matrix ,n 3))))))
      (dim 0)
      (dim 1)
      (dim 2)
      (dim 3)
      result)))
