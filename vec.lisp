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

(defmacro define-opt-fun (name lambda-list doc &key no-compiler-macro)
  (let ((vm/1-name (symbolicate "%%" name "/1"))
        (vm-name (symbolicate "%" name))
        (form (gensym "FORM")))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel)
         (note-optimizable-fun ',name ',vm/1-name))
       (declaim (inline ,name))
       (defun ,name ,lambda-list ,doc (,vm-name (alloc-vec) ,@lambda-list))
       ,@(unless no-compiler-macro
                 `((define-compiler-macro ,name (&whole ,form ,@lambda-list)
                     (declare (ignore ,@lambda-list))
                     (optimize-vec-allocation ,form)))))))

(defmacro define-opt-fun2 (name lambda-list doc)
  (let ((vm/1-name (symbolicate "%%" name "/1"))
        (vm/2-name (symbolicate "%%" name "/2"))
        (vm-name (symbolicate "%" name))
        (form (gensym "FORM")))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel)
         (note-optimizable-fun ',name ',vm/1-name ',vm/2-name))
       (declaim (inline ,name))
       (defun ,name ,lambda-list ,doc (,vm-name (alloc-vec) ,@lambda-list))
       (define-compiler-macro ,name (&whole ,form ,@lambda-list)
         (declare (ignore ,@lambda-list))
         (optimize-vec-allocation ,form)))))

(defmacro define-alloc-fun (name lambda-list &body body)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel)
       (pushnew ',name *alloc-funs*))
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@body)))

;;;; CONSTRUCTORS

(declaim (ftype (sfunction () vec) alloc-vec))
(define-alloc-fun alloc-vec ()
  "Allocate a zero-initialized VEC."
  (make-array 3 :element-type 'single-float :initial-element 0.0))

(declaim (ftype (sfunction (single-float single-float single-float) vec) vec))
(define-alloc-fun vec (x y z)
  "Allocate a 3d vector [X, Y, Z]."
  (make-array 3 :element-type 'single-float :initial-contents (list x y z)))

;;;; COPYING

(declaim (inline %%copy-vec/1))
(defun %%copy-vec/1 (vec)
  ;; Not really a copy, but allows optimizing copies away.
  vec)

(declaim (ftype (sfunction (vec) vec) copy-vec))
(define-opt-fun copy-vec (vec)
  "Allocate a fresh copy of VEC.")

;;;; ARITHMETIC

(declaim (ftype (sfunction (vec vec) vec) vec+))
(define-opt-fun2 vec+ (a b)
  "Add VEC A and VEC B, return result as a freshly allocated VEC.")

(declaim (ftype (sfunction (vec vec) vec) vec-))
(define-opt-fun2 vec- (a b)
  "Substract VEC B from VEC A, return result as a freshly allocated VEC.")

(declaim (ftype (sfunction (vec single-float) vec) vec*))
(define-opt-fun vec* (a f)
  "Multiply VEC A with single-float F, return result as a freshly allocated
VEC.")

(declaim (ftype (sfunction (vec single-float) vec) vec/))
(define-opt-fun vec/ (a f)
  "Divide VEC A by single-float F, return result as a freshly allocated VEC.")

;;; FIXME: Unless this is inline SBCL doesn't seem to trust
;;; the declared type!
(declaim (ftype (sfunction (vec vec) single-float) dot-product)
         (inline dot-product))
(defun dot-product (a b)
  "Compute dot product VEC A and VEC B."
  (%dot-product a b))

(declaim (ftype (sfunction (vec vec) vec) hadamard-product))
(define-opt-fun2 hadamard-product (a b)
  "Compute hadamard product (elementwise product) of VEC A and VEC B,
return result as a freshly allocated VEC.")

(declaim (ftype (sfunction (vec) single-float) vec-length))
(defun vec-length (a)
  "Length of VEC A."
  (%vec-length a))

(declaim (ftype (sfunction (vec) vec)))
(define-opt-fun normalize (a)
  "Normalize VEC A, return result as a freshly allocated VEC."
  :no-compiler-macro t)
(define-compiler-macro normalize (&whole form a)
  (if (consp a)
      (let ((op (car a))
            (args (cdr a)))
        (case op
          (vec
           `(%%normalized-vec (alloc-vec) ,@args))
          (cross-product
           `(%normalized-cross-product (alloc-vec) ,@args))
          (otherwise
           (optimize-vec-allocation form))))
      form))

(declaim (ftype (sfunction (vec vec single-float) vec) vec-lerp))
(define-opt-fun2 vec-lerp (a b f)
  "Linear interpolate VEC A and VEC B using single-float F as the
interpolation factor, return result as a freshly allocated VEC.")

;;; FIXME: It's a bit ugly that %VEC-MIN and %VEC-MAX have a fixed
;;; arity, whereas VEC-MIN and VEC-MAX have variable arity. Generalize
;;; and write a compiler-macro.
;;;
;;; Similarly it would be nice for most VEC operations to have
;;; a variable arity, but it has to be made efficient...
(declaim (ftype (sfunction (vec vec vec) vec) %vec-min)
         (inline %vec-min))
(defun %vec-min (result a b)
  "Elementwise minimum of VEC A and VEC B, store result in VEC RESULT."
  (macrolet ((dim (n)
               `(setf (aref result ,n) (min (aref a ,n) (aref b ,n)))))
    (dim 0)
    (dim 1)
    (dim 2))
  result)

(declaim (ftype (sfunction (vec vec vec) vec) %vec-max)
         (inline %vec-max))
(defun %vec-max (result a b)
  "Elementwise maximum of VEC A and VEC B, store result in VEC RESULT."
  (macrolet ((dim (n)
               `(setf (aref result ,n) (max (aref a ,n) (aref b ,n)))))
    (dim 0)
    (dim 1)
    (dim 2))
  result)

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

;;;; CROSS-PRODUCT

(defun %cross-product (result a b)
  (let ((a1 (aref a 0))
        (a2 (aref a 1))
        (a3 (aref a 2))
        (b1 (aref b 0))
        (b2 (aref b 1))
        (b3 (aref b 2)))
    (setf (aref result 0) (- (* a2 b3) (* a3 b2))
          (aref result 1) (- (* a3 b1) (* a1 b3))
          (aref result 2) (- (* a1 b2) (* a2 b1)))
    result))

(defun %normalized-cross-product (result a b)
  (let ((a1 (aref a 0))
        (a2 (aref a 1))
        (a3 (aref a 2))
        (b1 (aref b 0))
        (b2 (aref b 1))
        (b3 (aref b 2)))
    (%%normalized-vec result
                      (- (* a2 b3) (* a3 b2))
                      (- (* a3 b1) (* a1 b3))
                      (- (* a1 b2) (* a2 b1)))
    result))

(declaim (ftype (sfunction (vec vec) vec) cross-product))
(define-alloc-fun cross-product (a b)
  "Cross product of 3D vector A and 3D vector B, return result as a freshly
allocated VEC."
  (declare (optimize speed))
  (%cross-product (alloc-vec) a b))

;;;; COMPARISON

(declaim (ftype (sfunction (vec vec) boolean) vec=))
(defun vec= (a b)
  "Return true if VEC A and VEC B are elementwise identical."
  (%vec= a b))

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

(declaim (ftype (sfunction (vec vec single-float) vec) adjust-vec))
(define-opt-fun2 adjust-vec (point direction distance)
  "Multiply VEC DIRECTION by single-float DISTANCE adding the result to VEC POINT.
Return result as a freshly allocated VEC.")

;;;; TRANSFORMATIONS

(declaim (ftype (sfunction (vec matrix) vec) transform-point))
(define-opt-fun transform-point (vec matrix)
  "Apply transformation MATRIX to VEC, return result as a
freshly allocated VEC.")

(declaim (ftype (sfunction (vec matrix) vec) transform-direction))
(define-opt-fun transform-direction (vec matrix)
  "Apply transformation MATRIX to VEC ignoring the translation component,
return result as a freshly allocated VEC.")

(declaim (ftype (function (vec vec matrix) (values vec vec &optional))
                transform-bounds))
(defun transform-bounds (v1 v2 matrix)
  "Transform the axis-aligned bounding box specified by its extreme corners
V1 and V2 using MATRIX. Return new extreme corners (minimum and maximum
coordinates) as freshly allocted VECs, as the primary and secondary value."
  ;; Naive method: transform all corners.
  ;; See http://www.ics.uci.edu/~arvo/code/TransformingBoxes.c
  ;; for a better way.
  (let* ((min (transform-point v1 matrix))
         (max (copy-vec min)))
    (flet ((tran (i j k)
             (let ((tmp (vec i j k)))
               (declare (dynamic-extent tmp))
               (%%transform-point/1 tmp matrix)
               (%vec-min min min tmp)
               (%vec-max max max tmp))))
      (tran (aref v1 0) (aref v1 1) (aref v2 2))
      (tran (aref v1 0) (aref v2 1) (aref v1 2))
      (tran (aref v1 0) (aref v2 1) (aref v2 2))
      (tran (aref v2 0) (aref v1 1) (aref v1 2))
      (tran (aref v2 0) (aref v1 1) (aref v2 2))
      (tran (aref v2 0) (aref v2 1) (aref v1 2))
      (tran (aref v2 0) (aref v2 1) (aref v2 2)))
    (values min max)))

;;;; HASHING

(defun sxhash-vec (vec)
  (declare (type vec vec))
  (logand most-positive-fixnum
          (+ (sxhash (aref vec 0))
             (sxhash (aref vec 1))
             (sxhash (aref vec 2)))))

