;;;; by Nikodemus Siivola <nikodemus@random-state.net>, 2009.
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

(defmacro define-vm-fun (name lambda-list &body generic-body)
  (multiple-value-bind (forms declarations doc)
      (sb-int:parse-body generic-body :doc-string-allowed t)
    (declare (ignorable forms))
    `(progn
       #-sb-cga-sse2
       (declaim (inline ,name))
       (defun ,name ,lambda-list
         ,@(when doc (list doc))
         ,@declarations
         (declare (optimize (speed 3) (safety 1) (debug 1) (sb-c::recognize-self-calls 0)))
         #+sb-cga-sse2
         (,name ,@lambda-list)
         #-sb-cga-sse2
         (progn ,@forms)))))

;;;; COMPARISON

(define-vm-fun %vec= (a b)
  (macrolet ((dim (n)
               `(= (aref a ,n) (aref b ,n))))
    (and (dim 0) (dim 1) (dim 2))))

;;;; VECTOR COPYING

(define-vm-fun %copy-vec (result vec)
  "Copy contents of VEC into RESULT, return RESULT. Unsafe."
  (macrolet ((dim (n)
               `(setf (aref result ,n) (aref vec ,n))))
    (dim 0)
    (dim 1)
    (dim 2)
    result))

;;;; VECTOR ADDITION

(define-vm-fun %vec+ (result a b)
  "Add VEC A and B, store result in VEC RESULT. Return RESULT. Unsafe"
  (macrolet ((dim (n)
               `(setf (aref result ,n) (+ (aref a ,n) (aref b ,n)))))
    (dim 0)
    (dim 1)
    (dim 2)
    result))

(define-vm-fun %%vec+/1 (a b) (%vec+ a a b))
(define-vm-fun %%vec+/2 (a b) (%vec+ b a b))

;;;; VECTOR SUBSTRACTION

(define-vm-fun %vec- (result a b)
  "Substract VEC B from VEC A, store result in VEC RESULT. Return RESULT.
Unsafe."
  (macrolet ((dim (n)
               `(setf (aref result ,n) (- (aref a ,n) (aref b ,n)))))
    (dim 0)
    (dim 1)
    (dim 2)
    result))

(define-vm-fun %%vec-/1 (a b) (%vec- a a b))
(define-vm-fun %%vec-/2 (a b) (%vec- b a b))

;;;; VECTOR/SCALAR MULTIPLICATION

(define-vm-fun %vec* (result a f)
  "Multiply VEC A with single-float F, store result in VEC RESULT. Return
RESULT. Unsafe."
  (macrolet ((dim (n)
               `(setf (aref result ,n) (* (aref a ,n) f))))
    (dim 0)
    (dim 1)
    (dim 2)
    result))

(define-vm-fun %%vec*/1 (a f) (%vec* a a f))

;;;; VECTOR/SCALAR DIVISION

(define-vm-fun %vec/ (result a f)
  "Divide VEC A by single-float F, store result in VEC RESULT. Return RESULT.
Unsafe."
  (macrolet ((dim (n)
               `(setf (aref result ,n) (/ (aref a ,n) f))))
    (dim 0)
    (dim 1)
    (dim 2)
    result))

(define-vm-fun %%vec//1 (a f) (%vec/ a a f))

;;;; DOT PRODUCT

(define-vm-fun %dot-product (a b)
  (macrolet ((dim (n)
               `(* (aref a ,n) (aref b ,n))))
    (+ (dim 0) (dim 1) (dim 2))))

;;;; HADAMARD PRODUCT

(define-vm-fun %hadamard-product (result a b)
  "Compute hadamard product (elementwise product) of VEC A and VEC B, store
result in VEC RESULT. Return RESULT. Unsafe."
  (macrolet ((dim (n)
               `(setf (aref result ,n) (* (aref a ,n) (aref b ,n)))))
    (dim 0)
    (dim 1)
    (dim 2)
    result))

(define-vm-fun %%hadamard-product/1 (a b) (%hadamard-product a a b))
(define-vm-fun %%hadamard-product/2 (a b) (%hadamard-product b a b))

;;;; LENGTH

(define-vm-fun %vec-length (a)
  (macrolet ((dim (n)
               `(let ((d (aref a ,n)))
                  (* d d))))
    (sqrt (+ (dim 0) (dim 1) (dim 2)))))

;;;; NORMALIZATION

(define-vm-fun %normalize (result a)
  "Normalize VEC A, store result into VEC RESULT. Return RESULT. Unsafe."
  (let* ((va (aref a 0))
         (vb (aref a 1))
         (vc (aref a 2))
         (len (sqrt (+ (* va va) (* vb vb) (* vc vc)))))
    (setf (aref result 0) (/ va len)
          (aref result 1) (/ vb len)
          (aref result 2) (/ vc len))
    result))

(define-vm-fun %%normalize/1 (a) (%normalize a a))

;;;; LINEAR INTERPOLATION

(define-vm-fun %vec-lerp (result a b f)
  "Linear interpolate VEC A and VEC B using single-float F as the
interpolation factor, store result in VEC RESULT. Return RESULT. Unsafe."
  (let ((f2 (- 1.0 f)))
    (macrolet ((dim (n)
                 `(setf (aref result ,n) (+ (* f2 (aref a ,n)) (* f (aref b ,n))))))
      (dim 0)
      (dim 1)
      (dim 2))
    result))

(define-vm-fun %%vec-lerp/1 (a b d) (%vec-lerp a a b f))
(define-vm-fun %%vec-lerp/2 (a b d) (%vec-lerp b a b f))

;;;; TRANSFORMING A VECTOR -- either as a point or a direction

(define-vm-fun %transform-point (result vec matrix)
  "Apply transformation MATRIX to VEC, store result in RESULT. Return RESULT. Unsafe."
  (let ((a (aref vec 0))
        (b (aref vec 1))
        (c (aref vec 2)))
    (macrolet ((dim (n)
                 `(setf (aref result ,n)
                        (+ (* a (mref matrix ,n 0))
                           (* b (mref matrix ,n 1))
                           (* c (mref matrix ,n 2))
                           (mref matrix ,n 3)))))
      (dim 0)
      (dim 1)
      (dim 2)
      result)))

(define-vm-fun %%transform-point/1 (vec matrix)
  (%transform-point vec vec matrix))

(define-vm-fun %transform-direction (result vec matrix)
  "Apply transformation MATRIX to VEC, store result in RESULT. Return RESULT. Unsafe."
  (let ((a (aref vec 0))
        (b (aref vec 1))
        (c (aref vec 2)))
    (macrolet ((dim (n)
                 `(setf (aref result ,n)
                        (+ (* a (mref matrix ,n 0))
                           (* b (mref matrix ,n 1))
                           (* c (mref matrix ,n 2))))))
      (dim 0)
      (dim 1)
      (dim 2)
      result)))

(define-vm-fun %%transform-direction/1 (vec matrix)
  (%transform-direction vec vec matrix))

;;;; ADJUSTING A VECTOR

(define-vm-fun %adjust-vec (result point direction distance)
  "Multiply VEC DIRECTION by single-float DISTANCE adding the result to VEC POINT.
Store result in RESULT, and return it."
  (macrolet ((dim (n)
               `(setf (aref result ,n)
                      (+ (aref point ,n) (* (aref direction ,n) distance)))))
    (dim 0)
    (dim 1)
    (dim 2)
    result))

(define-vm-fun %%adjust-vec/1 (point direction distance)
  (%adjust-vec point point direction distance))
(define-vm-fun %%adjust-vec/2 (point direction distance)
  (%adjust-vec direction point direction distance))

;;;; Mapping from n-ary consing to non-consing versions where the first
;;;; argument is both an operand an a place to store the result.

(defvar *optimizable-funs* nil)

(defun optimize-vec-allocation (form)
  ;; If the first or second argument is known to return a freshly
  ;; consed value, reuse it by rewriting:
  ;;
  ;;  (foo (bar ...) ...)   -> (%%foo/1 (bar ...) ...)
  ;;  (foo x (bar ...) ...) -> (%%foo/2 x (bar ...) ...)
  ;;
  (flet ((opt-arg-p (arg)
           (and (consp arg)
                (assoc (car arg) *optimizable-funs* :test #'eq))))
    (destructuring-bind (name/1 name/2)
        (cdr (assoc (car form) *optimizable-funs* :test #'eq))
      (let ((res (if name/2
                     (destructuring-bind (arg1 arg2 &rest more) (cdr form)
                       (cond ((opt-arg-p arg1)
                              `(,name/1 ,arg1 ,arg2 ,@more))
                             ((opt-arg-p arg2)
                              `(,name/2 ,arg1 ,arg2 ,@more))
                             (t
                              form)))
                     (destructuring-bind (arg1 &rest more) (cdr form)
                       (if (opt-arg-p arg1)
                           `(,name/1 ,arg1 ,@more)
                           form)))))
        #+nil
        (unless (eq form res) (break "~S -> ~S" form res))
        res))))

(defun note-optimizable-fun (name name/1 &optional name/2)
  (let ((cell (assoc name *optimizable-funs* :test #'eq)))
    (if cell
        (setf (cdr cell) (list name/1 name/2))
        (push (list name name/1 name/2) *optimizable-funs*))
    name))
