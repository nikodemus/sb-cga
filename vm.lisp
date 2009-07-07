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

#+sb-cga-sse2
(progn
  (defmacro ea-for-row (vector &optional (index 0))
    `(sb-vm::make-ea :dword :base ,vector
                     :disp (- (+ (* sb-vm:vector-data-offset sb-vm:n-word-bytes)
                                 (* ,index (/ 4 (/ sb-vm:n-word-bytes 4)) sb-vm:n-word-bytes))
                              sb-vm:other-pointer-lowtag)))
  (defmacro load-row (xmm vector &optional (index 0))
    `(inst movaps ,xmm (ea-for-row ,vector ,index)))
  (defmacro store-row (xmm vector &optional (index 0))
    `(inst movaps (ea-for-row ,vector ,index) ,xmm)))

;;;; VECTOR COMPARISON

(defknown %vec= (vec vec) boolean
    (any #+sb-cga-sse2 always-translatable))

#+sb-cga-sse2
(define-vop (%vec=)
    (:translate %vec=)
    (:policy :fast-safe)
    (:args (vector1 :scs (descriptor-reg))
           (vector2 :scs (descriptor-reg)))
    (:conditional :e)
    ;; FIXME: Because there is no plain XMM-REG SC, we abuse SINGLE-REG pretty
    ;; horribly -- same for all the VOPs that follow.
    (:temporary (:sc single-reg) tmp)
    (:temporary (:sc descriptor-reg) mask)
    (:generator 10
      ;; Load vector into TMP
      (load-row tmp vector1)
      ;; Compare
      (inst cmpps :neq tmp (ea-for-row vector2))
      ;; Grab sign bits & check for zero.
      (inst movmskps mask tmp)
      (inst test mask mask)))

#-sb-cga-sse2
(progn
  (declaim (inline %vec=))
  (defun %vec= (a b)
    (macrolet ((dim (n)
                 `(= (aref a ,n) (aref b ,n))))
      (and (dim 0) (dim 1) (dim 2) (dim 3)))))

;;;; VECTOR COPYING

(defknown %copy-vec (vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%copy-vec)
  (:translate %copy-vec)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (vector :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) tmp)
  (:generator 10
    ;; Load vector into TMP
    (load-row tmp vector)
    ;; Save copy to source vector
    (store-row tmp result-vector)
    (move result result-vector)))

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

(defknown %vec+ (vec vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%vec+)
  (:translate %vec+)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (vector1 :scs (descriptor-reg))
         (vector2 :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) tmp)
  (:generator 10
    ;; Load vector into TMP
    (load-row tmp vector1)
    ;; Add
    (inst addps tmp (ea-for-row vector2))
    ;; Save result to source vector
    (store-row tmp result-vector)
    (move result result-vector)))

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

(defknown %vec- (vec vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%vec-)
  (:translate %vec-)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (vector1 :scs (descriptor-reg))
         (vector2 :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) tmp)
  (:generator 10
    ;; Load vector into TMP
    (load-row tmp vector1)
    ;; Sub
    (inst subps tmp (ea-for-row vector2))
    ;; Save result to source vector
    (store-row tmp result-vector)
    (move result result-vector)))

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

(defknown %vec* (vec vec single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%vec*)
  (:translate %vec*)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (vector :scs (descriptor-reg))
         (float :scs (single-reg)))
  (:arg-types * * single-float)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) tmp)
  (:temporary (:sc single-reg) floats)
  (:generator 10
    ;; Load vector into TMP
    (load-row tmp vector)
    ;; Fill XMM reg with the float.
    (inst movss floats float)
    (inst unpcklps floats floats)
    (inst unpcklps floats floats)
    ;; Multiply
    (inst mulps tmp floats)
    ;; Save result to result vector
    (store-row tmp result-vector)
    (move result result-vector)))

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

(defknown %vec/ (vec vec single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%vec/)
  (:translate %vec/)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (vector :scs (descriptor-reg))
         (float :scs (single-reg)))
  (:arg-types * * single-float)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) tmp)
  (:temporary (:sc single-reg) floats)
  (:generator 10
    ;; Load vector into TMP
    (load-row tmp vector)
    ;; Fill XMM reg with the float.
    (inst movss floats float)
    (inst unpcklps floats floats)
    (inst unpcklps floats floats)
    ;; Divide
    (inst divps tmp floats)
    ;; Save result to source vector
    (store-row tmp result-vector)
    (move result result-vector)))

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

;;;; DOT PRODUCT

(defknown %dot-product (vec vec) single-float
    (any #+sb-cga-sse2 always-translatable))

#+sb-cga-sse2
(define-vop (%dot-product)
  (:translate %dot-product)
  (:policy :fast-safe)
  (:args (vector1 :scs (descriptor-reg))
         (vector2 :scs (descriptor-reg)))
  (:arg-types * *)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:sc single-reg) tmp)
  (:temporary (:sc single-reg) tmp2)
  (:generator 10
    ;; Load vector into TMP
    (load-row tmp vector1)
    ;; Multiply elementwise
    (inst mulps tmp (ea-for-row vector2))
    ;; Get low half into high half of a copy
    (inst movlhps tmp2 tmp)
    ;; First two additions -- result in high half of tmp2
    (inst addps tmp2 tmp)
    ;; Low half of the result into first word of tmp2,
    ;; and high half into third word of tmp2
    (inst unpckhps tmp2 tmp2)
    ;; High half of result into first word of tmp
    (inst movaps tmp tmp2)
    (inst unpckhps tmp tmp)
    ;; Final addition
    (inst addss tmp tmp2)
    (inst movss result tmp)))

#-sb-cga-sse2
(progn
  (declaim (inline %dot-product))
  (defun %dot-product (a b)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (macrolet ((dim (n)
                 `(* (aref a ,n) (aref b ,n))))
      (+ (dim 0) (dim 1) (dim 2) (dim 3)))))

;;;; HADAMARD PRODUCT

(defknown %hadamard-product (vec vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%hadamard-product)
  (:translate %hadamard-product)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (vector1 :scs (descriptor-reg))
         (vector2 :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) tmp)
  (:generator 10
    ;; Load vector into TMP
    (load-row tmp vector1)
    ;; Multiply elementwise
    (inst mulps tmp (ea-for-row vector2))
    ;; Result
    (store-row tmp result-vector)
    (move result result-vector)))

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

;;; VECTOR LENGTH

(defknown %vec-length (vec) single-float
    (any #+sb-cga-sse2 always-translatable))

#+sb-cga-sse2
(define-vop (%vec-length)
  (:translate %vec-length)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg)))
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:sc single-reg) tmp)
  (:temporary (:sc single-reg) tmp2)
  (:generator 10
    ;; Load vector into TMP
    (load-row tmp vector)
    ;; Multiply elementwise
    (inst mulps tmp tmp)
    ;; Get low half into high half of a copy
    (inst movlhps tmp2 tmp)
    ;; First addition -- result in high half of tmp2
    (inst addps tmp2 tmp)
    ;; Low half of the result into first word of tmp2,
    ;; and high half into third word of tmp2
    (inst unpckhps tmp2 tmp2)
    ;; High half of result into first word of tmp
    (inst movaps tmp tmp2)
    (inst unpckhps tmp tmp)
    ;; Final addition
    (inst addss tmp tmp2)
    ;; Square root into result
    (inst sqrtss result tmp)))

#-sb-cga-sse2
(progn
  (declaim (inline %vec-length))
  (defun %vec-length (a)
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    (macrolet ((dim (n)
                 `(let ((d (aref a ,n)))
                     (* d d))))
      (sqrt (+ (dim 0) (dim 1) (dim 2) (dim 3))))))

;;;; NORMALIZATION

(defknown %normalize (vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%normalize)
  (:translate %normalize)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (vector :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) tmp)
  (:temporary (:sc single-reg) tmp2)
  (:temporary (:sc single-reg) tmp3)
  (:generator 10
    ;; Load vector into TMP and TMP2
    (load-row tmp vector)
    (inst movaps tmp3 tmp)
    ;; Multiply elementwise
    (inst mulps tmp tmp)
    ;; Get low half into high half of a copy
    (inst movlhps tmp2 tmp)
    ;; First addition -- result in high half of tmp2
    (inst addps tmp2 tmp)
    ;; Low half of the result into first word of tmp2,
    ;; and high half into third word of tmp2
    (inst unpckhps tmp2 tmp2)
    ;; High half of result into first word of tmp
    (inst movaps tmp tmp2)
    (inst unpckhps tmp tmp)
    ;; Final addition
    (inst addss tmp tmp2)
    ;; Square root
    (inst sqrtss tmp tmp)
    ;; Fill into tmp
    (inst unpcklps tmp tmp)
    (inst unpcklps tmp tmp)
    ;; Divide original
    (inst divps tmp3 tmp)
    ;; Store result
    (store-row tmp3 result-vector)
    (move result result-vector)))

#-sse2
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

(defknown %vec-lerp (vec vec vec single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%vec-lerp)
  (:translate %vec-lerp)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (vector1 :scs (descriptor-reg))
         (vector2 :scs (descriptor-reg))
         (float :scs (single-reg)))
  (:arg-types * * * single-float)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) tmp)
  (:temporary (:sc single-reg) tmp2)
  (:temporary (:sc single-reg) floats)
  (:temporary (:sc single-reg) 1-floats)
  (:generator 10
    ;; Load vectors
    (load-row tmp vector1)
    (load-row tmp2 vector2)
    ;; Fill XMM reg with the float.
    (inst movss floats float)
    (inst unpcklps floats floats)
    (inst unpcklps floats floats)
    ;; Same for the the 1- version
    (inst movss 1-floats (register-inline-constant 1.0))
    (inst subss 1-floats float)
    (inst unpcklps 1-floats 1-floats)
    (inst unpcklps 1-floats 1-floats)
    ;; Multiply VECTOR1 by 1-FLOATS, and VECTOR2 by FLOATS
    (inst mulps tmp 1-floats)
    (inst mulps tmp2 floats)
    ;; Add
    (inst addps tmp tmp2)
    ;; Save result and return
    (store-row tmp result-vector)
    (move result result-vector)))

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
