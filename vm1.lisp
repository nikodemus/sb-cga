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
  (defmacro ea-for-slice (vector &optional (index 0))
    `(sb-vm::make-ea :dword :base ,vector
                     :disp (- (+ (* sb-vm:vector-data-offset sb-vm:n-word-bytes)
                                 (* ,index (/ 4 (/ sb-vm:n-word-bytes 4)) sb-vm:n-word-bytes))
                              sb-vm:other-pointer-lowtag)))
  (defmacro load-slice (xmm vector &optional (index 0))
    `(inst movaps ,xmm (ea-for-slice ,vector ,index)))
  (defmacro store-slice (xmm vector &optional (index 0))
    `(inst movaps (ea-for-slice ,vector ,index) ,xmm)))

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
      (load-slice tmp vector1)
      ;; Compare
      (inst cmpps :neq tmp (ea-for-slice vector2))
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
    (load-slice tmp vector)
    ;; Save copy to source vector
    (store-slice tmp result-vector)
    (move result result-vector)))

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
    (load-slice tmp vector1)
    ;; Add
    (inst addps tmp (ea-for-slice vector2))
    ;; Save result to source vector
    (store-slice tmp result-vector)
    (move result result-vector)))

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
    (load-slice tmp vector1)
    ;; Sub
    (inst subps tmp (ea-for-slice vector2))
    ;; Save result to source vector
    (store-slice tmp result-vector)
    (move result result-vector)))

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
    (load-slice tmp vector)
    ;; Fill XMM reg with the float.
    (inst movss floats float)
    (inst unpcklps floats floats)
    (inst unpcklps floats floats)
    ;; Multiply
    (inst mulps tmp floats)
    ;; Save result to result vector
    (store-slice tmp result-vector)
    (move result result-vector)))

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
    (load-slice tmp vector)
    ;; Fill XMM reg with the float.
    (inst movss floats float)
    (inst unpcklps floats floats)
    (inst unpcklps floats floats)
    ;; Divide
    (inst divps tmp floats)
    ;; Save result to source vector
    (store-slice tmp result-vector)
    (move result result-vector)))

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
    (load-slice tmp vector1)
    ;; Multiply elementwise
    (inst mulps tmp (ea-for-slice vector2))
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
    (declare (optimize (speed 3) (safety 0) (debug 1)))
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
    (load-slice tmp vector1)
    ;; Multiply elementwise
    (inst mulps tmp (ea-for-slice vector2))
    ;; Result
    (store-slice tmp result-vector)
    (move result result-vector)))

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
    (load-slice tmp vector)
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
    (declare (optimize (speed 3) (safety 0) (debug 1)))
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
    (load-slice tmp vector)
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
    (store-slice tmp3 result-vector)
    (move result result-vector)))

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
    (load-slice tmp vector1)
    (load-slice tmp2 vector2)
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
    (store-slice tmp result-vector)
    (move result result-vector)))

;;;; TRANSFORMING A VECTOR

(defknown %transform-vec (vec vec matrix) vec
    (any #+sb-cga-sse2 always-translatable))

(define-vop (%transform-vec)
  (:translate %transform-vec)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (vector :scs (descriptor-reg))
         (matrix :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) vec)
  (:temporary (:sc single-reg) tmp)
  (:temporary (:sc single-reg) col1)
  (:temporary (:sc single-reg) col2)
  (:temporary (:sc single-reg) col3)
  (:temporary (:sc single-reg) col4)
  (:note "oops")
  (:generator 10
    ;; Load stuff
    (load-slice vec vector)
    (load-slice col1 matrix 0)
    (load-slice col2 matrix 1)
    (load-slice col3 matrix 2)
    (load-slice col4 matrix 3)
    ;; Distribute vec[0] and multiply
    (inst movaps tmp vec)
    (inst unpckhps tmp tmp)
    (inst unpckhps tmp tmp)
    (inst mulps col4 tmp)
    ;; Distribute vec[1] and multiply
    (inst movaps tmp vec)
    (inst unpckhps tmp tmp)
    (inst unpcklps tmp tmp)
    (inst mulps col3 tmp)
    ;; Distribute vec[2] and multiply
    (inst movaps tmp vec)
    (inst unpcklps tmp tmp)
    (inst unpckhps tmp tmp)
    (inst mulps col2 tmp)
    ;; Distribute vec[3] and multiply
    (inst movaps tmp vec)
    (inst unpcklps tmp tmp)
    (inst unpcklps tmp tmp)
    (inst mulps col1 tmp)
    ;; Add rows
    (inst addps col1 col2)
    (inst addps col3 col4)
    (inst addps col1 col3)
    ;; Store result
    (store-slice col1 result-vector)
    (move result result-vector)))