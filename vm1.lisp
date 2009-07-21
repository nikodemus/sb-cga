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

;;;; VOPs for operations on VECs
;;;;
;;;; %FOO always has the result as the first argument, but. the result
;;;; argument itself is not an operand.
;;;;
;;;; %FOO/1 is the same operation as %FOO, but with one less argument: the
;;;; _first_ argument both receives the result, and is use as an operand.
;;;;
;;;; %FOO/2 is the same operation as %FOO, but with one less argument: the
;;;; _second_ argument both receives the result, and is use as an operand.
;;;;
;;;; Not everything has a %FOO/1 or %FOO/2 version.

;;;; KLUDGE: SBCL doesn't currently have SSE2 feature, but it's cleaner to
;;;; conditionalize on a single feature (relevant once x86 gets SSE2
;;;; instructions.)
#+x86-64
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :sb-cga-sse2 *features*))

#+sb-cga-sse2
(progn
  (defmacro ea-for-data (vector index)
    `(sb-vm::make-ea :dword :base ,vector
                     :disp (- (+ (* sb-vm:vector-data-offset sb-vm:n-word-bytes)
                                 ;; 4 bytes per single-float
                                 (* ,index 4))
                              sb-vm:other-pointer-lowtag)))
  (defmacro ea-for-slice (vector &optional (index 0))
    `(sb-vm::make-ea :dword :base ,vector
                     :disp (- (+ (* sb-vm:vector-data-offset sb-vm:n-word-bytes)
                                 ;; 4 bytes per single-float, 16 per slice.
                                 (* ,index 16))
                              sb-vm:other-pointer-lowtag)))
  (defmacro load-slice (xmm vector &optional (index 0))
    `(inst movaps ,xmm (ea-for-slice ,vector ,index)))
  (defmacro store-slice (xmm vector &optional (index 0))
    `(inst movaps (ea-for-slice ,vector ,index) ,xmm))
  (defmacro fill-xmm (target source &optional (index 0))
    `(progn
       ,(if (consp source)
            `(inst movss ,target ,source)
            `(unless (sb-c:location= ,target ,source)
               (inst movaps ,target ,source)))
       ,@(ecase index
                (0 `((inst unpcklps ,target ,target)
                     (inst unpcklps ,target ,target)))
                (1 `((inst unpcklps ,target ,target)
                     (inst unpckhps ,target ,target)))
                (2 `((inst unpckhps ,target ,target)
                     (inst unpcklps ,target ,target)))
                (3 `((inst unpckhps ,target ,target)
                     (inst unpckhps ,target ,target)))))))

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
    ;; Grab sign bits, mask 4th elt & check for zero.
    (inst movmskps mask tmp)
    (inst and mask #b0111)
    (inst test mask mask)))

;;;; VECTOR COPYING

(defknown %copy-vec (vec vec) vec (any)
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

(defknown %vec+ (vec vec vec) vec (any)
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

(defknown %%vec+/1 (vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %%vec+/2 (vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 1)

#+sb-cga-sse2
(macrolet ((def (name target1 target2 result)
             `(define-vop (,name)
                (:translate ,name)
                (:policy :fast-safe)
                (:args (vector1 :scs (descriptor-reg) ,@target1)
                       (vector2 :scs (descriptor-reg) ,@target2))
                (:results (result :scs (descriptor-reg)))
                (:temporary (:sc single-reg) tmp)
                (:generator 10
                  ;; Load vector into TMP
                  (load-slice tmp vector1)
                  ;; Add
                  (inst addps tmp (ea-for-slice vector2))
                  (store-slice tmp ,result)
                  (move result ,result)))))
  (def %%vec+/1 (:target result) () vector1)
  (def %%vec+/2 () (:target result) vector2))

;;;; VECTOR SUBSTRACTION

(defknown %vec- (vec vec vec) vec (any)
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

(defknown %%vec-/1 (vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %%vec-/2 (vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 1)

#+sb-cga-sse2
(macrolet ((def (name target1 target2 result)
             `(define-vop (,name)
                (:translate ,name)
                (:policy :fast-safe)
                (:args (vector1 :scs (descriptor-reg) ,@target1)
                       (vector2 :scs (descriptor-reg) ,@target2))
                (:results (result :scs (descriptor-reg)))
                (:temporary (:sc single-reg) tmp)
                (:generator 10
                  ;; Load vector into TMP
                  (load-slice tmp vector1)
                  ;; Sub
                  (inst subps tmp (ea-for-slice vector2))
                  (store-slice tmp ,result)
                  (move result ,result)))))
  (def %%vec-/1 (:target result) () vector1)
  (def %%vec-/2 () (:target result) vector2))

;;;; VECTOR/SCALAR MULTIPLICATION

(defknown %vec* (vec vec single-float) vec (any)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%vec*)
  (:translate %vec*)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (vector :scs (descriptor-reg))
         (f :scs (single-reg) :target tmp))
  (:arg-types * * single-float)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) tmp)
  (:generator 10
    (fill-xmm tmp f)
    ;; Multiply
    (inst mulps tmp (ea-for-slice vector))
    ;; Save result to result vector
    (store-slice tmp result-vector)
    (move result result-vector)))

(defknown %%vec*/1 (vec single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%%vec*/1)
  (:translate %%vec*/1)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg) :target result)
         (f :scs (single-reg) :target result))
  (:arg-types * single-float)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) tmp)
  (:generator 10
    (fill-xmm tmp f)
    ;; Multiply
    (inst mulps tmp (ea-for-slice vector))
    (store-slice tmp vector)
    (move result vector)))

;;;; VECTOR/SCALAR DIVISION

(defknown %vec/ (vec vec single-float) vec (any)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%vec/)
  (:translate %vec/)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (vector :scs (descriptor-reg))
         (f :scs (single-reg) :target floats))
  (:arg-types * * single-float)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) tmp)
  (:temporary (:sc single-reg) floats)
  (:generator 10
    ;; Load vector into TMP
    (load-slice tmp vector)
    (fill-xmm floats f)
    ;; Divide
    (inst divps tmp floats)
    ;; Save result to source vector
    (store-slice tmp result-vector)
    (move result result-vector)))

(defknown %%vec//1 (vec single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%%vec//1)
  (:translate %%vec//1)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg) :target result)
         (f :scs (single-reg) :target floats))
  (:arg-types * single-float)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) tmp)
  (:temporary (:sc single-reg) floats)
  (:generator 10
    (fill-xmm floats f)
    ;; Divide
    (load-slice tmp vector)
    (inst divps tmp floats)
    (store-slice tmp vector)
    (move result vector)))

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
  (:temporary (:sc single-reg) tmp2)
  (:temporary (:sc single-reg) tmp3)
  (:generator 10
    ;; Load elements of VECTOR1
    (inst movss result (ea-for-data vector1 0))
    (inst movss tmp2 (ea-for-data vector1 1))
    (inst movss tmp3 (ea-for-data vector1 2))
    ;; Multiply by elements of VECTOR2
    (inst mulss result (ea-for-data vector2 0))
    (inst mulss tmp2 (ea-for-data vector2 1))
    (inst mulss tmp3 (ea-for-data vector2 2))
    ;; Add
    (inst addss result tmp2)
    (inst addss result tmp3)))

;;;; CROSS PRODUCT
;;;;
;;;; No VOP, just the defknown.

(defknown %cross-product (vec vec vec) vec
    (any)
  :result-arg 0)

;;;; HADAMARD PRODUCT

(defknown %hadamard-product (vec vec vec) vec (any)
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

(defknown %%hadamard-product/1 (vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %%hadamard-product/2 (vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 1)

#+sb-cga-sse2
(macrolet ((def (name target1 target2 result)
             `(define-vop (,name)
                (:translate ,name)
                (:policy :fast-safe)
                (:args (vector1 :scs (descriptor-reg) ,@target1)
                       (vector2 :scs (descriptor-reg) ,@target2))
                (:results (result :scs (descriptor-reg)))
                (:temporary (:sc single-reg) tmp)
                (:generator 10
                  ;; Load vector into TMP
                  (load-slice tmp vector2)
                  ;; Multiply elementwise
                  (inst mulps tmp (ea-for-slice vector1))
                  (store-slice tmp ,result)
                  (move result ,result)))))
  (def %%hadamard-product/1 (:target result) () vector1)
  (def %%hadamard-product/2 () (:target result) vector2))

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
  (:temporary (:sc single-reg) tmp1)
  (:temporary (:sc single-reg) tmp2)
  (:generator 10
    ;; Again, vector ops not so hot -- so just hand-optimize.
    (inst movss result (ea-for-data vector 0))
    (inst movss tmp1 (ea-for-data vector 1))
    (inst movss tmp2 (ea-for-data vector 2))
    (inst mulss result result)
    (inst mulss tmp1 tmp1)
    (inst mulss tmp2 tmp2)
    (inst addss result tmp1)
    (inst addss result tmp2)
    (inst sqrtss result result)))

;;;; NORMALIZATION

(defknown %normalize (vec vec) vec (any)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%normalize)
  (:translate %normalize)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (vector :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) tmp1)
  (:temporary (:sc single-reg) tmp2)
  (:temporary (:sc single-reg) tmp3)
  (:temporary (:sc single-reg) tmp4)
  (:temporary (:sc single-reg) tmp5)
  (:temporary (:sc single-reg) tmp6)
  (:generator 10
    (inst movss tmp1 (ea-for-data vector 0))
    (inst movss tmp2 (ea-for-data vector 1))
    (inst movss tmp3 (ea-for-data vector 2))
    (inst movss tmp4 tmp1)
    (inst movss tmp5 tmp2)
    (inst movss tmp6 tmp3)
    ;; Compute 1/length into tmp1
    (inst mulss tmp1 tmp1)
    (inst mulss tmp2 tmp2)
    (inst mulss tmp3 tmp3)
    (inst addss tmp1 tmp2)
    (inst addss tmp1 tmp3)
    (inst rsqrtss tmp1 tmp1)
    ;; Multiply original -- is this faster then loading again and
    ;; using MULPS?
    (inst mulss tmp4 tmp1)
    (inst mulss tmp5 tmp1)
    (inst mulss tmp6 tmp1)
    ;; Store result
    (inst movss (ea-for-data result-vector 0) tmp4)
    (inst movss (ea-for-data result-vector 1) tmp5)
    (inst movss (ea-for-data result-vector 2) tmp6)
    (move result result-vector)))

(defknown %%normalize/1 (vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%%normalize/1)
  (:translate %%normalize/1)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg) :target result))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) tmp1)
  (:temporary (:sc single-reg) tmp2)
  (:temporary (:sc single-reg) tmp3)
  (:temporary (:sc single-reg) tmp4)
  (:temporary (:sc single-reg) tmp5)
  (:temporary (:sc single-reg) tmp6)
  (:generator 10
    (inst movss tmp1 (ea-for-data vector 0))
    (inst movss tmp2 (ea-for-data vector 1))
    (inst movss tmp3 (ea-for-data vector 2))
    (inst movss tmp4 tmp1)
    (inst movss tmp5 tmp2)
    (inst movss tmp6 tmp3)
    ;; Compute the length into tmp1
    (inst mulss tmp1 tmp1)
    (inst mulss tmp2 tmp2)
    (inst mulss tmp3 tmp3)
    (inst addss tmp1 tmp2)
    (inst addss tmp1 tmp3)
    (inst rsqrtss tmp1 tmp1)
    ;; Divide original -- is this faster then loading again and
    ;; using MULPS?
    (inst mulss tmp4 tmp1)
    (inst mulss tmp5 tmp1)
    (inst mulss tmp6 tmp1)
    ;; Store result
    (inst movss (ea-for-data vector 0) tmp4)
    (inst movss (ea-for-data vector 1) tmp5)
    (inst movss (ea-for-data vector 2) tmp6)
    (move result vector)))

;;;; LINEAR INTERPOLATION

(defknown %vec-lerp (vec vec vec single-float) vec (any)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%vec-lerp)
  (:translate %vec-lerp)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (vector1 :scs (descriptor-reg))
         (vector2 :scs (descriptor-reg))
         (f :scs (single-reg) :target floats))
  (:arg-types * * * single-float)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) floats)
  (:temporary (:sc single-reg) 1-floats)
  (:generator 10
    (fill-xmm floats f)
    (fill-xmm 1-floats (register-inline-constant 1.0))
    (inst subps 1-floats floats)
    ;; Multiply VECTOR1 by 1-FLOATS, and VECTOR2 by FLOATS
    (inst mulps 1-floats (ea-for-slice vector1))
    (inst mulps floats (ea-for-slice vector2))
    ;; Add
    (inst addps floats 1-floats)
    ;; Save result and return
    (store-slice floats result-vector)
    (move result result-vector)))

(defknown %%vec-lerp/1 (vec vec single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %%vec-lerp/2 (vec vec single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 1)

#+sb-cga-sse2
(macrolet ((def (name target1 target2 result)
             `(define-vop (,name)
              (:translate ,name)
              (:policy :fast-safe)
              (:args (vector1 :scs (descriptor-reg) ,@target1)
                     (vector2 :scs (descriptor-reg) ,@target2)
                     (f :scs (single-reg) :target floats))
              (:arg-types * * single-float)
              (:results (result :scs (descriptor-reg)))
              (:temporary (:sc single-reg) floats)
              (:temporary (:sc single-reg) 1-floats)
              (:generator 10
                (fill-xmm floats f)
                (fill-xmm 1-floats (register-inline-constant 1.0))
                (inst subps 1-floats floats)
                ;; Multiply VECTOR1 by 1-FLOATS, and VECTOR2 by FLOATS
                (inst mulps 1-floats (ea-for-slice vector1))
                (inst mulps floats (ea-for-slice vector2))
                ;; Add
                (inst addps floats 1-floats)
                ;; Save result and return
                (store-slice floats ,result)
                (move result ,result)))))
  (def %%vec-lerp/1 (:target result) () vector1)
  (def %%vec-lerp/2 () (:target result) vector2))

;;;; TRANSFORMING A POINT

(defknown %transform-point (vec vec matrix) vec (any)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%transform-point)
  (:translate %transform-point)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (vector :scs (descriptor-reg))
         (matrix :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) vec)
  (:temporary (:sc single-reg) col1)
  (:temporary (:sc single-reg) col2)
  (:temporary (:sc single-reg) col3)
  (:generator 10
    (load-slice vec vector)
    ;; Distribute vec[2] and multiply
    (inst movaps col3 vec)
    (inst unpckhps col3 col3)
    (inst unpcklps col3 col3)
    (inst mulps col3 (ea-for-slice matrix 2))
    ;; Distribute vec[1] and multiply
    (inst movaps col2 vec)
    (inst unpcklps col2 col2)
    (inst unpckhps col2 col2)
    (inst mulps col2 (ea-for-slice matrix 1))
    ;; Distribute vec[0] and multiply
    (inst movaps col1 vec)
    (inst unpcklps col1 col1)
    (inst unpcklps col1 col1)
    (inst mulps col1 (ea-for-slice matrix 0))
    ;; Add rows -- including the translation column.
    (inst addps col1 (ea-for-slice matrix 3))
    (inst addps col3 col2)
    (inst addps col1 col3)
    ;; Store result
    (store-slice col1 result-vector)
    (move result result-vector)))

(defknown %%transform-point/1 (vec matrix) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%%transform-point/1)
  (:translate %%transform-point/1)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg) :target result)
         (matrix :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) vec)
  (:temporary (:sc single-reg) col1)
  (:temporary (:sc single-reg) col2)
  (:temporary (:sc single-reg) col3)
  (:generator 10
    (load-slice vec vector)
    ;; Distribute vec[2] and multiply
    (inst movaps col3 vec)
    (inst unpckhps col3 col3)
    (inst unpcklps col3 col3)
    (inst mulps col3 (ea-for-slice matrix 2))
    ;; Distribute vec[1] and multiply
    (inst movaps col2 vec)
    (inst unpcklps col2 col2)
    (inst unpckhps col2 col2)
    (inst mulps col2 (ea-for-slice matrix 1))
    ;; Distribute vec[0] and multiply
    (inst movaps col1 vec)
    (inst unpcklps col1 col1)
    (inst unpcklps col1 col1)
    (inst mulps col1 (ea-for-slice matrix 0))
    ;; Add rows -- including the translation column.
    (inst addps col1 (ea-for-slice matrix 3))
    (inst addps col3 col2)
    (inst addps col1 col3)
    ;; Store result
    (store-slice col1 vector)
    (move result vector)))

;;;; TRANSFORMING A DIRECTION

(defknown %transform-direction (vec vec matrix) vec (any)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%transform-direction)
  (:translate %transform-direction)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (vector :scs (descriptor-reg))
         (matrix :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) vec)
  (:temporary (:sc single-reg) col1)
  (:temporary (:sc single-reg) col2)
  (:temporary (:sc single-reg) col3)
  (:generator 10
    ;; Load stuff
    (load-slice vec vector)
    ;; Distribute vec[2] and multiply
    (inst movaps col3 vec)
    (inst unpckhps col3 col3)
    (inst unpcklps col3 col3)
    (inst mulps col3 (ea-for-slice matrix 2))
    ;; Distribute vec[1] and multiply
    (inst movaps col2 vec)
    (inst unpcklps col2 col2)
    (inst unpckhps col2 col2)
    (inst mulps col2 (ea-for-slice matrix 1))
    ;; Distribute vec[0] and multiply
    (inst movaps col1 vec)
    (inst unpcklps col1 col1)
    (inst unpcklps col1 col1)
    (inst mulps col1 (ea-for-slice matrix 0))
    ;; Add rows -- except for translation.
    (inst addps col1 col2)
    (inst addps col1 col3)
    ;; Store result
    (store-slice col1 result-vector)
    (move result result-vector)))

(defknown %%transform-direction/1 (vec matrix) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%%transform-direction/1)
  (:translate %%transform-direction/1)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg) :target result)
         (matrix :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) vec)
  (:temporary (:sc single-reg) col1)
  (:temporary (:sc single-reg) col2)
  (:temporary (:sc single-reg) col3)
  (:generator 10
    ;; Load stuff
    (load-slice vec vector)
    ;; Distribute vec[2] and multiply
    (inst movaps col3 vec)
    (inst unpckhps col3 col3)
    (inst unpcklps col3 col3)
    (inst mulps col3 (ea-for-slice matrix 2))
    ;; Distribute vec[1] and multiply
    (inst movaps col2 vec)
    (inst unpcklps col2 col2)
    (inst unpckhps col2 col2)
    (inst mulps col2 (ea-for-slice matrix 1))
    ;; Distribute vec[0] and multiply
    (inst movaps col1 vec)
    (inst unpcklps col1 col1)
    (inst unpcklps col1 col1)
    (inst mulps col1 (ea-for-slice matrix 0))
    ;; Add rows -- except for translation.
    (inst addps col1 col2)
    (inst addps col1 col3)
    ;; Store result
    (store-slice col1 vector)
    (move result vector)))

;;;; ADJUSTING A VECTOR

(defknown %adjust-vec (vec vec vec single-float) vec (any)
  :result-arg 0)

#+sb-cga-sse2
(define-vop (%adjust-vec)
  (:translate %adjust-vec)
  (:policy :fast-safe)
  (:args (result-vector :scs (descriptor-reg) :target result)
         (point :scs (descriptor-reg))
         (direction :scs (descriptor-reg))
         (distance :scs (single-reg) :target tmp))
  (:arg-types * * * single-float)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) tmp)
  (:generator 10
    (fill-xmm tmp distance)
    ;; Multiply by DIRECTION
    (inst mulps tmp (ea-for-slice direction))
    ;; Add POINT
    (inst addps tmp (ea-for-slice point))
    ;; Result
    (store-slice tmp result-vector)
    (move result result-vector)))

(defknown %%adjust-vec/1 (vec vec single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %%adjust-vec/2 (vec vec single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 1)

#+sb-cga-sse2
(macrolet ((def (name target1 target2 result)
             `(define-vop (,name)
                (:translate ,name)
                (:policy :fast-safe)
                (:args (point :scs (descriptor-reg) ,@target1)
                       (direction :scs (descriptor-reg) ,@target2)
                       (distance :scs (single-reg) :target tmp))
                (:arg-types * * single-float)
                (:results (result :scs (descriptor-reg)))
                (:temporary (:sc single-reg) tmp)
                (:generator 10
                  (fill-xmm tmp distance)
                  ;; Multiply by DIRECTION
                  (inst mulps tmp (ea-for-slice direction))
                  ;; Add POINT
                  (inst addps tmp (ea-for-slice point))
                  (store-slice tmp ,result)
                  (move result ,result)))))
  (def %%adjust-vec/1 (:target result) () point)
  (def %%adjust-vec/2 () (:target result) direction))

