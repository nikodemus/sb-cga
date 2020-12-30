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

#-sbcl
(error "This file is SBCL only!")

(eval-when (:compile-toplevel :load-toplevel)
  (sb-int:set-floating-point-modes :traps nil))

;;;; Misc bits.

(macrolet ((def (name cname ctype ltype one)
             `(progn
                (declaim (inline ,name))
                ,(if (sb-sys:find-foreign-symbol-address cname)
                     `(sb-alien:define-alien-routine (,cname ,name)
                        ,ctype (,ctype ,ctype))
                     ;; In absence of nice cbrt implementation (Windows...)
                     ;; let's use the stupid one. Note: this fails
                     ;; cubic-roots-above.1 test by finding more roots than
                     ;; expected. Haven't checked if this is because this
                     ;; implementation is more or less precise and libc cbrt.
                     ;;
                     ;; FIXME: Could just implement Kahan's cbrt in Lisp.
                     `(defun ,name (n)
                        (declare (,ltype n))
                        (if (minusp n)
                            (- (expt (- n) ,(/ one 3)))
                            (expt n ,(/ one 3))))))))
  (def cbrt/single "cbrtf" sb-alien:float single-float 1f0)
  (def cbrt/double "cbrt" sb-alien:double double-float 1d0))

(declaim (inline cbrt))
(defun cbrt (float)
  "Cube root of FLOAT."
  (etypecase float
    (single-float
     (cbrt/single float))
    (double-float
     (cbrt/double float))))

(declaim (inline single-float-quiet-nan))
(defun single-float-quiet-nan ()
  (sb-kernel:make-single-float #x-400000))

(declaim (inline double-float-quiet-nan))
(defun double-float-quiet-nan ()
  (sb-kernel:make-double-float #x-80000 0))

(declaim (inline float-nan-p))
(defun float-nan-p (x)
  (sb-ext:float-nan-p x))

;;;; :SB-CGA-SSE2 tells vm.lisp if it should use the Lisp versions of various
;;;; functions.
#+x86-64
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :sb-cga-sse2 *features*))

;;;; Aliases for this file
(eval-when (:compile-toplevel)
  (defmacro inst (instruction &rest args)
    `(sb-assem:inst ,instruction ,@args))
  (defmacro move (dst src)
    `(sb-c:move ,dst ,src))
  (defmacro define-vop (name/inherits &body body)
    `(sb-c:define-vop ,name/inherits
       ,@(subst 'sb-vm::descriptor-reg 'descriptor-reg
                (subst 'sb-vm::single-reg 'single-reg body)))))

;;;; Utilities for writing VOPs

#+sb-cga-sse2
(progn
  (defmacro ea-for-data (vector index)
    `(sb-vm::ea (- (+ (* sb-vm:vector-data-offset sb-vm:n-word-bytes)
                      ;; 4 bytes per single-float
                      (* ,index 4))
                   sb-vm:other-pointer-lowtag)
                ,vector))
  (defmacro ea-for-slice (vector &optional (index 0))
    `(sb-vm::ea (- (+ (* sb-vm:vector-data-offset sb-vm:n-word-bytes)
                      ;; 4 bytes per single-float, 16 per slice.
                      (* ,index 16))
                   sb-vm:other-pointer-lowtag)
                ,vector))
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

;;;; HADAMARD PRODUCT

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

#+sb-cga-sse2
(define-vop (%%normalized-vec)
  (:translate %%normalized-vec)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg) :target result)
         (x :scs (single-reg))
         (y :scs (single-reg))
         (z :scs (single-reg)))
  (:arg-types t single-float single-float single-float)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc single-reg) a)
  (:temporary (:sc single-reg) b)
  (:temporary (:sc single-reg) c)
  (:generator 10
    (inst movss a x)
    (inst movss b y)
    (inst movss c z)
    ;; Compute the length into X
    (inst mulss x x)
    (inst mulss y y)
    (inst mulss z z)
    (inst addss x y)
    (inst addss x z)
    (inst rsqrtss x x)
    ;; Divide original -- is this faster then loading again and
    ;; using MULPS?
    (inst mulss a x)
    (inst mulss b x)
    (inst mulss c x)
    ;; Store result
    (inst movss (ea-for-data vector 0) a)
    (inst movss (ea-for-data vector 1) b)
    (inst movss (ea-for-data vector 2) c)
    (move result vector)))

;;;; LINEAR INTERPOLATION

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
    (fill-xmm 1-floats (sb-c:register-inline-constant 1.0))
    (inst subps 1-floats floats)
    ;; Multiply VECTOR1 by 1-FLOATS, and VECTOR2 by FLOATS
    (inst mulps 1-floats (ea-for-slice vector1))
    (inst mulps floats (ea-for-slice vector2))
    ;; Add
    (inst addps floats 1-floats)
    ;; Save result and return
    (store-slice floats result-vector)
    (move result result-vector)))

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
                (fill-xmm 1-floats (sb-c:register-inline-constant 1.0))
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
