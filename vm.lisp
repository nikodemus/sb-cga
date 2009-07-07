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
