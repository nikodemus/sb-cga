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

;;;; KLUDGE: DEFKNOWN macro for use in this file -- reader conditionals
;;;; allowed here. Easier then adding multiple port-files, I think.
;;;;
;;;; Non-SBCL/CMUCL implementations should expand into an FTYPE proclamation
;;;; unless the implementation has something closer to CMU-style defknown.

(eval-when (:compile-toplevel)
  #+sbcl
  (defmacro defknown (name arg-types ret-type attributes &rest keys)
    `(sb-c:defknown ,name ,arg-types ,ret-type
         ,(mapcar (lambda (attr)
                    (intern (symbol-name attr) :sb-c))
                  attributes)
       ,@keys)))

;;;; VOPs for operations on VECs
;;;;
;;;; %FOO always has the result as the first argument, but the result
;;;; argument itself is not an operand.
;;;;
;;;; %FOO/1 is the same operation as %FOO, but with one less argument: the
;;;; _first_ argument both receives the result, and is used as an operand.
;;;;
;;;; %FOO/2 is the same operation as %FOO, but with one less argument: the
;;;; _second_ argument both receives the result, and is used as an operand.
;;;;
;;;; Not everything has a %FOO/1 or %FOO/2 version.
;;;;
;;;;

(defknown %vec= (vec vec) boolean
    (any #+sb-cga-sse2 always-translatable))

(defknown %copy-vec (vec vec) vec
    (any)
  :result-arg 0)

(defknown %vec+ (vec vec vec) vec
    (any)
  :result-arg 0)

(defknown %%vec+/1 (vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %%vec+/2 (vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 1)

(defknown %vec- (vec vec vec) vec
    (any)
  :result-arg 0)

(defknown %%vec-/1 (vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %%vec-/2 (vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 1)

(defknown %vec* (vec vec single-float) vec
    (any)
  :result-arg 0)

(defknown %%vec*/1 (vec single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %vec/ (vec vec single-float) vec
    (any)
  :result-arg 0)

(defknown %%vec//1 (vec single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %dot-product (vec vec) single-float
    (any #+sb-cga-sse2 always-translatable))

(defknown %cross-product (vec vec vec) vec
    (any)
  :result-arg 0)

(defknown %normalized-cross-product (vec vec vec) vec
    (any)
  :result-arg 0)

(defknown %hadamard-product (vec vec vec) vec (any)
  :result-arg 0)

(defknown %%hadamard-product/1 (vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %%hadamard-product/2 (vec vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 1)

(defknown %vec-length (vec) single-float
    (any #+sb-cga-sse2 always-translatable))

(defknown %normalize (vec vec) vec (any)
  :result-arg 0)

(defknown %%normalize/1 (vec) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %%normalized-vec (vec single-float single-float single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %vec-lerp (vec vec vec single-float) vec
    (any)
  :result-arg 0)

(defknown %%vec-lerp/1 (vec vec single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %%vec-lerp/2 (vec vec single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 1)

(defknown %transform-point (vec vec matrix) vec
    (any)
  :result-arg 0)

(defknown %%transform-point/1 (vec matrix) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %transform-direction (vec vec matrix) vec
    (any)
  :result-arg 0)

(defknown %%transform-direction/1 (vec matrix) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %adjust-vec (vec vec vec single-float) vec
    (any)
  :result-arg 0)

(defknown %%adjust-vec/1 (vec vec single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 0)

(defknown %%adjust-vec/2 (vec vec single-float) vec
    (any #+sb-cga-sse2 always-translatable)
  :result-arg 1)
