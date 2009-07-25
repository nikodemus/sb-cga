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

(defpackage :sb-cga
  (:documentation
   "Computer graphics algebra package for SBCL.")
  (:export
   #:+default-epsilon+
   #:alloc-vec
   #:adjust-vec #:%adjust-vec
   #:copy-vec #:%copy-vec
   #:cross-product #:%cross-product
   #:dot-product
   #:hadamard-product #:%hadamard-product
   #:identity-matrix #:+identity-matrix+
   #:inverse-matrix
   #:matrix
   #:matrix*
   #:matrix-determinant
   #:matrix=
   #:matrixp
   #:matrix~
   #:mref
   #:normalize #:%normalize
   #:quadratic-roots
   #:quadratic-roots-above
   #:reorient
   #:rotate #:rotate*
   #:rotate-around
   #:scale #:scale*
   #:sxhash-vec
   #:transform-bounds
   #:transform-direction #:%transform-direction
   #:transform-point #:%transform-point
   #:translate #:translate*
   #:transpose-matrix
   #:vec
   #:vec* #:%vec*
   #:vec+ #:%vec+
   #:vec- #:%vec-
   #:vec-length
   #:vec-lerp #:%vec-lerp
   #:vec-max #:%vec-min
   #:vec-min #:%vec-max
   #:vec/ #:%vec/
   #:vec=
   #:vec~
   #:zero-matrix
   )
  (:use :cl :sb-int :sb-c)
  (:import-from :sb-vm
                #:inst
                #:descriptor-reg
                #:single-reg
                )
  (:lock t))
