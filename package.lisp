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
   #:alloc-vec
   #:matrix
   #:point
   #:point->vector3
   #:pointp
   #:vec
   #:vecp
   #:vector3
   #:vector3->point
   #:vector3p
   )
  (:use :cl :sb-int))

;;;; KLUDGE: SBCL doesn't currently have SSE2 feature, but it's cleaner to
;;;; conditionalize on a single feature (relevant once x86 gets SSE2
;;;; instructions.)
#+x86-64
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :sb-cga-sse2 *features*))

(defpackage :sb-cga-vm
  (:export
   #:%vec=)
  (:use :cl :sb-c :sb-int :sb-cga)
  #+sb-cga-sse2
  (:import-from :sb-vm
                ;; General purpose stuff
                #:inst
                #:descriptor-reg
                #:single-reg
                ;; Instruction names
                #:test
                #:cmpps
                #:movmskps
                #:movaps
                ))
