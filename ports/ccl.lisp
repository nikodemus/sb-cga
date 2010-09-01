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

;;; unoptimized CCL port
;;;

(in-package :sb-cga)

#-ccl
(error "This file is CCL only!")

(defun single-float-quiet-nan () 1f+-0)
(defun double-float-quiet-nan () 1d+-0)

(defun float-nan-p (x)
  (and (ccl::nan-or-infinity-p x)
       (not (ccl::infinity-p x))))

(declaim (inline cbrt))
(defun cbrt (float)
  "Cube root of FLOAT."
  ;; this should probably use FFI like the sbcl version...
  (if (minusp float)
      (- (expt (- float) 1/3))
      (expt float 1/3)))
