;;;; By Bart Botta <00003b@gmail.com>, 2010.
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

;;; unoptimized ECL port
;;;

(in-package :sb-cga)

#-ecl
(error "This file is ecl only!")

(defun single-float-quiet-nan ()
  ;;can't build with literal NaNs as of 10.3.1
  (declare (notinline /))
  (/ 0 0f0))
(defun double-float-quiet-nan ()
  (declare (notinline /))
  (/ 0 0d0))

(defun float-nan-p (x)
  ;; hmm, this returns NIL for nan, T for floats in 9.8.3, 10.3.1?
  (not (sys:float-nan-p x)))

(declaim (inline cbrt))
(defun cbrt (float)
  "Cube root of FLOAT."
  ;; todo: inline c or ffi?
  (if (minusp float)
      (- (expt (- float) 1/3))
      (expt float 1/3)))
