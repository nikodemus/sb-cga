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

;;;; PREDICATES

(declaim (ftype (sfunction (t) boolean))
         (inline vecp))
(defun vecp (object)
  "Return true if OBJECT is a VEC.."
  (typep object 'vec))

(declaim (ftype (sfunction (t) boolean) pointp)
         (inline pointp))
(defun pointp (object)
  "Return true if OBJECT is a point."
  (and (vecp object) (= 1.0 (aref object 3))))

(declaim (ftype (sfunction (t) boolean) pointp)
         (inline pointp))
(defun vector3p (object)
  "Return true if OBJECT is a 3D vector."
  (and (vecp object) (= 0.0 (aref object 3))))
