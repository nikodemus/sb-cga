;;;; by Nikodemus Siivola <nikodemus@random-state.net>, 2009.
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

;;;; QUADRATIC ROOTS

(declaim (ftype (function (single-float single-float single-float single-float)
                          (values single-float single-float &optional))
                quadratic-roots-above)
         (inline quadratic-roots-above))
(defun quadratic-roots-above (limit a b c)
  "Real-valued roots greater than LIMIT for Ax^2+Bx+C. Smallest positive root
is returned as primary value, and the other as secondary. LIMIT indicates lack
of a real-valued root above LIMIT."
  (declare (optimize speed))
  (let ((d (- (* b b) (* 4.0 a c))))
    (tagbody
       (unless (< d 0.0)
         (go :something))
     :nothing
       (return-from quadratic-roots-above (values limit limit))
     :something
       (let* ((sqrt-d (sqrt d))
              (1/2a (/ 1.0 (+ a a)))
              (-b (- b))
              (r1 (* (+ -b sqrt-d) 1/2a))
              (r2 (* (- -b sqrt-d) 1/2a)))
         (when (> r1 r2)
           (psetf r2 r1
                  r1 r2))
         (if (> r2 limit)
             (return-from quadratic-roots-above
               (if (> r1 limit)
                   (values r1 r2)
                   (values r2 limit)))
             (go :nothing))))))

(declaim (ftype (function (single-float single-float single-float)
                          (values single-float single-float &optional))
                quadratic-roots)
         (inline quadratic-roots))
(defun quadratic-roots (a b c)
  "Real-valued roots for Ax^2+Bx+C. Smallest positive root is returned as
primary value, and the otheras secondary. NaN indicates lack of a real-valued
root."
  (declare (optimize speed))
  (let ((d (- (* b b) (* 4.0 a c)))
        (nan #.(/ 0.0 0.0)))
    (if (< d 0.0)
        (values nan nan)
        (let* ((sqrt-d (sqrt d))
               (1/2a (/ 1.0 (+ a a)))
               (-b (- b))
               (r1 (* (+ -b sqrt-d) 1/2a))
               (r2 (* (- -b sqrt-d) 1/2a)))
          (when (> r1 r2)
            (psetf r2 r1
                   r1 r2))
          (values r1 r2)))))
