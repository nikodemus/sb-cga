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

(declaim (ftype (sfunction (single-float single-float single-float)
                          (values single-float single-float))
                quadratic-roots)
         (inline quadratic-roots))
(defun quadratic-roots (a b c)
  "Real-valued roots for Ax^2+Bx+C. Smallest real root is returned as primary
value, and the other as the secondary. In case of a double root both the
primary and secondary values are the same. NaN indicates lack of a real-valued
root."
  (declare (optimize speed))
  (let ((d (- (* b b) (* 4.0 a c)))
        (nan (/ 0.0 0.0)))
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

(declaim (ftype (sfunction (double-float double-float double-float double-float)
                           (values double-float double-float double-float))
                %cubic-roots/normal)
         (inline %cubic-roots/normal))
(defun %cubic-roots/normal (a b c replacement)
  ;; Substitute x = y - a/3 to eliminate quadric term: x^3 + px + q = 0
  (let* ((a^2 (* a a))
         (p (* 1/3 (+ (* -1/3 a^2) b)))
         (q (* 1/2 (+ (* 2/27 a a^2) (* -1/3 a b) c))))
    ;; Use Cardano's formula
    (let* ((p^3 (* p p p))
           (D (+ (* q q) p^3))
           ;; for resubstitution
           (sub (* 1/3 a)))
      (if (is-zero D)
          (if (is-zero q)
              ;; one triple solution
              (let ((x1 (- sub)))
                (values x1 replacement replacement))
              ;; one single and one double solution
              (let* ((u (cbrt (- q)))
                     (x1 (- (* 2.0d0 u) sub))
                     (x2 (- (- u) sub)))
                (values x1 x2 replacement)))
          (if (minusp D)
              ;; Casus irreducibilis: three real solutions
              (locally (declare (type (double-float * 0.0d0) p^3 p))
                (let ((phi (* 1/3 (acos (the (double-float -1.0d0 1.0d0)
                                          (/ (- q) (sqrt (- p^3)))))))
                      (th (* 2 (sqrt (- p)))))
                  (values (- (* th (cos phi)) sub)
                          (- (* (- th) (cos (+ phi (/ +pi+ 3)))) sub)
                          (- (* (- th) (cos (- phi (/ +pi+ 3)))) sub))))
              ;; One real solution
              (locally (declare (type (double-float 0.0d0) D))
                (let* ((sqrt-D (sqrt D))
                       (u (cbrt (- sqrt-D q)))
                       (v (- (cbrt (+ sqrt-D q))))
                       (x1 (- (+ u v) sub)))
                  (values x1 replacement replacement))))))))

(declaim (ftype (sfunction (single-float single-float single-float single-float)
                           (values single-float single-float single-float))
                cubic-roots)
         (inline cubic-roots))
(defun cubic-roots (a b c d)
  "Real-valued roots for Ax^2+Bx+C. Smallest real root is returned as primary
value, and the others as the successive values. NaN indicates lack of a
real-valued root."
  (declare (optimize speed))
  ;; Convert to double-floats & normal form: we lose too much precision during
  ;; the solution otherwise.
  ;;
  ;; Normal form: x^3 +(B/A)x^2 + (C/A)x + (D/A) = 0
  (let ((1/a (/ 1.0d0 a))
        (nan (/ 0.0d0 0.0d0)))
    (multiple-value-bind (x1 x2 x3) (%cubic-roots/normal (* b 1/a) (* c 1/a) (* d 1/a) nan)
      (values (coerce x1 'single-float)
              (coerce x2 'single-float)
              (coerce x3 'single-float)))))

(declaim (ftype (sfunction (single-float single-float single-float single-float single-float)
                           (values single-float single-float single-float))
                cubic-roots-above)
         (inline cubic-roots-above))
(defun cubic-roots-above (limit a b c d)
  "Real-valued roots greater than LIMIT for Ax^3+Bx^2+Cx+D. Smallest positive
root is returned as primary value, and others in increasing order. LIMIT
indicates lack of a real-valued root above LIMIT."
  (declare (optimize speed))
  (macrolet ((order-values (&rest vals)
               (ecase (length (the list vals))
                 (2
                  `(if (< ,(first vals) ,(second vals))
                       (values ,(first vals) ,(second vals) limit)
                       (values ,(second vals) ,(first vals) limit)))
                 (3
                  `(if (< x1 x2)
                       (if (< x2 x3)
                           (values x1 x2 x3)
                           (if (< x1 x3)
                               (values x1 x3 x2)
                               (values x3 x1 x2)))
                       (if (< x1 x3)
                           (values x2 x1 x3)
                           (if (< x2 x3)
                               (values x2 x3 x1)
                               (values x3 x2 x1))))))))
    ;; Convert to double-floats & normal form: we lose too much precision during
    ;; the solution otherwise.
    ;;
    ;; Normal form: x^3 +(B/A)x^2 + (C/A)x + (D/A) = 0
    (let ((1/a (/ 1.0d0 a))
          (d-limit (coerce limit 'double-float)))
    (multiple-value-bind (x1 x2 x3) (%cubic-roots/normal (* b 1/a) (* c 1/a) (* d 1/a) d-limit)
      (let ((x1 (coerce x1 'single-float))
            (x2 (coerce x2 'single-float))
            (x3 (coerce x3 'single-float)))
        (cond ((> x1 limit)
               (cond ((> x2 limit)
                      (if (> x3 limit)
                          (order-values x1 x2 x3)
                          (order-values x1 x2)))
                     ((> x3 limit)
                      (order-values x1 x3))
                     (t
                      (values x1 limit limit))))
              ((> x2 limit)
               (if (> x3 limit)
                   (order-values x2 x3)
                   (values x2 limit limit)))
              ((> x3 limit)
               (values x3 limit limit))
              (t
               (values limit limit limit))))))))
