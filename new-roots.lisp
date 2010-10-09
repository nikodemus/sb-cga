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

;;;; Based on Graphics Gems Roots3And4.c by Jochen Schwarze (schwarze@isa.de),
;;;; see http://tog.acm.org/resources/GraphicsGems/gems/Roots3And4.c for the
;;;; original.
;;;;
;;;; This is mostly a 1-1 translation into CL from that.

;;; quadrics and cubics tested, quartics are probably still buggy...

(in-package :sb-cga)

(defconstant +eqn-eps+ 1d-9)

(declaim (inline is-zero))
(defun is-zero (x)
  (~ 0.0d0 x +eqn-eps+))

(declaim (inline make-roots))
(defun make-roots (n)
  (declare (fixnum n))
  (make-array n :element-type 'double-float))

(defun %solve-quadric (quadric roots offset)
  (declare (type (simple-array double-float (*)) quadric roots))
  ;; normal form: x^2 + px + q = 0
  (let* ((K (aref quadric 2))
         (p (/ (aref quadric 1) (* 2.0d0 K)))
         (q (/ (aref quadric 0) K))
         (D (- (* p p) q)))
    (cond ((is-zero D)
           (setf (aref roots (+ offset 0)) (- p))
           (values 1 roots))
          ((minusp D)
           (values 0 roots))
          (t
           (let ((sqrt-D (sqrt D)))
             (setf (aref roots (+ offset 0)) (- sqrt-D p)
                   (aref roots (+ offset 1)) (- (- sqrt-D) p))
             (values 2 roots))))))

(declaim (inline solve-quadric))
(defun solve-quadric (quadric &optional (roots (make-roots 2)))
  "Solve QUADRIC, returning number of real roots and storing their values in ROOTS,
starting at OFFSET."
  (%solve-quadric quadric roots 0))

(defun solve-cubic (cubic &optional (roots (make-roots 3)))
  "Solve CUBIC, returning number of real roots and storing their values in ROOTS."
  (declare (type (simple-array double-float (*)) cubic roots))
  ;; Convert to double-floats: we lose too much precision during the solution
  ;; otherwise.
  ;;
  ;; Normal form: x^3 + Ax^2 + Bx + C = 0
  (let* ((K (aref cubic 3))
         (A (/ (aref cubic 2) K))
         (B (/ (aref cubic 1) K))
         (C (/ (aref cubic 0) K))
         (n-roots 0))
    #+nil
    (when (zerop C)
      (setf (aref roots 0) 0.0)
      (return-from solve-cubic 1))
    ;; Substitute x = y - A/3 to eliminate quadric term: x^3 + px + q = 0
    (let* ((A^2 (* A A))
           (p (* 1/3 (+ (* -1/3 A^2) B)))
           (q (* 1/2 (+ (* 2/27 A A^2) (* -1/3 A B) C))))
      ;; Use Cardano's formula
      (let* ((p^3 (* p p p))
             (D (+ (* q q) p^3)))
        (if (is-zero D)
            (if (is-zero q)
                ;; one triple solution
                (setf (aref roots 0) 0.0d0
                      n-roots 1)
                ;; one single and one double solution
                (let ((u (cbrt (- q))))
                  (setf (aref roots 0) (* 2 u)
                        (aref roots 1) (- u)
                        n-roots 2)))
            (if (minusp D)
                ;; Casus irreducibilis: three real solutions
                (let ((phi (* 1/3 (acos (/ (- q) (sqrt (- p^3))))))
                      (th (* 2 (sqrt (- p)))))
                  (setf (aref roots 0) (* th (cos phi))
                        (aref roots 1) (* (- th) (cos (+ phi (/ +pi+ 3))))
                        (aref roots 2) (* (- th) (cos (- phi (/ +pi+ 3))))
                        n-roots 3))
                ;; One real solution
                (let* ((sqrt-D (sqrt D))
                       (u (cbrt (- sqrt-D q)))
                       (v (- (cbrt (+ sqrt-D q)))))
                  (setf (aref roots 0) (+ u v)
                        n-roots 1))))
        ;; Resubstitute
        (let ((sub (* 1/3 A)))
          (dotimes (i n-roots)
            (decf (aref roots i) sub)))
        ;; All done.
        (values n-roots roots)))))

(defun solve-quartic (quartic &optional (roots (make-roots 4)))
  (declare (type (simple-array double-float (*)) quartic roots))
  (let ((coeffs (make-array 4 :element-type 'double-float)))
    (declare (dynamic-extent coeffs))
    (let* ((K (aref quartic 4))
           ;; normal form: x^4 + Ax^3 + Bx^2 + Cx + D = 0
           (A (/ (aref quartic 3) K))
           (B (/ (aref quartic 2) K))
           (C (/ (aref quartic 1) K))
           (D (/ (aref quartic 0) K))
           ;; substitute x = y - A/4 to eliminate cubic term: x^4 + px^2 + qx + r = 0
           (A^2 (* A A))
           (p (+ (* -3/8 A^2) B))
           (q (+ (* 1/8 A^2 A) (* -1/2 A B) C))
           (r (+ (* -3/256 A^2 A^2) (* 1/16 A^2 B) (* -1/4 A C) D))
           (n-roots 0))
      (cond ((is-zero r)
             ;; no absolute term: y(y^3 + py + q) = 0
             (setf (aref coeffs 0) q
                   (aref coeffs 1) p
                   (aref coeffs 2) 0.0d0
                   (aref coeffs 3) 1.0d0
                   n-roots (solve-cubic coeffs roots)
                   (aref roots n-roots) 0.0d0)
             (incf n-roots))
            (t
             ;; solve the resolvent cubic ...
             (setf (aref coeffs 0) (- (* 1/2 r p) (* 1/8 q q))
                   (aref coeffs 1) (- r)
                   (aref coeffs 2) (* -1/2 p)
                   (aref coeffs 3) 1.0d0)
             (solve-cubic coeffs roots)
             ;; ... and take the one real solution ...
             (let ((z (aref roots 0)))
               ;; ... to build two quadric equations
               (let ((u (- (* z z) r))
                     (v (- (* 2 z) p)))
                 (cond ((~ 0 u)
                        (setf u 0.0d0))
                       ((> u 0)
                        (setf u (sqrt u)))
                       (t
                        (return-from solve-quartic (values 0 roots))))
                 (cond ((~ 0 v)
                        (setf v 0.0d0))
                       ((> v 0)
                        (setf v (sqrt v)))
                       (t
                        (return-from solve-quartic (values 0 roots))))
                 (setf (aref coeffs 0) (- z u)
                       (aref coeffs 1) (if (< q 0) (- v) v)
                       (aref coeffs 2) 1.0d0)
                 (setf n-roots (%solve-quadric coeffs roots 0))
                 (setf (aref coeffs 0) (+ z u)
                       (aref coeffs 1) (if (< q 0) v (- v))
                       (aref coeffs 2) 1.0d0)
                 (incf n-roots (%solve-quadric coeffs roots n-roots))))))
      ;; resubstitute
      (let ((sub (* 1/4 A)))
        (dotimes (i n-roots)
          (decf (aref roots i) sub))
        ;; All done!
        (values n-roots roots)))))

;;; Tests

(defun equation (&rest coeffs)
  (make-array (length coeffs)
              :element-type 'double-float
              :initial-contents (mapcar (lambda (c) (coerce c 'double-float))
                                        (reverse coeffs))))

(defun map-coefficients (fun n start stop &optional (step 1))
  (labels ((rec (n coeffs)
             (if (zerop n)
                 ;; No degenerate cases.
                 (unless (zerop (car coeffs))
                   (apply fun coeffs))
                 (loop for c from start upto stop by step
                       do (rec (1- n) (cons c coeffs))))))
    (rec n nil)))

(defun test-quadric ()
  (let ((n 0))
    (map-coefficients
     (lambda (a b c)
       (multiple-value-bind (n-roots roots) (solve-quadric (equation a b c))
         (incf n n-roots)
         (dotimes (i n-roots)
           (let* ((x (aref roots i))
                  (res (+ (* a x x) (* b x) c)))
             (unless (~ 0.0 res 0.0001)
               (error "~S, ~S, ~S: ~S, ~S => ~S"
                      a b c
                      (subseq roots 0 n-roots)
                      x res))))))
     3 -20 20)
    n))

(defun test-cubic ()
  (let ((n 0))
    (map-coefficients
     (lambda (a b c d)
       (multiple-value-bind (n-roots roots) (solve-cubic (equation a b c d))
         (incf n n-roots)
         (dotimes (i n-roots)
           (let* ((x (aref roots i))
                  (res (+ (* a (expt x 3)) (* b (expt x 2)) (* c x) d)))
             (unless (~ 0 res 0.0001)
               (error "(test-1-cubic ~S ~S ~S ~S) => ~S : ~S, ~S"
                      a b c d
                      (subseq roots 0 n-roots)
                      x res))))))
     4 -10 10 1)
    n))

(defun test-quartic ()
  (let ((n 0))
    (map-coefficients
     (lambda (a b c d e)
       (multiple-value-bind (n-roots roots) (solve-quartic (equation a b c d e))
         (incf n n-roots)
         (dotimes (i n-roots)
           (let* ((x (aref roots i))
                  (res (+ (* a (expt x 4)) (* b (expt x 3)) (* c (expt x 2)) (* d x) e)))
             (unless (~ 0 res 0.009)
               (error "quartic: ~S ~S ~S ~S ~S ~S~%~S roots: ~S~%~S => ~S"
                      a b c d e (list (/ b a) (/ c a) (/ d a) (/ e a))
                      n-roots roots
                      x res))))))
     5 -10.0 10.0 1.0)
    n))
