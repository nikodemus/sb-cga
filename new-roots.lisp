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

(sb-alien:define-alien-routine ("cbrtf" cbrt/single) float (float float))
(sb-alien:define-alien-routine ("cbrt" cbrt/double) sb-alien:double (double sb-alien:double))

(declaim (inline cbrt))
(defun cbrt (float)
  "Cube root of FLOAT."
  (etypecase float
    (single-float
     (cbrt/single float))
    (double-float
     (cbrt/double float))))

(defun solve-quadric (quadric roots &key (offset 0))
  "Solve QUADRIC, returning number of real roots and storing their values in ROOTS,
starting at OFFSET."
  (declare (type (simple-array single-float (*)) quadric roots))
  ;; normal form: x^2 + px + q = 0
  (let* ((K (aref quadric 2))
         (p (/ (aref quadric 1) (* 2.0 K)))
         (q (/ (aref quadric 0) K))
         (D (- (* p p) q)))
    (cond ((~ 0 D)
           (setf (aref roots 0) (- p))
           1)
          ((< D 0)
           0)
          (t
           (let ((sqrt-D (sqrt D)))
             (setf (aref roots (+ offset 0)) (- sqrt-D p)
                   (aref roots (+ offset 1)) (- (- sqrt-D) p))
             2)))))

(defun solve-cubic (cubic roots)
  "Solve CUBIC, returning number of real roots and storing their values in ROOTS."
  (declare (type (simple-array single-float (*)) cubic roots))
  ;; Convert to double-floats: we lose too much precision during the solution
  ;; otherwise.
  ;;
  ;; Normal form: x^3 + Ax^2 + Bx + C = 0
  (let* ((K (/ 1.0d0 (aref cubic 3)))
         (A (* (aref cubic 2) K))
         (B (* (aref cubic 1) K))
         (C (* (aref cubic 0) K))
         (n-roots 0))
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
        (if (~ 0 D)
            (if (~ 0 q)
                ;; one triple solution
                (setf (aref roots 0) 0.0
                      n-roots 1)
                ;; one single and one double solution
                (let ((u (cbrt (- q))))
                  (setf (aref roots 0) (coerce (* 2 u) 'single-float)
                        (aref roots 1) (coerce (- u) 'single-float)
                        n-roots 2)))
            (if (< D 0)
                ;; Casus irreducibilis: three real solutions
                (let ((phi (* 1/3 (acos (/ (- q) (sqrt (- p^3))))))
                      (th (* 2 (sqrt (- p)))))
                  (setf (aref roots 0) (coerce (* th (cos phi)) 'single-float)
                        (aref roots 1) (coerce (* (- th) (cos (+ phi (/ +pi+ 3)))) 'single-float)
                        (aref roots 2) (coerce (* (- th) (cos (- phi (/ +pi+ 3)))) 'single-float)
                        n-roots 3))
                ;; One real solution
                (let* ((sqrt-D (sqrt D))
                       (u (cbrt (- sqrt-D q)))
                       (v (- (cbrt (+ sqrt-D q)))))
                  (setf (aref roots 0) (coerce (+ u v) 'single-float)
                        n-roots 1))))
        ;; Resubstitute
        (let ((sub (coerce (* 1/3 A) 'single-float)))
          (dotimes (i n-roots)
            (decf (aref roots i) sub)))
        ;; All done.
        n-roots))))

(defun solve-quartic (quartic roots)
  (declare (type (simple-array single-float (5)) quartic)
           (type (simple-array single-float (4)) roots))
  (let ((coeffs (make-array 4 :element-type 'single-float)))
    (declare (dynamic-extent coeffs))
    (let* ((K (/ 1.0 (aref quartic 4)))
           ;; normal form: x^4 + Ax^3 + Bx^2 + Cx + D = 0
           (A (* (aref quartic 3) K))
           (B (* (aref quartic 2) K))
           (C (* (aref quartic 1) K))
           (D (* (aref quartic 0) K))
           ;; substitute x = y - A/4 to eliminate cubic term: x^4 + px^2 + qx + r = 0
           (A^2 (* A A))
           (p (+ (* -3/8 A^2) B))
           (q (+ (- (* 1/8 A^2) (* 1/2 A B)) C))
           (r (+ (* -3/256 A^2 A^2) (* 1/16 A^2 B) (* -1/4 A C) D))
           (n-roots 0))
      (cond ((~ 0 r)
             ;; no absolute term: y(y^3 + py + q) = 0
             (setf (aref coeffs 0) q
                   (aref coeffs 1) p
                   (aref coeffs 2) 0.0
                   (aref coeffs 3) 1.0
                   n-roots (solve-cubic coeffs roots)
                   (aref roots n-roots) 0.0)
             (incf n-roots))
            (t
             ;; solve the resolvent cubic ...
             (setf (aref coeffs 0) (+ (* 1/2 r p) (* -1/8 q q))
                   (aref coeffs 1) (- r)
                   (aref coeffs 2) (* -1/2 p)
                   (aref coeffs 3) 1.0)
             (solve-cubic coeffs roots)
             ;; ... and take the one real solution ...
             (let ((z (aref roots 0)))
               ;; ... to build two quadric equations
               (let ((u (- (* z z) r))
                     (v (- (* 2 z) p)))
                 (cond ((~ 0 u)
                        (setf u 0.0))
                       ((> u 0)
                        (setf u (sqrt u)))
                       (t
                        (return-from solve-quartic 0)))
                 (cond ((~ 0 v)
                        (setf v 0.0))
                       ((> v 0)
                        (setf v (sqrt v)))
                       (t
                        (return-from solve-quartic 0)))
                 (setf (aref coeffs 0) (- z u)
                       (aref coeffs 1) (if (< q 0) (- v) v)
                       (aref coeffs 2) 1.0)
                 (setf n-roots (solve-quadric coeffs roots))
                 (setf (aref coeffs 0) (+ z u)
                       (aref coeffs 1) (if (< q 0) v (- v))
                       (aref coeffs 2) 1.0)
                 (incf n-roots (solve-quadric coeffs roots :offset n-roots))))))
      ;; resubstitute
      (let ((sub (* 1/4 A)))
        (dotimes (i n-roots)
          (decf (aref roots i) sub))
        ;; All done!
        n-roots))))

;;; Tests

(defun test-quadratic ()
  (let ((n 0))
    (loop for a from -20.0 upto 20.0 by 1.0
          do (unless (zerop a)
               (loop for b from -20.0 upto 20.0 by 1.0
                     do (loop for c from -20.0 upto 20.0 by 1.0
                              do (let* ((quad (make-array 3 :element-type 'single-float
                                                          :initial-contents (list c b a)))
                                        (sol (make-array 2 :element-type 'single-float))
                                        (roots (sb-cga::solve-quadric quad sol)))
                                   (incf n roots)
                                   (dotimes (i roots)
                                     (let* ((x (aref sol i))
                                            (y (+ (* a x x) (* b x) c)))
                                       (unless (sb-cga::~ 0 y 0.0001)
                                         (error "~S, ~S, ~S: ~S, ~S => ~S"
                                                a b c
                                                (subseq sol 0 roots)
                                                x y)))))))))
    n))


(defun test-cubic ()
  (let ((n 0))
    (loop for a from -10.0 upto 10.0 by 1.0
          do (unless (zerop a)
               (loop for b from -10.0 upto 10.0 by 1.0
                     do (loop for c from -10.0 upto 10.0 by 1.0
                              do (loop for d from -10.0 upto 10.0 by 1.0
                                       do (incf n (test-1-cubic a b c d)))))))
    n))

(defun test-1-cubic (a b c d)
  (let* ((cubic (make-array 4 :element-type 'single-float
                            :initial-contents (list d c b a)))
         (sol (make-array 3 :element-type 'single-float))
         (roots (sb-cga::solve-cubic cubic sol)))
    (dotimes (i roots)
      (let* ((x (aref sol i))
             (y (+ (* a x x x) (* b x x) (* c x) d)))
        (unless (sb-cga::~ 0 y 0.00111)
          (error "(test-1-cubic ~S ~S ~S ~S) => ~S : ~S, ~S"
                 a b c d
                 (subseq sol 0 roots)
                 x y))))
    roots))
