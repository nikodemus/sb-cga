;;;; Based on Graphics Gems Roots3And4.c by Jochen Schwarze (schwarze@isa.de)

;;;; This is a mostly 1-1 translation into CL
;;;;
;;;; Next:
;;;;
;;;; - test
;;;; - refactor a bit
;;;; - integrate into SB-CGA

(define-alien-routine ("cbrtf" cbrt) float (cube float))

(defconstant +pi+ (coerce pi 'single-float))

(defconstant +eqn-eps+ 1e-9)

(defun is-zero (f)
  (< (- +eqn-eps+) f +eqn-eps+))

(defun solve-quadric (quadric roots &key (offset 0))
  "Solve QUADRIC, returning number of real roots and storing their values in ROOTS,
starting at OFFSET."
  (declare (type (simple-array single-float (*)) quadric roots))
  ;; normal form: x^2 + px + q = 0
  (let* ((K (aref quadric 2))
         (p (/ (aref quadric 1) (* 2.0 K)))
         (q (/ (aref quadric 0) K))
         (D (- (* p p) q)))
    (cond ((is-zero D)
           (setf (aref roots 0) (- p))
           1)
          ((< D 0)
           0)
          (t
           (let ((sqrt-D (sqrt D)))
             (setf (aref roots (+ offset 0)) (- sqrt-D p)
                   (aref roots (+ offset 1)) (- (- sqrt-D p)))
             2)))))

(defun solve-cubic (cubic roots)
  "Solve CUBIC, returning number of real roots and storing their values in ROOTS."
  (declare (type (simple-array single-float (*)) cubic roots))
  ;; Normal form: x^3 + Ax^2 + Bx + C = 0
  (let* ((K (aref cubic 3))
         (A (/ (aref cubic 2) K))
         (B (/ (aref cubic 1) K))
         (C (/ (aref cubic 0) K))
         (n-roots 0))
    ;; Substitute x = y - A/3 to eliminate quadric term: x^3 + px + q = 0
    (let* ((A^2 (* A A))
           (p (* 1/3 (+ (* -1/3 A^2) * B)))
           (q (* 1/2 (+ (- (* 2/27 A A^2) (* 1/3 A B)) C))))
      ;; Use Cardano's formula
      (let* ((p^3 (* p p p))
             (d (* q q p^3)))
        (if (is-zero d)
            (if (is-zero q)
                ;; one triple solution
                (setf (aref roots 0) 0.0
                      n-roots 1)
                ;; one single and one double solution
                (let ((u (cbrt (- q))))
                  (setf (aref roots 0) (* 2 u)
                        (aref roots 1) (- u)
                        n-roots 2)))
            (if (< d 0)
                ;; Casus irreducibilis: three real solutions
                (let ((phi (* 1/3 (acos (/ (- q) (sqrt (- p^3))))))
                      (th (* 2 (sqrt (- p)))))
                  (setf (aref roots 0) (* th (cos phi))
                        (aref roots 1) (* th (cos (+ phi (/ +pi+ 3))))
                        (aref roots 2) (* th (cos (- phi (/ +pi+ 3))))
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
      (cond ((is-zero r)
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
                 (cond ((is-zero u)
                        (setf u 0.0))
                       ((> u 0)
                        (setf u (sqrt u)))
                       (t
                        (return-from solve-quartic 0)))
                 (cond ((is-zero v)
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