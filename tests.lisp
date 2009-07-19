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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-rt))

(defpackage :sb-cga-test
  (:use :cl :sb-rt :sb-cga))

(in-package :sb-cga-test)

(defconstant +pi+ (coerce pi 'single-float))

;;; Cheap, cheap.
(defmacro is ((test result (op &rest args) &rest test-args))
  (let* ((temps (sb-int:make-gensym-list (length args)))
         (form `(,op ,@args))
         (lambda1 `(lambda ,temps (,op ,@temps)))
         (lambda2 `(lambda () (,op ,@args))))
    `(and (,test ,result (eval ',form) ,@test-args)
          (,test ,result (funcall (compile nil ',lambda1) ,@args) ,@test-args)
          (,test ,result (funcall (compile nil ',lambda2)) ,@test-args)
          t)))

(deftest alloc-vec.1
    (is (vec= (vec 0.0 0.0 0.0)
              (alloc-vec)))
  t)

(deftest vec=.1
    (is (eq t (vec= (vec 1.0 1.0 1.0)
                    (vec 1.0 1.0 1.0))))
  t)

(deftest vec=.2
    (is (eq nil (vec= (vec -1.0 1.0 1.0)
                      (vec 1.0 1.0 1.0))))
  t)

(deftest vec~.1
    (vec~ (vec 1.1 2.1 3.1)
          (vec 1.0 2.0 3.0)
          0.100001)
  t)

(deftest vec~.2
    (vec~ (vec 1.11 2.11 3.11)
          (vec 1.0 2.0 3.0)
          0.1)
  nil)

(deftest copy-vec.1
    (let* ((orig (vec 1.0 2.0 3.0))
           (copy (copy-vec orig)))
      (and (not (eq orig copy))
           (vec= orig copy)))
  t)

(deftest %copy-vec.1
    (let* ((orig (vec 1.0 2.0 3.0))
           (other (vec 0.1 0.2 0.3))
           (copy (%copy-vec other orig)))
      (and (eq other copy)
           (not (eq orig copy))
           (vec= copy orig)))
  t)

(deftest %copy-vec.2
    (is (vec= (vec 1.0 0.1 3.0)
              (%copy-vec (alloc-vec) (vec 1.0 0.1 3.0))))
  t)

(deftest vec+.1
    (is (vec= (vec 1.0 2.0 -3.0)
              (vec+ (vec 1.0 0.0 1.0)
                    (vec 0.0 2.0 -4.0))))
  t)

(deftest %vec+.1
    (is (vec= (vec 0.1 0.2 0.3)
              (%vec+ (vec 1.0 1.0 1.0)
                     (vec 0.1 0.1 0.1)
                     (vec 0.0 0.1 0.2))))
  t)

(deftest vec-.1
    (is (vec= (vec -0.100000024 -2.0 5.0)
              (vec- (vec 1.0 0.0 1.0)
                    (vec 1.1 2.0 -4.0))))
  t)

(deftest %vec-.1
    (is (vec= (vec -0.1 0.0 0.1)
              (%vec- (vec 1.0 1.0 1.0)
                     (vec 0.0 0.1 0.2)
                     (vec 0.1 0.1 0.1))))
  t)

(deftest vec*.1
    (is (vec= (vec 1.0 2.0 3.0)
              (vec* (vec 0.5 1.0 1.5) 2.0)))
  t)

(deftest %vec*.1
    (is (vec= (vec 1.0 2.0 3.0)
              (%vec* (vec 5.5 5.5 5.5)
                     (vec 0.5 1.0 1.5) 2.0)))
  t)

(deftest vec/.1
    (is (vec= (vec 1.0 2.0 3.0)
              (vec/ (vec 0.5 1.0 1.5) 0.5)))
  t)

(deftest %vec/.1
    (is (vec= (vec 1.0 2.0 3.0)
              (%vec/ (vec 5.5 5.5 5.5)
                     (vec 0.5 1.0 1.5) 0.5)))
  t)

(deftest dot-product
    (flet ((dot (ax ay az bx by bz)
             (+ (* ax bx) (* ay by) (* az bz)))
           (r (f)
             (- (random (+ 1.0 f)) (/ (+ 1.0 f) 2.0))))
      (dotimes (i 100000)
        (let* ((ax (r i))
               (ay (r i))
               (az (r i))
               (bx (r i))
               (by (r i))
               (bz (r i))
               (a (vec ax ay az))
               (b (vec bx by bz)))
          (unless (= (dot ax ay az bx by bz)
                     (dot-product a b))
            (return (list a b))))))
  nil)

(deftest dot-product.2
    (is (= 0.0 (dot-product (vec 0.0 0.0 0.0)
                            (vec 1.1 2.2 3.3))))
  t)

(deftest dot-product.3
    (is (= 0.0 (dot-product (vec 0.0 0.0 0.0)
                            (vec 1.1 2.2 3.3))))
  t)

(deftest hadamard-product.1
    (is (vec= (vec 1.2 4.0 10.0)
              (hadamard-product (vec 0.1 1.0 4.0)
                                (vec 12.0 4.0 2.5))))
  t)

(deftest %hadamard-product.1
    (is (vec= (vec 1.2 4.0 10.0)
              (%hadamard-product (alloc-vec)
                                 (vec 0.1 1.0 4.0)
                                 (vec 12.0 4.0 2.5))))
  t)

(deftest vec-length.1
    (is (= 1.0 (vec-length (vec 1.0 0.0 0.0))))
  t)

(deftest vec-length.2
    (is (= 1.7320508 (vec-length (vec -1.0 1.0 1.0))))
  t)

(deftest vec-length.3
    (is (= 41.916584 (vec-length (vec -4.0 -30.0 -29.0))))
  t)

(deftest normalize.1
    (is (vec~ (vec 0.26726124 0.5345225 0.8017837)
               (normalize (vec 1.0 2.0 3.0))
               0.001))
  t)

(deftest normalize.2
    (< 0.999 (vec-length (normalize (vec 1.0 2.0 3.0))) 1.0)
  t)

(deftest %normalize.1
    (< 0.999 (vec-length (%normalize (alloc-vec) (vec 1.0 2.0 4.0))) 1.0)
  t)

(deftest vec-lerp.1
    (is (vec= (vec 1.5 1.5 1.5)
              (vec-lerp (vec 1.0 1.0 1.0)
                        (vec 2.0 2.0 2.0)
                        0.5)))
  t)

(deftest %vec-lerp.1
    (is (vec= (vec 1.75 1.75 1.75)
              (%vec-lerp (alloc-vec)
                         (vec 1.0 1.0 1.0)
                         (vec 2.0 2.0 2.0)
                         0.75)))
  t)

(deftest vec-min.1
    (is (vec= (vec 0.1 0.2 0.3)
              (vec-min (vec 0.1 2.0 3.0)
                       (vec 1.0 0.2 3.0)
                       (vec 1.0 2.0 0.3)
                       (vec 1.0 2.0 3.0))))
  t)

(deftest vec-max.1
    (is (vec= (vec 0.1 0.2 0.3)
              (vec-max (vec 0.1 -2.0 -3.0)
                       (vec -1.0 0.2 -3.0)
                       (vec -1.0 -2.0 0.3)
                       (vec -1.0 -2.0 -3.0))))
  t)

(deftest cross-product.1
    (is (vec= (vec 0.0 0.0 1.0)
              (cross-product (vec 1.0 0.0 0.0)
                             (vec 0.0 1.0 0.0))))
  t)

(deftest mref.1
    (let ((m (matrix 1.0 2.0 3.0 4.0
                     5.0 6.0 7.0 8.0
                     9.0 10.0 11.0 12.0
                     13.0 14.0 15.0 16.0)))
      (setf (mref m 2 0) (- 9.0))
      (values (mref m 2 0)
              (mref m 2 1)
              (mref m 1 2)
              (mref m 1 3)))
  -9.0
  10.0
  7.0
  8.0)

(deftest matrixp.1
    (values (matrixp 1.0)
            (matrixp (vec 1.0 2.0 3.0))
            (matrixp (zero-matrix)))
  nil
  nil
  t)

(deftest matrix=
    (values (matrix= (zero-matrix) (identity-matrix))
            (matrix= (matrix 1.0 2.0 3.0 4.0
                             4.0 6.0 7.0 8.0
                             9.0 10.0 11.0 12.0
                             13.0 14.0 15.0 16.0)
                     (matrix 1.0 2.0 3.0 4.0
                             4.0 6.0 7.0 8.0
                             9.0 10.0 11.0 12.0
                             13.0 14.0 15.0 16.0)))
  nil
  t)

(deftest translate.1
    (values (is (vec= (vec -1.0 2.0 3.0)
                      (transform-point (vec 0.0 0.0 0.0)
                                       (translate (vec -1.0 2.0 3.0)))))
            (is (vec= (vec 0.0 0.0 0.0)
                      (transform-direction (vec 0.0 0.0 0.0)
                                           (translate (vec -1.0 2.0 3.0))))))
  t t)

(deftest translate.2
    (is (vec= (vec 1.1 2.2 3.3)
              (transform-point (vec 0.1 0.2 0.3)
                               (translate (vec 1.0 2.0 3.0)))))
  t)

(deftest translate.3
    (is (vec= (vec 0.0 0.0 0.0)
              (transform-direction (vec 0.0 0.0 0.0)
                                   (translate (vec 1.0 2.0 3.0)))))
  t)

(deftest scale.1
    (is (vec= (vec 1.0 2.0 3.0)
              (transform-point (vec 0.5 4.0 1.0)
                               (scale (vec 2.0 0.5 3.0)))))
  t)

(deftest scale.2
    (is (vec= (vec 1.0 2.0 3.0)
              (transform-direction (vec 0.5 4.0 1.0)
                                   (scale (vec 2.0 0.5 3.0)))))
  t)

(defun random-matrix (&optional (scale 1.0))
  (let ((m (zero-matrix)))
    (dotimes (i 4)
      (dotimes (j 4)
        (setf (mref m i j) (random scale))))
    m))

(defun random-affine-matrix (&optional (scale 1.0))
  (let ((m (zero-matrix)))
    (dotimes (i 3)
      (dotimes (j 4)
        (setf (mref m i j) (random scale))))
    (setf (mref m 3 3) 1.0)
    m))

(deftest matrix*.1
    (let ((r (random-matrix))
          (i (identity-matrix)))
      (values (matrix= r (matrix* r i))
              (matrix= r (matrix* i r))))
  t t)

(deftest matrix*.2
    (let* ((scale (scale* 1.0 2.0 2.0))
           (trans (translate* -1.0 -2.0 -4.0)))
      (declare (optimize (debug 3)))
      (values (vec= (vec 1.0 4.0 4.0)
                    (transform-point (vec 1.0 1.0 1.0) (matrix* scale scale)))
              (vec= (vec -1.0 -3.0 -7.0)
                    (transform-point (vec 1.0 1.0 1.0) (matrix* trans trans)))
              (vec= (vec 0.0 -2.0 -6.0)
                    (transform-point (vec 1.0 1.0 1.0) (matrix* scale trans)))
              (vec= (vec -1.0 -4.0 -10.0)
                    (transform-point (vec 1.0 1.0 1.0) (matrix* trans scale trans)))))
  t t t t)

(deftest rotate.1
    ;; One at a time.
    (values (is (vec= (vec 1.0 0.0 0.0)
                      (transform-point (vec 1.0 0.0 0.0)
                                       (rotate (vec 3.0 0.0 0.0)))))
            (is (vec= (vec 1.0 0.0 0.0)
                      (transform-point (vec 1.0 0.0 0.0)
                                       (rotate (vec 3.0 0.0 0.0)))))
            (is (vec~ (vec -1.0 0.0 0.0)
                      (transform-point (vec 1.0 0.0 0.0)
                                       (rotate (vec 0.0 +pi+ 0.0)))))
            (is (vec~ (vec -1.0 0.0 0.0)
                      (transform-point (vec 1.0 0.0 0.0)
                                       (rotate (vec 0.0 +pi+ 0.0)))))
            (is (vec~ (vec -1.0 0.0 0.0)
                      (transform-point (vec 1.0 0.0 0.0)
                                       (rotate (vec 0.0 0.0 +pi+)))))
            (is (vec~ (vec -1.0 0.0 0.0)
                      (transform-point (vec 1.0 0.0 0.0)
                                       (rotate (vec 0.0 0.0 +pi+))))))
  t t
  t t
  t t)

(deftest rotate.2
    ;; Order of rotations should be x, y ,z
    (values ;; Explicit order
     (is (vec~ (vec 0.0 1.0 0.0)
               (transform-point
                (transform-point
                 (transform-point (vec 0.0 1.0 0.0)
                                  (rotate* (/ +pi+ 4) 0.0 0.0))
                 (rotate* 0.0 (/ +pi+ 2) 0.0))
                (rotate* 0.0 0.0 (/ +pi+ 4)))))
     ;; Same thing with implicit order
     (is (vec~ (vec 0.0 1.0 0.0)
               (transform-point (vec 0.0 1.0 0.0)
                                (rotate* (/ +pi+ 4) (/ +pi+ 2) (/ +pi+ 4))))))
  t t)

(deftest rotate-around.1
    (values (vec~ (vec -1.0 0.0 0.0)
                  (transform-point (vec 1.0 0.0 0.0)
                                   (rotate-around (vec 0.0 1.0 0.0) +pi+)))
            (vec~ (vec 0.0 -1.0 0.0)
                  (transform-point (vec 0.0 1.0 0.0)
                                   (rotate-around (vec 1.0 0.0 0.0) +pi+)))
            (vec~ (vec 0.0 -1.0 0.0)
                  (transform-point (vec 0.0 1.0 0.0)
                                   (rotate-around (vec 0.0 0.0 1.0) +pi+)))
            (vec~ (vec 0.0 1.0 0.0)
                  (transform-point (vec 1.0 0.0 0.0)
                                   (rotate-around (normalize (vec 1.0 1.0 0.0)) +pi+))
                  0.001))
  t t t t)

(deftest reorient.1
    (vec~ (normalize (vec 1.0 1.0 0.5))
          (transform-point (vec 1.0 0.0 0.0)
                           (reorient (vec 1.0 0.0 0.0)
                                     (vec 1.0 1.0 0.5)))
          0.001)
  t)

(deftest inverse-matrix.1
    (let* ((ma (random-affine-matrix 100.0))
           (ma2 (inverse-matrix ma))
           (m (loop for m = (random-matrix 100.0)
                    when (> (abs (matrix-determinant m)) +default-epsilon+)
                    return m))
           (m2 (inverse-matrix m))
           (i (identity-matrix)))
      (values (matrix~ i (matrix* ma2 ma) 0.001)
              (matrix~ i (matrix* ma ma2) 0.001)
              (matrix~ i (matrix* m2 m) 0.001)
              (matrix~ i (matrix* m m2) 0.001)))
  t t t t)

(deftest optimize-alloction
    ;; Test all pairs
    (let ((vm-ops '(transform-direction transform-point))
          (vvf-ops '(adjust-vec vec-lerp))
          (v-ops '(normalize copy-vec))
          (vv-ops '(hadamard-product vec- vec+))
          (vf-ops '(vec/ vec*))
          (sb-ext:*evaluator-mode* :interpret)
          (problems nil))
      (labels ((r () (- (random 10.0) 5.0))
               (vr () `(vec ,(r) ,(r) ,(r)))
               (args (op)
                 (cond ((member op vm-ops)
                        (list (vr) `(matrix ,@(map 'list #'identity
                                                   (transpose-matrix (random-affine-matrix))))))
                       ((member op vvf-ops)
                        (list (vr) (vr) (r)))
                       ((member op v-ops)
                        (list (vr)))
                       ((member op vv-ops)
                        (list (vr) (vr)))
                       ((member op vf-ops)
                        (list (vr) (r)))
                       (t
                        (error "unknown op ~S" op)))))
        (dolist (spec1 sb-cga::*optimizable-funs*)
          (dolist (spec2 sb-cga::*optimizable-funs*)
            (let* ((name1 (car spec1))
                   (name2 (car spec2))
                   (args1 (cdr (args name1)))
                   (args2 (args name2))
                   (gensyms1 (sb-int:make-gensym-list (length args1)))
                   (gensyms2 (sb-int:make-gensym-list (length args2)))
                   (form `(,name1 (,name2 ,@args2) ,@args1))
                   (fun (compile nil
                                 `(lambda (,@gensyms2 ,@gensyms1)
                                    (,name1 (,name2 ,@gensyms2) ,@gensyms1)))))
              (unless (equalp (eval form)
                              (apply fun
                                     (append (mapcar #'eval args2)
                                             (mapcar #'eval args1))))
                (push form problems))
              (when (third spec1)
                (let* ((form2 `(,name1 ,(car args1) (,name2 ,@args2) ,@(cdr args1)))
                       (fun2 (compile nil
                                      `(lambda (,@gensyms2 ,@gensyms1)
                                         (,name1 ,(car gensyms1) (,name2 ,@gensyms2) ,@(cdr gensyms1))))))
                  (unless (equalp (eval form2)
                                  (apply fun2
                                         (append (mapcar #'eval args2)
                                                 (mapcar #'eval args1)))))))))))
      problems)
  nil)
