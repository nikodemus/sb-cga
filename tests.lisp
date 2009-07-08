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
    (is (vec= (vec 0.0 0.0 0.0 0.0)
              (alloc-vec)))
  t)

(deftest vecp.1
    (vecp (alloc-vec))
  t)

(deftest vecp.2
    (vecp (make-array 3 :element-type 'single-float))
  nil)

(deftest vec=.1
    (is (eq t (vec= (vec 1.0 1.0 1.0 1.0)
                    (vec 1.0 1.0 1.0 1.0))))
  t)

(deftest vec=.2
    (is (eq nil (vec= (vec -1.0 1.0 1.0 1.0)
                      (vec 1.0 1.0 1.0 1.0))))
  t)

(deftest vec~.1
    (vec~ (vec 1.1 2.1 3.1 3.9)
          (vec 1.0 2.0 3.0 4.0)
          0.100001)
  t)

(deftest vec~.2
    (vec~ (vec 1.1 2.1 3.1 3.9)
          (vec 1.0 2.0 3.0 4.0)
          0.1)
  nil)

(deftest copy-vec.1
    (let* ((orig (vec 1.0 2.0 3.0 4.0))
           (copy (copy-vec orig)))
      (and (not (eq orig copy))
           (vec= orig copy)))
  t)

(deftest %copy-vec.1
    (let* ((orig (vec 1.0 2.0 3.0 4.0))
           (other (vec 0.1 0.2 0.3 0.4))
           (copy (%copy-vec other orig)))
      (and (eq other copy)
           (not (eq orig copy))
           (vec= copy orig)))
  t)

(deftest %copy-vec.2
    (is (vec= (vec 1.0 0.1 3.0 4.0)
              (%copy-vec (alloc-vec) (vec 1.0 0.1 3.0 4.0))))
  t)

(deftest point.1
    (pointp (point 1.0 2.0 3.0))
  t)

(deftest point.2
    (vector3p (point 1.0 2.0 3.0))
  nil)

(deftest vector3.1
    (vector3p (vector3 1.0 2.0 3.0))
  t)

(deftest vector3.2
    (pointp (vector3 1.0 2.0 3.0))
  nil)

(deftest point->vector3.1
    (is (vec= (vector3 1.0 2.0 3.0)
              (point->vector3 (point 1.0 2.0 3.0))))
  t)

(deftest point->vector3.1
    (handler-case
        (point->vector3 (vector3 1.0 2.0 3.0))
      (type-error ()
        :error))
  :error)

(deftest vector3->point.1
    (is (vec= (point 1.0 2.0 3.0)
              (vector3->point (vector3 1.0 2.0 3.0))))
  t)

(deftest vector3->point.1
    (handler-case
        (vector3->point (point 1.0 2.0 3.0))
      (type-error ()
        :error))
  :error)

(deftest vec+.1
    (is (vec= (vec 1.0 2.0 -3.0 -4.0)
              (vec+ (vec 1.0 0.0 1.0 0.1)
                    (vec 0.0 2.0 -4.0 -4.1))))
  t)

(deftest %vec+.1
    (is (vec= (vec 0.1 0.2 0.3 0.4)
              (%vec+ (vec 1.0 1.0 1.0 1.0)
                     (vec 0.1 0.1 0.1 0.1)
                     (vec 0.0 0.1 0.2 0.3))))
  t)

(deftest vec-.1
    (is (vec= (vec -0.100000024 -2.0 5.0 4.2)
              (vec- (vec 1.0 0.0 1.0 0.1)
                    (vec 1.1 2.0 -4.0 -4.1))))
  t)

(deftest %vec-.1
    (is (vec= (vec -0.1 0.0 0.1 0.20000002)
              (%vec- (vec 1.0 1.0 1.0 1.0)
                     (vec 0.0 0.1 0.2 0.3)
                     (vec 0.1 0.1 0.1 0.1))))
  t)

(deftest vec*.1
    (is (vec= (vec 1.0 2.0 3.0 4.0)
              (vec* (vec 0.5 1.0 1.5 2.0) 2.0)))
  t)

(deftest %vec*.1
    (is (vec= (vec 1.0 2.0 3.0 4.0)
              (%vec* (vec 5.5 5.5 5.5 5.5)
                     (vec 0.5 1.0 1.5 2.0) 2.0)))
  t)

(deftest vec/.1
    (is (vec= (vec 1.0 2.0 3.0 4.0)
              (vec/ (vec 0.5 1.0 1.5 2.0) 0.5)))
  t)

(deftest %vec/.1
    (is (vec= (vec 1.0 2.0 3.0 4.0)
              (%vec/ (vec 5.5 5.5 5.5 5.5)
                     (vec 0.5 1.0 1.5 2.0) 0.5)))
  t)

(deftest dot-product.1
    (is (= 4.0 (dot-product (vec 1.0 1.0 1.0 1.0)
                            (vec 1.0 1.0 1.0 1.0))))
  t)

(deftest dot-product.2
    (is (= 0.0 (dot-product (vec 0.0 0.0 0.0 0.0)
                            (vec 1.1 2.2 3.3 4.4))))
  t)

(deftest hadamard-product.1
    (is (vec= (vec 1.2 4.0 10.0 3.0)
              (hadamard-product (vec 0.1 1.0 4.0 1.5)
                                (vec 12.0 4.0 2.5 2.0))))
  t)

(deftest %hadamard-product.1
    (is (vec= (vec 1.2 4.0 10.0 3.0)
              (%hadamard-product (alloc-vec)
                                 (vec 0.1 1.0 4.0 1.5)
                                 (vec 12.0 4.0 2.5 2.0))))
  t)

(deftest vec-length.1
    (is (= 1.0 (vec-length (vector3 1.0 0.0 0.0))))
  t)

(deftest vec-length.1
    (is (= 1.7320508 (vec-length (vector3 -1.0 1.0 1.0))))
  t)

(deftest normalize.1
    (is (vec= (vec 0.26726124 0.5345225 0.8017837 0.0)
              (normalize (vector3 1.0 2.0 3.0))))
  t)

(deftest normalize.2
    (= 0.99999994 (vec-length (normalize (vector3 1.0 2.0 3.0))))
  t)

(deftest %normalize.1
    (= 0.99999994 (vec-length (%normalize (alloc-vec)
                                          (vector3 1.0 2.0 4.0))))
  t)

(deftest vec-lerp.1
    (is (vec= (point 1.5 1.5 1.5)
              (vec-lerp (point 1.0 1.0 1.0)
                        (point 2.0 2.0 2.0)
                        0.5)))
  t)

(deftest %vec-lerp.1
    (is (vec= (point 1.75 1.75 1.75)
              (%vec-lerp (alloc-vec)
                         (point 1.0 1.0 1.0)
                         (point 2.0 2.0 2.0)
                         0.75)))
  t)

(deftest vec-min.1
    (is (vec= (vec 0.1 0.2 0.3 0.4)
              (vec-min (vec 0.1 2.0 3.0 4.0)
                       (vec 1.0 0.2 3.0 4.0)
                       (vec 1.0 2.0 0.3 4.0)
                       (vec 1.0 2.0 3.0 0.4))))
  t)

(deftest vec-max.1
    (is (vec= (vec 0.1 0.2 0.3 0.4)
              (vec-max (vec 0.1 -2.0 -3.0 -4.0)
                       (vec -1.0 0.2 -3.0 -4.0)
                       (vec -1.0 -2.0 0.3 -4.0)
                       (vec -1.0 -2.0 -3.0 0.4))))
  t)

(deftest cross-product.1
    (is (vec= (vector3 0.0 0.0 1.0)
              (cross-product (vector3 1.0 0.0 0.0)
                             (vector3 0.0 1.0 0.0))))
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
            (matrixp (vector3 1.0 2.0 3.0))
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
    (is (vec= (point 1.0 2.0 3.0)
              (transform-vec (point 0.0 0.0 0.0)
                             (translate (vector3 1.0 2.0 3.0)))))
  t)

(deftest translate.2
    (is (vec= (point 1.1 2.2 3.3)
              (transform-vec (point 0.1 0.2 0.3)
                             (translate (vector3 1.0 2.0 3.0)))))
  t)

(deftest translate.3
    (is (vec= (vector3 0.0 0.0 0.0)
              (transform-vec (vector3 0.0 0.0 0.0)
                             (translate (vector3 1.0 2.0 3.0)))))
  t)

(deftest scale.1
    (is (vec= (point 1.0 2.0 3.0)
              (transform-vec (point 0.5 4.0 1.0)
                             (scale (vector3 2.0 0.5 3.0)))))
  t)

(deftest scale.2
    (is (vec= (vector3 1.0 2.0 3.0)
              (transform-vec (vector3 0.5 4.0 1.0)
                             (scale (vector3 2.0 0.5 3.0)))))
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
      (values (vec= (point 1.0 4.0 4.0)
                    (transform-vec (point 1.0 1.0 1.0) (matrix* scale scale)))
              (vec= (point -1.0 -3.0 -7.0)
                    (transform-vec (point 1.0 1.0 1.0) (matrix* trans trans)))
              (vec= (point 0.0 -2.0 -6.0)
                    (transform-vec (point 1.0 1.0 1.0) (matrix* scale trans)))
              (vec= (point -1.0 -4.0 -10.0)
                    (transform-vec (point 1.0 1.0 1.0) (matrix* trans scale trans)))))
  t t t t)

(deftest rotate.1
    ;; One at a time.
    (values (is (vec= (point 1.0 0.0 0.0)
                      (transform-vec (point 1.0 0.0 0.0)
                                     (rotate (vector3 3.0 0.0 0.0)))))
            (is (vec= (vector3 1.0 0.0 0.0)
                      (transform-vec (vector3 1.0 0.0 0.0)
                                     (rotate (vector3 3.0 0.0 0.0)))))
            (is (vec~ (point -1.0 0.0 0.0)
                      (transform-vec (point 1.0 0.0 0.0)
                                     (rotate (vector3 0.0 +pi+ 0.0)))))
            (is (vec~ (vector3 -1.0 0.0 0.0)
                      (transform-vec (vector3 1.0 0.0 0.0)
                                     (rotate (vector3 0.0 +pi+ 0.0)))))
            (is (vec~ (point -1.0 0.0 0.0)
                      (transform-vec (point 1.0 0.0 0.0)
                                     (rotate (vector3 0.0 0.0 +pi+)))))
            (is (vec~ (vector3 -1.0 0.0 0.0)
                      (transform-vec (vector3 1.0 0.0 0.0)
                                     (rotate (vector3 0.0 0.0 +pi+))))))
  t t
  t t
  t t)

(deftest rotate.2
    ;; Order of rotations should be x, y ,z
    (values ;; Explicit order
     (is (vec~ (point 0.0 1.0 0.0)
               (transform-vec
                (transform-vec
                 (transform-vec (point 0.0 1.0 0.0)
                                (rotate* (/ +pi+ 4) 0.0 0.0))
                 (rotate* 0.0 (/ +pi+ 2) 0.0))
                (rotate* 0.0 0.0 (/ +pi+ 4)))))
     ;; Same thing with implicit order
     (is (vec~ (point 0.0 1.0 0.0)
               (transform-vec (point 0.0 1.0 0.0)
                              (rotate* (/ +pi+ 4) (/ +pi+ 2) (/ +pi+ 4))))))
  t t)

(deftest rotate-around.1
    (values (vec~ (point -1.0 0.0 0.0)
                  (transform-vec (point 1.0 0.0 0.0)
                                 (rotate-around (vector3 0.0 1.0 0.0) +pi+)))
            (vec~ (point 0.0 -1.0 0.0)
                  (transform-vec (point 0.0 1.0 0.0)
                                 (rotate-around (vector3 1.0 0.0 0.0) +pi+)))
            (vec~ (point 0.0 -1.0 0.0)
                  (transform-vec (point 0.0 1.0 0.0)
                                 (rotate-around (vector3 0.0 0.0 1.0) +pi+)))
            (vec~ (point 0.0 1.0 0.0)
                  (transform-vec (point 1.0 0.0 0.0)
                                 (rotate-around (normalize (vector3 1.0 1.0 0.0)) +pi+))))
  t t t t)

(deftest reorient.1
    (vec~ (normalize (vector3 1.0 1.0 0.5))
          (transform-vec (vector3 1.0 0.0 0.0)
                         (reorient (vector3 1.0 0.0 0.0)
                                   (vector3 1.0 1.0 0.5))))
  t)

(deftest inverse-matrix.1
    (let* ((m (random-affine-matrix))
           (m^-1 (inverse-matrix m)))
      (matrix~ (identity-matrix) (matrix* m m^-1)))
  t)
