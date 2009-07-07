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

(require :sb-rt)

(defpackage :sb-cga-test
  (:use :cl :sb-rt :sb-cga))

(in-package :sb-cga-test)

;;; Cheap, cheap.
(defmacro is ((test result (op &rest args)))
  (let* ((temps (sb-int:make-gensym-list (length args)))
         (form `(,op ,@args))
         (lambda `(lambda ,temps (,op ,@temps))))
    `(and (,test ,result (eval ',form))
          (,test ,result (funcall (compile nil ',lambda) ,@args))
          t)))

(deftest alloc-vec.1
    (is (vec= (vec 0.0 0.0 0.0 0.0)
              (alloc-vec)))
  t)

(deftest vecp.1
    (vecp (alloc-vector))
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

(deftest copy-vec.1
    (let* ((orig (vec 1.0 2.0 3.0 4.0))
           (copy (copy-vec orig)))
      (and (not (eq orig copy))
           (vec= orig copy)))
  t)

(deftest %copy-vec.2
    (let* ((orig (vec 1.0 2.0 3.0 4.0))
           (other (vec 0.1 0.2 0.3 0.4))
           (copy (%copy-vec other orig)))
      (and (eq other copy)
           (not (eq orig copy))
           (vec= copy orig)))
  t)

(deftest %copy-vec.3
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
