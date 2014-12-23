(ql:quickload :iheap)
(ql:quickload :ptester)

(defpackage :iheap-test
  (:use :cl :ptester :iheap)
  (:export #:run-all))

(in-package :iheap-test)

(defmacro ete-test ()
  (test '(0 1 2 3 4 4 5 6 9 57 194)
        (heapsort '(1 57 4 5 194 0 2 3 4 6 9))
        :test #'equal))

(defmacro heap-insert-test ()
  (test '(0 1 2 3)
        (heap->list (heap-insert (heapify '(0 2 3)) 1))
        :test #'equal))

(defmacro heap-pop-test ()
  (test 0 (nth-value 0 (heap-pop (heapify '(2 3 0 1)))))
  (test '(1 2 3)
        (heap->list (nth-value 1 (heap-pop (heapify '(2 3 0 1)))))
        :test #'equal))

(defmacro heap-peek-test ()
  (test 0 (heap-peek (heapify '(2 3 0 1)))))

(defmacro heap-find-pos-test ()
  (test 1 (nth-value 0 (heap-find-pos (heapify '(0 2 3)) 1)))
  (test 0 (nth-value 1 (heap-find-pos (heapify '(0 2 3)) 1))))

(defmacro heap-comparator-test ()
  (test 0 (heap-peek (heapify '(2 3 0 1))))
  (test 3 (heap-peek (heapify '(2 3 0 1) :comp #'>))))

(defmacro run-all ()
  (ete-test)
  (heap-insert-test)
  (heap-pop-test)
  (heap-peek-test)
  (heap-find-pos-test)
  (heap-comparator-test))
