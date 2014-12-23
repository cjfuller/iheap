(defpackage :iheap
  (:use :cl)
  (:export #:heap
           #:heap-empty?
           #:heap-find-pos
           #:heap-insert
           #:heap-pop
           #:heap-peek
           #:heap-size
           #:heapify
           #:heapsort))

(in-package :iheap)

(defun parent-index (index)
  (floor (/ (1- index) 2)))

(defclass heap (standard-object)
    ((storage :accessor heap-storage
              :initform (fset:empty-seq)
              :initarg :s)
     (comparator :initform #'<
                 :initarg :comp-fn
                 :accessor heap-comp)
     )
  )

(defmethod heap-insert ((h heap) n)
  (let ((newheap
         (make-instance 'heap
                        :s (fset:with-last (heap-storage h) n)
                        :comp-fn (heap-comp h))))
    (heap-bubble newheap (1- (fset:size (heap-storage newheap))))))

(defmethod heap-pop ((h heap))
  (values (fset:first (heap-storage h))
          (heap-sink (let ((last-elt (fset:last (heap-storage h))))
                       (make-instance 'heap
                                      :s (fset:with
                                          (fset:less-last (heap-storage h))
                                          0
                                          last-elt)
                                    :comp-fn (heap-comp h)))
                     0)))

(defmethod heap-swap ((h heap) (i0 number) (i1 number))
  (let ((orig0 (fset:@ (heap-storage h) i0)))
    (make-instance 'heap
                   :s (fset:with
                       (fset:with (heap-storage h) i0 (fset:@ (heap-storage h) i1))
                       i1 orig0)
                   :comp-fn (heap-comp h))
                   ))

(defmethod heap-bubble ((h heap) (index number))
  (let* ((child-index index)
         (parent (parent-index child-index)))
    (if (or (< parent 0)
            (not (funcall (heap-comp h) (fset:@ (heap-storage h) index) (fset:@ (heap-storage h) parent))))
        h
        (heap-bubble (heap-swap h child-index parent) parent)
        )))

(defmethod heap-peek ((h heap))
  (fset:first (heap-storage h)))

(defmethod heap-size ((h heap))
  (fset:size (heap-storage h)))

(defmethod heap-empty? ((h heap))
  (= 0 (heap-size h)))

(defmethod heap-find-pos ((h heap) elt)
  (heap-find-pos-helper h elt 0 nil))

(defmethod heap-find-pos-helper ((h heap) elt start prev-elt)
  (if (heap-empty? h)
      (values start prev-elt)
      (multiple-value-bind (popped newheap)
          (heap-pop h)
        (if (funcall (heap-comp h) popped elt)
            (heap-find-pos-helper newheap elt (1+ start) popped)
            (values start prev-elt)))))

(defmethod heap-sink ((h heap) (index number))
  (let* ((left (1+ (* 2 index)))
         (right (+ 2 (* 2 index))))
    (multiple-value-bind (lesser-child greater-child)
        (cond ((<= (fset:size (heap-storage h)) left)
               (values nil nil))
              ((<= (fset:size (heap-storage h)) right)
               (values left left))
              ((funcall (heap-comp h)
                        (fset:@ (heap-storage h) left)
                        (fset:@ (heap-storage h) right))
               (values left right))
              (t (values right left)))
      (cond ((and lesser-child (funcall (heap-comp h)
                                        (fset:@ (heap-storage h) lesser-child)
                                        (fset:@ (heap-storage h) index)))
             (heap-sink (heap-swap h lesser-child index)
                        lesser-child))
            ((and greater-child (funcall (heap-comp h)
                                         (fset:@ (heap-storage h) greater-child)
                                         (fset:@ (heap-storage h) index)))
             (heap-sink (heap-swap h greater-child index)
                        greater-child))
            (t h)))))

(defun heapify (lst)
  (let ((h (make-instance 'heap)))
    (mapc (lambda (elt)
            (setf h (heap-insert h elt)))
          lst)
    h))

(defun heapsort (lst)
  (let ((h (heapify lst)))
    (mapcar (lambda (elt)
              (declare (ignore elt))
              (multiple-value-bind (fst newheap) (heap-pop h)
                (setf h newheap)
                fst))
            lst)))

;; (defparameter hhh (make-instance 'heap))

;; (print (heap-storage (heap-insert (heap-insert (heap-insert hhh 3) 92) 1)))

;; (let ((lst '(1 57 4 5 194 0 2 3 4 6 9)))
;;   (print lst)
;;   (print (heapsort lst))
;;   (print (multiple-value-list (heap-find-pos (heapify lst) 58)))
;;   )
