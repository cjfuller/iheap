(defpackage :iheap
  (:use :cl)
  (:export #:heap
           #:heap-empty?
           #:heap-find-pos
           #:heap-insert
           #:heap-pop
           #:heap-peek
           #:heap-size
           #:heap->list
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
  ;; Insert an item into the heap using the heap's comparator.
  ;; Args:
  ;;   h: the heap into which to insert the item
  ;;   n: the item to insert.  Must be valid as an argument to the heap's
  ;;      comparator.
  ;; Return: a new heap with the item inserted.
  ;;
  (let ((newheap
         (make-instance 'heap
                        :s (fset:with-last (heap-storage h) n)
                        :comp-fn (heap-comp h))))
    (heap-bubble newheap (1- (fset:size (heap-storage newheap))))))

(defmethod heap-pop ((h heap))
  ;; Remove the first item from the heap.  (E.g. if this is a min-heap using <
  ;; as the comparator, remove the min element.)
  ;; Args:
  ;;   h: the heap from which to remove the item
  ;; Return: two values
  ;;   - the element being removed
  ;;   - a new heap with the element removed
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
  ;; Internal: return a new heap with items at the given indices swapped.
  (let ((orig0 (fset:@ (heap-storage h) i0)))
    (make-instance 'heap
                   :s (fset:with
                       (fset:with (heap-storage h) i0 (fset:@ (heap-storage h) i1))
                       i1 orig0)
                   :comp-fn (heap-comp h))
                   ))

(defmethod heap-bubble ((h heap) (index number))
  ;; Internal: move the item at the specified index up in the heap
  (let* ((child-index index)
         (parent (parent-index child-index)))
    (if (or (< parent 0)
            (not (funcall (heap-comp h) (fset:@ (heap-storage h) index) (fset:@ (heap-storage h) parent))))
        h
        (heap-bubble (heap-swap h child-index parent) parent)
        )))

(defmethod heap-peek ((h heap))
  ;; Peek at, but don't remove the first item in the heap (E.g. if this is a
  ;; min-heap using < as the comparator, peek at the min element.)
  ;; Args:
  ;;   h: the heap at which to peek
  ;; Return: the first item in the heap
  (multiple-value-bind (val _) (fset:first (heap-storage h))
    val))

(defmethod heap-size ((h heap))
  ;; Return the number of items in the specified heap.
  (fset:size (heap-storage h)))

(defmethod heap-empty? ((h heap))
  ;; Predicate: is the heap empty? (i.e. size 0)
  (= 0 (heap-size h)))

(defmethod heap-find-pos ((h heap) elt)
  ;; Find the position at which an item would be inserted into the priority queue represented by this heap.
  ;; Args:
  ;;   h: the heap into which the item would be inserted.
  ;;   elt: the item that would be inserted
  ;; Return: two values
  ;;   - the 0-indexed position at which the element would be inserted.  How
  ;;     equivalent priorities are dealt with is determined by the comparator.
  ;;     This index is the first position, i, at which something like
  ;;     (comparator elt (nth i pqueue)) is nil (assuming nth hypothetically
  ;;     worked for the priority queue like it does for lists).
  ;;   - the element just prior to the insertion point
  (heap-find-pos-helper h elt 0 nil))

(defmethod heap-find-pos-helper ((h heap) elt start prev-elt)
  ;; Internal: helper to find the position at which an item would be inserted.
  (if (heap-empty? h)
      (values start prev-elt)
      (multiple-value-bind (popped newheap)
          (heap-pop h)
        (if (funcall (heap-comp h) popped elt)
            (heap-find-pos-helper newheap elt (1+ start) popped)
            (values start prev-elt)))))

(defmethod heap-sink ((h heap) (index number))
  ;; Internal: move the item at the specified index downward in the heap
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

(defmethod heap->list ((h heap))
  ;; Return a list containing the heap's elements in order.
  (let ((_h h))
    (loop repeat (heap-size h)
       with elt
       do (multiple-value-setq (elt _h) (heap-pop _h))
       collect elt)))

(defun heapify (lst &key (comp nil))
  ;; Return a heap containing the provided list's elements.
  (let ((h (if comp
               (make-instance 'heap :comp-fn comp)
               (make-instance 'heap))))
    (mapc (lambda (elt)
            (setf h (heap-insert h elt)))
          lst)
    h))

(defun heapsort (lst &key comp)
  ;; Sort the elements in the provided list using heapsort.
  ;; Return: a new list containing the sorted elements.
  (heap->list (heapify lst :comp comp)))

;; (defparameter hhh (make-instance 'heap))

;; (print (heap-storage (heap-insert (heap-insert (heap-insert hhh 3) 92) 1)))

;; (let ((lst '(1 57 4 5 194 0 2 3 4 6 9)))
;;   (print lst)
;;   (print (heapsort lst))
;;   (print (multiple-value-list (heap-find-pos (heapify lst) 58)))
;;   )
