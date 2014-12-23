### iheap: immutable, functional binary heaps (and priority queues)

#### Creating heaps:

- Use CLOS to make a new heap object: `(make-instance 'heap)`

  Optional parameters:
  - `:comp-fn`, a function used for ordering the heap.  Defaults to #'< (i.e. min-heaps, priority-queues sorted in ascending order).
  - `:s`, the internal storage for the heap.  (Don't supply this parameter for normal usage; just use the default.)  It's expected that this is an fset:seq.

- `heapify`: shortcut to make a heap from the elements of a list (use the optional `:comp` kwarg to supply a comparator for the heap)

#### Heapsort:

`(heapsort lst)`: takes a list, gives back a sorted list.  Optionally, use the kwarg `:comp` to supply a comparator for the sort.

`(heap->list h)`: takes a heap, gives back a sorted list.

#### Adding elements to a heap:

`(heap-insert h el)` adds element `el` to heap `h`.  `el` must be a valid argument to the heap's comparator.  Returns a new heap with the element added.

#### Removing elements from a heap:

`(heap-pop h)` removes the first element from the heap.  Returns `(values el new-h)` where el is the element removed, and new-h is a new heap with that element removed.

#### Examining a heap:

`(heap-peek h)` returns the first element of a heap without returning it.

`(heap-find-pos h elt)` finds the position where `elt` would be inserted into heap `h`.  Returns `(values index prev-elt)` where `index` is the position where the insertion would happen and `prev-elt` is the element just before the insertion point.  (Note that this is not a particularly efficient search.)

`(heap-empty? h)` check if the provided heap is completely empty.

`(heap-size h)` get the number of elements in the provided heap.
