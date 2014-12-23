(asdf:defsystem iheap
    :description "Immutable heaps / priority queues."
    :version "0.0.1"
    :license "MIT"
    :depends-on (:fset)
    :components
    ((:module "src"
              :components
              ((:file "heap")))))
