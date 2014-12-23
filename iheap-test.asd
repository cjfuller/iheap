(asdf:defsystem iheap-test
    :description "Tests for iheap."
    :version "0.0.1"
    :license "MIT"
    :depends-on (:iheap :ptester)
    :components
    ((:module "test"
              :components
              ((:file "heap-test")))))
