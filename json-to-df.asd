(defsystem "json-to-df"
  :version "0.0.1"
  :author "gassechen@gmail.com"
  :license "MIT"
  :depends-on (:yason
               :dexador
               :str
               :lisp-stat)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "json-to-df/tests"))))

(defsystem "json-to-df/tests"
  :author ""
  :license ""
  :depends-on ("json-to-df"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for json-to-df"
  :perform (test-op (op c) (symbol-call :rove :run c)))




