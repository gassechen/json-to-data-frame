(defsystem "json-to-df"
  :version "0.0.1"
  :author "gassechen@gmail.com"
  :license "MIT"
  :depends-on (:yason
               :dexador
               :str
               :lisp-stat
               :data-frame
               #+sbcl "sb-cltl2")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "json-to-df/tests"))))

(defsystem "json-to-df/tests"
  :author "gassechen@gmail.com"
  :license "MIT"
  :depends-on ("json-to-df"
               "clunit2")
  :serial t
  :pathname "tests/"
  :components ((:file "main"))
  :description "Test system for json-to-df"
  :perform (test-op (o s)
                    (let ((*print-pretty* t)) ; work around clunit issue #9
                      (symbol-call :clunit :run-suite
                                   (find-symbol* :json-to-df-suite
                                                 :json-to-df-tests)
                                   :use-debugger nil))))
