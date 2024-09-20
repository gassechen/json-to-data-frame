(asdf:defsystem #:data-frame-json
  :description "Un proyecto para manejar data frames y JSON en Common Lisp"
  :version "0.1"
  :author "gassechen@gmail.com"
  :license "MIT"
  :depends-on (:yason
               :dexador
               :str
               :lisp-stat
               :alexandria
               :data-frame) 
  :components ((:file "data-frame-json")))  
