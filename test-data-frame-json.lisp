(defpackage :test-data-frame-json
  (:use :cl :fiveam :data-frame-json :yason))

(in-package :test-data-frame-json)

(defun run-tests ()
  (fiveam:run!))

(def-suite dataframe-tests
  :description "Pruebas para la función make-dataframe en el paquete data-frame-json")

(test test-make-dataframe-valid-json
  (let* ((json-string "{\"posts\":[{\"userId\":1,\"id\":1,\"title\":\"Title 1\",\"body\":\"Body 1\"},{\"userId\":1,\"id\":2,\"title\":\"Title 2\",\"body\":\"Body 2\"}]}")
         (expected-keys '(:userId :id :title :body))
         (data (yason:parse json-string)))
    (make-dataframe data)
    (let ((df *db*))
      (assert (equal (extract-keys-columns df) expected-keys) nil "Las claves del dataframe no son las esperadas."))))

(test test-make-dataframe-empty-json
  (let* ((json-string "{\"posts\":[]}")
         (data (yason:parse json-string)))
    (make-dataframe data)
    (let ((df *db*))
      (assert (null df) nil "El dataframe no debería contener datos."))))

(test test-make-dataframe-invalid-json
  (let* ((json-string "invalid json")
         (data (ignore-errors (yason:parse json-string))))
    (assert (null data) nil "El JSON debería ser inválido y no debería ser procesado.")))

;; Ejecutar las pruebas
(run-tests)
