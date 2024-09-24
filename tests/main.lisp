(uiop:define-package #:json-to-df-tests
  (:use
   #:cl
   #:alexandria
   #:anaphora
   #:clunit
   #:let-plus
   #:select
   #:data-frame
   #:json-to-df)
  (:import-from #:nu #:as-alist #:as-plist)
  (:export #:run))

(in-package :json-to-df-tests)


(defsuite json-to-df-suite ())

;;simple
(deftest test-json-placeholder (json-to-df-suite)
  ;; Llamar a la API y obtener los datos en formato JSON
  (let* ((json-data (get-from-url "https://jsonplaceholder.typicode.com/posts" "posts"))
         ;; Convertir los datos JSON a un dataframe
         )
    
    ;; Verificar que el dataframe fue creado correctamente
    (assert (typep json-data 'data-frame))))


;;complex
(deftest test-json-users (json-to-df-suite)
  ;; Llamar a la API y obtener los datos en formato JSON
  (let* ((json-data (get-from-url "https://jsonplaceholder.typicode.com/users" "usersDF"))
         ;; Convertir los datos JSON a un dataframe
         )
    
    ;; Verificar que el dataframe fue creado correctamente
    (assert (typep json-data 'data-frame))))


;; with countries
(deftest test-json-countries (json-to-df-suite)
  ;; Llamar a la API y obtener los datos en formato JSON
  (let* ((json-data (get-from-url "https://restcountries.com/v3.1/all" "countriesDF"))
         ;; Convertir los datos JSON a un dataframe
         )
    
    ;; Verificar que el dataframe fue creado correctamente
    (assert (typep json-data 'data-frame))))



;; with key
(deftest test-json-planets (json-to-df-suite)
  ;; Llamar a la API y obtener los datos en formato JSON
  (let* ((json-data (get-from-url "https://www.swapi.tech/api/planets/" "planetsDF" "results"))
         ;; Convertir los datos JSON a un dataframe
         )
    
    ;; Verificar que el dataframe fue creado correctamente
    (assert (typep json-data 'data-frame))))







;; Ejecutar las pruebas
(defun run (&optional interactive?)
  (run-suite 'json-to-df-suite :use-debugger interactive?))


