(defpackage :json-to-df/tests/main
  (:use :cl :json-to-df :yason :rove :lisp-stat :data-frame))

(in-package :json-to-df/tests/main)


(defun data-frame-columns (df)
  ;; return list of column names
  (mapcar #'column-name (data-frame-columns-list df)))

(defun data-frame-rows (df)
  ;; return list of rows
  (data-frame-rows-list df))




(defun validate-data-frame (df)
  ;; Check if the data frame is non-nil and has the expected columns
  (and df
       (typep df 'data-frame)
       (equalp (data-frame-columns df) '(:userId :id :title :body))
       (> (data-frame-rows df) 0)))




;; Test 1: Simple JSON from API
(deftest simple-json-from-api-test
  (let ((json-data (call-api *url*)))
    (let ((df (json-to-df:json-to-df json-data)))
      (ok (validate-data-frame df)))))

;; Test 2: Nested JSON from a different API
(deftest nested-json-from-api-test
  (let* ((nested-url "https://jsonplaceholder.typicode.com/comments")
         (json-data (call-api nested-url)))
    (let ((df (json-to-df:json-to-df json-data)))
      (ok (validate-data-frame df)))))

;; Test 3: JSON with arrays (e.g., handling multiple objects)
(deftest json-with-arrays-from-api-test
  (let* ((array-url "https://jsonplaceholder.typicode.com/albums")
         (json-data (call-api array-url)))
    (let ((df (json-to-df:json-to-df json-data)))
      (ok (validate-data-frame df)))))
