(defpackage json-to-df
  (:use :cl)
  (:export #:json-to-df
	   #:get-from-url
	   #:get-from-file
	   #:dump-db))

(in-package :json-to-df)

(defvar *db* nil)
(defvar *data* nil)

(defparameter a "áéíóúüñ")
(defparameter b "aeiouun")

(setf data-frame:*ask-on-redefine* nil)

(defun dump-db (db)
  (format t "~{~{~a:~10t~a~%~}~%~}" db))


(defun save-db (filename db)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print db out))))


(defun load-db (filename db)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf db (read in)))))


(defun select-db (selector-fn db)
  (remove-if-not selector-fn db ))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
    (loop while fields
	  collecting (make-comparison-expr (pop fields) (pop fields))))


(defun make-comparison-expr* (field value)
  (if (string= value "*")
      `(not (null (getf cd ,field)))
      `(equal (getf cd ,field) ,value)))


(defmacro where-db (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

(defun select-column-values (key alist)
  (mapcar (lambda (pair)
            (getf pair key))
          alist))


(defun extract-keys-columns (data)
  (let ((flat-data (alexandria:flatten data)))
       (remove-duplicates(remove-if-not #'symbolp flat-data))))



(defun make-data-frame (key-list values)
  (let ((na-value :na))  ; Definir el valor de reemplazo para NIL
    (reverse-df(data-frame:make-df key-list
                       (mapcar (lambda (key)
                                 (let ((column-values (select-column-values key values)))
                                   ;; Reemplazar NIL por :na en la lista de valores
                                   (setq column-values (mapcar (lambda (value)
                                                                (if (null value) na-value value))
                                                              column-values))
                                   (coerce column-values 'vector)))
                               key-list)))))


(defun process-hash-table (hash-table &optional key-prefix tmp-list)
  
  (maphash (lambda (key val)
	     ;;(print (clean-string (remove-whitespace key)))
             (let* ((value (boolean-to-sqlite val))
		    (keyc (sanitize-column-names key))
		   (full-key
                     (if key-prefix (concatenate 'string key-prefix "_" keyc) keyc)))
               (if (typep value 'hash-table)
		   
		   (setf tmp-list (process-hash-table value full-key tmp-list)))
	       
	       (progn
		 (when (typep value 'list)
		   (let ((counter-value 0))
		     (dolist (l value)
		       
		       (push  l tmp-list)
		       (push (alexandria:make-keyword
			      (string-upcase
			       (concatenate 'string full-key "_"
					    (write-to-string (incf counter-value)))))
			     tmp-list))))
		 (unless (or
			  (typep value 'hash-table)
			  (typep value 'list))
		   (push value tmp-list)
                   (push (alexandria:make-keyword
			  (string-upcase full-key))
			 tmp-list)
                   ))))
	   
           hash-table)
  tmp-list)


(defun boolean-to-sqlite (value)
 ;; (print value)
  (cond ((equal value'YASON:TRUE ) 1)
        ((equal value 'YASON:FALSE) 0)
	(t value)))


(defun sanitize-column-names (input-string)
  (remove-accents (str:camel-case input-string)))


(defun remove-whitespace (input-string)
  "Elimina todos los espacios en blanco de la cadena."
  (coerce (remove-if #'(lambda (char) (char= char #\Space)) input-string) 'string))


(defun clean-string (input-string)
  "Elimina espacios en blanco y reemplaza caracteres no alfanuméricos con _."
  (with-output-to-string (*standard-output*)
    (loop for char across input-string
          do (princ (if (or (alphanumericp char) (char= char #\_))
                        char
                        #\_)))))


(defun remove-underscore (input-string)
  "Elimina todos los espacios en blanco de la cadena."
  (coerce (remove-if #'(lambda (char) (char= char #\_)) input-string) 'string))


(defun get-hash-table-list (key)
  (let ((hash-table (gethash key *data*)))
    (cdr hash-table)))

(defun process-hash-table-list (hash-table-list)
  (let ((tmp-list '()))
    (dolist (hash-table hash-table-list)
       (setq *db*
	    (append *db*
		    (list (process-hash-table hash-table tmp-list)))))))


(defun get-keys-as-list (hash-table)
  (let ((keys '()))
    (maphash (lambda (key value)
	       (push key keys))
             hash-table)
    keys))



(defun check-structure-type (data)
  (cond ((consp data)  
         (make-data-structure-correct data))
        ((and (stringp data)  
              (every #'characterp data))  
         (make-data-structure-correct (parse-json-string data)))
        ((hash-table-p data)  
            (h-table data))
        (t  
         data)))


  (defun make-data-structure-correct(data)
    (let ((table (make-hash-table :test 'equal)))
    (setf (gethash "data" table) data)
    table))


(defun parse-json-string (json-string)
  "Parses a JSON string into a Lisp structure using YASON."
  (yason:parse (make-string-input-stream json-string)))



(defun h-table (data)
  (let* ((entries (extract-entries data))
         (processed-data (make-data-structure-correct (mapcar #'cdr entries))))
    (if (> (hash-table-count data) 1)
        processed-data
        (gethash "data" processed-data))))



;; (defun h-table (data)
;;   (when (> (hash-table-count data) 1)
;;     (let ((entries (extract-entries data)))
;;       (mapcar #'cdr entries))))
      

  

(defun json-to-df (data &optional (df-name "DF"))
  ;; Ensure session is not NIL
  ;; Verify and set the structure of the data
  (setf *data* (check-structure-type data))

  ;; Initialize the database
  (setf *db* nil)

  ;; Extract keys from data and process them
  (let* ((keys-list (get-keys-as-list *data*)))
    (dolist (key keys-list)
      (let* ((hash-table-list (get-hash-table-list key)))
        (process-hash-table-list hash-table-list)))

    ;; Define the dataframe using lisp-stat:defdf
    (eval  `(lisp-stat:defdf ,(intern (str:upcase df-name))
		,(make-data-frame (extract-keys-columns *db*) *db*)))))


  ;; Apply heuristic type inference to the dataframe
  ;;(eval `(lisp-stat:heuristicate-types ,(get-session-data *session* 'myproject::df-name)))))




(defun reverse-df (df)
  "Return DF with columns in reverse order"
  (lisp-stat:make-df (reverse (lisp-stat:keys df)) (reverse (lisp-stat:columns df))))


(defun extract-entries (hash-table)
  (let ((entries '()))
    (maphash (lambda (key value)
               (push (cons key value) entries))
             hash-table)
    entries))



(defun remove-accents (str)
  (let* ((trans (make-hash-table :test 'equal))
         (chars (coerce a 'list))
         (replacements (coerce b 'list)))
    (loop for char in chars
          for replacement in replacements
          do (setf (gethash char trans) replacement))
    (coerce (loop for char across str
                  collect (gethash char trans char)) 
            'string)))


(defun call-api (url-get)
  (let* ((yason:*parse-json-booleans-as-symbols* t)
         (yason:*parse-json-arrays-as-vectors* nil)
         (respuesta
           (yason:parse
            (dex:get url-get
                     :keep-alive t
                     :use-connection-pool t
                     :connect-timeout 60
                     :want-stream t))))
    respuesta))

(defun get-from-url (url-get &optional (df-name "DF") (key nil))
  "Obtiene los datos desde la URL y, si `key` no es nil, busca el valor asociado con esa clave.
   Si `key` es nil, pasa directamente la respuesta JSON completa a `json-to-df`."
  (let ((response (call-api url-get)))
    (if key
        (json-to-df (gethash key response) df-name)
        (json-to-df response df-name))))



(defun read-json-file (file-path)
  "Lee un archivo JSON y lo parsea."
  (let* ((yason:*parse-json-booleans-as-symbols* t)
         (yason:*parse-json-arrays-as-vectors* nil)
         (json-content (uiop:read-file-string file-path)))
    (yason:parse json-content)))

(defun get-from-file (file-path &optional (df-name "DF") (key nil))
  "Obtiene los datos desde un archivo JSON y, si `key` no es nil, busca el valor asociado con esa clave.
   Si `key` es nil, pasa directamente la respuesta JSON completa a `json-to-df`."
  (let ((response (read-json-file file-path)))
    (if key
        (json-to-df (gethash key response) df-name)
        (json-to-df response df-name))))






