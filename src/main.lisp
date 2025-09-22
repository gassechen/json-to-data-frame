(defpackage json-to-df
  (:use :cl :data-frame :alexandria)
  (:export #:json-to-df
           #:get-from-url
           #:get-from-file
           #:dump-db))

(in-package :json-to-df)

;; Variables globales para el estado del programa
(defvar *db* nil)
(defvar *data* nil)

;; Parámetros para la limpieza de cadenas
(defparameter a "áéíóúüñ")
(defparameter b "aeiouun")
(setf data-frame:*ask-on-redefine* nil)

;; Funciones de limpieza y utilidad
(defun dump-db (db)
  (format t "~{~{~a:~10t~a~%~}~%~}" db))

(defun boolean-to-sqlite (value)
  (cond ((equal value'YASON:TRUE ) 1)
        ((equal value 'YASON:FALSE) 0)
        (t value)))

(defun sanitize-column-names (input-string)
  (remove-accents (str:camel-case input-string)))

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

(defun get-keys-as-list (hash-table)
  "Obtiene una lista de las claves de un hash-table."
  (let ((keys '()))
    (maphash (lambda (key value)
               (declare (ignore value))
               (push key keys))
             hash-table)
    keys))

(defun select-column-values (key alist)
  "Extrae los valores de una clave de una lista de alistas."
  (mapcar (lambda (pair)
            (getf pair key))
          alist))

(defun extract-keys-columns (data)
  "Extrae las claves de las listas de propiedades (alists) para definir las columnas."
  (let ((flat-data (alexandria:flatten data)))
    (remove-duplicates (remove-if-not #'symbolp flat-data))))

(defun reverse-df (df)
  "Invierte el orden de las columnas de un data frame."
  (lisp-stat:make-df (reverse (lisp-stat:keys df)) (reverse (lisp-stat:columns df))))

(defun extract-entries (hash-table)
  (let ((entries '()))
    (maphash (lambda (key value)
               (push (cons key value) entries))
             hash-table)
    entries))

;; --- Funciones para el nuevo aplanamiento recursivo ---

(defun flatten-hash-table (hash-table &optional prefix)
  "Recorre un hash-table de forma recursiva y lo aplana en una alist (lista de propiedades)."
  (let ((alist '()))
    (maphash (lambda (key val)
               (let* ((value (boolean-to-sqlite val))
                      (keyc (sanitize-column-names key))
                      (full-key (if prefix
                                    (concatenate 'string prefix "_" keyc)
                                    keyc)))
                 (cond ((typep value 'hash-table)
                        ;; Si el valor es otro hash-table, lo aplana de forma recursiva
                        (setf alist (append alist (flatten-hash-table value full-key))))
                       ((listp value)
                        ;; Si es una lista, la procesa como un array de valores o de objetos
                        (setf alist (append alist (flatten-list-of-values value full-key))))
                       (t
                        ;; Si es un valor simple, lo añade a la alist aplanada
                        (push (cons (alexandria:make-keyword (string-upcase full-key))
                                    value)
                              alist)))))
             hash-table)
    alist))

(defun flatten-list-of-values (list-of-values &optional prefix)
  "Recorre una lista de valores y los aplana. Si son objetos, los aplana de forma recursiva."
  (let ((alist '()))
    (loop for item in list-of-values
          for i from 0
          do (let* ((full-key (concatenate 'string prefix "_" (princ-to-string i))))
               (cond ((typep item 'hash-table)
                      (setf alist (append alist (flatten-hash-table item full-key))))
                     ((listp item)
                      (setf alist (append alist (flatten-list-of-values item full-key))))
                     (t
                      (push (cons (alexandria:make-keyword (string-upcase full-key))
                                  item)
                            alist)))))
    alist))


;; --- Funciones principales ---

(defun json-to-df (data &optional (df-name "DF"))
  "Convierte datos JSON en un data frame aplanado."
  (let ((yason:*parse-json-booleans-as-symbols* t)
        (yason:*parse-json-arrays-as-vectors* nil)
        (records-list '()))
    (setf *data* data)
    ;; Asume que la mayoría de los JSONs son una lista de objetos en la raíz
    (if (listp *data*)
        (dolist (item *data*)
          (if (hash-table-p item)
              (push (flatten-hash-table item) records-list)
              ;; Maneja arrays de valores simples
              (push (list (cons :data item)) records-list)))
        ;; Si es un solo objeto o valor, lo convierte en una lista para procesarlo
        (push (flatten-hash-table (if (hash-table-p *data*) *data* (list (cons "data" *data*))))
              records-list))
    (setf records-list (reverse records-list))
    (eval `(lisp-stat:defdf ,(intern (str:upcase df-name))
             ,(make-data-frame (extract-keys-columns records-list) records-list)))))

(defun make-data-frame (key-list values)
  "Crea el data frame final a partir de las claves y los valores aplanados."
  (let ((na-value :na))
    (data-frame:make-df key-list
                        (mapcar (lambda (key)
                                  (let ((column-values (select-column-values key values)))
                                    (setq column-values (mapcar (lambda (value)
                                                                  (if (null value) na-value value))
                                                                column-values))
                                    (coerce column-values 'vector)))
                                key-list))))


(defun call-api (url-get)
  "Llama a una URL y parsea la respuesta JSON."
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
  "Obtiene los datos desde la URL y los convierte en un data frame."
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
  "Obtiene los datos desde un archivo JSON y los convierte en un data frame."
  (let ((response (read-json-file file-path)))
    (if key
        (json-to-df (gethash key response) df-name)
        (json-to-df response df-name))))






