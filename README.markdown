Certainly! Below is a formatted version of the README content for your GitHub repository, which includes the example usage of the `json-to-df` package:

---

# JSON to Data Frame

This repository provides a Common Lisp library to convert JSON data into a data frame using the `json-to-df` package. The package leverages the `yason` library for JSON parsing and `dfio` for data frame operations.

## Installation

1. Clone the repository into your Quicklisp local-projects directory:
   ```sh
   git clone https://github.com/gassechen/json-to-data-frame.git ~/quicklisp/local-projects
   ```

2. Load the package in your REPL:
   ```lisp
   (ql:quickload :json-to-df)
   ```

3. Switch to the `json-to-df` package:
   ```lisp
   (in-package :json-to-df)
   ```

## Example Usage

### Step 1: Define the URL for the JSON API

```lisp
(defparameter *url* "https://jsonplaceholder.typicode.com/posts")
```

### Step 2: Define a function to call the API and parse the JSON response

```lisp
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
```

### Step 3: Convert the JSON response to a data frame

```lisp
(json-to-df (call-api *url*))
```

You will be prompted to select a symbol to be made accessible in the `DFIO` package:

```lisp
Select a symbol to be made accessible in package DFIO:
  1. DATA-FRAME::BODY
  2. DFIO::BODY

Enter an integer (between 1 and 2): 1
```

### Step 4: Display the data frames

```lisp
(lisp-stat:show-data-frames)
```

### Step 5: Assign the data frame to a variable and print it

```lisp
(json-to-df (call-api *url*) "my-df")
(lisp-stat:show-data-frames)
```

### Step 6: Print the data frame

```lisp
(df:print-data my-df)
```

## Output

The `df:print-data` function will print the contents of the data frame `my-df`:

```lisp
JSON-TO-DF> (df:print-data my-df)

;; Output will display the data frame contents here
```

# New function 
```lisp

(defun get-from-url (url-get &optional (df-name "DF") (key nil))

;;with name df
JSON-TO-DF> (get-from-url "https://jsonplaceholder.typicode.com/users" "usersDF")
#<DATA-FRAME:DATA-FRAME (9 observations of 15 variables)>

;;with key

JSON-TO-DF> (get-from-url "https://www.swapi.tech/api/planets/" "planetsDF" "results")
#<DATA-FRAME:DATA-FRAME (9 observations of 3 variables)>

```
# New function 
```lisp

(defun get-from-file (file-path &optional (df-name "DF") (key nil))

JSON-TO-DF> (get-from-file "../tests/files/simpleobj.json" "filesimple")
#<DATA-FRAME:DATA-FRAME (2 observations of 3 variables)>

JSON-TO-DF> (get-from-file "../tests/files/multiplekesy.json" "multiplekey"  "empleados")
#<DATA-FRAME:DATA-FRAME (1 observations of 8 variables)>


```
# TEST
JSON-TO-DF> (asdf:test-system 'json-to-df)

PROGRESS:
=========

    JSON-TO-DF-SUITE: (Test Suite)
        TEST-JSON-COMPLEXOBJ: E
        TEST-JSON-SIMPLEOBJ: 
        TEST-JSON-MULTIPLEKEY: 
        TEST-JSON-NESTEDFILEANDARRAY: 
        TEST-JSON-NESTEDFILE: 
        TEST-JSON-SIMPLEFILE: 
        TEST-JSON-PLANETS: 
        TEST-JSON-COUNTRIES: 
        TEST-JSON-USERS: 
        TEST-JSON-PLACEHOLDER: 

FAILURE DETAILS:
================

    JSON-TO-DF-SUITE: (Test Suite)
        TEST-JSON-COMPLEXOBJ: The value
  (#<HASH-TABLE :TEST EQUAL :COUNT 3 {1015B12FB3}>)
is not of type
  HASH-TABLE


SUMMARY:
========
    Test functions:
        Executed: 10
        Skipped:  0

    Tested 1 assertion.
        Errors: 1/1 all tests had errors
T



---

This README provides a clear and concise guide on how to use the `json-to-df` package to convert JSON data into a data frame and display it.
