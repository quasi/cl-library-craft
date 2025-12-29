# Fukamachi Style Template

Use for: web applications, ORMs, modern web stack components, high-performance servers.

## Structure

```
{{name}}/
├── {{name}}.asd
├── src/
│   ├── {{name}}.lisp     # Main entry point, re-exports
│   ├── package.lisp      # Package definition
│   ├── specials.lisp     # Special variables
│   ├── conditions.lisp   # Conditions
│   ├── util.lisp         # Utilities
│   └── core/             # Core implementation modules
│       ├── parser.lisp
│       └── ...
├── t/                    # Tests (not test/)
│   ├── package.lisp
│   └── {{name}}.lisp
├── README.markdown       # Note: .markdown not .md
└── LICENSE
```

## {{name}}.asd

```lisp
#|
  {{Description}}

  Author: {{Author}} ({{email}})
|#

(in-package :cl-user)
(defpackage {{name}}-asd
  (:use :cl :asdf))
(in-package :{{name}}-asd)

(defsystem "{{name}}"
  :version "0.1.0"
  :author "{{Author}}"
  :license "{{License}}"  ; Often "MIT" or "BSD 3-Clause"
  :description "{{Description}}"
  :depends-on (:alexandria
               :trivia              ; Pattern matching
               :iterate             ; Loop alternative
               :split-sequence
               ;; Add as needed:
               ;; :cl-ppcre
               ;; :bordeaux-threads
               ;; :local-time
               )
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "specials" :depends-on ("package"))
                 (:file "conditions" :depends-on ("package"))
                 (:file "util" :depends-on ("package"))
                 (:file "core" :depends-on ("package" "specials" "util"))
                 (:file "{{name}}" :depends-on ("package" "core")))))
  :in-order-to ((test-op (test-op "{{name}}/test"))))

(defsystem "{{name}}/test"
  :author "{{Author}}"
  :license "{{License}}"
  :depends-on ("{{name}}"
               "rove")  ; Or "fiveam"
  :components ((:module "t"
                :components
                ((:file "package")
                 (:file "{{name}}"))))
  :perform (test-op (o c)
             (symbol-call :rove :run c)))
```

## src/package.lisp

```lisp
(in-package :cl-user)

(defpackage :{{name}}
  (:use :cl)
  (:import-from :alexandria
                :if-let
                :when-let
                :with-gensyms
                :once-only
                :ensure-list)
  (:import-from :trivia
                :match
                :ematch)
  (:export
   ;; Main API
   #:create
   #:find-by
   #:delete
   
   ;; Classes
   #:{{name}}-object
   
   ;; Accessors
   #:object-id
   #:object-name
   
   ;; Conditions
   #:{{name}}-error
   
   ;; Configuration
   #:*connection*
   #:with-connection))
```

## src/specials.lisp

```lisp
(in-package :{{name}})

(defvar *connection* nil
  "Current connection object.
Bind this or use WITH-CONNECTION.")

(defvar *debug-mode* nil
  "When non-NIL, print debug information.")

(defparameter *default-timeout* 30
  "Default timeout in seconds.")
```

## src/conditions.lisp

```lisp
(in-package :{{name}})

(define-condition {{name}}-error (error)
  ((message :initarg :message
            :initform nil
            :reader {{name}}-error-message))
  (:report (lambda (c s)
             (format s "{{Name}} error~@[: ~A~]"
                     ({{name}}-error-message c)))))

(define-condition connection-error ({{name}}-error)
  ((host :initarg :host :reader connection-error-host)
   (port :initarg :port :reader connection-error-port))
  (:report (lambda (c s)
             (format s "Failed to connect to ~A:~A~@[: ~A~]"
                     (connection-error-host c)
                     (connection-error-port c)
                     ({{name}}-error-message c)))))

(define-condition not-found-error ({{name}}-error)
  ((type :initarg :type :reader not-found-error-type)
   (id :initarg :id :reader not-found-error-id))
  (:report (lambda (c s)
             (format s "~A with id ~A not found"
                     (not-found-error-type c)
                     (not-found-error-id c)))))
```

## src/util.lisp

```lisp
(in-package :{{name}})

(defun symbolize (string)
  "Convert STRING to a keyword symbol."
  (intern (string-upcase string) :keyword))

(defmacro with-connection ((connection) &body body)
  "Execute BODY with *CONNECTION* bound to CONNECTION."
  `(let ((*connection* ,connection))
     ,@body))

(defun ensure-connection ()
  "Signal error if *CONNECTION* is not bound."
  (unless *connection*
    (error '{{name}}-error
           :message "No connection. Use WITH-CONNECTION.")))
```

## src/core.lisp

```lisp
(in-package :{{name}})

;;;; Classes

(defclass {{name}}-object ()
  ((id :initarg :id
       :accessor object-id
       :documentation "Unique identifier")
   (name :initarg :name
         :accessor object-name
         :documentation "Human-readable name")
   (created-at :initarg :created-at
               :accessor object-created-at
               :initform (get-universal-time)))
  (:documentation "Base class for {{name}} objects."))

(defmethod print-object ((obj {{name}}-object) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A" (object-name obj))))

;;;; Core Operations

(defgeneric create (class &rest initargs)
  (:documentation "Create a new instance of CLASS."))

(defmethod create ((class (eql '{{name}}-object)) &rest initargs)
  (ensure-connection)
  (apply #'make-instance class initargs))

(defgeneric find-by (class &key id name)
  (:documentation "Find instances of CLASS by ID or NAME."))

(defmethod find-by ((class (eql '{{name}}-object)) &key id name)
  (ensure-connection)
  ;; Implementation
  (match (list id name)
    ((list (some id) _) (find-by-id class id))
    ((list _ (some name)) (find-by-name class name))
    (_ (error '{{name}}-error :message "Specify :id or :name"))))

(defun find-by-id (class id)
  "Find by ID. Internal."
  (declare (ignore class id))
  ;; Implementation
  nil)

(defun find-by-name (class name)
  "Find by name. Internal."
  (declare (ignore class name))
  ;; Implementation
  nil)
```

## src/{{name}}.lisp

```lisp
(in-package :{{name}})

;;;; High-level API
;;;;
;;;; This file provides the main entry points and re-exports.

(defun version ()
  "Return the {{name}} version string."
  (asdf:component-version (asdf:find-system :{{name}})))

;; Re-export from submodules if needed
;; (cl-reexport:reexport-from :{{name}}.submodule)
```

## t/package.lisp

```lisp
(in-package :cl-user)

(defpackage :{{name}}/test
  (:use :cl :{{name}})
  (:import-from :rove
                :deftest
                :ok
                :ng
                :signals))
```

## t/{{name}}.lisp

```lisp
(in-package :{{name}}/test)

(deftest test-create
  (with-connection ((make-test-connection))
    (let ((obj (create '{{name}}-object :name "test")))
      (ok (not (null obj)))
      (ok (equal (object-name obj) "test")))))

(deftest test-find-by-requires-arg
  (with-connection ((make-test-connection))
    (signals {{name}}-error
      (find-by '{{name}}-object))))

(deftest test-version
  (ok (stringp (version))))

;;; Test utilities

(defun make-test-connection ()
  "Create a connection for testing."
  ;; Return mock or test connection
  t)
```

## README.markdown

```markdown
# {{Name}}

[![Quicklisp](http://quickdocs.org/badge/{{name}}.svg)](http://quickdocs.org/{{name}}/)
[![Build Status](https://travis-ci.org/{{username}}/{{name}}.svg?branch=master)](https://travis-ci.org/{{username}}/{{name}})

{{Description}}

## Installation

```lisp
(ql:quickload :{{name}})
```

## Usage

```lisp
(use-package :{{name}})

;; Create a connection
(with-connection ((make-connection :host "localhost"))
  ;; Create objects
  (create '{{name}}-object :name "example")
  
  ;; Find objects
  (find-by '{{name}}-object :name "example"))
```

## API

### `create` class &rest initargs

Create a new instance.

### `find-by` class &key id name

Find instances by ID or name.

### `with-connection` (connection) &body body

Execute body with connection bound.

## Configuration

### `*connection*`

Current connection. Use `with-connection` to bind.

### `*debug-mode*`

Set to `T` for debug output.

## Running Tests

```lisp
(asdf:test-system :{{name}})
```

Or with Rove:

```lisp
(ql:quickload :rove)
(rove:run :{{name}}/test)
```

## Author

* {{Author}} ({{email}})

## License

{{License}}
```
