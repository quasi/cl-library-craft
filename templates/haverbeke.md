# Haverbeke Style Template

Use for: database interfaces, DSLs, layered systems where components can be used independently.

## Structure

```
{{name}}/
├── {{name}}.asd              # High-level system
├── {{name}}-core.asd         # Core/low-level system
├── {{name}}-extra.asd        # Optional extensions
├── {{name}}/                 # High-level module
│   ├── package.lisp
│   ├── config.lisp
│   └── api.lisp
├── {{name}}-core/            # Core module
│   ├── package.lisp
│   ├── protocol.lisp
│   └── implementation.lisp
├── {{name}}-extra/           # Extensions module
│   ├── package.lisp
│   └── extensions.lisp
├── doc/
│   ├── {{name}}.html
│   └── {{name}}-core.html
├── README.md
└── LICENSE
```

## {{name}}-core.asd (Low-level, standalone)

```lisp
(defsystem "{{name}}-core"
  :description "Low-level {{name}} implementation"
  :author "{{author}}"
  :license "{{license}}"
  :version "0.1.0"
  :depends-on (:alexandria)  ; Minimal deps for core
  :components
  ((:module "{{name}}-core"
    :components
    ((:file "package")
     (:file "protocol" :depends-on ("package"))
     (:file "implementation" :depends-on ("package" "protocol")))))
  :in-order-to ((test-op (test-op "{{name}}-core/test"))))

(defsystem "{{name}}-core/test"
  :depends-on ("{{name}}-core" "fiveam")
  :components
  ((:module "{{name}}-core/test"
    :components ((:file "tests"))))
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :{{name}}-core-tests)))
```

## {{name}}.asd (High-level, uses core)

```lisp
(defsystem "{{name}}"
  :description "High-level {{name}} interface"
  :author "{{author}}"
  :license "{{license}}"
  :version "0.1.0"
  :depends-on ("{{name}}-core"
               :alexandria
               :closer-mop)  ; Additional deps for high-level
  :components
  ((:module "{{name}}"
    :components
    ((:file "package")
     (:file "config" :depends-on ("package"))
     (:file "api" :depends-on ("package" "config")))))
  :in-order-to ((test-op (test-op "{{name}}/test"))))

(defsystem "{{name}}/test"
  :depends-on ("{{name}}" "{{name}}-core/test" "fiveam")
  :components
  ((:module "{{name}}/test"
    :components ((:file "tests"))))
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :{{name}}-tests)))
```

## {{name}}-extra.asd (Optional extensions)

```lisp
(defsystem "{{name}}-extra"
  :description "Optional extensions for {{name}}"
  :author "{{author}}"
  :license "{{license}}"
  :version "0.1.0"
  :depends-on ("{{name}}"
               :local-time)  ; Extra dependencies
  :components
  ((:module "{{name}}-extra"
    :components
    ((:file "package")
     (:file "extensions" :depends-on ("package"))))))
```

## {{name}}-core/package.lisp

```lisp
(defpackage :{{name}}-core
  (:use :cl)
  (:export
   ;; Low-level protocol
   #:connection
   #:open-connection
   #:close-connection
   #:execute-raw
   
   ;; Protocol generics
   #:send-message
   #:receive-message
   
   ;; Conditions
   #:{{name}}-core-error
   #:connection-error
   #:protocol-error))
```

## {{name}}-core/protocol.lisp

```lisp
(in-package :{{name}}-core)

;;;; Connection class

(defclass connection ()
  ((host :initarg :host :reader connection-host)
   (port :initarg :port :reader connection-port)
   (stream :initarg :stream :accessor connection-stream))
  (:documentation "Represents a connection to the server."))

;;;; Protocol generics

(defgeneric send-message (connection message)
  (:documentation "Send MESSAGE over CONNECTION."))

(defgeneric receive-message (connection)
  (:documentation "Receive a message from CONNECTION."))

;;;; Public API

(defun open-connection (host port)
  "Open a connection to HOST:PORT.
Returns a CONNECTION object."
  (let ((stream (open-socket host port)))
    (make-instance 'connection
                   :host host
                   :port port
                   :stream stream)))

(defun close-connection (connection)
  "Close CONNECTION."
  (when (connection-stream connection)
    (close (connection-stream connection))
    (setf (connection-stream connection) nil)))

(defun execute-raw (connection command)
  "Execute raw COMMAND on CONNECTION.
Returns the raw response."
  (send-message connection command)
  (receive-message connection))

;;;; Internal

(defun open-socket (host port)
  "Open a socket. Internal."
  ;; Implementation using usocket or sb-bsd-sockets
  (declare (ignore host port))
  nil)
```

## {{name}}/package.lisp

```lisp
(defpackage :{{name}}
  (:use :cl :{{name}}-core)
  (:export
   ;; Re-export core API
   #:connection
   #:open-connection
   #:close-connection
   
   ;; High-level API
   #:with-connection
   #:query
   #:execute
   
   ;; Object mapping
   #:define-mapping
   #:find-object
   #:save-object
   
   ;; Configuration
   #:*default-host*
   #:*default-port*))
```

## {{name}}/config.lisp

```lisp
(in-package :{{name}})

(defvar *default-host* "localhost"
  "Default server host.")

(defvar *default-port* 5000
  "Default server port.")

(defvar *current-connection* nil
  "Current connection for WITH-CONNECTION.")
```

## {{name}}/api.lisp

```lisp
(in-package :{{name}})

;;;; Connection management

(defmacro with-connection ((&key (host '*default-host*)
                                 (port '*default-port*))
                           &body body)
  "Execute BODY with a connection to HOST:PORT."
  `(let ((*current-connection* (open-connection ,host ,port)))
     (unwind-protect
          (progn ,@body)
       (close-connection *current-connection*))))

;;;; High-level queries

(defun query (sql &rest args)
  "Execute SQL query with ARGS.
Returns results as a list of plists."
  (let ((command (apply #'format nil sql args)))
    (parse-response (execute-raw *current-connection* command))))

(defun execute (sql &rest args)
  "Execute SQL statement with ARGS.
Returns number of affected rows."
  (let ((result (apply #'query sql args)))
    (getf result :affected-rows 0)))

;;;; Object mapping

(defmacro define-mapping (name table &body slot-specs)
  "Define a mapping between class NAME and TABLE."
  `(progn
     (defclass ,name ()
       ,(loop for (slot-name column . options) in slot-specs
              collect `(,slot-name :initarg ,(intern (symbol-name slot-name) :keyword)
                                   :accessor ,slot-name
                                   ,@options)))
     (setf (get ',name 'table-name) ',table)
     (setf (get ',name 'column-map)
           ',(loop for (slot-name column . _) in slot-specs
                   collect (cons slot-name column)))
     ',name))

(defun find-object (class &rest keys)
  "Find object of CLASS by KEYS."
  ;; Implementation
  (declare (ignore class keys))
  nil)

(defun save-object (object)
  "Save OBJECT to database."
  ;; Implementation
  (declare (ignore object))
  nil)

;;;; Internal

(defun parse-response (raw-response)
  "Parse RAW-RESPONSE into Lisp data. Internal."
  ;; Implementation
  raw-response)
```

## Usage Examples

```lisp
;;; Using just the core (low-level)
(ql:quickload :{{name}}-core)

(let ((conn ({{name}}-core:open-connection "localhost" 5000)))
  (unwind-protect
       ({{name}}-core:execute-raw conn "RAW COMMAND")
    ({{name}}-core:close-connection conn)))

;;; Using the high-level API
(ql:quickload :{{name}})

({{name}}:with-connection ()
  ({{name}}:query "SELECT * FROM users WHERE id = ~A" 42))

;;; Using object mapping
({{name}}:define-mapping user "users"
  (id "id")
  (name "name")
  (email "email"))

({{name}}:with-connection ()
  ({{name}}:find-object 'user :id 42))
```

## Key Principles

1. **Core is standalone** - Can be used without high-level wrapper
2. **Explicit dependencies** - Use `:depends-on` not `:serial t`
3. **Separate packages** - Each system has its own package
4. **Re-export judiciously** - High-level re-exports useful core symbols
5. **Test each layer** - Separate test systems for each component
