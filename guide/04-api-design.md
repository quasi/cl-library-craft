# API Design

## Function Design

### Keyword Arguments with Defaults

```lisp
(defun process-data (data &key 
                     (format :json)
                     (validate t)
                     (on-error :signal))
  "Process DATA according to FORMAT.
   
   FORMAT - Output format, one of :JSON, :XML, :SEXP (default :JSON)
   VALIDATE - If true, validate input before processing (default T)
   ON-ERROR - Error handling: :SIGNAL, :RETURN-NIL, or :USE-DEFAULT"
  ...)
```

### Multiple Return Values

Use for related information, not for error handling:

```lisp
;; Good: Related values
(defun parse-header (string)
  "Parse STRING as a header.
   Returns (values name value) or (values nil nil) if invalid."
  (let ((pos (position #\: string)))
    (if pos
        (values (subseq string 0 pos)
                (string-trim " " (subseq string (1+ pos))))
        (values nil nil))))

;; Good: Primary + metadata
(defun find-match (regex string)
  "Returns (values match-start match-end reg-starts reg-ends)."
  ...)
```

### Optional vs Keyword Arguments

**Use optional** for:
- Single obvious optional value
- Backwards compatibility

```lisp
(defun greet (&optional (name "World"))
  (format nil "Hello, ~A!" name))
```

**Use keywords** for:
- Multiple optional values
- Self-documenting calls
- Future extensibility

```lisp
(defun connect (&key host (port 80) (timeout 30) ssl)
  ...)
```

## Generic Functions

### When to Use Generics

Use generic functions for:
- User customization points
- Type-based dispatch
- Protocol definitions

```lisp
(defgeneric process-request (acceptor request)
  (:documentation "Process REQUEST on ACCEPTOR. Specialize for custom behavior."))

(defmethod process-request ((acceptor acceptor) request)
  "Default implementation for all acceptors."
  ...)
```

### Sensible Defaults

```lisp
(defgeneric session-cookie-name (acceptor)
  (:documentation "Return cookie name for sessions.")
  (:method ((acceptor acceptor))
    "hunchentoot-session"))  ; Useful default
```

### Protocol Pattern

```lisp
;; Define protocol
(defgeneric start (thing)
  (:documentation "Start THING. Returns THING."))

(defgeneric stop (thing)
  (:documentation "Stop THING. Returns THING."))

;; Implement for concrete class
(defmethod start ((acceptor acceptor))
  (start-listening acceptor)
  acceptor)
```

## Accessor Conventions

### Standard Accessors

```lisp
(defclass thing ()
  ((name :initarg :name :accessor thing-name)
   (value :initarg :value :accessor thing-value)))
```

### Reader vs Accessor

```lisp
(defclass session ()
  ;; Read-only: use reader
  ((id :reader session-id)
   ;; Mutable: use accessor
   (max-time :accessor session-max-time)))
```

### Convenience Accessors

```lisp
;; Base accessor works on object
(defgeneric header-in (name request)
  (:documentation "Return header NAME from REQUEST."))

;; Star variant uses *REQUEST*
(defun header-in* (name &optional (request *request*))
  "Return header NAME from current or specified REQUEST."
  (header-in name request))
```

## Macro Design

### Defining Macros

```lisp
(defmacro define-handler ((name &key uri method) &body body)
  "Define a request handler NAME for URI.
   
   Example:
     (define-handler (home :uri \"/\")
       \"Welcome!\")"
  `(progn
     (defun ,name ()
       ,@body)
     (register-handler ',name :uri ,uri :method ,method)))
```

### Context Macros

```lisp
(defmacro with-session ((&optional session) &body body)
  "Execute BODY with SESSION bound, starting one if needed."
  `(let ((,session (or *session* (start-session))))
     ,@body))
```

### Iteration Macros

```lisp
(defmacro do-matches ((match-start match-end regex string 
                       &optional result) &body body)
  "Iterate over matches of REGEX in STRING.
   MATCH-START and MATCH-END are bound to match boundaries."
  (with-gensyms (scanner target)
    `(let ((,scanner (create-scanner ,regex))
           (,target ,string))
       (block nil
         (scan-all ,scanner ,target
           (lambda (,match-start ,match-end)
             ,@body))
         ,result))))
```

## Naming Conventions

### Function/Macro Names

| Pattern | Meaning | Examples |
|---------|---------|----------|
| `verb-noun` | Action on object | `create-scanner`, `parse-string` |
| `noun-verb` | Object operation | `session-start`, `request-method` |
| `noun` | Accessor | `header-in`, `cookie-out` |

### Predicate Names

```lisp
(defun http-token-p (object)
  "Return true if OBJECT is a valid HTTP token."
  ...)

(defun ssl-p (&optional acceptor)
  "Return true if ACCEPTOR uses SSL."
  ...)
```

### Special Variable Names

```lisp
;; Global configuration
(defvar *default-content-type* "text/html")

;; Request-scoped binding
(defvar *request* nil)
(defvar *reply* nil)
(defvar *session* nil)
```

### Constant Names

```lisp
(defconstant +http-ok+ 200)
(defconstant +http-not-found+ 404)
(defconstant +max-url-length+ 2048)
```

## Error Signaling in APIs

### Signal, Don't Return Error Codes

```lisp
;; Bad: Returning nil for errors
(defun parse-json (string)
  (handler-case (actual-parse string)
    (error () nil)))  ; Caller can't distinguish nil value from error

;; Good: Signal condition
(defun parse-json (string)
  (handler-case (actual-parse string)
    (parse-error (c)
      (error 'json-parse-error :string string :cause c))))
```

### Keyword for Error Behavior Options

```lisp
(defun find-thing (name &key (if-does-not-exist :error))
  "Find thing by NAME.
   IF-DOES-NOT-EXIST - :ERROR (default), :CREATE, or NIL"
  (or (lookup name)
      (ecase if-does-not-exist
        (:error (error 'thing-not-found :name name))
        (:create (create-thing name))
        ((nil) nil))))
```

## API Evolution

### Adding Optional Parameters

```lisp
;; Version 1
(defun process (data))

;; Version 2 - backwards compatible
(defun process (data &key validate)
  ...)
```

### Deprecation

```lisp
(defun old-function (arg)
  "DEPRECATED: Use NEW-FUNCTION instead."
  (warn 'simple-warning 
        :format-control "~S is deprecated, use ~S"
        :format-arguments '(old-function new-function))
  (new-function arg))
```

## Documentation in APIs

### Docstring Standards

```lisp
(defun create-scanner (regex &key case-insensitive-mode
                                  multi-line-mode
                                  single-line-mode)
  "Create a scanner for REGEX.
   
   REGEX can be a string (Perl syntax) or a parse tree (S-expression).
   
   Keyword arguments control regex modes:
   - CASE-INSENSITIVE-MODE: If true, matching is case-insensitive
   - MULTI-LINE-MODE: If true, ^ and $ match at line boundaries
   - SINGLE-LINE-MODE: If true, . matches newlines
   
   Returns a scanner function suitable for use with SCAN.
   
   Example:
     (let ((scanner (create-scanner \"foo+\")))
       (scan scanner \"foooo\"))
     => 0, 5, #(), #()"
  ...)
```

### Required Docstring Elements

1. One-line summary
2. Parameter descriptions
3. Return value description
4. Example (for complex functions)
5. Related functions (see also)
