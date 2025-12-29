# Error Handling

## Condition Hierarchy Design

### Basic Hierarchy

```lisp
;; Top-level library condition (for handler-case)
(define-condition my-library-condition ()
  ()
  (:documentation "Base condition for my-library."))

;; Error type (subtype of error for standard handling)
(define-condition my-library-error (my-library-condition error)
  ()
  (:documentation "Base error for my-library."))

;; Warning type
(define-condition my-library-warning (my-library-condition warning)
  ()
  (:documentation "Base warning for my-library."))
```

### Specific Conditions

```lisp
;; Invocation error (bad arguments)
(define-condition my-library-invocation-error (my-library-error)
  ((function :initarg :function :reader condition-function))
  (:documentation "Signaled when a function is called incorrectly.")
  (:report (lambda (c s)
             (format s "Invalid call to ~A" (condition-function c)))))

;; Syntax/parsing error (with position info)
(define-condition syntax-error (my-library-error)
  ((input :initarg :input :reader error-input)
   (position :initarg :position :reader error-position))
  (:documentation "Signaled on parse errors.")
  (:report (lambda (c s)
             (format s "Syntax error in ~S at position ~D"
                     (error-input c) (error-position c)))))

;; Resource error
(define-condition resource-not-found (my-library-error)
  ((name :initarg :name :reader resource-name))
  (:documentation "Signaled when a required resource is not found.")
  (:report (lambda (c s)
             (format s "Resource not found: ~A" (resource-name c)))))
```

### Real Examples

#### CL-PPCRE Conditions

```lisp
(define-condition ppcre-error (simple-error)
  ()
  (:documentation "Base for all CL-PPCRE errors."))

(define-condition ppcre-invocation-error (ppcre-error)
  ()
  (:documentation "Called with wrong arguments."))

(define-condition ppcre-syntax-error (ppcre-error)
  ((string :initarg :string :reader ppcre-syntax-error-string)
   (pos :initarg :pos :reader ppcre-syntax-error-pos))
  (:documentation "Regex syntax error with location info."))
```

#### Hunchentoot Conditions

```lisp
(define-condition hunchentoot-condition ()
  ()
  (:documentation "Superclass for all Hunchentoot conditions."))

(define-condition hunchentoot-error (hunchentoot-condition error)
  ()
  (:documentation "Superclass for all Hunchentoot errors."))

(define-condition parameter-error (hunchentoot-error)
  ()
  (:documentation "Inconsistent or illegal parameters."))

(define-condition hunchentoot-warning (hunchentoot-condition warning)
  ())
```

## Signaling Conditions

### Basic Signaling

```lisp
;; Simple error
(error 'resource-not-found :name "config.lisp")

;; With format string (for simple-error subtypes)
(error 'my-library-error 
       :format-control "Cannot process ~S: ~A"
       :format-arguments (list input reason))

;; Warning
(warn 'deprecation-warning :old 'foo :new 'bar)
```

### Conditional Signaling

```lisp
(defun process (data)
  (unless (valid-p data)
    (error 'validation-error :data data))
  (when (deprecated-format-p data)
    (warn 'format-deprecation :format (data-format data)))
  (do-processing data))
```

### Helper Functions

```lisp
;; Convenience function for common error
(defun parameter-error (format-control &rest format-arguments)
  "Signal a PARAMETER-ERROR with the given message."
  (error 'parameter-error
         :format-control format-control
         :format-arguments format-arguments))

;; Usage
(parameter-error "Invalid value ~S for parameter ~A" value name)
```

## Restarts

### Defining Restarts

```lisp
(defun load-config (path)
  "Load configuration from PATH.
   
   Restarts:
   - USE-DEFAULT: Use default configuration
   - RETRY-WITH-PATH: Try a different path"
  (restart-case
      (if (probe-file path)
          (read-config-file path)
          (error 'config-not-found :path path))
    (use-default ()
      :report "Use default configuration"
      *default-config*)
    (retry-with-path (new-path)
      :report "Try a different path"
      :interactive (lambda ()
                     (format t "Enter path: ")
                     (list (read-line)))
      (load-config new-path))))
```

### Standard Restart Patterns

```lisp
;; Continue with default
(restart-case (risky-operation)
  (use-value (value)
    :report "Use a specific value"
    :interactive (lambda () (list (read)))
    value))

;; Retry operation
(restart-case (network-request url)
  (retry ()
    :report "Retry the request"
    (network-request url)))

;; Skip and continue
(restart-case (process-item item)
  (skip ()
    :report "Skip this item"
    nil))
```

### Invoking Restarts

```lisp
;; From handler
(handler-bind ((config-not-found
                 (lambda (c)
                   (invoke-restart 'use-default))))
  (load-config "missing.conf"))

;; With value
(handler-bind ((validation-error
                 (lambda (c)
                   (invoke-restart 'use-value (default-for c)))))
  (validate-input input))
```

## Error Handling Patterns

### Handler-Case (Catch and Handle)

```lisp
(defun safe-parse (string)
  "Parse STRING, returning NIL on error."
  (handler-case (parse string)
    (parse-error (c)
      (log-error c)
      nil)))
```

### Handler-Bind (React but Don't Unwind)

```lisp
(defun process-with-logging (data)
  "Process DATA, logging any warnings."
  (handler-bind ((warning
                   (lambda (c)
                     (log-warning c)
                     (muffle-warning c))))
    (process data)))
```

### Ignore-Errors (Last Resort)

```lisp
;; Only when you truly don't care about errors
(defun try-cleanup (path)
  (ignore-errors (delete-file path)))
```

### Unwind-Protect for Cleanup

```lisp
(defun with-temp-file (fn)
  "Call FN with a temporary file, ensuring cleanup."
  (let ((path (make-temp-file)))
    (unwind-protect
         (funcall fn path)
      (when (probe-file path)
        (delete-file path)))))
```

## Logging and Debugging

### Log Levels

```lisp
(defun log-message (level format-string &rest args)
  (when (>= (log-level-priority level) *log-threshold*)
    (apply #'format *log-stream* format-string args)))

(defun log-error (c)
  (log-message :error "Error: ~A~%" c))

(defun log-warning (c) 
  (log-message :warning "Warning: ~A~%" c))

(defun log-info (format-string &rest args)
  (apply #'log-message :info format-string args))
```

### Debug Mode

```lisp
(defvar *debug-mode* nil
  "When true, don't catch errors for debugging.")

(defmacro with-error-handling (&body body)
  `(if *debug-mode*
       (progn ,@body)
       (handler-case (progn ,@body)
         (error (c)
           (log-error c)
           (error-response c)))))
```

## Best Practices

### DO

1. Define library-specific condition hierarchy
2. Include relevant context in conditions (slots)
3. Provide useful :report methods
4. Signal specific condition types
5. Document restarts in docstrings
6. Use handler-bind when you can recover
7. Clean up with unwind-protect

### DON'T

1. Signal raw strings: `(error "something bad")`
2. Use ignore-errors casually
3. Catch conditions you can't handle
4. Lose context by re-signaling generic errors
5. Leave resources dangling on error

## Error Messages

### Good Error Messages

```lisp
;; Include: what happened, where, what to do
(:report (lambda (c s)
           (format s "Cannot parse regex ~S: ~
                      unexpected ~A at position ~D.~
                      ~@[~%Hint: ~A~]"
                   (error-input c)
                   (error-unexpected c)
                   (error-position c)
                   (error-hint c))))
```

### Message Guidelines

1. State what went wrong clearly
2. Include relevant values
3. Indicate position/context when available
4. Suggest fixes when possible
5. Don't include internal implementation details
