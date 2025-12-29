# Edi Weitz Style Template

Use for: utility libraries, parsers, protocol implementations, standalone tools.

## Structure

```
{{name}}/
├── {{name}}.asd
├── packages.lisp
├── specials.lisp      # Optional: only if you have special variables
├── conditions.lisp    # Optional: only if you define conditions
├── util.lisp          # Optional: internal helpers
├── {{main}}.lisp      # Core implementation (one or more files)
├── README.md
├── LICENSE
├── CHANGELOG          # Optional but recommended
└── doc/               # Optional: HTML documentation
    └── index.html
```

## {{name}}.asd

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(defsystem :{{name}}
  :description "{{description}}"
  :author "{{author}} <{{email}}>"
  :license "{{license}}"  ; Usually "BSD" or "MIT"
  :version "0.1.0"
  :serial t
  :components ((:file "packages")
               ;; Include only files that exist:
               (:file "specials")     ; If you have special variables
               (:file "conditions")   ; If you define conditions
               (:file "util")         ; If you have internal utilities
               (:file "{{main}}"))    ; Your core implementation
  :depends-on (
               ;; List dependencies - prefer minimal
               ;; Common choices:
               ;; :alexandria        ; General utilities
               ;; :cl-ppcre          ; Regular expressions
               ;; :flexi-streams     ; Character encoding
               ;; :bordeaux-threads  ; Threading
               )
  :in-order-to ((test-op (test-op "{{name}}/test"))))

(defsystem :{{name}}/test
  :description "Tests for {{name}}"
  :depends-on (:{{name}} :fiveam)
  :serial t
  :components ((:module "test"
                :components ((:file "packages")
                             (:file "tests"))))
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :{{name}}-tests)))
```

## packages.lisp

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage :{{name}}
  (:use :cl)
  (:export
   ;; Main API - group by category
   ;; Primary functions
   #:main-function
   #:other-function
   
   ;; Accessors (if applicable)
   #:thing-name
   #:thing-value
   
   ;; Predicates
   #:thing-p
   #:valid-p
   
   ;; Conditions
   #:{{name}}-error
   #:{{name}}-warning
   
   ;; Special variables
   #:*default-setting*
   
   ;; Constants (rare in exports)
   #:+some-constant+
   ))
```

## specials.lisp

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {{NAME}}; Base: 10 -*-

(in-package :{{name}})

(defvar *default-setting* :default
  "Documentation for this special variable.
Describes what it controls and valid values.")

(defvar *another-setting* nil
  "Another configuration option.")

;; Constants (if any)
(defconstant +buffer-size+ 4096
  "Default buffer size in bytes.")
```

## conditions.lisp

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {{NAME}}; Base: 10 -*-

(in-package :{{name}})

;;; Base conditions

(define-condition {{name}}-condition ()
  ()
  (:documentation "Base condition for all {{name}} conditions."))

(define-condition {{name}}-error ({{name}}-condition error)
  ()
  (:documentation "Base error for {{name}}."))

(define-condition {{name}}-warning ({{name}}-condition warning)
  ()
  (:documentation "Base warning for {{name}}."))

;;; Specific conditions

(define-condition parse-error ({{name}}-error)
  ((message :initarg :message :reader parse-error-message)
   (position :initarg :position :reader parse-error-position :initform nil))
  (:report (lambda (condition stream)
             (format stream "Parse error~@[ at position ~D~]: ~A"
                     (parse-error-position condition)
                     (parse-error-message condition))))
  (:documentation "Signaled when parsing fails."))

(define-condition invalid-input-error ({{name}}-error)
  ((input :initarg :input :reader invalid-input-error-input)
   (reason :initarg :reason :reader invalid-input-error-reason))
  (:report (lambda (condition stream)
             (format stream "Invalid input ~S: ~A"
                     (invalid-input-error-input condition)
                     (invalid-input-error-reason condition))))
  (:documentation "Signaled when input is invalid."))
```

## util.lisp

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {{NAME}}; Base: 10 -*-

(in-package :{{name}})

;;; Internal utilities - not exported

(defun ensure-string (thing)
  "Convert THING to a string if it isn't already."
  (etypecase thing
    (string thing)
    (symbol (symbol-name thing))
    (character (string thing))))

(defmacro with-gensyms ((&rest names) &body body)
  "Bind NAMES to fresh gensyms in BODY."
  `(let ,(loop for name in names
               collect `(,name (gensym ,(symbol-name name))))
     ,@body))

(declaim (inline whitespace-p))
(defun whitespace-p (char)
  "Return T if CHAR is whitespace."
  (member char '(#\Space #\Tab #\Newline #\Return)))
```

## {{main}}.lisp (Example: parser.lisp)

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: {{NAME}}; Base: 10 -*-

(in-package :{{name}})

;;;; Public API

(defun parse (input)
  "Parse INPUT and return the result.

INPUT can be a string or a stream.

Returns the parsed structure.

Signals PARSE-ERROR if parsing fails.

Example:
  (parse \"example input\")
  => <parsed-result>"
  (etypecase input
    (string (parse-string input))
    (stream (parse-stream input))))

(defun parse-file (pathname)
  "Parse the file at PATHNAME.

Returns the parsed structure.

Signals PARSE-ERROR if parsing fails.
Signals FILE-ERROR if the file cannot be read."
  (with-open-file (stream pathname :direction :input)
    (parse-stream stream)))

;;;; Internal implementation

(defun parse-string (string)
  "Parse STRING. Internal function."
  (with-input-from-string (stream string)
    (parse-stream stream)))

(defun parse-stream (stream)
  "Parse STREAM. Internal function."
  ;; Implementation here
  (let ((result nil))
    ;; ... parsing logic ...
    result))
```

## README.md

```markdown
# {{Name}}

{{Description}}

## Installation

{{Name}} is available via Quicklisp (once submitted):

```lisp
(ql:quickload :{{name}})
```

Or clone this repository to your local-projects:

```bash
cd ~/quicklisp/local-projects
git clone https://github.com/{{username}}/{{name}}.git
```

Then:

```lisp
(ql:quickload :{{name}})
```

## Usage

```lisp
(use-package :{{name}})

;; Basic usage
(parse "your input here")

;; With options
(let ((*default-setting* :custom))
  (parse input))
```

## API Reference

### Functions

#### `parse` input => result

Parse INPUT (string or stream) and return the result.

#### `parse-file` pathname => result

Parse the file at PATHNAME.

### Conditions

#### `{{name}}-error`

Base error class for all {{name}} errors.

#### `parse-error`

Signaled when parsing fails. Slots:
- `message` - Error description
- `position` - Position in input (may be NIL)

### Special Variables

#### `*default-setting*`

Controls default behavior. Default: `:default`

## License

{{License}}
```

## LICENSE (BSD example)

```
Copyright (c) {{year}}, {{author}}
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
```

## test/packages.lisp

```lisp
(in-package :cl-user)

(defpackage :{{name}}/test
  (:use :cl :{{name}} :fiveam)
  (:export #:run-tests))
```

## test/tests.lisp

```lisp
(in-package :{{name}}/test)

(def-suite {{name}}-tests
  :description "Test suite for {{name}}")

(in-suite {{name}}-tests)

(defun run-tests ()
  "Run all {{name}} tests."
  (run! '{{name}}-tests))

;;; Basic tests

(test parse-basic
  "Test basic parsing"
  (let ((result (parse "simple input")))
    (is (not (null result)))))

(test parse-empty
  "Test parsing empty input"
  (is (null (parse ""))))

;;; Error tests

(test parse-error-handling
  "Test that invalid input signals parse-error"
  (signals parse-error
    (parse "invalid input that should fail")))

;;; Edge cases

(test parse-unicode
  "Test Unicode handling"
  (let ((result (parse "日本語")))
    (is (not (null result)))))
```
