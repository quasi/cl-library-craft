# Testing

## Test Framework Choices

### FiveAM (Recommended)

Most widely used, simple, good defaults:

```lisp
(ql:quickload :fiveam)

(def-suite my-library-tests
  :description "Tests for my-library")

(in-suite my-library-tests)

(test basic-functionality
  (is (= 4 (add 2 2)))
  (is (string= "hello" (greet))))

(test error-handling
  (signals my-error
    (risky-operation nil)))

;; Run tests
(run! 'my-library-tests)
```

### Parachute

More features, better output:

```lisp
(ql:quickload :parachute)

(define-test my-library-tests)

(define-test basic-test
  :parent my-library-tests
  (true (valid-p input))
  (is = 4 (compute 2 2))
  (fail (broken-function) error))
```

## Test Organization

### Directory Structure

```
library/
├── library.asd
├── src/
│   └── ...
└── test/
    ├── packages.lisp     # Test package
    ├── suite.lisp        # Test suite definition
    ├── test-core.lisp    # Core functionality tests
    ├── test-api.lisp     # Public API tests
    └── test-edge.lisp    # Edge cases
```

### Test System Definition

```lisp
(defsystem :my-library/test
  :description "Tests for my-library"
  :depends-on (:my-library :fiveam)
  :pathname "test/"
  :serial t
  :components ((:file "packages")
               (:file "suite")
               (:file "test-core")
               (:file "test-api"))
  :perform (test-op (op c)
             (symbol-call :fiveam :run! :my-library-tests)))
```

### Test Package

```lisp
(defpackage :my-library/test
  (:use :cl :my-library :fiveam)
  (:export #:run-tests))

(in-package :my-library/test)

(def-suite my-library-tests
  :description "My library test suite")

(defun run-tests ()
  (run! 'my-library-tests))
```

## Test Categories

### Unit Tests

Test individual functions in isolation:

```lisp
(test parse-integer-valid
  "Parse valid integer strings"
  (is (= 42 (parse-integer "42")))
  (is (= -5 (parse-integer "-5")))
  (is (= 0 (parse-integer "0"))))

(test parse-integer-invalid
  "Signal error on invalid input"
  (signals parse-error
    (parse-integer "not a number"))
  (signals parse-error
    (parse-integer "")))
```

### Integration Tests

Test component interactions:

```lisp
(test request-response-cycle
  "Full request-response cycle"
  (let ((server (make-test-server)))
    (unwind-protect
         (progn
           (start server)
           (let ((response (http-request (server-url server))))
             (is (= 200 (response-status response)))
             (is (string= "OK" (response-body response)))))
      (stop server))))
```

### Edge Case Tests

```lisp
(test empty-input
  (is (null (process nil)))
  (is (null (process "")))
  (is (null (process #()))))

(test large-input
  (let ((big (make-string 1000000 :initial-element #\x)))
    (finishes (process big))))

(test unicode-input
  (is (= 3 (length (process "日本語")))))
```

## Test Assertions (FiveAM)

```lisp
;; Basic assertions
(is (= expected actual))           ; With =
(is (equal expected actual))       ; With EQUAL
(is (string= expected actual))     ; Strings

;; Truth tests
(is-true expr)                     ; Must be true
(is-false expr)                    ; Must be false/nil

;; Condition tests  
(signals condition-type expr)      ; Must signal condition
(finishes expr)                    ; Must not signal

;; Failure
(fail "Reason")                    ; Unconditional fail
(skip "Reason")                    ; Skip test
```

## Test Fixtures

### Setup and Teardown

```lisp
(def-fixture with-temp-file ()
  (let ((path (make-temp-pathname)))
    (unwind-protect
         (&body)  ; Test body runs here
      (when (probe-file path)
        (delete-file path)))))

(test file-operations
  (with-fixture with-temp-file
    (write-data path)
    (is (probe-file path))))
```

## Running Tests

### From REPL

```lisp
;; Load and run
(asdf:test-system :my-library)

;; Or manually
(ql:quickload :my-library/test)
(my-library/test:run-tests)

;; Run specific test
(fiveam:run! 'my-library/test::specific-test)
```

### From Command Line

```bash
sbcl --eval '(ql:quickload :my-library/test)' \
     --eval '(unless (fiveam:run! :my-library-tests) 
               (uiop:quit 1))' \
     --quit
```

## Test Best Practices

### DO

1. **Test public API thoroughly** - Every exported function
2. **Test error conditions** - Not just happy paths
3. **Use descriptive test names** - What's being tested
4. **Keep tests independent** - No order dependencies
5. **Test edge cases** - Empty, nil, large inputs

### DON'T

1. **Test implementation details** - Tests break on refactor
2. **Share state between tests** - Leads to flaky tests
3. **Ignore failing tests** - Fix or remove
4. **Write slow tests** - Keep suite fast

## Example Test Suite

```lisp
(in-suite cl-ppcre-tests)

(test scan-basic
  "Basic scan functionality"
  (multiple-value-bind (start end regs-start regs-end)
      (scan "a+" "xaaab")
    (is (= 1 start))
    (is (= 4 end))))

(test scan-no-match
  "Scan returns NIL on no match"
  (is (null (scan "x" "aaaa"))))

(test syntax-error
  "Invalid regex signals syntax error"
  (signals ppcre-syntax-error
    (create-scanner "a{2,1}")))
```
