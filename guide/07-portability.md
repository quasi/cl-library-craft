# Portability

## Cross-Implementation Strategy

### Preferred Approach: Compatibility Libraries

Use well-maintained abstraction libraries rather than rolling your own:

| Domain | Library | Notes |
|--------|---------|-------|
| Threading | bordeaux-threads | Standard CL threading API |
| Networking | usocket | Portable sockets |
| Streams | flexi-streams | Flexible encoding streams |
| Gray streams | trivial-gray-streams | Gray stream portability |
| Filesystem | cl-fad | File and directory operations |
| Features | trivial-features | Normalize *features* |
| Garbage collection | trivial-garbage | Portable weak refs, finalization |

### Example: Hunchentoot Portability

```lisp
;; System definition
:depends-on ((:feature (:not :lispworks) :usocket)
             (:feature (:not :lispworks) :bordeaux-threads))

;; Conditional loading
(:file "lispworks" :if-feature :lispworks)
(:file "compat" :if-feature (:not :lispworks))
```

## Feature Conditionals

### Reader Conditionals

```lisp
;; Single feature
#+sbcl (sb-ext:gc)
#+lispworks (hcl:gc-if-needed)

;; Feature absence
#-sbcl (portable-gc)

;; Complex conditions
#+(or sbcl ccl ecl) (implementation-specific-thing)
#+(and sbcl (not windows)) (unix-sbcl-thing)
```

### In Code

```lisp
(defun get-thread-id ()
  #+sbcl (sb-thread:thread-os-tid sb-thread:*current-thread*)
  #+ccl (ccl::current-thread-id)
  #+lispworks (mp:get-current-process)
  #-(or sbcl ccl lispworks)
  (error "Thread ID not implemented for this Lisp"))
```

### In System Definitions

```lisp
:depends-on ((:feature :sbcl :sb-posix)
             (:feature (:not :sbcl) :osicat))

:components ((:file "posix-sbcl" :if-feature :sbcl)
             (:file "posix-portable" :if-feature (:not :sbcl)))
```

## Compatibility Files Pattern

### Structure

```
library/
├── impl-sbcl.lisp      # SBCL-specific
├── impl-ccl.lisp       # CCL-specific  
├── impl-lispworks.lisp # LispWorks-specific
└── compat.lisp         # Fallback/portable
```

### Implementation

```lisp
;;; compat.lisp - Portable implementations

(defun make-thread (function &key name)
  "Create and start a new thread."
  (bt:make-thread function :name name))

(defun current-thread ()
  "Return the current thread."
  (bt:current-thread))
```

```lisp
;;; impl-sbcl.lisp - SBCL optimizations

(defun make-thread (function &key name)
  "SBCL-optimized thread creation."
  (sb-thread:make-thread function :name name))
```

## Common Portability Issues

### Character Encoding

```lisp
;; Use flexi-streams for consistent encoding
(with-open-file (s file :external-format :utf-8)
  ...)

;; Or with flexi-streams explicitly
(flex:with-input-from-sequence (s bytes)
  (flex:octets-to-string s :external-format :utf-8))
```

### Filesystem Paths

```lisp
;; Portable pathname construction
(merge-pathnames 
  (make-pathname :name "config" :type "lisp")
  (user-homedir-pathname))

;; Use cl-fad for operations
(cl-fad:walk-directory dir #'process-file)
```

### Threads and Locks

```lisp
;; Always use bordeaux-threads
(bt:make-lock "my-lock")
(bt:with-lock-held (lock)
  ...)

;; Conditions/wait
(bt:condition-wait condition lock)
(bt:condition-notify condition)
```

### Weak References

```lisp
;; Use trivial-garbage
(tg:make-weak-pointer object)
(tg:weak-pointer-value weak-ptr)
```

### Special Floating Point Values

```lisp
;; Different impls represent infinity differently
#+(or sbcl ccl) sb-ext:double-float-positive-infinity
#+lispworks 1D++0
```

## Implementation-Specific Optimizations

### Declaration Pattern

```lisp
(defun fast-operation (array index)
  (declare (optimize (speed 3) (safety 0)))
  (declare (type simple-vector array)
           (type fixnum index))
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (svref array index))
```

### SBCL Specific

```lisp
#+sbcl
(progn
  (declaim (sb-ext:freeze-type my-struct))
  (declaim (inline critical-function)))
```

### LispWorks Specific

```lisp
#+lispworks
(progn
  (eval-when (:compile-toplevel)
    (proclaim '(optimize (speed 3))))
  (define-simple-parser ...))
```

## Testing Portability

### CI Configuration

Test on multiple implementations:
- SBCL (most common)
- CCL (second most common)
- ECL (embedded use)
- ABCL (JVM)
- LispWorks (commercial, if available)

### Portability Test Macro

```lisp
(defmacro test-on-all-impls (name &body body)
  `(test ,name
     (handler-case
         (progn ,@body)
       (error (c)
         (skip "~A: ~A" (lisp-implementation-type) c)))))
```

## Best Practices

1. **Start portable**: Use compatibility libraries from the beginning
2. **Test early**: CI on multiple implementations
3. **Isolate specifics**: Keep implementation code in separate files
4. **Document requirements**: Note implementation dependencies
5. **Provide fallbacks**: Graceful degradation when possible
6. **Avoid #+ in public API**: Keep conditional code internal

## What to Avoid

1. **Rolling your own abstractions**: Use existing compat libraries
2. **Untested conditionals**: Easy to break on update
3. **Hard dependencies on one impl**: Limits adoption
4. **Feature assumptions**: Check, don't assume
5. **Version-specific code**: Implementations evolve
