# SBCL Optimizations

SBCL (Steel Bank Common Lisp) is the most popular CL implementation for production use. This guide covers SBCL-specific optimizations found in high-quality CL libraries.

## Declaration Fundamentals

### Optimization Qualities

```lisp
(declare (optimize (speed 3)        ; 0-3, maximize speed
                   (safety 1)        ; 0-3, runtime checks
                   (debug 1)         ; 0-3, debugging info
                   (space 0)         ; 0-3, minimize size
                   (compilation-speed 0))) ; 0-3, compile fast
```

**Common Profiles**:

```lisp
;; Production hot path
(declare (optimize (speed 3) (safety 0) (debug 0)))

;; Development
(declare (optimize (speed 1) (safety 3) (debug 3)))

;; Balanced production
(declare (optimize (speed 3) (safety 1) (debug 1)))
```

### Type Declarations

```lisp
;; Function argument types
(defun fast-add (a b)
  (declare (type fixnum a b))
  (the fixnum (+ a b)))

;; Local variable types
(let ((sum 0))
  (declare (type fixnum sum))
  ...)

;; Array element types
(defun process-bytes (data)
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  ...)

;; Multiple declarations
(defun complex-fn (x y z)
  (declare (type double-float x y)
           (type (integer 0 100) z)
           (optimize (speed 3) (safety 0)))
  ...)
```

## Type Specifiers for Performance

### Numeric Types

```lisp
;; Fixnums - machine word integers, no boxing
fixnum                              ; Platform-dependent range
(integer 0 255)                     ; Explicit range
(unsigned-byte 8)                   ; 0-255
(unsigned-byte 16)                  ; 0-65535
(unsigned-byte 32)                  ; 0-4294967295
(signed-byte 32)                    ; -2^31 to 2^31-1

;; Floats - IEEE 754
single-float                        ; 32-bit
double-float                        ; 64-bit

;; Complex numbers
(complex single-float)
(complex double-float)
```

### Array Types

```lisp
;; Simple arrays (no fill pointer, not adjustable, not displaced)
(simple-array element-type (dimensions))
(simple-array (unsigned-byte 8) (*))      ; 1D byte vector
(simple-array double-float (100 100))     ; 2D float matrix
(simple-array t (*))                       ; General 1D

;; Specialized vectors
(simple-vector)                            ; (simple-array t (*))
simple-string                              ; (simple-array character (*))
simple-bit-vector                          ; (simple-array bit (*))

;; With known size
(simple-array (unsigned-byte 8) (1024))    ; Exactly 1024 bytes
```

## SBCL-Specific Features

### Compiler Macros

Define fast paths for common call patterns:

```lisp
(defun generic-lookup (table key &optional default)
  "General lookup function."
  (gethash key table default))

(define-compiler-macro generic-lookup (&whole form table key &optional default)
  "Optimize when we know the table type."
  (if (and (constantp default) (null (eval default)))
      `(gethash ,key ,table)  ; Simpler form
      form))                   ; Fall back to function
```

### DEFKNOWN - Declare Function Properties

```lisp
;; Tell SBCL about function behavior
(sb-c:defknown my-pure-function (fixnum fixnum) fixnum
  (sb-c:foldable sb-c:flushable sb-c:movable)
  :overwrite-fndb-silently t)

;; Properties:
;; - foldable: Can compute at compile time if args are constant
;; - flushable: Can be eliminated if result unused
;; - movable: Can be reordered
;; - always-translatable: Has a VOP
```

### VOPs (Virtual Operations)

For ultimate performance, define assembly-level operations:

```lisp
;; From ironclad - fast byte operations
(sb-c:define-vop (fast-xor-block)
  (:translate xor-block)
  (:policy :fast-safe)
  (:args (dst :scs (sb-vm::descriptor-reg))
         (src :scs (sb-vm::descriptor-reg)))
  (:arg-types simple-array-unsigned-byte-8
              simple-array-unsigned-byte-8)
  (:generator 10
    ;; Assembly code here
    ...))
```

### SB-EXT Extensions

```lisp
;; Word-sized operations (no overflow checking)
(sb-ext:truly-the fixnum x)           ; Promise type, no check

;; Memory barriers
(sb-thread:barrier (:read))
(sb-thread:barrier (:write))
(sb-thread:barrier (:memory))

;; Weak pointers
(sb-ext:make-weak-pointer object)
(sb-ext:weak-pointer-value weak-ptr)

;; Finalizers
(sb-ext:finalize object #'cleanup-fn)
(sb-ext:cancel-finalization object)
```

## Patterns from Real Libraries

### Ironclad: Crypto Optimization

```lisp
;; Type declarations for tight loops
(defun xor-block (block1 block2 result)
  (declare (type (simple-array (unsigned-byte 8) (*)) 
                 block1 block2 result)
           (optimize (speed 3) (safety 0) (debug 0)))
  (dotimes (i (length block1))
    (setf (aref result i)
          (logxor (aref block1 i) (aref block2 i)))))

;; Unrolled loops
(defmacro with-unrolled-loop ((var count) &body body)
  `(progn
     ,@(loop for i below count
             collect `(let ((,var ,i)) ,@body))))
```

### CL-PPCRE: Scanner Optimization

```lisp
;; Inline small functions
(declaim (inline char-equal-ignore-case))
(defun char-equal-ignore-case (c1 c2)
  (declare (type character c1 c2)
           (optimize (speed 3) (safety 0)))
  (char-equal c1 c2))

;; Constant scanner compilation
(define-compiler-macro create-scanner (&whole form regex &rest args)
  (if (stringp regex)
      `(load-time-value (create-scanner ,regex ,@args))
      form))
```

### Hunchentoot: String Operations

```lisp
;; Fast string building
(defun fast-format-to-string (control-string &rest args)
  (declare (optimize (speed 3)))
  (with-output-to-string (s)
    (apply #'format s control-string args)))

;; Avoid consing in hot paths
(defun parse-header-line (line start end)
  (declare (type simple-string line)
           (type fixnum start end)
           (optimize (speed 3) (safety 0)))
  ;; Use displaced arrays instead of subseq
  ...)
```

### Woo: Event Loop Optimization

```lisp
;; Static vectors for zero-copy I/O
(cffi:defcfun "read" :int
  (fd :int)
  (buf :pointer)
  (count :unsigned-int))

(let ((buffer (static-vectors:make-static-vector 
               4096 :element-type '(unsigned-byte 8))))
  (static-vectors:with-pointer-to-vector-data (ptr buffer)
    (read fd ptr 4096)))
```

## Common Optimization Patterns

### 1. Hot Path Declarations

```lisp
(defun process-request (request)
  "Called for every HTTP request - must be fast."
  (declare (optimize (speed 3) (safety 1)))
  ...)

(defun parse-config (file)
  "Called once at startup - clarity over speed."
  (declare (optimize (speed 1) (safety 3) (debug 3)))
  ...)
```

### 2. Avoiding Boxing

```lisp
;; Bad: creates boxed floats
(defun compute (x y)
  (let ((result (* x y)))
    (+ result 1.0)))

;; Good: stays unboxed
(defun compute (x y)
  (declare (type double-float x y)
           (optimize (speed 3) (safety 0)))
  (let ((result (* x y)))
    (declare (type double-float result))
    (the double-float (+ result 1.0d0))))
```

### 3. Stack Allocation

```lisp
;; Allocate on stack instead of heap
(defun process-data (data)
  (declare (optimize (speed 3)))
  (let ((buffer (make-array 256 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buffer))  ; Stack allocate
    ...))
```

### 4. Inline Functions

```lisp
;; Declare before definition
(declaim (inline small-helper))
(defun small-helper (x)
  (1+ x))

;; Or use DEFINE-COMPILER-MACRO for conditional inlining
```

### 5. Loop Optimization

```lisp
;; Prefer DO/DOTIMES with declarations
(defun sum-array (arr)
  (declare (type (simple-array fixnum (*)) arr)
           (optimize (speed 3) (safety 0)))
  (let ((sum 0))
    (declare (type fixnum sum))
    (dotimes (i (length arr) sum)
      (incf sum (aref arr i)))))

;; LOOP is often slower due to complexity
```

## Profiling and Verification

### Check Generated Code

```lisp
;; Disassemble to verify optimization
(disassemble 'my-function)

;; Check for notes during compilation
(compile 'my-function)
;; Watch for "unable to optimize" notes
```

### Time Measurements

```lisp
;; Simple timing
(time (my-function args))

;; Statistical profiling
(sb-sprof:with-profiling (:report :flat)
  (dotimes (i 10000)
    (my-function args)))

;; Deterministic profiling  
(sb-profile:profile my-function)
(my-function args)
(sb-profile:report)
(sb-profile:unprofile my-function)
```

### Memory Profiling

```lisp
;; Check consing
(time (my-function args))
;; Look at "N bytes consed"

;; Allocation profiling
(sb-sprof:with-profiling (:mode :alloc :report :flat)
  ...)
```

## Conditional Compilation

### Feature Expressions

```lisp
;; SBCL-only code
#+sbcl
(defun sbcl-optimized-version ()
  (declare (optimize (speed 3)))
  ...)

#-sbcl
(defun portable-version ()
  ...)

;; Version-specific
#+(and sbcl (or x86 x86-64))
(load "sbcl-x86-vops")
```

### In ASDF

```lisp
(defsystem :my-lib
  :components
  ((:file "portable")
   (:module "sbcl-opt"
    :if-feature :sbcl
    :components ((:file "vops")
                 (:file "assembly")))))
```

## Best Practices

### DO

1. **Profile first** - Only optimize measured bottlenecks
2. **Declare types in hot paths** - Especially numeric loops
3. **Use simple-array** - Enables better optimization
4. **Prefer fixnum arithmetic** - Avoids bignum allocation
5. **Check disassembly** - Verify optimizations apply
6. **Test with safety 3** - Catch type errors in development

### DON'T

1. **Premature optimization** - Clarity first
2. **Safety 0 everywhere** - Only in proven hot paths
3. **Ignore compiler notes** - They indicate problems
4. **Assume optimization** - Always verify with disassemble
5. **Micro-optimize cold code** - Startup code doesn't matter
6. **Over-declare** - Wrong types cause subtle bugs

## Quick Reference

```lisp
;; Maximum speed template
(defun hot-function (x y z)
  (declare (type fixnum x y)
           (type (simple-array double-float (*)) z)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((result 0.0d0))
    (declare (type double-float result))
    (dotimes (i (length z))
      (declare (type fixnum i))
      (incf result (aref z i)))
    (the double-float (* result (coerce (+ x y) 'double-float)))))
```
