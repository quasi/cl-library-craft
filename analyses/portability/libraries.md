# Portability Libraries

These libraries provide cross-implementation compatibility layers, essential infrastructure for portable CL code.

## Overview

| Library | Author | Purpose | Stars |
|---------|--------|---------|-------|
| **bordeaux-threads** | Stelian Ionescu | Threading abstraction | ~238 |
| **closer-mop** | Pascal Costanza | MOP compatibility | ~166 |
| **ironclad** | Sharplispers | Cryptography | ~170 |

---

## Bordeaux-Threads

### Purpose

Provides portable threading primitives across CL implementations.

### ASDF Pattern

```lisp
(defsystem :bordeaux-threads
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :licence "MIT"
  :description "Portable shared-state concurrency for Common Lisp."
  :version (:read-file-form "version.sexp")
  :depends-on (:alexandria :global-vars :trivial-features :trivial-garbage
               #+(and allegro (version>= 9)) (:require "smputil")
               #+(and allegro (not (version>= 9))) (:require "process")
               (:feature :corman (:require "threads")))
  :components ((:static-file "version.sexp")
               (:module "api-v1"
                :pathname "apiv1/"
                :serial t
                :components ...)))
```

### Key Pattern: Two API Versions

```
apiv1/              # Original API
  default-implementations.lisp
  impl-*.lisp       # Implementation-specific
apiv2/              # New API (being developed)
```

### Core API

```lisp
;; Thread creation
(bt:make-thread #'function :name "thread-name")
(bt:current-thread)
(bt:all-threads)
(bt:thread-name thread)

;; Locks
(bt:make-lock &optional name)
(bt:with-lock-held (lock) &body body)
(bt:acquire-lock lock &optional wait-p)
(bt:release-lock lock)

;; Recursive locks
(bt:make-recursive-lock &optional name)
(bt:with-recursive-lock-held (lock) &body body)

;; Condition variables
(bt:make-condition-variable &key name)
(bt:condition-wait condition lock &key timeout)
(bt:condition-notify condition)
(bt:condition-broadcast condition)

;; Semaphores (v2)
(bt2:make-semaphore &key name count)
(bt2:signal-semaphore semaphore &optional count)
(bt2:wait-on-semaphore semaphore &key timeout)

;; Thread-local variables
(bt:*default-special-bindings*)
```

### Implementation Strategy

```lisp
;; Default implementation provides interface
(defgeneric make-thread (function &key name))

;; Each implementation provides methods
;; impl-sbcl.lisp:
(defmethod make-thread (function &key name)
  (sb-thread:make-thread function :name name))

;; impl-ccl.lisp:
(defmethod make-thread (function &key name)
  (ccl:process-run-function name function))
```

---

## Closer-MOP

### Purpose

Rectifies absent or incorrect CLOS MOP features across implementations.

### ASDF Pattern

```lisp
(defsystem #:closer-mop
  :name "Closer to MOP"
  :author "Pascal Costanza"
  :version "1.0.0"
  :licence "MIT-style license"
  :serial t
  :components
  ((:file "closer-mop-packages")
   (:file "closer-mop-shared")
   #+clisp (:file "closer-clisp")
   #-clisp
   (:module "implementation"
    :pathname ""
    :components
    (#+allegro (:file "closer-allegro")
     #+ccl (:file "closer-clozure")
     #+cmu (:file "closer-cmu")
     #+ecl (:file "closer-ecl")
     #+lispworks (:file "closer-lispworks")
     #+sbcl (:file "closer-sbcl")
     ...))))
```

### Package Strategy

Provides drop-in replacement packages:

```lisp
(defpackage #:closer-mop
  (:use #:common-lisp)
  (:nicknames #:c2mop)
  
  ;; Shadow implementation-specific symbols
  #+(or allegro clozure lispworks)
  (:shadow #:standard-class)
  
  #+(or allegro clisp clozure ecl lispworks sbcl)
  (:shadow #:defgeneric #:defmethod #:standard-generic-function)
  ...)

;; Full replacement for COMMON-LISP package
(defpackage #:closer-common-lisp
  (:nicknames #:c2cl)
  ...)
```

### Core Exports

```lisp
;; Class metaobjects
#:standard-class
#:funcallable-standard-class
#:forward-referenced-class

;; Slot metaobjects  
#:standard-slot-definition
#:direct-slot-definition
#:effective-slot-definition

;; Generic function metaobjects
#:standard-generic-function
#:standard-method

;; Introspection
#:class-direct-slots
#:class-slots
#:class-precedence-list
#:compute-class-precedence-list

;; Finalization
#:finalize-inheritance
#:class-finalized-p

;; Slot access
#:slot-value-using-class
#:slot-boundp-using-class
#:slot-makunbound-using-class

;; Method combination
#:compute-effective-method
```

### Utility Functions

```lisp
;; Ensure class is finalized before use
(defun ensure-finalized (class &optional (errorp t))
  (if (typep class 'class)
      (unless (class-finalized-p class)
        (finalize-inheritance class))
      (when errorp (error "~S is not a class." class)))
  class)

;; Check subclass relationship
(defun subclassp (class superclass)
  ...)
```

---

## Ironclad

### Purpose

Pure CL cryptography library - ciphers, digests, MACs, public key algorithms.

### Key Design Decisions

1. **No FFI** - Pure Common Lisp
2. **Portable with optimizations** - Works everywhere, fast on SBCL
3. **Comprehensive** - All major algorithms

### Structure

```
ironclad/
├── ironclad.asd
├── src/
│   ├── package.lisp
│   ├── conditions.lisp
│   ├── ciphers/
│   │   ├── aes.lisp
│   │   ├── des.lisp
│   │   ├── blowfish.lisp
│   │   └── ...
│   ├── digests/
│   │   ├── sha256.lisp
│   │   ├── md5.lisp
│   │   └── ...
│   ├── macs/
│   │   ├── hmac.lisp
│   │   └── ...
│   ├── public-key/
│   │   ├── rsa.lisp
│   │   ├── dsa.lisp
│   │   └── ...
│   └── opt/
│       └── sbcl/          # SBCL optimizations
│           └── x86oid-vm.lisp
└── testing/
    └── test-vectors/      # From official sources
```

### API Design

```lisp
;; Digests
(ironclad:make-digest :sha256)
(ironclad:update-digest digest data)
(ironclad:produce-digest digest)

;; Convenience
(ironclad:digest-sequence :sha256 data)
(ironclad:digest-file :sha256 path)

;; Ciphers
(ironclad:make-cipher :aes :key key :mode :cbc :initialization-vector iv)
(ironclad:encrypt cipher plaintext ciphertext)
(ironclad:decrypt cipher ciphertext plaintext)

;; MACs
(ironclad:make-mac :hmac key :digest :sha256)
(ironclad:update-mac mac data)
(ironclad:produce-mac mac)

;; Public key
(ironclad:generate-key-pair :rsa :num-bits 2048)
(ironclad:sign-message private-key message)
(ironclad:verify-signature public-key message signature)
```

### Thread Safety Note

```lisp
;; Objects are NOT thread-safe
;; Each thread needs its own digest/cipher/mac objects

;; PRNG is thread-local with bordeaux-threads
#+thread-support
(pushnew '(*prng* . (make-prng :os)) 
         bt:*default-special-bindings*
         :test #'equal)
```

---

## Lessons for AI Code Generation

### 1. Portability Library Structure

For cross-implementation libraries:

```
lib/
├── packages.lisp       # Unified API
├── shared.lisp         # Common implementation
├── impl-sbcl.lisp      # Implementation-specific
├── impl-ccl.lisp
├── impl-ecl.lisp
└── ...
```

### 2. Shadow Import Pattern

```lisp
;; Export unified API, shadow implementation differences
(defpackage :my-portable-lib
  (:use :cl)
  #+(or sbcl ccl) (:shadow #:some-function)
  (:export #:some-function))

;; Provide portable implementation
(defun some-function (...)
  #+sbcl (sb-ext:some-function ...)
  #+ccl (ccl:some-function ...)
  #-(or sbcl ccl) (portable-fallback ...))
```

### 3. Test Vectors for Crypto

Always include official test vectors:

```lisp
(defsystem :ironclad/tests
  :depends-on (:ironclad :fiveam)
  :components 
  ((:module "testing"
    :components
    ((:static-file "test-vectors/sha256.testvec")
     ...))))
```

### 4. Version Files

Store version in separate file:

```lisp
;; version.sexp
"0.9.0"

;; In .asd
:version (:read-file-form "version.sexp")
```

### 5. Optimization Modules

Structure optimizations separately:

```
src/
├── portable/      # Works everywhere
└── opt/
    ├── sbcl/      # SBCL-specific fast paths
    ├── ccl/       # CCL-specific
    └── ...
```

---

## Comparison

| Aspect | bordeaux-threads | closer-mop | ironclad |
|--------|------------------|------------|----------|
| Purpose | Threading | MOP | Cryptography |
| Approach | Generic functions | Shadow/replace | Pure CL + opts |
| Dependencies | Few utility libs | None | None |
| Complexity | Medium | High | High |
| Optimization | N/A | N/A | SBCL-specific |
