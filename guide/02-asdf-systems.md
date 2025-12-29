# ASDF System Definition

## Basic System Template

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(defsystem :my-library
  :version "1.0.0"
  :description "Clear one-line description of what library does"
  :author "Your Name <email@example.com>"
  :license "BSD-2-Clause"
  :serial t
  :depends-on (:alexandria
               :bordeaux-threads)
  :components ((:file "packages")
               (:file "specials")
               (:file "conditions")
               (:file "util")
               (:file "my-library"))
  :in-order-to ((test-op (test-op :my-library/test))))
```

## System Metadata

### Required Fields
- `:description` - Clear, one-line description
- `:license` - SPDX identifier preferred (BSD-2-Clause, MIT, Apache-2.0)

### Recommended Fields
- `:version` - Semantic versioning (MAJOR.MINOR.PATCH)
- `:author` - Name and email

### Optional Fields
- `:maintainer` - If different from author
- `:homepage` - Project URL
- `:bug-tracker` - Issue tracker URL
- `:source-control` - Repository URL

## Component Ordering

### Serial Loading (`:serial t`)

Simplest approach - each file loads after the previous:

```lisp
:serial t
:components ((:file "packages")
             (:file "specials")
             (:file "conditions")
             (:file "util")
             (:file "core")
             (:file "api"))
```

**Use when**: Order is strictly linear, most libraries.

### Explicit Dependencies

For complex dependency graphs:

```lisp
:components ((:file "packages")
             (:file "specials" :depends-on ("packages"))
             (:file "conditions" :depends-on ("packages"))
             (:file "util" :depends-on ("packages" "specials"))
             (:file "core" :depends-on ("util" "conditions"))
             (:file "api" :depends-on ("core")))
```

**Use when**: Parallel loading desired, circular dependencies to break.

### Module Organization

For subdirectories:

```lisp
:components ((:file "packages")
             (:module "core"
               :serial t
               :components ((:file "base")
                           (:file "advanced")))
             (:module "extensions"
               :depends-on ("core")
               :components ((:file "ext1")
                           (:file "ext2"))))
```

## Conditional Loading

### Feature-Based Loading

```lisp
:components ((:file "packages")
             (:file "portable-base")
             (:file "sbcl-optimizations" :if-feature :sbcl)
             (:file "lispworks-compat" :if-feature :lispworks)
             (:file "fallback" :if-feature (:not (:or :sbcl :lispworks))))
```

### Optional SSL Example (Hunchentoot)

```lisp
:depends-on (:flexi-streams
             :chunga
             (:feature (:not (:or :lispworks :hunchentoot-no-ssl))
                       :cl+ssl))
```

## Test System Definition

### Separate Test System

```lisp
(defsystem :my-library/test
  :description "Tests for my-library"
  :license "BSD-2-Clause"
  :depends-on (:my-library
               :fiveam)  ; or :parachute, :rove
  :components ((:module "test"
                :serial t
                :components ((:file "packages")
                            (:file "test-core")
                            (:file "test-api"))))
  :perform (test-op (op c)
             (symbol-call :fiveam :run! :my-library-tests)))
```

### Connecting Main System to Tests

```lisp
;; In main system definition
:in-order-to ((test-op (test-op :my-library/test)))
```

## Dependency Patterns

### Minimal Dependencies (Preferred)

```lisp
:depends-on ()  ; No deps if possible
```

### Common Utility Dependencies

```lisp
:depends-on (:alexandria)  ; Widely used, stable utilities
```

### Portability Dependencies

```lisp
:depends-on (:bordeaux-threads  ; Threading
             :usocket           ; Networking
             :flexi-streams)    ; Stream handling
```

### Feature-Specific Dependencies

```lisp
:depends-on (:cl-ppcre         ; Regex
             :local-time       ; Date/time
             :cl-json          ; JSON
             :dexador)         ; HTTP client
```

## Real-World Examples

### CL-PPCRE System

```lisp
(defsystem :cl-ppcre
  :version "2.1.2"
  :description "Perl-compatible regular expression library"
  :author "Dr. Edi Weitz"
  :license "BSD"
  :serial t
  :components ((:file "packages")
               (:file "specials")
               (:file "util")
               (:file "errors")
               (:file "charset")
               (:file "charmap")
               (:file "chartest")
               (:file "lexer")
               (:file "parser")
               (:file "regex-class")
               (:file "regex-class-util")
               (:file "convert")
               (:file "optimize")
               (:file "closures")
               (:file "repetition-closures")
               (:file "scanner")
               (:file "api"))
  :in-order-to ((test-op (test-op :cl-ppcre/test))))
```

### Hunchentoot System (Complex)

```lisp
(defsystem :hunchentoot
  :version "1.3.1"
  :serial t
  :description "Web server written in Common Lisp"
  :depends-on (:chunga
               :cl-base64
               :cl-fad
               :cl-ppcre
               :flexi-streams
               :md5
               :rfc2388
               :trivial-backtrace
               (:feature (:not (:or :lispworks :hunchentoot-no-ssl))
                         :cl+ssl)
               (:feature (:not :lispworks) :usocket)
               (:feature (:not :lispworks) :bordeaux-threads))
  :components ((:module "url-rewrite"
                 :serial t
                 :components ((:file "packages")
                              (:file "specials")
                              (:file "primitives")
                              (:file "util")
                              (:file "url-rewrite")))
               (:file "packages")
               (:file "lispworks" :if-feature :lispworks)
               (:file "compat" :if-feature (:not :lispworks))
               (:file "specials")
               (:file "conditions")
               (:file "mime-types")
               (:file "util")
               ;; ... more files
               ))
```

## Best Practices

1. **Version semantically**: MAJOR.MINOR.PATCH
2. **Order matters**: packages → specials → conditions → util → implementation
3. **Use `:serial t`** unless you need fine-grained control
4. **Separate test systems**: Keep test deps out of main system
5. **Document conditionals**: Comment why feature conditionals exist
6. **Minimal deps**: Each dependency is a maintenance burden
7. **Pin to known-good versions** in production (via Quicklisp dist pins)
