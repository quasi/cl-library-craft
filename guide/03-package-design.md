# Package Design

## Basic Package Template

```lisp
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage :my-library
  (:use :cl)
  (:export
   ;; Main API
   #:create-thing
   #:process-thing
   #:thing-value
   
   ;; Conditions
   #:my-library-error
   #:my-library-warning
   
   ;; Special variables
   #:*default-setting*
   
   ;; Classes (if user-subclassable)
   #:thing
   
   ;; Constants
   #:+max-things+))
```

## Package Naming Conventions

### Primary Package
- Match system name: `:cl-ppcre`, `:hunchentoot`
- Use lowercase with hyphens
- Avoid prefixes like `cl-` unless disambiguating

### Internal Packages
```lisp
(defpackage :my-library.internal
  (:use :cl :my-library)
  (:export #:internal-function))
```

### Nicknames
```lisp
(defpackage :hunchentoot
  (:nicknames :tbnl)  ; Historical nickname
  (:use :cl)
  ...)
```

Use sparingly - nicknames can cause conflicts.

## Export Strategy

### Explicit Export List (Recommended)

```lisp
(:export
 ;; Group by category with comments
 
 ;; Core API
 #:scan
 #:create-scanner
 #:scan-to-strings
 
 ;; Iteration macros
 #:do-scans
 #:do-matches
 
 ;; Conditions
 #:ppcre-error
 #:ppcre-syntax-error
 
 ;; Configuration
 #:*allow-quoting*
 #:*use-bmh-matchers*)
```

### What to Export

**DO export:**
- Public API functions
- User-facing macros
- Condition types (for handling)
- Special variables for configuration
- Classes intended for subclassing
- Constants users need

**DON'T export:**
- Internal utilities
- Implementation details
- Helper functions
- Internal condition slots

### Export Naming Patterns

| Pattern | Usage | Example |
|---------|-------|---------|
| `name` | Primary function/accessor | `scan`, `header-in` |
| `name*` | Variant (often with optional arg) | `cookies-in*` |
| `name-p` | Predicate | `started-p`, `ssl-p` |
| `*name*` | Special variable | `*request*`, `*session*` |
| `+name+` | Constant | `+http-ok+` |
| `define-name` | Defining macro | `define-easy-handler` |
| `with-name` | Context macro | `with-output-to-string` |
| `make-name` | Constructor | `make-instance` |

## Import Patterns

### Minimal Use (Preferred)

```lisp
(defpackage :my-library
  (:use :cl))  ; Only CL
```

### Selective Import

```lisp
(defpackage :my-library
  (:use :cl)
  (:import-from :alexandria
                #:if-let
                #:when-let
                #:with-gensyms))
```

### When to Use vs Import-From

**Use `:use`** for:
- `:cl` (always)
- Closely related packages in same project

**Use `:import-from`** for:
- Specific utilities from libraries
- Avoiding namespace pollution
- Making dependencies explicit

## Internal Symbol Access

### Double-Colon Access

```lisp
;; Within library code, access internal symbols
(my-library::internal-function arg)
```

### Shadow/Shadowing-Import

```lisp
(defpackage :my-library
  (:use :cl)
  (:shadow #:find))  ; Shadow CL:FIND
```

Use sparingly - can cause confusion.

## Package Patterns from Examples

### CL-PPCRE Pattern

Single package, everything exported explicitly:

```lisp
(defpackage :cl-ppcre
  (:nicknames :ppcre)
  (:use :cl)
  (:export #:create-scanner
           #:scan
           #:scan-to-strings
           ;; ... 40+ exports organized by category
           ))
```

### Hunchentoot Pattern

Main package plus internal reexports:

```lisp
(defpackage :hunchentoot
  (:use :cl :url-rewrite)  ; Uses own url-rewrite submodule
  (:export 
   ;; 150+ exports
   ;; Organized: Acceptors, Requests, Replies, Sessions, etc.
   ))
```

### Multi-Package Library

For very large libraries:

```lisp
;; Core functionality
(defpackage :big-library.core ...)

;; Extensions
(defpackage :big-library.ext ...)

;; Main user-facing package
(defpackage :big-library
  (:use :cl)
  (:use :big-library.core :big-library.ext)
  (:reexport ;; not standard, but pattern exists
   :big-library.core
   :big-library.ext))
```

## Best Practices

1. **One primary package per system**: Simple, clear API
2. **Explicit exports**: List everything, group by category
3. **Minimal :use**: Just `:cl` for most libraries
4. **Import-from for utilities**: Makes deps explicit
5. **Document exports**: Docstrings on all exported symbols
6. **Stable API surface**: Think carefully before exporting
7. **Naming conventions**: Follow CL conventions (*earmuffs*, +CONSTANTS+)
8. **Order exports logically**: By category, not alphabetically

## Anti-Patterns

1. **Export everything**: `(:export . #.(loop for s being the symbols ...))`
2. **`:use` too many packages**: Namespace pollution
3. **Conflicting nicknames**: Collisions with other libraries
4. **Undocumented exports**: API without explanation
5. **Changing exports without notice**: Breaking user code
