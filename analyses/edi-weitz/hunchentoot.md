# Hunchentoot Analysis

**Repository**: https://github.com/edicl/hunchentoot  
**Author**: Dr. Edmund Weitz (Edi Weitz)  
**Type**: Web server and toolkit  
**Size**: Large (~5000+ LOC)  
**Stars**: 731 (as of analysis)

## Overview

Hunchentoot is a full-featured HTTP/1.1 web server written in Common Lisp. It serves as both a standalone server and a framework for building web applications. Originally "TBNL" (To Be Named Later), it evolved into Hunchentoot around 2005.

## Project Structure

```
hunchentoot/
├── hunchentoot.asd       # System definition
├── packages.lisp         # Package definition
├── specials.lisp         # Special variables, parameters
├── conditions.lisp       # Error conditions
├── mime-types.lisp       # MIME type mapping
├── util.lisp             # Utility functions
├── log.lisp              # Logging functionality
├── cookie.lisp           # Cookie handling
├── headers.lisp          # HTTP header handling
├── request.lisp          # Request object
├── reply.lisp            # Reply object
├── session.lisp          # Session management
├── acceptor.lisp         # Connection acceptor
├── taskmaster.lisp       # Threading/connection management
├── easy-handlers.lisp    # Simplified handler framework
├── ssl.lisp              # SSL support
├── set-timeouts.lisp     # Timeout handling
├── compat.lisp           # Non-LispWorks compatibility
├── lispworks.lisp        # LispWorks-specific code
├── url-rewrite/          # Embedded URL rewriting module
├── docs/                 # HTML documentation
├── test/                 # Test suite
├── README.md
├── CHANGELOG
└── LICENSE (BSD)
```

### Observations

- **Flat structure**: All source files at root level
- **Purpose-based naming**: Clear file names (cookie.lisp, session.lisp)
- **Embedded module**: url-rewrite/ as internal submodule
- **Separate docs**: Full HTML documentation

## ASDF System

```lisp
(defsystem :hunchentoot
  :version "1.3.1"
  :serial t
  :description "Web server written in Common Lisp"
  :depends-on (:chunga :cl-base64 :cl-fad :cl-ppcre :flexi-streams
               :md5 :rfc2388 :trivial-backtrace
               (:feature (:not (:or :lispworks :hunchentoot-no-ssl)) :cl+ssl)
               (:feature (:not :lispworks) :usocket)
               (:feature (:not :lispworks) :bordeaux-threads))
  :components ((:module "url-rewrite" ...)
               (:file "packages")
               (:file "lispworks" :if-feature :lispworks)
               (:file "compat" :if-feature (:not :lispworks))
               (:file "specials")
               (:file "conditions")
               ...))
```

### Notable Patterns

1. **Conditional dependencies**: SSL, threading based on implementation
2. **Feature-based loading**: LispWorks vs other implementations
3. **Embedded module**: url-rewrite loaded as module
4. **Serial loading**: `:serial t` for simplicity

## Package Design

Single package `:hunchentoot` with ~150 exports organized by category:

- Acceptors (acceptor, ssl-acceptor, start, stop)
- Requests (request, *request*, parameter, header-in, etc.)
- Replies (reply, *reply*, header-out, content-type*, etc.)
- Sessions (session, start-session, session-value, etc.)
- Handlers (define-easy-handler, dispatch functions)
- Utilities (url-encode, escape-for-html, etc.)
- Conditions (hunchentoot-error, parameter-error, etc.)

### Export Naming Patterns

| Pattern | Example | Meaning |
|---------|---------|---------|
| `*name*` | `*request*`, `*reply*` | Current context object |
| `name*` | `header-in*`, `cookies-out*` | Variant using *request*/*reply* |
| `name-p` | `started-p`, `ssl-p` | Predicate |
| `+name+` | `+http-ok+`, `+http-not-found+` | HTTP status constants |
| `define-name` | `define-easy-handler` | Macro for definitions |

## API Design Highlights

### Dual Accessor Pattern

```lisp
;; Generic reader (for subclassing)
(defgeneric header-in (name request)
  (:documentation "Return header NAME from REQUEST."))

;; Convenience function (uses *request*)
(defun header-in* (name &optional (request *request*))
  "Return header NAME from current REQUEST."
  (header-in name request))
```

This pattern appears throughout: `cookies-in`/`cookies-in*`, `content-type`/`content-type*`, etc.

### Extensibility via CLOS

```lisp
;; Users can subclass
(defclass my-acceptor (hunchentoot:acceptor)
  ((custom-slot :accessor custom-slot)))

;; And specialize
(defmethod acceptor-dispatch-request ((acceptor my-acceptor) request)
  ;; Custom dispatch
  (call-next-method))  ; Or fall through
```

### Handler Framework

```lisp
(define-easy-handler (greet :uri "/greet") (name)
  (setf (content-type*) "text/plain")
  (format nil "Hello, ~A!" (or name "World")))
```

## Error Handling

### Condition Hierarchy

```lisp
hunchentoot-condition
├── hunchentoot-error (error)
│   └── parameter-error
└── hunchentoot-warning (warning)
```

### Error Control Variables

```lisp
*catch-errors-p*       ; Whether to catch handler errors
*show-lisp-errors-p*   ; Show errors in HTML output
*log-lisp-errors-p*    ; Log errors
*log-lisp-backtraces-p* ; Include backtraces in log
```

## Session Management

- Cookie-based session tracking
- URL rewriting fallback for cookie-less clients
- Configurable timeout (`*session-max-time*`)
- Garbage collection (`*session-gc-frequency*`)
- Secret-based verification (`*session-secret*`)

## Key Design Decisions

### 1. Generic Functions for Customization
Everything users might want to customize is a generic function with a default method.

### 2. Special Variables for Context
`*request*`, `*reply*`, `*session*`, `*acceptor*` provide implicit context to handlers.

### 3. Layered API
- Low-level: Direct request/reply manipulation
- Mid-level: Handler registration
- High-level: `define-easy-handler` DSL

### 4. LispWorks-First with Portability
Written for LispWorks originally, compat.lisp provides abstraction for others.

### 5. Comprehensive Documentation
HTML docs with examples for every exported symbol.

## Patterns to Adopt

1. **Dual accessor pattern** (foo/foo*) for context-aware functions
2. **Generic functions for extension points**
3. **Layered complexity** (simple API over powerful primitives)
4. **Configurable via special variables**
5. **Comprehensive condition hierarchy**
6. **Detailed HTML documentation with examples**

## Patterns to Consider

1. **Flat file structure** works but may not scale
2. **Embedded modules** (url-rewrite) adds complexity
3. **Many exports** (~150) - consider if all are needed
4. **LispWorks-centric** - may need more abstraction for other impls
