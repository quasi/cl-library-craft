# Author Style Comparison

This document compares the coding styles of prominent CL library authors to help AI assistants adapt their code generation to match project conventions.

## Style Families

### 1. Edi Weitz Style (Traditional Excellence)

**Libraries**: Hunchentoot, CL-PPCRE, Drakma, CL-WHO, Flexi-Streams

**Characteristics**:
- Flat file structure at project root
- `:serial t` in ASDF (implicit dependency order)
- Single comprehensive package per library
- Minimal external dependencies
- Extensive HTML documentation
- BSD licensing

```lisp
;; Typical ASDF
(defsystem :my-lib
  :serial t
  :license "BSD"
  :components ((:file "packages")
               (:file "specials")
               (:file "conditions")
               (:file "util")
               (:file "main")))

;; Typical package
(defpackage :my-lib
  (:use :cl)
  (:export #:main-function
           #:main-function*     ; Variant
           #:*default-setting*  ; Configuration
           #:my-lib-error))     ; Conditions
```

**When to use**: General-purpose libraries, when stability and minimal dependencies matter.

---

### 2. Marijn Haverbeke Style (Layered Systems)

**Libraries**: Postmodern, S-SQL, CL-Postgres

**Characteristics**:
- Multiple independent systems that compose
- Explicit `:depends-on` between files
- Tiered packages with re-exports
- Compile-time DSL processing
- Documentation in separate HTML files

```lisp
;; Multiple .asd files at root
postmodern.asd      ; High-level, depends on others
s-sql.asd           ; DSL layer
cl-postgres.asd     ; Low-level protocol

;; Explicit dependencies
(:file "config" :depends-on ("package"))
(:file "main" :depends-on ("package" "config" "util"))
```

**When to use**: Complex systems with multiple usage levels, database interfaces, DSLs.

---

### 3. Fukamachi Style (Modern Web)

**Libraries**: Mito, SXQL, Woo, Clack, Lack

**Characteristics**:
- `src/` directory organization
- Heavy use of `:module` in ASDF
- Many small dependencies (trivia, iterate, etc.)
- Pattern matching and modern idioms
- Tests in `t/` directory
- README.md documentation

```lisp
;; Module-based ASDF
(defsystem "my-lib"
  :depends-on (:trivia :iterate :alexandria)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "main" :depends-on ("package"))))))

;; Modern idioms
(trivia:match result
  ((list :ok value) value)
  ((list :error msg) (error msg)))
```

**When to use**: Web applications, ORMs, performance-critical servers.

---

### 4. Portability Library Style

**Libraries**: bordeaux-threads, closer-mop, usocket

**Authors**: Stelian Ionescu, Pascal Costanza, various

**Characteristics**:
- Implementation-specific files (impl-sbcl.lisp, impl-ccl.lisp)
- Shadow/replace packages
- Extensive feature conditionals
- Generic function dispatch for implementations
- No external dependencies (for core)

```lisp
;; Implementation dispatch structure
(defsystem :my-portable-lib
  :serial t
  :components
  ((:file "packages")
   (:file "shared")
   #+sbcl (:file "impl-sbcl")
   #+ccl (:file "impl-ccl")
   #+ecl (:file "impl-ecl")))

;; Shadow imports
(defpackage :my-lib
  (:use :cl)
  #+(or sbcl ccl) (:shadow #:some-symbol)
  ...)
```

**When to use**: Cross-implementation compatibility layers, system-level functionality.

---

### 5. Sharplispers Style (Community Maintained)

**Libraries**: ironclad, cl-json, split-sequence

**Characteristics**:
- Often originally by one author, now community maintained
- Extensive test suites with external test vectors
- Pure CL with optional optimizations
- Careful backwards compatibility
- Detailed NEWS/CHANGELOG files

```lisp
;; Separate optimization modules
(:module "src"
 :components
 ((:file "portable")
  (:module "opt"
   :components
   (#+sbcl (:file "sbcl-optimizations")))))
```

**When to use**: Foundational libraries, cryptography, data structures.

---

## Decision Matrix

| Project Type | Recommended Style |
|-------------|-------------------|
| Utility library | Edi Weitz |
| Web framework | Fukamachi |
| Database interface | Haverbeke |
| Portability layer | Portability |
| Algorithm library | Sharplispers |
| Small focused tool | Edi Weitz |
| Full-stack web app | Fukamachi |

## Style Markers

When analyzing an existing project, look for these markers:

| Marker | Indicates |
|--------|-----------|
| `:serial t` | Edi Weitz style |
| Multiple .asd files | Haverbeke style |
| `src/` directory | Fukamachi style |
| `impl-*.lisp` files | Portability style |
| `test-vectors/` | Sharplispers style |
| `:trivia` dependency | Fukamachi style |
| HTML docs in `doc/` | Edi Weitz style |
| README.md only | Fukamachi style |

## Mixing Styles

It's acceptable to mix styles when appropriate:

```lisp
;; Edi Weitz base + Haverbeke modular systems
(defsystem :my-lib
  :serial t  ; Edi Weitz simplicity
  ...)

(defsystem :my-lib/extra  ; Haverbeke layering
  :depends-on (:my-lib)
  ...)

;; Fukamachi modern idioms in Edi Weitz structure
(defsystem :my-lib
  :serial t
  :depends-on (:alexandria :trivia)  ; Modern deps
  :components ...)
```

## Anti-Patterns to Avoid

1. **Mixing serial and explicit deps**: Pick one
2. **Multiple packages without clear boundaries**: One package unless needed
3. **Over-modularization**: Don't create tiny files
4. **Under-documentation**: Match documentation to library complexity
5. **Ignoring implementation differences**: Use feature conditionals properly
