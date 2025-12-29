# Fukamachi Libraries (Mito, SXQL, Woo)

## Overview

Eitaro Fukamachi represents a **modern CL style** distinct from the Edi Weitz tradition. His libraries form a cohesive web development stack.

| Library | Purpose | Stars |
|---------|---------|-------|
| **Mito** | ORM with migrations | ~260 |
| **SXQL** | SQL query builder | ~330 |
| **Woo** | Fast HTTP server | ~1,200 |

## Common Patterns Across Fukamachi Libraries

### Structure: src/ Module Organization

```
library/
├── library.asd
├── src/
│   ├── library.lisp      # Main entry, re-exports
│   ├── package.lisp      # Package definition
│   ├── feature/          # Feature modules
│   │   ├── subfeature.lisp
│   │   └── ...
│   └── util.lisp
├── t/                    # Tests (not test/)
│   ├── package.lisp
│   └── tests.lisp
└── README.markdown
```

### ASDF: Module-Based Organization

```lisp
(defsystem "sxql"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:trivia :iterate :cl-annot :trivial-types
               :split-sequence :named-readtables :alexandria
               :cl-package-locks)
  :components ((:module "src"
                :components
                ((:file "sxql" :depends-on ("statement" ...))
                 (:file "compile" :depends-on ("sql-type" "syntax"))
                 (:file "sql-type" :depends-on ("syntax"))
                 ...))))
```

### Dependency Philosophy

More dependencies than traditional CL:
- `trivia` - Pattern matching
- `iterate` - Loop macro
- `cl-annot` - Annotations
- `named-readtables` - Custom syntax
- `alexandria` - Utilities

---

## SXQL - SQL Query Builder

### Core API

```lisp
;; S-expression SQL
(select :*
  (from :users)
  (where (:= :active 1))
  (order-by :name)
  (limit 10))
;; => #<SXQL-STATEMENT: SELECT * FROM users WHERE active = 1 
;;                       ORDER BY name LIMIT 10>

;; Compile to SQL
(yield *)
;; => "SELECT * FROM users WHERE (active = ?) ORDER BY name LIMIT ?"
;;    (1 10)  ; placeholders

;; Full compilation (no placeholders)
(let ((sxql:*use-placeholder* nil))
  (yield (select :* (from :users))))
;; => "SELECT * FROM users"
```

### Query Composition

```lisp
;; Immutable composition with threading macro
(-> (select :*)
    (from :users)
    (where (:= :active 1))
    (order-by :name))

;; Conditional building
(let ((query (select :* (from :users))))
  (when filter-active
    (setf query (merge-queries query (where (:= :active 1)))))
  query)
```

### Operator System

```lisp
;; Operators are functions
(:= :column value)      ; column = value
(:like :name "%foo%")   ; name LIKE '%foo%'
(:in :id '(1 2 3))      ; id IN (1, 2, 3)
(:and expr1 expr2)      ; expr1 AND expr2
(:or expr1 expr2)       ; expr1 OR expr2
(:raw "custom SQL")     ; Raw SQL insertion
```

---

## Mito - ORM

### Table Definition

```lisp
(mito:deftable user ()
  ((name :col-type (:varchar 64))
   (email :col-type (:varchar 128)))
  (:unique-keys email))

;; Generates:
;; CREATE TABLE user (
;;   id BIGSERIAL NOT NULL PRIMARY KEY,
;;   name VARCHAR(64) NOT NULL,
;;   email VARCHAR(128) NOT NULL,
;;   created_at TIMESTAMP,
;;   updated_at TIMESTAMP,
;;   UNIQUE (email)
;; )
```

### Automatic Timestamps

Mito adds `id`, `created_at`, `updated_at` automatically.

### CRUD Operations

```lisp
;; Create
(mito:create-dao 'user :name "Alice" :email "alice@example.com")

;; Read
(mito:find-dao 'user :id 1)
(mito:find-dao 'user :email "alice@example.com")

;; Update
(setf (slot-value user 'name) "Alice Smith")
(mito:save-dao user)

;; Delete
(mito:delete-dao user)

;; Query with SXQL
(mito:select-dao 'user
  (sxql:where (:like :email "%@example.com")))
```

### Migrations

```lisp
;; Generate migration
(mito:migrate-table 'user)

;; Applies ALTER TABLE statements automatically
;; when table definition changes
```

### Relationships

```lisp
(mito:deftable tweet ()
  ((status :col-type :text)
   (user :col-type user)))  ; Foreign key reference

;; Query with relationship
(mito:create-dao 'tweet :user *user* :status "Hello")
(mito:find-dao 'tweet :user *user*)
```

---

## Woo - HTTP Server

### High Performance Focus

Woo is designed for speed:
- Uses libev for event loop
- Zero-copy where possible
- SBCL optimizations

### Basic Usage

```lisp
(woo:run
  (lambda (env)
    '(200 (:content-type "text/plain") ("Hello, World!")))
  :port 8080)
```

### ASDF Pattern

Heavy use of feature conditionals:

```lisp
(defsystem "woo"
  :version "0.12.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("lev"
               "clack-socket"
               "swap-bytes"
               "cffi"
               "static-vectors"
               "bordeaux-threads"
               "fast-http"
               "quri"
               "fast-io"
               "smart-buffer"
               "trivial-utf-8"
               "trivial-mimes"
               "vom"
               "alexandria"
               (:feature :sbcl "sb-posix")
               (:feature :sbcl "sb-concurrency")
               (:feature (:not :sbcl) "cl-speedy-queue")
               (:feature (:not :woo-no-ssl) "cl+ssl"))
  ...)
```

---

## Lessons for AI Code Generation

### 1. Modern CL Style

Fukamachi represents a more "batteries included" approach:

```lisp
;; Use pattern matching
(trivia:match result
  ((list status headers body) ...))

;; Use iterate for complex loops
(iterate:iterate
  (iterate:for row in-query query)
  (iterate:collect row))
```

### 2. Immutable Query Building

For SQL builders, prefer immutable composition:

```lisp
;; Bad: mutating query
(add-where query condition)

;; Good: returning new query
(-> query (where condition))
```

### 3. Automatic Conveniences

ORMs should provide:
- Auto-incrementing IDs
- Timestamps (created_at, updated_at)
- Migrations
- Relationship handling

### 4. Pluggable Backends

Abstract database differences:

```lisp
(ecase (mito:driver-type conn)
  (:postgres ...)
  (:mysql ...)
  (:sqlite3 ...))
```

### 5. Performance-Critical Systems

For servers, use:
- CFFI for OS integration
- Static vectors for zero-copy
- SBCL-specific optimizations
- Feature conditionals for portability

---

## Comparison: Fukamachi vs Edi Weitz Style

| Aspect | Fukamachi | Edi Weitz |
|--------|-----------|-----------|
| Dependencies | Many small libs | Few, well-chosen |
| Structure | src/ module tree | Flat files |
| ASDF | :module + :depends-on | :serial t |
| Features | Pattern matching, iterate | Standard CL |
| Tests | t/ directory | test/ directory |
| Documentation | README.md | HTML docs |
| Package style | Single, focused | Single, comprehensive |

Both approaches are valid - Edi Weitz is more conservative, Fukamachi more modern/experimental.
