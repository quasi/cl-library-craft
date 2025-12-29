# Postmodern

## Overview

| Attribute | Value |
|-----------|-------|
| **Author** | Marijn Haverbeke (original), Sabra Crolleton (maintainer) |
| **Repository** | https://github.com/marijnh/Postmodern |
| **Stars** | ~423 |
| **License** | zlib |
| **Size** | ~15,000+ LOC across 4 systems |
| **Purpose** | PostgreSQL programming interface |

## Architecture: Multi-System Design

Postmodern demonstrates a **modular multi-system architecture** - four independent but coordinated systems:

```
postmodern/              # Root (multiple .asd files at root)
├── postmodern.asd       # High-level wrapper
├── cl-postgres.asd      # Low-level PostgreSQL protocol
├── s-sql.asd            # SQL DSL compiler
├── simple-date.asd      # Date/time types
├── postmodern/          # Module directory
│   ├── package.lisp
│   ├── config.lisp
│   ├── connect.lisp
│   ├── query.lisp
│   └── ...
├── cl-postgres/         # Module directory
├── s-sql/               # Module directory
├── simple-date/         # Module directory
└── doc/                 # HTML documentation
```

### Key Insight: Layered Independence

Each system can be used independently:
- **cl-postgres**: Direct PostgreSQL protocol (use alone for raw access)
- **s-sql**: SQL DSL (use with any database interface)
- **simple-date**: Date types (use anywhere)
- **postmodern**: High-level ORM combining all

## ASDF Patterns

### Multi-System Coordination

```lisp
;; postmodern.asd - wraps everything
(defsystem "postmodern"
  :depends-on ("alexandria" "cl-postgres" "s-sql" 
               "global-vars" "split-sequence" "uiop"
               (:feature :postmodern-use-mop "closer-mop")
               (:feature :postmodern-thread-safe "bordeaux-threads"))
  :components ((:module "postmodern" ...)))

;; cl-postgres.asd - standalone low-level
(defsystem "cl-postgres"
  :depends-on ("md5" "split-sequence" "ironclad" "cl-base64" "uax-15"
               (:feature (:or :allegro :ccl :clisp ...) "usocket")
               (:feature :sbcl (:require :sb-bsd-sockets)))
  :components ...)

;; s-sql.asd - depends only on cl-postgres
(defsystem "s-sql"
  :depends-on ("cl-postgres" "alexandria")
  :components ((:module "s-sql" ...)))
```

### Feature-Controlled Dependencies

Extensive use of `:feature` for optional functionality:
- MOP support: `:postmodern-use-mop`
- Thread safety: `:postmodern-thread-safe`
- Socket libraries vary by implementation

### Explicit Dependencies (Not Serial)

Unlike Edi Weitz's `:serial t`, uses explicit `:depends-on`:

```lisp
(:file "config" :depends-on ("package"))
(:file "s-sql" :depends-on ("package" "config"))
```

## Package Design

### Tiered Package Strategy

Each system has its own package, with the main `postmodern` re-exporting:

```lisp
;; cl-postgres package - low-level
(defpackage :cl-postgres
  (:use :cl)
  (:export #:database-connection
           #:open-database
           #:exec-query
           ...))

;; s-sql package - SQL DSL
(defpackage :s-sql
  (:use :cl)
  (:export #:sql #:sql-compile
           #:select #:insert-into #:update
           ...))

;; postmodern package - high-level, re-exports
(defpackage :postmodern
  (:use :cl :s-sql :cl-postgres)
  (:export #:connect #:disconnect
           #:query #:execute
           #:dao-class #:get-dao
           ;; Re-export from s-sql
           #:select #:insert-into
           ...))
```

## API Design

### S-SQL: Compile-Time SQL Generation

```lisp
;; S-expressions compile to SQL at macro-expansion time
(sql (:select 'name 'age :from 'person :where (:> 'age 18)))
;; => "SELECT name, age FROM person WHERE (age > 18)"

;; Keywords become SQL keywords
;; Symbols become identifiers
;; Lisp values are escaped
```

### Query Result Formats

Rich result format options via keyword argument:

```lisp
(query "SELECT * FROM users" :rows)      ; List of lists
(query "SELECT * FROM users" :plists)    ; List of plists
(query "SELECT * FROM users" :alists)    ; List of alists
(query "SELECT name FROM users" :column) ; Single column
(query "SELECT count(*) FROM users" :single) ; Single value
(query "SELECT * FROM users" :dao 'user) ; DAO objects
```

### DAO (Database Access Object) Pattern

```lisp
(defclass country ()
  ((name :col-type string :initarg :name :reader country-name)
   (inhabitants :col-type integer :accessor country-inhabitants)
   (sovereign :col-type (or db-null string) :accessor country-sovereign))
  (:metaclass dao-class)
  (:keys name))

;; Automatic CRUD
(get-dao 'country "Croatia")
(insert-dao (make-instance 'country :name "France" ...))
(update-dao country-instance)
```

### Connection Management

Multiple strategies for different use cases:

```lisp
;; Development: single persistent connection
(connect-toplevel "testdb" "user" "pass" "localhost")

;; Production: lexically scoped
(with-connection '("testdb" "user" "pass" "localhost")
  ...)

;; Pooled connections
(with-connection '("testdb" "user" "pass" "localhost" :pooled-p t)
  ...)
```

## Notable Patterns

### 1. Compile-Time SQL (S-SQL)

SQL is generated at compile time, not runtime:

```lisp
(defmacro sql (form)
  `(sql-compile ',form))

;; Errors caught at compile time
;; No runtime parsing overhead
```

### 2. Read Table Customization

S-SQL uses custom reader syntax:

```lisp
;; $1, $2 for prepared statement parameters
(defprepared get-user
  (:select '* :from 'users :where (:= 'id '$1))
  :single)
```

### 3. Type Integration

Seamless Lisp ↔ PostgreSQL type conversion:

```lisp
;; Rationals preserved
(query "SELECT 78239/100") ;; => 78239/100 (ratio)

;; Arrays work naturally
(query "SELECT ARRAY[1,2,3]") ;; => #(1 2 3)

;; Optional date library integration
(ql:quickload :simple-date/postgres-glue)
```

### 4. Transaction Macros

```lisp
(with-transaction ()
  (execute (:update 'accounts ...))
  (execute (:update 'accounts ...)))

;; Savepoints
(with-transaction ()
  (with-savepoint sp
    (execute dangerous-operation)
    (abort-savepoint sp)))  ; Rollback to savepoint
```

## Testing

```lisp
;; Separate test systems for each component
(defsystem "postmodern/tests"
  :depends-on ("postmodern" "fiveam")
  ...)

(defsystem "cl-postgres/tests" ...)
(defsystem "s-sql/tests" ...)
```

## Lessons for AI Code Generation

### Multi-System Architecture

When building complex libraries:

1. **Layer independently usable systems**
2. **Higher levels compose lower levels**
3. **Each system has clear responsibilities**
4. **Allow using just parts you need**

### Compile-Time Processing

For DSLs, prefer compile-time processing:

```lisp
;; Good: Compile-time SQL generation
(defmacro sql (form) `(sql-compile ',form))

;; Allows: Static analysis, early errors, no runtime overhead
```

### Result Flexibility

Provide multiple result formats:

```lisp
(query sql :rows)   ; Raw data
(query sql :plists) ; Convenient for processing
(query sql :dao class) ; ORM integration
```

### Connection Pattern Variety

Support different usage patterns:

```lisp
;; Simple/REPL: connect-toplevel
;; Production: with-connection
;; High-load: pooled connections
```

## Comparison to Edi Weitz Style

| Aspect | Postmodern | Edi Weitz |
|--------|------------|-----------|
| Structure | Multi-system | Single system |
| Dependencies | Explicit | Serial |
| Packages | Tiered/re-export | Single package |
| Documentation | Comprehensive HTML | Comprehensive HTML |
| Focus | Compile-time DSL | Runtime flexibility |
