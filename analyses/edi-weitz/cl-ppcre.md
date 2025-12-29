# CL-PPCRE Analysis

**Repository**: https://github.com/edicl/cl-ppcre  
**Author**: Dr. Edmund Weitz (Edi Weitz)  
**Type**: Regular expression library  
**Size**: Medium (~3000 LOC)  
**Stars**: 316 (as of analysis)

## Overview

CL-PPCRE (Portable Perl-compatible Regular Expressions) is a high-performance regex library for Common Lisp. Notable for:
- Perl compatibility
- S-expression syntax alternative
- Thread safety
- No external dependencies
- Extensive optimization

## Project Structure

```
cl-ppcre/
├── cl-ppcre.asd          # Main system
├── cl-ppcre-unicode.asd  # Unicode extension system
├── packages.lisp         # Package definition
├── specials.lisp         # Special variables
├── errors.lisp           # Error conditions (named "errors" not "conditions")
├── util.lisp             # Utilities
├── charset.lisp          # Character set handling
├── charmap.lisp          # Character map optimization
├── chartest.lisp         # Character testing
├── lexer.lisp            # Regex string lexer
├── parser.lisp           # Regex parser
├── regex-class.lisp      # Internal regex representation
├── regex-class-util.lisp # Regex class utilities
├── convert.lisp          # Parse tree conversion
├── optimize.lisp         # Regex optimization
├── closures.lisp         # Matcher closures
├── repetition-closures.lisp # Repetition handling
├── scanner.lisp          # Scanner creation
├── api.lisp              # Public API
├── cl-ppcre-unicode/     # Unicode support extension
├── docs/                 # HTML documentation
├── test/                 # Test suite
├── LICENSE
├── CHANGELOG
└── README.md
```

### Observations

- **Compiler architecture**: lexer → parser → convert → optimize → closures
- **Clear separation**: api.lisp is separate from implementation
- **Extension pattern**: cl-ppcre-unicode as optional add-on
- **No external deps**: Fully self-contained

## ASDF System

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

### Notable Patterns

1. **Zero dependencies**: No :depends-on
2. **Clean compilation pipeline**: Files ordered by processing stage
3. **Separate test system**: Using :in-order-to
4. **Extension system**: cl-ppcre-unicode separate

## Package Design

Single package `:cl-ppcre` (nickname `:ppcre`) with focused exports:

### Core Functions (~20 exports)
- `create-scanner` - Create regex scanner
- `scan` - Match regex
- `scan-to-strings` - Match returning strings
- `split` - Split string by regex
- `regex-replace` / `regex-replace-all` - Substitution

### Iteration Macros
- `do-scans`, `do-matches`, `do-matches-as-strings`
- `do-register-groups`
- `register-groups-bind`

### Utility Functions
- `parse-string` - Convert string to parse tree
- `quote-meta-chars` - Escape metacharacters
- `regex-apropos` / `regex-apropos-list`

### Configuration Variables
- `*allow-quoting*`, `*allow-named-registers*`
- `*use-bmh-matchers*`, `*optimize-char-classes*`
- `*regex-char-code-limit*`

### Conditions
- `ppcre-error`, `ppcre-invocation-error`, `ppcre-syntax-error`

## API Design Highlights

### Dual Input Syntax

```lisp
;; String syntax (Perl-compatible)
(scan "(a+)b" "aaab")

;; S-expression syntax (Lispy)
(scan '(:sequence 
         (:register 
           (:greedy-repetition 1 nil #\a))
         #\b)
      "aaab")
```

Both compile to same internal representation.

### Generic Function Dispatch

```lisp
(defgeneric create-scanner (regex &key ...)
  (:documentation "..."))

(defmethod create-scanner ((regex string) &key ...)
  "Parse string as Perl regex.")

(defmethod create-scanner ((regex function) &key ...)
  "Return existing scanner as-is.")

(defmethod create-scanner ((regex t) &key ...)
  "Interpret as parse tree.")
```

### Multiple Return Values

```lisp
(scan "(a+)" "xaaab")
;; => 1        ; match-start
;;    4        ; match-end  
;;    #(1)     ; register-starts
;;    #(4)     ; register-ends
```

### Iteration Macros

```lisp
(do-matches (start end "\\w+" "foo bar baz")
  (format t "Match: ~A-~A~%" start end))

(register-groups-bind (first second)
    ("(\\w+)-(\\w+)" "foo-bar")
  (list first second))
;; => ("foo" "bar")
```

### Parse Tree Synonyms

```lisp
(setf (parse-tree-synonym 'word)
      '(:greedy-repetition 1 nil :word-char-class))

(scan '(:sequence word #\!) "hello!")
```

## Error Handling

### Condition Hierarchy

```lisp
simple-error
└── ppcre-error
    ├── ppcre-invocation-error  ; Wrong arguments
    └── ppcre-syntax-error      ; Parse errors
        ├── string slot         ; Input that caused error
        └── pos slot            ; Position of error
```

### Error Reporting

```lisp
(define-condition ppcre-syntax-error (ppcre-error)
  ((string :initarg :string :reader ppcre-syntax-error-string)
   (pos :initarg :pos :reader ppcre-syntax-error-pos))
  (:documentation "Parse error with location info."))

;; Error message includes position
"Quantifier '*' not allowed at position 4 in string \"foo**x\""
```

## Key Design Decisions

### 1. Dual Representation
String syntax for Perl users, S-expressions for Lispers. Both first-class.

### 2. Compiler Architecture
Proper compiler pipeline: lex → parse → IR → optimize → codegen (closures).

### 3. No Dependencies
Self-contained - no external libraries. Maximum portability.

### 4. Performance Focus
- BMH matchers for constant strings
- Character class optimization
- Careful closure generation

### 5. Extensibility Points
- `*property-resolver*` for custom character properties
- `parse-tree-synonym` for DSL extension
- Generic function dispatch for scanner creation

### 6. Configuration Variables
Tune behavior without modifying code:
- `*regex-char-code-limit*` - character range
- `*use-bmh-matchers*` - matching algorithm
- `*optimize-char-classes*` - compile-time optimization

## Patterns to Adopt

1. **Dual input syntax** (string + S-expression) where applicable
2. **Clean compiler architecture** with distinct phases
3. **Zero dependencies** when feasible
4. **Parse tree synonyms** for user-extensible DSLs
5. **Rich iteration macros** (do-X, X-bind patterns)
6. **Configuration via special variables**
7. **Syntax errors with position information**

## Notable Techniques

### Compiler Macro for Constant Scanners

```lisp
;; If regex is constant, compile at load time
(define-compiler-macro scan (&whole form regex target &rest rest)
  (if (constantp regex)
      `(scan (load-time-value (create-scanner ,regex)) ,target ,@rest)
      form))
```

### Filter Functions in Parse Trees

```lisp
;; User-defined matchers
(:filter my-filter-function length)
```

Allows embedding arbitrary Lisp predicates in regex.

## Code Quality Notes

- Comprehensive docstrings on all exported symbols
- Extensive HTML documentation with examples
- Test suite covers Perl compatibility cases
- Performance benchmarks available
- Clean separation of API from implementation
