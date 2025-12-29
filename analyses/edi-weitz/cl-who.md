# CL-WHO

## Overview

| Attribute | Value |
|-----------|-------|
| **Author** | Dr. Edi Weitz |
| **Repository** | https://github.com/edicl/cl-who |
| **Stars** | ~116 |
| **License** | BSD |
| **Size** | ~1,000 LOC |
| **Purpose** | HTML/XML generation DSL |

## Project Structure

Minimal flat structure:

```
cl-who/
├── cl-who.asd
├── packages.lisp
├── specials.lisp
├── util.lisp
├── who.lisp
├── test/
└── docs/
```

## ASDF Pattern

```lisp
(defsystem :cl-who
  :serial t
  :version "1.1.5"
  :license "BSD"
  :components ((:file "packages")
               (:file "specials")
               (:file "util")
               (:file "who"))
  :in-order-to ((test-op (test-op "cl-who/test"))))
```

Notable: **Zero dependencies** - pure ANSI Common Lisp.

## Package Design

```lisp
(defpackage :cl-who
  (:use :cl)
  (:export
   ;; Main macros
   #:with-html-output
   #:with-html-output-to-string
   
   ;; Configuration
   #:*prologue*
   #:*html-mode*
   #:*attribute-quote-char*
   #:*empty-tag-end*
   
   ;; Escaping
   #:escape-string
   #:escape-string-minimal
   #:escape-string-all
   #:escape-char
   
   ;; Utilities
   #:str
   #:fmt
   #:esc
   #:htm
   
   ;; Tag conversion
   #:convert-tag-to-string-list
   #:*html-empty-tags*))
```

## API Design

### Core Macro: WITH-HTML-OUTPUT

```lisp
(with-html-output (stream)
  (:html
   (:head (:title "Hello"))
   (:body
    (:h1 "Welcome")
    (:p "Some text"))))

;; Output:
;; <html><head><title>Hello</title></head>
;; <body><h1>Welcome</h1><p>Some text</p></body></html>
```

### Attribute Syntax

```lisp
;; Attributes via keyword/value pairs
(:a :href "http://example.com" "Click here")
;; => <a href="http://example.com">Click here</a>

;; Class shorthand
(:div.container.main "Content")
;; => <div class="container main">Content</div>

;; ID shorthand
(:div#header "Header")
;; => <div id="header">Header</div>
```

### Embedded Lisp

```lisp
(with-html-output (s)
  (:ul
   (loop for item in items do
     (htm (:li (str item))))))

;; str - output escaped string
;; htm - output HTML from s-expression
;; fmt - format string output
;; esc - escape and output
```

### HTML Mode Control

```lisp
;; XHTML mode (default)
(setf *html-mode* :xml)
(:br)  ; => <br />

;; HTML5 mode
(setf *html-mode* :html5)
(:br)  ; => <br>

;; SGML mode
(setf *html-mode* :sgml)
(:br)  ; => <br>
```

## Notable Patterns

### 1. Compile-Time Transformation

Most work happens at macro-expansion:

```lisp
(with-html-output-to-string (s)
  (:p "Hello"))

;; Expands to efficient string operations
;; No runtime parsing of S-expressions
```

### 2. Minimal Special Variables

```lisp
(defvar *prologue* 
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\"...>"
  "Prologue to output before HTML.")

(defvar *attribute-quote-char* #\"
  "Character to quote attributes.")

(defvar *empty-tag-end* " />"
  "How to end empty tags.")
```

### 3. Streaming Output

Direct to stream, not string building:

```lisp
(with-html-output (*standard-output*)
  (:p "Output directly to stream"))
```

### 4. Empty Tag Handling

```lisp
(defparameter *html-empty-tags*
  '(:area :base :br :col :embed :hr :img :input
    :link :meta :param :source :track :wbr)
  "Tags that don't need closing.")
```

## Lessons for AI Code Generation

### 1. Zero Dependencies for Core DSLs

When possible, avoid dependencies:
- Faster loading
- No version conflicts
- Maximum portability

### 2. Compile-Time Efficiency

For DSLs, do work at compile time:

```lisp
;; Template is known at compile time
;; Only data varies at runtime
(with-html-output-to-string (s)
  (:p (str name)))  ; Only 'name' evaluated at runtime
```

### 3. Mode Variables

Support different output modes:

```lisp
(ecase *html-mode*
  (:xml ...)
  (:html5 ...)
  (:sgml ...))
```

### 4. Convenient Shortcuts

Provide syntactic sugar:

```lisp
;; str - escape and output string
;; htm - embed HTML generation
;; fmt - format string
;; esc - escape string
```

## Comparison to Other HTML Libraries

| Aspect | CL-WHO | Spinneret |
|--------|--------|-----------|
| Dependencies | None | cl-who, parenscript |
| Style | Keyword-based | Function-based |
| Extensibility | Moderate | High |
| Learning curve | Low | Higher |
