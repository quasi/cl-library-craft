# Documentation

## Docstring Standards

### Function Docstrings

```lisp
(defun scan (regex target-string &key start end)
  "Search TARGET-STRING for REGEX.
   
   Returns four values on match:
   - MATCH-START: Start index of match
   - MATCH-END: End index of match  
   - REG-STARTS: Vector of register start indices
   - REG-ENDS: Vector of register end indices
   
   Returns NIL on no match.
   
   REGEX may be:
   - A string in Perl regex syntax
   - A parse tree (S-expression)
   - A scanner created by CREATE-SCANNER
   
   START and END limit the search region (default: entire string).
   
   Example:
     (scan \"(a+)b\" \"xaaab\")
     => 1, 4, #(1), #(4)"
  ...)
```

### Required Elements

1. **First line**: One-line summary (what it does)
2. **Parameters**: Each parameter documented
3. **Return value(s)**: What's returned
4. **Side effects**: If any
5. **Example**: For non-obvious functions

### Macro Docstrings

```lisp
(defmacro do-matches ((match-start match-end regex target 
                       &optional result) 
                      &body body)
  "Iterate over all matches of REGEX in TARGET.
   
   MATCH-START and MATCH-END are bound to match boundaries.
   Returns RESULT (default NIL) after all iterations.
   
   Example:
     (do-matches (s e \"\\\\w+\" \"foo bar\")
       (print (subseq \"foo bar\" s e)))"
  ...)
```

### Class Docstrings

```lisp
(defclass acceptor ()
  ((port :initarg :port :reader acceptor-port
         :documentation "The port to listen on (default 80)."))
  (:documentation 
   "HTTP connection acceptor.
    
    Create with MAKE-INSTANCE, start with START.
    Customize by subclassing and specializing generic functions.
    
    Important slots:
    - PORT: Listening port
    - ADDRESS: Bind address (nil = all interfaces)
    - DOCUMENT-ROOT: Static file directory
    
    See also: SSL-ACCEPTOR, EASY-ACCEPTOR"))
```

### Condition Docstrings

```lisp
(define-condition ppcre-syntax-error (ppcre-error)
  ((string :initarg :string 
           :reader ppcre-syntax-error-string
           :documentation "The regex string that caused the error.")
   (pos :initarg :pos 
        :reader ppcre-syntax-error-pos
        :documentation "Position in string where error detected."))
  (:documentation 
   "Signaled when REGEX string contains syntax error.
    
    Use PPCRE-SYNTAX-ERROR-STRING and PPCRE-SYNTAX-ERROR-POS
    to get details about the error location."))
```

### Variable Docstrings

```lisp
(defvar *request* nil
  "The current REQUEST object during request handling.
   
   Bound automatically by Hunchentoot. Access via REQUEST
   accessors like HEADER-IN*, PARAMETER, etc.
   
   NIL outside of request context.")

(defparameter *default-content-type* "text/html; charset=utf-8"
  "Default Content-Type header for responses.
   
   Used when handler doesn't set content type explicitly.")
```

## README Structure

### Minimal README

```markdown
# Library Name

One-line description.

## Installation

```lisp
(ql:quickload :library-name)
```

## Quick Start

```lisp
(library:do-thing "example")
=> result
```

## Documentation

Full docs at https://...

## License

BSD-2-Clause
```

### Complete README

```markdown
# Library Name

Longer description explaining what it does and why.

## Features

- Feature 1
- Feature 2
- Feature 3

## Installation

```lisp
(ql:quickload :library-name)
```

Or manually:
```lisp
(asdf:load-system :library-name)
```

## Quick Start

```lisp
;; Basic usage
(library:create-thing)

;; More complex example
(library:with-thing (t)
  (library:process t))
```

## API Overview

### Core Functions

- `create-thing` - Create a new thing
- `process-thing` - Process an existing thing

### Macros

- `with-thing` - Execute code with thing context

### Configuration

- `*default-setting*` - Control default behavior

## Examples

### Example 1: Basic Usage

```lisp
;; Detailed example with comments
```

### Example 2: Advanced Usage

```lisp
;; More complex example
```

## Contributing

...

## License

BSD-2-Clause. See LICENSE file.

## Acknowledgements

...
```

## HTML Documentation

### Edi Weitz Documentation Style

Comprehensive HTML documentation with:

1. **Table of Contents** - Linked sections
2. **Installation** - Step by step
3. **Tutorial** - Getting started
4. **API Reference** - Every exported symbol
5. **Examples** - Runnable code for each function
6. **Compatibility** - Notes on implementations
7. **History** - Changelog summary
8. **Symbol Index** - Alphabetical listing

### Structure

```html
<!DOCTYPE html>
<html>
<head><title>Library Name</title></head>
<body>
<h1>Library Name</h1>

<h2>Contents</h2>
<ul>
  <li><a href="#install">Installation</a></li>
  <li><a href="#tutorial">Tutorial</a></li>
  <li><a href="#reference">Reference</a>
    <ul>
      <li><a href="#functions">Functions</a></li>
      <li><a href="#macros">Macros</a></li>
      <li><a href="#variables">Variables</a></li>
      <li><a href="#conditions">Conditions</a></li>
    </ul>
  </li>
</ul>

<h2 id="install">Installation</h2>
...

<h2 id="reference">Reference</h2>

<h3>Functions</h3>

<div class="function">
  <p><b>function-name</b> <i>args</i> => <i>result</i></p>
  <blockquote>
    Description...
    <pre>(example-code)</pre>
  </blockquote>
</div>

</body>
</html>
```

## CHANGELOG Format

```
Version X.Y.Z (YYYY-MM-DD)
--------------------------
- New: Added feature X
- Fixed: Bug in function Y (issue #123)
- Changed: Behavior of Z (BREAKING)
- Deprecated: Function W, use V instead
- Removed: Old function (was deprecated in X.0.0)

Version X.Y.Z-1 (YYYY-MM-DD)
----------------------------
...
```

### Categories

- **New**: New features
- **Fixed**: Bug fixes
- **Changed**: Behavior changes
- **Deprecated**: Will be removed
- **Removed**: Deleted features
- **Security**: Security fixes

## Example Code

### In Docstrings

```lisp
"...
Example:
  (scan \"a+\" \"aaab\")
  => 0, 3, #(), #()
  
  (scan \"x\" \"aaab\")
  => NIL"
```

### In Documentation

```lisp
;;; Example: Processing a list of files
;;; 
;;; (dolist (file (directory "*.txt"))
;;;   (with-open-file (s file)
;;;     (process-stream s)))
```

### Standalone Example Files

```
examples/
├── basic-usage.lisp
├── advanced-features.lisp
└── integration-example.lisp
```

## Best Practices

1. **Every export needs a docstring** - No exceptions
2. **Examples in docstrings** - For complex functions
3. **Document parameters** - Type and purpose
4. **Document return values** - All return values
5. **Document side effects** - If any
6. **Link related functions** - "See also: X"
7. **Update docs with code** - Keep in sync
8. **Test examples** - Make sure they work
