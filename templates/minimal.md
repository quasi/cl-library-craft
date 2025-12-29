# Minimal Template

Use for: tiny utilities, single-purpose tools, personal helpers.

The smallest possible valid CL library.

## Structure

```
{{name}}/
├── {{name}}.asd
├── {{name}}.lisp    # Everything in one file
├── README.md
└── LICENSE
```

## {{name}}.asd

```lisp
(defsystem :{{name}}
  :description "{{description}}"
  :author "{{author}}"
  :license "MIT"
  :version "0.1.0"
  :components ((:file "{{name}}")))
```

## {{name}}.lisp

```lisp
;;;; {{name}}.lisp - {{description}}

(defpackage :{{name}}
  (:use :cl)
  (:export #:main-function))

(in-package :{{name}})

(defun main-function (arg)
  "Do the thing with ARG.

Example:
  (main-function \"input\") => result"
  ;; Implementation
  arg)
```

## README.md

```markdown
# {{name}}

{{description}}

## Installation

```lisp
(ql:quickload :{{name}})
```

## Usage

```lisp
({{name}}:main-function "input")
```

## License

MIT
```

## LICENSE

```
MIT License

Copyright (c) {{year}} {{author}}

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

## When to Use

- Personal utilities
- Single-function libraries
- Quick prototypes
- Learning/teaching examples

## When NOT to Use

- Libraries you plan to publish
- Anything with multiple features
- Code that needs error handling
- Production systems

Minimal is great for starting - you can always grow into edi-weitz style later.
