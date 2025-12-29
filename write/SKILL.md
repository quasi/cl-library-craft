# Write Sub-Skill

Generate idiomatic Common Lisp libraries following established best practices.

## When to Use

- "Create a CL library for..."
- "Write a Common Lisp package that..."
- "Generate a Lisp library to..."
- "Scaffold a CL project..."
- "Build a CL system for..."

## Generation Process

### Step 1: Gather Requirements

Determine or ask:
1. **Purpose** - What does the library do?
2. **Scope** - Simple utility or full framework?
3. **Dependencies** - What external libs needed?
4. **Audience** - Public package or internal tool?

### Step 2: Select Style

| Project Type | Style | Template |
|-------------|-------|----------|
| Simple utility | minimal | `../templates/minimal.md` |
| General library | edi-weitz | `../templates/edi-weitz.md` |
| Web/ORM | fukamachi | `../templates/fukamachi.md` |
| Multi-layer system | haverbeke | `../templates/haverbeke.md` |

### Step 3: Generate Files

**Always generate in this order:**

1. `{{name}}.asd` - System definition
2. `packages.lisp` - Package definition
3. `specials.lisp` - Special variables (if needed)
4. `conditions.lisp` - Conditions (if needed)
5. `util.lisp` - Internal utilities (if needed)
6. Core implementation files
7. `README.md` - Documentation
8. `LICENSE` - License file
9. Test files (if requested)

### Step 4: Apply Patterns

Reference guides in `../guide/` for:
- Naming conventions → `03-package-design.md`
- API patterns → `04-api-design.md`
- Error handling → `05-error-handling.md`
- Docstrings → `06-documentation.md`

## Templates

Full templates with placeholders are in `../templates/`:

- `edi-weitz.md` - Flat structure, `:serial t`
- `fukamachi.md` - `src/` modules, modern deps
- `haverbeke.md` - Multi-system layered
- `minimal.md` - Smallest valid library
- `snippets.md` - Common code patterns

## Quick Reference

### Minimal Library (< 100 LOC)

```
{{name}}/
├── {{name}}.asd
├── {{name}}.lisp
└── README.md
```

### Standard Library

```
{{name}}/
├── {{name}}.asd
├── packages.lisp
├── specials.lisp      # if needed
├── conditions.lisp    # if needed
├── {{name}}.lisp
├── README.md
└── LICENSE
```

### Large Library

```
{{name}}/
├── {{name}}.asd
├── packages.lisp
├── specials.lisp
├── conditions.lisp
├── util.lisp
├── core.lisp
├── api.lisp
├── test/
│   └── tests.lisp
├── README.md
└── LICENSE
```

## Naming Conventions

```lisp
;; Functions
parse-thing          ; action-noun
thing-value          ; accessor
do-things            ; iteration
with-thing           ; context macro
define-thing         ; definition macro

;; Predicates
thing-p              ; type predicate

;; Variants
thing                ; primary
thing*               ; variant (implicit args)

;; Variables
*thing*              ; special variable
+thing+              ; constant
```

## Essential Patterns

### Package Definition

```lisp
(defpackage :{{name}}
  (:use :cl)
  (:export
   #:main-function
   #:{{name}}-error
   #:*default-setting*))
```

### ASDF System

```lisp
(defsystem :{{name}}
  :description "{{description}}"
  :author "{{author}}"
  :license "MIT"
  :version "0.1.0"
  :serial t
  :components ((:file "packages")
               (:file "{{name}}"))
  :depends-on (:alexandria))
```

### Condition Hierarchy

```lisp
(define-condition {{name}}-error (error)
  ((message :initarg :message :reader {{name}}-error-message))
  (:report (lambda (c s)
             (format s "{{Name}} error: ~A"
                     ({{name}}-error-message c)))))
```

### Function with Docstring

```lisp
(defun process (input &key (format :auto))
  "Process INPUT and return result.

FORMAT controls parsing (:AUTO, :JSON, :XML).

Returns processed data.
Signals {{NAME}}-ERROR on failure.

Example:
  (process \"data\") => result"
  ...)
```

## Checklist Before Completion

- [ ] ASDF system has description, author, license, version
- [ ] All exported symbols have docstrings
- [ ] Conditions inherit from `error` or `warning`
- [ ] Special variables have `*earmuffs*`
- [ ] README has installation and usage
- [ ] Code loads without warnings

## Common Mistakes

1. **Wrong package order** - Define package before `(in-package ...)`
2. **Missing exports** - Export all public API
3. **No error inheritance** - Conditions must inherit from `error`/`warning`
4. **Missing metadata** - ASDF needs description, author, license
5. **Hardcoded paths** - Use `asdf:system-relative-pathname`
