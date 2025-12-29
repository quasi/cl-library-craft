# Project Structure

## Recommended Directory Layout

### Minimal Library (Single-Purpose)

```
library-name/
├── library-name.asd      # System definition
├── packages.lisp         # Package definition(s)
├── specials.lisp         # Special variables, constants
├── library-name.lisp     # Main implementation
├── README.md             # Overview, quick start
├── LICENSE               # License text
└── CHANGELOG             # Version history (no extension)
```

Example: CL-PPCRE follows this pattern with additional files for distinct functionality.

### Medium Library

```
library-name/
├── library-name.asd
├── packages.lisp
├── specials.lisp
├── conditions.lisp       # Error conditions, warnings
├── util.lisp             # Internal utilities
├── core.lisp             # Core functionality
├── api.lisp              # Public API functions
├── README.md
├── LICENSE
├── CHANGELOG
├── docs/
│   └── index.html        # Full documentation
└── test/
    ├── packages.lisp
    └── tests.lisp
```

### Complex Library (Hunchentoot-scale)

```
library-name/
├── library-name.asd
├── packages.lisp
├── specials.lisp
├── conditions.lisp
├── util.lisp
├── compat.lisp           # Portability layer
├── lispworks.lisp        # Implementation-specific (if needed)
├── [feature].lisp        # Feature-specific files
├── [feature].lisp
├── README.md
├── LICENSE
├── CHANGELOG
├── docs/
│   ├── index.html
│   └── [images, css]
└── test/
    ├── packages.lisp
    ├── test-handlers.lisp
    └── [test resources]
```

## File Naming Conventions

### Source Files
- Use lowercase with hyphens: `my-feature.lisp`
- Name by purpose/responsibility: `conditions.lisp`, `util.lisp`, `api.lisp`
- Main system file matches system name: `cl-ppcre.lisp` or `hunchentoot.lisp`

### Standard File Names (Edi Weitz Convention)

| File | Purpose |
|------|---------|
| `packages.lisp` | Package definitions, always loaded first |
| `specials.lisp` | Special variables, parameters, constants |
| `conditions.lisp` | Condition classes and related functions |
| `util.lisp` | Internal utility functions |
| `compat.lisp` | Cross-implementation compatibility |
| `[impl].lisp` | Implementation-specific code (e.g., `lispworks.lisp`) |

### Test Files
- Place in `test/` subdirectory
- Own `packages.lisp` for test package
- Test system separate from main system

## File Loading Order

ASDF handles this, but the logical order is:

1. `packages.lisp` - Define packages first
2. `specials.lisp` - Variables/constants before use
3. `conditions.lisp` - Condition classes before signaling
4. `compat.lisp` - Compatibility before implementation
5. `util.lisp` - Utilities before main code
6. Feature files in dependency order
7. `api.lisp` - High-level API last (if separate)

## Documentation Placement

### Inline Documentation
- Docstrings on all exported symbols
- Comments for non-obvious implementation details
- File-level comments explaining purpose

### External Documentation
- `README.md`: Quick start, installation, basic usage
- `docs/index.html`: Full API reference with examples
- `CHANGELOG`: Version history, breaking changes

### Example: Hunchentoot docs/ Structure
```
docs/
├── index.html      # Complete documentation
├── LICENSE.txt     # License for docs
└── hunchentoot.gif # Logo/images
```

## Patterns by Library Size

### Small Libraries (< 500 LOC)
- Single source file acceptable
- Inline tests or simple test file
- README-only documentation may suffice

### Medium Libraries (500-2000 LOC)  
- Split by responsibility (conditions, util, core)
- Separate test system
- HTML documentation recommended

### Large Libraries (> 2000 LOC)
- Subdirectory organization if needed
- Multiple test files by feature
- Comprehensive documentation with examples
- Consider tutorial/cookbook section

## What NOT to Include

- IDE/editor configuration files (.projectile, .dir-locals.el)
- Build artifacts
- Temporary files
- Large binary assets (use separate repository)
- CI/CD configuration in root (if needed, use .github/ or similar)
