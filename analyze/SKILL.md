# Analyze Sub-Skill

Analyze Common Lisp libraries to extract patterns, conventions, and design decisions.

## When to Use

- "Analyze this CL library..."
- "What patterns does [library] use?"
- "Compare [library A] and [library B]"
- "How is [library] structured?"
- "Extract the conventions from..."

## Analysis Process

### 1. Identify the Library

Get the repository URL or local path. For GitHub repos, fetch:
- README for overview
- `.asd` file(s) for system structure
- `packages.lisp` for exports
- Main implementation files for patterns

### 2. Analyze These Dimensions

#### Project Structure
- File organization (flat vs src/ vs modules)
- File naming conventions
- Standard files present (packages.lisp, specials.lisp, conditions.lisp)

#### ASDF System
- `:serial t` vs explicit `:depends-on`
- Dependency philosophy (minimal vs rich)
- Test system setup
- Feature conditionals

#### Package Design
- Single vs multiple packages
- Export strategy (minimal vs comprehensive)
- Import patterns (`:use` vs `:import-from`)
- Naming conventions

#### API Design
- Function signatures (positional vs keyword args)
- Return value patterns (single vs multiple values)
- Generic functions vs regular functions
- Macro usage

#### Error Handling
- Condition hierarchy
- Restart usage
- Error reporting

#### Documentation
- Docstring style
- README structure
- External documentation

### 3. Classify the Style

Match to known author styles:

| Style | Indicators |
|-------|------------|
| **Edi Weitz** | Flat structure, `:serial t`, minimal deps, HTML docs |
| **Fukamachi** | `src/` dir, `:module`, trivia/iterate deps, README.md |
| **Haverbeke** | Multiple .asd files, explicit deps, layered packages |
| **Portability** | `impl-*.lisp` files, feature conditionals, shadow imports |

### 4. Document Findings

Create analysis in `../analyses/[author]/[library].md` with:

```markdown
# [Library Name]

## Overview
| Attribute | Value |
|-----------|-------|
| Author | ... |
| Repository | ... |
| Stars | ... |
| License | ... |
| Size | ... |
| Purpose | ... |

## Project Structure
[Directory tree and explanation]

## ASDF Patterns
[System definition analysis]

## Package Design
[Package structure and exports]

## API Design
[Key patterns and examples]

## Notable Patterns
[Unique or interesting approaches]

## Lessons for AI Code Generation
[Actionable takeaways]
```

## Existing Analyses

Pre-analyzed libraries are in `../analyses/`:

### Edi Weitz Libraries
- `edi-weitz/hunchentoot.md` - Web server
- `edi-weitz/cl-ppcre.md` - Regex library
- `edi-weitz/drakma.md` - HTTP client
- `edi-weitz/cl-who.md` - HTML generation

### Marijn Haverbeke Libraries
- `marijn-haverbeke/postmodern.md` - PostgreSQL interface

### Fukamachi Libraries
- `fukamachi/libraries.md` - Mito, SXQL, Woo

### Portability Libraries
- `portability/libraries.md` - bordeaux-threads, closer-mop, ironclad

## Quick Analysis Template

For fast analysis, extract:

```
Library: [name]
Author: [author]
Style: [edi-weitz|fukamachi|haverbeke|portability|other]

Structure: [flat|src/|multi-system]
ASDF: [serial|explicit-deps|modules]
Packages: [single|multiple|tiered]
Deps: [none|minimal|moderate|heavy]
Docs: [docstrings|readme|html|comprehensive]

Key patterns:
- [pattern 1]
- [pattern 2]
- [pattern 3]

Unique approaches:
- [approach 1]
```

## Comparison Analysis

When comparing libraries, use this format:

```markdown
## Comparison: [Library A] vs [Library B]

| Aspect | Library A | Library B |
|--------|-----------|-----------|
| Structure | ... | ... |
| Dependencies | ... | ... |
| API Style | ... | ... |
| Error Handling | ... | ... |

### When to Use A
- [scenario]

### When to Use B
- [scenario]
```
