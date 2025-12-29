# CL Library Craft - A Claude Skill

> An AI skill for analyzing and generating idiomatic Common Lisp libraries.

## Overview

CL Library Craft teaches AI assistants to understand and create Common Lisp libraries that follow the conventions and patterns established by the CL community's most respected authors. It's built from deep analysis of exemplary libraries by Edi Weitz, Marijn Haverbeke, Eitaro Fukamachi, and others.

### What It Does

- **Analyze** existing CL libraries to extract patterns, conventions, and design decisions
- **Generate** new CL libraries following established best practices
- **Reference** battle-tested patterns from real-world production libraries

## Installation

### For Claude Desktop / Claude.ai with Skills

1. Download or clone this repository
2. Add to your Claude skills directory:
   ```
   /cl-library-craft/
   ```

### For Other AI Systems

The skill is organized as markdown documentation that can be included in any AI system's context or knowledge base.

## Usage

### Analyzing a Library

```
User: Analyze the patterns used in Hunchentoot

Claude: [Reads analyze/SKILL.md, then analyses/edi-weitz/hunchentoot.md]
        [Provides detailed analysis of structure, ASDF, packages, API design...]
```

### Creating a Library

```
User: Create a CL library for parsing YAML files

Claude: [Reads write/SKILL.md, selects edi-weitz style for parsers]
        [Generates: cl-yaml.asd, packages.lisp, conditions.lisp, parser.lisp, README.md]
```

### Understanding Patterns

```
User: How should I handle errors in Common Lisp?

Claude: [Reads guide/05-error-handling.md]
        [Explains condition hierarchies, restarts, signaling patterns with examples]
```

## Structure

```
cl-library-craft/
├── SKILL.md                      # Main entry - routes to sub-skills
│
├── analyze/                      # Analysis sub-skill
│   └── SKILL.md                  
│
├── write/                        # Generation sub-skill
│   └── SKILL.md                  
│
├── guide/                        # Design pattern guides
│   ├── 00-philosophy.md          # Core CL design principles
│   ├── 01-project-structure.md   # Directory layouts
│   ├── 02-asdf-systems.md        # System definitions
│   ├── 03-package-design.md      # Package patterns
│   ├── 04-api-design.md          # API conventions
│   ├── 05-error-handling.md      # Conditions & restarts
│   ├── 06-documentation.md       # Docstrings & docs
│   ├── 07-portability.md         # Cross-implementation
│   ├── 08-testing.md             # Test patterns
│   ├── 09-pattern-matrix.md      # Comparison of approaches
│   ├── 10-author-styles.md       # Style guide by author
│   └── 11-sbcl-optimizations.md  # SBCL-specific optimization
│
├── templates/                    # Ready-to-use templates
│   ├── edi-weitz.md              # Traditional flat style
│   ├── fukamachi.md              # Modern src/ style
│   ├── haverbeke.md              # Multi-system layered
│   ├── minimal.md                # Smallest valid library
│   └── snippets.md               # Common code patterns
│
└── analyses/                     # Library analyses
    ├── edi-weitz/
    │   ├── hunchentoot.md        # Web server
    │   ├── cl-ppcre.md           # Regex engine
    │   ├── drakma.md             # HTTP client
    │   └── cl-who.md             # HTML generation
    ├── marijn-haverbeke/
    │   └── postmodern.md         # PostgreSQL interface
    ├── fukamachi/
    │   └── libraries.md          # Mito, SXQL, Woo
    └── portability/
        └── libraries.md          # bordeaux-threads, closer-mop, ironclad
```

## Supported Styles

The skill recognizes and can generate four distinct library styles:

| Style | Author | Characteristics | Best For |
|-------|--------|-----------------|----------|
| **edi-weitz** | Edi Weitz | Flat structure, `:serial t`, minimal deps | General libraries, parsers |
| **fukamachi** | Eitaro Fukamachi | `src/` modules, modern deps | Web apps, ORMs |
| **haverbeke** | Marijn Haverbeke | Multi-system, layered | Database interfaces, DSLs |
| **minimal** | - | Single file | Tiny utilities |

## Key Patterns Documented

### From Edi Weitz (Hunchentoot, CL-PPCRE, Drakma)
- Dual accessor pattern (`foo` / `foo*`)
- Comprehensive condition hierarchies
- HTML documentation generation
- Flat file structure with purpose-based naming

### From Marijn Haverbeke (Postmodern)
- Multi-system architecture (cl-postgres → s-sql → postmodern)
- Compile-time SQL generation
- Tiered packages with re-exports
- DAO (Database Access Object) patterns

### From Eitaro Fukamachi (Mito, SXQL, Woo)
- Modern `src/` directory layout
- Pattern matching with Trivia
- Immutable query composition
- High-performance event loops

### From Portability Libraries (bordeaux-threads, closer-mop)
- Implementation-specific files (`impl-*.lisp`)
- Feature conditional compilation
- Shadow/replace package patterns

## Context Management

The skill uses sub-skills to minimize context usage:

1. **Main SKILL.md** - Small router file, always read first
2. **Sub-skills** - Only the relevant one is loaded (`analyze/` or `write/`)
3. **Reference material** - Loaded on-demand from `guide/`, `templates/`, `analyses/`

This means generating a simple library might only need:
- `SKILL.md` (routing)
- `write/SKILL.md` (generation instructions)
- `templates/minimal.md` (template)

While analyzing a complex library might load:
- `SKILL.md` (routing)
- `analyze/SKILL.md` (analysis instructions)
- `guide/10-author-styles.md` (for comparison)
- Relevant files from `analyses/`

## Contributing

### Adding Library Analyses

1. Analyze the library following the template in `analyze/SKILL.md`
2. Create a file in `analyses/[author]/[library].md`
3. Include: overview, structure, ASDF, packages, API patterns, lessons

### Improving Guides

The guides in `guide/` are living documents. Improvements welcome for:
- Additional patterns discovered in other libraries
- Corrections or clarifications
- New optimization techniques

### Adding Templates

New templates should include:
- Complete file structure
- All standard files with placeholders
- Usage examples
- When to use / when not to use

## License

MIT License - See LICENSE file

## Acknowledgments

This skill is built from studying the work of:

- **Edi Weitz** - For setting the standard of CL library quality
- **Marijn Haverbeke** - For demonstrating elegant multi-system design
- **Eitaro Fukamachi** - For modernizing CL web development
- **Pascal Costanza** - For closer-mop and MOP expertise
- **Stelian Ionescu** - For bordeaux-threads and portability patterns
- **The Sharplispers** - For maintaining critical infrastructure

And the broader Common Lisp community whose collective wisdom is encoded in these patterns.

---

*"The best way to learn Common Lisp style is to read code written by experts. This skill distills that expertise into actionable patterns for AI assistants."*
