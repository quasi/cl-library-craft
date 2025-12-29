# Pattern Comparison Matrix

This document compares different approaches to common CL library design decisions, with pros, cons, and recommendations.

## Project Structure

| Approach | Example | Pros | Cons | When to Use |
|----------|---------|------|------|-------------|
| **Flat** | CL-PPCRE, Hunchentoot | Simple, easy navigation | Can get crowded | < 20 source files |
| **src/ directory** | Many modern projects | Clear separation | Extra directory level | Any size |
| **Feature modules** | Large frameworks | Logical grouping | Complexity | Very large projects |

**Recommendation**: Flat for most libraries, src/ for larger projects.

## Dependency Strategy

| Approach | Pros | Cons | When to Use |
|----------|------|------|-------------|
| **Zero deps** | Maximum portability, simple | More code to write | Core utilities, parsers |
| **Minimal deps** | Balance of effort/portability | Some maintenance burden | Most libraries |
| **Rich deps** | Rapid development | Dependency hell risk | Applications |

**Recommendation**: Minimal deps (alexandria + domain-specific).

## Package Export Strategy

| Approach | Pros | Cons | When to Use |
|----------|------|------|-------------|
| **Minimal exports** | Clean API, easy to maintain | May need to expose more later | Public libraries |
| **Export everything** | Maximum flexibility | API instability, confusion | Internal tools |
| **Tiered packages** | Separation of concerns | Complexity | Large frameworks |

**Recommendation**: Minimal exports with explicit list.

## API Style

| Approach | Example | Pros | Cons | When to Use |
|----------|---------|------|------|-------------|
| **Functions only** | Simple utilities | Simple, fast | No extensibility | Utilities, algorithms |
| **Generic functions** | Hunchentoot | Extensible, OO | Dispatch overhead | Frameworks |
| **Macros** | DSLs | Powerful, expressive | Harder to debug | Domain-specific |

**Recommendation**: Functions by default, generics for extension points.

## Accessor Patterns

| Pattern | Example | When to Use |
|---------|---------|-------------|
| `foo` | `header-in` | Primary accessor |
| `foo*` | `header-in*` | Variant with implicit arg |
| `foo-p` | `ssl-p` | Predicates |
| `*foo*` | `*request*` | Special variables |
| `+foo+` | `+http-ok+` | Constants |

**Recommendation**: Follow all conventions consistently.

## Error Handling

| Approach | Pros | Cons | When to Use |
|----------|------|------|-------------|
| **Return NIL** | Simple for caller | Can't distinguish nil value | Never (anti-pattern) |
| **Signal conditions** | Rich info, restarts | More complex | Always for errors |
| **Multiple values** | Distinguish nil/error | Caller must handle | Success + metadata |

**Recommendation**: Signal conditions for errors, multiple values for success variants.

## Condition Hierarchy

| Pattern | Structure | When to Use |
|---------|-----------|-------------|
| **Flat** | All conditions at same level | Very simple libraries |
| **Two-level** | base-error → specific-errors | Most libraries |
| **Deep** | Multiple inheritance levels | Complex domains |

**Recommendation**: Two-level (library-error → specific types).

## Documentation

| Approach | Pros | Cons | When to Use |
|----------|------|------|-------------|
| **Docstrings only** | Simple, always with code | Limited formatting | Small libraries |
| **README + docstrings** | Good for GitHub | No API reference | Medium libraries |
| **Full HTML docs** | Professional, searchable | Maintenance burden | Public libraries |

**Recommendation**: README + docstrings minimum, HTML for public libraries.

## Testing

| Framework | Pros | Cons |
|-----------|------|------|
| **FiveAM** | Widely used, simple | Less features |
| **Parachute** | Better output, fixtures | Less common |
| **Rove** | Minimal, modern | Newer, less battle-tested |

**Recommendation**: FiveAM for compatibility, Parachute for features.

## Portability

| Approach | Pros | Cons | When to Use |
|----------|------|------|-------------|
| **Pure ANSI CL** | Maximum portability | Limited functionality | Core algorithms |
| **Compatibility libs** | Good portability, rich features | Dependencies | Most libraries |
| **Implementation-specific** | Best performance/features | Limited reach | Niche tools |

**Recommendation**: Use compatibility libraries (bordeaux-threads, usocket, etc.).

## Configuration

| Approach | Example | Pros | Cons |
|----------|---------|------|------|
| **Special variables** | `*default-timeout*` | Simple, dynamic | Global state |
| **Keyword args** | `:timeout 30` | Explicit, local | Verbose |
| **Config objects** | `(make-config ...)` | Structured | Complexity |

**Recommendation**: Keywords for local config, special variables for global defaults.

## Iteration Patterns

| Pattern | Example | When to Use |
|---------|---------|-------------|
| `do-X` | `do-matches` | Side effects, body evaluation |
| `map-X` | `mapcar` | Transform and collect |
| `X-bind` | `register-groups-bind` | Destructuring single match |
| `collect-X` | `all-matches` | Return list of results |

**Recommendation**: Provide both `do-X` and collecting variants.

## Extension Mechanisms

| Mechanism | Example | Best For |
|-----------|---------|----------|
| **Generic functions** | Hunchentoot acceptors | Behavior customization |
| **Hooks (functions)** | `*file-upload-hook*` | Simple callbacks |
| **Subclassing** | `my-acceptor` | Full customization |
| **Parse tree synonyms** | CL-PPCRE | DSL extension |

**Recommendation**: Generic functions for behavior, hooks for simple callbacks.

## Summary: Recommended Stack

For a new CL library in 2024+:

| Aspect | Recommendation |
|--------|----------------|
| Structure | Flat or src/ |
| ASDF | `:serial t`, separate test system |
| Package | Single package, minimal exports |
| Deps | alexandria + domain-specific |
| API | Functions + generics for extension |
| Errors | Condition hierarchy |
| Docs | README + docstrings + HTML |
| Tests | FiveAM |
| Portability | bordeaux-threads, usocket |
| Config | Keywords + special vars |

This represents a balance of:
- Following established patterns (Edi Weitz style)
- Modern tooling (Quicklisp era)
- Practical tradeoffs (maintenance vs features)
