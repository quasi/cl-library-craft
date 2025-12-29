# Common Lisp Library Design Philosophy

## Overarching Principles

### 1. Simplicity Over Cleverness

The best CL libraries are straightforward. Edi Weitz's code demonstrates this: clear function names, obvious control flow, minimal magic. The goal is code that a competent Lisper can understand without extensive archaeology.

### 2. Composability

Design libraries as building blocks. CL-PPCRE can create scanners from strings OR S-expressions. Hunchentoot's acceptors are extensible via CLOS. Each piece should work independently and combine well with others.

### 3. Minimal External Dependencies

Prefer fewer, well-maintained dependencies. Edi Weitz's libraries form a coherent ecosystem where he controls quality. When external deps are needed, prefer widely-used compatibility layers (usocket, bordeaux-threads, alexandria).

### 4. Explicit Over Implicit

- Export only what's intended as public API
- Document behavior in docstrings, not just comments
- Use keyword arguments with defaults rather than relying on caller knowledge
- Make customization points obvious (generic functions, special variables)

### 5. Portability With Pragmatism

Write portable code by default, but don't sacrifice quality for portability. Use `#+feature` for implementation-specific optimizations. Rely on compatibility layers (flexi-streams, usocket, bordeaux-threads) rather than rolling your own.

### 6. The CLOS Way

Use CLOS generics for:
- Extensibility points users should customize
- Operations that vary by type
- Methods with sensible defaults that can be specialized

Use regular functions for:
- Internal utilities
- Performance-critical paths where dispatch overhead matters
- Simple operations with no variation

### 7. Error Handling Philosophy

- Define condition hierarchy (library-error → specific-errors)
- Provide meaningful error messages with context
- Use restarts for recoverable situations
- Signal warnings for non-fatal issues
- Don't catch conditions you don't handle specifically

### 8. Documentation Is Part of the API

Code without documentation is incomplete. Every exported symbol needs:
- A docstring explaining what it does
- Parameter documentation
- Return value description
- Example usage for complex functions

### 9. Testing Reality

Tests are insurance, not proof. Cover:
- Happy paths
- Error conditions
- Edge cases from actual usage
- Regression tests for bugs found

### 10. Evolution Over Revolution

Maintain backwards compatibility where possible. When breaking changes are needed:
- Document in CHANGELOG
- Provide migration path
- Consider deprecation warnings before removal

## The Edi Weitz Style

Characteristics observed across his libraries:

1. **Self-contained systems**: Each library is complete and usable on its own
2. **Comprehensive documentation**: HTML docs with full API reference and examples
3. **Clear extension points**: Users know exactly where/how to customize
4. **Defensive error handling**: Input validation, clear error messages
5. **Performance awareness**: Optimizations where they matter, simplicity elsewhere
6. **BSD licensing**: Maximum freedom for users

## Anti-Patterns to Avoid

1. **Over-abstraction**: Creating interfaces for their own sake
2. **Undocumented magic**: Special behaviors without explanation
3. **Kitchen-sink APIs**: Exporting everything "just in case"
4. **NIH syndrome**: Reimplementing what compatibility layers provide
5. **Silent failures**: Returning NIL when an error occurred
6. **Mutable defaults**: Default keyword arguments that can be mutated
7. **Circular dependencies**: Between packages or systems
