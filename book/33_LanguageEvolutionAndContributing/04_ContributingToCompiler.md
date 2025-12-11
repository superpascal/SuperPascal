# Contributing to Compiler

**Part of:** [Chapter 33: Language Evolution & Contributing](./README.md)

---

## Introduction

Contributing to the SuperPascal compiler is a great way to:
- **Learn compiler design** — Understand how compilers work
- **Improve the language** — Add features you need
- **Help the community** — Make SuperPascal better for everyone
- **Build your skills** — Gain valuable experience

This chapter explains how to contribute effectively.

---

## Getting Started

### Prerequisites

**Required:**
- Rust installed (latest stable)
- Git installed
- Basic Rust knowledge
- Understanding of compiler concepts (helpful)

**Helpful:**
- Understanding of SuperPascal language
- Experience with parsers/compilers
- Knowledge of target platforms (Z80, 65C816, 68060)

### Setting Up Development Environment

1. **Clone repository:**
```bash
git clone https://github.com/your-org/superpascal.git
cd superpascal
```

2. **Build compiler:**
```bash
cd crates/compiler-rs
cargo build
```

3. **Run tests:**
```bash
cargo test
```

4. **Run compiler:**
```bash
cargo run -- compile program.pas
```

---

## Contribution Areas

### 1. Compiler Frontend

**Lexer:**
- Token recognition
- Error handling
- Directive parsing

**Parser:**
- Grammar implementation
- AST construction
- Error recovery

**Semantics:**
- Type checking
- Symbol resolution
- Error reporting

### 2. Compiler Backend

**IR Generation:**
- IR instruction generation
- Optimization

**Code Generation:**
- Instruction selection
- Register allocation
- Platform-specific codegen

**Object File Writing:**
- Object format
- Symbol tables
- Debug information

### 3. Standard Library

**Core Libraries:**
- System units
- Math libraries
- String processing

**Platform Libraries:**
- Hardware abstraction
- Platform-specific utilities

### 4. Documentation

**Language Specification:**
- Grammar updates
- Semantics documentation
- Examples

**Tutorials:**
- Code examples
- Learning materials
- Best practices

### 5. Testing

**Test Suite:**
- Unit tests
- Integration tests
- Compliance tests

**Test Cases:**
- Normal cases
- Edge cases
- Error cases

---

## Development Workflow

### 1. Find an Issue

**Ways to find work:**
- Browse GitHub issues
- Look for "good first issue" labels
- Check roadmap for planned features
- Propose your own feature

### 2. Create a Branch

```bash
git checkout -b feature/my-feature
```

**Branch naming:**
- `feature/` — New features
- `fix/` — Bug fixes
- `docs/` — Documentation
- `test/` — Tests

### 3. Make Changes

**Development process:**
1. Make code changes
2. Write tests
3. Run tests: `cargo test`
4. Build: `cargo build`
5. Test compiler: `cargo run -- compile test.pas`

### 4. Commit Changes

```bash
git add .
git commit -m "Add feature X"
```

**Commit messages:**
- Clear and descriptive
- Reference issues: "Fix #123"
- Follow project conventions

### 5. Push and Create PR

```bash
git push origin feature/my-feature
```

**Pull request:**
- Clear description
- Reference issues
- Include tests
- Update documentation

---

## Code Standards

### Rust Style

**Follow Rust conventions:**
- `rustfmt` formatting
- `clippy` linting
- Standard naming conventions

**Run formatter:**
```bash
cargo fmt
```

**Run linter:**
```bash
cargo clippy
```

### Code Organization

**Module structure:**
- One module per compiler stage
- Clear separation of concerns
- Well-documented interfaces

**Naming:**
- Clear, descriptive names
- Follow Rust conventions
- Consistent terminology

### Testing

**Test requirements:**
- Unit tests for functions
- Integration tests for features
- Edge case coverage
- Error case coverage

**Test structure:**
```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_feature() {
        // Test code
    }
}
```

---

## Compiler-Specific Guidelines

### Lexer Guidelines

**Token recognition:**
- Handle all token types
- Proper error reporting
- Efficient scanning

**Example:**
```rust
fn scan_token(&mut self) -> Token {
    match self.peek() {
        Some('+') => Token::Plus,
        Some('-') => Token::Minus,
        // ...
    }
}
```

### Parser Guidelines

**Grammar implementation:**
- Follow EBNF specification
- Proper error recovery
- Clear AST construction

**Example:**
```rust
fn parse_statement(&mut self) -> Result<Stmt> {
    match self.peek_token() {
        Token::If => self.parse_if_statement(),
        Token::While => self.parse_while_statement(),
        // ...
    }
}
```

### Semantics Guidelines

**Type checking:**
- Follow type system rules
- Clear error messages
- Proper scope handling

**Example:**
```rust
fn check_type(&mut self, expr: &Expr) -> Type {
    match expr {
        Expr::Literal(Literal::Integer(_)) => Type::Integer,
        Expr::Variable(name) => self.lookup_type(name),
        // ...
    }
}
```

---

## Testing Your Changes

### Unit Tests

**Test individual functions:**
```rust
#[test]
fn test_lexer_keyword() {
    let mut lexer = Lexer::new("program");
    let token = lexer.next_token();
    assert_eq!(token.kind, TokenKind::Program);
}
```

### Integration Tests

**Test compiler pipeline:**
```rust
#[test]
fn test_compile_program() {
    let source = "program Test; begin end.";
    let result = compile(source);
    assert!(result.is_ok());
}
```

### Compliance Tests

**Test language compliance:**
- Run full test suite
- Verify all tests pass
- Check for regressions

---

## Documentation Requirements

### Code Documentation

**Document functions:**
```rust
/// Parses a statement from the token stream.
/// 
/// Returns the parsed statement or an error if parsing fails.
fn parse_statement(&mut self) -> Result<Stmt> {
    // ...
}
```

### Specification Updates

**Update language spec:**
- Grammar changes
- Semantics changes
- New features

### Examples

**Provide examples:**
- Code samples
- Usage patterns
- Best practices

---

## Review Process

### Pull Request Review

**Reviewers check:**
- Code quality
- Test coverage
- Documentation
- Design alignment
- Backward compatibility

### Feedback

**Be open to feedback:**
- Address review comments
- Discuss design decisions
- Improve based on suggestions

### Iteration

**Iterate on changes:**
- Make requested changes
- Update tests
- Update documentation
- Re-request review

---

## Common Contribution Tasks

### Adding a New Intrinsic

1. **Specify** — Document in language spec
2. **Lexer** — (usually no changes)
3. **Parser** — Add intrinsic call parsing
4. **Semantics** — Add type checking
5. **Codegen** — Generate code
6. **Test** — Add tests
7. **Document** — Update reference

### Fixing a Bug

1. **Reproduce** — Create test case
2. **Debug** — Find root cause
3. **Fix** — Implement fix
4. **Test** — Verify fix
5. **Document** — Update if needed

### Improving Error Messages

1. **Identify** — Find unclear error
2. **Improve** — Make message clearer
3. **Test** — Verify improvement
4. **Document** — Update examples

---

## Getting Help

### Resources

**Documentation:**
- Language specification
- Compiler architecture
- Contributing guide

**Community:**
- GitHub discussions
- Issue tracker
- Code reviews

### Asking Questions

**Good questions:**
- Clear problem description
- What you've tried
- Relevant code snippets
- Expected vs actual behavior

---

## Summary

**Getting Started:**
- Clone repository
- Build compiler
- Run tests
- Find an issue

**Development Workflow:**
1. Create branch
2. Make changes
3. Write tests
4. Commit
5. Create PR

**Code Standards:**
- Follow Rust conventions
- Write tests
- Document code
- Update specifications

**Contribution Areas:**
- Compiler frontend/backend
- Standard library
- Documentation
- Testing

**Next:** Explore future directions for SuperPascal.

---

**Next Section:** [Future Directions](./05_FutureDirections.md)  
**Contributing Guide:** See [CONTRIBUTING.md](../../../CONTRIBUTING.md)  
**Last Updated:** 2025-01-XX

