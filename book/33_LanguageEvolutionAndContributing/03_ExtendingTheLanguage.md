# Extending the Language

**Part of:** [Chapter 33: Language Evolution & Contributing](./README.md)

---

## Introduction

SuperPascal is designed to be extensible. This chapter explains how to add new features to the language, from simple intrinsics to major language extensions.

**Important:** All language extensions must:
- Follow design principles (educational clarity, platform-agnostic core)
- Be backward compatible
- Be properly specified
- Include tests

---

## Types of Extensions

### 1. Intrinsics

**Intrinsics** are built-in functions that map directly to hardware or runtime operations.

**Examples:**
- `Peek(addr)` — Read memory
- `Poke(addr, value)` — Write memory
- `WaitVBlank()` — Wait for vertical blank
- `SpriteSet(id, x, y)` — Set sprite position

**Adding an Intrinsic:**

1. **Specify behavior** — Document what it does
2. **Add to lexer** — Recognize intrinsic name
3. **Add to parser** — Parse intrinsic call
4. **Add to semantics** — Type checking
5. **Add to codegen** — Generate code
6. **Add tests** — Verify behavior

### 2. Compiler Directives

**Directives** are compiler instructions: `{$DIRECTIVE}`

**Examples:**
- `{$INLINE}` — Inline function
- `{$UNROLL}` — Unroll loop
- `{$RANGE_CHECK ON}` — Enable bounds checking

**Adding a Directive:**

1. **Specify syntax** — Define directive format
2. **Add to lexer** — Recognize directive
3. **Add to parser** — Parse directive
4. **Add to semantics** — Apply directive
5. **Add to codegen** — Use directive
6. **Add tests** — Verify behavior

### 3. Language Features

**Language features** are new syntax or semantics.

**Examples:**
- C-style struct syntax
- Exception handling
- Fixed-point types

**Adding a Language Feature:**

1. **Design** — Follow design principles
2. **Specify** — Update language specification
3. **Update grammar** — Add EBNF rules
4. **Update lexer** — Add tokens
5. **Update parser** — Add AST nodes
6. **Update semantics** — Add type checking
7. **Update codegen** — Generate code
8. **Add tests** — Comprehensive test suite

---

## Extension Process

### Step 1: Design

**Questions to answer:**
- What problem does this solve?
- Does it fit design principles?
- Is it platform-agnostic?
- Is it backward compatible?
- How does it interact with existing features?

**Document:**
- Use case
- Syntax
- Semantics
- Examples
- Edge cases

### Step 2: Specification

**Update language specification:**
- Add to grammar (EBNF)
- Add to type system
- Add to semantics
- Add examples
- Document platform-specific behavior

### Step 3: Implementation

**Follow compiler pipeline:**
1. **Lexer** — Recognize new syntax
2. **Parser** — Build AST nodes
3. **Semantics** — Type checking, validation
4. **IR** — Generate IR instructions
5. **Codegen** — Generate machine code

### Step 4: Testing

**Test thoroughly:**
- Normal cases
- Edge cases
- Error cases
- Platform-specific behavior
- Backward compatibility

### Step 5: Documentation

**Update documentation:**
- Language specification
- Language reference
- Examples
- Tutorials (if major feature)

---

## Example: Adding a Simple Intrinsic

### Goal

Add `GetFrameCount()` intrinsic that returns the current frame number.

### Step 1: Design

**Specification:**
```pascal
function GetFrameCount(): integer;
```
- Returns current frame number (starts at 0)
- Platform-agnostic (runtime tracks frame count)
- Simple integer return

### Step 2: Specification Update

Add to `languageSpecification/intrinsicsAndDirectives/`:
```markdown
## GetFrameCount

**Syntax:** `function GetFrameCount(): integer;`

**Description:** Returns the current frame number since program start.

**Platform:** All platforms

**Example:**
```pascal
var frame: integer;
frame := GetFrameCount();
WriteLn('Frame: ', frame);
```
```

### Step 3: Implementation

**Lexer:** (no changes needed — identifier)

**Parser:** Add to intrinsic call parsing

**Semantics:** Add type checking (returns integer)

**Codegen:** Generate runtime call

**Runtime:** Implement frame counter

### Step 4: Testing

```pascal
program TestGetFrameCount;
var f: integer;
begin
  f := GetFrameCount();
  if f >= 0 then
    WriteLn('OK')
  else
    WriteLn('ERROR');
end.
```

### Step 5: Documentation

Update language reference with new intrinsic.

---

## Example: Adding a Compiler Directive

### Goal

Add `{$OPTIMIZE LEVEL}` directive to control optimization.

### Step 1: Design

**Specification:**
```pascal
{$OPTIMIZE 0}  // No optimization
{$OPTIMIZE 1}  // Basic optimization
{$OPTIMIZE 2}  // Full optimization
```

### Step 2: Specification Update

Add to compiler directives documentation.

### Step 3: Implementation

**Lexer:** Recognize `{$OPTIMIZE}`

**Parser:** Parse optimization level

**Semantics:** Store optimization level

**Codegen:** Apply optimizations based on level

### Step 4: Testing

Test with different optimization levels.

### Step 5: Documentation

Update compiler directives reference.

---

## Major Language Extensions

### Adding Generics (Future)

**Design considerations:**
- Syntax: `type TList<T> = ...`
- Type checking: Generic type parameters
- Codegen: Monomorphization or runtime dispatch
- Backward compatibility: Optional feature

**Process:**
1. Design syntax and semantics
2. Update specification
3. Implement in all compiler stages
4. Comprehensive testing
5. Documentation

### Adding Type Inference (Future)

**Design considerations:**
- Syntax: `var x := 10;` (infer integer)
- Type checking: Infer from initializer
- Backward compatibility: Optional

**Process:**
1. Design inference rules
2. Update specification
3. Implement type inference
4. Testing
5. Documentation

---

## Platform-Specific Extensions

### Adding Platform Intrinsics

**Process:**
1. **Specify** — Document in platform intrinsics
2. **Implement** — Add to platform codegen
3. **Test** — Platform-specific tests
4. **Document** — Platform documentation

**Example:** Adding Foenix-specific intrinsic
- Document in `platforms/Foenix65C816/intrinsics/`
- Implement in Foenix codegen
- Test on Foenix hardware/emulator

---

## Extension Guidelines

### Do's

✅ **Follow design principles** — Educational clarity, platform-agnostic core  
✅ **Be backward compatible** — Don't break existing code  
✅ **Specify first** — Design before implementation  
✅ **Test thoroughly** — Normal, edge, error cases  
✅ **Document completely** — Specification, reference, examples  

### Don'ts

❌ **Don't break compatibility** — No breaking changes  
❌ **Don't add platform-specific syntax** — Use intrinsics instead  
❌ **Don't skip specification** — Always document first  
❌ **Don't skip tests** — Test everything  
❌ **Don't add complexity** — Keep it simple  

---

## Contributing Extensions

### Proposal Process

1. **Discuss** — Open issue or discussion
2. **Design** — Create specification draft
3. **Review** — Get feedback from maintainers
4. **Implement** — Code implementation
5. **Test** — Comprehensive tests
6. **Document** — Update all documentation
7. **Submit** — Pull request

### Review Criteria

**Maintainers evaluate:**
- Does it fit design principles?
- Is it backward compatible?
- Is it well-specified?
- Is it well-tested?
- Is it well-documented?

---

## Summary

**Extension Types:**
- **Intrinsics** — Built-in functions
- **Directives** — Compiler instructions
- **Language Features** — New syntax/semantics

**Extension Process:**
1. Design
2. Specify
3. Implement
4. Test
5. Document

**Guidelines:**
- Follow design principles
- Be backward compatible
- Specify first
- Test thoroughly
- Document completely

**Next:** Learn how to contribute to compiler development.

---

**Next Section:** [Contributing to Compiler](./04_ContributingToCompiler.md)  
**Language Specification:** See [06_IntrinsicsAndDirectives.md](../../languageSpecification/06_IntrinsicsAndDirectives.md)  
**Last Updated:** 2025-01-XX

