# Type Casting and Conversions

**Part of:** [Chapter 02: Variables, Types, and Expressions](./README.md)

---

## Introduction

**Type casting** allows you to convert values from one type to another. SuperPascal is strongly typed, so explicit conversions are required for most type changes.

**Why casting matters:**
- **Type safety** — Prevents accidental type mismatches
- **Precision control** — Choose when to convert integers to fixed-point
- **Explicit intent** — Makes conversions clear in code
- **Performance** — Compiler can optimize conversions

---

## Explicit Type Casting

### Basic Syntax

**Type casting uses parentheses:**
```pascal
var i: integer;
var w: word;
begin
  i := -100;
  w := word(i);  // Explicit cast: integer to word
  // Note: Negative values wrap around (65535 - 100 + 1 = 65436)
end.
```

### Integer to Fixed-Point

**Convert integer to fixed-point:**
```pascal
var i: integer;
var f: Q8.8;
begin
  i := 10;
  f := Q8.8(i);  // Convert integer to Q8.8
  // f is now 10.0 (in Q8.8 format)
end.
```

**Fixed-point to integer:**
```pascal
var f: Q8.8;
var i: integer;
begin
  f := 10.5;
  i := integer(f);  // Convert Q8.8 to integer (truncates)
  // i is now 10 (fractional part lost)
end.
```

### Integer Type Conversions

**Convert between integer types:**
```pascal
var b: byte;
var i: integer;
var w: word;
begin
  i := 100;
  b := byte(i);   // integer to byte (if value fits)
  w := word(i);   // integer to word (if value fits)
  
  // Range checking: if value doesn't fit, behavior is undefined
  // Use with caution!
end.
```

---

## Conversion Functions

### IntToFloat

**Convert integer to fixed-point explicitly:**
```pascal
function IntToFloat(x: integer): Q8.8;
begin
  IntToFloat := Q8.8(x);
end;

var i: integer;
var f: Q8.8;
begin
  i := 42;
  f := IntToFloat(i);  // f is 42.0
end.
```

**Alternative: Direct cast:**
```pascal
var i: integer;
var f: Q8.8;
begin
  i := 42;
  f := Q8.8(i);  // Same as IntToFloat(i)
end.
```

### FloatToInt

**Convert fixed-point to integer (truncates):**
```pascal
function FloatToInt(x: Q8.8): integer;
begin
  FloatToInt := integer(x);  // Truncates fractional part
end;

var f: Q8.8;
var i: integer;
begin
  f := 42.7;
  i := FloatToInt(f);  // i is 42 (0.7 is lost)
end.
```

**Alternative: Direct cast:**
```pascal
var f: Q8.8;
var i: integer;
begin
  f := 42.7;
  i := integer(f);  // Same as FloatToInt(f)
end.
```

### Rounding Conversions

**Use Round, Ceil, Floor for different rounding:**
```pascal
var f: Q8.8;
var i: integer;
begin
  f := 42.7;
  
  i := Round(f);   // 43 (rounds to nearest)
  i := Ceil(f);    // 43 (rounds up)
  i := Floor(f);   // 42 (rounds down)
  i := Trunc(f);   // 42 (truncates, same as integer(f))
end.
```

---

## Type Conversion Rules

### Safe Conversions

**These conversions are always safe:**
- **Subrange to base type** — If value fits in range
- **Integer literal to compatible type** — Compiler checks
- **Derived class to base class** — Inheritance relationship

### Unsafe Conversions

**These require explicit casts and may lose data:**
- **Integer to byte/word** — May overflow if value too large
- **Fixed-point to integer** — Loses fractional part
- **Large integer to small integer** — May overflow
- **Signed to unsigned** — Negative values wrap around

### Example: Overflow

```pascal
var i: integer;
var b: byte;
begin
  i := 300;        // Valid integer
  b := byte(i);    // Unsafe! 300 > 255, wraps to 44
  // b is now 44 (300 - 256 = 44)
end.
```

---

## Common Patterns

### Integer to Fixed-Point for Math

```pascal
var x, y: integer;
var result: Q8.8;
begin
  x := 10;
  y := 3;
  
  // Convert to fixed-point before division
  result := Q8.8(x) / Q8.8(y);
  // result is 3.333... (in Q8.8)
end.
```

### Fixed-Point to Integer for Display

```pascal
var health: Q8.8;
var display: integer;
begin
  health := 75.5;
  
  // Convert to integer for display
  display := Round(health);
  WriteLn('Health: ', display);  // Health: 76
end.
```

### Type Checking Before Cast

```pascal
function SafeByteCast(x: integer): byte;
begin
  if (x >= 0) and (x <= 255) then
    SafeByteCast := byte(x)
  else
  begin
    WriteLn('Error: Value out of range');
    SafeByteCast := 0;
  end;
end.
```

---

## Best Practices

### 1. Use Explicit Casts

**Always use explicit casts for clarity:**
```pascal
// Good: Explicit cast
var f: Q8.8;
f := Q8.8(10);

// Avoid: Implicit conversion (if allowed)
// f := 10;  // May be ambiguous
```

### 2. Check Ranges

**Check ranges before unsafe casts:**
```pascal
var i: integer;
var b: byte;
begin
  i := 300;
  
  if (i >= 0) and (i <= 255) then
    b := byte(i)
  else
    WriteLn('Value out of range');
end.
```

### 3. Use Appropriate Functions

**Use Round, Ceil, Floor for different needs:**
```pascal
var f: Q8.8;
var i: integer;
begin
  f := 42.7;
  
  // Round to nearest
  i := Round(f);   // 43
  
  // Always round up
  i := Ceil(f);    // 43
  
  // Always round down
  i := Floor(f);   // 42
end.
```

### 4. Document Intent

**Comment why you're casting:**
```pascal
// Convert to fixed-point for precise division
var result: Q8.8;
result := Q8.8(x) / Q8.8(y);

// Convert to integer for display (truncate fractional part)
var display: integer;
display := integer(health);
```

---

## Platform Considerations

### Fixed-Point Precision

**Q8.8 vs Q12.12:**
```pascal
var i: integer;
var f8: Q8.8;
var f12: Q12.12;
begin
  i := 100;
  
  f8 := Q8.8(i);   // 100.0 (8 bits integer, 8 bits fraction)
  f12 := Q12.12(i); // 100.0 (12 bits integer, 12 bits fraction)
  
  // Q12.12 has more precision for fractional part
end.
```

### Performance

**Type conversions:**
- **Integer to fixed-point** — Fast (bit shift)
- **Fixed-point to integer** — Fast (bit shift)
- **Range checking** — Adds overhead (use when needed)

---

## Summary

**Key Concepts:**
- **Type casting** — Explicit conversion using `TypeName(value)`
- **IntToFloat** — Convert integer to fixed-point
- **FloatToInt** — Convert fixed-point to integer (truncates)
- **Rounding** — Use Round, Ceil, Floor for different behaviors
- **Type safety** — Explicit casts prevent errors

**Conversion Functions:**
- `Q8.8(x)` — Integer to Q8.8
- `Q12.12(x)` — Integer to Q12.12
- `integer(x)` — Fixed-point to integer (truncates)
- `Round(x)` — Round to nearest integer
- `Ceil(x)` — Round up
- `Floor(x)` — Round down
- `Trunc(x)` — Truncate (same as integer cast)

**Best Practices:**
- Use explicit casts
- Check ranges for unsafe conversions
- Use appropriate rounding functions
- Document conversion intent

**Next:** Continue with arithmetic and operators, or move to procedures and functions.

---

**Previous Section:** [Arithmetic and Operators](./03_ArithmeticAndOperators.md)  
**Language Specification:** See [03_TypeSystem.md](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

