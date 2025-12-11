# Math Functions and Constants

**Part of:** [Chapter 19: Mathematics for Graphics and Games](./README.md)

---

## Introduction

**Math functions and constants** are essential tools for game development. SuperPascal provides a comprehensive set of mathematical functions and constants for common operations.

**This section covers:**
- **Rounding functions** — Round, Ceil, Floor, Trunc
- **Absolute value** — Abs
- **Square root** — Sqrt
- **Power functions** — Pow, Exp, Log
- **Math constants** — Pi, E, Tau
- **Trigonometric functions** — Covered in next section

---

## Rounding Functions

### Round

**Round to nearest integer:**
```pascal
function Round(x: Q8.8): integer;
```

**Examples:**
```pascal
var f: Q8.8;
var i: integer;
begin
  f := 3.4;
  i := Round(f);  // 3
  
  f := 3.6;
  i := Round(f);  // 4
  
  f := 3.5;
  i := Round(f);  // 4 (rounds to even: banker's rounding)
end.
```

### Ceil (Round Up)

**Always round up:**
```pascal
function Ceil(x: Q8.8): integer;
```

**Examples:**
```pascal
var f: Q8.8;
var i: integer;
begin
  f := 3.1;
  i := Ceil(f);  // 4 (always up)
  
  f := 3.9;
  i := Ceil(f);  // 4
  
  f := 3.0;
  i := Ceil(f);  // 3 (already integer)
end.
```

### Floor (Round Down)

**Always round down:**
```pascal
function Floor(x: Q8.8): integer;
```

**Examples:**
```pascal
var f: Q8.8;
var i: integer;
begin
  f := 3.1;
  i := Floor(f);  // 3 (always down)
  
  f := 3.9;
  i := Floor(f);  // 3
  
  f := 3.0;
  i := Floor(f);  // 3 (already integer)
end.
```

### Trunc (Truncate)

**Truncate fractional part:**
```pascal
function Trunc(x: Q8.8): integer;
```

**Examples:**
```pascal
var f: Q8.8;
var i: integer;
begin
  f := 3.7;
  i := Trunc(f);  // 3 (same as Floor for positive)
  
  f := -3.7;
  i := Trunc(f);  // -3 (different from Floor for negative)
end.
```

**Trunc vs Floor:**
- **Trunc** — Removes fractional part (toward zero)
- **Floor** — Rounds down (toward negative infinity)
- **Same for positive**, different for negative

---

## Absolute Value

### Abs

**Get absolute value:**
```pascal
function Abs(x: integer): integer;
function Abs(x: Q8.8): Q8.8;
```

**Examples:**
```pascal
var i: integer;
var f: Q8.8;
begin
  i := -10;
  i := Abs(i);  // 10
  
  f := -3.5;
  f := Abs(f);  // 3.5
end.
```

**Common use: Distance:**
```pascal
function Distance(x1, y1, x2, y2: integer): integer;
var dx, dy: integer;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  Distance := Abs(dx) + Abs(dy);  // Manhattan distance
end.
```

---

## Square Root

### Sqrt

**Calculate square root:**
```pascal
function Sqrt(x: Q8.8): Q8.8;
```

**Examples:**
```pascal
var x, result: Q8.8;
begin
  x := 16.0;
  result := Sqrt(x);  // 4.0
  
  x := 2.0;
  result := Sqrt(x);  // 1.414... (approximate)
end.
```

**Common use: Euclidean distance:**
```pascal
function Distance(x1, y1, x2, y2: integer): Q8.8;
var dx, dy: integer;
var dx2, dy2: Q8.8;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  dx2 := Q8.8(dx) * Q8.8(dx);
  dy2 := Q8.8(dy) * Q8.8(dy);
  Distance := Sqrt(dx2 + dy2);
end.
```

---

## Power Functions

### Pow

**Raise to power:**
```pascal
function Pow(base, exponent: Q8.8): Q8.8;
```

**Examples:**
```pascal
var x, result: Q8.8;
begin
  x := 2.0;
  result := Pow(x, 2.0);   // 4.0 (2²)
  result := Pow(x, 3.0);   // 8.0 (2³)
  result := Pow(x, 0.5);   // 1.414... (√2)
end.
```

**Common uses:**
```pascal
// Square
var x, square: Q8.8;
square := Pow(x, 2.0);

// Cube
var cube: Q8.8;
cube := Pow(x, 3.0);

// Square root (alternative to Sqrt)
var sqrt: Q8.8;
sqrt := Pow(x, 0.5);
```

### Exp

**Exponential function (e^x):**
```pascal
function Exp(x: Q8.8): Q8.8;
```

**Examples:**
```pascal
var x, result: Q8.8;
begin
  x := 1.0;
  result := Exp(x);  // e¹ ≈ 2.718
  
  x := 0.0;
  result := Exp(x);  // e⁰ = 1.0
end.
```

### Log

**Natural logarithm (ln):**
```pascal
function Log(x: Q8.8): Q8.8;
```

**Examples:**
```pascal
var x, result: Q8.8;
begin
  x := 2.718;  // e
  result := Log(x);  // ≈ 1.0
  
  x := 1.0;
  result := Log(x);  // 0.0
end.
```

**Logarithm base 10:**
```pascal
function Log10(x: Q8.8): Q8.8;
begin
  Log10 := Log(x) / Log(10.0);
end;
```

---

## Math Constants

### Pi (π)

**Pi constant:**
```pascal
const
  PI = 3.141592653589793;  // Q8.8 or Q12.12 representation
```

**Usage:**
```pascal
var radius: Q8.8;
var circumference: Q8.8;
begin
  radius := 5.0;
  circumference := 2.0 * PI * radius;
  // circumference ≈ 31.416
end.
```

### E (Euler's Number)

**Euler's number:**
```pascal
const
  E = 2.718281828459045;  // Q8.8 or Q12.12 representation
```

**Usage:**
```pascal
var x: Q8.8;
var result: Q8.8;
begin
  x := 1.0;
  result := Exp(x);  // e¹ ≈ E
end.
```

### Tau (τ)

**Tau constant (2π):**
```pascal
const
  TAU = 6.283185307179586;  // 2 * PI
```

**Usage:**
```pascal
var radius: Q8.8;
var circumference: Q8.8;
begin
  radius := 5.0;
  circumference := TAU * radius;  // Same as 2 * PI * radius
end.
```

**Why Tau?**
- **Tau = 2π** — More natural for circles
- **One full rotation = τ radians** — Simpler than 2π
- **Common in graphics** — Rotation calculations

---

## Complete Function Reference

### Rounding Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `Round` | `Round(x: Q8.8): integer` | Round to nearest |
| `Ceil` | `Ceil(x: Q8.8): integer` | Round up |
| `Floor` | `Floor(x: Q8.8): integer` | Round down |
| `Trunc` | `Trunc(x: Q8.8): integer` | Truncate |

### Basic Math Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `Abs` | `Abs(x: integer): integer`<br>`Abs(x: Q8.8): Q8.8` | Absolute value |
| `Sqrt` | `Sqrt(x: Q8.8): Q8.8` | Square root |
| `Pow` | `Pow(base, exp: Q8.8): Q8.8` | Power |
| `Exp` | `Exp(x: Q8.8): Q8.8` | e^x |
| `Log` | `Log(x: Q8.8): Q8.8` | Natural logarithm |
| `Log10` | `Log10(x: Q8.8): Q8.8` | Base-10 logarithm |

### Math Constants

| Constant | Value | Description |
|----------|-------|-------------|
| `PI` | 3.141592653589793 | Pi (π) |
| `E` | 2.718281828459045 | Euler's number (e) |
| `TAU` | 6.283185307179586 | Tau (τ = 2π) |

---

## Common Patterns

### Distance Calculation

```pascal
function Distance(x1, y1, x2, y2: integer): Q8.8;
var dx, dy: integer;
var dx2, dy2: Q8.8;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  dx2 := Q8.8(dx) * Q8.8(dx);
  dy2 := Q8.8(dy) * Q8.8(dy);
  Distance := Sqrt(dx2 + dy2);
end.
```

### Clamping Values

```pascal
function Clamp(value, min, max: Q8.8): Q8.8;
begin
  if value < min then
    Clamp := min
  else if value > max then
    Clamp := max
  else
    Clamp := value;
end;
```

### Linear Interpolation

```pascal
function Lerp(a, b, t: Q8.8): Q8.8;
begin
  Lerp := a + t * (b - a);
end;
```

### Smooth Step

```pascal
function SmoothStep(t: Q8.8): Q8.8;
begin
  // t²(3 - 2t)
  SmoothStep := t * t * (3.0 - 2.0 * t);
end;
```

---

## Best Practices

### 1. Use Appropriate Rounding

**Choose the right function:**
```pascal
// For display (round to nearest)
var display: integer;
display := Round(value);

// For grid snapping (round down)
var gridPos: integer;
gridPos := Floor(value / gridSize) * gridSize;

// For positive truncation (same as Floor)
var truncated: integer;
truncated := Trunc(value);
```

### 2. Cache Expensive Operations

**Cache square roots and powers:**
```pascal
// Bad: Calculate multiple times
if Sqrt(x) > threshold then
  // ...
if Sqrt(x) < maxValue then
  // ...

// Good: Calculate once
var sqrtX: Q8.8;
sqrtX := Sqrt(x);
if sqrtX > threshold then
  // ...
if sqrtX < maxValue then
  // ...
```

### 3. Use Constants

**Use math constants instead of literals:**
```pascal
// Bad
circumference := 2 * 3.14159 * radius;

// Good
circumference := TAU * radius;
```

### 4. Handle Edge Cases

**Check for invalid inputs:**
```pascal
function SafeSqrt(x: Q8.8): Q8.8;
begin
  if x < 0.0 then
  begin
    WriteLn('Error: Negative value');
    SafeSqrt := 0.0;
  end
  else
    SafeSqrt := Sqrt(x);
end;
```

---

## Platform Considerations

### Fixed-Point Precision

**Q8.8 vs Q12.12:**
- **Q8.8** — Good for most calculations
- **Q12.12** — Better for high precision (trigonometry, complex math)
- **Constants** — Available in both formats

### Performance

**Function performance:**
- **Abs, Round, Ceil, Floor** — Very fast (simple operations)
- **Sqrt, Pow** — Moderate (iterative algorithms)
- **Exp, Log** — Slower (complex calculations)
- **On ZealZ80** — Consider caching expensive operations

---

## Summary

**Key Concepts:**
- **Rounding functions** — Round, Ceil, Floor, Trunc
- **Absolute value** — Abs
- **Square root** — Sqrt
- **Power functions** — Pow, Exp, Log
- **Math constants** — Pi, E, Tau

**Functions:**
- Rounding: `Round`, `Ceil`, `Floor`, `Trunc`
- Basic: `Abs`, `Sqrt`
- Power: `Pow`, `Exp`, `Log`, `Log10`

**Constants:**
- `PI` — 3.14159...
- `E` — 2.71828...
- `TAU` — 6.28318... (2π)

**Best Practices:**
- Use appropriate rounding function
- Cache expensive operations
- Use constants instead of literals
- Handle edge cases

**Next:** Learn about trigonometry for rotation and angles.

---

**Next Section:** [Trigonometry](./03_Trigonometry.md)  
**Language Specification:** See [03_TypeSystem.md](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

