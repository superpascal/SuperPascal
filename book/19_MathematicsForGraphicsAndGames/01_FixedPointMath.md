# Fixed-Point Math

**Part of:** [Chapter 19: Mathematics for Graphics and Games](./README.md)

---

## Introduction

Fixed-point math is essential for game development on retro platforms. This section teaches you how fixed-point numbers work, how to use them, and when to choose them over integers or floating-point.

**Key concepts:**
- **Fixed-point representation** — How decimal numbers are stored
- **Q8.8 format** — Standard fixed-point (8.8 bits)
- **Q12.12 format** — High precision fixed-point (12.12 bits)
- **Fixed-point arithmetic** — Addition, subtraction, multiplication, division
- **Precision and range** — Understanding limitations

---

## What is Fixed-Point Math?

### The Problem with Floating-Point

**Floating-point is slow on retro systems:**

- **Hardware support** — Many retro CPUs don't have floating-point units
- **Performance** — Software floating-point is very slow
- **Memory** — Floating-point takes more memory
- **Determinism** — Floating-point can have rounding differences

**Solution: Fixed-point arithmetic**

### Fixed-Point Solution

**Fixed-point uses integers to represent decimal numbers:**

- **Fast** — Integer arithmetic (very fast)
- **Deterministic** — Same results everywhere
- **Memory efficient** — Same size as integers
- **Hardware-friendly** — Can be FPGA-accelerated

**How it works:**
```
Integer:     1234
Fixed-point: 1234 (represents 12.34)
             ^^^^
             Bits are divided into integer and fraction parts
```

---

## Q8.8 Format

### Understanding Q8.8

**Q8.8 means 8 bits integer, 8 bits fraction:**

```
16-bit value: [IIIIIIII][FFFFFFFF]
              ^^^^^^^^  ^^^^^^^^
              Integer   Fraction
              8 bits    8 bits
```

**Example:**
```
Value: 0x1234 (binary: 00010010 00110100)
Integer part: 0x12 = 18
Fraction part: 0x34 = 52

Decimal value: 18 + (52 / 256) = 18.203125
```

### Q8.8 Range and Precision

**Range:**
- **Minimum**: -128.0
- **Maximum**: 127.99609375 (approximately)
- **Step size**: 1/256 = 0.00390625

**Precision:**
- **Smallest value**: 0.00390625 (1/256)
- **Largest integer**: 127
- **Fractional precision**: 8 bits (256 levels)

**Example:**
```pascal
var x: Q8.8;
begin
  x := 10.5;   // Stored as 0x0A80 (10 + 128/256)
  x := 0.5;    // Stored as 0x0080 (0 + 128/256)
  x := 127.0;  // Stored as 0x7F00 (127 + 0/256)
end;
```

### Q8.8 Literals

**Write fixed-point values directly:**

```pascal
var x: Q8.8;
begin
  x := 10.5;    // 10.5
  x := 0.25;    // 0.25
  x := 100.0;   // 100.0
  x := -5.75;   // -5.75
end;
```

**Compiler converts to fixed-point format automatically.**

---

## Q12.12 Format

### Understanding Q12.12

**Q12.12 means 12 bits integer, 12 bits fraction:**

```
24-bit value: [IIIIIIIIIIII][FFFFFFFFFFFF]
              ^^^^^^^^^^^^  ^^^^^^^^^^^^
              Integer       Fraction
              12 bits       12 bits
```

**Example:**
```
Value: 0x123456
Integer part: 0x123 = 291
Fraction part: 0x456 = 1110

Decimal value: 291 + (1110 / 4096) = 291.27099609375
```

### Q12.12 Range and Precision

**Range:**
- **Minimum**: -2048.0
- **Maximum**: 2047.999755859375 (approximately)
- **Step size**: 1/4096 = 0.000244140625

**Precision:**
- **Smallest value**: 0.000244140625 (1/4096)
- **Largest integer**: 2047
- **Fractional precision**: 12 bits (4096 levels)

**When to use Q12.12:**
- **Higher precision needed** — More accurate calculations
- **Larger range needed** — Values beyond Q8.8 range
- **Physics calculations** — More accurate physics
- **Camera movement** — Smooth camera positioning

---

## Fixed-Point Arithmetic

### Addition and Subtraction

**Addition and subtraction work normally:**

```pascal
var a, b, result: Q8.8;
begin
  a := 10.5;
  b := 5.25;
  result := a + b;  // 15.75
  result := a - b;  // 5.25
end;
```

**How it works:**
- Fixed-point values are just integers
- Addition/subtraction of integers = addition/subtraction of fixed-point
- No special handling needed!

### Multiplication

**Multiplication requires adjustment:**

```pascal
var a, b, result: Q8.8;
begin
  a := 10.5;
  b := 2.0;
  // Compiler handles this automatically
  result := a * b;  // 21.0
end;
```

**Behind the scenes:**
```pascal
// Manual multiplication (for understanding)
// a = 10.5 = 0x0A80 (2688 in decimal)
// b = 2.0 = 0x0200 (512 in decimal)
// result = (2688 * 512) / 256 = 5376 / 256 = 21.0
```

**Compiler automatically:**
- Multiplies the integer values
- Shifts right by 8 bits (divides by 256)
- Handles overflow/underflow

### Division

**Division also requires adjustment:**

```pascal
var a, b, result: Q8.8;
begin
  a := 21.0;
  b := 2.0;
  // Compiler handles this automatically
  result := a / b;  // 10.5
end;
```

**Behind the scenes:**
```pascal
// Manual division (for understanding)
// a = 21.0 = 0x1500 (5376 in decimal)
// b = 2.0 = 0x0200 (512 in decimal)
// result = (5376 * 256) / 512 = 1376256 / 512 = 2688 = 10.5
```

**Compiler automatically:**
- Shifts left by 8 bits (multiplies by 256)
- Divides the integer values
- Handles precision loss

---

## Type Conversions

### Integer to Fixed-Point

**Convert integer to fixed-point:**

```pascal
var i: integer;
var f: Q8.8;
begin
  i := 10;
  f := Q8.8(i);      // 10.0
  f := IntToFloat(i); // Same as Q8.8(i)
end;
```

**How it works:**
- Integer value is shifted left by 8 bits
- `10` becomes `10 * 256 = 2560 = 0x0A00` (represents 10.0)

### Fixed-Point to Integer

**Convert fixed-point to integer:**

```pascal
var f: Q8.8;
var i: integer;
begin
  f := 10.5;
  i := integer(f);    // 10 (truncates)
  i := Round(f);      // 11 (rounds)
  i := Floor(f);      // 10 (rounds down)
  i := Ceil(f);       // 11 (rounds up)
end;
```

**How it works:**
- Fixed-point value is shifted right by 8 bits
- `0x0A80` (10.5) becomes `0x0A` (10) when truncated
- Rounding functions handle fractional part

---

## Practical Examples

### Game Coordinates

**Use Q8.8 for smooth movement:**

```pascal
var
  playerX, playerY: Q8.8;
  velocityX, velocityY: Q8.8;
begin
  playerX := 100.5;
  playerY := 50.25;
  velocityX := 2.5;
  velocityY := 1.0;
  
  // Update position
  playerX := playerX + velocityX;  // 103.0
  playerY := playerY + velocityY;  // 51.25
  
  // Convert to integer for rendering
  var screenX := Round(playerX);  // 103
  var screenY := Round(playerY);  // 51
end;
```

### Scaling

**Use Q8.8 for scaling factors:**

```pascal
var
  spriteX, spriteY: integer;
  scale: Q8.8;
  scaledX, scaledY: integer;
begin
  spriteX := 100;
  spriteY := 50;
  scale := 1.5;  // 150% scale
  
  // Scale coordinates
  scaledX := Round(spriteX * scale);  // 150
  scaledY := Round(spriteY * scale);  // 75
end;
```

### Interpolation

**Use Q8.8 for smooth interpolation:**

```pascal
function Lerp(start, target: integer; t: Q8.8): integer;
begin
  // Linear interpolation
  Lerp := start + Round((target - start) * t);
end;

// Usage
var
  startX, targetX: integer;
  t: Q8.8;
  currentX: integer;
begin
  startX := 0;
  targetX := 100;
  t := 0.5;  // Halfway
  
  currentX := Lerp(startX, targetX, t);  // 50
end;
```

---

## Precision Considerations

### Precision Loss

**Fixed-point has limited precision:**

```pascal
var a, b, result: Q8.8;
begin
  a := 10.1;  // Actually stored as 10.09765625 (closest value)
  b := 0.01;  // Actually stored as 0.01171875 (closest value)
  result := a + b;  // 10.109375 (not exactly 10.11)
end;
```

**Q8.8 precision:**
- Can represent: 0, 0.00390625, 0.0078125, 0.01171875, ...
- Cannot represent: 0.01, 0.02, 0.03 exactly
- Closest to 0.01 is 0.01171875 (error: 0.00171875)

**Q12.12 precision:**
- Can represent: 0, 0.000244140625, 0.00048828125, ...
- Much more precise than Q8.8
- Closest to 0.01 is 0.010009765625 (much smaller error)

### When Precision Matters

**Use Q12.12 when:**
- **High precision needed** — Physics calculations
- **Small increments** — Smooth camera movement
- **Accumulation** — Many operations (errors accumulate)

**Use Q8.8 when:**
- **Game coordinates** — Pixel positions (Q8.8 is usually enough)
- **Velocities** — Movement speeds
- **Simple math** — Basic calculations

---

## Common Patterns

### Fixed-Point Constants

**Define constants with proper type:**

```pascal
const
  PI: Q8.8 = 3.14159265;
  GRAVITY: Q8.8 = 9.8;
  SPEED: Q8.8 = 5.5;
```

### Fixed-Point Helper Functions

**Create helper functions for common operations:**

```pascal
// Convert Q8.8 to string for display
function Q8_8ToStr(x: Q8.8): string;
var
  intPart: integer;
  fracPart: integer;
begin
  intPart := integer(x);
  fracPart := Abs(Round((x - Q8.8(intPart)) * 256));
  Q8_8ToStr := IntToStr(intPart) + '.' + IntToStr(fracPart);
end;

// Clamp Q8.8 value
function ClampQ8_8(x, min, max: Q8.8): Q8.8;
begin
  if x < min then
    ClampQ8_8 := min
  else if x > max then
    ClampQ8_8 := max
  else
    ClampQ8_8 := x;
end;
```

### Mixing Types

**Be careful when mixing integer and fixed-point:**

```pascal
var
  i: integer;
  f: Q8.8;
  result: Q8.8;
begin
  i := 10;
  f := 5.5;
  
  // ✅ GOOD: Explicit conversion
  result := Q8.8(i) + f;  // 15.5
  
  // ❌ BAD: Implicit conversion (may not work as expected)
  // result := i + f;  // Compiler error or unexpected result
end;
```

---

## Best Practices

### 1. Choose Right Format

**Select format based on needs:**

```pascal
// ✅ GOOD: Q8.8 for game coordinates
var x, y: Q8.8;

// ✅ GOOD: Q12.12 for physics
var velocity: Q12.12;

// ❌ BAD: Q12.12 for simple coordinates
var x, y: Q12.12;  // Overkill, wastes memory
```

### 2. Be Aware of Precision

**Understand precision limitations:**

```pascal
// ✅ GOOD: Know precision limits
// Q8.8 can't represent 0.01 exactly

// ❌ BAD: Expecting infinite precision
// Don't assume fixed-point is exact
```

### 3. Use Rounding Functions

**Use appropriate rounding:**

```pascal
// ✅ GOOD: Round when converting to integer
var screenX := Round(worldX);

// ❌ BAD: Truncate (loses precision)
var screenX := integer(worldX);  // May cause jitter
```

### 4. Avoid Accumulation Errors

**Be careful with many operations:**

```pascal
// ✅ GOOD: Reset periodically
if frameCount mod 60 = 0 then
  position := Round(position);  // Snap to integer periodically

// ❌ BAD: Many operations accumulate errors
// position := position + velocity;  // Repeated many times
```

### 5. Document Fixed-Point Usage

**Make fixed-point usage clear:**

```pascal
// ✅ GOOD: Document format
var cameraX: Q8.8;  // Camera X position (Q8.8 format)

// ❌ BAD: Unclear type
var cameraX: Q8.8;  // What does this represent?
```

---

## Exercises

### Exercise 1: Basic Fixed-Point

Write a program that:
1. Creates Q8.8 variables
2. Performs addition, subtraction, multiplication, division
3. Converts to/from integers
4. Displays results

### Exercise 2: Smooth Movement

Write a program that:
1. Uses Q8.8 for entity position
2. Moves entity with fractional velocity
3. Converts to integer for rendering
4. Demonstrates smooth movement

### Exercise 3: Precision Comparison

Write a program that:
1. Compares Q8.8 and Q12.12 precision
2. Shows precision differences
3. Demonstrates when each is appropriate
4. Measures precision errors

### Exercise 4: Fixed-Point Math

Write a program that:
1. Implements common math operations with fixed-point
2. Calculates distances, angles, etc.
3. Uses fixed-point for all calculations
4. Converts to integers only for rendering

---

**Previous Chapter:** [Chapter 18: Physics and Movement](../18_PhysicsAndMovement/README.md)  
**Next Section:** [Math Functions and Constants](./02_MathFunctionsAndConstants.md)  
**Language Specification:** See [Type System](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

