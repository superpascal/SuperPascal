# Fixed-Point Arithmetic

**Part of:** [Algorithms Appendix](../99_Algorithms_Appendix.md)

---

## Overview

Fixed-point arithmetic provides efficient decimal number representation using integers. Critical for systems without floating-point units (FPU).

**Source:** `docs/mikro_docs_archive/Coding/2/FIXEDPOI.TXT`

**Platform Support:**
- **No-FPU platforms** (ZealZ80, CommanderX16, Foenix65C816, FoenixA2560M): Mandatory
- **FPU platforms** (Raspberry Pi 5): Optional performance optimization

**See Also:**
- Book Chapter: [Chapter 19: Fixed-Point Math](../../book/19_MathematicsForGraphicsAndGames/01_FixedPointMath.md)
- Type System: [Type System Specification](../03_TypeSystem.md#fixed-point-types)

---

## Fixed-Point Formats

SuperPascal supports multiple fixed-point formats:

| Format | Type | Size | Integer Bits | Fraction Bits | Range | Precision |
|--------|------|------|--------------|---------------|-------|-----------|
| Q8.8 | `Q8.8` | 16 bits | 8 | 8 | -128.0 to 127.996 | 1/256 ≈ 0.0039 |
| Q12.12 | `Q12.12` | 24 bits | 12 | 12 | -2048.0 to 2047.999 | 1/4096 ≈ 0.00024 |
| 16.16 | `Fixed16` | 32 bits | 16 | 16 | -32768.0 to 32767.999 | 1/65536 ≈ 0.000015 |
| 32.32 | `Fixed32` | 64 bits | 32 | 32 | -2.1B to 2.1B | 1/4.3B ≈ 2.3e-10 |

**Note:** `Fixed16` and `Fixed32` are Tier 2 types (available but not taught until later).

---

## Conversion Algorithms

### Float to Fixed-Point

**Algorithm:**
```pascal
function FloatToFixed16(f: Real): Fixed16;
begin
  // Fixed16 uses 16.16 format (16 bits integer, 16 bits fraction)
  // Multiply by 65536 (2^16) to shift decimal point
  Result := Trunc(f * 65536);
end;
```

**Example:**
```pascal
var f: Real;
var fixed: Fixed16;
begin
  f := 123.456;
  fixed := FloatToFixed16(f);  // 0x01E274 (123 + 29876/65536)
  // Represents: 123.4560546875 (close to 123.456)
end;
```

**Precision:** 16-bit fraction provides ~4-5 decimal digits of precision.

### Fixed-Point to Float

**Algorithm:**
```pascal
function Fixed16ToFloat(f: Fixed16): Real;
begin
  // Divide by 65536 to shift decimal point back
  Result := f / 65536.0;
end;
```

**Example:**
```pascal
var fixed: Fixed16;
var f: Real;
begin
  fixed := 0x01E274;  // 123.4560546875
  f := Fixed16ToFloat(fixed);  // 123.4560546875
end;
```

---

## Arithmetic Operations

### Addition and Subtraction

**Algorithm:** Direct integer addition/subtraction (no adjustment needed)

```pascal
function Fixed16Add(a, b: Fixed16): Fixed16;
begin
  Result := a + b;  // No adjustment needed!
end;

function Fixed16Sub(a, b: Fixed16): Fixed16;
begin
  Result := a - b;  // No adjustment needed!
end;
```

**Why it works:** Fixed-point values are just integers with an implied decimal point. Adding integers is equivalent to adding fixed-point values.

**Example:**
```pascal
var a, b, result: Fixed16;
begin
  a := FloatToFixed16(10.5);   // 0x0A8000
  b := FloatToFixed16(5.25);   // 0x054000
  result := a + b;              // 0x0FC000 = 15.75
end;
```

### Multiplication

**Algorithm:** Multiply then shift right by fraction bits

```pascal
function Fixed16Mul(a, b: Fixed16): Fixed16;
begin
  // Multiply creates 64-bit result, shift right by 16 bits
  // On 32-bit systems: (a * b) shr 16
  // On systems without 64-bit: Use assembly or extended precision
  Result := (Int64(a) * Int64(b)) shr 16;
end;
```

**Why it works:**
- `a = A * 65536` (where A is the real value)
- `b = B * 65536` (where B is the real value)
- `a * b = A * B * 65536 * 65536`
- We want `A * B * 65536`, so divide by 65536 (shift right 16 bits)

**Example:**
```pascal
var a, b, result: Fixed16;
begin
  a := FloatToFixed16(10.5);   // 0x0A8000
  b := FloatToFixed16(2.0);    // 0x020000
  result := Fixed16Mul(a, b);  // 0x150000 = 21.0
end;
```

**Performance Note:** On systems with 64-bit multiply (like m68k), this is very fast. On 8-bit systems, may need extended precision arithmetic.

### Division

**Algorithm:** Shift left then divide

```pascal
function Fixed16Div(a, b: Fixed16): Fixed16;
begin
  // Shift left by 16 bits, then divide
  // On 32-bit systems: (Int64(a) shl 16) div b
  // On systems without 64-bit: Use assembly or extended precision
  Result := (Int64(a) shl 16) div b;
end;
```

**Why it works:**
- `a = A * 65536` (where A is the real value)
- `b = B * 65536` (where B is the real value)
- We want `(A / B) * 65536`
- `(A * 65536) / (B * 65536) = A / B` (no fixed-point factor)
- So we need to multiply by 65536: `(A * 65536) * 65536 / (B * 65536) = (A / B) * 65536`
- Which is: `(a shl 16) / b`

**Example:**
```pascal
var a, b, result: Fixed16;
begin
  a := FloatToFixed16(21.0);   // 0x150000
  b := FloatToFixed16(2.0);    // 0x020000
  result := Fixed16Div(a, b);   // 0x0A8000 = 10.5
end;
```

**Performance Note:** Division is slower than multiplication. Consider using lookup tables or reciprocal multiplication when possible.

---

## Polynomial Evaluation

**Source:** Mikro archive discusses polynomial evaluation with fixed-point.

**Algorithm:** Horner's method with fixed-point

```pascal
// Evaluate: y = A*x*x + B*x + C
function EvaluatePolynomial(x: Fixed16; A, B, C: Fixed16): Fixed16;
var
  xSquared: Fixed16;
begin
  xSquared := Fixed16Mul(x, x);        // x*x
  Result := Fixed16Mul(A, xSquared);   // A*x*x
  Result := Result + Fixed16Mul(B, x); // + B*x
  Result := Result + C;                 // + C
end;
```

**With Rounding (to minimize error):**
```pascal
function EvaluatePolynomialRounded(x: Fixed16; A, B, C: Fixed16): Fixed16;
var
  xSquared: Fixed16;
  half: Fixed16;  // 0.5 in fixed-point = 32768
begin
  half := 32768;  // 0.5 in 16.16 format
  xSquared := Fixed16Mul(x, x);
  Result := Fixed16Mul(A, xSquared);
  Result := Result + Fixed16Mul(B, x);
  Result := Result + C;
  Result := Result + half;  // Add 0.5 for rounding
  Result := Result shr 16;  // Shift back (if needed, depends on format)
end;
```

**Performance:** On MC68000, this takes 5-6 instructions of inline code (very fast).

---

## Lookup Tables for Trigonometry

**Source:** Mikro archive recommends lookup tables for sin/cos.

**Algorithm:** Pre-compute fixed-point sin/cos table

```pascal
// Generate sin table (0-255 degrees, scaled by 32767)
procedure GenerateSinTable(var table: array[0..255] of Fixed16);
var
  i: integer;
  angle: Real;
begin
  for i := 0 to 255 do
  begin
    angle := (i * 2.0 * PI) / 256.0;  // Convert to radians
    table[i] := Trunc(Sin(angle) * 32767.0);  // Scale to Fixed16 range
  end;
end;

// Use sin table
function SinFixed(angle: integer): Fixed16;  // angle: 0-255
begin
  Result := SinTable[angle and $FF];
end;

// Cos using sin table (cos(x) = sin(x+90))
function CosFixed(angle: integer): Fixed16;
begin
  Result := SinFixed((angle + 64) and $FF);  // 64 = 90 degrees in 256-degree circle
end;
```

**Space Optimization:** Since sin and cos share data, you can use sin table from 0-250 degrees and compute cos as `sin(angle + 90)`.

---

## Performance Characteristics

**From Mikro Archive Analysis:**

| Operation | Speed | Notes |
|-----------|-------|-------|
| Addition | ⚡⚡⚡⚡⚡ Very Fast | Direct integer add |
| Subtraction | ⚡⚡⚡⚡⚡ Very Fast | Direct integer subtract |
| Multiplication | ⚡⚡⚡⚡ Fast | One multiply + shift (5-6 instructions on m68k) |
| Division | ⚡⚡⚡ Moderate | One shift + divide (slower than multiply) |
| Conversion | ⚡⚡⚡⚡ Fast | Can be done at compile time |

**Platform-Specific Notes:**
- **8-bit systems** (ZealZ80, CommanderX16): May need extended precision for multiply/divide
- **16-bit systems** (Foenix65C816): Native 32-bit operations available
- **32-bit systems** (FoenixA2560M): Native 64-bit multiply available (very fast)
- **64-bit systems** (Raspberry Pi 5): Native 64-bit operations (fast, but FPU may be faster)

---

## Common Pitfalls

1. **Overflow:** Fixed-point multiply can overflow easily
   - **Solution:** Use larger intermediate types (Int64 for 32-bit fixed-point)
   
2. **Precision Loss:** Repeated operations accumulate errors
   - **Solution:** Periodically snap to integer values, use higher precision (Fixed32)

3. **Division by Zero:** Same as integer division
   - **Solution:** Check for zero before dividing

4. **Range Limitations:** Fixed-point has limited range
   - **Solution:** Choose appropriate format (Q8.8 vs Fixed16 vs Fixed32)

---

## Implementation Notes

**For Compiler Implementation:**
- Fixed-point operations should be inlined when possible
- Use platform-specific optimizations (e.g., m68k has fast 64-bit multiply)
- Consider providing intrinsics for critical operations

**For Runtime:**
- Fixed-point math unit should be in standard library
- Provide both generic and platform-optimized versions
- Document performance characteristics per platform

---

**Previous:** [Algorithms Appendix](../99_Algorithms_Appendix.md)  
**Next:** [Mathematical Algorithms](./02_MathematicalAlgorithms.md)  
**Last Updated:** 2025-01-XX

