# Math Library

**Location:** `SuperPascal/lib/math/`

---

## Overview

Mathematical algorithms library for SuperPascal, providing fixed-point arithmetic, matrix operations, trigonometry, square root, and vector math.

**Source Material:**
- `algorithms/01_FixedPointArithmetic.md`
- `algorithms/02_MathematicalAlgorithms.md`
- Mikro Documentation Archive: `FIXEDPOI.TXT`, `OTMMATX.TXT`, `SIN.TXT`, `SINCOSC.TXT`, `SQROOT.TXT`, `SQRT.TXT`

---

## Module Structure

```
lib/math/
├── mod.pas          # Main module entry point (unit Math)
├── types.pas        # Core types and constants (unit Math_Types)
├── fixed.pas        # Fixed-point arithmetic (unit Math_Fixed)
├── matrix.pas       # Matrix operations (unit Math_Matrix)
├── trig.pas         # Trigonometry (unit Math_Trig)
├── sqrt.pas         # Square root (unit Math_Sqrt)
├── vector.pas       # Vector math (unit Math_Vector)
└── README.md        # This file
```

---

## Modules

### `types.pas` - Core Types

**Exports:**
- `Fixed16`, `Fixed32` - Fixed-point types
- `TVector2`, `TVector3` - Vector types
- `TMatrix2x2`, `TMatrix3x3`, `TMatrix4x4` - Matrix types
- Math constants (PI, E, TAU, etc.)

### `fixed.pas` - Fixed-Point Arithmetic

**Exports:**
- Conversion: `IntToFixed16`, `Fixed16ToInt`, `IntToFixed32`, `Fixed32ToInt`
- Arithmetic: `Fixed16Add`, `Fixed16Sub`, `Fixed16Mul`, `Fixed16Div`
- Comparison: `Fixed16Eq`, `Fixed16Lt`, `Fixed16Gt`, etc.
- Advanced: `Fixed16Sqrt`, `Fixed16Pow`

**Source:** `algorithms/01_FixedPointArithmetic.md`

### `matrix.pas` - Matrix Operations

**Exports:**
- `MatrixIdentity` - Create identity matrix
- `MatrixMultiply` - Multiply two matrices
- `MatrixRotateX/Y/Z` - Rotation matrices
- `MatrixTranslate` - Translation matrix
- `MatrixScale` - Scale matrix
- `TransformVector` - Transform vector by matrix

**Source:** `algorithms/02_MathematicalAlgorithms.md` (OTMMATX.TXT)

### `trig.pas` - Trigonometry

**Exports:**

#### Low-Precision Angle Functions (0-255 range, 360° circle, ~1.4° per step, 8-bit)
- `SinFixed(angle: integer): Fixed16` - Sine (angle: 0-255, representing 0-360°)
- `CosFixed(angle: integer): Fixed16` - Cosine (angle: 0-255)
- `TanFixed(angle: integer): Fixed16` - Tangent (angle: 0-255)

#### High-Precision Signed Angle Functions (-180 to +180, full 360° circle, ~0.0055° per step, 16-bit)
- `SinSigned(angle: SmallInt): Fixed16` - Sine (angle: -180 to +180, SmallInt-based)
- `CosSigned(angle: SmallInt): Fixed16` - Cosine (angle: -180 to +180)
- `TanSigned(angle: SmallInt): Fixed16` - Tangent (angle: -180 to +180)
- **Note:** -180° = +180° (same point on unit circle), so this covers the full 360° circle efficiently

#### Radian-Based Functions (Fixed16 radians, standard math interface)
- `RadiansToFixedAngle(radians: Fixed16): integer` - Convert radians to low-precision (0-255)
- `RadiansToSignedAngle(radians: Fixed16): SmallInt` - Convert radians to high-precision (-180 to +180)
- `FixedAngleToRadians(angle: integer): Fixed16` - Convert low-precision (0-255) to radians
- `SignedAngleToRadians(angle: SmallInt): Fixed16` - Convert high-precision (-180 to +180) to radians
- `SinR(radians: Fixed16): Fixed16` - Sine (radians, uses high-precision signed angles)
- `CosR(radians: Fixed16): Fixed16` - Cosine (radians, uses high-precision signed angles)
- `TanR(radians: Fixed16): Fixed16` - Tangent (radians, uses high-precision signed angles)

#### Degree-Based Functions (integer degrees, convenient for users)
- `DegreesToFixedAngle(degrees: integer): integer` - Convert degrees to low-precision (0-255)
- `DegreesToSignedAngle(degrees: integer): SmallInt` - Convert degrees to high-precision (-180 to +180)
- `FixedAngleToDegrees(angle: integer): integer` - Convert low-precision (0-255) to degrees
- `SignedAngleToDegrees(angle: SmallInt): integer` - Convert high-precision (-180 to +180) to degrees
- `DegreesToRadians(degrees: integer): Fixed16` - Convert degrees to radians
- `RadiansToDegrees(radians: Fixed16): integer` - Convert radians to degrees
- `SinD(degrees: integer): Fixed16` - Sine (degrees, uses high-precision signed angles)
- `CosD(degrees: integer): Fixed16` - Cosine (degrees, uses high-precision signed angles)
- `TanD(degrees: integer): Fixed16` - Tangent (degrees, uses high-precision signed angles)

#### Inverse Functions (return Fixed16 radians)
- `ArcSinFixed(x: Fixed16): Fixed16` - Arcsine (returns radians)
- `ArcCosFixed(x: Fixed16): Fixed16` - Arccosine (returns radians)
- `ArcTanFixed(x: Fixed16): Fixed16` - Arctangent (returns radians)
- `ArcTan2Fixed(y, x: Fixed16): Fixed16` - Arctangent2 (returns radians, handles all quadrants)

#### Inverse Functions (return integer degrees)
- `ArcSinD(x: Fixed16): integer` - Arcsine (returns degrees)
- `ArcCosD(x: Fixed16): integer` - Arccosine (returns degrees)
- `ArcTanD(x: Fixed16): integer` - Arctangent (returns degrees)
- `ArcTan2D(y, x: Fixed16): integer` - Arctangent2 (returns degrees, handles all quadrants)

#### Table Initialization
- `InitTrigTables` - Initialize lookup tables (recursive method, no floating-point)
- `GenerateTrigTables` - Generate tables using floating-point (FPU platforms only)
- `GenerateSinTableRecursive` - Recursive table generation (no floating-point)

**Features:**
- **Two angle systems for full 360° circle:**
  - **Low precision (0-255):** Fast, 8-bit, 256 discrete angles (~1.4° per step)
  - **High precision (-180 to +180):** SmallInt-based, 65536 discrete angles (~0.0055° per step), uses interpolation
- **Memory efficient:** Only 512 bytes for base lookup tables (256 entries × 2 bytes × 2 tables)
- **Integer-only by default:** Works on all platforms including no-FPU
- **Lookup table optimization:** Fast 256-entry sin/cos tables with linear interpolation for high precision
- **Recursive generation:** No floating-point required for table initialization

**Source:** `algorithms/02_MathematicalAlgorithms.md` (SIN.TXT, SINCOSC.TXT)

### `sqrt.pas` - Square Root

**Exports:**
- `InitSqrtTable` - Initialize lookup table
- `IntegerSqrt` - Fast integer square root
- `FixedSqrt`, `Fixed16Sqrt` - Fixed-point square root

**Source:** `algorithms/02_MathematicalAlgorithms.md` (SQROOT.TXT, SQRT.TXT)

### `vector.pas` - Vector Math

**Exports:**
- Vector2: `Vector2Add`, `Vector2Sub`, `Vector2Scale`, `Vector2Dot`, `Vector2Length`, `Vector2Normalize`
- Vector3: `Vector3Add`, `Vector3Sub`, `Vector3Scale`, `Vector3Dot`, `Vector3Cross`, `Vector3Length`, `Vector3Normalize`

**Source:** `algorithms/02_MathematicalAlgorithms.md`

---

## Usage

### Simple Usage (Import Everything)

```pascal
program MathDemo;
uses Math;  // Imports mod.pas, which re-exports everything

var
  a, b, result: Fixed16;
  vec: TVector2;
  matrix: TMatrix4x4;
begin
  // Fixed-point arithmetic
  a := IntToFixed16(100);
  b := IntToFixed16(200);
  result := Fixed16Mul(a, b);
  
  // Vector math
  vec.X := IntToFixed16(10);
  vec.Y := IntToFixed16(20);
  vec := Vector2Normalize(vec);
  
  // Matrix operations
  matrix := MatrixIdentity;
  matrix := MatrixRotateX(IntToFixed16(45));
  
  // Trigonometry
  result := SinFixed(64);  // sin(90 degrees) - low precision
  result := SinSigned(16384);  // sin(90 degrees) - high precision (-180 to +180)
  result := SinD(90);  // sin(90 degrees) - degree-based, uses high precision
end.
```

### Advanced Usage (Import Specific Modules)

```pascal
program MathDemo;
uses
  Math_Types,    // Only types
  Math_Fixed,    // Fixed-point arithmetic
  Math_Vector;   // Vector math

var
  a: Math_Types.Fixed16;
begin
  a := Math_Fixed.IntToFixed16(100);
end.
```

---

## Dependencies

```
Math (mod.pas)
  ├── Math_Types (no dependencies)
  ├── Math_Fixed
  │     └── Math_Types
  ├── Math_Matrix
  │     ├── Math_Types
  │     └── Math_Fixed
  ├── Math_Trig
  │     ├── Math_Types
  │     └── Math_Fixed
  ├── Math_Sqrt
  │     ├── Math_Types
  │     └── Math_Fixed
  └── Math_Vector
        ├── Math_Types
        ├── Math_Fixed
        └── Math_Sqrt
```

---

## Platform Support

**All Platforms:**
- Fixed-point arithmetic (mandatory for no-FPU platforms)
- Matrix operations
- Trigonometry (lookup tables)
- Square root (fast integer approximation)
- Vector math

**Performance Notes:**
- Fixed-point operations are very fast on all platforms
- Trigonometry uses lookup tables (O(1) access)
- Square root uses fast integer approximation (16-27 cycles)
- Matrix operations use fixed-point multiply (optimized per platform)

---

## Implementation Status

- ✅ `types.pas` - Complete
- ✅ `fixed.pas` - Complete
- ✅ `matrix.pas` - Complete (trig integration pending)
- ✅ `trig.pas` - Complete
- ✅ `sqrt.pas` - Complete
- ✅ `vector.pas` - Complete
- ✅ `mod.pas` - Complete

**Note:** `matrix.pas` rotation functions use `Math_Trig` for sin/cos lookups (fully integrated).

---

**Last Updated:** 2025-01-XX  
**Status:** Math library complete (6 modules)

