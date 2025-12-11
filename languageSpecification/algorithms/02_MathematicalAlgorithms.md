# Mathematical Algorithms

**Part of:** [Algorithms Appendix](../99_Algorithms_Appendix.md)

---

## Overview

Mathematical algorithms for graphics, games, and general computation. These algorithms are **generic** and work on **all platforms**.

**Source Material:** Mikro Documentation Archive

**See Also:**
- Book Chapter: [Chapter 19: Mathematics for Graphics and Games](../../book/19_MathematicsForGraphicsAndGames/README.md)
- Standard Library: [Math Unit](../13_StandardLibrary.md#math-functions)
- Fixed-Point Arithmetic: [Fixed-Point Arithmetic](./01_FixedPointArithmetic.md)

---

## Matrix Math

**Source:** `docs/mikro_docs_archive/Coding/1/OTMMATX.TXT`

### Matrix Representation

A 4x4 transformation matrix represents:
- **X, Y, Z columns:** World space coordinates of local axis vectors (unit vectors)
- **C column:** Translation/origin (always [0, 0, 0, 1])
- **O row:** World space coordinates of object's origin

**Identity Matrix:**
```pascal
type
  TMatrix4x4 = array[0..3, 0..3] of Fixed16;  // Using fixed-point

const
  IdentityMatrix: TMatrix4x4 = (
    (65536, 0,      0,      0),      // X axis: (1, 0, 0, 0)
    (0,      65536, 0,      0),      // Y axis: (0, 1, 0, 0)
    (0,      0,      65536, 0),      // Z axis: (0, 0, 1, 0)
    (0,      0,      0,      65536)  // Origin: (0, 0, 0, 1)
  );
```

### Matrix Multiplication

**Algorithm:** Concatenate two transformation matrices

```pascal
procedure MatrixMultiply(var a: TMatrix4x4; const b: TMatrix4x4);
var
  temp: TMatrix4x4;
  i, j: integer;
begin
  // Transform by columns first, then by rows
  for j := 0 to 3 do
    for i := 0 to 3 do
      temp[i, j] := Fixed16Mul(a[i, 0], b[0, j]) +
                    Fixed16Mul(a[i, 1], b[1, j]) +
                    Fixed16Mul(a[i, 2], b[2, j]) +
                    Fixed16Mul(a[i, 3], b[3, j]);
  
  // Copy result back to matrix a
  a := temp;
end;
```

**Note:** Matrix multiplication is **not commutative**: `[a] * [b] ≠ [b] * [a]`

### Transform Vector by Matrix

**Algorithm:** Project vector onto transformed axis vectors using dot product

```pascal
type
  TVector3 = record
    X, Y, Z: Fixed16;
  end;

function TransformVector(const v: TVector3; const m: TMatrix4x4): TVector3;
begin
  // Transform using dot product with matrix columns
  Result.X := Fixed16Mul(v.X, m[0, 0]) +
              Fixed16Mul(v.Y, m[1, 0]) +
              Fixed16Mul(v.Z, m[2, 0]) +
              m[3, 0];  // Translation
  
  Result.Y := Fixed16Mul(v.X, m[0, 1]) +
              Fixed16Mul(v.Y, m[1, 1]) +
              Fixed16Mul(v.Z, m[2, 1]) +
              m[3, 1];  // Translation
  
  Result.Z := Fixed16Mul(v.X, m[0, 2]) +
              Fixed16Mul(v.Y, m[1, 2]) +
              Fixed16Mul(v.Z, m[2, 2]) +
              m[3, 2];  // Translation
end;
```

### Rotation Matrices

**Rotate about X axis:**
```pascal
function MatrixRotateX(angle: Fixed16): TMatrix4x4;
var
  cosA, sinA: Fixed16;
begin
  cosA := CosFixed(angle);
  sinA := SinFixed(angle);
  
  Result := IdentityMatrix;
  Result[1, 1] := cosA;
  Result[1, 2] := sinA;
  Result[2, 1] := -sinA;
  Result[2, 2] := cosA;
end;
```

**Rotate about Y axis:**
```pascal
function MatrixRotateY(angle: Fixed16): TMatrix4x4;
var
  cosA, sinA: Fixed16;
begin
  cosA := CosFixed(angle);
  sinA := SinFixed(angle);
  
  Result := IdentityMatrix;
  Result[0, 0] := cosA;
  Result[0, 2] := -sinA;
  Result[2, 0] := sinA;
  Result[2, 2] := cosA;
end;
```

**Rotate about Z axis:**
```pascal
function MatrixRotateZ(angle: Fixed16): TMatrix4x4;
var
  cosA, sinA: Fixed16;
begin
  cosA := CosFixed(angle);
  sinA := SinFixed(angle);
  
  Result := IdentityMatrix;
  Result[0, 0] := cosA;
  Result[0, 1] := sinA;
  Result[1, 0] := -sinA;
  Result[1, 1] := cosA;
end;
```

### Translation Matrix

```pascal
function MatrixTranslate(x, y, z: Fixed16): TMatrix4x4;
begin
  Result := IdentityMatrix;
  Result[3, 0] := x;
  Result[3, 1] := y;
  Result[3, 2] := z;
end;
```

### Scale Matrix

```pascal
function MatrixScale(sx, sy, sz: Fixed16): TMatrix4x4;
begin
  Result := IdentityMatrix;
  Result[0, 0] := sx;
  Result[1, 1] := sy;
  Result[2, 2] := sz;
end;
```

---

## Trigonometry

**Source:** `docs/mikro_docs_archive/Coding/2/SIN.TXT`, `SINCOSC.TXT`

### Lookup Tables

**Pre-compute sin/cos tables for fast access:**

```pascal
var
  SinTable: array[0..255] of Fixed16;
  CosTable: array[0..255] of Fixed16;

procedure GenerateTrigTables;
var
  i: integer;
  angle: Real;
begin
  for i := 0 to 255 do
  begin
    angle := (i * 2.0 * PI) / 256.0;  // Convert to radians
    SinTable[i] := Trunc(Sin(angle) * 32767.0);
    CosTable[i] := Trunc(Cos(angle) * 32767.0);
  end;
end;

function SinFixed(angle: integer): Fixed16;  // angle: 0-255
begin
  Result := SinTable[angle and $FF];
end;

function CosFixed(angle: integer): Fixed16;
begin
  Result := CosTable[angle and $FF];
end;
```

**Space Optimization:** Use only sin table, compute cos as `sin(angle + 64)` (64 = 90° in 256° circle)

### Recursive Sin/Cos Generation

**Source:** `docs/mikro_docs_archive/Coding/2/SIN.TXT`

**Algorithm:** Generate sin/cos using recursive formula (no floating-point needed)

```pascal
procedure GenerateSinTableRecursive(var table: array[0..1023] of Fixed16);
var
  i: integer;
  cos2PiN: Fixed16;  // cos(2π/N) where N = 1024
begin
  // First two values: cos(0) = 1, cos(2π/1024)
  table[0] := 16777216;  // 2^24 (scaling factor)
  table[1] := 16776900;  // 2^24 * cos(2π/1024) (pre-computed)
  
  cos2PiN := table[1];
  
  // Recursive formula: cos(k) = 2*cos(2π/N)*cos(k-1) - cos(k-2)
  for i := 2 to 1023 do
  begin
    table[i] := Fixed16Mul(Fixed16Mul(2, cos2PiN), table[i - 1]) - table[i - 2];
    // Shift right to maintain precision
    table[i] := table[i] shr 23;
  end;
end;
```

**Formula:**
- `cos(k) = 2*cos(2π/N)*cos(k-1) - cos(k-2)`
- `sin(k) = 2*cos(2π/N)*sin(k-1) - sin(k-2)`

---

## Square Root

**Source:** `docs/mikro_docs_archive/Coding/1/SQROOT.TXT`

### Integer Square Root (Fast Approximation)

**Algorithm:** Binary search for highest bit, then lookup table

**Performance:** 16-27 cycles (much faster than FPU on systems without hardware sqrt)

```pascal
var
  SqrtTable: array[0..255] of byte;

procedure SetupSqrtTable;
var
  i: integer;
begin
  for i := 0 to 255 do
    SqrtTable[i] := Trunc(256.0 * Sqrt(i / 256.0));
end;

function IntegerSqrt(n: LongInt): integer;
var
  bitPos: integer;
  shifted: LongInt;
begin
  if n = 0 then
  begin
    Result := 0;
    exit;
  end;
  
  // Find highest bit position using binary search
  if n >= $10000000 then
    bitPos := 30  // Bit 30-31
  else if n >= $1000000 then
    bitPos := 26  // Bit 26-29
  else if n >= $100000 then
    bitPos := 20  // Bit 20-25
  else if n >= $10000 then
    bitPos := 16  // Bit 16-19
  else if n >= $1000 then
    bitPos := 12  // Bit 12-15
  else if n >= $100 then
    bitPos := 8   // Bit 8-11
  else if n >= $10 then
    bitPos := 4   // Bit 4-7
  else
    bitPos := 0;  // Bit 0-3
  
  // Shift to get value in 0..255 range
  shifted := n shr (bitPos - 8);
  
  // Lookup in table
  Result := SqrtTable[shifted and $FF];
  
  // Shift result back
  Result := Result shl ((bitPos div 2) - 4);
end;
```

**Accuracy:** Error < 0.75% for most numbers, improves for larger numbers

**Fixed-Point Square Root:**
```pascal
function FixedSqrt(x: Fixed16): Fixed16;
begin
  // Square root of fixed-point: sqrt(x * 2^16) = sqrt(x) * 2^8
  Result := IntegerSqrt(x) shl 8;
end;
```

---

## Vector Math

### Vector Types

```pascal
type
  TVector2 = record
    X, Y: Fixed16;
  end;
  
  TVector3 = record
    X, Y, Z: Fixed16;
  end;
```

### Vector Addition

```pascal
function Vector2Add(const a, b: TVector2): TVector2;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
end;

function Vector3Add(const a, b: TVector3): TVector3;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
  Result.Z := a.Z + b.Z;
end;
```

### Vector Subtraction

```pascal
function Vector2Sub(const a, b: TVector2): TVector2;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
end;

function Vector3Sub(const a, b: TVector3): TVector3;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
  Result.Z := a.Z - b.Z;
end;
```

### Scalar Multiplication

```pascal
function Vector2Scale(const v: TVector2; s: Fixed16): TVector2;
begin
  Result.X := Fixed16Mul(v.X, s);
  Result.Y := Fixed16Mul(v.Y, s);
end;

function Vector3Scale(const v: TVector3; s: Fixed16): TVector3;
begin
  Result.X := Fixed16Mul(v.X, s);
  Result.Y := Fixed16Mul(v.Y, s);
  Result.Z := Fixed16Mul(v.Z, s);
end;
```

### Dot Product

```pascal
function Vector2Dot(const a, b: TVector2): Fixed16;
begin
  Result := Fixed16Mul(a.X, b.X) + Fixed16Mul(a.Y, b.Y);
end;

function Vector3Dot(const a, b: TVector3): Fixed16;
begin
  Result := Fixed16Mul(a.X, b.X) +
            Fixed16Mul(a.Y, b.Y) +
            Fixed16Mul(a.Z, b.Z);
end;
```

### Cross Product (3D only)

```pascal
function Vector3Cross(const a, b: TVector3): TVector3;
begin
  Result.X := Fixed16Mul(a.Y, b.Z) - Fixed16Mul(a.Z, b.Y);
  Result.Y := Fixed16Mul(a.Z, b.X) - Fixed16Mul(a.X, b.Z);
  Result.Z := Fixed16Mul(a.X, b.Y) - Fixed16Mul(a.Y, b.X);
end;
```

### Vector Length (Magnitude)

```pascal
function Vector2Length(const v: TVector2): Fixed16;
begin
  Result := FixedSqrt(Fixed16Mul(v.X, v.X) + Fixed16Mul(v.Y, v.Y));
end;

function Vector3Length(const v: TVector3): Fixed16;
begin
  Result := FixedSqrt(Fixed16Mul(v.X, v.X) +
                      Fixed16Mul(v.Y, v.Y) +
                      Fixed16Mul(v.Z, v.Z));
end;
```

### Vector Normalization

```pascal
function Vector2Normalize(const v: TVector2): TVector2;
var
  len: Fixed16;
begin
  len := Vector2Length(v);
  if len > 0 then
  begin
    Result.X := Fixed16Div(v.X, len);
    Result.Y := Fixed16Div(v.Y, len);
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

function Vector3Normalize(const v: TVector3): TVector3;
var
  len: Fixed16;
begin
  len := Vector3Length(v);
  if len > 0 then
  begin
    Result.X := Fixed16Div(v.X, len);
    Result.Y := Fixed16Div(v.Y, len);
    Result.Z := Fixed16Div(v.Z, len);
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
    Result.Z := 0;
  end;
end;
```

---

## Performance Notes

**Matrix Operations:**
- Matrix multiplication: O(64) fixed-point multiplies + adds
- Vector transformation: O(12) fixed-point multiplies + adds
- Consider platform-specific optimizations (e.g., m68k has fast 64-bit multiply)

**Trigonometry:**
- Lookup tables: O(1) access time
- Table generation: O(n) one-time cost
- Space: 256 entries × 2 bytes = 512 bytes (very small)

**Square Root:**
- Integer sqrt: 16-27 cycles (much faster than FPU)
- Fixed-point sqrt: Similar performance
- Accuracy: Good enough for most game applications

**Vector Operations:**
- Addition/Subtraction: O(1) - very fast
- Dot product: O(n) where n = dimensions
- Cross product: O(3) - 3D only
- Normalization: O(n) + square root

---

**Previous:** [Fixed-Point Arithmetic](./01_FixedPointArithmetic.md)  
**Next:** [Sorting Algorithms](./03_SortingAlgorithms.md)  
**Last Updated:** 2025-01-XX
