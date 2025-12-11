# Linear Algebra

**Part of:** [Chapter 19: Mathematics for Graphics and Games](./README.md)  
**Level:** Advanced (Optional)

---

## Introduction

**Linear algebra** is the mathematics of vectors and matrices. It's essential for:
- **Graphics transformations** — Rotate, scale, translate objects
- **Physics** — Forces, velocities, accelerations
- **3D graphics** — 3D transformations, projections
- **Game mechanics** — Collision detection, pathfinding

This section builds on matrices from Chapter 06 and shows how linear algebra powers graphics and physics.

**Note:** This is an **advanced section** for students interested in deeper mathematics and advanced graphics.

---

## What is Linear Algebra?

**Linear algebra** studies:
- **Vectors** — Quantities with magnitude and direction
- **Matrices** — Arrays of numbers representing transformations
- **Vector spaces** — Collections of vectors
- **Linear transformations** — Operations that preserve vector addition and scalar multiplication

**Applications:**
- **Graphics** — Transform objects
- **Physics** — Represent forces, velocities
- **Game development** — Collision, physics, graphics

---

## Vectors

### What is a Vector?

A **vector** represents:
- **Magnitude** — How much (length)
- **Direction** — Which way

**2D Vector:**
```pascal
type
  TVec2 = record
    X, Y: Q8.8;
  end;
```

**3D Vector:**
```pascal
type
  TVec3 = record
    X, Y, Z: Q8.8;
  end;
```

### Vector Representation

**Vectors as points:**
```pascal
var position: TVec2;
begin
  position.X := 10.5;
  position.Y := 20.25;
  // Vector from origin (0,0) to point (10.5, 20.25)
end.
```

**Vectors as directions:**
```pascal
var direction: TVec2;
begin
  direction.X := 1.0;   // Right
  direction.Y := 0.0;   // Not up/down
  // Unit vector pointing right
end.
```

---

## Vector Operations

### Vector Addition

**Add corresponding components:**
```pascal
function Vec2Add(const A, B: TVec2): TVec2;
var result: TVec2;
begin
  result.X := A.X + B.X;
  result.Y := A.Y + B.Y;
  Vec2Add := result;
end;
```

**Geometric meaning:** Move by vector B from position A.

### Vector Subtraction

**Subtract corresponding components:**
```pascal
function Vec2Subtract(const A, B: TVec2): TVec2;
var result: TVec2;
begin
  result.X := A.X - B.X;
  result.Y := A.Y - B.Y;
  Vec2Subtract := result;
end;
```

**Geometric meaning:** Vector from B to A.

### Scalar Multiplication

**Multiply each component by scalar:**
```pascal
function Vec2Scale(const V: TVec2; scalar: Q8.8): TVec2;
var result: TVec2;
begin
  result.X := V.X * scalar;
  result.Y := V.Y * scalar;
  Vec2Scale := result;
end;
```

**Geometric meaning:** Scale vector length.

### Vector Length (Magnitude)

**Calculate vector length:**
```pascal
function Vec2Length(const V: TVec2): Q8.8;
begin
  // Length = sqrt(X² + Y²)
  Vec2Length := Sqrt(V.X * V.X + V.Y * V.Y);
end;
```

### Normalization

**Make vector unit length:**
```pascal
function Vec2Normalize(const V: TVec2): TVec2;
var len: Q8.8;
var result: TVec2;
begin
  len := Vec2Length(V);
  if len > 0.0 then
  begin
    result.X := V.X / len;
    result.Y := V.Y / len;
  end
  else
  begin
    result.X := 0.0;
    result.Y := 0.0;
  end;
  Vec2Normalize := result;
end;
```

---

## Dot Product

### What is Dot Product?

**Dot product** combines two vectors to produce a scalar:
```
A · B = AₓBₓ + AᵧBᵧ
```

### Implementation

```pascal
function Vec2Dot(const A, B: TVec2): Q8.8;
begin
  Vec2Dot := A.X * B.X + A.Y * B.Y;
end;
```

### Geometric Meaning

**Dot product tells us:**
- **A · B = |A||B|cos(θ)** — Related to angle between vectors
- **A · B > 0** — Vectors point in similar direction
- **A · B < 0** — Vectors point in opposite directions
- **A · B = 0** — Vectors are perpendicular

### Applications

**Angle between vectors:**
```pascal
function Vec2Angle(const A, B: TVec2): Q8.8;
var dot, lenA, lenB, cosAngle: Q8.8;
begin
  dot := Vec2Dot(A, B);
  lenA := Vec2Length(A);
  lenB := Vec2Length(B);
  
  if (lenA > 0.0) and (lenB > 0.0) then
  begin
    cosAngle := dot / (lenA * lenB);
    Vec2Angle := ArcCos(cosAngle);  // Angle in radians
  end
  else
    Vec2Angle := 0.0;
end;
```

**Projection:**
```pascal
function Vec2Project(const A, B: TVec2): TVec2;
var dot, lenSq: Q8.8;
var scale: Q8.8;
begin
  dot := Vec2Dot(A, B);
  lenSq := Vec2Length(B);
  lenSq := lenSq * lenSq;  // Length squared
  
  if lenSq > 0.0 then
  begin
    scale := dot / lenSq;
    Vec2Project := Vec2Scale(B, scale);
  end
  else
  begin
    Vec2Project.X := 0.0;
    Vec2Project.Y := 0.0;
  end;
end;
```

---

## Cross Product (3D)

### What is Cross Product?

**Cross product** (3D only) produces a vector perpendicular to both inputs:
```
A × B = (AᵧBᵢ - AᵢBᵧ, AᵢBₓ - AₓBᵢ, AₓBᵧ - AᵧBₓ)
```

### Implementation

```pascal
function Vec3Cross(const A, B: TVec3): TVec3;
var result: TVec3;
begin
  result.X := A.Y * B.Z - A.Z * B.Y;
  result.Y := A.Z * B.X - A.X * B.Z;
  result.Z := A.X * B.Y - A.Y * B.X;
  Vec3Cross := result;
end;
```

### Applications

**Normal vector (perpendicular):**
```pascal
// Normal to triangle (for 3D graphics)
function TriangleNormal(const P0, P1, P2: TVec3): TVec3;
var edge1, edge2: TVec3;
begin
  edge1 := Vec3Subtract(P1, P0);
  edge2 := Vec3Subtract(P2, P0);
  TriangleNormal := Vec3Cross(edge1, edge2);
  TriangleNormal := Vec3Normalize(TriangleNormal);
end;
```

---

## Matrix Transformations

### Rotation Matrix (2D)

**Rotate vector by angle θ:**
```pascal
procedure CreateRotationMatrix2D(angle: Q8.8; var mat: array[0..1, 0..1] of Q8.8);
var cosA, sinA: Q8.8;
begin
  cosA := Cos(angle);
  sinA := Sin(angle);
  
  // [cos(θ)  -sin(θ)]
  // [sin(θ)   cos(θ)]
  mat[0, 0] := cosA;  mat[0, 1] := -sinA;
  mat[1, 0] := sinA;  mat[1, 1] := cosA;
end;

function RotateVector2D(const V: TVec2; angle: Q8.8): TVec2;
var rotMat: array[0..1, 0..1] of Q8.8;
var result: TVec2;
begin
  CreateRotationMatrix2D(angle, rotMat);
  
  // Multiply matrix by vector
  result.X := rotMat[0, 0] * V.X + rotMat[0, 1] * V.Y;
  result.Y := rotMat[1, 0] * V.X + rotMat[1, 1] * V.Y;
  
  RotateVector2D := result;
end;
```

### Scaling Matrix

**Scale vector:**
```pascal
procedure CreateScaleMatrix2D(scaleX, scaleY: Q8.8; var mat: array[0..1, 0..1] of Q8.8);
begin
  // [sx  0 ]
  // [0   sy]
  mat[0, 0] := scaleX;  mat[0, 1] := 0.0;
  mat[1, 0] := 0.0;     mat[1, 1] := scaleY;
end;
```

### Translation (Homogeneous Coordinates)

**Translation requires 3×3 matrix (homogeneous coordinates):**
```pascal
type
  TVec2H = record
    X, Y, W: Q8.8;  // Homogeneous coordinates (W usually 1)
  end;

procedure CreateTranslationMatrix2D(tx, ty: Q8.8; var mat: array[0..2, 0..2] of Q8.8);
begin
  // [1  0  tx]
  // [0  1  ty]
  // [0  0  1 ]
  CreateIdentityMatrix3x3(mat);
  mat[0, 2] := tx;
  mat[1, 2] := ty;
end;
```

### Combining Transformations

**Multiply transformation matrices:**
```pascal
// Combine: Scale, then Rotate, then Translate
procedure CreateTransform2D(scaleX, scaleY, angle, tx, ty: Q8.8;
                             var result: array[0..2, 0..2] of Q8.8);
var scaleMat, rotMat, transMat, temp: array[0..2, 0..2] of Q8.8;
begin
  CreateScaleMatrix2D(scaleX, scaleY, scaleMat);
  CreateRotationMatrix2D(angle, rotMat);
  CreateTranslationMatrix2D(tx, ty, transMat);
  
  // Combine: result = transMat × rotMat × scaleMat
  MatrixMultiply3x3(rotMat, scaleMat, temp);
  MatrixMultiply3x3(transMat, temp, result);
end;
```

---

## Linear Systems

### What is a Linear System?

**Linear system** — Set of linear equations:
```
a₁x + b₁y = c₁
a₂x + b₂y = c₂
```

**Matrix form:**
```
[a₁  b₁] [x]   [c₁]
[a₂  b₂] [y] = [c₂]
```

### Solving Linear Systems

**Gaussian elimination (simplified 2×2):**
```pascal
function SolveLinearSystem2x2(const A: array[0..1, 0..1] of Q8.8;
                               const B: array[0..1] of Q8.8;
                               var X: array[0..1] of Q8.8): boolean;
var det: Q8.8;
var invA: array[0..1, 0..1] of Q8.8;
begin
  // det = a₀₀a₁₁ - a₀₁a₁₀
  det := A[0, 0] * A[1, 1] - A[0, 1] * A[1, 0];
  
  if det = 0.0 then
  begin
    SolveLinearSystem2x2 := false;  // No unique solution
    Exit;
  end;
  
  // Inverse matrix
  invA[0, 0] := A[1, 1] / det;
  invA[0, 1] := -A[0, 1] / det;
  invA[1, 0] := -A[1, 0] / det;
  invA[1, 1] := A[0, 0] / det;
  
  // X = A⁻¹B
  X[0] := invA[0, 0] * B[0] + invA[0, 1] * B[1];
  X[1] := invA[1, 0] * B[0] + invA[1, 1] * B[1];
  
  SolveLinearSystem2x2 := true;
end;
```

---

## Applications in Games

### Collision Detection

**Line-line intersection:**
```pascal
function LineIntersect(const P1, P2, P3, P4: TVec2;
                      var intersection: TVec2): boolean;
var d: Q8.8;
var t, u: Q8.8;
var dir1, dir2: TVec2;
begin
  dir1 := Vec2Subtract(P2, P1);
  dir2 := Vec2Subtract(P4, P3);
  
  // d = (x₂-x₁)(y₄-y₃) - (y₂-y₁)(x₄-x₃)
  d := dir1.X * dir2.Y - dir1.Y * dir2.X;
  
  if d = 0.0 then
  begin
    LineIntersect := false;  // Parallel lines
    Exit;
  end;
  
  var diff: TVec2;
  diff := Vec2Subtract(P3, P1);
  
  t := (diff.X * dir2.Y - diff.Y * dir2.X) / d;
  u := (diff.X * dir1.Y - diff.Y * dir1.X) / d;
  
  if (t >= 0.0) and (t <= 1.0) and (u >= 0.0) and (u <= 1.0) then
  begin
    intersection.X := P1.X + t * dir1.X;
    intersection.Y := P1.Y + t * dir1.Y;
    LineIntersect := true;
  end
  else
    LineIntersect := false;
end;
```

### Physics: Forces

**Apply force to object:**
```pascal
type
  TObject = record
    Position: TVec2;
    Velocity: TVec2;
    Mass: Q8.8;
  end;

procedure ApplyForce(var obj: TObject; const force: TVec2; deltaTime: Q8.8);
var acceleration: TVec2;
begin
  // F = ma, so a = F/m
  acceleration := Vec2Scale(force, 1.0 / obj.Mass);
  
  // v = v + a*dt
  obj.Velocity := Vec2Add(obj.Velocity, Vec2Scale(acceleration, deltaTime));
  
  // p = p + v*dt
  obj.Position := Vec2Add(obj.Position, Vec2Scale(obj.Velocity, deltaTime));
end;
```

### Graphics: Sprite Transformation

**Transform sprite vertices:**
```pascal
procedure TransformSprite(var vertices: array[0..3] of TVec2;
                         const transform: array[0..2, 0..2] of Q8.8);
var i: integer;
var v: TVec2;
begin
  for i := 0 to 3 do
  begin
    v := vertices[i];
    // Apply transformation matrix
    vertices[i].X := transform[0, 0] * v.X + transform[0, 1] * v.Y + transform[0, 2];
    vertices[i].Y := transform[1, 0] * v.X + transform[1, 1] * v.Y + transform[1, 2];
  end;
end;
```

---

## Vector Examples

### Example 1: Movement

```pascal
var position, velocity: TVec2;
begin
  position.X := 10.0;
  position.Y := 20.0;
  velocity.X := 1.0;
  velocity.Y := 0.5;
  
  // Update position
  position := Vec2Add(position, velocity);
  // position is now (11.0, 20.5)
end.
```

### Example 2: Direction to Target

```pascal
function DirectionToTarget(const from, to: TVec2): TVec2;
var direction: TVec2;
begin
  direction := Vec2Subtract(to, from);
  DirectionToTarget := Vec2Normalize(direction);
end;
```

### Example 3: Distance

```pascal
function Distance(const A, B: TVec2): Q8.8;
var diff: TVec2;
begin
  diff := Vec2Subtract(B, A);
  Distance := Vec2Length(diff);
end;
```

---

## Best Practices

### 1. Use Appropriate Precision

**2D vectors:** Q8.8 is usually sufficient  
**3D vectors:** Q12.12 for better precision  
**Transformations:** Match vector precision

### 2. Normalize When Needed

**Normalize direction vectors:**
```pascal
var direction: TVec2;
direction := DirectionToTarget(playerPos, targetPos);
direction := Vec2Normalize(direction);  // Unit vector
```

### 3. Cache Calculations

**Cache expensive operations:**
```pascal
// Cache length if used multiple times
var len: Q8.8;
len := Vec2Length(vector);
// Use len instead of recalculating
```

---

## Platform Considerations

### Performance

**Vector operations:**
- **Fast** — Simple arithmetic
- **Cache-friendly** — Vectors are small
- **On ZealZ80** — Keep vectors 2D when possible (3D is more expensive)

### Memory

**Vectors are small:**
- **2D vector** — 4 bytes (2 × Q8.8)
- **3D vector** — 6-8 bytes (3 × Q8.8 or Q12.12)
- **Efficient** — Very memory-friendly

---

## Summary

**Key Concepts:**
- **Vectors** represent magnitude and direction
- **Vector operations** — Addition, subtraction, scalar multiplication
- **Dot product** — Angle between vectors, projection
- **Cross product** — Perpendicular vector (3D)
- **Matrix transformations** — Rotate, scale, translate
- **Linear systems** — Solve equations

**Vector Operations:**
- Addition — Add components
- Subtraction — Subtract components
- Scalar multiplication — Scale components
- Length — Magnitude of vector
- Normalization — Unit vector
- Dot product — Scalar result
- Cross product — Vector result (3D)

**Applications:**
- Graphics transformations
- Physics forces
- Collision detection
- Movement and direction
- Distance calculations

**Next:** Learn about calculus basics for rates of change and optimization.

---

**Next Section:** [Calculus Basics](./06_CalculusBasics.md)  
**Language Specification:** See [03_TypeSystem.md](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

