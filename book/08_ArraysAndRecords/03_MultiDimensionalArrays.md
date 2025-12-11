# Multi-Dimensional Arrays and Matrices

**Part of:** [Chapter 06: Arrays and Records](./README.md)

---

## Introduction

**Multi-dimensional arrays** let you organize data in grids, tables, and matrices. They're essential for:
- **2D grids** — Game boards, tilemaps, images
- **Matrices** — Mathematical operations, transformations
- **Tables** — Data organized in rows and columns
- **3D data** — Volumes, 3D arrays (advanced)

This chapter focuses on **2D arrays and matrices**, which are fundamental to graphics and game development.

---

## What is a 2D Array?

A **2D array** is an array of arrays:
- **Rows and columns** — Organized in a grid
- **Two indices** — Row index and column index
- **Rectangular** — All rows have the same number of columns
- **Matrix** — Mathematical term for 2D array

**Analogy:** Think of a 2D array like a spreadsheet:
- **Rows** — Horizontal lines (0, 1, 2, ...)
- **Columns** — Vertical lines (0, 1, 2, ...)
- **Cell** — Intersection of row and column
- **Value** — What's stored in each cell

---

## 2D Array Declaration

### Basic Syntax

**2D array declaration:**
```pascal
var arrayName: array[rowRange, colRange] of elementType;
```

**Example:**
```pascal
var grid: array[0..2, 0..2] of integer;
```

**This creates:**
- A 3×3 grid (3 rows, 3 columns)
- 9 total elements
- Each element is an `integer`

### Visual Representation

```
grid[0,0]  grid[0,1]  grid[0,2]
grid[1,0]  grid[1,1]  grid[1,2]
grid[2,0]  grid[2,1]  grid[2,2]
```

### Type Declaration

**You can declare 2D array types:**
```pascal
type
  Matrix3x3 = array[0..2, 0..2] of integer;
  Grid10x10 = array[0..9, 0..9] of integer;

var mat: Matrix3x3;
var board: Grid10x10;
```

---

## Accessing 2D Array Elements

### Indexing

**Access elements using two indices:**
```pascal
var grid: array[0..2, 0..2] of integer;
begin
  grid[0, 0] := 1;  // Top-left
  grid[0, 1] := 2;  // Top-middle
  grid[0, 2] := 3;  // Top-right
  grid[1, 0] := 4;  // Middle-left
  grid[1, 1] := 5;  // Center
  grid[1, 2] := 6;  // Middle-right
  grid[2, 0] := 7;  // Bottom-left
  grid[2, 1] := 8;  // Bottom-middle
  grid[2, 2] := 9;  // Bottom-right
end.
```

### Row and Column Indices

**First index is row, second is column:**
```pascal
grid[row, col] := value;
```

**Example:**
```pascal
grid[1, 2] := 10;  // Row 1, Column 2
```

---

## Processing 2D Arrays

### Pattern 1: Iterate All Elements

```pascal
var grid: array[0..2, 0..2] of integer;
var row, col: integer;
begin
  // Initialize
  for row := 0 to 2 do
    for col := 0 to 2 do
      grid[row, col] := row * 3 + col + 1;
  
  // Print
  for row := 0 to 2 do
  begin
    for col := 0 to 2 do
      Write(grid[row, col], ' ');
    WriteLn;  // New line after each row
  end;
end.
```

**Output:**
```
1 2 3
4 5 6
7 8 9
```

### Pattern 2: Process by Row

```pascal
var grid: array[0..2, 0..2] of integer;
var row, col: integer;
var rowSum: integer;
begin
  // Initialize grid...
  
  // Sum each row
  for row := 0 to 2 do
  begin
    rowSum := 0;
    for col := 0 to 2 do
      rowSum := rowSum + grid[row, col];
    WriteLn('Row ', row, ' sum: ', rowSum);
  end;
end.
```

### Pattern 3: Process by Column

```pascal
var grid: array[0..2, 0..2] of integer;
var row, col: integer;
var colSum: integer;
begin
  // Initialize grid...
  
  // Sum each column
  for col := 0 to 2 do
  begin
    colSum := 0;
    for row := 0 to 2 do
      colSum := colSum + grid[row, col];
    WriteLn('Column ', col, ' sum: ', colSum);
  end;
end.
```

---

## What is a Matrix?

A **matrix** is a mathematical term for a 2D array of numbers. Matrices are used for:
- **Transformations** — Rotate, scale, translate objects
- **Graphics** — 3D graphics, 2D transformations
- **Game development** — Sprite transformations, camera matrices
- **Linear algebra** — Mathematical operations

**Matrix notation:**
```
[ a  b  c ]
[ d  e  f ]
[ g  h  i ]
```

**In code:**
```pascal
var mat: array[0..2, 0..2] of integer;
// mat[0,0]=a, mat[0,1]=b, mat[0,2]=c
// mat[1,0]=d, mat[1,1]=e, mat[1,2]=f
// mat[2,0]=g, mat[2,1]=h, mat[2,2]=i
```

---

## Matrix Operations

### Matrix Addition

**Add corresponding elements:**
```pascal
procedure MatrixAdd(const A, B: array[0..2, 0..2] of integer;
                    var Result: array[0..2, 0..2] of integer);
var i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] := A[i, j] + B[i, j];
end;

var matA, matB, matC: array[0..2, 0..2] of integer;
begin
  // Initialize matA and matB...
  MatrixAdd(matA, matB, matC);
  // matC now contains A + B
end.
```

**Mathematical notation:**
```
C = A + B
C[i,j] = A[i,j] + B[i,j]
```

### Matrix Subtraction

**Subtract corresponding elements:**
```pascal
procedure MatrixSubtract(const A, B: array[0..2, 0..2] of integer;
                         var Result: array[0..2, 0..2] of integer);
var i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] := A[i, j] - B[i, j];
end;
```

### Scalar Multiplication

**Multiply each element by a number:**
```pascal
procedure MatrixScale(var Mat: array[0..2, 0..2] of integer; scalar: integer);
var i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      Mat[i, j] := Mat[i, j] * scalar;
end;
```

---

## Matrix Multiplication

### What is Matrix Multiplication?

**Matrix multiplication** combines two matrices to produce a third matrix. It's **not** element-wise multiplication!

**Key concept:**
- **Row × Column** — Multiply row of first matrix by column of second matrix
- **Dot product** — Sum of products of corresponding elements
- **Result size** — If A is m×n and B is n×p, result is m×p

### How Matrix Multiplication Works

**For 3×3 matrices:**

Given matrices A and B, result C = A × B:

```
C[i,j] = A[i,0]*B[0,j] + A[i,1]*B[1,j] + A[i,2]*B[2,j]
```

**Step-by-step for C[0,0]:**
1. Take row 0 of A: `[A[0,0], A[0,1], A[0,2]]`
2. Take column 0 of B: `[B[0,0], B[1,0], B[2,0]]`
3. Multiply corresponding elements: `A[0,0]*B[0,0] + A[0,1]*B[1,0] + A[0,2]*B[2,0]`
4. Sum the products: This is C[0,0]

### Matrix Multiplication Implementation

```pascal
procedure MatrixMultiply(const A, B: array[0..2, 0..2] of integer;
                         var Result: array[0..2, 0..2] of integer);
var i, j, k: integer;
var sum: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
    begin
      sum := 0;
      for k := 0 to 2 do
        sum := sum + A[i, k] * B[k, j];
      Result[i, j] := sum;
    end;
end;
```

**Explanation:**
- **Outer loops** (i, j) — Iterate through result matrix
- **Inner loop** (k) — Calculate dot product of row i of A and column j of B
- **Sum** — Accumulates the products

### Matrix Multiplication Example

```pascal
var A, B, C: array[0..2, 0..2] of integer;
var i, j: integer;
begin
  // Initialize A
  // [1 2 3]
  // [4 5 6]
  // [7 8 9]
  A[0, 0] := 1; A[0, 1] := 2; A[0, 2] := 3;
  A[1, 0] := 4; A[1, 1] := 5; A[1, 2] := 6;
  A[2, 0] := 7; A[2, 1] := 8; A[2, 2] := 9;
  
  // Initialize B (identity matrix)
  // [1 0 0]
  // [0 1 0]
  // [0 0 1]
  B[0, 0] := 1; B[0, 1] := 0; B[0, 2] := 0;
  B[1, 0] := 0; B[1, 1] := 1; B[1, 2] := 0;
  B[2, 0] := 0; B[2, 1] := 0; B[2, 2] := 1;
  
  // Multiply: C = A × B
  MatrixMultiply(A, B, C);
  
  // C should equal A (multiplying by identity matrix)
  // Print result
  for i := 0 to 2 do
  begin
    for j := 0 to 2 do
      Write(C[i, j], ' ');
    WriteLn;
  end;
end.
```

### Visual Example

**Multiplying:**
```
A = [1  2]    B = [5  6]
    [3  4]        [7  8]

C = A × B

C[0,0] = A[0,0]*B[0,0] + A[0,1]*B[1,0] = 1*5 + 2*7 = 5 + 14 = 19
C[0,1] = A[0,0]*B[0,1] + A[0,1]*B[1,1] = 1*6 + 2*8 = 6 + 16 = 22
C[1,0] = A[1,0]*B[0,0] + A[1,1]*B[1,0] = 3*5 + 4*7 = 15 + 28 = 43
C[1,1] = A[1,0]*B[0,1] + A[1,1]*B[1,1] = 3*6 + 4*8 = 18 + 32 = 50

Result: [19  22]
        [43  50]
```

---

## Special Matrices

### Identity Matrix

**Identity matrix** — Multiplying by it leaves matrix unchanged:
```pascal
procedure CreateIdentity(var Mat: array[0..2, 0..2] of integer);
var i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      if i = j then
        Mat[i, j] := 1
      else
        Mat[i, j] := 0;
end;
```

**Identity matrix (3×3):**
```
[1  0  0]
[0  1  0]
[0  0  1]
```

**Property:** A × I = I × A = A

### Zero Matrix

**Zero matrix** — All elements are zero:
```pascal
procedure CreateZero(var Mat: array[0..2, 0..2] of integer);
var i, j: integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      Mat[i, j] := 0;
end;
```

---

## Matrix Applications for Games

### Why Matrices Matter

**Matrices are used for:**
- **Rotation** — Rotate sprites and objects
- **Scaling** — Make objects bigger or smaller
- **Translation** — Move objects
- **Combining transformations** — Multiple operations in one matrix

**Example:** Rotating a sprite
- **2D rotation matrix** — Rotates points around origin
- **Apply to sprite** — Rotate all sprite points
- **Result** — Rotated sprite

### Preparing for Transformations

**Later chapters will cover:**
- **Rotation matrices** — Using trigonometry (sin, cos)
- **Transformation matrices** — Combining rotation, scale, translation
- **Sprite transformations** — Applying matrices to game objects

**For now:** Understand that matrix multiplication combines transformations.

**Example concept (preview):**
```pascal
// Rotation matrix (will use trigonometry later)
// [cos(θ)  -sin(θ)]
// [sin(θ)   cos(θ)]

// To rotate a point (x, y) by angle θ:
// [x']   [cos(θ)  -sin(θ)] [x]
// [y'] = [sin(θ)   cos(θ)] [y]

// This will be covered in detail in the Mathematics chapter
```

---

## Matrix Examples

### Example 1: Matrix Addition

```pascal
var A, B, C: array[0..2, 0..2] of integer;
var i, j: integer;
begin
  // Initialize A and B
  for i := 0 to 2 do
    for j := 0 to 2 do
    begin
      A[i, j] := i * 3 + j + 1;
      B[i, j] := (i + 1) * 10;
    end;
  
  // Add: C = A + B
  MatrixAdd(A, B, C);
  
  // Print result
  for i := 0 to 2 do
  begin
    for j := 0 to 2 do
      Write(C[i, j], ' ');
    WriteLn;
  end;
end.
```

### Example 2: Matrix Multiplication

```pascal
var A, B, C: array[0..1, 0..1] of integer;
begin
  // A = [1  2]
  //     [3  4]
  A[0, 0] := 1; A[0, 1] := 2;
  A[1, 0] := 3; A[1, 1] := 4;
  
  // B = [5  6]
  //     [7  8]
  B[0, 0] := 5; B[0, 1] := 6;
  B[1, 0] := 7; B[1, 1] := 8;
  
  // C = A × B
  MatrixMultiply(A, B, C);
  
  // C should be [19  22]
  //             [43  50]
  WriteLn('C[0,0] = ', C[0, 0]);  // 19
  WriteLn('C[0,1] = ', C[0, 1]);  // 22
  WriteLn('C[1,0] = ', C[1, 0]);  // 43
  WriteLn('C[1,1] = ', C[1, 1]);  // 50
end.
```

### Example 3: Game Grid

```pascal
var gameBoard: array[0..7, 0..7] of integer;  // 8×8 chess board
var row, col: integer;
begin
  // Initialize board (0 = empty, 1 = piece)
  for row := 0 to 7 do
    for col := 0 to 7 do
      gameBoard[row, col] := 0;
  
  // Place pieces
  gameBoard[0, 0] := 1;  // Piece at top-left
  gameBoard[7, 7] := 1;  // Piece at bottom-right
  
  // Print board
  for row := 0 to 7 do
  begin
    for col := 0 to 7 do
      Write(gameBoard[row, col], ' ');
    WriteLn;
  end;
end.
```

---

## Polygons and Arrays

### What are Polygons?

**Polygons** are shapes made of connected points (vertices). They're fundamental to graphics:
- **Triangles** — 3 vertices (most basic polygon)
- **Quadrilaterals** — 4 vertices (squares, rectangles)
- **Polygons** — N vertices (any number of sides)

**Polygons can be represented using arrays!**

### Representing Polygons as Arrays

**Option 1: Array of Points (Records)**
```pascal
type
  TPoint = record
    X, Y: integer;
  end;

var triangle: array[0..2] of TPoint;
begin
  triangle[0].X := 10; triangle[0].Y := 10;  // Vertex 1
  triangle[1].X := 50; triangle[1].Y := 10;  // Vertex 2
  triangle[2].X := 30; triangle[2].Y := 50;  // Vertex 3
end.
```

**Option 2: 2D Array (Coordinates)**
```pascal
var triangle: array[0..2, 0..1] of integer;  // 3 vertices, X and Y
begin
  triangle[0, 0] := 10; triangle[0, 1] := 10;  // Vertex 1: (10, 10)
  triangle[1, 0] := 50; triangle[1, 1] := 10;  // Vertex 2: (50, 10)
  triangle[2, 0] := 30; triangle[2, 1] := 50;  // Vertex 3: (30, 50)
end.
```

**Both approaches work!** Records are clearer, 2D arrays are more compact.

### Creating Polygons

**Example: Square**
```pascal
type
  TPoint = record
    X, Y: integer;
  end;

var square: array[0..3] of TPoint;
begin
  square[0].X := 10; square[0].Y := 10;  // Top-left
  square[1].X := 50; square[1].Y := 10;  // Top-right
  square[2].X := 50; square[2].Y := 50;  // Bottom-right
  square[3].X := 10; square[3].Y := 50;  // Bottom-left
end.
```

**Example: Pentagon**
```pascal
var pentagon: array[0..4] of TPoint;
var i: integer;
var angle: Q8.8;  // Will use trigonometry later
begin
  // For now, manually set vertices
  // Later: Use trigonometry to calculate positions
  pentagon[0].X := 30; pentagon[0].Y := 10;   // Top
  pentagon[1].X := 50; pentagon[1].Y := 25;   // Top-right
  pentagon[2].X := 45; pentagon[2].Y := 50;   // Bottom-right
  pentagon[3].X := 15; pentagon[3].Y := 50;   // Bottom-left
  pentagon[4].X := 10; pentagon[4].Y := 25;   // Top-left
end.
```

### Processing Polygons

**Example: Draw Polygon (Outline)**
```pascal
procedure DrawPolygon(const vertices: array[0..N-1] of TPoint; vertexCount: integer);
var i: integer;
begin
  // Draw lines connecting vertices
  for i := 0 to vertexCount - 2 do
    DrawLine(vertices[i], vertices[i + 1]);
  
  // Close polygon: connect last vertex to first
  DrawLine(vertices[vertexCount - 1], vertices[0]);
end;
```

**Example: Translate Polygon (Move)**
```pascal
procedure TranslatePolygon(var vertices: array[0..N-1] of TPoint; 
                           vertexCount: integer; 
                           dx, dy: integer);
var i: integer;
begin
  for i := 0 to vertexCount - 1 do
  begin
    vertices[i].X := vertices[i].X + dx;
    vertices[i].Y := vertices[i].Y + dy;
  end;
end;
```

**Example: Scale Polygon**
```pascal
procedure ScalePolygon(var vertices: array[0..N-1] of TPoint;
                       vertexCount: integer;
                       scaleX, scaleY: Q8.8);
var i: integer;
begin
  for i := 0 to vertexCount - 1 do
  begin
    vertices[i].X := Trunc(vertices[i].X * scaleX);
    vertices[i].Y := Trunc(vertices[i].Y * scaleY);
  end;
end;
```

### Polygon Examples

**Example 1: Triangle**
```pascal
type
  TPoint = record
    X, Y: integer;
  end;

var triangle: array[0..2] of TPoint;
begin
  // Create equilateral triangle
  triangle[0].X := 30; triangle[0].Y := 10;   // Top
  triangle[1].X := 10; triangle[1].Y := 50;   // Bottom-left
  triangle[2].X := 50; triangle[2].Y := 50;   // Bottom-right
end.
```

**Example 2: Rectangle**
```pascal
function CreateRectangle(x, y, width, height: integer): array[0..3] of TPoint;
var rect: array[0..3] of TPoint;
begin
  rect[0].X := x;         rect[0].Y := y;          // Top-left
  rect[1].X := x + width; rect[1].Y := y;          // Top-right
  rect[2].X := x + width; rect[2].Y := y + height; // Bottom-right
  rect[3].X := x;         rect[3].Y := y + height; // Bottom-left
  CreateRectangle := rect;
end;
```

**Example 3: Regular Polygon (Preview)**
```pascal
// This will use trigonometry (covered later)
procedure CreateRegularPolygon(var vertices: array[0..N-1] of TPoint;
                                centerX, centerY: integer;
                                radius: integer;
                                sides: integer);
var i: integer;
var angle: Q8.8;
begin
  for i := 0 to sides - 1 do
  begin
    // angle = (2 * π * i) / sides
    // vertices[i].X = centerX + radius * cos(angle)
    // vertices[i].Y = centerY + radius * sin(angle)
    // Will implement with trigonometry functions later
  end;
end;
```

### Why Polygons Matter

**Polygons are the foundation of:**
- **2D Graphics** — All shapes are polygons (or approximated by polygons)
- **Sprites** — Sprite boundaries, collision shapes
- **Game Objects** — Ship shapes, obstacles, platforms
- **UI Elements** — Buttons, panels, custom shapes
- **3D Graphics** — 3D models are made of polygons (triangles)

**Later you'll learn:**
- **Rotation** — Rotate polygons using rotation matrices
- **Collision Detection** — Check if polygons intersect
- **Rendering** — Draw filled polygons
- **Transformation** — Combine translation, rotation, scaling

### Polygon Arrays

**You can have arrays of polygons:**
```pascal
type
  TPoint = record
    X, Y: integer;
  end;
  TPolygon = array[0..9] of TPoint;  // Polygon with up to 10 vertices

var shapes: array[0..4] of TPolygon;  // Array of 5 polygons
var i: integer;
begin
  // Initialize each polygon
  for i := 0 to 4 do
  begin
    // Set vertices for each polygon
    shapes[i][0].X := i * 20;
    shapes[i][0].Y := i * 20;
    // ... set other vertices
  end;
end.
```

### Applications

**Polygons open up:**
- **Custom shapes** — Create any shape you want
- **Dynamic geometry** — Modify shapes at runtime
- **Collision shapes** — Define collision boundaries
- **Procedural generation** — Generate shapes algorithmically
- **Graphics primitives** — Build complex graphics from simple polygons

---

## 3D Arrays (Advanced)

### What is a 3D Array?

**3D arrays** add a third dimension:
```pascal
var cube: array[0..2, 0..2, 0..2] of integer;
```

**Access:**
```pascal
cube[x, y, z] := value;
```

**Use cases:**
- **3D volumes** — Voxel data, 3D grids
- **3D transformations** — 3D graphics matrices (4×4)
- **Advanced games** — 3D game worlds

**Note:** 3D arrays are advanced. Focus on 2D arrays first.

---

## Best Practices

### 1. Use Meaningful Names

**Bad:**
```pascal
var a: array[0..2, 0..2] of integer;
```

**Good:**
```pascal
var rotationMatrix: array[0..2, 0..2] of integer;
var gameGrid: array[0..7, 0..7] of integer;
```

### 2. Use Constants for Dimensions

**Bad:**
```pascal
var grid: array[0..9, 0..9] of integer;
for i := 0 to 9 do  // Magic numbers!
```

**Good:**
```pascal
const GRID_SIZE = 10;
var grid: array[0..GRID_SIZE-1, 0..GRID_SIZE-1] of integer;
for i := 0 to GRID_SIZE-1 do
```

### 3. Keep Matrices Small

**On ZealZ80:**
- **3×3 matrices** — 9 integers = 18 bytes (good)
- **4×4 matrices** — 16 integers = 32 bytes (acceptable)
- **Large matrices** — May not fit in memory

### 4. Understand Matrix Multiplication

**Key points:**
- **Not commutative** — A × B ≠ B × A (usually)
- **Associative** — (A × B) × C = A × (B × C)
- **Order matters** — Apply transformations in correct order

---

## Common Mistakes

### Mistake 1: Wrong Index Order

**Wrong:**
```pascal
var grid: array[0..2, 0..2] of integer;
grid[col, row] := value;  // Wrong: should be [row, col]
```

**Correct:**
```pascal
grid[row, col] := value;  // Correct: row first, then column
```

### Mistake 2: Element-Wise Multiplication

**Wrong:**
```pascal
// This is NOT matrix multiplication!
for i := 0 to 2 do
  for j := 0 to 2 do
    C[i, j] := A[i, j] * B[i, j];  // Element-wise, not matrix multiplication
```

**Correct:**
```pascal
// Use MatrixMultiply procedure for proper matrix multiplication
MatrixMultiply(A, B, C);
```

### Mistake 3: Wrong Matrix Dimensions

**Wrong:**
```pascal
var A: array[0..2, 0..2] of integer;  // 3×3
var B: array[0..1, 0..1] of integer;  // 2×2
MatrixMultiply(A, B, C);  // ERROR: Dimensions don't match!
```

**Correct:**
```pascal
// For A (m×n) × B (n×p), columns of A must equal rows of B
var A: array[0..2, 0..2] of integer;  // 3×3
var B: array[0..2, 0..2] of integer;  // 3×3 (matches!)
MatrixMultiply(A, B, C);  // OK: 3×3 × 3×3 = 3×3
```

---

## Platform Considerations

### Memory Usage

**2D arrays use memory:**
- **Size** = (rows) × (columns) × (element size)
- **Example:** `array[0..9, 0..9] of integer` = 100 × 2 bytes = 200 bytes

**On ZealZ80 (512KB RAM):**
- **Be careful** with large 2D arrays
- **3×3 matrices** — Very efficient (18 bytes)
- **10×10 grids** — Acceptable (200 bytes)
- **Large arrays** — May not fit

### Performance

**2D array access:**
- **Fast** — Direct memory access
- **Cache-friendly** — Elements stored row by row
- **Matrix multiplication** — O(n³) for n×n matrices (can be optimized)

---

## Summary

**Key Concepts:**
- **2D arrays** organize data in rows and columns
- **Matrices** are 2D arrays used for mathematical operations
- **Matrix multiplication** combines matrices (not element-wise)
- **Row × Column** — Dot product of row and column
- **Polygons** can be represented as arrays of points
- **Applications** — Transformations, graphics, games, custom shapes

**Syntax:**
```pascal
var arr: array[rowRange, colRange] of elementType;
arr[row, col] := value;
```

**Matrix Operations:**
- **Addition** — Element-wise addition
- **Subtraction** — Element-wise subtraction
- **Scalar multiplication** — Multiply all elements by number
- **Matrix multiplication** — Row × Column dot products

**Applications:**
- **Game grids** — Boards, tilemaps
- **Transformations** — Rotation, scaling (with trigonometry later)
- **Graphics** — 2D and 3D transformations
- **Polygons** — Custom shapes, sprites, collision boundaries
- **Dynamic geometry** — Procedural shapes, animated objects

**Next:** Learn about records (structs) for structured data.

---

**Next Section:** [Records and Structs](./04_RecordsStructs.md)  
**Also See:** [Lists and Linked Lists](./05_ListsAndLinkedLists.md) for dynamic data structures  
**Language Specification:** See [03_TypeSystem.md](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

