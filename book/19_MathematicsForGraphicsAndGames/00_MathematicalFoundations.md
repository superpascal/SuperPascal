# Mathematical Foundations

**Part of:** [Chapter 21: Mathematics for Graphics and Games](./README.md)

---

## Introduction

Before diving into advanced mathematics for game development, you need a solid foundation in basic mathematical concepts. This section reviews essential mathematical knowledge that you'll use throughout game programming.

**Target Levels:**
- **GCSE:** Basic arithmetic, simple algebra, coordinate systems
- **A-Level:** Algebra, geometry, coordinate systems, basic functions
- **University:** Review of fundamentals, preparation for advanced topics

---

## Arithmetic Fundamentals

### Basic Operations

**Four basic operations:**
- **Addition** (`+`) — Combining values
- **Subtraction** (`-`) — Taking away values
- **Multiplication** (`*`) — Repeated addition
- **Division** (`/`) — Splitting into parts

**In SuperPascal:**
```pascal
var sum, diff, product, quotient: integer;
sum := 5 + 3;        // 8
diff := 10 - 4;      // 6
product := 6 * 7;    // 42
quotient := 20 div 5; // 4 (integer division)
```

### Order of Operations

**PEMDAS (Parentheses, Exponents, Multiplication, Division, Addition, Subtraction):**

```pascal
var result: integer;
result := 2 + 3 * 4;      // 14 (not 20)
result := (2 + 3) * 4;    // 20
result := 10 - 4 / 2;     // 8 (not 3)
```

### Modulo Operation

**Modulo (`mod`) gives remainder:**
```pascal
var remainder: integer;
remainder := 10 mod 3;  // 1 (10 ÷ 3 = 3 remainder 1)
remainder := 15 mod 5;  // 0 (15 ÷ 5 = 3 remainder 0)
remainder := 7 mod 2;   // 1 (odd number)
```

**Uses:**
- Checking if number is even/odd
- Wrapping values (e.g., screen coordinates)
- Creating repeating patterns

---

## Algebra Basics

### Variables

**Variables represent unknown or changing values:**
```pascal
var x, y: integer;
x := 5;
y := x + 3;  // y = 8
```

### Equations

**Equations express relationships:**
- `y = x + 3` — Linear relationship
- `y = x * 2` — Proportional relationship
- `y = x * x` — Quadratic relationship

**In code:**
```pascal
function Linear(x: integer): integer;
begin
  Result := x + 3;
end;

function Proportional(x: integer): integer;
begin
  Result := x * 2;
end;

function Quadratic(x: integer): integer;
begin
  Result := x * x;
end;
```

### Functions

**Functions map inputs to outputs:**
- Input: `x`
- Function: `f(x) = x + 3`
- Output: `y`

**In SuperPascal:**
```pascal
function AddThree(x: integer): integer;
begin
  Result := x + 3;
end;

var result: integer;
result := AddThree(5);  // result = 8
```

---

## Coordinate Systems

### 2D Coordinates

**2D coordinates use (x, y):**
- **X-axis** — Horizontal (left-right)
- **Y-axis** — Vertical (up-down)
- **Origin** — (0, 0) at top-left (screen) or center (math)

**Screen coordinates (top-left origin):**
```
(0,0) ────────────────> X
  │
  │
  │
  │
  ↓
  Y
```

**Mathematical coordinates (center origin):**
```
      Y
      ↑
      │
      │
──────┼──────> X
      │
      │
```

### Distance Formula

**Distance between two points:**
```
distance = √((x₂ - x₁)² + (y₂ - y₁)²)
```

**In SuperPascal:**
```pascal
function Distance(x1, y1, x2, y2: integer): integer;
var
  dx, dy: integer;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  Result := Sqrt(dx * dx + dy * dy);
end;
```

### Midpoint Formula

**Midpoint between two points:**
```
midpoint_x = (x₁ + x₂) / 2
midpoint_y = (y₁ + y₂) / 2
```

**In SuperPascal:**
```pascal
procedure Midpoint(x1, y1, x2, y2: integer; var mx, my: integer);
begin
  mx := (x1 + x2) div 2;
  my := (y1 + y2) div 2;
end;
```

---

## Geometry Basics

### Angles

**Angles measure rotation:**
- **Degrees** — 0° to 360° (full circle)
- **Radians** — 0 to 2π (full circle)
- **Conversion:** radians = degrees × π / 180

**Common angles:**
- 0° / 0 rad — Right (east)
- 90° / π/2 rad — Up (north)
- 180° / π rad — Left (west)
- 270° / 3π/2 rad — Down (south)

### Right Triangles

**Right triangle has 90° angle:**
- **Hypotenuse** — Longest side (opposite right angle)
- **Opposite** — Side opposite angle
- **Adjacent** — Side next to angle

**Pythagorean theorem:**
```
a² + b² = c²
```

**In SuperPascal:**
```pascal
function Hypotenuse(a, b: integer): integer;
begin
  Result := Sqrt(a * a + b * b);
end;
```

---

## Number Systems

### Integers

**Whole numbers:**
- Positive: 1, 2, 3, ...
- Negative: -1, -2, -3, ...
- Zero: 0

**In SuperPascal:**
```pascal
var x: integer;
x := 42;
x := -10;
x := 0;
```

### Absolute Value

**Distance from zero:**
- `|5| = 5`
- `|-5| = 5`
- `|0| = 0`

**In SuperPascal:**
```pascal
var absValue: integer;
absValue := Abs(5);   // 5
absValue := Abs(-5); // 5
```

### Minimum and Maximum

**Finding min/max:**
```pascal
var minVal, maxVal: integer;
minVal := Min(5, 10);  // 5
maxVal := Max(5, 10);  // 10
```

---

## Basic Functions

### Linear Functions

**Linear: `y = mx + b`**
- `m` — Slope (rate of change)
- `b` — Y-intercept (starting value)

**In SuperPascal:**
```pascal
function Linear(m, x, b: integer): integer;
begin
  Result := m * x + b;
end;
```

### Quadratic Functions

**Quadratic: `y = ax² + bx + c`**
- Creates curves (parabolas)
- Used for acceleration, easing

**In SuperPascal:**
```pascal
function Quadratic(a, b, c, x: integer): integer;
begin
  Result := a * x * x + b * x + c;
end;
```

---

## Mathematical Constants

### Pi (π)

**Pi = 3.14159...**
- Ratio of circle's circumference to diameter
- Used for circles, angles, rotations

**In SuperPascal:**
```pascal
var circumference: integer;
circumference := 2 * PI * radius;
```

### E (Euler's Number)

**E = 2.71828...**
- Base of natural logarithm
- Used in exponential functions

### Tau (τ)

**Tau = 2π**
- Full circle in radians
- Sometimes more intuitive than 2π

---

## Practical Applications

### Game Coordinates

**Screen position:**
```pascal
type
  TPoint = record
    X, Y: integer;
  end;

var playerPos: TPoint;
playerPos.X := 100;
playerPos.Y := 200;
```

### Distance Calculations

**Check if in range:**
```pascal
function InRange(x1, y1, x2, y2, range: integer): boolean;
var
  dist: integer;
begin
  dist := Distance(x1, y1, x2, y2);
  Result := dist <= range;
end;
```

### Wrapping Values

**Wrap screen coordinates:**
```pascal
function Wrap(value, min, max: integer): integer;
begin
  if value < min then
    Result := max
  else if value > max then
    Result := min
  else
    Result := value;
end;

// Or using modulo:
function WrapMod(value, max: integer): integer;
begin
  Result := ((value mod max) + max) mod max;
end;
```

---

## Exercises

### Exercise 1: Basic Arithmetic

Write functions for:
1. Calculate area of rectangle (width × height)
2. Calculate perimeter of rectangle (2 × width + 2 × height)
3. Convert Celsius to Fahrenheit (F = C × 9/5 + 32)

### Exercise 2: Coordinate Systems

Write functions for:
1. Distance between two points
2. Midpoint between two points
3. Check if point is inside rectangle

### Exercise 3: Geometry

Write functions for:
1. Area of circle (π × r²)
2. Circumference of circle (2 × π × r)
3. Check if point is inside circle

---

**Next Section:** [Fixed-Point Math](./01_FixedPointMath.md)  
**Last Updated:** 2025-01-XX

