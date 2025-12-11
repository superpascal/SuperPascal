# Trigonometry and the Unit Circle

**Part of:** [Chapter 19: Mathematics for Graphics and Games](./README.md)

---

## Introduction

**Trigonometry** is the mathematics of triangles and circles. It's essential for:
- **Rotation** — Rotating sprites, objects, cameras
- **Movement** — Moving in any direction
- **Angles** — Calculating angles between objects
- **Waves** — Creating smooth animations
- **Graphics** — 2D and 3D transformations

**The unit circle** is the foundation of trigonometry. Understanding it properly unlocks all trigonometric functions and their relationships.

**Why most schools fail:**
- They teach formulas without understanding
- They skip the visual intuition
- They don't connect it to practical applications
- They rush through the unit circle

**This section fixes that** — We'll build understanding from the ground up, visually and intuitively.

---

## What is the Unit Circle?

### The Circle

**A unit circle** is a circle with:
- **Radius = 1** (unit length)
- **Center at origin** (0, 0)
- **All points** are distance 1 from center

**Visual representation:**
```
        y
        ↑
        |
   (0,1)|
        |
(-1,0)  |  (1,0)
--------+--------→ x
        |
   (0,-1)|
        |
```

**Key insight:** Every point on the unit circle represents an angle!

### Angles on the Circle

**An angle** measures rotation from the positive x-axis:
- **0° (0 radians)** — Points right (1, 0)
- **90° (π/2 radians)** — Points up (0, 1)
- **180° (π radians)** — Points left (-1, 0)
- **270° (3π/2 radians)** — Points down (0, -1)
- **360° (2π radians)** — Back to right (1, 0)

**Visual:**
```
        90° (π/2)
        ↑
        |
180°    |    0°
(π) ←---+---→ (0 or 2π)
        |
        ↓
      270° (3π/2)
```

---

## Radians vs Degrees

### Why Radians?

**Radians are natural** — They measure angles by arc length:
- **1 radian** = angle where arc length = radius
- **Full circle** = 2π radians (circumference = 2πr, r=1)
- **Half circle** = π radians
- **Quarter circle** = π/2 radians

### Conversion

**Degrees to radians:**
```pascal
function DegreesToRadians(degrees: Q8.8): Q8.8;
begin
  DegreesToRadians := degrees * PI / 180.0;
end;
```

**Radians to degrees:**
```pascal
function RadiansToDegrees(radians: Q8.8): Q8.8;
begin
  RadiansToDegrees := radians * 180.0 / PI;
end;
```

**Common angles:**
| Degrees | Radians | Description |
|---------|---------|-------------|
| 0° | 0 | Right |
| 30° | π/6 | |
| 45° | π/4 | |
| 60° | π/3 | |
| 90° | π/2 | Up |
| 180° | π | Left |
| 270° | 3π/2 | Down |
| 360° | 2π | Full circle |

**SuperPascal uses radians** — All trigonometric functions expect radians!

---

## The Unit Circle: Visual Understanding

### Coordinates on the Circle

**Every point on the unit circle** has coordinates (x, y):
- **x = cos(θ)** — Horizontal coordinate
- **y = sin(θ)** — Vertical coordinate

**Where θ (theta) is the angle** from positive x-axis.

**Visual:**
```
        (0,1)
        ↑
        |  /
        | /  ← Point at angle θ
        |/θ
--------+--------→ x
        |
        |
```

**The point is at:**
- **x = cos(θ)**
- **y = sin(θ)**

### Key Points to Memorize

**Memorize these key points** — They're the foundation:

| Angle | Radians | Point (x, y) | cos(θ) | sin(θ) |
|-------|---------|--------------|--------|--------|
| 0° | 0 | (1, 0) | 1 | 0 |
| 30° | π/6 | (√3/2, 1/2) | √3/2 ≈ 0.866 | 1/2 = 0.5 |
| 45° | π/4 | (√2/2, √2/2) | √2/2 ≈ 0.707 | √2/2 ≈ 0.707 |
| 60° | π/3 | (1/2, √3/2) | 1/2 = 0.5 | √3/2 ≈ 0.866 |
| 90° | π/2 | (0, 1) | 0 | 1 |
| 120° | 2π/3 | (-1/2, √3/2) | -1/2 = -0.5 | √3/2 ≈ 0.866 |
| 135° | 3π/4 | (-√2/2, √2/2) | -√2/2 ≈ -0.707 | √2/2 ≈ 0.707 |
| 150° | 5π/6 | (-√3/2, 1/2) | -√3/2 ≈ -0.866 | 1/2 = 0.5 |
| 180° | π | (-1, 0) | -1 | 0 |
| 210° | 7π/6 | (-√3/2, -1/2) | -√3/2 ≈ -0.866 | -1/2 = -0.5 |
| 225° | 5π/4 | (-√2/2, -√2/2) | -√2/2 ≈ -0.707 | -√2/2 ≈ -0.707 |
| 240° | 4π/3 | (-1/2, -√3/2) | -1/2 = -0.5 | -√3/2 ≈ -0.866 |
| 270° | 3π/2 | (0, -1) | 0 | -1 |
| 300° | 5π/3 | (1/2, -√3/2) | 1/2 = 0.5 | -√3/2 ≈ -0.866 |
| 315° | 7π/4 | (√2/2, -√2/2) | √2/2 ≈ 0.707 | -√2/2 ≈ -0.707 |
| 330° | 11π/6 | (√3/2, -1/2) | √3/2 ≈ 0.866 | -1/2 = -0.5 |
| 360° | 2π | (1, 0) | 1 | 0 |

**Pattern to remember:**
- **First quadrant (0-90°):** Both x and y positive
- **Second quadrant (90-180°):** x negative, y positive
- **Third quadrant (180-270°):** Both x and y negative
- **Fourth quadrant (270-360°):** x positive, y negative

---

## Understanding Sin and Cos

### Sin (Sine)

**Sin(θ) = y-coordinate** on the unit circle.

**What it means:**
- **Vertical distance** from x-axis
- **Height** of the point
- **Range:** -1 to 1

**Visual:**
```
        sin(θ)
        ↑
        |
        |  /
        | /  ← Point
        |/θ
--------+--------→
        |
```

**Properties:**
- **sin(0) = 0** — On x-axis
- **sin(π/2) = 1** — At top
- **sin(π) = 0** — On x-axis
- **sin(3π/2) = -1** — At bottom
- **sin(2π) = 0** — Back to start

### Cos (Cosine)

**Cos(θ) = x-coordinate** on the unit circle.

**What it means:**
- **Horizontal distance** from y-axis
- **Width** of the point
- **Range:** -1 to 1

**Visual:**
```
        |
        |
        |  /
        | /  ← Point
        |/θ
--------+--------→ cos(θ)
        |
```

**Properties:**
- **cos(0) = 1** — At right
- **cos(π/2) = 0** — On y-axis
- **cos(π) = -1** — At left
- **cos(3π/2) = 0** — On y-axis
- **cos(2π) = 1** — Back to start

### The Relationship

**Key insight:** For any angle θ:
- **x = cos(θ)**
- **y = sin(θ)**
- **x² + y² = 1** (Pythagorean theorem on unit circle)

**This is the fundamental relationship!**

---

## Tan (Tangent)

### What is Tangent?

**Tan(θ) = sin(θ) / cos(θ)** = y / x

**What it means:**
- **Slope** of the line from origin to point
- **Ratio** of vertical to horizontal
- **Range:** -∞ to +∞ (undefined when cos(θ) = 0)

**Visual:**
```
        |
        |  /
        | /  ← Line with slope tan(θ)
        |/θ
--------+--------→
        |
```

**Properties:**
- **tan(0) = 0** — Horizontal line
- **tan(π/4) = 1** — 45° line (slope 1)
- **tan(π/2) = undefined** — Vertical line (cos = 0)
- **tan(π) = 0** — Horizontal line
- **tan(3π/2) = undefined** — Vertical line

### Why Tan Matters

**Tan gives us direction:**
- **Positive tan** — Angle in first or third quadrant
- **Negative tan** — Angle in second or fourth quadrant
- **Large tan** — Steep angle (close to vertical)
- **Small tan** — Shallow angle (close to horizontal)

---

## The Unit Circle: Complete Picture

### Visualizing All Functions

**For any angle θ on the unit circle:**

```
        y
        ↑
        |
        |  /|
        | / |  ← Point at (cos(θ), sin(θ))
        |/θ |
--------+---+--------→ x
        |   |
        |   |
```

**Functions:**
- **cos(θ)** = x-coordinate (horizontal)
- **sin(θ)** = y-coordinate (vertical)
- **tan(θ)** = sin(θ) / cos(θ) = slope

**Distance from origin:**
- Always 1 (unit circle)
- **√(cos²(θ) + sin²(θ)) = 1** (Pythagorean identity)

### Quadrant Signs

**Remember the signs in each quadrant:**

```
        II      I
        - +    + +
        |      |
--------+--------→
        |      |
        - -    + -
       III     IV
```

**Quadrant I (0-90°):** cos > 0, sin > 0  
**Quadrant II (90-180°):** cos < 0, sin > 0  
**Quadrant III (180-270°):** cos < 0, sin < 0  
**Quadrant IV (270-360°):** cos > 0, sin < 0

---

## Inverse Functions

### ArcSin, ArcCos, ArcTan

**Inverse functions** find the angle from a value:

**ArcSin(y) = θ** where sin(θ) = y  
**ArcCos(x) = θ** where cos(θ) = x  
**ArcTan(slope) = θ** where tan(θ) = slope

**Important:** Inverse functions return angles in specific ranges:
- **ArcSin:** -π/2 to π/2 (quadrants I and IV)
- **ArcCos:** 0 to π (quadrants I and II)
- **ArcTan:** -π/2 to π/2 (quadrants I and IV)

### ArcTan2 (The Important One!)

**ArcTan2(y, x)** finds angle from point (x, y):
- **Returns angle in full range** -π to π
- **Handles all quadrants** correctly
- **Most useful** for game development!

**Example:**
```pascal
var x, y: Q8.8;
var angle: Q8.8;
begin
  x := 1.0;
  y := 1.0;
  angle := ArcTan2(y, x);  // 45° (π/4)
  
  x := -1.0;
  y := 1.0;
  angle := ArcTan2(y, x);  // 135° (3π/4) - correct quadrant!
end.
```

**Why ArcTan2?**
- **ArcTan(y/x)** doesn't work for x = 0 (division by zero)
- **ArcTan(y/x)** can't tell quadrant (1,1) vs (-1,-1) both give same result
- **ArcTan2(y, x)** handles all cases correctly

---

## Practical Applications in Games

### 1. Rotating a Sprite

**Rotate sprite around center:**
```pascal
procedure RotateSprite(centerX, centerY: integer;
                      angle: Q8.8;
                      var spriteX, spriteY: integer);
var cosA, sinA: Q8.8;
var dx, dy: integer;
var newX, newY: Q8.8;
begin
  cosA := Cos(angle);
  sinA := Sin(angle);
  
  // Offset from center
  dx := spriteX - centerX;
  dy := spriteY - centerY;
  
  // Rotate offset
  newX := Q8.8(dx) * cosA - Q8.8(dy) * sinA;
  newY := Q8.8(dx) * sinA + Q8.8(dy) * cosA;
  
  // New position
  spriteX := centerX + integer(newX);
  spriteY := centerY + integer(newY);
end;
```

### 2. Moving in a Direction

**Move object at angle with speed:**
```pascal
procedure MoveAtAngle(var x, y: integer;
                     angle: Q8.8;
                     speed: Q8.8);
var dx, dy: Q8.8;
begin
  dx := speed * Cos(angle);
  dy := speed * Sin(angle);
  
  x := x + integer(dx);
  y := y + integer(dy);
end;
```

**Example:**
```pascal
var playerX, playerY: integer;
var angle: Q8.8;
begin
  playerX := 100;
  playerY := 100;
  angle := PI / 4;  // 45 degrees
  
  MoveAtAngle(playerX, playerY, angle, 2.0);
  // Player moves diagonally up-right
end.
```

### 3. Finding Angle to Target

**Calculate angle from object to target:**
```pascal
function AngleToTarget(fromX, fromY, toX, toY: integer): Q8.8;
var dx, dy: Q8.8;
begin
  dx := Q8.8(toX - fromX);
  dy := Q8.8(toY - fromY);
  
  AngleToTarget := ArcTan2(dy, dx);
end;
```

**Example:**
```pascal
var playerX, playerY: integer;
var enemyX, enemyY: integer;
var angle: Q8.8;
begin
  playerX := 100;
  playerY := 100;
  enemyX := 200;
  enemyY := 150;
  
  angle := AngleToTarget(playerX, playerY, enemyX, enemyY);
  // Angle points from player toward enemy
end.
```

### 4. Circular Motion

**Move object in circle:**
```pascal
procedure UpdateCircularMotion(var x, y: integer;
                               centerX, centerY: integer;
                               radius: Q8.8;
                               var angle: Q8.8;
                               angularSpeed: Q8.8);
begin
  // Update angle
  angle := angle + angularSpeed;
  if angle >= TAU then
    angle := angle - TAU;  // Wrap around
  
  // Calculate position on circle
  x := centerX + integer(radius * Cos(angle));
  y := centerY + integer(radius * Sin(angle));
end;
```

**Example: Orbiting object:**
```pascal
var orbitX, orbitY: integer;
var angle: Q8.8;
begin
  angle := 0.0;
  
  while true do
  begin
    UpdateCircularMotion(orbitX, orbitY, 160, 120, 50.0, angle, 0.1);
    // Object orbits around center (160, 120) with radius 50
    WaitVBlank;
  end;
end.
```

### 5. Smooth Rotation

**Rotate smoothly toward target angle:**
```pascal
function RotateToward(currentAngle, targetAngle: Q8.8;
                     maxRotation: Q8.8): Q8.8;
var diff: Q8.8;
begin
  diff := targetAngle - currentAngle;
  
  // Normalize to -π to π
  while diff > PI do
    diff := diff - TAU;
  while diff < -PI do
    diff := diff + TAU;
  
  // Clamp rotation speed
  if diff > maxRotation then
    diff := maxRotation
  else if diff < -maxRotation then
    diff := -maxRotation;
  
  RotateToward := currentAngle + diff;
  
  // Normalize result
  if RotateToward >= TAU then
    RotateToward := RotateToward - TAU
  else if RotateToward < 0.0 then
    RotateToward := RotateToward + TAU;
end;
```

### 6. Distance and Direction

**Calculate distance and angle between points:**
```pascal
procedure DistanceAndAngle(x1, y1, x2, y2: integer;
                           var distance: Q8.8;
                           var angle: Q8.8);
var dx, dy: Q8.8;
begin
  dx := Q8.8(x2 - x1);
  dy := Q8.8(y2 - y1);
  
  distance := Sqrt(dx * dx + dy * dy);
  angle := ArcTan2(dy, dx);
end;
```

---

## Common Patterns

### Pattern 1: Direction Vector from Angle

**Get direction vector (unit length) from angle:**
```pascal
procedure AngleToDirection(angle: Q8.8;
                          var dirX, dirY: Q8.8);
begin
  dirX := Cos(angle);
  dirY := Sin(angle);
end;
```

**Use for normalized movement:**
```pascal
var dirX, dirY: Q8.8;
var speed: Q8.8;
begin
  AngleToDirection(PI / 4, dirX, dirY);  // 45 degrees
  speed := 2.0;
  
  // Move in direction
  x := x + integer(speed * dirX);
  y := y + integer(speed * dirY);
end.
```

### Pattern 2: Angle from Direction Vector

**Get angle from direction vector:**
```pascal
function DirectionToAngle(dirX, dirY: Q8.8): Q8.8;
begin
  DirectionToAngle := ArcTan2(dirY, dirX);
end;
```

### Pattern 3: Rotate Vector

**Rotate a vector by angle:**
```pascal
procedure RotateVector(var vx, vy: Q8.8; angle: Q8.8);
var cosA, sinA: Q8.8;
var newX, newY: Q8.8;
begin
  cosA := Cos(angle);
  sinA := Sin(angle);
  
  newX := vx * cosA - vy * sinA;
  newY := vx * sinA + vy * cosA;
  
  vx := newX;
  vy := newY;
end;
```

---

## Best Practices

### 1. Always Use Radians

**SuperPascal functions expect radians:**
```pascal
// Good: Use radians
var angle: Q8.8;
angle := PI / 4;  // 45 degrees
var x: Q8.8;
x := Cos(angle);

// Bad: Don't use degrees directly
// x := Cos(45);  // Wrong! 45 radians, not 45 degrees
```

### 2. Normalize Angles

**Keep angles in 0 to 2π range:**
```pascal
function NormalizeAngle(angle: Q8.8): Q8.8;
begin
  while angle >= TAU do
    angle := angle - TAU;
  while angle < 0.0 do
    angle := angle + TAU;
  NormalizeAngle := angle;
end;
```

### 3. Use ArcTan2 for Angles

**Always use ArcTan2, not ArcTan:**
```pascal
// Good: Handles all quadrants
angle := ArcTan2(dy, dx);

// Bad: Doesn't handle quadrants correctly
// angle := ArcTan(dy / dx);  // Fails if dx = 0
```

### 4. Cache Expensive Operations

**Cache sin and cos if used multiple times:**
```pascal
// Bad: Calculate multiple times
x := radius * Cos(angle);
y := radius * Sin(angle);
x2 := radius2 * Cos(angle);  // Recalculating!

// Good: Calculate once
var cosA, sinA: Q8.8;
cosA := Cos(angle);
sinA := Sin(angle);
x := radius * cosA;
y := radius * sinA;
x2 := radius2 * cosA;  // Reuse cached value
```

### 5. Understand Quadrants

**Remember quadrant signs:**
- **Quadrant I:** cos > 0, sin > 0
- **Quadrant II:** cos < 0, sin > 0
- **Quadrant III:** cos < 0, sin < 0
- **Quadrant IV:** cos > 0, sin < 0

---

## Platform Considerations

### Fixed-Point Precision

**Trigonometric functions use fixed-point:**
- **Q8.8** — Good for most game applications
- **Q12.12** — Better for high precision (complex calculations)
- **Precision** — Affects accuracy of angles and rotations

### Performance

**Trigonometric functions:**
- **Moderate cost** — More expensive than basic arithmetic
- **Cache results** — If using same angle multiple times
- **On ZealZ80** — Consider lookup tables for common angles
- **Hardware acceleration** — May be available on some platforms

### Lookup Tables

**For performance-critical code, use lookup tables:**
```pascal
const
  SIN_TABLE_SIZE = 256;
  SIN_TABLE: array[0..SIN_TABLE_SIZE-1] of Q8.8 = (
    // Precomputed sin values
    // ... (generated at compile time)
  );

function FastSin(angle: Q8.8): Q8.8;
var index: integer;
begin
  index := integer((angle / TAU) * SIN_TABLE_SIZE) mod SIN_TABLE_SIZE;
  FastSin := SIN_TABLE[index];
end;
```

---

## Summary

**Key Concepts:**
- **Unit circle** — Circle with radius 1, center at origin
- **Radians** — Natural angle measure (2π = full circle)
- **Sin(θ)** — y-coordinate on unit circle
- **Cos(θ)** — x-coordinate on unit circle
- **Tan(θ)** — sin(θ) / cos(θ) = slope
- **ArcTan2** — Find angle from point (handles all quadrants)

**Unit Circle Facts:**
- **x = cos(θ), y = sin(θ)** — Fundamental relationship
- **x² + y² = 1** — Always true (Pythagorean identity)
- **Quadrant signs** — Remember: I(+,+), II(-,+), III(-,-), IV(+,-)

**Common Angles:**
- **0° (0)** — (1, 0) — Right
- **90° (π/2)** — (0, 1) — Up
- **180° (π)** — (-1, 0) — Left
- **270° (3π/2)** — (0, -1) — Down

**Applications:**
- Rotating sprites
- Moving in directions
- Finding angles to targets
- Circular motion
- Smooth rotation
- Direction vectors

**Best Practices:**
- Always use radians
- Normalize angles (0 to 2π)
- Use ArcTan2 for angles
- Cache sin/cos if used multiple times
- Understand quadrants

**Next:** Learn about waves and periodic motion (applications of trigonometry).

---

**Next Section:** [Waves and Periodic Motion](./04_WavesAndPeriodicMotion.md)  
**Language Specification:** See [13_StandardLibrary.md](../../languageSpecification/13_StandardLibrary.md)  
**Last Updated:** 2025-01-XX

