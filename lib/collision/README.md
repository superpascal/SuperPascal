# Collision Detection Library

**Location:** `lib/collision/`  
**Status:** ✅ Complete  
**Modules:** 4 modules (mod, types, aabb, circle, polygon)

---

## Overview

Fast collision detection algorithms for game physics and interactions. All algorithms are **generic** and work on **all platforms** (8-bit through 64-bit).

**Source:** `algorithms/05_CollisionDetection.md`

---

## Module Structure

### `mod.pas` - Main Entry Point

Re-exports all collision detection functionality for convenient access.

**Usage:**
```pascal
uses Collision;  // Import everything
```

### `types.pas` - Core Types

Defines collision detection types and helper functions.

**Types:**
- `TAABB` - 2D Axis-Aligned Bounding Box
- `TCircle` - 2D Circle
- `TPoint2D` - 2D Point
- `TPolygon2D` - 2D Polygon (array of points)
- `TAABB3D` - 3D Axis-Aligned Bounding Box
- `TSphere` - 3D Sphere
- `TPoint3D` - 3D Point

**Helper Functions:**
- `CreateAABB(centerX, centerY, width, height)` - Create AABB from center and size
- `CreateAABBFromPoints(x1, y1, x2, y2)` - Create AABB from two points
- `CreateCircle(centerX, centerY, radius)` - Create circle
- `ExpandAABB(var box, x, y)` - Expand AABB to include point
- `ExpandAABBToInclude(var box, const other)` - Expand AABB to include another AABB

### `aabb.pas` - AABB Collision Detection

**2D Functions:**
- `AABBCollision(const a, b: TAABB): boolean` - Check if two AABBs overlap
- `PointInAABB(x, y: integer; const box: TAABB): boolean` - Check if point is inside AABB
- `AABBContains(const outer, inner: TAABB): boolean` - Check if AABB contains another
- `AABBIntersection(const a, b: TAABB; var intersection: TAABB): boolean` - Get intersection AABB
- `AABBUnion(const a, b: TAABB): TAABB` - Get union AABB (bounding box)
- `AABBCenter(const box: TAABB; var centerX, centerY: integer)` - Get AABB center
- `AABBSize(const box: TAABB; var width, height: integer)` - Get AABB size

**3D Functions:**
- `AABB3DCollision(const a, b: TAABB3D): boolean` - Check if two 3D AABBs overlap
- `PointInAABB3D(x, y, z: integer; const box: TAABB3D): boolean` - Check if point is inside 3D AABB

**Performance:** O(1) - constant time

**Best For:** Initial broad-phase collision detection

### `circle.pas` - Circle Collision Detection

**Functions:**
- `CircleCollision(const a, b: TCircle): boolean` - Check if two circles overlap
- `PointInCircle(x, y: integer; const circle: TCircle): boolean` - Check if point is inside circle
- `CircleContains(const outer, inner: TCircle): boolean` - Check if circle contains another
- `CircleDistanceSquared(const a, b: TCircle): LongInt` - Get squared distance between centers
- `CircleAABBCollision(const circle: TCircle; const box: TAABB): boolean` - Circle vs AABB
- `AABBCircleCollision(const box: TAABB; const circle: TCircle): boolean` - AABB vs Circle

**Performance:** O(1) - constant time

**Optimization:** Uses squared distances to avoid square root calculations

**Best For:** Circular objects (projectiles, particles, balls)

### `polygon.pas` - Polygon Collision Detection

**Functions:**
- `PointInPolygon(x, y: integer; const polygon: TPolygon2D): boolean` - Point-in-polygon test (ray casting)
- `ClipPolygonToScreen(...)` - Clip polygon to screen boundaries (Sutherland-Hodgman algorithm)
- `ClipEdgeToScreen(...)` - Clip edge to screen boundaries
- `PolygonAABBCollision(const polygon: TPolygon2D; const box: TAABB): boolean` - Polygon vs AABB
- `AABBPolygonCollision(const box: TAABB; const polygon: TPolygon2D): boolean` - AABB vs Polygon
- `PolygonCollision(const a, b: TPolygon2D): boolean` - Polygon vs Polygon (basic)
- `PolygonGetAABB(const polygon: TPolygon2D): TAABB` - Get AABB bounding box for polygon

**Performance:**
- Point-in-polygon: O(n) where n = number of vertices
- Polygon clipping: O(n × m) where n = vertices, m = clipping planes
- Polygon collision: O(n × m) where n, m = vertices of each polygon

**Best For:** Complex shapes, screen boundaries, view frustum culling

---

## Usage Examples

### AABB Collision

```pascal
uses Collision;

var
  box1, box2: TAABB;
  colliding: boolean;
begin
  // Create two AABBs
  box1 := CreateAABB(100, 100, 50, 50);  // Center at (100, 100), 50x50
  box2 := CreateAABB(120, 120, 30, 30);  // Center at (120, 120), 30x30
  
  // Check collision
  colliding := AABBCollision(box1, box2);
  
  if colliding then
    WriteLn('Boxes are colliding!');
end;
```

### Circle Collision

```pascal
uses Collision;

var
  circle1, circle2: TCircle;
  colliding: boolean;
begin
  // Create two circles
  circle1 := CreateCircle(100, 100, 25);  // Center at (100, 100), radius 25
  circle2 := CreateCircle(120, 120, 15);  // Center at (120, 120), radius 15
  
  // Check collision
  colliding := CircleCollision(circle1, circle2);
  
  if colliding then
    WriteLn('Circles are colliding!');
end;
```

### Polygon Clipping

```pascal
uses Collision;

var
  inputPoly, outputPoly: TPolygon2D;
  vertexCount: integer;
begin
  // Initialize input polygon (triangle)
  SetLength(inputPoly.Points, 3);
  inputPoly.Count := 3;
  inputPoly.Points[0] := (X: 50; Y: 50);
  inputPoly.Points[1] := (X: 150; Y: 50);
  inputPoly.Points[2] := (X: 100; Y: 150);
  
  // Clip to screen (320x200)
  SetLength(outputPoly.Points, 10);  // Allocate space for clipped polygon
  vertexCount := ClipPolygonToScreen(
    inputPoly, outputPoly,
    0, 0, 319, 199  // Screen boundaries
  );
  
  if vertexCount > 0 then
    WriteLn('Polygon has ', vertexCount, ' vertices after clipping');
end;
```

### Point-in-Polygon Test

```pascal
uses Collision;

var
  polygon: TPolygon2D;
  inside: boolean;
begin
  // Initialize polygon (square)
  SetLength(polygon.Points, 4);
  polygon.Count := 4;
  polygon.Points[0] := (X: 50; Y: 50);
  polygon.Points[1] := (X: 150; Y: 50);
  polygon.Points[2] := (X: 150; Y: 150);
  polygon.Points[3] := (X: 50; Y: 150);
  
  // Check if point is inside
  inside := PointInPolygon(100, 100, polygon);
  
  if inside then
    WriteLn('Point is inside polygon!');
end;
```

---

## Performance Characteristics

| Algorithm | Time Complexity | Best For |
|-----------|----------------|----------|
| AABB Collision | O(1) | Broad-phase collision detection |
| Circle Collision | O(1) | Circular objects |
| Point-in-Polygon | O(n) | Point queries |
| Polygon Clipping | O(n × m) | Screen boundaries |
| Polygon Collision | O(n × m) | Complex shapes |

**Optimization Tips:**
1. Use AABB for initial broad-phase (very fast)
2. Use circle collision for circular objects (fast, no sqrt)
3. Use polygon collision only when necessary (slower)
4. Consider spatial partitioning for many objects

---

## Platform Compatibility

✅ **All Platforms:** All algorithms use integer arithmetic only
- ✅ ZealZ80 (Z80)
- ✅ CommanderX16 (65C02)
- ✅ Foenix65C816 (65C816)
- ✅ FoenixA2560M (MC68LC060)
- ✅ RaspberryPi5 (ARM64)

**No Floating-Point:** All algorithms use integer-only arithmetic

---

## Dependencies

- `Math_Types` - For vector types (if needed)

---

## Algorithm Sources

- **AABB Collision:** Standard algorithm
- **Circle Collision:** Distance-based with squared distance optimization
- **Polygon Clipping:** Sutherland-Hodgman algorithm
- **Point-in-Polygon:** Ray casting algorithm

**See:** `algorithms/05_CollisionDetection.md` for detailed algorithm descriptions

---

**Last Updated:** 2025-01-XX  
**Status:** ✅ Complete (4 modules: types, aabb, circle, polygon)

