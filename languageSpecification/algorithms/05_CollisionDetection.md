# Collision Detection

**Part of:** [Algorithms Appendix](../99_Algorithms_Appendix.md)

---

## Overview

Collision detection algorithms for game physics and interactions. These algorithms are **generic** and work on **all platforms**.

**Source Material:** Mikro Documentation Archive

**See Also:**
- Book Chapter: [Chapter 18: Physics and Movement](../../book/18_PhysicsAndMovement/README.md)
- Game Engine: [Game Engine Concepts](../09_GameEngine_Concepts.md)

---

## AABB (Axis-Aligned Bounding Box)

### AABB Collision Detection

**Algorithm:** Check if two axis-aligned boxes overlap

**Key Features:**
- Very fast (O(1))
- Simple to implement
- Good for initial broad-phase collision detection

**Algorithm:**
```pascal
type
  TAABB = record
    MinX, MinY: integer;
    MaxX, MaxY: integer;
  end;

function AABBCollision(const a, b: TAABB): boolean;
begin
  Result := (a.MinX < b.MaxX) and (a.MaxX > b.MinX) and
            (a.MinY < b.MaxY) and (a.MaxY > b.MinY);
end;
```

**3D AABB:**
```pascal
type
  TAABB3D = record
    MinX, MinY, MinZ: integer;
    MaxX, MaxY, MaxZ: integer;
  end;

function AABB3DCollision(const a, b: TAABB3D): boolean;
begin
  Result := (a.MinX < b.MaxX) and (a.MaxX > b.MinX) and
            (a.MinY < b.MaxY) and (a.MaxY > b.MinY) and
            (a.MinZ < b.MaxZ) and (a.MaxZ > b.MinZ);
end;
```

### AABB vs Point

```pascal
function PointInAABB(x, y: integer; const box: TAABB): boolean;
begin
  Result := (x >= box.MinX) and (x <= box.MaxX) and
            (y >= box.MinY) and (y <= box.MaxY);
end;
```

---

## Circle Collision

### Circle-Circle Collision

**Algorithm:** Check if distance between centers < sum of radii

```pascal
type
  TCircle = record
    X, Y: integer;
    Radius: integer;
  end;

function CircleCollision(const a, b: TCircle): boolean;
var
  dx, dy: integer;
  distanceSquared: LongInt;
  radiusSum: integer;
begin
  dx := a.X - b.X;
  dy := a.Y - b.Y;
  distanceSquared := dx * dx + dy * dy;
  radiusSum := a.Radius + b.Radius;
  Result := distanceSquared < (radiusSum * radiusSum);
end;
```

**Optimization:** Compare squared distances to avoid square root

### Circle-AABB Collision

```pascal
function CircleAABBCollision(const circle: TCircle; const box: TAABB): boolean;
var
  closestX, closestY: integer;
  dx, dy: integer;
  distanceSquared: LongInt;
begin
  // Find closest point on AABB to circle center
  closestX := circle.X;
  if closestX < box.MinX then
    closestX := box.MinX
  else if closestX > box.MaxX then
    closestX := box.MaxX;
  
  closestY := circle.Y;
  if closestY < box.MinY then
    closestY := box.MinY
  else if closestY > box.MaxY then
    closestY := box.MaxY;
  
  // Check if closest point is within circle
  dx := circle.X - closestX;
  dy := circle.Y - closestY;
  distanceSquared := dx * dx + dy * dy;
  Result := distanceSquared < (circle.Radius * circle.Radius);
end;
```

---

## Polygon Clipping

**Source:** `docs/mikro_docs_archive/Coding/1/PLYGCLIP.TXT`, `Coding/2/POLYCLIP.TXT`

### Scanline Clipping

**Algorithm:** Clip polygon edges to screen boundaries during scanline rendering

**Key Concept:** Clip edges to top/bottom/left/right boundaries before drawing

```pascal
procedure ClipEdgeToScreen(
  x1, y1, x2, y2: integer;
  var clippedX1, clippedY1, clippedX2, clippedY2: integer;
  var visible: boolean
);
const
  SCREEN_LEFT = 0;
  SCREEN_RIGHT = 319;
  SCREEN_TOP = 0;
  SCREEN_BOTTOM = 199;
begin
  visible := true;
  clippedX1 := x1;
  clippedY1 := y1;
  clippedX2 := x2;
  clippedY2 := y2;
  
  // Clip to top
  if (clippedY1 < SCREEN_TOP) and (clippedY2 < SCREEN_TOP) then
  begin
    visible := false;
    exit;
  end;
  if clippedY1 < SCREEN_TOP then
  begin
    // Intersect with top edge
    clippedX1 := clippedX1 + ((SCREEN_TOP - clippedY1) * (clippedX2 - clippedX1)) div (clippedY2 - clippedY1);
    clippedY1 := SCREEN_TOP;
  end;
  if clippedY2 < SCREEN_TOP then
  begin
    clippedX2 := clippedX2 + ((SCREEN_TOP - clippedY2) * (clippedX1 - clippedX2)) div (clippedY1 - clippedY2);
    clippedY2 := SCREEN_TOP;
  end;
  
  // Clip to bottom
  if (clippedY1 > SCREEN_BOTTOM) and (clippedY2 > SCREEN_BOTTOM) then
  begin
    visible := false;
    exit;
  end;
  if clippedY1 > SCREEN_BOTTOM then
  begin
    clippedX1 := clippedX1 + ((SCREEN_BOTTOM - clippedY1) * (clippedX2 - clippedX1)) div (clippedY2 - clippedY1);
    clippedY1 := SCREEN_BOTTOM;
  end;
  if clippedY2 > SCREEN_BOTTOM then
  begin
    clippedX2 := clippedX2 + ((SCREEN_BOTTOM - clippedY2) * (clippedX1 - clippedX2)) div (clippedY1 - clippedY2);
    clippedY2 := SCREEN_BOTTOM;
  end;
  
  // Clip to left
  if (clippedX1 < SCREEN_LEFT) and (clippedX2 < SCREEN_LEFT) then
  begin
    visible := false;
    exit;
  end;
  if clippedX1 < SCREEN_LEFT then
  begin
    clippedY1 := clippedY1 + ((SCREEN_LEFT - clippedX1) * (clippedY2 - clippedY1)) div (clippedX2 - clippedX1);
    clippedX1 := SCREEN_LEFT;
  end;
  if clippedX2 < SCREEN_LEFT then
  begin
    clippedY2 := clippedY2 + ((SCREEN_LEFT - clippedX2) * (clippedY1 - clippedY2)) div (clippedX1 - clippedX2);
    clippedX2 := SCREEN_LEFT;
  end;
  
  // Clip to right
  if (clippedX1 > SCREEN_RIGHT) and (clippedX2 > SCREEN_RIGHT) then
  begin
    visible := false;
    exit;
  end;
  if clippedX1 > SCREEN_RIGHT then
  begin
    clippedY1 := clippedY1 + ((SCREEN_RIGHT - clippedX1) * (clippedY2 - clippedY1)) div (clippedX2 - clippedX1);
    clippedX1 := SCREEN_RIGHT;
  end;
  if clippedX2 > SCREEN_RIGHT then
  begin
    clippedY2 := clippedY2 + ((SCREEN_RIGHT - clippedX2) * (clippedY1 - clippedY2)) div (clippedX1 - clippedX2);
    clippedX2 := SCREEN_RIGHT;
  end;
end;
```

### Arithmetic Clipping (Sutherland-Hodgman)

**Algorithm:** Clip polygon against each clipping plane, creating new vertices where edges cross planes

**Key Concept:** Process polygon against each clipping plane sequentially

```pascal
function ClipPolygonToScreen(
  const input: array of TVector2;
  var output: array of TVector2
): integer;
const
  SCREEN_LEFT = 0;
  SCREEN_RIGHT = 319;
  SCREEN_TOP = 0;
  SCREEN_BOTTOM = 199;
begin
  // Clip against each edge: left, right, top, bottom
  // For each clipping plane:
  //   - For each edge of polygon:
  //     - If both vertices inside: add second vertex
  //     - If first inside, second outside: add intersection
  //     - If first outside, second inside: add intersection and second vertex
  //     - If both outside: skip
  
  // Implementation omitted for brevity - see PLYGCLIP.TXT for details
  Result := 0;  // Number of output vertices
end;
```

---

## Spatial Partitioning

### Grid-Based Partitioning

**Algorithm:** Divide space into grid cells, check collisions only within same/adjacent cells

```pascal
const
  GRID_CELL_SIZE = 32;
  GRID_WIDTH = 10;
  GRID_HEIGHT = 10;

type
  TGridCell = record
    Objects: array of integer;  // Object IDs
    Count: integer;
  end;
  
  TSpatialGrid = array[0..GRID_HEIGHT - 1, 0..GRID_WIDTH - 1] of TGridCell;

function WorldToGrid(x, y: integer): TVector2;
begin
  Result.X := x div GRID_CELL_SIZE;
  Result.Y := y div GRID_CELL_SIZE;
end;

procedure CheckCollisionsInGrid(const grid: TSpatialGrid);
var
  cellX, cellY: integer;
  i, j: integer;
begin
  for cellY := 0 to GRID_HEIGHT - 1 do
  begin
    for cellX := 0 to GRID_WIDTH - 1 do
    begin
      // Check collisions within cell
      for i := 0 to grid[cellY, cellX].Count - 1 do
      begin
        for j := i + 1 to grid[cellY, cellX].Count - 1 do
        begin
          CheckCollision(
            grid[cellY, cellX].Objects[i],
            grid[cellY, cellX].Objects[j]
          );
        end;
      end;
      
      // Check collisions with adjacent cells
      // (implementation omitted for brevity)
    end;
  end;
end;
```

**Performance:** O(n) where n = objects per cell (much better than O(n²) brute force)

---

## Performance Notes

**AABB Collision:**
- **Time:** O(1) - constant time
- **Best for:** Initial broad-phase collision detection
- **Limitation:** Only works for axis-aligned boxes

**Circle Collision:**
- **Time:** O(1) - constant time
- **Optimization:** Use squared distances to avoid sqrt
- **Best for:** Circular objects (projectiles, particles)

**Polygon Clipping:**
- **Time:** O(vertices × clipping planes)
- **Best for:** Screen boundaries, view frustum
- **Optimization:** Early rejection if polygon fully outside

**Spatial Partitioning:**
- **Time:** O(n) where n = objects per cell
- **Best for:** Many objects in large space
- **Trade-off:** Memory vs. speed

---

**Previous:** [Graphics Algorithms](./04_GraphicsAlgorithms.md)  
**Next:** [Physics Simulation](./06_PhysicsSimulation.md)  
**Last Updated:** 2025-01-XX
