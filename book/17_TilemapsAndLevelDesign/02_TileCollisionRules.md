# Tile Collision Rules

**Part of:** [Chapter 17: Tilemaps and Level Design](./README.md)

---

## Introduction

Collision detection with tiles is essential for platformers and tile-based games. This section teaches you how to detect collisions between entities and tiles, handle different collision types, and respond appropriately.

**Key concepts:**
- **Tile collision detection** — Checking if entity collides with tile
- **Collision types** — Solid, one-way, trigger, etc.
- **Collision response** — What happens when collision occurs
- **Tile properties** — Flags for collision behavior
- **Efficient collision** — Fast collision checking

---

## Tile Collision Basics

### What is Tile Collision?

**Tile collision detects when an entity overlaps with a tile:**

```
Entity:     Tilemap:
[Player]    [Wall][Wall][Wall]
  at        [Wall][Floor][Wall]
(100, 50)   [Wall][Wall][Wall]

Check: Does player overlap with wall tile?
```

**Common uses:**
- **Platformers** — Player stands on platforms, hits walls
- **RPGs** — Characters can't walk through walls
- **Puzzle games** — Blocks can't move through obstacles

### Tile Properties

**Tiles can have collision properties:**

```pascal
type
  TTileProperty = (
    tpSolid,      // Solid collision (can't pass through)
    tpOneWay,     // One-way platform (can pass from below)
    tpTrigger,    // Trigger tile (activates something)
    tpLadder,     // Ladder tile (can climb)
    tpWater,      // Water tile (swimming)
    tpHazard      // Hazard tile (damages player)
  );
  
  TTileProperties = set of TTileProperty;

const
  TILE_WALL = 1;
  TILE_FLOOR = 0;
  TILE_PLATFORM = 2;
  TILE_HAZARD = 3;
  
  // Tile properties
  TileProperties: array[0..255] of TTileProperties = (
    TILE_FLOOR:   [];              // No collision
    TILE_WALL:    [tpSolid];       // Solid
    TILE_PLATFORM: [tpOneWay];     // One-way platform
    TILE_HAZARD:  [tpSolid, tpHazard];  // Solid and hazardous
    // ... etc
  );
```

---

## Collision Detection

### Point vs Tile

**Check if a point is inside a solid tile:**

```pascal
function PointCollidesWithTile(worldX, worldY: integer): boolean;
var
  tileX, tileY: word;
  tile: byte;
begin
  // Convert world coordinates to tile coordinates
  tileX := worldX div TILE_SIZE;
  tileY := worldY div TILE_SIZE;
  
  // Get tile at position
  tile := GetTile(level1, tileX, tileY);
  
  // Check if tile is solid
  PointCollidesWithTile := tpSolid in TileProperties[tile];
end;
```

### AABB vs Tile

**Check if entity AABB collides with tile:**

```pascal
function EntityCollidesWithTile(
  entityX, entityY, entityW, entityH: integer
): boolean;
var
  // Get tiles that entity overlaps
  minTileX, maxTileX: word;
  minTileY, maxTileY: word;
  tileX, tileY: word;
  tile: byte;
begin
  // Calculate tile range
  minTileX := entityX div TILE_SIZE;
  maxTileX := (entityX + entityW - 1) div TILE_SIZE;
  minTileY := entityY div TILE_SIZE;
  maxTileY := (entityY + entityH - 1) div TILE_SIZE;
  
  // Check all overlapping tiles
  for tileY := minTileY to maxTileY do
    for tileX := minTileX to maxTileX do
    begin
      tile := GetTile(level1, tileX, tileY);
      if tpSolid in TileProperties[tile] then
      begin
        EntityCollidesWithTile := true;
        Exit;
      end;
    end;
  
  EntityCollidesWithTile := false;
end;
```

### Using CollisionCheckTile

**SuperPascal provides an intrinsic for tile collision:**

```pascal
function CollisionCheckTile(
  id: TEntityID; 
  tileX, tileY: integer
): boolean;
```

**Usage:**
```pascal
var
  player: TEntityID;
  playerX, playerY: integer;
  tileX, tileY: word;
  
begin
  EntityGetPosition(player, playerX, playerY);
  tileX := playerX div TILE_SIZE;
  tileY := playerY div TILE_SIZE;
  
  if CollisionCheckTile(player, tileX, tileY) then
    WriteLn('Player collides with tile at ', tileX, ', ', tileY);
end;
```

---

## Collision Types

### Solid Collision

**Solid tiles block movement:**

```pascal
procedure CheckSolidCollision(entity: TEntityID);
var
  x, y, vx, vy: integer;
  newX, newY: integer;
  tileX, tileY: word;
begin
  EntityGetPosition(entity, x, y);
  EntityGetVelocity(entity, vx, vy);
  
  // Calculate new position
  newX := x + vx;
  newY := y + vy;
  
  // Check collision at new position
  tileX := newX div TILE_SIZE;
  tileY := newY div TILE_SIZE;
  var tile := GetTile(level1, tileX, tileY);
  
  if tpSolid in TileProperties[tile] then
  begin
    // Block movement
    EntitySetVelocity(entity, 0, 0);
  end
  else
  begin
    // Allow movement
    EntitySetPosition(entity, newX, newY);
  end;
end;
```

### One-Way Platform

**One-way platforms allow passing from below:**

```pascal
procedure CheckOneWayPlatform(entity: TEntityID);
var
  x, y, vx, vy: integer;
  newY: integer;
  tileX, tileY: word;
  tile: byte;
begin
  EntityGetPosition(entity, x, y);
  EntityGetVelocity(entity, vx, vy);
  
  newY := y + vy;
  
  // Check tile below entity
  tileX := x div TILE_SIZE;
  tileY := (newY + ENTITY_HEIGHT) div TILE_SIZE;
  tile := GetTile(level1, tileX, tileY);
  
  if (tpOneWay in TileProperties[tile]) and (vy > 0) then
  begin
    // Falling onto platform - allow
    EntitySetPosition(entity, x, newY);
  end
  else if (tpOneWay in TileProperties[tile]) and (vy < 0) then
  begin
    // Moving up through platform - block
    EntitySetVelocity(entity, vx, 0);
  end;
end;
```

### Trigger Tiles

**Trigger tiles activate events:**

```pascal
procedure CheckTriggerTiles(entity: TEntityID);
var
  x, y: integer;
  tileX, tileY: word;
  tile: byte;
begin
  EntityGetPosition(entity, x, y);
  tileX := x div TILE_SIZE;
  tileY := y div TILE_SIZE;
  tile := GetTile(level1, tileX, tileY);
  
  if tpTrigger in TileProperties[tile] then
  begin
    OnTriggerTile(tileX, tileY);
    // Could remove trigger after activation
    SetTile(level1, tileX, tileY, TILE_FLOOR);
  end;
end;
```

### Hazard Tiles

**Hazard tiles damage entities:**

```pascal
procedure CheckHazardTiles(entity: TEntityID);
var
  x, y: integer;
  tileX, tileY: word;
  tile: byte;
begin
  EntityGetPosition(entity, x, y);
  tileX := x div TILE_SIZE;
  tileY := y div TILE_SIZE;
  tile := GetTile(level1, tileX, tileY);
  
  if tpHazard in TileProperties[tile] then
  begin
    TakeDamage(entity, 1);
    // Could add invincibility frames, etc.
  end;
end;
```

---

## Collision Response

### Stop on Collision

**Stop entity when colliding:**

```pascal
procedure HandleCollision(entity: TEntityID);
var
  x, y, vx, vy: integer;
  newX, newY: integer;
begin
  EntityGetPosition(entity, x, y);
  EntityGetVelocity(entity, vx, vy);
  
  newX := x + vx;
  newY := y + vy;
  
  // Check X collision
  if EntityCollidesWithTile(newX, y, ENTITY_WIDTH, ENTITY_HEIGHT) then
    vx := 0;  // Stop horizontal movement
  
  // Check Y collision
  if EntityCollidesWithTile(x, newY, ENTITY_WIDTH, ENTITY_HEIGHT) then
    vy := 0;  // Stop vertical movement
  
  // Update position if no collision
  if (vx = 0) and (vy = 0) then
    // Already at valid position
  else
    EntitySetPosition(entity, x + vx, y + vy);
  
  EntitySetVelocity(entity, vx, vy);
end;
```

### Slide Along Walls

**Slide along walls instead of stopping:**

```pascal
procedure HandleSlidingCollision(entity: TEntityID);
var
  x, y, vx, vy: integer;
  newX, newY: integer;
begin
  EntityGetPosition(entity, x, y);
  EntityGetVelocity(entity, vx, vy);
  
  newX := x + vx;
  newY := y + vy;
  
  // Try X movement first
  if not EntityCollidesWithTile(newX, y, ENTITY_WIDTH, ENTITY_HEIGHT) then
    x := newX
  else
    vx := 0;  // Block X movement
  
  // Try Y movement
  if not EntityCollidesWithTile(x, newY, ENTITY_WIDTH, ENTITY_HEIGHT) then
    y := newY
  else
    vy := 0;  // Block Y movement
  
  EntitySetPosition(entity, x, y);
  EntitySetVelocity(entity, vx, vy);
end;
```

### Push Out of Tile

**Push entity out of colliding tile:**

```pascal
procedure PushOutOfTile(entity: TEntityID);
var
  x, y: integer;
  tileX, tileY: word;
  tileWorldX, tileWorldY: integer;
begin
  EntityGetPosition(entity, x, y);
  tileX := x div TILE_SIZE;
  tileY := y div TILE_SIZE;
  
  // Get tile world position
  tileWorldX := tileX * TILE_SIZE;
  tileWorldY := tileY * TILE_SIZE;
  
  // Push entity to nearest edge
  if x < tileWorldX + TILE_SIZE div 2 then
    x := tileWorldX - ENTITY_WIDTH  // Push left
  else
    x := tileWorldX + TILE_SIZE;   // Push right
  
  if y < tileWorldY + TILE_SIZE div 2 then
    y := tileWorldY - ENTITY_HEIGHT  // Push up
  else
    y := tileWorldY + TILE_SIZE;     // Push down
  
  EntitySetPosition(entity, x, y);
end;
```

---

## Efficient Collision Detection

### Check Only Relevant Tiles

**Only check tiles that entity overlaps:**

```pascal
procedure CheckCollisionEfficient(entity: TEntityID);
var
  x, y, w, h: integer;
  minTileX, maxTileX: word;
  minTileY, maxTileY: word;
  tileX, tileY: word;
begin
  EntityGetPosition(entity, x, y);
  // Assume entity has width w and height h
  
  // Calculate tile range
  minTileX := x div TILE_SIZE;
  maxTileX := (x + w - 1) div TILE_SIZE;
  minTileY := y div TILE_SIZE;
  maxTileY := (y + h - 1) div TILE_SIZE;
  
  // Only check overlapping tiles
  for tileY := minTileY to maxTileY do
    for tileX := minTileX to maxTileX do
    begin
      var tile := GetTile(level1, tileX, tileY);
      if tpSolid in TileProperties[tile] then
      begin
        HandleCollision(entity, tileX, tileY);
        Exit;  // Stop after first collision
      end;
    end;
end;
```

### Cache Tile Lookups

**Cache tile data to avoid repeated lookups:**

```pascal
var
  cachedTileX, cachedTileY: word;
  cachedTile: byte;
  
function GetCachedTile(x, y: integer): byte;
var
  tileX, tileY: word;
begin
  tileX := x div TILE_SIZE;
  tileY := y div TILE_SIZE;
  
  // Use cache if same tile
  if (tileX = cachedTileX) and (tileY = cachedTileY) then
    GetCachedTile := cachedTile
  else
  begin
    // Update cache
    cachedTileX := tileX;
    cachedTileY := tileY;
    cachedTile := GetTile(level1, tileX, tileY);
    GetCachedTile := cachedTile;
  end;
end;
```

---

## Complete Collision Example

**Putting it all together:**

```pascal
program TileCollisionDemo;

const
  TILE_SIZE = 8;
  ENTITY_WIDTH = 16;
  ENTITY_HEIGHT = 16;
  
  TILE_FLOOR = 0;
  TILE_WALL = 1;
  TILE_PLATFORM = 2;
  TILE_HAZARD = 3;

var
  player: TEntityID;
  level1: TTilemap;

procedure CheckTileCollision(entity: TEntityID);
var
  x, y, vx, vy: integer;
  newX, newY: integer;
  minTileX, maxTileX, minTileY, maxTileY: word;
  tileX, tileY: word;
  tile: byte;
begin
  EntityGetPosition(entity, x, y);
  EntityGetVelocity(entity, vx, vy);
  
  newX := x + vx;
  newY := y + vy;
  
  // Calculate tile range
  minTileX := newX div TILE_SIZE;
  maxTileX := (newX + ENTITY_WIDTH - 1) div TILE_SIZE;
  minTileY := newY div TILE_SIZE;
  maxTileY := (newY + ENTITY_HEIGHT - 1) div TILE_SIZE;
  
  // Check X collision
  var canMoveX := true;
  for tileY := minTileY to maxTileY do
  begin
    tile := GetTile(level1, minTileX, tileY);
    if tpSolid in TileProperties[tile] then
    begin
      canMoveX := false;
      vx := 0;
      Break;
    end;
  end;
  
  // Check Y collision
  var canMoveY := true;
  for tileX := minTileX to maxTileX do
  begin
    tile := GetTile(level1, tileX, minTileY);
    if tpSolid in TileProperties[tile] then
    begin
      canMoveY := false;
      vy := 0;
      Break;
    end;
  end;
  
  // Update position
  if canMoveX then x := newX;
  if canMoveY then y := newY;
  
  EntitySetPosition(entity, x, y);
  EntitySetVelocity(entity, vx, vy);
end;

begin
  player := EntityCreate;
  EntitySetPosition(player, 100, 50);
  
  while true do
  begin
    UpdateGame;
    CheckTileCollision(player);
    WaitVBlank;
    RenderGame;
  end;
end.
```

---

## Best Practices

### 1. Check Collision After Movement

**Check collision after calculating new position:**

```pascal
// ✅ GOOD: Check after movement
newX := x + vx;
if not CollidesWithTile(newX, y) then
  x := newX;

// ❌ BAD: Check before movement
if not CollidesWithTile(x, y) then
  x := x + vx;  // May still collide
```

### 2. Separate X and Y Collision

**Check X and Y separately for better response:**

```pascal
// ✅ GOOD: Separate checks
CheckXCollision;
CheckYCollision;

// ❌ BAD: Combined check
if CollidesWithTile(newX, newY) then
  // Don't know which axis to block
```

### 3. Use Tile Properties

**Use tile properties instead of hardcoding:**

```pascal
// ✅ GOOD: Properties
if tpSolid in TileProperties[tile] then
  // Handle collision

// ❌ BAD: Hardcoded
if tile = TILE_WALL then
  // Hard to extend
```

### 4. Cache Tile Lookups

**Cache tile data when checking multiple times:**

```pascal
// ✅ GOOD: Cache
var tile := GetCachedTile(x, y);
if tpSolid in TileProperties[tile] then
  // ...

// ❌ BAD: Repeated lookups
if tpSolid in TileProperties[GetTile(x, y)] then
  // Lookup every time
```

### 5. Check Only Relevant Tiles

**Only check tiles entity overlaps:**

```pascal
// ✅ GOOD: Range check
for tileY := minTileY to maxTileY do
  for tileX := minTileX to maxTileX do
    // Check only overlapping tiles

// ❌ BAD: Check all tiles
for tileY := 0 to tilemapHeight do
  for tileX := 0 to tilemapWidth do
    // Checks unnecessary tiles
```

---

## Exercises

### Exercise 1: Basic Tile Collision

Write a program that:
1. Creates a tilemap with walls
2. Creates a player entity
3. Detects collision between player and walls
4. Stops player when colliding with walls

### Exercise 2: One-Way Platforms

Write a program that:
1. Creates a tilemap with one-way platforms
2. Allows player to pass through from below
3. Blocks player when moving up through platform
4. Player can stand on platform

### Exercise 3: Multiple Collision Types

Write a program that:
1. Implements solid, one-way, trigger, and hazard tiles
2. Handles each collision type appropriately
3. Responds to triggers and hazards
4. Displays collision feedback

### Exercise 4: Efficient Collision

Write a program that:
1. Implements efficient tile collision (only check overlapping tiles)
2. Caches tile lookups
3. Handles multiple entities efficiently
4. Measures collision performance

---

**Previous Section:** [Creating Tilemaps](./01_CreatingTilemaps.md)  
**Next Section:** [Scrolling and Camera Movement](./03_ScrollingAndCameraMovement.md)  
**Language Specification:** See [Game Engine Intrinsics](../../languageSpecification/intrinsicsAndDirectives/10_GameEngineIntrinsics.md)  
**Last Updated:** 2025-01-XX

