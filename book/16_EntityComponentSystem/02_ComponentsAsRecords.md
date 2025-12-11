# Components as Records

**Part of:** [Chapter 16: Entity Component System](./README.md)

---

## Introduction

Components are **data containers** — they store information but have no behavior. This section teaches you how components work, how they're stored, and how to access them.

**Key concepts:**
- **Components are records** — Data structures, no methods
- **Structure-of-Arrays (SoA)** — Efficient storage pattern
- **Component access** — Getting and setting component data
- **Component composition** — Entities have multiple components
- **Component types** — Position, Velocity, Sprite, etc.

---

## What are Components?

### Components are Data

**Components are records that store data:**

```pascal
type
  TPosition = record
    X, Y: integer;  // Position data
  end;
  
  TVelocity = record
    VX, VY: integer;  // Velocity data
  end;
  
  TSprite = record
    SpriteID: byte;
    TileID: word;
    Palette: byte;
    Visible: boolean;
  end;
```

**Components have NO behavior:**
- No methods
- No procedures
- Just data fields

**Why no behavior?**
- Behavior is in **systems** (next section)
- Components are pure data
- Systems process components

### Components vs Objects

**Comparison:**

```pascal
// ❌ BAD: Object-oriented (behavior in object)
type
  TEntity = class
    X, Y: integer;
    procedure Move(dx, dy: integer);  // Behavior in object
  end;

// ✅ GOOD: ECS (data in component, behavior in system)
type
  TPosition = record
    X, Y: integer;  // Just data
  end;
// Behavior is in MovementSystem (separate)
```

---

## Common Component Types

### Position Component

**Stores entity position:**

```pascal
type
  TPosition = record
    X, Y: integer;  // 2D coordinates
  end;
```

**Usage:**
```pascal
var
  player: TEntityID;
  x, y: integer;
  
begin
  player := EntityCreate;
  EntitySetPosition(player, 100, 50);
  EntityGetPosition(player, x, y);
  WriteLn('Player at: ', x, ', ', y);
end;
```

### Velocity Component

**Stores movement velocity:**

```pascal
type
  TVelocity = record
    VX, VY: integer;  // Velocity in pixels per frame
  end;
```

**Usage:**
```pascal
var
  bullet: TEntityID;
  vx, vy: integer;
  
begin
  bullet := EntityCreate;
  EntitySetVelocity(bullet, 5, 0);  // Move right
  EntityGetVelocity(bullet, vx, vy);
end;
```

### Sprite Component

**Stores visual representation:**

```pascal
type
  TSprite = record
    SpriteID: byte;   // Hardware sprite index
    TileID: word;     // Tile/graphic index
    Palette: byte;     // Color palette
    Visible: boolean;  // Visibility flag
  end;
```

**Usage:**
```pascal
var
  player: TEntityID;
  
begin
  player := EntityCreate;
  EntitySetSprite(player, 0, 42, 1);  // Sprite 0, tile 42, palette 1
end;
```

### Collider Component

**Stores collision shape:**

```pascal
type
  TColliderAABB = record
    X, Y: integer;    // Offset from entity position
    W, H: integer;    // Width and height
    Solid: boolean;   // Solid collision flag
  end;
  
  TColliderCircle = record
    X, Y: integer;    // Offset from entity position
    Radius: integer;  // Circle radius
    Solid: boolean;
  end;
```

**Usage:**
```pascal
var
  wall: TEntityID;
  
begin
  wall := EntityCreate;
  EntitySetColliderAABB(wall, 0, 0, 16, 16, true);  // 16x16 solid
end;
```

---

## Structure-of-Arrays (SoA) Storage

### What is SoA?

**Structure-of-Arrays stores component data in separate arrays:**

```pascal
// Instead of Array-of-Structures (AoS):
type
  TEntity = record
    X, Y: integer;
    VX, VY: integer;
  end;
var entities: array[0..255] of TEntity;  // AoS

// Use Structure-of-Arrays (SoA):
var
  PositionX: array[0..255] of integer;
  PositionY: array[0..255] of integer;
  VelocityX: array[0..255] of integer;
  VelocityY: array[0..255] of integer;  // SoA
```

### Why SoA?

**Benefits of SoA:**

1. **Cache-friendly** — Process all X positions, then all Y positions
2. **SIMD-friendly** — Can process multiple values in parallel
3. **Memory-efficient** — No padding between components
4. **Fast iteration** — Loop through one component type at a time

**Example:**
```pascal
// ✅ GOOD: SoA - process all positions at once
procedure UpdateAllPositions;
var
  i: word;
begin
  for i := 0 to entityCount - 1 do
  begin
    PositionX[i] := PositionX[i] + VelocityX[i];
    PositionY[i] := PositionY[i] + VelocityY[i];
  end;
end;

// ❌ BAD: AoS - mixed data access
procedure UpdateAllPositions;
var
  i: word;
begin
  for i := 0 to entityCount - 1 do
  begin
    entities[i].X := entities[i].X + entities[i].VX;  // Mixed access
    entities[i].Y := entities[i].Y + entities[i].VY;
  end;
end;
```

### SoA Storage Pattern

**Each component field has its own array:**

```pascal
// Position component (X, Y)
var
  PositionX: array[0..MAX_ENTITIES-1] of integer;
  PositionY: array[0..MAX_ENTITIES-1] of integer;

// Velocity component (VX, VY)
var
  VelocityX: array[0..MAX_ENTITIES-1] of integer;
  VelocityY: array[0..MAX_ENTITIES-1] of integer;

// Sprite component
var
  SpriteID: array[0..MAX_ENTITIES-1] of byte;
  TileID: array[0..MAX_ENTITIES-1] of word;
  Palette: array[0..MAX_ENTITIES-1] of byte;
  Visible: array[0..MAX_ENTITIES-1] of boolean;
```

**Entity ID is the array index:**
- Entity 0 → `PositionX[0]`, `PositionY[0]`
- Entity 1 → `PositionX[1]`, `PositionY[1]`
- Entity N → `PositionX[N]`, `PositionY[N]`

---

## Component Access

### Setting Components

**Use intrinsics to set component data:**

```pascal
// Position
procedure EntitySetPosition(id: TEntityID; x, y: integer);

// Velocity
procedure EntitySetVelocity(id: TEntityID; vx, vy: integer);

// Sprite
procedure EntitySetSprite(id: TEntityID; spriteID: byte; tileID: word; palette: byte);
```

**Usage:**
```pascal
var
  player: TEntityID;
  
begin
  player := EntityCreate;
  EntitySetPosition(player, 100, 50);
  EntitySetVelocity(player, 2, 0);
  EntitySetSprite(player, 0, 42, 1);
end;
```

### Getting Components

**Use intrinsics to get component data:**

```pascal
// Position
procedure EntityGetPosition(id: TEntityID; var x, y: integer);

// Velocity
procedure EntityGetVelocity(id: TEntityID; var vx, vy: integer);
```

**Usage:**
```pascal
var
  player: TEntityID;
  x, y: integer;
  vx, vy: integer;
  
begin
  player := EntityCreate;
  EntitySetPosition(player, 100, 50);
  EntityGetPosition(player, x, y);
  WriteLn('Position: ', x, ', ', y);
  
  EntityGetVelocity(player, vx, vy);
  WriteLn('Velocity: ', vx, ', ', vy);
end;
```

### Direct Array Access (Advanced)

**You can access arrays directly (if needed):**

```pascal
var
  player: TEntityID;
  
begin
  player := EntityCreate;
  
  // Set via intrinsic (recommended)
  EntitySetPosition(player, 100, 50);
  
  // Or set directly (advanced, use with caution)
  PositionX[player] := 100;
  PositionY[player] := 50;
end;
```

**Note:** Direct access bypasses validation and change tracking. Use intrinsics when possible.

---

## Component Composition

### Entities Have Multiple Components

**An entity can have any combination of components:**

```pascal
var
  player: TEntityID;
  bullet: TEntityID;
  wall: TEntityID;
  
begin
  // Player: Position + Velocity + Sprite
  player := EntityCreate;
  EntitySetPosition(player, 100, 50);
  EntitySetVelocity(player, 2, 0);
  EntitySetSprite(player, 0, 42, 1);
  
  // Bullet: Position + Velocity + Sprite
  bullet := EntityCreate;
  EntitySetPosition(bullet, 50, 100);
  EntitySetVelocity(bullet, 5, 0);
  EntitySetSprite(bullet, 1, 10, 1);
  
  // Wall: Position + Collider (no velocity, no sprite)
  wall := EntityCreate;
  EntitySetPosition(wall, 200, 100);
  EntitySetColliderAABB(wall, 0, 0, 16, 16, true);
end;
```

### Component Presence

**Check if entity has a component:**

```pascal
function EntityHasPosition(id: TEntityID): boolean;
begin
  // Check if position component exists
  // (Implementation depends on component tracking)
  EntityHasPosition := HasComponent(id, COMPONENT_POSITION);
end;

// Usage
if EntityHasPosition(player) then
  EntityGetPosition(player, x, y);
```

---

## Component Patterns

### Component Flags

**Track which components an entity has:**

```pascal
type
  TComponentFlags = word;
  
const
  COMPONENT_POSITION = $0001;
  COMPONENT_VELOCITY = $0002;
  COMPONENT_SPRITE = $0004;
  COMPONENT_COLLIDER = $0008;

var
  ComponentFlags: array[0..MAX_ENTITIES-1] of TComponentFlags;

procedure AddComponent(id: TEntityID; component: TComponentFlags);
begin
  ComponentFlags[id] := ComponentFlags[id] or component;
end;

function HasComponent(id: TEntityID; component: TComponentFlags): boolean;
begin
  HasComponent := (ComponentFlags[id] and component) <> 0;
end;
```

### Component Initialization

**Initialize components when adding:**

```pascal
procedure AddPositionComponent(id: TEntityID; x, y: integer);
begin
  EntitySetPosition(id, x, y);
  AddComponent(id, COMPONENT_POSITION);
end;

procedure AddVelocityComponent(id: TEntityID; vx, vy: integer);
begin
  EntitySetVelocity(id, vx, vy);
  AddComponent(id, COMPONENT_VELOCITY);
end;
```

### Component Removal

**Remove components when destroying entity:**

```pascal
procedure RemoveComponent(id: TEntityID; component: TComponentFlags);
begin
  ComponentFlags[id] := ComponentFlags[id] and not component;
  // Clear component data (set to defaults)
end;

procedure EntityDestroy(id: TEntityID);
begin
  // Remove all components
  RemoveComponent(id, COMPONENT_POSITION);
  RemoveComponent(id, COMPONENT_VELOCITY);
  RemoveComponent(id, COMPONENT_SPRITE);
  // ... etc
  // Mark entity as free
end;
```

---

## Complete Component Example

**Putting it all together:**

```pascal
program ComponentDemo;

var
  player: TEntityID;
  enemy: TEntityID;
  bullet: TEntityID;

procedure CreatePlayer;
begin
  player := EntityCreate;
  if player <> ENTITY_NULL then
  begin
    EntitySetPosition(player, 160, 120);
    EntitySetVelocity(player, 0, 0);
    EntitySetSprite(player, 0, 42, 1);
  end;
end;

procedure CreateEnemy(x, y: integer);
begin
  enemy := EntityCreate;
  if enemy <> ENTITY_NULL then
  begin
    EntitySetPosition(enemy, x, y);
    EntitySetVelocity(enemy, -1, 0);
    EntitySetSprite(enemy, 1, 10, 2);
    EntitySetColliderAABB(enemy, 0, 0, 16, 16, true);
  end;
end;

procedure CreateBullet(x, y: integer);
begin
  bullet := EntityCreate;
  if bullet <> ENTITY_NULL then
  begin
    EntitySetPosition(bullet, x, y);
    EntitySetVelocity(bullet, 5, 0);
    EntitySetSprite(bullet, 2, 5, 1);
  end;
end;

begin
  CreatePlayer;
  CreateEnemy(300, 100);
  CreateBullet(170, 120);
  
  // Game loop would update and render entities
end.
```

---

## Best Practices

### 1. Use Intrinsics

**Use intrinsics instead of direct array access:**

```pascal
// ✅ GOOD: Intrinsics
EntitySetPosition(player, 100, 50);

// ❌ BAD: Direct access
PositionX[player] := 100;
PositionY[player] := 50;  // Bypasses validation
```

### 2. Validate Before Access

**Check entity validity before accessing components:**

```pascal
// ✅ GOOD: Validate first
if EntityValid(player) then
  EntityGetPosition(player, x, y);

// ❌ BAD: No validation
EntityGetPosition(player, x, y);  // May crash if invalid
```

### 3. Initialize Components

**Set all component fields when creating entity:**

```pascal
// ✅ GOOD: Initialize all fields
EntitySetPosition(player, 100, 50);
EntitySetVelocity(player, 0, 0);
EntitySetSprite(player, 0, 42, 1);

// ❌ BAD: Missing initialization
EntitySetPosition(player, 100, 50);
// Velocity and sprite not set (undefined values)
```

### 4. Use SoA Storage

**Store components in separate arrays:**

```pascal
// ✅ GOOD: SoA
var
  PositionX: array[0..255] of integer;
  PositionY: array[0..255] of integer;

// ❌ BAD: AoS
type
  TEntity = record
    X, Y: integer;
  end;
var entities: array[0..255] of TEntity;
```

### 5. Group Related Components

**Keep related components together:**

```pascal
// ✅ GOOD: Related components
EntitySetPosition(player, x, y);
EntitySetVelocity(player, vx, vy);  // Both movement-related

// ❌ BAD: Mixed concerns
EntitySetPosition(player, x, y);
EntitySetSprite(enemy, 0, 10, 1);  // Different entities
EntitySetVelocity(player, vx, vy);
```

---

## Exercises

### Exercise 1: Basic Components

Write a program that:
1. Creates an entity
2. Adds Position, Velocity, and Sprite components
3. Gets and displays component values
4. Updates position based on velocity

### Exercise 2: Component Composition

Write a program that:
1. Creates multiple entities with different component combinations
2. Player: Position + Velocity + Sprite
3. Wall: Position + Collider (no velocity)
4. Bullet: Position + Velocity + Sprite (no collider)
5. Displays which components each entity has

### Exercise 3: SoA Storage

Write a program that:
1. Creates 10 entities with Position components
2. Accesses PositionX and PositionY arrays directly
3. Updates all positions in a loop (SoA pattern)
4. Demonstrates cache-friendly access

### Exercise 4: Component Management

Write a program that:
1. Tracks component flags for each entity
2. Adds and removes components dynamically
3. Checks component presence before access
4. Cleans up components when destroying entities

---

**Previous Section:** [Entities as IDs](./01_EntitiesAsIDs.md)  
**Next Section:** [Systems as Loops](./03_SystemsAsLoops.md)  
**Language Specification:** See [Game Engine Concepts](../../languageSpecification/09_GameEngine_Concepts.md)  
**Last Updated:** 2025-01-XX

