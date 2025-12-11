# Collision Resolution

**Part of:** [Chapter 18: Physics and Movement](./README.md)

---

## Introduction

When entities collide, you need to resolve the collision by separating them and responding appropriately. This section teaches you collision resolution techniques, how to separate colliding entities, and how to handle different collision responses.

**Key concepts:**
- **Collision detection** — Finding collisions
- **Collision resolution** — Separating colliding entities
- **Collision response** — What happens after collision
- **Separation** — Moving entities apart
- **Impulse** — Applying forces from collisions

---

## Understanding Collision Resolution

### What is Collision Resolution?

**Collision resolution fixes overlapping entities:**

```
Before:        After Resolution:
[Entity1]      [Entity1]
  [Entity2]      [Entity2]
(Overlapping)   (Separated)
```

**Steps:**
1. **Detect collision** — Find overlapping entities
2. **Calculate separation** — How far to move apart
3. **Separate entities** — Move entities to non-overlapping positions
4. **Apply response** — Bounce, stop, damage, etc.

### Why Resolution Matters

**Without resolution:**
- Entities can overlap
- Entities can pass through each other
- Physics breaks down
- Game feels broken

**With resolution:**
- Entities stay separated
- Realistic collisions
- Proper physics
- Game feels solid

---

## Collision Detection

### AABB Collision

**Axis-Aligned Bounding Box collision:**

```pascal
function AABBCollision(
  x1, y1, w1, h1: integer;
  x2, y2, w2, h2: integer
): boolean;
begin
  AABBCollision :=
    (x1 < x2 + w2) and (x1 + w1 > x2) and
    (y1 < y2 + h2) and (y1 + h1 > y2);
end;
```

### Using CollisionCheck

**SuperPascal provides an intrinsic:**

```pascal
function CollisionCheck(id1, id2: TEntityID): boolean;
```

**Usage:**
```pascal
if CollisionCheck(player, enemy) then
  HandleCollision(player, enemy);
```

### Collision Detection System

**Check all entity pairs:**

```pascal
procedure CollisionDetectionSystem;
var
  i, j: word;
begin
  for i := 0 to entityCount - 1 do
  begin
    if EntityValid(i) and HasComponent(i, COMPONENT_COLLIDER) then
    begin
      for j := i + 1 to entityCount - 1 do
      begin
        if EntityValid(j) and HasComponent(j, COMPONENT_COLLIDER) then
        begin
          if CollisionCheck(i, j) then
            HandleCollision(i, j);
        end;
      end;
    end;
  end;
end;
```

---

## Collision Separation

### Calculating Overlap

**Calculate how much entities overlap:**

```pascal
type
  TOverlap = record
    X, Y: integer;  // Overlap amount
    Axis: byte;      // 0 = X axis, 1 = Y axis
  end;

function CalculateOverlap(
  x1, y1, w1, h1: integer;
  x2, y2, w2, h2: integer
): TOverlap;
var
  overlapX, overlapY: integer;
begin
  // Calculate X overlap
  if x1 < x2 then
    overlapX := (x1 + w1) - x2
  else
    overlapX := (x2 + w2) - x1;
  
  // Calculate Y overlap
  if y1 < y2 then
    overlapY := (y1 + h1) - y2
  else
    overlapY := (y2 + h2) - y1;
  
  // Choose axis with smallest overlap
  if overlapX < overlapY then
  begin
    CalculateOverlap.X := overlapX;
    CalculateOverlap.Y := 0;
    CalculateOverlap.Axis := 0;  // X axis
  end
  else
  begin
    CalculateOverlap.X := 0;
    CalculateOverlap.Y := overlapY;
    CalculateOverlap.Axis := 1;  // Y axis
  end;
end;
```

### Separating Entities

**Move entities apart:**

```pascal
procedure SeparateEntities(id1, id2: TEntityID);
var
  x1, y1, x2, y2: integer;
  w1, h1, w2, h2: integer;
  overlap: TOverlap;
begin
  EntityGetPosition(id1, x1, y1);
  EntityGetCollider(id1, w1, h1);
  EntityGetPosition(id2, x2, y2);
  EntityGetCollider(id2, w2, h2);
  
  overlap := CalculateOverlap(x1, y1, w1, h1, x2, y2, w2, h2);
  
  // Separate along axis with smallest overlap
  if overlap.Axis = 0 then
  begin
    // Separate on X axis
    if x1 < x2 then
      x1 := x1 - overlap.X div 2
    else
      x1 := x1 + overlap.X div 2;
    x2 := x2 - (overlap.X - (x1 - (x1 - overlap.X div 2)));
  end
  else
  begin
    // Separate on Y axis
    if y1 < y2 then
      y1 := y1 - overlap.Y div 2
    else
      y1 := y1 + overlap.Y div 2;
    y2 := y2 - (overlap.Y - (y1 - (y1 - overlap.Y div 2)));
  end;
  
  EntitySetPosition(id1, x1, y1);
  EntitySetPosition(id2, x2, y2);
end;
```

### Using CollisionResolve

**SuperPascal provides an intrinsic:**

```pascal
procedure CollisionResolve(id1, id2: TEntityID);
```

**Usage:**
```pascal
if CollisionCheck(player, enemy) then
  CollisionResolve(player, enemy);
```

---

## Collision Response

### Stop on Collision

**Stop entities when they collide:**

```pascal
procedure StopOnCollision(id1, id2: TEntityID);
var
  vx1, vy1, vx2, vy2: integer;
begin
  // Stop both entities
  EntitySetVelocity(id1, 0, 0);
  EntitySetVelocity(id2, 0, 0);
end;
```

### Bounce

**Bounce entities off each other:**

```pascal
const
  BOUNCE_DAMPING = 0.8;  // Energy loss on bounce

procedure BounceCollision(id1, id2: TEntityID);
var
  vx1, vy1, vx2, vy2: integer;
  x1, y1, x2, y2: integer;
  dx, dy: integer;
  overlap: TOverlap;
begin
  EntityGetPosition(id1, x1, y1);
  EntityGetPosition(id2, x2, y2);
  EntityGetVelocity(id1, vx1, vy1);
  EntityGetVelocity(id2, vx2, vy2);
  
  // Calculate collision normal
  dx := x2 - x1;
  dy := y2 - y1;
  
  overlap := CalculateOverlap(...);
  
  // Reverse velocity along collision axis
  if overlap.Axis = 0 then
  begin
    // X axis collision
    vx1 := -Trunc(vx1 * BOUNCE_DAMPING);
    vx2 := -Trunc(vx2 * BOUNCE_DAMPING);
  end
  else
  begin
    // Y axis collision
    vy1 := -Trunc(vy1 * BOUNCE_DAMPING);
    vy2 := -Trunc(vy2 * BOUNCE_DAMPING);
  end;
  
  EntitySetVelocity(id1, vx1, vy1);
  EntitySetVelocity(id2, vx2, vy2);
  
  // Separate entities
  CollisionResolve(id1, id2);
end;
```

### Push Apart

**Push entities apart based on mass:**

```pascal
procedure PushApart(id1, id2: TEntityID);
var
  x1, y1, x2, y2: integer;
  mass1, mass2: integer;
  overlap: TOverlap;
  push1, push2: integer;
begin
  EntityGetPosition(id1, x1, y1);
  EntityGetPosition(id2, x2, y2);
  mass1 := GetMass(id1);
  mass2 := GetMass(id2);
  
  overlap := CalculateOverlap(...);
  
  // Push based on mass ratio
  var totalMass := mass1 + mass2;
  push1 := Trunc((overlap.X + overlap.Y) * mass2 / totalMass);
  push2 := Trunc((overlap.X + overlap.Y) * mass1 / totalMass);
  
  // Move entities apart
  if x1 < x2 then
  begin
    x1 := x1 - push1;
    x2 := x2 + push2;
  end
  else
  begin
    x1 := x1 + push1;
    x2 := x2 - push2;
  end;
  
  EntitySetPosition(id1, x1, y1);
  EntitySetPosition(id2, x2, y2);
end;
```

---

## Collision Response Types

### Solid Collision

**Solid entities block movement:**

```pascal
procedure HandleSolidCollision(id1, id2: TEntityID);
var
  vx1, vy1, vx2, vy2: integer;
begin
  EntityGetVelocity(id1, vx1, vy1);
  EntityGetVelocity(id2, vx2, vy2);
  
  // Stop movement in collision direction
  // (Simplified - would need collision normal)
  vx1 := 0;
  vy1 := 0;
  vx2 := 0;
  vy2 := 0;
  
  EntitySetVelocity(id1, vx1, vy1);
  EntitySetVelocity(id2, vx2, vy2);
  
  // Separate
  CollisionResolve(id1, id2);
end;
```

### Trigger Collision

**Trigger entities activate events:**

```pascal
procedure HandleTriggerCollision(id1, id2: TEntityID);
begin
  // Don't separate - triggers don't block
  // Just activate event
  OnTriggerActivated(id1, id2);
end;
```

### Damage Collision

**Collisions cause damage:**

```pascal
procedure HandleDamageCollision(id1, id2: TEntityID);
begin
  // Apply damage
  TakeDamage(id1, 1);
  TakeDamage(id2, 1);
  
  // Separate
  CollisionResolve(id1, id2);
  
  // Apply knockback
  ApplyKnockback(id1, id2);
  ApplyKnockback(id2, id1);
end;
```

---

## Advanced Resolution

### Minimum Translation Vector (MTV)

**Find minimum distance to separate:**

```pascal
type
  TMTV = record
    X, Y: integer;  // Translation vector
    Distance: integer;  // Distance to move
  end;

function CalculateMTV(
  x1, y1, w1, h1: integer;
  x2, y2, w2, h2: integer
): TMTV;
var
  overlapX, overlapY: integer;
begin
  // Calculate overlaps
  if x1 < x2 then
    overlapX := (x1 + w1) - x2
  else
    overlapX := (x2 + w2) - x1;
  
  if y1 < y2 then
    overlapY := (y1 + h1) - y2
  else
    overlapY := (y2 + h2) - y1;
  
  // Choose axis with smallest overlap
  if overlapX < overlapY then
  begin
    CalculateMTV.X := Sign(x2 - x1);
    CalculateMTV.Y := 0;
    CalculateMTV.Distance := overlapX;
  end
  else
  begin
    CalculateMTV.X := 0;
    CalculateMTV.Y := Sign(y2 - y1);
    CalculateMTV.Distance := overlapY;
  end;
end;
```

### Impulse-Based Resolution

**Apply impulse to separate entities:**

```pascal
procedure ImpulseResolution(id1, id2: TEntityID);
var
  x1, y1, x2, y2: integer;
  vx1, vy1, vx2, vy2: integer;
  mass1, mass2: integer;
  dx, dy: integer;
  distance: integer;
  impulseX, impulseY: integer;
begin
  EntityGetPosition(id1, x1, y1);
  EntityGetPosition(id2, x2, y2);
  EntityGetVelocity(id1, vx1, vy1);
  EntityGetVelocity(id2, vx2, vy2);
  mass1 := GetMass(id1);
  mass2 := GetMass(id2);
  
  // Calculate collision normal
  dx := x2 - x1;
  dy := y2 - y1;
  distance := Trunc(Sqrt(dx * dx + dy * dy));
  
  if distance > 0 then
  begin
    // Normalize
    dx := Trunc((dx * 256) / distance);  // Fixed-point
    dy := Trunc((dy * 256) / distance);
    
    // Calculate relative velocity
    var relVx := vx2 - vx1;
    var relVy := vy2 - vy1;
    
    // Calculate impulse
    var impulse := Trunc((relVx * dx + relVy * dy) / (mass1 + mass2));
    
    impulseX := Trunc((impulse * dx) / 256);
    impulseY := Trunc((impulse * dy) / 256);
    
    // Apply impulse
    vx1 := vx1 + Trunc(impulseX * mass2 / (mass1 + mass2));
    vy1 := vy1 + Trunc(impulseY * mass2 / (mass1 + mass2));
    vx2 := vx2 - Trunc(impulseX * mass1 / (mass1 + mass2));
    vy2 := vy2 - Trunc(impulseY * mass1 / (mass1 + mass2));
    
    EntitySetVelocity(id1, vx1, vy1);
    EntitySetVelocity(id2, vx2, vy2);
  end;
  
  // Separate
  CollisionResolve(id1, id2);
end;
```

---

## Complete Collision System

**Putting it all together:**

```pascal
program CollisionResolutionDemo;

var
  player: TEntityID;
  enemy: TEntityID;

procedure InitEntities;
begin
  player := EntityCreate;
  EntitySetPosition(player, 100, 100);
  EntitySetVelocity(player, 2, 0);
  EntitySetColliderAABB(player, 0, 0, 16, 16, true);
  
  enemy := EntityCreate;
  EntitySetPosition(enemy, 200, 100);
  EntitySetVelocity(enemy, -1, 0);
  EntitySetColliderAABB(enemy, 0, 0, 16, 16, true);
end;

procedure CollisionSystem;
begin
  // Check collision
  if CollisionCheck(player, enemy) then
  begin
    // Resolve collision
    CollisionResolve(player, enemy);
    
    // Apply response
    BounceCollision(player, enemy);
  end;
end;

procedure UpdateGame;
begin
  // Update physics
  PhysicsApplyGravity(player);
  PhysicsApplyGravity(enemy);
  PhysicsApplyVelocity(player);
  PhysicsApplyVelocity(enemy);
  
  // Check and resolve collisions
  CollisionSystem;
end;

begin
  InitGraphics;
  InitEntities;
  
  while true do
  begin
    UpdateGame;
    WaitVBlank;
    RenderGame;
  end;
end.
```

---

## Best Practices

### 1. Resolve After Movement

**Resolve collisions after all movement:**

```pascal
// ✅ GOOD: Movement then resolution
PhysicsApplyVelocity(player);
PhysicsApplyVelocity(enemy);
CollisionSystem;  // Resolve after movement

// ❌ BAD: Resolution before movement
CollisionSystem;  // Resolve before movement
PhysicsApplyVelocity(player);
```

### 2. Separate Before Response

**Separate entities before applying response:**

```pascal
// ✅ GOOD: Separate then respond
CollisionResolve(id1, id2);
BounceCollision(id1, id2);

// ❌ BAD: Respond then separate
BounceCollision(id1, id2);
CollisionResolve(id1, id2);  // May still overlap
```

### 3. Use Smallest Overlap

**Separate along axis with smallest overlap:**

```pascal
// ✅ GOOD: Smallest overlap
if overlapX < overlapY then
  SeparateX
else
  SeparateY;

// ❌ BAD: Always separate X
SeparateX;  // May cause tunneling
```

### 4. Handle Multiple Collisions

**Resolve all collisions, not just first:**

```pascal
// ✅ GOOD: Resolve all
for i := 0 to entityCount - 1 do
  for j := i + 1 to entityCount - 1 do
    if CollisionCheck(i, j) then
      CollisionResolve(i, j);

// ❌ BAD: Stop after first
if CollisionCheck(i, j) then
begin
  CollisionResolve(i, j);
  Exit;  // Misses other collisions
end;
```

### 5. Check Collision Types

**Handle different collision types differently:**

```pascal
// ✅ GOOD: Type-based response
case CollisionType(id1, id2) of
  ctSolid: HandleSolidCollision(id1, id2);
  ctTrigger: HandleTriggerCollision(id1, id2);
  ctDamage: HandleDamageCollision(id1, id2);
end;

// ❌ BAD: Same response for all
CollisionResolve(id1, id2);  // Doesn't handle types
```

---

## Exercises

### Exercise 1: Basic Resolution

Write a program that:
1. Creates two entities that move toward each other
2. Detects collision between them
3. Separates entities when they collide
4. Stops entities on collision

### Exercise 2: Bounce Collision

Write a program that:
1. Creates entities that bounce off each other
2. Reverses velocity on collision
3. Applies damping to reduce energy
4. Entities bounce realistically

### Exercise 3: Mass-Based Resolution

Write a program that:
1. Creates entities with different masses
2. Separates entities based on mass ratio
3. Heavier entities push lighter ones more
4. Demonstrates mass effects

### Exercise 4: Complete Collision System

Write a program that:
1. Implements collision detection for all entities
2. Resolves all collisions
3. Handles different collision types (solid, trigger, damage)
4. Applies appropriate responses

---

**Previous Section:** [Gravity Model](./02_GravityModel.md)  
**Next Chapter:** [Chapter 19: Mathematics for Graphics and Games](../19_MathematicsForGraphicsAndGames/README.md)  
**Language Specification:** See [Game Engine Intrinsics](../../languageSpecification/intrinsicsAndDirectives/10_GameEngineIntrinsics.md)  
**Last Updated:** 2025-01-XX

