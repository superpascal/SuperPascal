# Position and Velocity

**Part of:** [Chapter 18: Physics and Movement](./README.md)

---

## Introduction

Position and velocity are the foundation of physics simulation. This section teaches you how position and velocity work together to create movement, how to update them, and how to use them in your games.

**Key concepts:**
- **Position** — Where an entity is
- **Velocity** — How fast and in what direction an entity moves
- **Movement** — Updating position based on velocity
- **Forces** — Changing velocity
- **Physics update** — Applying physics each frame

---

## Understanding Position

### What is Position?

**Position is the location of an entity in the game world:**

```pascal
type
  TPosition = record
    X, Y: integer;  // Coordinates in pixels
  end;
```

**Position represents:**
- **X coordinate** — Horizontal position (left to right)
- **Y coordinate** — Vertical position (top to bottom)
- **Origin** — Usually top-left corner (0, 0)

**Example:**
```pascal
var
  player: TEntityID;
  x, y: integer;
  
begin
  player := EntityCreate;
  EntitySetPosition(player, 100, 50);  // Position at (100, 50)
  EntityGetPosition(player, x, y);
  WriteLn('Player at: ', x, ', ', y);  // Output: Player at: 100, 50
end;
```

### Position Units

**Position can be in different units:**

- **Pixels** — Screen pixels (most common)
- **Tiles** — Tile coordinates (for tile-based games)
- **World units** — Arbitrary units (for scaling)

**Example:**
```pascal
const
  TILE_SIZE = 8;  // Pixels per tile

// Position in pixels
EntitySetPosition(player, 100, 50);

// Position in tiles (convert to pixels)
var tileX := 5;
var tileY := 3;
EntitySetPosition(player, tileX * TILE_SIZE, tileY * TILE_SIZE);
```

---

## Understanding Velocity

### What is Velocity?

**Velocity is the rate of change of position:**

```pascal
type
  TVelocity = record
    VX, VY: integer;  // Velocity in pixels per frame
  end;
```

**Velocity represents:**
- **VX** — Horizontal velocity (positive = right, negative = left)
- **VY** — Vertical velocity (positive = down, negative = up)
- **Speed** — Magnitude of velocity
- **Direction** — Direction of movement

**Example:**
```pascal
var
  bullet: TEntityID;
  vx, vy: integer;
  
begin
  bullet := EntityCreate;
  EntitySetVelocity(bullet, 5, 0);  // Move right at 5 pixels/frame
  EntityGetVelocity(bullet, vx, vy);
  WriteLn('Velocity: ', vx, ', ', vy);  // Output: Velocity: 5, 0
end;
```

### Velocity Units

**Velocity is typically in pixels per frame:**

- **Pixels per frame** — Most common (simple, frame-dependent)
- **Pixels per second** — Requires delta time (frame-independent)

**Example:**
```pascal
// Pixels per frame (simple)
EntitySetVelocity(bullet, 5, 0);  // 5 pixels per frame

// Pixels per second (with delta time)
const SPEED = 100;  // Pixels per second
var dt: Q8.8;
EntitySetVelocity(bullet, Trunc(SPEED * dt), 0);
```

---

## Movement: Position + Velocity

### Basic Movement

**Update position based on velocity:**

```pascal
procedure UpdateMovement(entity: TEntityID);
var
  x, y, vx, vy: integer;
begin
  EntityGetPosition(entity, x, y);
  EntityGetVelocity(entity, vx, vy);
  
  // Apply velocity to position
  x := x + vx;
  y := y + vy;
  
  EntitySetPosition(entity, x, y);
end;
```

### Using PhysicsApplyVelocity

**SuperPascal provides an intrinsic:**

```pascal
procedure PhysicsApplyVelocity(id: TEntityID);
```

**Usage:**
```pascal
var
  player: TEntityID;
  
begin
  player := EntityCreate;
  EntitySetVelocity(player, 2, 0);  // Set velocity
  
  // Apply velocity each frame
  while true do
  begin
    PhysicsApplyVelocity(player);  // Updates position
    WaitVBlank;
  end;
end;
```

### Movement System

**Complete movement system:**

```pascal
procedure MovementSystem;
var
  i: word;
begin
  for i := 0 to entityCount - 1 do
  begin
    if EntityValid(i) and 
       HasComponent(i, COMPONENT_POSITION) and
       HasComponent(i, COMPONENT_VELOCITY) then
    begin
      PhysicsApplyVelocity(i);
    end;
  end;
end;
```

---

## Changing Velocity

### Direct Velocity Changes

**Set velocity directly:**

```pascal
procedure MoveRight(entity: TEntityID);
var
  vx, vy: integer;
begin
  EntityGetVelocity(entity, vx, vy);
  vx := 2;  // Move right
  EntitySetVelocity(entity, vx, vy);
end;

procedure MoveLeft(entity: TEntityID);
var
  vx, vy: integer;
begin
  EntityGetVelocity(entity, vx, vy);
  vx := -2;  // Move left
  EntitySetVelocity(entity, vx, vy);
end;

procedure Jump(entity: TEntityID);
var
  vx, vy: integer;
begin
  EntityGetVelocity(entity, vx, vy);
  vy := -10;  // Jump up
  EntitySetVelocity(entity, vx, vy);
end;
```

### Adding to Velocity

**Add to existing velocity:**

```pascal
procedure Accelerate(entity: TEntityID; ax, ay: integer);
var
  vx, vy: integer;
begin
  EntityGetVelocity(entity, vx, vy);
  vx := vx + ax;  // Add acceleration
  vy := vy + ay;
  EntitySetVelocity(entity, vx, vy);
end;

// Usage
Accelerate(player, 1, 0);  // Accelerate right
```

### Velocity Limits

**Clamp velocity to maximum speed:**

```pascal
const
  MAX_SPEED = 10;

procedure ClampVelocity(entity: TEntityID);
var
  vx, vy: integer;
  speed: integer;
begin
  EntityGetVelocity(entity, vx, vy);
  
  // Calculate speed (magnitude)
  speed := Trunc(Sqrt(vx * vx + vy * vy));
  
  if speed > MAX_SPEED then
  begin
    // Normalize and scale to max speed
    vx := Trunc((vx * MAX_SPEED) / speed);
    vy := Trunc((vy * MAX_SPEED) / speed);
    EntitySetVelocity(entity, vx, vy);
  end;
end;
```

---

## Forces and Acceleration

### What are Forces?

**Forces change velocity (acceleration):**

- **Gravity** — Pulls down
- **Friction** — Slows down
- **Thrust** — Pushes forward
- **Wind** — Pushes sideways

**Force → Acceleration → Velocity → Position**

### Applying Forces

**Apply forces to change velocity:**

```pascal
procedure ApplyForce(entity: TEntityID; fx, fy: integer);
var
  vx, vy: integer;
begin
  EntityGetVelocity(entity, vx, vy);
  
  // Add force to velocity (acceleration)
  vx := vx + fx;
  vy := vy + fy;
  
  EntitySetVelocity(entity, vx, vy);
end;

// Usage
ApplyForce(player, 0, 1);   // Apply gravity (down)
ApplyForce(player, -1, 0);  // Apply friction (left, slows down)
```

### Acceleration

**Acceleration is change in velocity:**

```pascal
type
  TAcceleration = record
    AX, AY: integer;  // Acceleration in pixels per frame squared
  end;

procedure ApplyAcceleration(entity: TEntityID; ax, ay: integer);
var
  vx, vy: integer;
begin
  EntityGetVelocity(entity, vx, vy);
  
  // Add acceleration to velocity
  vx := vx + ax;
  vy := vy + ay;
  
  EntitySetVelocity(entity, vx, vy);
end;
```

---

## Friction

### Applying Friction

**Friction slows down movement:**

```pascal
const
  FRICTION = 0.9;  // Friction coefficient (0.0 to 1.0)

procedure ApplyFriction(entity: TEntityID);
var
  vx, vy: integer;
begin
  EntityGetVelocity(entity, vx, vy);
  
  // Apply friction (multiply by coefficient)
  vx := Trunc(vx * FRICTION);
  vy := Trunc(vy * FRICTION);
  
  // Stop if very slow
  if Abs(vx) < 1 then vx := 0;
  if Abs(vy) < 1 then vy := 0;
  
  EntitySetVelocity(entity, vx, vy);
end;
```

### Using PhysicsApplyFriction

**SuperPascal provides an intrinsic:**

```pascal
procedure PhysicsApplyFriction(id: TEntityID);
```

**Usage:**
```pascal
// Apply friction to entity
PhysicsApplyFriction(player);
```

### Ground Friction

**Apply friction only when on ground:**

```pascal
procedure ApplyGroundFriction(entity: TEntityID);
var
  vx, vy: integer;
  isGrounded: boolean;
begin
  EntityGetVelocity(entity, vx, vy);
  isGrounded := CheckGround(entity);
  
  if isGrounded then
  begin
    // Apply friction to horizontal movement
    vx := Trunc(vx * FRICTION);
    if Abs(vx) < 1 then vx := 0;
    EntitySetVelocity(entity, vx, vy);
  end;
end;
```

---

## Complete Movement Example

**Putting it all together:**

```pascal
program MovementDemo;

var
  player: TEntityID;

procedure InitPlayer;
begin
  player := EntityCreate;
  EntitySetPosition(player, 160, 120);
  EntitySetVelocity(player, 0, 0);
end;

procedure UpdatePlayerMovement;
var
  input: word;
  vx, vy: integer;
begin
  input := ReadInput;
  EntityGetVelocity(player, vx, vy);
  
  // Handle input
  if (input and BTN_LEFT) <> 0 then
    vx := -2
  else if (input and BTN_RIGHT) <> 0 then
    vx := 2
  else
    vx := 0;  // Stop horizontal movement
  
  if (input and BTN_UP) <> 0 then
    vy := -2
  else if (input and BTN_DOWN) <> 0 then
    vy := 2
  else
    vy := 0;  // Stop vertical movement
  
  EntitySetVelocity(player, vx, vy);
  
  // Apply velocity to position
  PhysicsApplyVelocity(player);
  
  // Apply friction
  PhysicsApplyFriction(player);
end;

begin
  InitGraphics;
  InitPlayer;
  
  while true do
  begin
    UpdatePlayerMovement;
    WaitVBlank;
    RenderGame;
  end;
end.
```

---

## Best Practices

### 1. Update Velocity Before Position

**Set velocity, then apply to position:**

```pascal
// ✅ GOOD: Velocity then position
EntitySetVelocity(player, vx, vy);
PhysicsApplyVelocity(player);

// ❌ BAD: Position then velocity
PhysicsApplyVelocity(player);
EntitySetVelocity(player, vx, vy);  // Too late
```

### 2. Use Intrinsics When Available

**Use physics intrinsics for consistency:**

```pascal
// ✅ GOOD: Use intrinsics
PhysicsApplyVelocity(player);
PhysicsApplyFriction(player);

// ❌ BAD: Manual implementation
// May not match engine behavior
```

### 3. Clamp Velocity

**Limit velocity to prevent extreme speeds:**

```pascal
// ✅ GOOD: Clamp velocity
ClampVelocity(player);

// ❌ BAD: No limits
// Velocity can grow unbounded
```

### 4. Separate Forces

**Apply different forces separately:**

```pascal
// ✅ GOOD: Separate forces
ApplyGravity(player);
ApplyFriction(player);
ApplyThrust(player);

// ❌ BAD: Combined forces
// Hard to debug and modify
```

### 5. Check Ground for Friction

**Only apply ground friction when on ground:**

```pascal
// ✅ GOOD: Check ground
if IsGrounded(player) then
  ApplyFriction(player);

// ❌ BAD: Always apply
ApplyFriction(player);  // Friction in air?
```

---

## Exercises

### Exercise 1: Basic Movement

Write a program that:
1. Creates an entity with position and velocity
2. Sets velocity based on input
3. Updates position each frame
4. Moves entity across screen

### Exercise 2: Velocity Limits

Write a program that:
1. Creates an entity that accelerates
2. Limits velocity to maximum speed
3. Clamps velocity when it exceeds limit
4. Displays current velocity

### Exercise 3: Friction

Write a program that:
1. Creates an entity that moves
2. Applies friction to slow down
3. Stops entity when velocity is very small
4. Demonstrates smooth deceleration

### Exercise 4: Forces

Write a program that:
1. Applies multiple forces (gravity, friction, thrust)
2. Combines forces correctly
3. Updates velocity based on forces
4. Shows force effects

---

**Previous Chapter:** [Chapter 17: Tilemaps and Level Design](../17_TilemapsAndLevelDesign/README.md)  
**Next Section:** [Gravity Model](./02_GravityModel.md)  
**Language Specification:** See [Game Engine Intrinsics](../../languageSpecification/intrinsicsAndDirectives/10_GameEngineIntrinsics.md)  
**Last Updated:** 2025-01-XX

