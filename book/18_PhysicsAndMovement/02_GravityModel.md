# Gravity Model

**Part of:** [Chapter 18: Physics and Movement](./README.md)

---

## Introduction

Gravity is a fundamental force in physics simulation. This section teaches you how to implement gravity, handle falling, jumping, and ground detection.

**Key concepts:**
- **Gravity** — Constant downward acceleration
- **Falling** — Objects fall due to gravity
- **Jumping** — Overcoming gravity with upward velocity
- **Ground detection** — Detecting when entity is on ground
- **Terminal velocity** — Maximum falling speed

---

## Understanding Gravity

### What is Gravity?

**Gravity is a constant downward force:**

- **Direction** — Always downward (positive Y)
- **Magnitude** — Constant acceleration (e.g., 1 pixel per frame squared)
- **Effect** — Increases downward velocity over time

**Visual representation:**
```
Frame 0:  [Entity]  vy = 0
Frame 1:  [Entity]  vy = 1  (gravity applied)
          ↓
Frame 2:  [Entity]  vy = 2  (gravity applied again)
          ↓
Frame 3:  [Entity]  vy = 3  (falling faster)
          ↓
```

### Gravity Constant

**Define gravity as a constant:**

```pascal
const
  GRAVITY = 1;  // Pixels per frame per frame (acceleration)
```

**Units:**
- **Pixels per frame squared** — Simple, frame-dependent
- **Pixels per second squared** — Requires delta time, frame-independent

**Example:**
```pascal
// Frame-dependent (simple)
const GRAVITY = 1;  // 1 pixel per frame per frame

// Frame-independent (with delta time)
const GRAVITY = 980;  // 980 pixels per second squared
// Apply: vy := vy + Trunc(GRAVITY * dt);
```

---

## Applying Gravity

### Basic Gravity

**Apply gravity to velocity:**

```pascal
procedure ApplyGravity(entity: TEntityID);
var
  vx, vy: integer;
begin
  EntityGetVelocity(entity, vx, vy);
  
  // Add gravity to vertical velocity
  vy := vy + GRAVITY;
  
  EntitySetVelocity(entity, vx, vy);
end;
```

### Using PhysicsApplyGravity

**SuperPascal provides an intrinsic:**

```pascal
procedure PhysicsApplyGravity(id: TEntityID);
```

**Usage:**
```pascal
var
  player: TEntityID;
  
begin
  player := EntityCreate;
  
  while true do
  begin
    PhysicsApplyGravity(player);  // Apply gravity each frame
    PhysicsApplyVelocity(player); // Update position
    WaitVBlank;
  end;
end;
```

### Gravity System

**Apply gravity to all entities:**

```pascal
procedure GravitySystem;
var
  i: word;
begin
  for i := 0 to entityCount - 1 do
  begin
    if EntityValid(i) and 
       HasComponent(i, COMPONENT_VELOCITY) and
       HasComponent(i, COMPONENT_PHYSICS) then
    begin
      // Only apply if not on ground
      if not PhysicsOnGround[i] then
        PhysicsApplyGravity(i);
    end;
  end;
end;
```

---

## Falling

### Basic Falling

**Entity falls when gravity is applied:**

```pascal
var
  entity: TEntityID;
  
begin
  entity := EntityCreate;
  EntitySetPosition(entity, 160, 0);  // Top of screen
  EntitySetVelocity(entity, 0, 0);   // Start with no velocity
  
  while true do
  begin
    PhysicsApplyGravity(entity);  // Increase downward velocity
    PhysicsApplyVelocity(entity);  // Move down
    WaitVBlank;
  end;
end;
```

### Terminal Velocity

**Limit falling speed (terminal velocity):**

```pascal
const
  GRAVITY = 1;
  TERMINAL_VELOCITY = 10;  // Maximum falling speed

procedure ApplyGravityWithTerminalVelocity(entity: TEntityID);
var
  vx, vy: integer;
begin
  EntityGetVelocity(entity, vx, vy);
  
  // Apply gravity
  vy := vy + GRAVITY;
  
  // Clamp to terminal velocity
  if vy > TERMINAL_VELOCITY then
    vy := TERMINAL_VELOCITY;
  
  EntitySetVelocity(entity, vx, vy);
end;
```

### Falling Detection

**Detect when entity is falling:**

```pascal
function IsFalling(entity: TEntityID): boolean;
var
  vx, vy: integer;
begin
  EntityGetVelocity(entity, vx, vy);
  IsFalling := vy > 0;  // Positive Y velocity = falling
end;
```

---

## Jumping

### Basic Jump

**Jump by setting upward velocity:**

```pascal
const
  JUMP_VELOCITY = -10;  // Negative = upward

procedure Jump(entity: TEntityID);
var
  vx, vy: integer;
  isGrounded: boolean;
begin
  isGrounded := CheckGround(entity);
  
  if isGrounded then
  begin
    EntityGetVelocity(entity, vx, vy);
    vy := JUMP_VELOCITY;  // Set upward velocity
    EntitySetVelocity(entity, vx, vy);
  end;
end;
```

### Variable Jump Height

**Jump height based on how long button is held:**

```pascal
var
  jumpPressed: boolean;
  jumpHoldTime: byte;
  
const
  JUMP_VELOCITY = -10;
  MAX_JUMP_HOLD = 10;  // Frames

procedure UpdateJump(entity: TEntityID);
var
  input: word;
  vx, vy: integer;
  isGrounded: boolean;
begin
  input := ReadInput;
  isGrounded := CheckGround(entity);
  
  // Start jump
  if WasJustPressed(input, BTN_A) and isGrounded then
  begin
    jumpPressed := true;
    jumpHoldTime := 0;
    EntityGetVelocity(entity, vx, vy);
    vy := JUMP_VELOCITY;
    EntitySetVelocity(entity, vx, vy);
  end;
  
  // Continue jump (variable height)
  if jumpPressed and IsPressed(input, BTN_A) then
  begin
    jumpHoldTime := jumpHoldTime + 1;
    if jumpHoldTime < MAX_JUMP_HOLD then
    begin
      EntityGetVelocity(entity, vx, vy);
      if vy < 0 then  // Still moving up
        vy := vy - 1;  // Add more upward velocity
      EntitySetVelocity(entity, vx, vy);
    end;
  end;
  
  // Release jump button
  if WasJustReleased(input, BTN_A) then
    jumpPressed := false;
end;
```

### Double Jump

**Allow second jump in air:**

```pascal
var
  jumpCount: byte;
  
const
  MAX_JUMPS = 2;

procedure DoubleJump(entity: TEntityID);
var
  input: word;
  vx, vy: integer;
  isGrounded: boolean;
begin
  input := ReadInput;
  isGrounded := CheckGround(entity);
  
  // Reset jump count when grounded
  if isGrounded then
    jumpCount := 0;
  
  // Jump
  if WasJustPressed(input, BTN_A) and (jumpCount < MAX_JUMPS) then
  begin
    jumpCount := jumpCount + 1;
    EntityGetVelocity(entity, vx, vy);
    vy := JUMP_VELOCITY;
    EntitySetVelocity(entity, vx, vy);
  end;
end;
```

---

## Ground Detection

### Checking Ground

**Detect if entity is on ground:**

```pascal
function CheckGround(entity: TEntityID): boolean;
var
  x, y: integer;
  tileX, tileY: word;
  tile: byte;
begin
  EntityGetPosition(entity, x, y);
  
  // Check tile below entity
  tileX := x div TILE_SIZE;
  tileY := (y + ENTITY_HEIGHT) div TILE_SIZE;  // Bottom of entity
  
  tile := GetTile(level1, tileX, tileY);
  
  // Ground if solid tile or one-way platform
  CheckGround := (tpSolid in TileProperties[tile]) or
                 (tpOneWay in TileProperties[tile]);
end;
```

### Using PhysicsOnGround

**SuperPascal tracks ground state:**

```pascal
var
  PhysicsOnGround: array[0..MAX_ENTITIES-1] of boolean;

// Check ground and update flag
procedure UpdateGroundState(entity: TEntityID);
begin
  PhysicsOnGround[entity] := CheckGround(entity);
end;
```

### Ground Collision Response

**Stop falling when hitting ground:**

```pascal
procedure HandleGroundCollision(entity: TEntityID);
var
  x, y, vx, vy: integer;
  isGrounded: boolean;
begin
  EntityGetPosition(entity, x, y);
  EntityGetVelocity(entity, vx, vy);
  isGrounded := CheckGround(entity);
  
  if isGrounded and (vy > 0) then
  begin
    // Stop falling
    vy := 0;
    EntitySetVelocity(entity, vx, vy);
    
    // Snap to ground
    var groundY := ((y + ENTITY_HEIGHT) div TILE_SIZE) * TILE_SIZE;
    y := groundY - ENTITY_HEIGHT;
    EntitySetPosition(entity, x, y);
  end;
end;
```

---

## Complete Gravity System

**Putting it all together:**

```pascal
program GravityDemo;

const
  GRAVITY = 1;
  JUMP_VELOCITY = -10;
  TERMINAL_VELOCITY = 10;

var
  player: TEntityID;

procedure InitPlayer;
begin
  player := EntityCreate;
  EntitySetPosition(player, 160, 0);
  EntitySetVelocity(player, 0, 0);
end;

procedure UpdatePlayerPhysics;
var
  input: word;
  vx, vy: integer;
  isGrounded: boolean;
begin
  input := ReadInput;
  EntityGetVelocity(player, vx, vy);
  isGrounded := CheckGround(player);
  
  // Handle input
  if (input and BTN_LEFT) <> 0 then
    vx := -2
  else if (input and BTN_RIGHT) <> 0 then
    vx := 2
  else
    vx := 0;
  
  // Jump
  if WasJustPressed(input, BTN_A) and isGrounded then
  begin
    vy := JUMP_VELOCITY;
  end;
  
  // Apply gravity (if not on ground)
  if not isGrounded then
  begin
    vy := vy + GRAVITY;
    
    // Clamp to terminal velocity
    if vy > TERMINAL_VELOCITY then
      vy := TERMINAL_VELOCITY;
  end;
  
  EntitySetVelocity(player, vx, vy);
  
  // Apply velocity
  PhysicsApplyVelocity(player);
  
  // Handle ground collision
  HandleGroundCollision(player);
end;

begin
  InitGraphics;
  InitPlayer;
  
  while true do
  begin
    UpdatePlayerPhysics;
    WaitVBlank;
    RenderGame;
  end;
end.
```

---

## Best Practices

### 1. Apply Gravity Before Movement

**Apply gravity, then update position:**

```pascal
// ✅ GOOD: Gravity then movement
PhysicsApplyGravity(player);
PhysicsApplyVelocity(player);

// ❌ BAD: Movement then gravity
PhysicsApplyVelocity(player);
PhysicsApplyGravity(player);  // Too late
```

### 2. Check Ground Before Jumping

**Only allow jump when grounded:**

```pascal
// ✅ GOOD: Check ground
if isGrounded and WasJustPressed(BTN_A) then
  Jump;

// ❌ BAD: No ground check
if WasJustPressed(BTN_A) then
  Jump;  // Can jump in air
```

### 3. Use Terminal Velocity

**Limit falling speed:**

```pascal
// ✅ GOOD: Terminal velocity
if vy > TERMINAL_VELOCITY then
  vy := TERMINAL_VELOCITY;

// ❌ BAD: No limit
// Velocity grows unbounded
```

### 4. Snap to Ground

**Snap entity to ground when landing:**

```pascal
// ✅ GOOD: Snap to ground
if isGrounded then
  y := groundY - ENTITY_HEIGHT;

// ❌ BAD: Float above ground
// Entity may be slightly above ground
```

### 5. Reset Jump on Ground

**Reset jump count when landing:**

```pascal
// ✅ GOOD: Reset on ground
if isGrounded then
  jumpCount := 0;

// ❌ BAD: Never reset
// Can't jump again after landing
```

---

## Exercises

### Exercise 1: Basic Gravity

Write a program that:
1. Creates an entity at top of screen
2. Applies gravity each frame
3. Entity falls down
4. Stops when hitting ground

### Exercise 2: Jumping

Write a program that:
1. Creates a player on ground
2. Applies gravity
3. Allows jumping with button press
4. Player can jump and fall

### Exercise 3: Variable Jump

Write a program that:
1. Implements variable jump height
2. Jump height depends on button hold time
3. Short press = short jump
4. Long press = high jump

### Exercise 4: Complete Physics

Write a program that:
1. Implements gravity with terminal velocity
2. Handles jumping and ground detection
3. Applies friction when on ground
4. Smooth movement and physics

---

**Previous Section:** [Position and Velocity](./01_PositionAndVelocity.md)  
**Next Section:** [Collision Resolution](./03_CollisionResolution.md)  
**Language Specification:** See [Game Engine Intrinsics](../../languageSpecification/intrinsicsAndDirectives/10_GameEngineIntrinsics.md)  
**Last Updated:** 2025-01-XX

