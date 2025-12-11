# Systems as Loops

**Part of:** [Chapter 16: Entity Component System](./README.md)

---

## Introduction

Systems are **loops that process components**. They contain all the behavior/logic in an ECS architecture. This section teaches you how systems work, common system patterns, and how to schedule systems.

**Key concepts:**
- **Systems are loops** — Iterate through entities and process components
- **System patterns** — Movement, rendering, collision, etc.
- **System scheduling** — Order systems run
- **System independence** — Systems can run in parallel
- **Component queries** — Finding entities with specific components

---

## What are Systems?

### Systems are Behavior

**Systems contain all behavior/logic:**

```pascal
// Components are data (no behavior)
type
  TPosition = record
    X, Y: integer;
  end;
  
  TVelocity = record
    VX, VY: integer;
  end;

// Systems are behavior (process components)
procedure MovementSystem;
var
  i: word;
  x, y, vx, vy: integer;
begin
  // Loop through all entities
  for i := 0 to entityCount - 1 do
  begin
    // Check if entity has Position and Velocity
    if HasComponent(i, COMPONENT_POSITION or COMPONENT_VELOCITY) then
    begin
      // Get components
      EntityGetPosition(i, x, y);
      EntityGetVelocity(i, vx, vy);
      
      // Update position based on velocity
      EntitySetPosition(i, x + vx, y + vy);
    end;
  end;
end;
```

### Systems vs Components

**Separation of concerns:**

- **Components** = Data (what an entity has)
- **Systems** = Behavior (what happens to entities)

**Example:**
```pascal
// Component: Data
type
  TPosition = record
    X, Y: integer;
  end;

// System: Behavior
procedure MovementSystem;
begin
  // Process all entities with Position component
  // Update their positions
end;
```

---

## Common System Patterns

### Movement System

**Updates position based on velocity:**

```pascal
procedure MovementSystem;
var
  i: word;
  x, y, vx, vy: integer;
begin
  for i := 0 to entityCount - 1 do
  begin
    if EntityValid(i) and 
       HasComponent(i, COMPONENT_POSITION) and
       HasComponent(i, COMPONENT_VELOCITY) then
    begin
      EntityGetPosition(i, x, y);
      EntityGetVelocity(i, vx, vy);
      
      // Apply velocity to position
      EntitySetPosition(i, x + vx, y + vy);
    end;
  end;
end;
```

### Physics System

**Applies physics (gravity, friction, etc.):**

```pascal
const
  GRAVITY = 1;

procedure PhysicsSystem;
var
  i: word;
  vx, vy: integer;
begin
  for i := 0 to entityCount - 1 do
  begin
    if EntityValid(i) and HasComponent(i, COMPONENT_VELOCITY) then
    begin
      EntityGetVelocity(i, vx, vy);
      
      // Apply gravity
      vy := vy + GRAVITY;
      
      // Apply friction (if on ground)
      if IsGrounded(i) then
        vx := Trunc(vx * 0.9);
      
      EntitySetVelocity(i, vx, vy);
    end;
  end;
end;
```

### Rendering System

**Draws entities with sprite components:**

```pascal
procedure RenderingSystem;
var
  i: word;
  x, y: integer;
  spriteID, tileID, palette: byte;
begin
  for i := 0 to entityCount - 1 do
  begin
    if EntityValid(i) and 
       HasComponent(i, COMPONENT_POSITION) and
       HasComponent(i, COMPONENT_SPRITE) then
    begin
      EntityGetPosition(i, x, y);
      EntityGetSprite(i, spriteID, tileID, palette);
      
      // Update sprite position
      ZVB_SpriteSetFull(spriteID, x, y, tileID, 0);
    end;
  end;
end;
```

### Collision System

**Detects collisions between entities:**

```pascal
procedure CollisionSystem;
var
  i, j: word;
  x1, y1, x2, y2: integer;
begin
  for i := 0 to entityCount - 1 do
  begin
    if EntityValid(i) and HasComponent(i, COMPONENT_COLLIDER) then
    begin
      EntityGetPosition(i, x1, y1);
      
      // Check collision with other entities
      for j := i + 1 to entityCount - 1 do
      begin
        if EntityValid(j) and HasComponent(j, COMPONENT_COLLIDER) then
        begin
          EntityGetPosition(j, x2, y2);
          
          if CollisionCheckAABB(x1, y1, 16, 16, x2, y2, 16, 16) then
          begin
            OnCollision(i, j);
          end;
        end;
      end;
    end;
  end;
end;
```

### Input System

**Processes input and updates entities:**

```pascal
procedure InputSystem;
var
  input: word;
  player: TEntityID;
  vx, vy: integer;
begin
  input := ReadInput;
  player := GetPlayerEntity;
  
  if EntityValid(player) then
  begin
    EntityGetVelocity(player, vx, vy);
    
    // Process input
    if (input and BTN_LEFT) <> 0 then
      vx := -2
    else if (input and BTN_RIGHT) <> 0 then
      vx := 2
    else
      vx := 0;
    
    if (input and BTN_UP) <> 0 then
      vy := -5;
    
    EntitySetVelocity(player, vx, vy);
  end;
end;
```

---

## System Scheduling

### Update Order

**Systems must run in the correct order:**

```pascal
procedure UpdateGame;
begin
  // 1. Input first (affects everything)
  InputSystem;
  
  // 2. Physics before movement (apply forces)
  PhysicsSystem;
  
  // 3. Movement (apply velocity to position)
  MovementSystem;
  
  // 4. Collision after movement (check after all movement)
  CollisionSystem;
  
  // 5. Rendering last (draw everything)
  WaitVBlank;
  RenderingSystem;
end;
```

### Why Order Matters

**Example of wrong order:**

```pascal
// ❌ BAD: Wrong order
procedure UpdateGame;
begin
  CollisionSystem;  // Check collisions before movement!
  MovementSystem;   // Entities move after collision check
  // Result: Collisions detected at wrong positions
end;

// ✅ GOOD: Correct order
procedure UpdateGame;
begin
  MovementSystem;   // Move entities first
  CollisionSystem;  // Then check collisions
  // Result: Collisions detected at correct positions
end;
```

### System Dependencies

**Some systems depend on others:**

```pascal
// InputSystem → affects Velocity
// PhysicsSystem → affects Velocity
// MovementSystem → uses Velocity to update Position
// CollisionSystem → uses Position to check collisions
// RenderingSystem → uses Position to draw

// Dependency chain:
// Input → Physics → Movement → Collision → Rendering
```

---

## Efficient System Implementation

### Component Queries

**Find entities with specific components:**

```pascal
type
  TEntityList = record
    Entities: array[0..255] of TEntityID;
    Count: word;
  end;

function QueryEntities(components: TComponentFlags): TEntityList;
var
  i: word;
  list: TEntityList;
begin
  list.Count := 0;
  for i := 0 to entityCount - 1 do
  begin
    if EntityValid(i) and 
       (ComponentFlags[i] and components) = components then
    begin
      list.Entities[list.Count] := i;
      list.Count := list.Count + 1;
    end;
  end;
  QueryEntities := list;
end;

// Usage
procedure MovementSystem;
var
  entities: TEntityList;
  i: word;
  x, y, vx, vy: integer;
begin
  // Query entities with Position and Velocity
  entities := QueryEntities(COMPONENT_POSITION or COMPONENT_VELOCITY);
  
  for i := 0 to entities.Count - 1 do
  begin
    EntityGetPosition(entities.Entities[i], x, y);
    EntityGetVelocity(entities.Entities[i], vx, vy);
    EntitySetPosition(entities.Entities[i], x + vx, y + vy);
  end;
end;
```

### Caching Entity Lists

**Cache entity lists to avoid repeated queries:**

```pascal
var
  movingEntities: TEntityList;
  renderingEntities: TEntityList;
  dirty: boolean;

procedure UpdateEntityLists;
begin
  if dirty then
  begin
    movingEntities := QueryEntities(COMPONENT_POSITION or COMPONENT_VELOCITY);
    renderingEntities := QueryEntities(COMPONENT_POSITION or COMPONENT_SPRITE);
    dirty := false;
  end;
end;

procedure MovementSystem;
var
  i: word;
  x, y, vx, vy: integer;
begin
  UpdateEntityLists;  // Update if needed
  
  for i := 0 to movingEntities.Count - 1 do
  begin
    EntityGetPosition(movingEntities.Entities[i], x, y);
    EntityGetVelocity(movingEntities.Entities[i], vx, vy);
    EntitySetPosition(movingEntities.Entities[i], x + vx, y + vy);
  end;
end;
```

---

## Complete ECS Example

**Putting it all together:**

```pascal
program ECSDemo;

var
  player: TEntityID;
  enemy: TEntityID;
  bullet: TEntityID;

procedure InitGame;
begin
  // Create player
  player := EntityCreate;
  EntitySetPosition(player, 160, 120);
  EntitySetVelocity(player, 0, 0);
  EntitySetSprite(player, 0, 42, 1);
  
  // Create enemy
  enemy := EntityCreate;
  EntitySetPosition(enemy, 300, 100);
  EntitySetVelocity(enemy, -1, 0);
  EntitySetSprite(enemy, 1, 10, 2);
  EntitySetColliderAABB(enemy, 0, 0, 16, 16, true);
end;

procedure InputSystem;
var
  input: word;
  vx, vy: integer;
begin
  input := ReadInput;
  
  if EntityValid(player) then
  begin
    EntityGetVelocity(player, vx, vy);
    
    if (input and BTN_LEFT) <> 0 then
      vx := -2
    else if (input and BTN_RIGHT) <> 0 then
      vx := 2
    else
      vx := 0;
    
    if (input and BTN_UP) <> 0 then
      vy := -5;
    
    EntitySetVelocity(player, vx, vy);
  end;
end;

procedure PhysicsSystem;
var
  i: word;
  vx, vy: integer;
begin
  for i := 0 to entityCount - 1 do
  begin
    if EntityValid(i) and HasComponent(i, COMPONENT_VELOCITY) then
    begin
      EntityGetVelocity(i, vx, vy);
      
      // Apply gravity
      vy := vy + 1;
      
      EntitySetVelocity(i, vx, vy);
    end;
  end;
end;

procedure MovementSystem;
var
  i: word;
  x, y, vx, vy: integer;
begin
  for i := 0 to entityCount - 1 do
  begin
    if EntityValid(i) and 
       HasComponent(i, COMPONENT_POSITION) and
       HasComponent(i, COMPONENT_VELOCITY) then
    begin
      EntityGetPosition(i, x, y);
      EntityGetVelocity(i, vx, vy);
      EntitySetPosition(i, x + vx, y + vy);
    end;
  end;
end;

procedure CollisionSystem;
var
  i, j: word;
  x1, y1, x2, y2: integer;
begin
  for i := 0 to entityCount - 1 do
  begin
    if EntityValid(i) and HasComponent(i, COMPONENT_COLLIDER) then
    begin
      EntityGetPosition(i, x1, y1);
      
      for j := i + 1 to entityCount - 1 do
      begin
        if EntityValid(j) and HasComponent(j, COMPONENT_COLLIDER) then
        begin
          EntityGetPosition(j, x2, y2);
          
          if CollisionCheckAABB(x1, y1, 16, 16, x2, y2, 16, 16) then
          begin
            WriteLn('Collision: ', i, ' and ', j);
          end;
        end;
      end;
    end;
  end;
end;

procedure RenderingSystem;
var
  i: word;
  x, y: integer;
  spriteID, tileID, palette: byte;
begin
  for i := 0 to entityCount - 1 do
  begin
    if EntityValid(i) and 
       HasComponent(i, COMPONENT_POSITION) and
       HasComponent(i, COMPONENT_SPRITE) then
    begin
      EntityGetPosition(i, x, y);
      EntityGetSprite(i, spriteID, tileID, palette);
      ZVB_SpriteSetFull(spriteID, x, y, tileID, 0);
    end;
  end;
end;

procedure UpdateGame;
begin
  InputSystem;
  PhysicsSystem;
  MovementSystem;
  CollisionSystem;
end;

procedure RenderGame;
begin
  WaitVBlank;
  RenderingSystem;
end;

begin
  InitGame;
  InitGraphics;
  
  while true do
  begin
    UpdateGame;
    RenderGame;
  end;
end.
```

---

## Best Practices

### 1. Systems Process Components

**Systems operate on components, not entities directly:**

```pascal
// ✅ GOOD: Process components
procedure MovementSystem;
begin
  for i := 0 to entityCount - 1 do
  begin
    if HasComponent(i, COMPONENT_POSITION or COMPONENT_VELOCITY) then
      // Process
  end;
end;

// ❌ BAD: Process entities directly
procedure MovementSystem;
begin
  for i := 0 to entityCount - 1 do
  begin
    if IsPlayer(i) then  // Entity-specific logic
      // Process
  end;
end;
```

### 2. Systems are Independent

**Systems should not depend on other systems:**

```pascal
// ✅ GOOD: Independent systems
procedure MovementSystem;
begin
  // Only uses Position and Velocity components
end;

procedure CollisionSystem;
begin
  // Only uses Position and Collider components
end;

// ❌ BAD: System dependencies
procedure MovementSystem;
begin
  // ...
  CollisionSystem;  // Calling another system!
end;
```

### 3. Query Efficiently

**Cache entity lists to avoid repeated queries:**

```pascal
// ✅ GOOD: Cache queries
var movingEntities: TEntityList;
// Query once, use many times

// ❌ BAD: Query every time
procedure MovementSystem;
begin
  entities := QueryEntities(...);  // Query every frame
end;
```

### 4. Process in Correct Order

**Run systems in dependency order:**

```pascal
// ✅ GOOD: Correct order
InputSystem;
PhysicsSystem;
MovementSystem;
CollisionSystem;
RenderingSystem;

// ❌ BAD: Wrong order
CollisionSystem;  // Before movement!
MovementSystem;
```

### 5. Keep Systems Focused

**Each system should do one thing:**

```pascal
// ✅ GOOD: Focused system
procedure MovementSystem;
begin
  // Only handles movement
end;

// ❌ BAD: Mixed concerns
procedure MovementAndCollisionSystem;
begin
  // Movement AND collision (should be separate)
end;
```

---

## Exercises

### Exercise 1: Basic System

Write a program that:
1. Creates entities with Position and Velocity components
2. Implements a MovementSystem that updates positions
3. Runs the system each frame
4. Displays entity positions

### Exercise 2: Multiple Systems

Write a program that:
1. Implements InputSystem (reads input, updates velocity)
2. Implements MovementSystem (updates position)
3. Implements RenderingSystem (draws entities)
4. Schedules systems in correct order

### Exercise 3: Collision System

Write a program that:
1. Creates entities with Position and Collider components
2. Implements a CollisionSystem that detects collisions
3. Responds to collisions (bounce, destroy, etc.)
4. Runs after MovementSystem

### Exercise 4: Complete ECS Game

Write a program that:
1. Creates player, enemies, and bullets
2. Implements all systems (Input, Physics, Movement, Collision, Rendering)
3. Schedules systems correctly
4. Manages entity lifecycle (create/destroy)
5. Runs a complete game loop

---

**Previous Section:** [Components as Records](./02_ComponentsAsRecords.md)  
**Next Chapter:** [Chapter 17: Tilemaps and Level Design](../17_TilemapsAndLevelDesign/README.md)  
**Language Specification:** See [Game Engine Concepts](../../languageSpecification/09_GameEngine_Concepts.md)  
**Last Updated:** 2025-01-XX

