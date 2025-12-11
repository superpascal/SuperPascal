# ECS Implementation - Iteration 5: System Registration and Execution

**Status:** ⚠️ Pending  
**Part of:** SuperPascal ECS Library Implementation

---

## Goal

Implement system registration and execution framework.

---

## Implementation

### System Type and Functions

```pascal
// Add to ECS unit interface section

// System type
type
  TSystem = procedure(var world: TWorld);

// System management
procedure SystemRegister(var world: TWorld; system: TSystem);
procedure SystemRunAll(var world: TWorld);
function SystemGetCount(var world: TWorld): Word;
```

### Update World Structure

```pascal
// Update TWorld record in interface section
type
  TWorld = record
    // ... existing fields ...
    
    // Systems
    Systems: array[0..MAX_SYSTEMS-1] of TSystem;
    SystemCount: Word;
  end;
```

### Implementation

```pascal
// Add to ECS unit implementation section

// Register system
procedure SystemRegister(var world: TWorld; system: TSystem);
begin
  // Check if we have room for more systems
  if world.SystemCount >= MAX_SYSTEMS then
    Exit;
  
  // Add system to array
  world.Systems[world.SystemCount] := system;
  world.SystemCount := world.SystemCount + 1;
end;

// Run all registered systems
procedure SystemRunAll(var world: TWorld);
var
  i: Word;
begin
  // Execute all systems in registration order
  for i := 0 to world.SystemCount - 1 do
  begin
    if world.Systems[i] <> nil then
      world.Systems[i](world);
  end;
end;

// Get system count
function SystemGetCount(var world: TWorld): Word;
begin
  Result := world.SystemCount;
end;
```

### Update WorldInit

```pascal
// Update WorldInit procedure
procedure WorldInit(var world: TWorld);
var
  i: Word;
begin
  // ... existing initialization ...
  
  // Initialize systems
  world.SystemCount := 0;
  for i := 0 to MAX_SYSTEMS - 1 do
    world.Systems[i] := nil;
end;
```

---

## Example Systems

### Movement System

```pascal
// Example system implementation
procedure MovementSystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
  pos: ^TPosition;
  vel: ^TVelocity;
begin
  query := QueryCreate(world, [COMPONENT_POSITION, COMPONENT_VELOCITY]);
  while QueryNext(query, entity) do
  begin
    pos := ComponentGetPosition(world, entity);
    vel := ComponentGetVelocity(world, entity);
    
    if (pos <> nil) and (vel <> nil) then
    begin
      pos^.X := pos^.X + vel^.DX;
      pos^.Y := pos^.Y + vel^.DY;
    end;
  end;
end;
```

### Render System (Placeholder)

```pascal
procedure RenderSystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
  pos: ^TPosition;
  sprite: ^TSprite;
begin
  query := QueryCreate(world, [COMPONENT_POSITION, COMPONENT_SPRITE]);
  while QueryNext(query, entity) do
  begin
    pos := ComponentGetPosition(world, entity);
    sprite := ComponentGetSprite(world, entity);
    
    if (pos <> nil) and (sprite <> nil) and sprite^.Visible then
    begin
      // Platform-specific rendering would go here
      // For now, just a placeholder
    end;
  end;
end;
```

---

## Testing

### Test 1: System Registration and Execution

```pascal
program TestECS_Iteration5;
uses ECS;

var
  world: TWorld;
  entity: TEntity;
  pos: TPosition;
  vel: TVelocity;
  systemExecuted: Boolean;

// Test system that sets a flag
procedure TestSystem(var world: TWorld);
begin
  systemExecuted := True;
end;

begin
  WorldInit(world);
  systemExecuted := False;
  
  // Register system
  SystemRegister(world, TestSystem);
  
  Assert(SystemGetCount(world) = 1, 'Should have 1 system registered');
  
  // Run systems
  SystemRunAll(world);
  
  Assert(systemExecuted, 'System should have been executed');
  
  WriteLn('Iteration 5: System registration test passed');
  
  WorldCleanup(world);
end.
```

### Test 2: Movement System Integration

```pascal
program TestECS_Iteration5_Movement;
uses ECS;

var
  world: TWorld;
  entity: TEntity;
  pos: TPosition;
  vel: TVelocity;
  posPtr: ^TPosition;
begin
  WorldInit(world);
  
  // Create entity with Position and Velocity
  entity := EntityCreate(world);
  ComponentAdd(world, entity, COMPONENT_POSITION);
  ComponentAdd(world, entity, COMPONENT_VELOCITY);
  
  // Set initial position and velocity
  pos.X := 100;
  pos.Y := 200;
  ComponentSetPosition(world, entity, pos);
  
  vel.DX := 5;
  vel.DY := -3;
  ComponentSetVelocity(world, entity, vel);
  
  // Register and run movement system
  SystemRegister(world, MovementSystem);
  SystemRunAll(world);
  
  // Verify position was updated
  posPtr := ComponentGetPosition(world, entity);
  Assert(posPtr <> nil, 'Position should exist');
  Assert(posPtr^.X = 105, 'X should be 105 (100 + 5)');
  Assert(posPtr^.Y = 197, 'Y should be 197 (200 - 3)');
  
  WriteLn('Iteration 5: Movement system integration test passed');
  
  WorldCleanup(world);
end.
```

---

## Status

⚠️ **Pending**

**What's Done:**
- SystemRegister procedure implemented
- SystemRunAll procedure implemented
- SystemGetCount function implemented
- World structure updated with systems array
- Example systems provided (MovementSystem, RenderSystem)

**What's Next:**
- Iteration 6: Integration with physics algorithms (refactor ParticleSystem, GravitySystem)

---

**Previous:** [Iteration 4: Query System](./04_Iteration4_QuerySystem.md)  
**Next:** [Iteration 6: Algorithm Integration](./06_Iteration6_AlgorithmIntegration.md)

