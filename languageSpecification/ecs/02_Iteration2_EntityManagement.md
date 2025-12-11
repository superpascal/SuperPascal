# ECS Implementation - Iteration 2: Entity Management

**Status:** ⚠️ Pending  
**Part of:** SuperPascal ECS Library Implementation

---

## Goal

Implement entity creation, destruction, and validation.

---

## Implementation

### Entity Management Functions

```pascal
// Add to ECS unit interface section

// Entity management
function EntityCreate(var world: TWorld): TEntity;
procedure EntityDestroy(var world: TWorld; entity: TEntity);
function EntityIsValid(var world: TWorld; entity: TEntity): Boolean;
```

### Implementation

```pascal
// Add to ECS unit implementation section

// Create new entity
function EntityCreate(var world: TWorld): TEntity;
var
  entity: TEntity;
begin
  // Check if we have free entities to reuse
  if world.FreeCount > 0 then
  begin
    // Reuse free entity
    world.FreeCount := world.FreeCount - 1;
    entity := world.FreeEntities[world.FreeCount];
  end
  else
  begin
    // Create new entity
    if world.EntityCount >= MAX_ENTITIES then
    begin
      // No more entities available
      Result := ENTITY_NULL;
      Exit;
    end;
    
    entity := world.EntityCount;
    world.EntityCount := world.EntityCount + 1;
  end;
  
  // Initialize entity
  world.EntityValid[entity] := True;
  world.EntityComponents[entity] := [];
  
  Result := entity;
end;

// Destroy entity
procedure EntityDestroy(var world: TWorld; entity: TEntity);
begin
  // Validate entity
  if not EntityIsValid(world, entity) then
    Exit;
  
  // Clear component data
  world.Positions[entity].X := 0;
  world.Positions[entity].Y := 0;
  world.Velocities[entity].DX := 0;
  world.Velocities[entity].DY := 0;
  world.Sprites[entity].SpriteID := 0;
  world.Sprites[entity].TileID := 0;
  world.Sprites[entity].Palette := 0;
  world.Sprites[entity].Visible := False;
  
  // Clear component presence
  world.EntityComponents[entity] := [];
  
  // Mark entity as invalid
  world.EntityValid[entity] := False;
  
  // Add to free list for reuse
  if world.FreeCount < MAX_ENTITIES then
  begin
    world.FreeEntities[world.FreeCount] := entity;
    world.FreeCount := world.FreeCount + 1;
  end;
end;

// Check if entity is valid
function EntityIsValid(var world: TWorld; entity: TEntity): Boolean;
begin
  // Check bounds
  if entity >= MAX_ENTITIES then
  begin
    Result := False;
    Exit;
  end;
  
  // Check validity flag
  Result := world.EntityValid[entity];
end;
```

---

## Testing

### Test 1: Entity Creation

```pascal
program TestECS_Iteration2;
uses ECS;

var
  world: TWorld;
  entity1, entity2, entity3: TEntity;
begin
  WorldInit(world);
  
  // Create entities
  entity1 := EntityCreate(world);
  entity2 := EntityCreate(world);
  entity3 := EntityCreate(world);
  
  // Verify entities are valid
  Assert(entity1 <> ENTITY_NULL, 'Entity1 should be valid');
  Assert(entity2 <> ENTITY_NULL, 'Entity2 should be valid');
  Assert(entity3 <> ENTITY_NULL, 'Entity3 should be valid');
  Assert(entity1 <> entity2, 'Entities should be unique');
  Assert(entity2 <> entity3, 'Entities should be unique');
  Assert(entity1 <> entity3, 'Entities should be unique');
  
  // Verify entity validity
  Assert(EntityIsValid(world, entity1), 'Entity1 should be valid');
  Assert(EntityIsValid(world, entity2), 'Entity2 should be valid');
  Assert(EntityIsValid(world, entity3), 'Entity3 should be valid');
  
  WriteLn('Iteration 2: Entity creation test passed');
  
  WorldCleanup(world);
end.
```

### Test 2: Entity Destruction and Reuse

```pascal
program TestECS_Iteration2_Destroy;
uses ECS;

var
  world: TWorld;
  entity1, entity2, entity3: TEntity;
  reused: TEntity;
begin
  WorldInit(world);
  
  // Create entities
  entity1 := EntityCreate(world);
  entity2 := EntityCreate(world);
  entity3 := EntityCreate(world);
  
  // Destroy entity2
  EntityDestroy(world, entity2);
  
  // Verify entity2 is invalid
  Assert(not EntityIsValid(world, entity2), 'Entity2 should be invalid');
  
  // Verify other entities are still valid
  Assert(EntityIsValid(world, entity1), 'Entity1 should still be valid');
  Assert(EntityIsValid(world, entity3), 'Entity3 should still be valid');
  
  // Create new entity (should reuse entity2's ID)
  reused := EntityCreate(world);
  Assert(reused = entity2, 'Should reuse destroyed entity ID');
  Assert(EntityIsValid(world, reused), 'Reused entity should be valid');
  
  WriteLn('Iteration 2: Entity destruction and reuse test passed');
  
  WorldCleanup(world);
end.
```

---

## Status

⚠️ **Pending**

**What's Done:**
- EntityCreate function implemented
- EntityDestroy function implemented
- EntityIsValid function implemented
- Entity pooling (free list) implemented

**What's Next:**
- Iteration 3: Component management (ComponentAdd, ComponentRemove, ComponentHas, accessors)

---

**Previous:** [Iteration 1: Core Types](./01_Iteration1_CoreTypes.md)  
**Next:** [Iteration 3: Component Management](./03_Iteration3_ComponentManagement.md)

