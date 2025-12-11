# ECS Implementation - Iteration 3: Component Management

**Status:** ⚠️ Pending  
**Part of:** SuperPascal ECS Library Implementation

---

## Goal

Implement component addition, removal, presence checking, and data accessors.

---

## Implementation

### Component Management Functions

```pascal
// Add to ECS unit interface section

// Component management
procedure ComponentAdd(var world: TWorld; entity: TEntity; componentID: TComponentID);
procedure ComponentRemove(var world: TWorld; entity: TEntity; componentID: TComponentID);
function ComponentHas(var world: TWorld; entity: TEntity; componentID: TComponentID): Boolean;

// Component data accessors
function ComponentGetPosition(var world: TWorld; entity: TEntity): ^TPosition;
function ComponentGetVelocity(var world: TWorld; entity: TEntity): ^TVelocity;
function ComponentGetSprite(var world: TWorld; entity: TEntity): ^TSprite;

procedure ComponentSetPosition(var world: TWorld; entity: TEntity; const pos: TPosition);
procedure ComponentSetVelocity(var world: TWorld; entity: TEntity; const vel: TVelocity);
procedure ComponentSetSprite(var world: TWorld; entity: TEntity; const sprite: TSprite);
```

### Implementation

```pascal
// Add to ECS unit implementation section

// Helper function to set component bit in mask
procedure SetComponentBit(var mask: TComponentMask; componentID: TComponentID);
begin
  mask := mask + [componentID];
end;

// Helper function to clear component bit in mask
procedure ClearComponentBit(var mask: TComponentMask; componentID: TComponentID);
begin
  mask := mask - [componentID];
end;

// Helper function to check component bit in mask
function HasComponentBit(const mask: TComponentMask; componentID: TComponentID): Boolean;
begin
  Result := componentID in mask;
end;

// Add component to entity
procedure ComponentAdd(var world: TWorld; entity: TEntity; componentID: TComponentID);
begin
  // Validate entity
  if not EntityIsValid(world, entity) then
    Exit;
  
  // Validate component ID
  if componentID >= MAX_COMPONENTS then
    Exit;
  
  // Add component to mask
  SetComponentBit(world.EntityComponents[entity], componentID);
  
  // Initialize component data based on type
  case componentID of
    COMPONENT_POSITION:
    begin
      world.Positions[entity].X := 0;
      world.Positions[entity].Y := 0;
    end;
    COMPONENT_VELOCITY:
    begin
      world.Velocities[entity].DX := 0;
      world.Velocities[entity].DY := 0;
    end;
    COMPONENT_SPRITE:
    begin
      world.Sprites[entity].SpriteID := 0;
      world.Sprites[entity].TileID := 0;
      world.Sprites[entity].Palette := 0;
      world.Sprites[entity].Visible := False;
    end;
  end;
end;

// Remove component from entity
procedure ComponentRemove(var world: TWorld; entity: TEntity; componentID: TComponentID);
begin
  // Validate entity
  if not EntityIsValid(world, entity) then
    Exit;
  
  // Validate component ID
  if componentID >= MAX_COMPONENTS then
    Exit;
  
  // Remove component from mask
  ClearComponentBit(world.EntityComponents[entity], componentID);
  
  // Clear component data based on type
  case componentID of
    COMPONENT_POSITION:
    begin
      world.Positions[entity].X := 0;
      world.Positions[entity].Y := 0;
    end;
    COMPONENT_VELOCITY:
    begin
      world.Velocities[entity].DX := 0;
      world.Velocities[entity].DY := 0;
    end;
    COMPONENT_SPRITE:
    begin
      world.Sprites[entity].SpriteID := 0;
      world.Sprites[entity].TileID := 0;
      world.Sprites[entity].Palette := 0;
      world.Sprites[entity].Visible := False;
    end;
  end;
end;

// Check if entity has component
function ComponentHas(var world: TWorld; entity: TEntity; componentID: TComponentID): Boolean;
begin
  // Validate entity
  if not EntityIsValid(world, entity) then
  begin
    Result := False;
    Exit;
  end;
  
  // Validate component ID
  if componentID >= MAX_COMPONENTS then
  begin
    Result := False;
    Exit;
  end;
  
  // Check component mask
  Result := HasComponentBit(world.EntityComponents[entity], componentID);
end;

// Get component data (Position)
function ComponentGetPosition(var world: TWorld; entity: TEntity): ^TPosition;
begin
  // Validate entity and component
  if not ComponentHas(world, entity, COMPONENT_POSITION) then
  begin
    Result := nil;
    Exit;
  end;
  
  Result := @world.Positions[entity];
end;

// Get component data (Velocity)
function ComponentGetVelocity(var world: TWorld; entity: TEntity): ^TVelocity;
begin
  // Validate entity and component
  if not ComponentHas(world, entity, COMPONENT_VELOCITY) then
  begin
    Result := nil;
    Exit;
  end;
  
  Result := @world.Velocities[entity];
end;

// Get component data (Sprite)
function ComponentGetSprite(var world: TWorld; entity: TEntity): ^TSprite;
begin
  // Validate entity and component
  if not ComponentHas(world, entity, COMPONENT_SPRITE) then
  begin
    Result := nil;
    Exit;
  end;
  
  Result := @world.Sprites[entity];
end;

// Set component data (Position)
procedure ComponentSetPosition(var world: TWorld; entity: TEntity; const pos: TPosition);
var
  p: ^TPosition;
begin
  p := ComponentGetPosition(world, entity);
  if p <> nil then
    p^ := pos;
end;

// Set component data (Velocity)
procedure ComponentSetVelocity(var world: TWorld; entity: TEntity; const vel: TVelocity);
var
  v: ^TVelocity;
begin
  v := ComponentGetVelocity(world, entity);
  if v <> nil then
    v^ := vel;
end;

// Set component data (Sprite)
procedure ComponentSetSprite(var world: TWorld; entity: TEntity; const sprite: TSprite);
var
  s: ^TSprite;
begin
  s := ComponentGetSprite(world, entity);
  if s <> nil then
    s^ := sprite;
end;
```

---

## Testing

### Test 1: Component Addition and Presence

```pascal
program TestECS_Iteration3;
uses ECS;

var
  world: TWorld;
  entity: TEntity;
  pos: TPosition;
begin
  WorldInit(world);
  
  // Create entity
  entity := EntityCreate(world);
  
  // Add position component
  ComponentAdd(world, entity, COMPONENT_POSITION);
  
  // Verify component presence
  Assert(ComponentHas(world, entity, COMPONENT_POSITION), 'Entity should have Position component');
  Assert(not ComponentHas(world, entity, COMPONENT_VELOCITY), 'Entity should not have Velocity component');
  
  // Set and get position
  pos.X := 100;
  pos.Y := 200;
  ComponentSetPosition(world, entity, pos);
  
  pos := ComponentGetPosition(world, entity)^;
  Assert(pos.X = 100, 'Position X should be 100');
  Assert(pos.Y = 200, 'Position Y should be 200');
  
  WriteLn('Iteration 3: Component management test passed');
  
  WorldCleanup(world);
end.
```

### Test 2: Component Removal

```pascal
program TestECS_Iteration3_Remove;
uses ECS;

var
  world: TWorld;
  entity: TEntity;
begin
  WorldInit(world);
  
  entity := EntityCreate(world);
  
  // Add components
  ComponentAdd(world, entity, COMPONENT_POSITION);
  ComponentAdd(world, entity, COMPONENT_VELOCITY);
  
  // Verify both components
  Assert(ComponentHas(world, entity, COMPONENT_POSITION), 'Should have Position');
  Assert(ComponentHas(world, entity, COMPONENT_VELOCITY), 'Should have Velocity');
  
  // Remove position component
  ComponentRemove(world, entity, COMPONENT_POSITION);
  
  // Verify removal
  Assert(not ComponentHas(world, entity, COMPONENT_POSITION), 'Should not have Position');
  Assert(ComponentHas(world, entity, COMPONENT_VELOCITY), 'Should still have Velocity');
  
  WriteLn('Iteration 3: Component removal test passed');
  
  WorldCleanup(world);
end.
```

---

## Status

⚠️ **Pending**

**What's Done:**
- ComponentAdd procedure implemented
- ComponentRemove procedure implemented
- ComponentHas function implemented
- Component data accessors implemented (Get/Set for Position, Velocity, Sprite)
- Component initialization on add
- Component clearing on remove

**What's Next:**
- Iteration 4: Query system (QueryCreate, QueryNext, component filtering)

---

**Previous:** [Iteration 2: Entity Management](./02_Iteration2_EntityManagement.md)  
**Next:** [Iteration 4: Query System](./04_Iteration4_QuerySystem.md)

