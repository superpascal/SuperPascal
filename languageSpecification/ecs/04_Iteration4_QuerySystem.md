# ECS Implementation - Iteration 4: Query System

**Status:** ⚠️ Pending  
**Part of:** SuperPascal ECS Library Implementation

---

## Goal

Implement query system for efficiently iterating over entities with specific components.

---

## Implementation

### Query Types and Functions

```pascal
// Add to ECS unit interface section

// Query type
type
  TQuery = record
    World: ^TWorld;
    Components: TComponentMask;
    CurrentIndex: Word;
  end;

// Query API
function QueryCreate(var world: TWorld; components: TComponentMask): TQuery;
function QueryNext(var query: TQuery; var entity: TEntity): Boolean;
procedure QueryReset(var query: TQuery);
```

### Implementation

```pascal
// Add to ECS unit implementation section

// Create query
function QueryCreate(var world: TWorld; components: TComponentMask): TQuery;
var
  query: TQuery;
begin
  query.World := @world;
  query.Components := components;
  query.CurrentIndex := 0;
  Result := query;
end;

// Get next entity from query
function QueryNext(var query: TQuery; var entity: TEntity): Boolean;
var
  i: Word;
  world: ^TWorld;
begin
  world := query.World;
  
  // Search from current index
  for i := query.CurrentIndex to world^.EntityCount - 1 do
  begin
    // Check if entity is valid
    if not world^.EntityValid[i] then
      Continue;
    
    // Check if entity has all required components
    if (query.Components <= world^.EntityComponents[i]) then
    begin
      entity := i;
      query.CurrentIndex := i + 1;
      Result := True;
      Exit;
    end;
  end;
  
  // No more entities found
  Result := False;
end;

// Reset query to start
procedure QueryReset(var query: TQuery);
begin
  query.CurrentIndex := 0;
end;
```

---

## Testing

### Test 1: Basic Query

```pascal
program TestECS_Iteration4;
uses ECS;

var
  world: TWorld;
  entity1, entity2, entity3: TEntity;
  query: TQuery;
  entity: TEntity;
  count: Word;
begin
  WorldInit(world);
  
  // Create entities
  entity1 := EntityCreate(world);
  entity2 := EntityCreate(world);
  entity3 := EntityCreate(world);
  
  // Add components
  ComponentAdd(world, entity1, COMPONENT_POSITION);
  ComponentAdd(world, entity1, COMPONENT_VELOCITY);
  
  ComponentAdd(world, entity2, COMPONENT_POSITION);
  ComponentAdd(world, entity2, COMPONENT_SPRITE);
  
  ComponentAdd(world, entity3, COMPONENT_POSITION);
  ComponentAdd(world, entity3, COMPONENT_VELOCITY);
  ComponentAdd(world, entity3, COMPONENT_SPRITE);
  
  // Query for entities with Position and Velocity
  query := QueryCreate(world, [COMPONENT_POSITION, COMPONENT_VELOCITY]);
  count := 0;
  while QueryNext(query, entity) do
  begin
    count := count + 1;
    Assert(ComponentHas(world, entity, COMPONENT_POSITION), 'Should have Position');
    Assert(ComponentHas(world, entity, COMPONENT_VELOCITY), 'Should have Velocity');
  end;
  
  Assert(count = 2, 'Should find 2 entities (entity1 and entity3)');
  
  WriteLn('Iteration 4: Query system test passed');
  
  WorldCleanup(world);
end.
```

### Test 2: Query Reset

```pascal
program TestECS_Iteration4_Reset;
uses ECS;

var
  world: TWorld;
  entity: TEntity;
  query: TQuery;
  count1, count2: Word;
begin
  WorldInit(world);
  
  // Create entity with Position
  entity := EntityCreate(world);
  ComponentAdd(world, entity, COMPONENT_POSITION);
  
  // Query
  query := QueryCreate(world, [COMPONENT_POSITION]);
  
  // First iteration
  count1 := 0;
  while QueryNext(query, entity) do
    count1 := count1 + 1;
  
  Assert(count1 = 1, 'First iteration should find 1 entity');
  
  // Reset and query again
  QueryReset(query);
  count2 := 0;
  while QueryNext(query, entity) do
    count2 := count2 + 1;
  
  Assert(count2 = 1, 'Second iteration should find 1 entity');
  
  WriteLn('Iteration 4: Query reset test passed');
  
  WorldCleanup(world);
end.
```

---

## Status

⚠️ **Pending**

**What's Done:**
- QueryCreate function implemented
- QueryNext function implemented
- QueryReset procedure implemented
- Component mask filtering implemented

**What's Next:**
- Iteration 5: System registration and execution (SystemRegister, SystemRunAll)

---

**Previous:** [Iteration 3: Component Management](./03_Iteration3_ComponentManagement.md)  
**Next:** [Iteration 5: System Registration](./05_Iteration5_SystemRegistration.md)

