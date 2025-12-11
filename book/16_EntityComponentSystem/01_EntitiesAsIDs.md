# Entities as IDs

**Part of:** [Chapter 18: Entity Component System](./README.md)

---

> **For GCSE students:**  
> In ECS, entities are like ID numbers for game objects. Instead of having a "player object", you have entity #1 which has position, sprite, and health components. It's like a filing system where each file has a number.
>
> **For A-Level students:**  
> ECS separates data (components) from behavior (systems). Entities are integer IDs that reference component data stored in arrays. This architecture enables efficient data-oriented design and cache-friendly memory access patterns.
>
> **For University students:**  
> ECS implements data-oriented design, separating data layout from logic. Entities are handles/IDs in a sparse set. Components use Structure-of-Arrays (SoA) for cache efficiency. Systems iterate over component arrays. Understanding ECS requires understanding memory layout, cache behavior, and data-oriented programming principles.

---

## Introduction

In the Entity-Component-System (ECS) architecture, **entities are just IDs**. This section teaches you how entities work, why they're IDs, and how to manage them.

**Key concepts:**
- **Entities are IDs** — Not objects, just identifiers
- **Entity management** — Creating and destroying entities
- **Entity lifecycle** — Birth, life, death of entities
- **Entity validation** — Checking if entities are valid
- **Entity pools** — Efficient entity allocation

---

## What is an Entity?

### Entities are Identifiers

**An entity is just a number (ID):**

```pascal
type
  TEntityID = word;  // Entity identifier

const
  ENTITY_NULL = $FFFF;  // Invalid entity ID
  MAX_ENTITIES = 256;   // Maximum entities
```

**Entities are NOT objects:**
- No entity class or record
- No entity methods
- No entity data (data is in components)
- Just an ID number

**Example:**
```pascal
var
  player: TEntityID;
  enemy: TEntityID;
  
begin
  player := EntityCreate;  // Returns ID, e.g., 0
  enemy := EntityCreate;   // Returns ID, e.g., 1
  
  // player = 0, enemy = 1
  // These are just numbers!
end;
```

### Why IDs Instead of Objects?

**Benefits of using IDs:**

1. **Flexibility** — Entities can have any combination of components
2. **Efficiency** — No object overhead, just a number
3. **Cache-friendly** — Component data stored in arrays (SoA)
4. **Simple** — Easy to pass around, copy, compare

**Comparison:**
```pascal
// ❌ BAD: Object-oriented approach
type
  TEntity = class
    X, Y: integer;
    SpriteID: byte;
    // ... many fields
  end;
var player: TEntity;
// Problem: All entities have same structure (inflexible)

// ✅ GOOD: ECS approach
type
  TEntityID = word;
var player: TEntityID;
// Entity can have any combination of components (flexible)
```

---

## Creating Entities

### EntityCreate

**Create a new entity:**

```pascal
function EntityCreate: TEntityID;
```

**Returns:** Entity ID or `ENTITY_NULL` if pool is full

**Usage:**
```pascal
var
  player: TEntityID;
  enemy: TEntityID;
  
begin
  player := EntityCreate;
  if player <> ENTITY_NULL then
    WriteLn('Player created: ', player)
  else
    WriteLn('Failed to create entity');
  
  enemy := EntityCreate;
  WriteLn('Enemy created: ', enemy);
end;
```

### Entity IDs are Sequential

**Entity IDs are assigned sequentially:**

```pascal
var
  e1, e2, e3: TEntityID;
  
begin
  e1 := EntityCreate;  // Returns 0
  e2 := EntityCreate;  // Returns 1
  e3 := EntityCreate;  // Returns 2
  
  WriteLn(e1);  // 0
  WriteLn(e2);  // 1
  WriteLn(e3);  // 2
end;
```

**Why sequential?**
- Efficient storage (array indices)
- Easy to iterate
- Cache-friendly

---

## Destroying Entities

### EntityDestroy

**Destroy an entity:**

```pascal
procedure EntityDestroy(id: TEntityID);
```

**What happens:**
- Entity ID is freed (can be reused)
- All components are removed
- Entity becomes invalid

**Usage:**
```pascal
var
  enemy: TEntityID;
  
begin
  enemy := EntityCreate;
  // ... use enemy ...
  EntityDestroy(enemy);
  // enemy is now invalid
end;
```

### Destroying Multiple Entities

**Destroy entities in a loop:**

```pascal
var
  enemies: array[0..9] of TEntityID;
  i: byte;
  
begin
  // Create enemies
  for i := 0 to 9 do
    enemies[i] := EntityCreate;
  
  // ... game logic ...
  
  // Destroy all enemies
  for i := 0 to 9 do
    EntityDestroy(enemies[i]);
end;
```

---

## Entity Validation

### EntityValid

**Check if an entity is valid:**

```pascal
function EntityValid(id: TEntityID): boolean;
```

**Returns:** `true` if entity exists and is active

**Usage:**
```pascal
var
  player: TEntityID;
  
begin
  player := EntityCreate;
  
  if EntityValid(player) then
    WriteLn('Player is valid')
  else
    WriteLn('Player is invalid');
  
  EntityDestroy(player);
  
  if EntityValid(player) then
    WriteLn('Still valid')  // Won't execute
  else
    WriteLn('Now invalid');  // Will execute
end;
```

### Checking Before Use

**Always validate before using:**

```pascal
procedure UpdateEntity(id: TEntityID);
begin
  if EntityValid(id) then
  begin
    // Safe to use entity
    EntitySetPosition(id, 100, 50);
  end
  else
    WriteLn('Entity ', id, ' is invalid');
end;
```

### Null Entity Check

**Check for null entity:**

```pascal
var
  entity: TEntityID;
  
begin
  entity := EntityCreate;
  
  if entity = ENTITY_NULL then
    WriteLn('Failed to create entity')
  else
    WriteLn('Entity created: ', entity);
end;
```

---

## Entity Lifecycle

### Birth, Life, Death

**Entity lifecycle stages:**

1. **Creation** — `EntityCreate` returns ID
2. **Life** — Entity exists, has components
3. **Destruction** — `EntityDestroy` removes entity

**Example:**
```pascal
var
  bullet: TEntityID;
  
begin
  // 1. Birth: Create entity
  bullet := EntityCreate;
  if bullet = ENTITY_NULL then
    Exit;  // Failed to create
  
  // 2. Life: Add components, use entity
  EntitySetPosition(bullet, 100, 50);
  EntitySetVelocity(bullet, 5, 0);
  EntitySetSprite(bullet, 0, 10, 1);
  
  // ... entity lives and moves ...
  
  // 3. Death: Destroy entity
  if EntityValid(bullet) then
    EntityDestroy(bullet);
end;
```

### Entity Pool

**Entities are managed in a pool:**

```pascal
// Internal entity pool (simplified)
var
  entityPool: array[0..MAX_ENTITIES-1] of boolean;  // true = in use
  nextEntityID: word;
  
function EntityCreate: TEntityID;
var
  i: word;
begin
  // Find free slot
  for i := 0 to MAX_ENTITIES - 1 do
  begin
    if not entityPool[i] then
    begin
      entityPool[i] := true;
      EntityCreate := i;
      Exit;
    end;
  end;
  EntityCreate := ENTITY_NULL;  // Pool full
end;

procedure EntityDestroy(id: TEntityID);
begin
  if (id < MAX_ENTITIES) and entityPool[id] then
  begin
    entityPool[id] := false;
    // Clear components (not shown)
  end;
end;
```

---

## Working with Entities

### Storing Entity IDs

**Store entity IDs in variables:**

```pascal
var
  player: TEntityID;
  enemies: array[0..9] of TEntityID;
  bullets: array[0..19] of TEntityID;
  
begin
  player := EntityCreate;
  
  // Create enemies
  var i: byte;
  for i := 0 to 9 do
    enemies[i] := EntityCreate;
  
  // Create bullets
  for i := 0 to 19 do
    bullets[i] := EntityCreate;
end;
```

### Passing Entities

**Pass entity IDs to procedures:**

```pascal
procedure MoveEntity(id: TEntityID; x, y: integer);
begin
  if EntityValid(id) then
    EntitySetPosition(id, x, y);
end;

// Usage
var player: TEntityID;
player := EntityCreate;
MoveEntity(player, 100, 50);
```

### Comparing Entities

**Compare entity IDs:**

```pascal
var
  e1, e2: TEntityID;
  
begin
  e1 := EntityCreate;  // Returns 0
  e2 := EntityCreate;  // Returns 1
  
  if e1 = e2 then
    WriteLn('Same entity')  // Won't execute
  else
    WriteLn('Different entities');  // Will execute
end;
```

---

## Entity Patterns

### Entity Lists

**Track entities in lists:**

```pascal
type
  TEntityList = record
    Entities: array[0..99] of TEntityID;
    Count: byte;
  end;

procedure AddEntity(var list: TEntityList; id: TEntityID);
begin
  if list.Count < 100 then
  begin
    list.Entities[list.Count] := id;
    list.Count := list.Count + 1;
  end;
end;

procedure RemoveEntity(var list: TEntityList; id: TEntityID);
var
  i: byte;
begin
  for i := 0 to list.Count - 1 do
  begin
    if list.Entities[i] = id then
    begin
      // Move last entity to this position
      list.Entities[i] := list.Entities[list.Count - 1];
      list.Count := list.Count - 1;
      Exit;
    end;
  end;
end;
```

### Entity Tags

**Tag entities for quick lookup:**

```pascal
const
  TAG_PLAYER = 1;
  TAG_ENEMY = 2;
  TAG_BULLET = 3;
  TAG_POWERUP = 4;

var
  playerEntities: array[0..9] of TEntityID;
  enemyEntities: array[0..49] of TEntityID;
  
procedure TagEntity(id: TEntityID; tag: byte);
begin
  case tag of
    TAG_PLAYER: AddToPlayerList(id);
    TAG_ENEMY: AddToEnemyList(id);
    // ... etc
  end;
end;
```

---

## Common Mistakes

### Mistake 1: Using Entity After Destroy

**Don't use entity after destroying:**

```pascal
// ❌ BAD
var player: TEntityID;
player := EntityCreate;
EntityDestroy(player);
EntitySetPosition(player, 100, 50);  // ERROR: Entity is invalid!

// ✅ GOOD
var player: TEntityID;
player := EntityCreate;
if EntityValid(player) then
  EntitySetPosition(player, 100, 50);
EntityDestroy(player);
```

### Mistake 2: Not Checking for Null

**Always check for null entity:**

```pascal
// ❌ BAD
var entity: TEntityID;
entity := EntityCreate;
EntitySetPosition(entity, 100, 50);  // May be ENTITY_NULL!

// ✅ GOOD
var entity: TEntityID;
entity := EntityCreate;
if entity <> ENTITY_NULL then
  EntitySetPosition(entity, 100, 50);
```

### Mistake 3: Storing Invalid Entity

**Don't store entity ID after destroy:**

```pascal
// ❌ BAD
var savedEntity: TEntityID;
savedEntity := EntityCreate;
EntityDestroy(savedEntity);
// Later...
EntitySetPosition(savedEntity, 100, 50);  // ERROR!

// ✅ GOOD
var savedEntity: TEntityID;
savedEntity := EntityCreate;
// Use entity...
if EntityValid(savedEntity) then
  EntitySetPosition(savedEntity, 100, 50);
// Don't use after destroy
```

---

## Best Practices

### 1. Always Validate

**Check entity validity before use:**

```pascal
// ✅ GOOD
if EntityValid(entity) then
  EntitySetPosition(entity, x, y);

// ❌ BAD
EntitySetPosition(entity, x, y);  // May be invalid
```

### 2. Check for Null

**Check for null after creation:**

```pascal
// ✅ GOOD
entity := EntityCreate;
if entity <> ENTITY_NULL then
  // Use entity

// ❌ BAD
entity := EntityCreate;
// Use entity (may be null)
```

### 3. Destroy When Done

**Destroy entities when no longer needed:**

```pascal
// ✅ GOOD
bullet := EntityCreate;
// ... use bullet ...
if IsOffScreen(bullet) then
  EntityDestroy(bullet);

// ❌ BAD
bullet := EntityCreate;
// ... use bullet ...
// Never destroyed (memory leak)
```

### 4. Use Meaningful Names

**Name entity variables clearly:**

```pascal
// ✅ GOOD
var player: TEntityID;
var enemy: TEntityID;

// ❌ BAD
var e1: TEntityID;
var e2: TEntityID;  // What are these?
```

---

## Exercises

### Exercise 1: Create and Destroy

Write a program that:
1. Creates 10 entities
2. Stores their IDs in an array
3. Validates each entity
4. Destroys all entities
5. Verifies they're invalid after destroy

### Exercise 2: Entity Lifecycle

Write a program that:
1. Creates a bullet entity
2. Moves it across the screen
3. Destroys it when it goes off-screen
4. Creates a new bullet when space is pressed
5. Tracks bullet lifecycle

### Exercise 3: Entity Pool

Write a program that:
1. Creates entities until pool is full
2. Handles `ENTITY_NULL` when pool is exhausted
3. Destroys some entities
4. Creates new entities (should reuse freed slots)
5. Tracks entity count

### Exercise 4: Entity Lists

**GCSE Level:**
Write a program that:
1. Creates multiple entities
2. Stores them in different lists (players, enemies, bullets)
3. Iterates through each list
4. Destroys entities from lists
5. Maintains lists correctly

---

## Level-Specific Exercises

### GCSE Level Exercises

**Exercise 1: Create and Destroy**
Write a program that:
1. Creates 10 entities
2. Displays their IDs
3. Destroys half of them
4. Verifies which ones are still valid

**Exercise 2: Entity Tracking**
Write a program that:
1. Creates entities for a game (player, enemies, coins)
2. Tracks which entities exist
3. Destroys entities when needed
4. Displays entity count

### A-Level Exercises

**Exercise 1: Entity Pools**
Implement an entity pool:
1. Pre-allocate entity IDs
2. Reuse destroyed entity IDs
3. Track available entities
4. Measure performance vs. direct allocation

**Exercise 2: Entity Tags**
Implement entity tagging system:
1. Assign tags to entities
2. Query entities by tag
3. Efficient tag lookup
4. Support multiple tags per entity

**Exercise 3: Entity Lifecycle**
Implement complete entity lifecycle:
1. Creation with initialization
2. Active state management
3. Destruction with cleanup
4. Event handling for lifecycle events

### University Level Exercises

**Exercise 1: Sparse Set Implementation**
Implement sparse set for entities:
1. O(1) add, remove, contains
2. O(1) iteration
3. Memory-efficient storage
4. Compare with other data structures
5. Analyze cache behavior

**Exercise 2: Entity ID Generation**
Design entity ID system:
1. Unique ID generation
2. ID validation
3. ID recycling
4. Handle ID overflow
5. Thread-safe ID generation (conceptual)

**Exercise 3: Entity Query System**
Implement entity query system:
1. Query by component types
2. Efficient filtering
3. Cache query results
4. Incremental updates
5. Performance analysis

---

**Previous Chapter:** [Chapter 11: Game Loop and Time-Based Programming](../15_GameLoopAndTimeBasedProgramming/README.md)  
**Next Section:** [Components as Records](./02_ComponentsAsRecords.md)  
**Language Specification:** See [Game Engine Concepts](../../languageSpecification/09_GameEngine_Concepts.md)  
**Last Updated:** 2025-01-XX

