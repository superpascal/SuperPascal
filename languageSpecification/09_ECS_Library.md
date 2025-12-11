# SuperPascal ECS Library Specification

**Part of:** SuperPascal Language Specification  
**Version:** 1.0  
**Status:** Design Specification

---

## Overview

The SuperPascal ECS (Entity-Component-System) Library is a **reusable, platform-agnostic** library for building data-driven game applications. It provides a clean, educational API that works across all SuperPascal target platforms.

**Key Principles:**
- **Reusable:** Import as a standard Pascal unit
- **Platform-Agnostic:** Core ECS works on all platforms
- **Educational:** Simple, understandable API
- **Performance:** Optimized for retro hardware constraints
- **Extensible:** Easy to add platform-specific features

---

## Library Structure

### Core Unit: `ECS`

The main ECS library unit provides all core functionality:

```pascal
unit ECS;

interface
  // Core types
  // Entity management
  // Component management
  // System execution
  // Query system
  
implementation
  // Core ECS implementation
end.
```

### Extension Units (Optional)

- `ECS_Physics` - Physics components and systems
- `ECS_Render` - Rendering components and systems
- `ECS_Audio` - Audio components and systems
- `ECS_Input` - Input handling components and systems

---

## Core Types

### Entity

```pascal
type
  TEntity = word;  // Entity ID (0-based index)
  
const
  ENTITY_NULL = $FFFF;  // Invalid entity ID
  MAX_ENTITIES = 256;   // Maximum entities (configurable per platform)
```

**Design Notes:**
- Simple integer ID for maximum compatibility
- `ENTITY_NULL` represents invalid/deleted entities
- Maximum entities configurable per platform (memory constraints)

### Component

Components are **Pascal records**:

```pascal
type
  // Example: Position component
  TPosition = record
    X, Y: Integer;  // Or Fixed16 for no-FPU platforms
  end;
  
  // Example: Velocity component
  TVelocity = record
    DX, DY: Integer;  // Or Fixed16
  end;
  
  // Example: Sprite component
  TSprite = record
    SpriteID: Byte;
    TileID: Word;
    Palette: Byte;
    Visible: Boolean;
  end;
```

**Design Notes:**
- Components are **plain data** (records)
- No methods, no inheritance
- Platform-agnostic types (Integer, Boolean, etc.)
- Use Fixed16/Fixed32 for no-FPU platforms

### Component Storage (Structure-of-Arrays)

The ECS library uses **Structure-of-Arrays (SoA)** storage for cache efficiency:

```pascal
type
  // Component storage arrays
  TPositionArray = array[0..MAX_ENTITIES-1] of TPosition;
  TVelocityArray = array[0..MAX_ENTITIES-1] of TVelocity;
  TSpriteArray = array[0..MAX_ENTITIES-1] of TSprite;
  
  // Component presence flags
  TComponentMask = set of Byte;  // Bitmask for component presence
  
var
  // Component storage (SoA)
  Positions: TPositionArray;
  Velocities: TVelocityArray;
  Sprites: TSpriteArray;
  
  // Component presence tracking
  EntityComponents: array[0..MAX_ENTITIES-1] of TComponentMask;
  
  // Entity validity
  EntityValid: array[0..MAX_ENTITIES-1] of Boolean;
```

**Design Notes:**
- SoA layout for cache-friendly iteration
- Component masks track which components each entity has
- Entity validity array tracks active entities

### World

The `World` manages all entities, components, and systems:

```pascal
type
  TWorld = record
    // Entity management
    EntityCount: Word;
    FreeEntities: array[0..MAX_ENTITIES-1] of TEntity;
    FreeCount: Word;
    
    // Component storage (SoA)
    Positions: TPositionArray;
    Velocities: TVelocityArray;
    Sprites: TSpriteArray;
    // ... more component arrays
    
    // Component presence
    EntityComponents: array[0..MAX_ENTITIES-1] of TComponentMask;
    EntityValid: array[0..MAX_ENTITIES-1] of Boolean;
    
    // Systems
    Systems: array[0..MAX_SYSTEMS-1] of TSystem;
    SystemCount: Word;
  end;
```

**Design Notes:**
- Single `World` instance per game
- Manages entity pool (free list for reuse)
- Stores all component arrays
- Manages system execution

### System

Systems are **procedures** that operate on entities with specific components:

```pascal
type
  TSystem = procedure(var world: TWorld);
```

**Example System:**
```pascal
procedure MovementSystem(var world: TWorld);
var
  i: Word;
  entity: TEntity;
begin
  for i := 0 to world.EntityCount - 1 do
  begin
    entity := i;
    if not world.EntityValid[entity] then Continue;
    
    // Check if entity has Position and Velocity components
    if HasComponent(world, entity, COMPONENT_POSITION) and
       HasComponent(world, entity, COMPONENT_VELOCITY) then
    begin
      // Update position based on velocity
      world.Positions[entity].X := world.Positions[entity].X + world.Velocities[entity].DX;
      world.Positions[entity].Y := world.Positions[entity].Y + world.Velocities[entity].DY;
    end;
  end;
end;
```

**Design Notes:**
- Systems are simple procedures
- Systems receive `world` as parameter
- Systems iterate over entities and check components
- Systems can be registered with the world

### Query

Queries provide a convenient way to iterate over entities with specific components:

```pascal
type
  TQuery = record
    World: ^TWorld;
    Components: TComponentMask;
    CurrentIndex: Word;
  end;
  
function QueryCreate(var world: TWorld; components: TComponentMask): TQuery;
function QueryNext(var query: TQuery; var entity: TEntity): Boolean;
```

**Example Usage:**
```pascal
procedure MovementSystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
begin
  query := QueryCreate(world, [COMPONENT_POSITION, COMPONENT_VELOCITY]);
  while QueryNext(query, entity) do
  begin
    world.Positions[entity].X := world.Positions[entity].X + world.Velocities[entity].DX;
    world.Positions[entity].Y := world.Positions[entity].Y + world.Velocities[entity].DY;
  end;
end;
```

**Design Notes:**
- Queries filter entities by component mask
- More convenient than manual iteration
- Can be optimized for performance

---

## Core API

### World Management

```pascal
// Initialize world
procedure WorldInit(var world: TWorld);

// Cleanup world
procedure WorldCleanup(var world: TWorld);

// Update world (run all systems)
procedure WorldUpdate(var world: TWorld);
```

### Entity Management

```pascal
// Create new entity
function EntityCreate(var world: TWorld): TEntity;

// Destroy entity
procedure EntityDestroy(var world: TWorld; entity: TEntity);

// Check if entity is valid
function EntityIsValid(var world: TWorld; entity: TEntity): Boolean;
```

### Component Management

```pascal
// Component type IDs
const
  COMPONENT_POSITION = 0;
  COMPONENT_VELOCITY = 1;
  COMPONENT_SPRITE = 2;
  // ... more component IDs

// Add component to entity
procedure ComponentAdd(var world: TWorld; entity: TEntity; componentID: Byte);

// Remove component from entity
procedure ComponentRemove(var world: TWorld; entity: TEntity; componentID: Byte);

// Check if entity has component
function ComponentHas(var world: TWorld; entity: TEntity; componentID: Byte): Boolean;

// Get component data
function ComponentGetPosition(var world: TWorld; entity: TEntity): ^TPosition;
function ComponentGetVelocity(var world: TWorld; entity: TEntity): ^TVelocity;
function ComponentGetSprite(var world: TWorld; entity: TEntity): ^TSprite;

// Set component data
procedure ComponentSetPosition(var world: TWorld; entity: TEntity; const pos: TPosition);
procedure ComponentSetVelocity(var world: TWorld; entity: TEntity; const vel: TVelocity);
procedure ComponentSetSprite(var world: TWorld; entity: TEntity; const sprite: TSprite);
```

### System Management

```pascal
// Register system
procedure SystemRegister(var world: TWorld; system: TSystem);

// Run all systems
procedure SystemRunAll(var world: TWorld);
```

### Query API

```pascal
// Create query
function QueryCreate(var world: TWorld; components: TComponentMask): TQuery;

// Get next entity from query
function QueryNext(var query: TQuery; var entity: TEntity): Boolean;

// Reset query
procedure QueryReset(var query: TQuery);
```

---

## Complete Library Interface

```pascal
unit ECS;

interface

type
  // Entity
  TEntity = Word;
  
  // Component masks
  TComponentMask = set of Byte;
  
  // World
  TWorld = record
    EntityCount: Word;
    FreeEntities: array[0..MAX_ENTITIES-1] of TEntity;
    FreeCount: Word;
    EntityComponents: array[0..MAX_ENTITIES-1] of TComponentMask;
    EntityValid: array[0..MAX_ENTITIES-1] of Boolean;
    // Component storage (SoA)
    Positions: array[0..MAX_ENTITIES-1] of TPosition;
    Velocities: array[0..MAX_ENTITIES-1] of TVelocity;
    Sprites: array[0..MAX_ENTITIES-1] of TSprite;
    // Systems
    Systems: array[0..MAX_SYSTEMS-1] of TSystem;
    SystemCount: Word;
  end;
  
  // System
  TSystem = procedure(var world: TWorld);
  
  // Query
  TQuery = record
    World: ^TWorld;
    Components: TComponentMask;
    CurrentIndex: Word;
  end;
  
  // Component types
  TPosition = record
    X, Y: Integer;
  end;
  
  TVelocity = record
    DX, DY: Integer;
  end;
  
  TSprite = record
    SpriteID: Byte;
    TileID: Word;
    Palette: Byte;
    Visible: Boolean;
  end;

const
  ENTITY_NULL = $FFFF;
  MAX_ENTITIES = 256;
  MAX_SYSTEMS = 32;
  
  COMPONENT_POSITION = 0;
  COMPONENT_VELOCITY = 1;
  COMPONENT_SPRITE = 2;

// World management
procedure WorldInit(var world: TWorld);
procedure WorldCleanup(var world: TWorld);
procedure WorldUpdate(var world: TWorld);

// Entity management
function EntityCreate(var world: TWorld): TEntity;
procedure EntityDestroy(var world: TWorld; entity: TEntity);
function EntityIsValid(var world: TWorld; entity: TEntity): Boolean;

// Component management
procedure ComponentAdd(var world: TWorld; entity: TEntity; componentID: Byte);
procedure ComponentRemove(var world: TWorld; entity: TEntity; componentID: Byte);
function ComponentHas(var world: TWorld; entity: TEntity; componentID: Byte): Boolean;
function ComponentGetPosition(var world: TWorld; entity: TEntity): ^TPosition;
function ComponentGetVelocity(var world: TWorld; entity: TEntity): ^TVelocity;
function ComponentGetSprite(var world: TWorld; entity: TEntity): ^TSprite;
procedure ComponentSetPosition(var world: TWorld; entity: TEntity; const pos: TPosition);
procedure ComponentSetVelocity(var world: TWorld; entity: TEntity; const vel: TVelocity);
procedure ComponentSetSprite(var world: TWorld; entity: TEntity; const sprite: TSprite);

// System management
procedure SystemRegister(var world: TWorld; system: TSystem);
procedure SystemRunAll(var world: TWorld);

// Query API
function QueryCreate(var world: TWorld; components: TComponentMask): TQuery;
function QueryNext(var query: TQuery; var entity: TEntity): Boolean;
procedure QueryReset(var query: TQuery);

implementation
  // Implementation details
end.
```

---

## Usage Examples

### Example 1: Basic Entity Creation

```pascal
program GameDemo;
uses ECS;

var
  world: TWorld;
  player: TEntity;
  pos: TPosition;
  vel: TVelocity;
  sprite: TSprite;
begin
  // Initialize world
  WorldInit(world);
  
  // Create player entity
  player := EntityCreate(world);
  
  // Add components
  ComponentAdd(world, player, COMPONENT_POSITION);
  ComponentAdd(world, player, COMPONENT_VELOCITY);
  ComponentAdd(world, player, COMPONENT_SPRITE);
  
  // Set component data
  pos.X := 100;
  pos.Y := 100;
  ComponentSetPosition(world, player, pos);
  
  vel.DX := 1;
  vel.DY := 0;
  ComponentSetVelocity(world, player, vel);
  
  sprite.SpriteID := 0;
  sprite.TileID := 42;
  sprite.Palette := 1;
  sprite.Visible := True;
  ComponentSetSprite(world, player, sprite);
  
  // Game loop
  while True do
  begin
    WorldUpdate(world);
    WaitVBlank;
  end;
  
  WorldCleanup(world);
end.
```

### Example 2: Movement System

```pascal
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
    
    pos^.X := pos^.X + vel^.DX;
    pos^.Y := pos^.Y + vel^.DY;
  end;
end;

// Register system
WorldInit(world);
SystemRegister(world, MovementSystem);
```

### Example 3: Rendering System

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
    
    if sprite^.Visible then
    begin
      // Platform-specific rendering
      {$IFDEF ZEALZ80}
      SpriteSetPosition(sprite^.SpriteID, pos^.X, pos^.Y);
      SpriteSetTile(sprite^.SpriteID, sprite^.TileID);
      {$ELSEIF DEFINED(COMMANDERX16)}
      VERA_SpriteSetPosition(sprite^.SpriteID, pos^.X, pos^.Y);
      VERA_SpriteSetTile(sprite^.SpriteID, sprite^.TileID);
      {$ENDIF}
    end;
  end;
end;
```

---

## Integration with Game Algorithms

### Physics Algorithms

All physics algorithms from the Mikro archive can be integrated as ECS systems:

```pascal
// From algorithms/06_PhysicsSimulation.md
procedure GravitySystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
  vel: ^TVelocity;
  physics: ^TPhysicsBody;  // Additional component
begin
  query := QueryCreate(world, [COMPONENT_VELOCITY, COMPONENT_PHYSICS]);
  while QueryNext(query, entity) do
  begin
    vel := ComponentGetVelocity(world, entity);
    physics := ComponentGetPhysics(world, entity);
    
    // Apply gravity (from GRAVITY.TXT)
    vel^.DY := vel^.DY + physics^.Gravity;
  end;
end;

procedure ParticleSystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
  particle: ^TParticle;  // Particle component
begin
  query := QueryCreate(world, [COMPONENT_PARTICLE]);
  while QueryNext(query, entity) do
  begin
    particle := ComponentGetParticle(world, entity);
    
    // Update particle (from PARTICLE.TXT)
    UpdateParticle(particle^);
    
    // Remove if expired
    if particle^.Energy <= 0 then
      EntityDestroy(world, entity);
  end;
end;
```

### Collision Detection Algorithms

```pascal
// From algorithms/05_CollisionDetection.md
procedure CollisionSystem(var world: TWorld);
var
  query1, query2: TQuery;
  entity1, entity2: TEntity;
  pos1, pos2: ^TPosition;
  collider1, collider2: ^TCollider;
begin
  query1 := QueryCreate(world, [COMPONENT_POSITION, COMPONENT_COLLIDER]);
  while QueryNext(query1, entity1) do
  begin
    pos1 := ComponentGetPosition(world, entity1);
    collider1 := ComponentGetCollider(world, entity1);
    
    query2 := QueryCreate(world, [COMPONENT_POSITION, COMPONENT_COLLIDER]);
    while QueryNext(query2, entity2) do
    begin
      if entity1 = entity2 then Continue;
      
      pos2 := ComponentGetPosition(world, entity2);
      collider2 := ComponentGetCollider(world, entity2);
      
      // Check collision (from CLIP.TXT, POLYCLIP.TXT)
      if CheckAABBCollision(pos1^, collider1^, pos2^, collider2^) then
      begin
        // Handle collision
        ResolveCollision(world, entity1, entity2);
      end;
    end;
  end;
end;
```

### Graphics Algorithms

```pascal
// From algorithms/04_GraphicsAlgorithms.md
procedure PolygonRenderSystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
  transform: ^TTransform;  // Transform component
  polygon: ^TPolygon;      // Polygon component
begin
  query := QueryCreate(world, [COMPONENT_TRANSFORM, COMPONENT_POLYGON]);
  while QueryNext(query, entity) do
  begin
    transform := ComponentGetTransform(world, entity);
    polygon := ComponentGetPolygon(world, entity);
    
    // Render polygon (from OTMPOLY.TXT)
    RenderPolygon(transform^, polygon^);
  end;
end;
```

---

## Extension Points

### Adding New Components

1. **Define component type:**
```pascal
type
  THealth = record
    Current: Integer;
    Maximum: Integer;
  end;
```

2. **Add to world storage:**
```pascal
type
  TWorld = record
    // ... existing fields
    Healths: array[0..MAX_ENTITIES-1] of THealth;
  end;
```

3. **Add component ID:**
```pascal
const
  COMPONENT_HEALTH = 3;
```

4. **Add accessor functions:**
```pascal
function ComponentGetHealth(var world: TWorld; entity: TEntity): ^THealth;
procedure ComponentSetHealth(var world: TWorld; entity: TEntity; const health: THealth);
```

### Adding New Systems

Simply create a procedure that matches the `TSystem` signature:

```pascal
procedure HealthSystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
  health: ^THealth;
begin
  query := QueryCreate(world, [COMPONENT_HEALTH]);
  while QueryNext(query, entity) do
  begin
    health := ComponentGetHealth(world, entity);
    
    // System logic here
    if health^.Current <= 0 then
      EntityDestroy(world, entity);
  end;
end;

// Register system
SystemRegister(world, HealthSystem);
```

---

## Platform-Specific Extensions

### ZealZ80 Extension

```pascal
unit ECS_ZealZ80;

interface
uses ECS;

// ZealZ80-specific sprite component
type
  TSpriteZealZ80 = record
    SpriteID: Byte;  // 0-127 (ZVB sprite limit)
    TileID: Word;
    Palette: Byte;
    Visible: Boolean;
  end;

// ZealZ80-specific rendering system
procedure RenderSystemZealZ80(var world: TWorld);

implementation
  // ZealZ80 hardware integration
end.
```

### CommanderX16 Extension

```pascal
unit ECS_CommanderX16;

interface
uses ECS;

// CommanderX16-specific sprite component
type
  TSpriteCommanderX16 = record
    SpriteID: Byte;  // VERA sprite index
    TileID: Word;
    Palette: Byte;
    Visible: Boolean;
  end;

// CommanderX16-specific rendering system
procedure RenderSystemCommanderX16(var world: TWorld);

implementation
  // VERA hardware integration
end.
```

---

## Performance Considerations

### Memory Efficiency

- **SoA storage:** Cache-friendly iteration
- **Component masks:** Efficient component presence checking
- **Entity pooling:** Reuse entities to avoid allocation

### Optimization Tips

1. **Batch processing:** Process all entities of same archetype together
2. **Early exit:** Skip entities without required components
3. **Query caching:** Cache queries when possible
4. **System ordering:** Order systems to minimize cache misses

### Platform-Specific Optimizations

- **ZealZ80:** Minimize entity count (target 50-100)
- **CommanderX16:** Optimize for VERA sprite limits
- **FoenixA2560M:** Can handle more entities (200+)
- **Raspberry Pi 5:** Can use more advanced optimizations

---

## Summary

The SuperPascal ECS Library provides:

1. **Reusable Core:** Import `ECS` unit in any project
2. **Platform-Agnostic:** Core works on all platforms
3. **Extensible:** Easy to add components and systems
4. **Educational:** Simple, understandable API
5. **Performance:** Optimized for retro hardware
6. **Integration:** Works with all game algorithms

**Next Steps:**
1. Implement core `ECS` unit
2. Create example systems using Mikro archive algorithms
3. Add platform-specific extensions
4. Integrate into book chapters

---

**See Also:**
- [Game Engine Specification](./09_GameEngine.md)
- [Bevy ECS Reference](../docs/BEVY_ECS_REFERENCE.md)
- [Book Chapter 16: Entity Component System](../book/16_EntityComponentSystem/README.md)

