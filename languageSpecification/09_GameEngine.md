# SuperPascal Language Specification — Game Engine Integration

## Game Engine Concepts and Platform Abstractions

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

**Note:** This document has been refactored. For general game engine concepts, see **[09_GameEngine_Concepts.md](./09_GameEngine_Concepts.md)**. For platform-specific implementations, see **[Platform Specifications](../platforms/README.md)**.

---

## 1. Overview

The SuperPascal Game Engine provides a high-level framework for game development across multiple retro computing platforms. It provides platform-agnostic concepts that map to platform-specific hardware:

- Entity-Component-System (ECS) architecture
- Scene management
- Physics and collision detection
- Animation system
- Input handling
- Audio integration
- Scripting support

This specification defines the language-level support for the game engine, including types, intrinsics, and standard library units.

---

## 2. Core Game Engine Types

### 2.1 Entity System

#### TEntityID

```pascal
type
  TEntityID = word;  // Entity identifier (0-based index)
```

Entities are represented as integer IDs. All component data is stored in Structure-of-Arrays (SoA) format.

#### Entity Constants

```pascal
const
  ENTITY_NULL = $FFFF;  // Invalid entity ID
  MAX_ENTITIES = 256;   // Maximum entities (configurable)
```

### 2.2 Component Types

#### Position Component

```pascal
type
  TPosition = record
    X, Y: integer;  // Fixed-point or integer coordinates
  end;
```

**Storage**: SoA arrays:
```pascal
var
  PositionX: array[0..MAX_ENTITIES-1] of integer;
  PositionY: array[0..MAX_ENTITIES-1] of integer;
```

#### Velocity Component

```pascal
type
  TVelocity = record
    VX, VY: integer;  // Velocity in pixels per frame
  end;
```

**Storage**: SoA arrays:
```pascal
var
  VelocityX: array[0..MAX_ENTITIES-1] of integer;
  VelocityY: array[0..MAX_ENTITIES-1] of integer;
```

#### Sprite Component (Abstract)

**Concept:** Visual representation of entity. Platform-specific implementations use naming convention `<Concept>Hardware<Platform>`.

**General concept:**
```pascal
type
  TSprite = record
    SpriteID: byte;      // Hardware sprite index (platform-specific range)
    TileID: word;        // Tile index in tileset
    Palette: byte;        // Palette index
    Visible: boolean;     // Visibility flag
  end;
```

**Platform-specific implementations:**
- `TSpriteHardwareZeal` - ZealZ80 ZVB sprite (0-127 sprites)
- `TSpriteHardwareF256` - Foenix F256x sprite (platform-specific)
- `TSpriteHardwareA2560` - Foenix A2560M sprite (platform-specific)

See [Platform Specifications](../platforms/README.md) for platform-specific sprite types.

**Storage**: SoA arrays:
```pascal
var
  SpriteID: array[0..MAX_ENTITIES-1] of byte;
  TileID: array[0..MAX_ENTITIES-1] of word;
  Palette: array[0..MAX_ENTITIES-1] of byte;
  Visible: array[0..MAX_ENTITIES-1] of boolean;
```

#### Collider Component

```pascal
type
  TColliderAABB = record
    X, Y: integer;      // Position offset from entity
    W, H: integer;      // Width and height
    Solid: boolean;      // Solid collision flag
  end;
  
  TColliderCircle = record
    X, Y: integer;      // Position offset from entity
    Radius: integer;    // Circle radius
    Solid: boolean;
  end;
```

**Storage**: SoA arrays for AABB:
```pascal
var
  ColliderX: array[0..MAX_ENTITIES-1] of integer;
  ColliderY: array[0..MAX_ENTITIES-1] of integer;
  ColliderW: array[0..MAX_ENTITIES-1] of integer;
  ColliderH: array[0..MAX_ENTITIES-1] of integer;
  ColliderSolid: array[0..MAX_ENTITIES-1] of boolean;
```

### 2.3 Scene Types

#### TScene

```pascal
type
  TScene = class
  public
    procedure Init;
    procedure Update;
    procedure Render;
    function AddEntity: TEntityID;
    procedure RemoveEntity(id: TEntityID);
    procedure Clear;
  end;
```

#### TGame

```pascal
type
  TGame = class
  private
    CurrentScene: TScene;
  public
    procedure Init;
    procedure Run;
    procedure SwitchScene(scene: TScene);
    property Scene: TScene read CurrentScene;
  end;
```

### 2.4 Animation Types

#### TKeyframe

```pascal
type
  TKeyframe = record
    TileID: byte;
    Duration: byte;  // Duration in frames
  end;
```

#### TAnimation

```pascal
type
  TAnimation = record
    Frames: ^TKeyframe;
    Count: byte;
    Loop: boolean;
  end;
```

#### TAnimationState

```pascal
type
  TAnimationState = (asIdle, asRun, asJump, asFall, asLand);
  
  TStateAnim = record
    State: TAnimationState;
    Anim: TAnimation;
  end;
```

**Storage**: SoA arrays:
```pascal
var
  AnimCurrentFrame: array[0..MAX_ENTITIES-1] of byte;
  AnimTime: array[0..MAX_ENTITIES-1] of byte;
  AnimState: array[0..MAX_ENTITIES-1] of TAnimationState;
  AnimPtr: array[0..MAX_ENTITIES-1] of ^TAnimation;
```

### 2.5 Physics Types

#### TPhysicsBody

```pascal
type
  TPhysicsBody = record
    Mass: integer;        // Mass (for future use)
    Friction: integer;     // Friction coefficient (fixed-point)
    Gravity: integer;     // Gravity multiplier
    OnGround: boolean;    // Ground collision flag
  end;
```

**Storage**: SoA arrays:
```pascal
var
  PhysicsMass: array[0..MAX_ENTITIES-1] of integer;
  PhysicsFriction: array[0..MAX_ENTITIES-1] of integer;
  PhysicsGravity: array[0..MAX_ENTITIES-1] of integer;
  PhysicsOnGround: array[0..MAX_ENTITIES-1] of boolean;
```

---

## 3. Game Engine Intrinsics

### 3.1 Entity Management

```pascal
function EntityCreate: TEntityID; intrinsic;
procedure EntityDestroy(id: TEntityID); intrinsic;
function EntityValid(id: TEntityID): boolean; intrinsic;
```

### 3.2 Component Access

```pascal
procedure EntitySetPosition(id: TEntityID; x, y: integer); intrinsic;
procedure EntityGetPosition(id: TEntityID; var x, y: integer); intrinsic;
procedure EntitySetVelocity(id: TEntityID; vx, vy: integer); intrinsic;
procedure EntityGetVelocity(id: TEntityID; var vx, vy: integer); intrinsic;
procedure EntitySetSprite(id: TEntityID; spriteID: byte; tileID: word; palette: byte); intrinsic;
procedure EntitySetVisible(id: TEntityID; visible: boolean); intrinsic;
```

### 3.3 Collision Detection

```pascal
function CollisionCheck(id1, id2: TEntityID): boolean; intrinsic;
function CollisionCheckTile(id: TEntityID; tileX, tileY: integer): boolean; intrinsic;
procedure CollisionResolve(id1, id2: TEntityID); intrinsic;
```

### 3.4 Animation

```pascal
procedure EntitySetAnimation(id: TEntityID; anim: ^TAnimation); intrinsic;
procedure EntitySetAnimationState(id: TEntityID; state: TAnimationState); intrinsic;
procedure AnimationUpdate(id: TEntityID); intrinsic;
```

### 3.5 Physics

```pascal
procedure PhysicsApplyGravity(id: TEntityID); intrinsic;
procedure PhysicsApplyVelocity(id: TEntityID); intrinsic;
procedure PhysicsApplyFriction(id: TEntityID); intrinsic;
```

### 3.6 Scene Management

```pascal
procedure SceneUpdate(scene: TScene); intrinsic;
procedure SceneRender(scene: TScene); intrinsic;
```

---

## 4. ECS Archetype System

### 4.1 Archetype Definitions

SuperPascal supports compile-time archetype definitions:

```pascal
type
  TArchetype = (atDynamic, atStatic, atVisual, atLogic, atParticle);
  
const
  ARCHETYPE_DYNAMIC = [cpPosition, cpVelocity, cpSprite, cpCollider];
  ARCHETYPE_STATIC  = [cpPosition, cpCollider];
  ARCHETYPE_VISUAL  = [cpPosition, cpSprite];
  ARCHETYPE_LOGIC   = [cpPosition, cpScript];
  ARCHETYPE_PARTICLE = [cpPosition, cpVelocity, cpLifetime];
```

### 4.2 Component Flags

```pascal
type
  TComponentFlag = (
    cfPosition,
    cfVelocity,
    cfSprite,
    cfCollider,
    cfAnimation,
    cfPhysics,
    cfScript
  );
  
  TComponentFlags = set of TComponentFlag;
```

### 4.3 Entity Component Masks

Each entity has a component mask:

```pascal
var
  EntityComponentMask: array[0..MAX_ENTITIES-1] of TComponentFlags;
```

### 4.4 Archetype Lists

Entities are grouped by archetype:

```pascal
var
  DynamicEntities: array[0..MAX_ENTITIES-1] of TEntityID;
  DynamicCount: word;
  
  StaticEntities: array[0..MAX_ENTITIES-1] of TEntityID;
  StaticCount: word;
  
  VisualEntities: array[0..MAX_ENTITIES-1] of TEntityID;
  VisualCount: word;
```

---

## 5. System Schedule

The game engine defines a fixed system execution order:

```pascal
procedure GameLoop;
begin
  // 1. Input system
  InputSystem.Update;
  
  // 2. Scripting system
  ScriptSystem.Update;
  
  // 3. Audio system
  AudioSystem.Update;
  
  // 4. Physics system
  PhysicsSystem.Update;
  
  // 5. Collision system
  CollisionSystem.Update;
  
  // 6. Animation system
  AnimationSystem.Update;
  
  // 7. AI system
  AISystem.Update;
  
  // 8. Scene transition system
  SceneTransitionSystem.Update;
  
  // 9. Render preparation
  RenderPrepSystem.Update;
  
  // 10. Rendering
  RenderSystem.Update;
  
  // 11. Audio streaming
  AudioStreamingSystem.Update;
end;
```

---

## 6. Change Detection

### 6.1 Change Flags

Each entity has change detection flags:

```pascal
type
  TChangeFlag = (
    chPosition,
    chSprite,
    chState,
    chVelocity
  );
  
  TChangeFlags = set of TChangeFlag;
  
var
  EntityChangeMask: array[0..MAX_ENTITIES-1] of TChangeFlags;
```

### 6.2 Change Detection Usage

Systems can skip unchanged entities:

```pascal
procedure RenderSystem;
var i: word;
    id: TEntityID;
begin
  for i := 0 to VisualCount - 1 do
  begin
    id := VisualEntities[i];
    if not (chPosition in EntityChangeMask[id]) and
       not (chSprite in EntityChangeMask[id]) then
      Continue;  // Skip unchanged entity
    
    // Render entity
    RenderEntity(id);
  end;
end;
```

### 6.3 Change Flag Clearing

Change flags are cleared at end of frame:

```pascal
procedure ClearChangeFlags;
var i: word;
begin
  for i := 0 to MAX_ENTITIES - 1 do
    EntityChangeMask[i] := [];
end;
```

---

## 7. Standard Library Units

### 7.1 Engine_ECS Unit

```pascal
unit Engine_ECS;

interface
  // Entity management
  function CreateEntity: TEntityID;
  procedure DestroyEntity(id: TEntityID);
  
  // Component management
  procedure AddComponent(id: TEntityID; component: TComponentFlag);
  procedure RemoveComponent(id: TEntityID; component: TComponentFlag);
  function HasComponent(id: TEntityID; component: TComponentFlag): boolean;
  
  // Archetype queries
  function GetEntities(archetype: TArchetype): ^TEntityID;
  function GetEntityCount(archetype: TArchetype): word;
  
implementation
end.
```

### 7.2 Engine_Scene Unit

```pascal
unit Engine_Scene;

interface
  type
    TScene = class
      procedure Init;
      procedure Update;
      procedure Render;
      function AddEntity: TEntityID;
      procedure RemoveEntity(id: TEntityID);
    end;
    
implementation
end.
```

### 7.3 Engine_Physics Unit

```pascal
unit Engine_Physics;

interface
  // Physics constants
  const
    GRAVITY_DEFAULT = 1;  // Pixels per frame squared
    FRICTION_DEFAULT = $E0;  // Fixed-point (0.875)
    
  // Physics functions
  procedure ApplyGravity(id: TEntityID);
  procedure ApplyVelocity(id: TEntityID);
  procedure ApplyFriction(id: TEntityID);
  function CheckGround(id: TEntityID): boolean;
  
implementation
end.
```

### 7.4 Engine_Collision Unit

```pascal
unit Engine_Collision;

interface
  // Collision types
  type
    TCollisionType = (ctNone, ctAABB, ctCircle, ctTile);
    
  // Collision functions
  function CheckCollision(id1, id2: TEntityID): boolean;
  function CheckTileCollision(id: TEntityID; tileX, tileY: integer): boolean;
  procedure ResolveCollision(id1, id2: TEntityID);
  
implementation
end.
```

### 7.5 Engine_Animation Unit

```pascal
unit Engine_Animation;

interface
  // Animation functions
  procedure SetAnimation(id: TEntityID; anim: ^TAnimation);
  procedure SetAnimationState(id: TEntityID; state: TAnimationState);
  procedure UpdateAnimation(id: TEntityID);
  function IsAnimationComplete(id: TEntityID): boolean;
  
implementation
end.
```

---

## 8. Compiler Optimizations for ECS

### 8.1 Loop Unrolling

The compiler recognizes ECS iteration patterns:

```pascal
{$UNROLL}
for i := 0 to DynamicCount - 1 do
begin
  id := DynamicEntities[i];
  PositionX[id] := PositionX[id] + VelocityX[id];
  PositionY[id] := PositionY[id] + VelocityY[id];
end;
```

### 8.2 Inlining

Small component access functions are inlined:

```pascal
{$INLINE}
function GetPositionX(id: TEntityID): integer;
begin
  Result := PositionX[id];
end;
```

### 8.3 SoA Optimization

The compiler optimizes Structure-of-Arrays access patterns:

```pascal
// Optimized: single loop, cache-friendly
for i := 0 to Count - 1 do
begin
  PositionX[i] := PositionX[i] + VelocityX[i];
  PositionY[i] := PositionY[i] + VelocityY[i];
end;
```

---

## 9. Performance Guidelines

### 9.1 Entity Limits

- **Maximum entities**: 256 (configurable)
- **Active entities per frame**: Target 100-200
- **Sprite budget**: 64 sprites stable, 96 stretch goal

### 9.2 System Performance

- **Update loop**: < 60% of frame time
- **Render loop**: < 30% of frame time
- **VBlank sync**: Remaining margin

### 9.3 Memory Usage

- **Component arrays**: Allocated in dedicated pages
- **Entity pools**: Reuse entities via free lists
- **Change masks**: 1 byte per entity

---

## 10. Example Usage

### 10.1 Simple Entity Creation

```pascal
program GameDemo;
uses Engine_ECS, Engine_Scene, Engine_Physics;

var
  player: TEntityID;
  x, y: integer;
  
begin
  // Create player entity
  player := CreateEntity;
  AddComponent(player, cfPosition);
  AddComponent(player, cfVelocity);
  AddComponent(player, cfSprite);
  AddComponent(player, cfCollider);
  
  // Set initial position
  PositionX[player] := 100;
  PositionY[player] := 100;
  
  // Set sprite
  SpriteID[player] := 0;
  TileID[player] := 42;
  Visible[player] := true;
  
  // Game loop
  while true do
  begin
    // Update physics
    ApplyGravity(player);
    ApplyVelocity(player);
    
    // Update sprite position
    EntitySetPosition(player, PositionX[player], PositionY[player]);
    
    WaitVBlank;
  end;
end.
```

### 10.2 ECS System Example

```pascal
procedure MovementSystem;
var i: word;
    id: TEntityID;
begin
  for i := 0 to DynamicCount - 1 do
  begin
    id := DynamicEntities[i];
    PositionX[id] := PositionX[id] + VelocityX[id];
    PositionY[id] := PositionY[id] + VelocityY[id];
    EntityChangeMask[id] := EntityChangeMask[id] + [chPosition];
  end;
end;
```

---

## 11. Platform-Specific Hardware Integration

The game engine integrates with platform-specific hardware through abstractions:

### 11.1 Naming Convention

Platform-specific hardware types and intrinsics use the pattern:
- `<Concept>Hardware<Platform>`

**Examples:**
- `TSpriteHardwareZeal` - ZealZ80 ZVB sprite
- `TSpriteHardwareF256` - Foenix F256x sprite
- `TSpriteHardwareA2560` - Foenix A2560M sprite

**Intrinsics:**
- `EntitySetSpriteHardwareZeal(...)` - ZealZ80 implementation
- `EntitySetSpriteHardwareF256(...)` - Foenix F256x implementation
- `EntitySetSpriteHardwareA2560(...)` - Foenix A2560M implementation

### 11.2 Platform-Specific Implementations

See platform-specific game engine documentation:
- **[ZealZ80 Game Engine](../platforms/ZealZ80/gameEngine/README.md)** - ZVB hardware integration
- **[Platform Specifications](../platforms/README.md)** - All platform-specific implementations

### 11.3 Conditional Compilation Pattern

```pascal
{$IFDEF ZEALZ80}
uses Engine_ECS_Zeal, Engine_Hardware_Zeal;
{$ELSEIF DEFINED(FOENIX65C816)}
uses Engine_ECS_F256, Engine_Hardware_F256;
{$ELSEIF DEFINED(FOENIXA2560M)}
uses Engine_ECS_A2560, Engine_Hardware_A2560;
{$ENDIF}
```

---

## 12. Summary

The SuperPascal Game Engine provides:

- **ECS architecture**: Structure-of-Arrays component storage (platform-agnostic)
- **Static archetypes**: Compile-time entity grouping (platform-agnostic)
- **Change detection**: Efficient rendering optimization (platform-agnostic)
- **System schedule**: Fixed execution order (platform-agnostic)
- **Performance targets**: 60 FPS with 100-200 entities (platform-configurable)
- **Platform abstractions**: Hardware-specific features through consistent naming

This enables high-performance game development across multiple retro computing platforms while maintaining clean, educational Pascal code.

**See also:**
- **[General Game Engine Concepts](./09_GameEngine_Concepts.md)** - Platform-agnostic concepts
- **[Platform Specifications](../platforms/README.md)** - Hardware-specific implementations

---

**End of Game Engine Specification**

