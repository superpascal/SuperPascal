# SuperPascal Language Specification — Game Engine Concepts

## General Game Engine Concepts (Platform-Agnostic)

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

---

## Overview

This document defines **general Game Engine concepts and principles** that apply across all SuperPascal platforms. These concepts are platform-agnostic and describe the **what** and **why** of game engine design.

**Platform-specific implementations** (the **how**) are documented in:
- **[ZealZ80 Game Engine](../platforms/ZealZ80/GameEngine.md)** - ZVB-specific hardware integration
- **[Platform Specifications](../platforms/README.md)** - All platform-specific game engine implementations

---

## 1. Game Engine Architecture

### 1.1 Entity-Component-System (ECS)

**Concept:** ECS is a data-oriented architecture pattern that separates:
- **Entities** - Unique identifiers (IDs)
- **Components** - Data containers (no behavior)
- **Systems** - Behavior/logic that operates on components

**Benefits:**
- Cache-friendly (Structure-of-Arrays storage)
- Flexible composition (entities = component combinations)
- Efficient iteration (process all components of same type)
- Parallel-friendly (systems can run independently)

### 1.2 Structure-of-Arrays (SoA) Storage

**Concept:** Component data stored in separate arrays per field.

**Example:**
```pascal
// Instead of Array-of-Structures (AoS):
type TEntity = record
  X, Y: integer;
  SpriteID: byte;
end;
var entities: array[0..255] of TEntity;

// Use Structure-of-Arrays (SoA):
var
  PositionX: array[0..255] of integer;
  PositionY: array[0..255] of integer;
  SpriteID: array[0..255] of byte;
```

**Benefits:**
- Better cache locality when processing single component type
- SIMD-friendly (can process multiple values in parallel)
- Memory-efficient (no padding between components)

---

## 2. Core Game Engine Types (Platform-Agnostic)

### 2.1 Entity System

#### TEntityID

**Concept:** Entities are represented as integer IDs.

```pascal
type
  TEntityID = word;  // Platform-specific size (word, dword, etc.)
```

**Characteristics:**
- Dense IDs (0-based, sequential)
- Invalid entity = sentinel value (typically $FFFF or platform-specific)
- No entity object/pointer - just an ID

#### Entity Constants

```pascal
const
  ENTITY_NULL = $FFFF;  // Invalid entity ID (platform-specific)
  MAX_ENTITIES = 256;   // Maximum entities (platform-configurable)
```

**Platform-specific:** ID size, sentinel value, maximum count

### 2.2 Component Types (General Concepts)

#### Position Component

**Concept:** 2D position in game world.

```pascal
type
  TPosition = record
    X, Y: integer;  // Coordinates (fixed-point or integer)
  end;
```

**Platform-specific:** Coordinate system, fixed-point precision, range

#### Velocity Component

**Concept:** Movement velocity per frame.

```pascal
type
  TVelocity = record
    VX, VY: integer;  // Velocity in pixels per frame
  end;
```

**Platform-specific:** Units, precision, range

#### Sprite Component (Abstract)

**Concept:** Visual representation of entity.

```pascal
type
  TSprite = record
    SpriteID: byte;      // Hardware sprite index (platform-specific)
    TileID: word;        // Tile/graphic index
    Palette: byte;        // Color palette index
    Visible: boolean;     // Visibility flag
  end;
```

**Platform-specific:** 
- Sprite ID range (Zeal: 0-127, F256: platform-specific)
- Tile ID format
- Palette format
- Hardware-specific fields

**Naming Convention:** Platform-specific sprite types use suffix:
- `TSpriteHardwareZeal` - ZealZ80 ZVB sprite
- `TSpriteHardwareF256` - Foenix F256x sprite
- `TSpriteHardwareA2560` - Foenix A2560M sprite

#### Collider Component

**Concept:** Collision detection shape.

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

**Platform-specific:** Collision detection algorithm, precision

### 2.3 Scene Types

#### TScene

**Concept:** Container for entities and systems.

```pascal
type
  TScene = record
    Entities: array[0..MAX_ENTITIES-1] of TEntityID;
    EntityCount: word;
    // Platform-specific fields
  end;
```

**Platform-specific:** Storage format, system integration

#### TGame

**Concept:** Top-level game state container.

```pascal
type
  TGame = record
    CurrentScene: ^TScene;
    // Platform-specific fields
  end;
```

---

## 3. Game Engine Intrinsics (General Concepts)

### 3.1 Entity Management

**General intrinsics:**
```pascal
function EntityCreate: TEntityID; intrinsic;
procedure EntityDestroy(id: TEntityID); intrinsic;
function EntityValid(id: TEntityID): boolean; intrinsic;
```

**Platform-specific:** Implementation details, memory management

### 3.2 Component Access

**General intrinsics:**
```pascal
procedure EntitySetPosition(id: TEntityID; x, y: integer); intrinsic;
procedure EntityGetPosition(id: TEntityID; var x, y: integer); intrinsic;
procedure EntitySetVelocity(id: TEntityID; vx, vy: integer); intrinsic;
procedure EntityGetVelocity(id: TEntityID; var vx, vy: integer); intrinsic;
```

**Platform-specific:** Parameter sizes, coordinate system

### 3.3 Hardware-Specific Component Access

**Naming Convention:** Platform-specific intrinsics use suffix:
- `EntitySetSpriteHardwareZeal(...)` - ZealZ80 ZVB sprite
- `EntitySetSpriteHardwareF256(...)` - Foenix F256x sprite
- `EntitySetSpriteHardwareA2560(...)` - Foenix A2560M sprite

**Example:**
```pascal
// General sprite concept (platform-agnostic)
procedure EntitySetSprite(id: TEntityID; spriteID: byte; tileID: word; palette: byte); intrinsic;

// Platform-specific implementations
procedure EntitySetSpriteHardwareZeal(id: TEntityID; spriteID: byte; tileID: word; palette: byte); intrinsic;
procedure EntitySetSpriteHardwareF256(id: TEntityID; spriteID: byte; tileID: word; palette: byte); intrinsic;
```

### 3.4 Collision Detection

**General intrinsics:**
```pascal
function CollisionCheck(id1, id2: TEntityID): boolean; intrinsic;
function CollisionCheckTile(id: TEntityID; tileX, tileY: integer): boolean; intrinsic;
procedure CollisionResolve(id1, id2: TEntityID); intrinsic;
```

**Platform-specific:** Algorithm, precision, performance characteristics

### 3.5 Animation

**General intrinsics:**
```pascal
procedure EntitySetAnimation(id: TEntityID; anim: ^TAnimation); intrinsic;
procedure EntitySetAnimationState(id: TEntityID; state: TAnimationState); intrinsic;
procedure AnimationUpdate(id: TEntityID); intrinsic;
```

**Platform-specific:** Animation format, timing, interpolation

### 3.6 Physics

**General intrinsics:**
```pascal
procedure PhysicsApplyGravity(id: TEntityID); intrinsic;
procedure PhysicsApplyVelocity(id: TEntityID); intrinsic;
procedure PhysicsApplyFriction(id: TEntityID); intrinsic;
```

**Platform-specific:** Physics constants, precision, integration method

### 3.7 Scene Management

**General intrinsics:**
```pascal
procedure SceneUpdate(scene: TScene); intrinsic;
procedure SceneRender(scene: TScene); intrinsic;
```

**Platform-specific:** Rendering pipeline, hardware integration

---

## 4. ECS Archetype System

### 4.1 Archetype Concept

**Concept:** Predefined component combinations for compile-time optimization.

**General approach:**
- Define archetypes at compile time
- Entities grouped by archetype
- Systems iterate over archetype lists
- Compiler can optimize archetype-specific code paths

**Platform-specific:** Archetype storage, iteration performance

### 4.2 Component Flags

**Concept:** Bit flags for component presence.

```pascal
type
  TComponentFlag = (cpPosition, cpVelocity, cpSprite, cpCollider, ...);
  TComponentFlags = set of TComponentFlag;
```

**Platform-specific:** Flag count, storage format

---

## 5. System Schedule

### 5.1 System Execution Order

**Concept:** Fixed execution order for game systems.

**Typical order:**
1. Input system
2. Logic systems (AI, scripts)
3. Physics system
4. Collision system
5. Animation system
6. Render system

**Platform-specific:** System implementation, timing, synchronization

---

## 6. Change Detection

### 6.1 Change Flags Concept

**Concept:** Track which components changed to optimize rendering.

**General approach:**
- Each component has change flag
- Systems set flags when modifying components
- Render system only processes changed entities
- Flags cleared after rendering

**Platform-specific:** Flag storage, clearing strategy

---

## 7. Standard Library Units (General)

### 7.1 Engine_ECS Unit

**Concept:** Core ECS functionality.

**General exports:**
- Entity management functions
- Component access functions
- Archetype management

**Platform-specific:** Implementation details, hardware integration

### 7.2 Engine_Scene Unit

**Concept:** Scene management.

**General exports:**
- Scene creation/destruction
- Entity addition/removal
- Scene update/render

**Platform-specific:** Scene format, rendering pipeline

### 7.3 Engine_Physics Unit

**Concept:** Physics simulation.

**General exports:**
- Gravity application
- Velocity integration
- Friction application

**Platform-specific:** Physics constants, precision

### 7.4 Engine_Collision Unit

**Concept:** Collision detection.

**General exports:**
- AABB collision
- Circle collision
- Tile collision

**Platform-specific:** Algorithm, performance

### 7.5 Engine_Animation Unit

**Concept:** Animation playback.

**General exports:**
- Animation loading
- Animation playback
- State management

**Platform-specific:** Animation format, timing

---

## 8. Compiler Optimizations for ECS

### 8.1 General Optimization Strategies

**Concepts:**
- Loop unrolling for small entity counts
- Inlining of component access
- SoA layout optimization
- Archetype-specific code generation

**Platform-specific:** Optimization effectiveness, code size trade-offs

---

## 9. Performance Guidelines

### 9.1 General Principles

**Concepts:**
- Entity limits (platform-configurable)
- System performance targets (FPS goals)
- Memory usage patterns
- Cache-friendly iteration

**Platform-specific:** Actual limits, performance characteristics

---

## 10. Platform Abstraction Strategy

### 10.1 Naming Conventions

**General pattern:** `<Concept>Hardware<Platform>`

**Examples:**
- `TSpriteHardwareZeal` - ZealZ80 sprite component
- `TSpriteHardwareF256` - Foenix F256x sprite component
- `TSpriteHardwareA2560` - Foenix A2560M sprite component

**Intrinsics:**
- `EntitySetSpriteHardwareZeal(...)`
- `EntitySetSpriteHardwareF256(...)`
- `EntitySetSpriteHardwareA2560(...)`

### 10.2 Conditional Compilation

**Pattern:**
```pascal
{$IFDEF ZEALZ80}
uses Engine_ECS_Zeal, Engine_Hardware_Zeal;
{$ELSEIF DEFINED(FOENIX65C816)}
uses Engine_ECS_F256, Engine_Hardware_F256;
{$ELSEIF DEFINED(FOENIXA2560M)}
uses Engine_ECS_A2560, Engine_Hardware_A2560;
{$ENDIF}
```

### 10.3 Abstraction Layers

**Three levels:**
1. **General concepts** (this document) - Platform-agnostic
2. **Platform abstractions** - Platform-specific but hardware-abstracted
3. **Hardware intrinsics** - Direct hardware access

**Example:**
```pascal
// Level 1: General concept
procedure EntitySetSprite(id: TEntityID; spriteID: byte; tileID: word);

// Level 2: Platform abstraction
procedure EntitySetSpriteHardwareZeal(id: TEntityID; spriteID: byte; tileID: word);

// Level 3: Hardware intrinsic
ZVB_SpriteSetFull(spriteID, x, y, tileID, flags);
```

---

## 11. Cross-Platform Game Development

### 11.1 Portable Game Code

**Strategy:**
- Use general ECS concepts
- Platform-specific code in separate units
- Conditional compilation for platform differences
- Hardware abstraction layer

### 11.2 Platform-Specific Features

**Each platform has unique capabilities:**
- **ZealZ80**: ZVB graphics, MMU memory management
- **Foenix65C816**: IO Pages, VICKY graphics, OPL3 audio
- **FoenixA2560M**: VICKY III, PCIe, multiple audio chips

**Design principle:** Expose platform features through consistent abstractions.

---

## See Also

- **[Platform-Specific Game Engine Implementations](../platforms/README.md)** - Hardware-specific game engine APIs
- **[ZealZ80 Game Engine](../platforms/ZealZ80/GameEngine.md)** - Complete ZVB game engine specification
- **[Language Specification Overview](./00_Overview.md)** - Complete specification index

---

**End of Game Engine Concepts Specification**

