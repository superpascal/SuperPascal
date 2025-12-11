# ZealZ80 Platform — Game Engine Intrinsics

**Platform:** ZealZ80 (Zeal 8-bit Computer)  
**Part of:** [ZealZ80 Platform Intrinsics](./README.md)

**See also:** 
- [General Game Engine Concepts](../../../languageSpecification/09_GameEngine_Concepts.md) for platform-agnostic concepts
- [ZealZ80 Game Engine Implementation](../gameEngine/README.md) for complete game engine specification

---

### 10.1 Entity Management

#### EntityCreate

**Syntax:**
```pascal
function EntityCreate: TEntityID;
```

**Purpose**: Create a new entity and return its ID.

**Returns**: Entity ID (word) or `ENTITY_NULL` if pool exhausted.

**Usage:**
```pascal
var player: TEntityID;
player := EntityCreate;
```

#### EntityDestroy

**Syntax:**
```pascal
procedure EntityDestroy(id: TEntityID);
```

**Purpose**: Destroy an entity and free its components.

**Usage:**
```pascal
EntityDestroy(player);
```

#### EntityValid

**Syntax:**
```pascal
function EntityValid(id: TEntityID): boolean;
```

**Purpose**: Check if entity ID is valid and active.

**Usage:**
```pascal
if EntityValid(player) then
  // Use entity
```

### 10.2 Component Access

#### EntitySetPosition

**Syntax:**
```pascal
procedure EntitySetPosition(id: TEntityID; x, y: integer);
```

**Purpose**: Set entity position and mark position as changed.

**Usage:**
```pascal
EntitySetPosition(player, 100, 50);
```

#### EntityGetPosition

**Syntax:**
```pascal
procedure EntityGetPosition(id: TEntityID; var x, y: integer);
```

**Purpose**: Get entity position.

**Usage:**
```pascal
var x, y: integer;
EntityGetPosition(player, x, y);
```

#### EntitySetVelocity

**Syntax:**
```pascal
procedure EntitySetVelocity(id: TEntityID; vx, vy: integer);
```

**Purpose**: Set entity velocity.

**Usage:**
```pascal
EntitySetVelocity(player, 2, -5);  // Move right, jump up
```

#### EntitySetSpriteHardwareZeal

**Syntax:**
```pascal
procedure EntitySetSpriteHardwareZeal(id: TEntityID; spriteID: byte; tileID: word; palette: byte; flags: byte); intrinsic;
```

**Purpose**: Configure ZVB sprite for entity and mark sprite as changed.

**Parameters:**
- `id`: Entity ID
- `spriteID`: ZVB sprite index (0-127)
- `tileID`: Tile index in tileset
- `palette`: Palette index (0-255)
- `flags`: ZVB sprite flags (priority, H-flip, V-flip)

**Usage:**
```pascal
EntitySetSpriteHardwareZeal(player, 0, 42, 1, 0);  // Sprite 0, tile 42, palette 1, no flags
```

**Note:** This is the ZealZ80-specific implementation. For general sprite concepts, see [Game Engine Concepts](../../../languageSpecification/09_GameEngine_Concepts.md). Other platforms use `EntitySetSpriteHardwareF256` or `EntitySetSpriteHardwareA2560`.

### 10.3 Collision Detection

#### CollisionCheck

**Syntax:**
```pascal
function CollisionCheck(id1, id2: TEntityID): boolean;
```

**Purpose**: Check if two entities collide (AABB or circle).

**Returns**: `true` if collision detected.

**Usage:**
```pascal
if CollisionCheck(player, enemy) then
  HandleCollision;
```

#### CollisionCheckTile

**Syntax:**
```pascal
function CollisionCheckTile(id: TEntityID; tileX, tileY: integer): boolean;
```

**Purpose**: Check if entity collides with tile at tile coordinates.

**Usage:**
```pascal
if CollisionCheckTile(player, tileX, tileY) then
  HandleTileCollision;
```

### 10.4 Animation

#### EntitySetAnimation

**Syntax:**
```pascal
procedure EntitySetAnimation(id: TEntityID; anim: ^TAnimation);
```

**Purpose**: Set entity's current animation.

**Usage:**
```pascal
EntitySetAnimation(player, @PlayerRunAnim);
```

#### AnimationUpdate

**Syntax:**
```pascal
procedure AnimationUpdate(id: TEntityID);
```

**Purpose**: Update entity animation frame (called every frame).

**Usage:**
```pascal
AnimationUpdate(player);
```

### 10.5 Physics

#### PhysicsApplyGravity

**Syntax:**
```pascal
procedure PhysicsApplyGravity(id: TEntityID);
```

**Purpose**: Apply gravity to entity velocity.

**Usage:**
```pascal
PhysicsApplyGravity(player);
```

#### PhysicsApplyVelocity

**Syntax:**
```pascal
procedure PhysicsApplyVelocity(id: TEntityID);
```

**Purpose**: Apply velocity to entity position.

**Usage:**
```pascal
PhysicsApplyVelocity(player);
```

---

## Naming Convention

Platform-specific hardware intrinsics use the pattern: `<Concept>Hardware<Platform>`

**Examples:**
- `EntitySetSpriteHardwareZeal` - ZealZ80 ZVB sprite
- `EntitySetTileHardwareZeal` - ZealZ80 ZVB tilemap
- `EntityRenderBatchHardwareZeal` - ZealZ80 ZVB DMA batch rendering

**See also:**
- [General Game Engine Concepts](../../../languageSpecification/09_GameEngine_Concepts.md) for platform-agnostic concepts
- [ZealZ80 Game Engine Implementation](../gameEngine/README.md) for complete game engine specification
- [Game Engine Directives](./13_GameEngineDirectives.md) for ECS optimization directives

