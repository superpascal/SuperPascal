# Game Engine Intrinsics

**Part of:** [06_IntrinsicsAndDirectives.md](../06_IntrinsicsAndDirectives.md)

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

#### EntitySetSprite

**Syntax:**
```pascal
procedure EntitySetSprite(id: TEntityID; spriteID: byte; tileID: word; palette: byte);
```

**Purpose**: Configure entity sprite and mark sprite as changed.

**Usage:**
```pascal
EntitySetSprite(player, 0, 42, 1);  // Sprite 0, tile 42, palette 1
```

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

**See also:**
- [Game Engine Specification](../09_GameEngine.md) for complete engine API
- [Game Engine Directives](./13_GameEngineDirectives.md)

