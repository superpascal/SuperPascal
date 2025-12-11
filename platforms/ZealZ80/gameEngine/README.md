# ZealZ80 Platform — Game Engine Implementation

## Platform-Specific Game Engine Specification

**Platform:** ZealZ80 (Zeal 8-bit Computer)  
**Part of:** SuperPascal Platform-Specific Specifications

**See also:** [General Game Engine Concepts](../../languageSpecification/09_GameEngine_Concepts.md) for platform-agnostic game engine principles.

---

## Overview

This document specifies the **platform-specific implementation** of the SuperPascal Game Engine for ZealZ80, including integration with ZVB (Zeal Video Board) hardware.

**Note:** For general game engine concepts (ECS architecture, component types, system design), see [09_GameEngine_Concepts.md](../../languageSpecification/09_GameEngine_Concepts.md).

---

## 1. Hardware Integration

### 1.1 ZVB (Zeal Video Board) Integration

The ZealZ80 game engine integrates directly with ZVB hardware:

- **Sprites**: ZVB sprite controller (128 sprites, hardware-accelerated)
- **Tiles**: ZVB tilemap layers (Layer 0, Layer 1)
- **Palettes**: ZVB color palettes (256 colors)
- **Video Modes**: ZVB video mode configuration
- **DMA**: ZVB DMA for fast data transfer

### 1.2 Hardware-Specific Types

#### TSpriteHardwareZeal

```pascal
type
  TSpriteHardwareZeal = record
    SpriteID: byte;      // ZVB sprite index (0-127)
    TileID: word;        // Tile index in tileset
    Palette: byte;        // Palette index (0-255)
    Visible: boolean;     // Visibility flag
    Flags: byte;          // ZVB sprite flags (priority, flip, etc.)
  end;
```

**ZVB-specific fields:**
- `SpriteID`: 0-127 (ZVB sprite limit)
- `Flags`: ZVB sprite flags (priority bit, H-flip, V-flip)

#### Hardware Constants

```pascal
const
  ZVB_SPRITE_MAX = 127;           // Maximum ZVB sprites
  ZVB_TILE_MAX = 65535;           // Maximum tile index
  ZVB_PALETTE_MAX = 255;          // Maximum palette index
  ZVB_LAYER0 = 0;                 // Tilemap layer 0
  ZVB_LAYER1 = 1;                 // Tilemap layer 1
```

---

## 2. Platform-Specific Intrinsics

### 2.1 Hardware Sprite Management

#### EntitySetSpriteHardwareZeal

**Syntax:**
```pascal
procedure EntitySetSpriteHardwareZeal(id: TEntityID; spriteID: byte; tileID: word; palette: byte; flags: byte); intrinsic;
```

**Purpose**: Set ZVB sprite for entity.

**Parameters:**
- `id`: Entity ID
- `spriteID`: ZVB sprite index (0-127)
- `tileID`: Tile index in tileset
- `palette`: Palette index (0-255)
- `flags`: ZVB sprite flags

**Usage:**
```pascal
EntitySetSpriteHardwareZeal(player, 0, 42, 1, 0);
```

#### EntityUpdateSpriteHardwareZeal

**Syntax:**
```pascal
procedure EntityUpdateSpriteHardwareZeal(id: TEntityID); intrinsic;
```

**Purpose**: Update ZVB sprite position/state from entity position.

**Usage:**
```pascal
EntitySetPosition(player, 100, 100);
EntityUpdateSpriteHardwareZeal(player);  // Syncs sprite to position
```

### 2.2 ZVB Tilemap Integration

#### EntitySetTileHardwareZeal

**Syntax:**
```pascal
procedure EntitySetTileHardwareZeal(id: TEntityID; layer: byte; tileX, tileY: integer; tileID: word); intrinsic;
```

**Purpose**: Set tile in ZVB tilemap layer.

**Parameters:**
- `id`: Entity ID
- `layer`: ZVB layer (0 or 1)
- `tileX, tileY`: Tile coordinates
- `tileID`: Tile index

**Usage:**
```pascal
EntitySetTileHardwareZeal(background, ZVB_LAYER0, 10, 5, 100);
```

### 2.3 ZVB DMA Integration

#### EntityRenderBatchHardwareZeal

**Syntax:**
```pascal
procedure EntityRenderBatchHardwareZeal(const entities: array of TEntityID; count: word); intrinsic;
```

**Purpose**: Batch render entities using ZVB DMA.

**Usage:**
```pascal
EntityRenderBatchHardwareZeal(VisualEntities, VisualCount);
```

---

## 3. Standard Library Units

### 3.1 Engine_Hardware_Zeal Unit

**Purpose**: ZVB hardware abstraction layer.

**Exports:**
```pascal
unit Engine_Hardware_Zeal;

interface

// ZVB sprite management
procedure SpriteHardwareZeal_Set(id: byte; x, y: integer; tileID: word; palette: byte; flags: byte);
procedure SpriteHardwareZeal_Show(id: byte; visible: boolean);
procedure SpriteHardwareZeal_Update(id: byte; x, y: integer);

// ZVB tilemap management
procedure TilemapHardwareZeal_SetTile(layer: byte; x, y: integer; tileID: word);
procedure TilemapHardwareZeal_SetScroll(layer: byte; scrollX, scrollY: integer);

// ZVB palette management
procedure PaletteHardwareZeal_Load(paletteID: byte; const colors: array of word);

implementation
// Uses ZVB intrinsics
end.
```

### 3.2 Engine_ECS_Zeal Unit

**Purpose**: ZealZ80-specific ECS implementation.

**Exports:**
```pascal
unit Engine_ECS_Zeal;

interface

// Entity management (platform-specific implementation)
function EntityCreate: TEntityID;
procedure EntityDestroy(id: TEntityID);
function EntityValid(id: TEntityID): boolean;

// Component access (ZVB-integrated)
procedure EntitySetSpriteHardwareZeal(id: TEntityID; spriteID: byte; tileID: word; palette: byte; flags: byte);
procedure EntityUpdateSpriteHardwareZeal(id: TEntityID);

implementation
// Platform-specific ECS implementation
end.
```

---

## 4. Rendering Pipeline

### 4.1 ZVB Render System

**Typical render loop:**
```pascal
procedure RenderSystem;
var i: word;
    id: TEntityID;
begin
  // Wait for vblank
  WaitVBlank;
  
  // Update tilemap scroll
  ZVB_SetLayer0Scroll(ScrollX, ScrollY);
  
  // Render sprites
  for i := 0 to VisualCount - 1 do
  begin
    id := VisualEntities[i];
    if Visible[id] then
    begin
      EntityUpdateSpriteHardwareZeal(id);
    end;
  end;
end;
```

### 4.2 Performance Targets

- **60 FPS** target frame rate
- **100-200 entities** typical limit
- **128 sprites** maximum (ZVB hardware limit)
- **DMA batching** for efficient rendering

---

## 5. Memory Management

### 5.1 Component Storage

**SoA arrays allocated in Z80 memory:**
```pascal
var
  PositionX: array[0..MAX_ENTITIES-1] of integer absolute $8000;
  PositionY: array[0..MAX_ENTITIES-1] of integer absolute $8100;
  SpriteID: array[0..MAX_ENTITIES-1] of byte absolute $8200;
  // ... more components
```

**Platform-specific:** Memory layout, MMU page mapping

### 5.2 Entity Pool

**Entity pool management:**
- Dense array of entity states
- Free list for recycling
- Maximum 256 entities (configurable)

---

## 6. Integration Examples

### 6.1 Simple Sprite Entity

```pascal
program SpriteDemo;
{$IFDEF ZEALZ80}
uses Engine_ECS_Zeal, Engine_Hardware_Zeal;
{$ENDIF}

var player: TEntityID;

begin
  player := EntityCreate;
  EntitySetPosition(player, 100, 100);
  EntitySetSpriteHardwareZeal(player, 0, 42, 1, 0);
  
  while true do
  begin
    EntityUpdateSpriteHardwareZeal(player);
    WaitVBlank;
  end;
end.
```

### 6.2 ECS System with ZVB

```pascal
procedure RenderSystem;
var i: word;
    id: TEntityID;
begin
  WaitVBlank;
  
  for i := 0 to VisualCount - 1 do
  begin
    id := VisualEntities[i];
    if Visible[id] then
    begin
      // Update ZVB sprite from entity position
      EntityUpdateSpriteHardwareZeal(id);
    end;
  end;
end;
```

---

## See Also

- **[General Game Engine Concepts](../../languageSpecification/09_GameEngine_Concepts.md)** - Platform-agnostic game engine principles
- **[ZealZ80 Platform Overview](../README.md)** - Complete ZealZ80 platform specification
- **[ZVB Intrinsics](../intrinsics/01_ZVBIntrinsics.md)** - Direct ZVB hardware access

---

**End of ZealZ80 Game Engine Specification**

