# Creating Tilemaps

**Part of:** [Chapter 17: Tilemaps and Level Design](./README.md)

---

## Introduction

Tilemaps are grids of tiles that form game levels. This section teaches you how to create tilemaps using SuperPascal's tilemap DSL (Domain-Specific Language), load them into video memory, and use them in your games.

**Key concepts:**
- **Tilemaps** — Grids of tiles forming levels
- **Tilemap DSL** — Text-based tilemap definition
- **ROW function** — Define tilemap rows
- **Tile legends** — Character to tile mapping
- **Loading tilemaps** — Copying to video memory

---

## What are Tilemaps?

### Understanding Tilemaps

**A tilemap is a 2D grid of tile indices:**

```
Tilemap (10x8):
##########
#........#
#..####..#
#........#
#..####..#
#........#
#........#
##########

Each character represents a tile:
'#' = Wall tile (index 1)
'.' = Floor tile (index 0)
```

**Benefits of tilemaps:**
- **Memory efficient** — Store tile indices, not full graphics
- **Easy to edit** — Text-based, human-readable
- **Reusable tiles** — Same tile used many times
- **Fast rendering** — Hardware can render tilemaps

### Tilemap Structure

**Tilemap consists of:**
1. **Tileset** — Graphics patterns (8x8 or 16x16 pixels)
2. **Tilemap data** — Grid of tile indices
3. **Palette** — Color lookup table

**Visual representation:**
```
Tileset:        Tilemap:        Result:
[Wall]  (0)     [1][1][1]       [Wall][Wall][Wall]
[Floor] (1)     [1][0][0]  →    [Wall][Floor][Floor]
[Grass] (2)     [1][2][2]       [Wall][Grass][Grass]
```

---

## The Tilemap DSL

### Basic Syntax

**SuperPascal provides a DSL for defining tilemaps:**

```pascal
const
  Level1 = (
    Width: 10;
    Height: 8;
    Data: (
      ROW('##########'),
      ROW('#........#'),
      ROW('#..####..#'),
      ROW('#........#'),
      ROW('#..####..#'),
      ROW('#........#'),
      ROW('#........#'),
      ROW('##########')
    )
  );
```

**Structure:**
- `Width` — Tilemap width in tiles
- `Height` — Tilemap height in tiles
- `Data` — Array of `ROW()` strings

### ROW Function

**ROW defines one row of tiles:**

```pascal
ROW('##########')
```

**Rules:**
- String length must equal `Width`
- Each character represents one tile
- Characters map to tile indices via tile legend

**Example:**
```pascal
const
  Level1 = (
    Width: 5;
    Height: 3;
    Data: (
      ROW('#####'),  // Row 0: 5 wall tiles
      ROW('#...#'),  // Row 1: walls with floor in middle
      ROW('#####')   // Row 2: 5 wall tiles
    )
  );
```

---

## Tile Legends

### Character to Tile Mapping

**Tile legend maps characters to tile indices:**

```pascal
const
  TILE_FLOOR = 0;
  TILE_WALL = 1;
  TILE_GRASS = 2;
  TILE_WATER = 3;
  TILE_DOOR = 4;
  
  // Legend:
  // ' ' (space) = TILE_FLOOR (0)
  // '#' = TILE_WALL (1)
  // '.' = TILE_GRASS (2)
  // '~' = TILE_WATER (3)
  // 'D' = TILE_DOOR (4)
```

### Default Mapping

**If no legend is defined:**

- Space (` `) = Tile index 0
- Other characters = ASCII value

**Example:**
```pascal
ROW('ABC')
// 'A' = 65 (ASCII)
// 'B' = 66 (ASCII)
// 'C' = 67 (ASCII)
```

### Custom Mapping

**Define your own character mapping:**

```pascal
// Option 1: Use constants and comments
const
  TILE_WALL = 1;
  TILE_FLOOR = 0;
  // Legend: '#' = TILE_WALL, '.' = TILE_FLOOR

// Option 2: Use a mapping function (if available)
function CharToTile(ch: char): byte;
begin
  case ch of
    ' ': CharToTile := TILE_FLOOR;
    '#': CharToTile := TILE_WALL;
    '.': CharToTile := TILE_GRASS;
    '~': CharToTile := TILE_WATER;
    'D': CharToTile := TILE_DOOR;
  else
    CharToTile := 0;  // Default
  end;
end;
```

---

## Creating Tilemaps

### Simple Room

**Create a simple room:**

```pascal
const
  Room1 = (
    Width: 20;
    Height: 15;
    Data: (
      ROW('####################'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('####################')
    )
  );
```

### Platformer Level

**Create a platformer level:**

```pascal
const
  Level1 = (
    Width: 40;
    Height: 20;
    Data: (
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('                                        '),
      ROW('########################################')
    )
  );
```

### Complex Level

**Create a complex level with multiple tile types:**

```pascal
const
  Dungeon = (
    Width: 30;
    Height: 20;
    Data: (
      ROW('##############################'),
      ROW('#............................#'),
      ROW('#..######################....#'),
      ROW('#..#....................#....#'),
      ROW('#..#....................#....#'),
      ROW('#..#....................#....#'),
      ROW('#..#....................#....#'),
      ROW('#..#....................#....#'),
      ROW('#..#....................#....#'),
      ROW('#..#....................#....#'),
      ROW('#..#....................#....#'),
      ROW('#..#....................#....#'),
      ROW('#..#....................#....#'),
      ROW('#..#....................#....#'),
      ROW('#..######################....#'),
      ROW('#............................#'),
      ROW('#............................#'),
      ROW('#............................#'),
      ROW('#............................#'),
      ROW('##############################')
    )
  );
```

---

## Loading Tilemaps

### Converting DSL to Tilemap Data

**The compiler converts DSL to tilemap array:**

```pascal
// DSL definition
const
  Level1 = (
    Width: 10;
    Height: 8;
    Data: (
      ROW('##########'),
      ROW('#........#'),
      // ... etc
    )
  );

// Compiler generates (conceptually):
var
  Level1Data: array[0..7, 0..9] of byte;
  // Level1Data[0, 0] = TILE_WALL
  // Level1Data[0, 1] = TILE_WALL
  // Level1Data[1, 0] = TILE_WALL
  // Level1Data[1, 1] = TILE_FLOOR
  // ... etc
```

### Loading to Video Memory

**Copy tilemap data to video memory:**

```pascal
procedure LoadTilemap(const tilemap: TTilemap);
var
  tilemapBase: dword;
  tilemapPtr: ^byte;
  i, j: word;
begin
  tilemapBase := ZVB_GetVideoMemBase + VID_MEM_LAYER0_OFFSET;
  tilemapPtr := MapVideoMemory(tilemapBase);
  
  // Copy tilemap data
  for j := 0 to tilemap.Height - 1 do
    for i := 0 to tilemap.Width - 1 do
    begin
      tilemapPtr^ := tilemap.Data[j, i];
      tilemapPtr := tilemapPtr + 1;
    end;
end;
```

### Using DMA

**Use DMA for faster copying:**

```pascal
procedure LoadTilemapDMA(const tilemap: TTilemap);
var
  tilemapBase: dword;
  tilemapPtr: ^byte;
begin
  tilemapBase := ZVB_GetVideoMemBase + VID_MEM_LAYER0_OFFSET;
  tilemapPtr := MapVideoMemory(tilemapBase);
  
  // Copy via DMA (fast)
  DMA_Copy(tilemap.Data, tilemapPtr^, tilemap.Width * tilemap.Height);
end;
```

---

## Tilemap Access

### Getting Tile at Position

**Get tile index at specific position:**

```pascal
function GetTile(tilemap: TTilemap; x, y: word): byte;
begin
  if (x < tilemap.Width) and (y < tilemap.Height) then
    GetTile := tilemap.Data[y, x]
  else
    GetTile := 0;  // Out of bounds
end;
```

### Setting Tile at Position

**Set tile index at specific position:**

```pascal
procedure SetTile(var tilemap: TTilemap; x, y: word; tile: byte);
begin
  if (x < tilemap.Width) and (y < tilemap.Height) then
    tilemap.Data[y, x] := tile;
end;
```

### Converting World to Tile Coordinates

**Convert pixel coordinates to tile coordinates:**

```pascal
const
  TILE_SIZE = 8;  // 8x8 tiles

function WorldToTile(worldX, worldY: integer): TTileCoord;
begin
  WorldToTile.X := worldX div TILE_SIZE;
  WorldToTile.Y := worldY div TILE_SIZE;
end;

// Usage
var
  playerX, playerY: integer;
  tileX, tileY: word;
begin
  playerX := 100;
  playerY := 50;
  
  var coord := WorldToTile(playerX, playerY);
  tileX := coord.X;
  tileY := coord.Y;
  
  var tile := GetTile(level1, tileX, tileY);
  WriteLn('Player is on tile: ', tile);
end;
```

---

## Complete Tilemap Example

**Putting it all together:**

```pascal
program TilemapDemo;

const
  TILE_FLOOR = 0;
  TILE_WALL = 1;
  
  Level1 = (
    Width: 20;
    Height: 15;
    Data: (
      ROW('####################'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('#..................#'),
      ROW('####################')
    )
  );

procedure LoadTileset;
begin
  // Load tile graphics
  LoadTile(0, TILE_FLOOR_GRAPHIC);
  LoadTile(1, TILE_WALL_GRAPHIC);
end;

procedure LoadLevel;
begin
  // Load tilemap to video memory
  LoadTilemap(Level1);
end;

begin
  InitGraphics;
  LoadTileset;
  LoadLevel;
  
  while true do
  begin
    UpdateGame;
    WaitVBlank;
    RenderGame;
  end;
end.
```

---

## Best Practices

### 1. Use Meaningful Characters

**Choose characters that represent tiles:**

```pascal
// ✅ GOOD: Clear meaning
ROW('##########')  // '#' = wall
ROW('#........#')  // '.' = floor

// ❌ BAD: Unclear
ROW('AAAAAAAAAA')  // What does 'A' mean?
```

### 2. Keep Tilemaps Readable

**Format tilemaps for readability:**

```pascal
// ✅ GOOD: Clear structure
const
  Level1 = (
    Width: 20;
    Height: 15;
    Data: (
      ROW('####################'),
      ROW('#..................#'),
      // ... clear rows
    )
  );

// ❌ BAD: Hard to read
const
  Level1 = (
    Width: 20;
    Height: 15;
    Data: (
      ROW('####################'), ROW('#..................#'),
      // ... all on one line
    )
  );
```

### 3. Document Tile Legends

**Always document character to tile mapping:**

```pascal
// ✅ GOOD: Documented
const
  TILE_WALL = 1;
  TILE_FLOOR = 0;
  // Legend: '#' = TILE_WALL, '.' = TILE_FLOOR

// ❌ BAD: No documentation
const
  TILE_WALL = 1;
  TILE_FLOOR = 0;
  // What characters map to what?
```

### 4. Validate Tilemap Size

**Check tilemap dimensions:**

```pascal
// ✅ GOOD: Validate
if (tilemap.Width > MAX_TILEMAP_WIDTH) or
   (tilemap.Height > MAX_TILEMAP_HEIGHT) then
  WriteLn('Tilemap too large');

// ❌ BAD: No validation
// May cause memory issues
```

### 5. Use Constants for Tile Sizes

**Define tile size as constant:**

```pascal
// ✅ GOOD: Constant
const
  TILE_SIZE = 8;
  
function WorldToTile(worldX: integer): word;
begin
  WorldToTile := worldX div TILE_SIZE;
end;

// ❌ BAD: Magic number
function WorldToTile(worldX: integer): word;
begin
  WorldToTile := worldX div 8;  // What is 8?
end;
```

---

## Exercises

### Exercise 1: Basic Tilemap

Write a program that:
1. Defines a simple tilemap using the DSL
2. Loads tileset graphics
3. Loads tilemap to video memory
4. Displays the tilemap on screen

### Exercise 2: Multiple Tile Types

Write a program that:
1. Defines a tilemap with 3-4 different tile types
2. Creates a tile legend
3. Loads all tile graphics
4. Displays the tilemap correctly

### Exercise 3: Tilemap Access

Write a program that:
1. Creates a tilemap
2. Gets tile at specific coordinates
3. Sets tile at specific coordinates
4. Converts world coordinates to tile coordinates
5. Displays tile information

### Exercise 4: Dynamic Tilemap

Write a program that:
1. Creates a tilemap
2. Modifies tiles at runtime
3. Updates video memory when tiles change
4. Demonstrates dynamic level editing

---

**Previous Chapter:** [Chapter 16: Entity Component System](../16_EntityComponentSystem/README.md)  
**Next Section:** [Tile Collision Rules](./02_TileCollisionRules.md)  
**Language Specification:** See [Tilemap DSL](../../languageSpecification/intrinsicsAndDirectives/07_TilemapDSL.md)  
**Last Updated:** 2025-01-XX

