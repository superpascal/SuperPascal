# Drawing Shapes and Tiles

**Part of:** [Chapter 09: Graphics](./README.md)

---

## Introduction

Now that you understand how video systems work, it's time to draw graphics! This section teaches you how to:
- **Draw pixels** — The fundamental building block
- **Draw shapes** — Lines, rectangles, circles
- **Work with tiles** — Reusable graphics patterns
- **Use sprites** — Movable graphics objects
- **Manipulate palettes** — Control colors

**Key concepts:**
- **Video memory layout** — Where graphics data lives
- **Pixel addressing** — How to calculate memory addresses
- **Tile systems** — Efficient graphics storage
- **Sprite hardware** — Hardware-accelerated graphics

---

## Drawing Pixels

### Understanding Pixel Coordinates

**Screen coordinates:**
- **Origin**: Top-left corner (0, 0)
- **X-axis**: Increases rightward (0 to width-1)
- **Y-axis**: Increases downward (0 to height-1)
- **Example**: 320x240 mode → X: 0-319, Y: 0-239

**Visual representation:**
```
(0,0) ────────────────→ (319, 0)
  │
  │
  │
  │
  ↓
(0,239)              (319, 239)
```

### Calculating Pixel Address

**Formula for pixel address in video memory:**

```pascal
// For 320x240 8-bit mode
function GetPixelAddress(x, y: word): word;
begin
  GetPixelAddress := y * 320 + x;  // Row * width + column
end;
```

**Example:**
```pascal
// Pixel at (100, 50) in 320x240 mode
var
  address: word;
begin
  address := 50 * 320 + 100;  // = 16100
  // This is the byte offset in video memory
end;
```

### Drawing a Single Pixel

**Direct memory access method:**

```pascal
// ZealZ80: Access video memory directly
var
  VRAM: array[0..76799] of byte absolute $C000;  // 320x240 = 76800 bytes
  x, y: word;
  color: byte;
begin
  x := 100;
  y := 50;
  color := 15;  // White (palette index)
  
  VRAM[y * 320 + x] := color;  // Draw pixel
end;
```

**Using pointers:**

```pascal
var
  vramBase: dword;
  vramPtr: ^byte;
  x, y: word;
  color: byte;
begin
  vramBase := ZVB_GetVideoMemBase;
  // Map to logical address (MMU)
  vramPtr := MapVideoMemory(vramBase);
  
  x := 100;
  y := 50;
  color := 15;
  
  // Calculate offset and draw
  vramPtr := vramPtr + (y * 320 + x);
  vramPtr^ := color;
end;
```

### Helper Function for Drawing Pixels

**Create a reusable pixel drawing function:**

```pascal
procedure DrawPixel(x, y: word; color: byte);
var
  VRAM: array[0..76799] of byte absolute $C000;
begin
  if (x < 320) and (y < 240) then  // Bounds check
    VRAM[y * 320 + x] := color;
end;
```

**Usage:**
```pascal
DrawPixel(100, 50, 15);  // White pixel at (100, 50)
DrawPixel(101, 50, 15);  // White pixel at (101, 50)
```

---

## Drawing Shapes

### Drawing Lines

**Bresenham's line algorithm (simplified):**

```pascal
procedure DrawLine(x1, y1, x2, y2: integer; color: byte);
var
  dx, dy, sx, sy, err, e2: integer;
begin
  dx := Abs(x2 - x1);
  dy := Abs(y2 - y1);
  
  if x1 < x2 then sx := 1 else sx := -1;
  if y1 < y2 then sy := 1 else sy := -1;
  
  err := dx - dy;
  
  while (x1 <> x2) or (y1 <> y2) do
  begin
    DrawPixel(x1, y1, color);
    
    e2 := 2 * err;
    if e2 > -dy then
    begin
      err := err - dy;
      x1 := x1 + sx;
    end;
    if e2 < dx then
    begin
      err := err + dx;
      y1 := y1 + sy;
    end;
  end;
  DrawPixel(x2, y2, color);  // Draw endpoint
end;
```

**Usage:**
```pascal
DrawLine(10, 10, 100, 50, 15);  // White line
DrawLine(0, 0, 319, 239, 7);    // Diagonal across screen
```

### Drawing Rectangles

**Filled rectangle:**

```pascal
procedure DrawRect(x, y, w, h: word; color: byte);
var
  i, j: word;
begin
  for j := 0 to h - 1 do
    for i := 0 to w - 1 do
      DrawPixel(x + i, y + j, color);
end;
```

**Outlined rectangle:**

```pascal
procedure DrawRectOutline(x, y, w, h: word; color: byte);
var
  i: word;
begin
  // Top and bottom edges
  for i := 0 to w - 1 do
  begin
    DrawPixel(x + i, y, color);        // Top
    DrawPixel(x + i, y + h - 1, color); // Bottom
  end;
  
  // Left and right edges
  for i := 0 to h - 1 do
  begin
    DrawPixel(x, y + i, color);        // Left
    DrawPixel(x + w - 1, y + i, color); // Right
  end;
end;
```

**Usage:**
```pascal
DrawRect(50, 50, 100, 50, 15);        // Filled white rectangle
DrawRectOutline(50, 50, 100, 50, 7);  // Outlined gray rectangle
```

### Drawing Circles

**Midpoint circle algorithm:**

```pascal
procedure DrawCircle(xc, yc, radius: word; color: byte);
var
  x, y: integer;
  d: integer;
begin
  x := 0;
  y := radius;
  d := 3 - 2 * radius;
  
  while x <= y do
  begin
    // Draw 8 symmetric points
    DrawPixel(xc + x, yc + y, color);
    DrawPixel(xc - x, yc + y, color);
    DrawPixel(xc + x, yc - y, color);
    DrawPixel(xc - x, yc - y, color);
    DrawPixel(xc + y, yc + x, color);
    DrawPixel(xc - y, yc + x, color);
    DrawPixel(xc + y, yc - x, color);
    DrawPixel(xc - y, yc - x, color);
    
    if d < 0 then
      d := d + 4 * x + 6
    else
    begin
      d := d + 4 * (x - y) + 10;
      y := y - 1;
    end;
    x := x + 1;
  end;
end;
```

**Filled circle:**

```pascal
procedure DrawCircleFilled(xc, yc, radius: word; color: byte);
var
  x, y: integer;
  d: integer;
begin
  x := 0;
  y := radius;
  d := 3 - 2 * radius;
  
  while x <= y do
  begin
    // Draw horizontal lines for filled circle
    DrawLine(xc - x, yc + y, xc + x, yc + y, color);
    DrawLine(xc - x, yc - y, xc + x, yc - y, color);
    DrawLine(xc - y, yc + x, xc + y, yc + x, color);
    DrawLine(xc - y, yc - x, xc + y, yc - x, color);
    
    if d < 0 then
      d := d + 4 * x + 6
    else
    begin
      d := d + 4 * (x - y) + 10;
      y := y - 1;
    end;
    x := x + 1;
  end;
end;
```

**Usage:**
```pascal
DrawCircle(160, 120, 50, 15);        // Outlined circle
DrawCircleFilled(160, 120, 50, 7);  // Filled circle
```

---

## Working with Tiles

### What are Tiles?

**Tiles are reusable graphics patterns:**

- **Size**: Typically 8x8 or 16x16 pixels
- **Storage**: Stored once in tileset, referenced many times
- **Efficiency**: Saves memory (store pattern once, use many times)
- **Use case**: Backgrounds, level maps, repeating patterns

**Example:**
```
Tileset (stored once):
  Tile 0: [Ground]
  Tile 1: [Wall]
  Tile 2: [Grass]
  
Tilemap (references tiles):
  [1][1][1][1][1]
  [1][0][0][0][1]
  [1][0][2][0][1]
  [1][0][0][0][1]
  [1][1][1][1][1]
```

### Loading Tiles into Video Memory

**Copy tile data to tileset region:**

```pascal
// Define a tile (8x8 pixels, 8-bit color)
type
  TTile = array[0..63] of byte;  // 8x8 = 64 bytes

const
  TILE_GROUND: TTile = (
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $11, $11, $11, $11, $11, $11, $00,
    $00, $11, $22, $22, $22, $22, $11, $00,
    $00, $11, $22, $33, $33, $22, $11, $00,
    $00, $11, $22, $33, $33, $22, $11, $00,
    $00, $11, $22, $22, $22, $22, $11, $00,
    $00, $11, $11, $11, $11, $11, $11, $00,
    $00, $00, $00, $00, $00, $00, $00, $00
  );

procedure LoadTile(tileIndex: word; const tile: TTile);
var
  tilesetBase: dword;
  tilesetPtr: ^byte;
begin
  tilesetBase := ZVB_GetVideoMemBase + VID_MEM_TILESET_OFFSET;
  tilesetPtr := MapVideoMemory(tilesetBase);
  
  // Copy tile to tileset
  tilesetPtr := tilesetPtr + (tileIndex * 64);  // 64 bytes per tile
  DMA_Copy(tile, tilesetPtr^, SizeOf(TTile));
end;
```

**Usage:**
```pascal
LoadTile(0, TILE_GROUND);  // Load tile 0
```

### Setting Up a Tilemap

**Tilemap defines which tiles go where:**

```pascal
// Tilemap: 40x30 tiles (320x240 screen, 8x8 tiles)
type
  TTilemap = array[0..29, 0..39] of byte;  // [row][col]

var
  level1: TTilemap;

procedure InitLevel1;
var
  i, j: word;
begin
  // Fill with ground tiles
  for j := 0 to 29 do
    for i := 0 to 39 do
      level1[j, i] := 0;  // Ground tile
  
  // Add walls around edges
  for i := 0 to 39 do
  begin
    level1[0, i] := 1;   // Top wall
    level1[29, i] := 1;  // Bottom wall
  end;
  for j := 0 to 29 do
  begin
    level1[j, 0] := 1;   // Left wall
    level1[j, 39] := 1;  // Right wall
  end;
end;
```

**Copying tilemap to video memory:**

```pascal
procedure LoadTilemap(const tilemap: TTilemap);
var
  tilemapBase: dword;
  tilemapPtr: ^byte;
begin
  tilemapBase := ZVB_GetVideoMemBase + VID_MEM_LAYER0_OFFSET;
  tilemapPtr := MapVideoMemory(tilemapBase);
  
  // Copy tilemap to layer 0
  DMA_Copy(tilemap, tilemapPtr^, SizeOf(TTilemap));
end;
```

### Using the Tilemap DSL

**SuperPascal provides a tilemap DSL for easy level design:**

```pascal
const
  Level1 = (
    Width: 10;
    Height: 8;
    Data: (
      ROW('##########'),
      ROW('#........#'),
      ROW('#..####..#'),
      ROW('#..#  #..#'),
      ROW('#..#  #..#'),
      ROW('#..####..#'),
      ROW('#........#'),
      ROW('##########')
    )
  );
```

**Tile legend (character to tile mapping):**
```pascal
const
  TILE_WALL = 1;
  TILE_FLOOR = 0;
  TILE_DOOR = 2;
  
  // Legend: '#' = TILE_WALL, '.' = TILE_FLOOR, ' ' = TILE_DOOR
```

**The compiler converts the DSL to tilemap data automatically.**

---

## Working with Sprites

### What are Sprites?

**Sprites are movable graphics objects:**

- **Hardware-accelerated**: Rendered by video chip (no CPU overhead)
- **Position**: X, Y coordinates on screen
- **Tile**: Which graphics pattern to use
- **Flags**: Flip, priority, visibility
- **Count**: Limited by hardware (e.g., 128 sprites on ZVB)

### Setting Up a Sprite

**Basic sprite setup:**

```pascal
// Load sprite tile into tileset
LoadTile(42, SPRITE_PLAYER);  // Load player sprite as tile 42

// Configure sprite
ZVB_SpriteSetFull(
  0,        // Sprite index (0-127)
  100,      // X position
  50,       // Y position
  42,       // Tile index
  0         // Flags
);

// Show sprite
ZVB_SpriteSetFlags(0, SPRITE_FLAG_VISIBLE);
```

### Moving Sprites

**Update sprite position:**

```pascal
var
  spriteX, spriteY: word;
begin
  spriteX := 100;
  spriteY := 50;
  
  // Move sprite
  spriteX := spriteX + 2;  // Move right
  spriteY := spriteY + 1;  // Move down
  
  // Update sprite position
  ZVB_SpriteSetX(0, spriteX);
  ZVB_SpriteSetY(0, spriteY);
end;
```

**Animation by changing tile:**

```pascal
var
  frame: byte;
begin
  frame := 0;
  
  // Animate sprite (cycle through tiles)
  ZVB_SpriteSetTile(0, 42 + frame);  // Tiles 42, 43, 44, 45...
  frame := (frame + 1) mod 4;  // Cycle 0-3
end;
```

### Sprite Flags

**Control sprite appearance:**

```pascal
const
  SPRITE_FLAG_TILE_BIT9  = $01;  // 9th bit of tile index
  SPRITE_FLAG_BEHIND_FG  = $02;  // Render behind foreground
  SPRITE_FLAG_FLIP_Y     = $04;  // Flip vertically
  SPRITE_FLAG_FLIP_X     = $08;  // Flip horizontally
  // Upper nibble: Palette index (0-15)

// Flip sprite horizontally
ZVB_SpriteSetFlags(0, SPRITE_FLAG_FLIP_X);

// Flip both directions
ZVB_SpriteSetFlags(0, SPRITE_FLAG_FLIP_X or SPRITE_FLAG_FLIP_Y);

// Use different palette
ZVB_SpriteSetFlags(0, $10);  // Palette 1 (upper nibble)
```

### Multiple Sprites

**Managing multiple sprites:**

```pascal
type
  TSprite = record
    X, Y: word;
    Tile: byte;
    Visible: boolean;
  end;

var
  sprites: array[0..9] of TSprite;  // 10 sprites

procedure UpdateSprites;
var
  i: byte;
begin
  for i := 0 to 9 do
  begin
    if sprites[i].Visible then
    begin
      ZVB_SpriteSetFull(
        i,
        sprites[i].X,
        sprites[i].Y,
        sprites[i].Tile,
        0
      );
    end
    else
    begin
      ZVB_SpriteSetFlags(i, 0);  // Hide
    end;
  end;
end;
```

---

## Palette Manipulation

### Understanding Palettes

**Palettes map color indices to RGB values:**

- **Color index**: 0-255 (8-bit mode) or 0-15 (4-bit mode)
- **RGB value**: Actual color (RGB565 format: 16 bits)
- **Palette RAM**: Stored in video memory
- **Efficiency**: Change palette to change all colors instantly

**RGB565 format:**
```
Bit layout: RRRRR GGGGGG BBBBB
           15-11  10-5   4-0
```

### Setting Palette Colors

**ZealZ80 ZVB palette (RGB565, 256 colors):**

```pascal
procedure SetPaletteColor(index: byte; r, g, b: byte);
var
  paletteBase: dword;
  palettePtr: ^word;
  rgb565: word;
begin
  // Convert RGB888 to RGB565
  rgb565 := (r shr 3) shl 11 or  // Red: 5 bits
            (g shr 2) shl 5 or   // Green: 6 bits
            (b shr 3);            // Blue: 5 bits
  
  paletteBase := ZVB_GetVideoMemBase + VID_MEM_PALETTE_OFFSET;
  palettePtr := MapVideoMemory(paletteBase);
  
  // Set palette entry
  palettePtr := palettePtr + (index * 2);  // 2 bytes per color
  palettePtr^ := rgb565;
end;
```

**Usage:**
```pascal
SetPaletteColor(0, 0, 0, 0);      // Black
SetPaletteColor(15, 255, 255, 255); // White
SetPaletteColor(1, 255, 0, 0);    // Red
SetPaletteColor(2, 0, 255, 0);    // Green
SetPaletteColor(3, 0, 0, 255);    // Blue
```

### Predefined Color Constants

**Common colors (RGB565):**

```pascal
const
  COLOR_BLACK   = $0000;  // RGB(0, 0, 0)
  COLOR_WHITE   = $FFFF;  // RGB(255, 255, 255)
  COLOR_RED     = $F800;  // RGB(255, 0, 0)
  COLOR_GREEN   = $07E0;  // RGB(0, 255, 0)
  COLOR_BLUE    = $001F;  // RGB(0, 0, 255)
  COLOR_YELLOW  = $FFE0;  // RGB(255, 255, 0)
  COLOR_CYAN    = $07FF;  // RGB(0, 255, 255)
  COLOR_MAGENTA = $F81F;  // RGB(255, 0, 255)
```

### Palette Animation

**Animate colors for effects:**

```pascal
var
  paletteOffset: byte;
begin
  paletteOffset := 0;
  
  // Rotate palette colors
  RotatePalette(paletteOffset);
  paletteOffset := (paletteOffset + 1) mod 256;
end;
```

---

## Complete Drawing Example

**Putting it all together:**

```pascal
program GraphicsDemo;

procedure InitGraphics;
begin
  ZVB_SetVideoMode(ZVB_MODE_GFX_320_8BIT);
  ZVB_EnableScreen(true);
  WaitVBlank;
  
  // Set up palette
  SetPaletteColor(0, 0, 0, 0);      // Black background
  SetPaletteColor(15, 255, 255, 255); // White
  SetPaletteColor(1, 255, 0, 0);    // Red
  SetPaletteColor(2, 0, 255, 0);   // Green
  SetPaletteColor(3, 0, 0, 255);     // Blue
end;

procedure DrawScene;
begin
  // Draw background
  DrawRect(0, 0, 320, 240, 0);  // Black background
  
  // Draw shapes
  DrawRect(50, 50, 100, 50, 15);        // White rectangle
  DrawRectOutline(60, 60, 80, 30, 1);  // Red outline
  DrawCircle(160, 120, 50, 2);         // Green circle
  DrawLine(10, 10, 310, 230, 3);       // Blue diagonal line
  
  // Set up sprite
  LoadTile(42, SPRITE_PLAYER);
  ZVB_SpriteSetFull(0, 100, 50, 42, 0);
end;

begin
  InitGraphics;
  DrawScene;
  
  // Main loop
  while true do
  begin
    WaitVBlank;
    // Update graphics here
  end;
end.
```

---

## Best Practices

### 1. Update During VBlank

**Always update graphics during VBlank:**

```pascal
// ✅ GOOD
WaitVBlank;
UpdateSprites;
UpdateTilemap;

// ❌ BAD (causes flicker)
UpdateSprites;  // Updating during active display
```

### 2. Use DMA for Bulk Transfers

**Use DMA for copying large amounts of data:**

```pascal
// ✅ GOOD (fast)
DMA_Copy(tilemap, vramPtr^, SizeOf(tilemap));

// ❌ BAD (slow)
for i := 0 to SizeOf(tilemap) - 1 do
  vramPtr[i] := tilemap[i];
```

### 3. Reuse Tiles

**Store graphics patterns once, use many times:**

```pascal
// ✅ GOOD: Store tile once, reference many times
LoadTile(0, TILE_GROUND);
// Use tile 0 in tilemap many times

// ❌ BAD: Store same pattern multiple times
// Wastes memory
```

### 4. Batch Sprite Updates

**Update all sprites at once during VBlank:**

```pascal
// ✅ GOOD
WaitVBlank;
UpdateAllSprites;  // Update all at once

// ❌ BAD
UpdateSprite(0);
WaitVBlank;  // Unnecessary wait
UpdateSprite(1);
```

### 5. Use Appropriate Color Depth

**Choose color depth based on needs:**

- **4-bit (16 colors)**: Simple games, more sprites
- **8-bit (256 colors)**: Rich graphics, fewer sprites

---

## Exercises

### Exercise 1: Draw a Simple Shape

Write a program that:
1. Initializes graphics
2. Draws a filled rectangle
3. Draws a circle outline
4. Draws a diagonal line

### Exercise 2: Create a Tilemap

Write a program that:
1. Defines 3 tiles (ground, wall, grass)
2. Creates a 20x15 tilemap
3. Loads tiles and tilemap to video memory
4. Displays the tilemap

### Exercise 3: Animate a Sprite

Write a program that:
1. Loads a sprite with 4 animation frames
2. Displays the sprite on screen
3. Animates it by cycling through frames
4. Moves the sprite across the screen

### Exercise 4: Palette Effects

Write a program that:
1. Sets up a palette with multiple colors
2. Draws graphics using different palette indices
3. Animates the palette (color cycling)
4. Creates a rainbow effect

---

**Previous Section:** [Understanding the Video System](./01_UnderstandingVideoSystem.md)  
**Next Section:** [Animation Basics](./03_AnimationBasics.md)  
**Language Specification:** See [Graphics Intrinsics](../../languageSpecification/intrinsicsAndDirectives/02_GraphicsIntrinsics.md) and [ZVB Intrinsics](../../languageSpecification/intrinsicsAndDirectives/02A_ZVBIntrinsics.md)  
**Last Updated:** 2025-01-XX

