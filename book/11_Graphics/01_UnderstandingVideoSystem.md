# Understanding the Video System

**Part of:** [Chapter 11: Graphics](./README.md)

---

> **For GCSE students:**  
> The video system is how your computer shows pictures on the screen. It draws one line at a time, from top to bottom, very fast. You need to wait for the right time (VBlank) to update what's shown.
>
> **For A-Level students:**  
> Video systems use raster scanning to display images. Understanding VBlank timing, video memory layout, and hardware registers is essential for smooth graphics. Different platforms (ZVB, VICKY) have different capabilities.
>
> **For University students:**  
> Video systems implement raster scanning with horizontal and vertical blanking periods. VBlank synchronization is critical for frame-accurate updates. Understanding video memory mapping, DMA, and hardware acceleration is essential for performance-critical graphics programming.

---

## Introduction

Before you can draw graphics, you need to understand how video systems work. Unlike modern computers with abstract graphics APIs, retro systems require direct interaction with video hardware. This chapter teaches you the fundamentals of video systems and how to use them in SuperPascal.

> **Connection to Part III:** If you haven't already, read [Chapter 16: Inside the Zeal Computer](../14_InsideTheZealComputer/README.md) for a conceptual introduction to the video system and FPGA hardware. This chapter provides the detailed implementation, while Part III provides the foundational concepts.

**Key concepts:**
- **Raster scanning** — How images are drawn on screen
- **Video modes** — Different ways to display graphics
- **VBlank** — Vertical blanking period (safe time to update)
- **Video memory** — Where graphics data is stored
- **Hardware registers** — How to control the video chip

---

## How Video Systems Work

### Raster Scanning

**Classic video systems use raster scanning:**

1. **Electron beam** (or equivalent) scans from top-left to bottom-right
2. **Horizontal lines** are drawn one at a time (left to right)
3. **Vertical refresh** happens 60 times per second (60 Hz)
4. **VBlank** occurs when the beam returns to the top (safe update time)

**Visual representation:**
```
Frame 1:  [==========]  ← Horizontal line
          [==========]
          [==========]
          ...
          [==========]
          [VBlank]     ← Safe to update
Frame 2:  [==========]
          ...
```

**Why this matters:**
- **During drawing**: Video chip is reading from video memory
- **During VBlank**: Safe to update video memory without flicker
- **Timing is critical**: Updates must happen at the right time

### Video Memory

**Video memory (VRAM) stores graphics data:**

- **Pixels** — Individual picture elements (dots on screen)
- **Tiles** — Reusable graphics patterns (8x8 or 16x16 pixels)
- **Sprites** — Movable graphics objects
- **Palette** — Color lookup table (maps indices to RGB colors)

**Memory layout:**
```
Video Memory:
- Palette RAM (256 colors)
- Tile data (tileset)
- Tilemap (which tiles go where)
- Sprite attribute table (sprite positions, tiles, flags)
- Font data (text mode)
```

**Accessing video memory:**
- Direct memory access (via `Peek`/`Poke` or pointers)
- DMA (Direct Memory Access) for bulk transfers
- Hardware registers for control

### VBlank (Vertical Blanking)

**VBlank is the period when the electron beam returns to the top:**

- **Duration**: ~1-2 milliseconds (depends on system)
- **Frequency**: 60 times per second (60 Hz)
- **Purpose**: Safe time to update video memory without flicker

**Why VBlank matters:**
- **During active display**: Updating video memory causes flicker/tearing
- **During VBlank**: Safe to update (screen is blank)
- **Synchronization**: `WaitVBlank` ensures updates happen at the right time

**Usage:**
```pascal
// Update graphics during VBlank
WaitVBlank;  // Wait for next VBlank
UpdateSprites;
UpdateTilemap;
// Changes take effect on next frame
```

---

## Platform-Specific Video Systems

### ZealZ80: ZVB (Zeal Video Board)

**ZVB is an FPGA-accelerated video chip:**

- **Resolutions**: 320x240 or 640x480
- **Color modes**: 4-bit (16 colors) or 8-bit (256 colors)
- **Features**: Tiles, sprites, scrolling layers, text mode
- **Memory**: Video RAM at physical address 0x100000
- **I/O**: Registers at 0x80-0xAF

**Video modes:**
```pascal
const
  ZVB_MODE_TEXT_640      = 0;  // Text mode 640x480
  ZVB_MODE_TEXT_320      = 1;  // Text mode 320x240
  ZVB_MODE_BITMAP_256    = 2;  // Bitmap 256-color
  ZVB_MODE_BITMAP_320    = 3;  // Bitmap 320-color
  ZVB_MODE_GFX_640_8BIT  = 4;  // GFX mode 640x480 8bpp
  ZVB_MODE_GFX_320_8BIT  = 5;  // GFX mode 320x240 8bpp (most common)
  ZVB_MODE_GFX_640_4BIT  = 6;  // GFX mode 640x480 4bpp
  ZVB_MODE_GFX_320_4BIT  = 7;  // GFX mode 320x240 4bpp
```

**Initialization:**
```pascal
// Set video mode
ZVB_SetVideoMode(ZVB_MODE_GFX_320_8BIT);

// Enable screen
ZVB_EnableScreen(true);

// Wait for VBlank to ensure mode change takes effect
WaitVBlank;
```

### Foenix65C816: VICKY Graphics Chip

**VICKY is a graphics controller chip:**

- **Variants**: TinyVicky, TinyVicky II, Vicky "The Fourth" (model-dependent)
- **Resolutions**: Varies by model
- **Features**: Text mode, graphics mode, bitmaps, tilemaps, sprites
- **Memory**: Memory-mapped I/O at 0xD000-0xDFFF
- **Registers**: Direct memory-mapped access

**Master control:**
```pascal
// Enable text mode and graphics mode
VICKY_SetMasterControl(
  TextMode := true,
  TextOverlay := false,
  GraphMode := true,
  Bitmap := false,
  TileMap := false,
  Sprite := false,
  Gamma := false,
  DisableVid := false
);
```

**Initialization:**
```pascal
// Set master control
VICKY_SetMasterControl(...);

// Set background color
VICKY_SetBackgroundColor(0, 0, 0);  // Black

// Enable screen (if needed)
// VICKY doesn't have explicit enable/disable
```

---

## Video Modes Explained

### Text Mode

**Text mode displays characters from a font:**

- **Resolution**: Character grid (e.g., 80x30 or 40x30)
- **Characters**: Fixed-size font (typically 8x8 or 8x16 pixels)
- **Colors**: Foreground and background colors per character
- **Use case**: Menus, HUD, text-based games

**Example:**
```pascal
// ZealZ80: Text mode
ZVB_SetVideoMode(ZVB_MODE_TEXT_320);
ZVB_MapPeripheral(ZVB_PERI_TEXT_IDX);
ZVB_TextSetCursor(10, 5);  // Row 10, Column 5
ZVB_TextSetColor(15, 0);   // White on black
ZVB_TextPrintChar('H');
ZVB_TextPrintChar('e');
ZVB_TextPrintChar('l');
ZVB_TextPrintChar('l');
ZVB_TextPrintChar('o');
```

### Graphics Mode

**Graphics mode displays pixels directly:**

- **Resolution**: Pixel grid (e.g., 320x240 or 640x480)
- **Color depth**: 4-bit (16 colors) or 8-bit (256 colors)
- **Memory**: Direct pixel data in video RAM
- **Use case**: Games, graphics applications

**Example:**
```pascal
// ZealZ80: Graphics mode
ZVB_SetVideoMode(ZVB_MODE_GFX_320_8BIT);
ZVB_EnableScreen(true);
WaitVBlank;  // Wait for mode change

// Now you can draw pixels directly to video memory
```

### Tile Mode

**Tile mode uses reusable graphics patterns:**

- **Tiles**: Small graphics patterns (8x8 or 16x16 pixels)
- **Tilemap**: Grid of tile indices (which tile goes where)
- **Memory efficient**: Store tiles once, reference many times
- **Use case**: Platformers, RPGs, tile-based games

**Example:**
```pascal
// Load tileset into video memory
// Set up tilemap (which tiles go where)
// Hardware automatically renders tiles
```

---

## Initializing the Video System

### Basic Initialization Pattern

**Every graphics program needs initialization:**

```pascal
program GraphicsDemo;

procedure InitGraphics;
begin
  // 1. Set video mode
  ZVB_SetVideoMode(ZVB_MODE_GFX_320_8BIT);
  
  // 2. Enable screen
  ZVB_EnableScreen(true);
  
  // 3. Wait for VBlank (ensures mode change takes effect)
  WaitVBlank;
  
  // 4. Initialize other graphics subsystems
  // (sprites, tilemaps, etc.)
end;

begin
  InitGraphics;
  
  // Main program loop
  while true do
  begin
    // Update graphics
    UpdateGame;
    
    // Wait for VBlank before next frame
    WaitVBlank;
  end;
end.
```

### Platform-Agnostic Initialization

**For multi-platform code, use conditional compilation:**

```pascal
{$IFDEF ZEALZ80}
procedure InitGraphics;
begin
  ZVB_SetVideoMode(ZVB_MODE_GFX_320_8BIT);
  ZVB_EnableScreen(true);
  WaitVBlank;
end;
{$ENDIF}

{$IFDEF FOENIX65C816}
procedure InitGraphics;
begin
  VICKY_SetMasterControl(
    TextMode := false,
    GraphMode := true,
    Bitmap := false,
    TileMap := false,
    Sprite := false,
    Gamma := false,
    DisableVid := false
  );
  VICKY_SetBackgroundColor(0, 0, 0);
end;
{$ENDIF}
```

---

## Video Memory Access

### Understanding Video Memory Layout

**Video memory is organized into regions:**

```pascal
// ZealZ80 ZVB video memory layout
const
  VID_MEM_PHYS_BASE      = $100000;  // Physical base address
  VID_MEM_LAYER0_OFFSET  = $0000;    // Layer 0 tilemap
  VID_MEM_PALETTE_OFFSET = $0E00;    // Palette RAM (256 colors)
  VID_MEM_LAYER1_OFFSET  = $1000;    // Layer 1 tilemap
  VID_MEM_SPRITE_OFFSET  = $2800;    // Sprite attribute table
  VID_MEM_FONT_OFFSET    = $3000;    // Font data
  VID_MEM_TILESET_OFFSET = $10000;   // Tileset data
```

### Accessing Video Memory

**Direct memory access (via pointers):**

```pascal
// Get video memory base address
var
  vramBase: dword;
  vramPtr: ^byte;
begin
  vramBase := ZVB_GetVideoMemBase;
  // Map to logical address space (MMU)
  // Access via pointer
  vramPtr := MapVideoMemory(vramBase);
  vramPtr^ := $FF;  // Write pixel
end;
```

**Using Peek/Poke (low-level):**

```pascal
// Access video memory via absolute addressing
var
  VRAM: array[0..76799] of byte absolute $C000;  // 320x240 = 76800 bytes
  
begin
  VRAM[0] := $FF;  // Set first pixel
end;
```

**DMA for bulk transfers:**

```pascal
// Copy sprite data to video memory
var
  spriteData: array[0..63] of byte;  // 8x8 sprite
  vramDest: pointer;
begin
  // Prepare sprite data
  // ... fill spriteData ...
  
  // Copy via DMA (fast)
  DMA_Copy(spriteData, vramDest^, SizeOf(spriteData));
end;
```

---

## Raster Position and Timing

### Reading Raster Position

**You can read the current scanline position:**

```pascal
// ZealZ80
var
  vpos, hpos: word;
begin
  vpos := ZVB_GetVPos;  // Vertical position (0-239 for 320 mode)
  hpos := ZVB_GetHPos;  // Horizontal position (0-319 for 320 mode)
  
  // Use for timing-sensitive operations
  if vpos < 100 then
    // Top of screen
  else if vpos > 200 then
    // Bottom of screen
end;
```

**Foenix65C816:**
```pascal
var
  x, y: word;
begin
  VICKY_GetRasterPos(x, y);  // Get current raster position
  // Use for timing
end;
```

### VBlank Detection

**Check if we're in VBlank:**

```pascal
// ZealZ80
if ZVB_IsVBlank then
begin
  // Safe to update video memory
  UpdateGraphics;
end;

// Check H-blank (horizontal blanking)
if ZVB_IsHBlank then
begin
  // Between scanlines
end;
```

---

## Common Video System Concepts

### Palettes

**Palettes map color indices to RGB values:**

```pascal
// ZealZ80: 256-color palette (RGB565 format)
// Each color is 2 bytes: RRRRRGGG GGGBBBBB
// Palette at offset $0E00 in video memory

// Set palette color 0 to red
var
  palettePtr: ^word;
begin
  palettePtr := GetPalettePtr(0);
  palettePtr^ := $F800;  // RGB565: Red = $F800
end;
```

### Sprites

**Sprites are movable graphics objects:**

- **Position**: X, Y coordinates on screen
- **Tile**: Which graphics pattern to use
- **Flags**: Flip, priority, visibility
- **Hardware**: Rendered by video chip (no CPU overhead)

**Example:**
```pascal
// Set sprite 0 at position (100, 50) with tile 42
ZVB_SpriteSetFull(0, 100, 50, 42, 0);

// Show sprite
ZVB_SpriteSetFlags(0, SPRITE_FLAG_VISIBLE);

// Hide sprite
ZVB_SpriteSetFlags(0, 0);
```

### Scrolling

**Scrolling moves the viewport over a larger background:**

```pascal
// Scroll layer 0
ZVB_SetLayer0Scroll(scrollX, scrollY);

// Smooth scrolling (update each frame)
scrollX := scrollX + 1;
if scrollX > 320 then
  scrollX := 0;
ZVB_SetLayer0Scroll(scrollX, scrollY);
```

---

## Best Practices

### 1. Always Wait for VBlank

**Update graphics during VBlank to avoid flicker:**

```pascal
// ✅ GOOD
WaitVBlank;
UpdateSprites;
UpdateTilemap;

// ❌ BAD (causes flicker)
UpdateSprites;  // Updating during active display
UpdateTilemap;
```

### 2. Initialize Before Use

**Set video mode before drawing:**

```pascal
// ✅ GOOD
ZVB_SetVideoMode(ZVB_MODE_GFX_320_8BIT);
ZVB_EnableScreen(true);
WaitVBlank;  // Wait for mode change
DrawGraphics;

// ❌ BAD (undefined behavior)
DrawGraphics;  // No video mode set yet
```

### 3. Use Appropriate Video Modes

**Choose the right mode for your needs:**

- **Text mode**: Menus, HUD, text-based games
- **Graphics mode**: Pixel-based graphics, games
- **Tile mode**: Tile-based games, efficient memory usage

### 4. Understand Memory Layout

**Know where video memory regions are:**

- Palette, tileset, tilemap, sprites all have specific locations
- Accessing wrong regions causes incorrect display
- Use constants for offsets (don't hardcode addresses)

---

## Exercises

### Exercise 1: Initialize Graphics

Write a program that:
1. Initializes the video system
2. Sets a graphics mode
3. Enables the screen
4. Waits for VBlank
5. Displays a message confirming initialization

### Exercise 2: Read Raster Position

Write a program that:
1. Initializes graphics
2. Reads the current raster position
3. Displays the position on screen (or via debug output)
4. Updates every frame

### Exercise 3: VBlank Detection

Write a program that:
1. Initializes graphics
2. Detects when VBlank occurs
3. Counts VBlank events
4. Displays the count (60 per second)

---

**Previous Chapter:** [Chapter 08: Units and Modular Programming](../10_UnitsAndModularProgramming/README.md)  
**Next Section:** [Drawing Shapes and Tiles](./02_DrawingShapesAndTiles.md)  
**Language Specification:** See [ZVB Intrinsics](../../languageSpecification/intrinsicsAndDirectives/02A_ZVBIntrinsics.md) and [VICKY Intrinsics](../../platforms/Foenix65C816/intrinsics/01_VICKYIntrinsics.md)  
**Last Updated:** 2025-01-XX

