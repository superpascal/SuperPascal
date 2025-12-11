# Graphics Library

**Location:** `lib/graphics/`

**Main Module:** `mod.pas`

---

## Overview

The Graphics library provides algorithms for rendering graphics primitives, polygons, circles, textures, and tilemaps. All algorithms are **generic** and work on **all platforms** (no-FPU and FPU).

**Source Material:**
- Mikro Documentation Archive
- `algorithms/04_GraphicsAlgorithms.md`

---

## Modules

### `types.pas`
Core graphics types and constants:
- `TPlotPixelProc` - Platform-specific pixel plotting function
- `TPoint`, `TRect` - Basic geometry types
- `TColor` - Color type (platform-specific, typically 8-bit palette index)
- `TPolygon` - Polygon type
- `TTexture` - Texture type
- `TTile`, `TTilemap` - Tilemap types

### `line.pas`
Line drawing algorithms:
- `DrawLineBresenham` - Bresenham's line algorithm (integer-only, very fast)

**Source:** `BRESENHA.TXT`

### `polygon.pas`
Polygon rendering algorithms:
- `ScanConvertEdge` - Scan conversion for polygon edges
- `FillPolygon` - Fill polygon using scan conversion
- `DrawPolygonOutline` - Draw polygon wireframe

**Source:** `OTMPOLY.TXT`

### `circle.pas`
Circle drawing algorithms:
- `DrawCircle` - Draw circle outline (midpoint algorithm)
- `FillCircle` - Fill circle (filled circle)

### `ellipse.pas`
Ellipse drawing algorithms:
- `DrawEllipse` - Draw ellipse outline (midpoint algorithm)
- `FillEllipse` - Fill ellipse (filled ellipse)

### `texture.pas`
Texture mapping algorithms:
- `DrawTexturedPolygon` - Basic texture mapping (affine mapping)
- `DrawTiledTexture` - Tiled texture mapping (repeating texture)

**Source:** `TEXTURE.TXT`, `TILETMAP.TXT`

### `tilemap.pas`
Tilemap rendering algorithms:
- `GetTile` - Get tile at tilemap coordinates
- `SetTile` - Set tile at tilemap coordinates
- `RenderTilemap` - Render tilemap to screen with camera support

**Source:** `TILETMAP.TXT`

---

## Usage

### Basic Example

```pascal
program GraphicsDemo;
uses Graphics;

var
  // Platform-specific pixel plotter
  procedure MyPlotPixel(x, y: integer; color: TColor);
  begin
    // Platform-specific implementation
    // e.g., VERA_SetPixel(x, y, color) for CommanderX16
  end;

begin
  // Draw a line
  DrawLineBresenham(10, 10, 100, 50, 1, MyPlotPixel);
  
  // Draw a circle
  DrawCircle(50, 50, 25, 2, MyPlotPixel);
  
  // Draw an ellipse
  DrawEllipse(100, 100, 40, 20, 3, MyPlotPixel);
  
  // Fill a polygon
  var vertices: array[0..2] of TPoint;
  vertices[0].X := 10; vertices[0].Y := 10;
  vertices[1].X := 50; vertices[1].Y := 10;
  vertices[2].X := 30; vertices[2].Y := 40;
  FillPolygon(vertices, 3, 3, MyPlotPixel);
end.
```

### Polygon Rendering

```pascal
var
  vertices: array[0..3] of TPoint;
  vertexCount: integer;
begin
  // Define rectangle
  vertices[0].X := 10; vertices[0].Y := 10;
  vertices[1].X := 50; vertices[1].Y := 10;
  vertices[2].X := 50; vertices[2].Y := 30;
  vertices[3].X := 10; vertices[3].Y := 30;
  vertexCount := 4;
  
  // Fill polygon
  FillPolygon(vertices, vertexCount, 5, MyPlotPixel);
  
  // Or draw outline
  DrawPolygonOutline(vertices, vertexCount, 1, MyPlotPixel);
end;
```

### Texture Mapping

```pascal
var
  texture: TTexture;
  vertices: array[0..2] of TPoint;
  texCoords: array[0..2] of TPoint;
begin
  // Setup texture (platform-specific)
  texture.Data := @MyTextureData;
  texture.Width := 64;
  texture.Height := 64;
  texture.Format := 0;  // Platform-specific format
  
  // Define triangle
  vertices[0].X := 10; vertices[0].Y := 10;
  vertices[1].X := 50; vertices[1].Y := 10;
  vertices[2].X := 30; vertices[2].Y := 40;
  
  // Define texture coordinates
  texCoords[0].X := 0; texCoords[0].Y := 0;
  texCoords[1].X := 63; texCoords[1].Y := 0;
  texCoords[2].X := 32; texCoords[2].Y := 63;
  
  // Draw textured polygon
  DrawTexturedPolygon(vertices, texCoords, 3, texture, MyPlotPixel);
end;
```

### Tilemap Rendering

```pascal
var
  tilemap: TTilemap;
  tiles: array[0..99] of TTile;  // 10x10 tilemap
  i: integer;
begin
  // Initialize tilemap
  tilemap.Tiles := @tiles;
  tilemap.Width := 10;
  tilemap.Height := 10;
  tilemap.TileWidth := 8;
  tilemap.TileHeight := 8;
  
  // Setup tiles (example)
  for i := 0 to 99 do
  begin
    tiles[i].TileID := i mod 16;
    tiles[i].Palette := 0;
    tiles[i].FlipX := False;
    tiles[i].FlipY := False;
  end;
  
  // Render tilemap with camera
  RenderTilemap(tilemap, 0, 0, 320, 200, MyPlotPixel);
end;
```

---

## Platform Considerations

### Pixel Plotting

All graphics functions require a platform-specific `TPlotPixelProc` callback:

```pascal
type
  TPlotPixelProc = procedure(x, y: integer; color: byte);
```

**Platform Examples:**
- **CommanderX16:** `VERA_SetPixel(x, y, color)`
- **Foenix65C816:** `VICKY_SetPixel(x, y, color)`
- **Raspberry Pi 5:** `FrameBuffer_SetPixel(x, y, color)`

### Texture Data Format

Texture data format is platform-specific. The `TTexture` type uses a `Pointer` for flexibility:

```pascal
type
  TTexture = record
    Data: Pointer;  // Platform-specific texture data
    Width: Word;
    Height: Word;
    Format: Byte;  // Texture format (platform-specific)
  end;
```

**Platform Examples:**
- **CommanderX16:** Palette indices (8-bit)
- **Foenix65C816:** VICKY tile format
- **Raspberry Pi 5:** RGB/RGBA format

### Tilemap Data Format

Similar to textures, tilemap data format is platform-specific:

```pascal
type
  TTilemap = record
    Tiles: Pointer;  // Array of TTile
    Width: Word;
    Height: Word;
    TileWidth: Byte;
    TileHeight: Byte;
  end;
```

---

## Algorithm Details

### Bresenham's Line Algorithm

- **Integer-only arithmetic** (no floating-point)
- **Very fast** (optimal for retro platforms)
- **Works for any slope**

### Polygon Scan Conversion

- **Fixed-point arithmetic** for edge calculations
- **Efficient** horizontal line filling
- **Handles concave polygons**

### Circle Midpoint Algorithm

- **8-way symmetry** (draws 8 pixels per iteration)
- **Integer-only arithmetic**
- **Optimal for retro platforms**

### Ellipse Midpoint Algorithm

- **4-way symmetry** (draws 4 pixels per iteration)
- **Integer-only arithmetic**
- **Handles different x and y radii**
- **Two regions** (steep and shallow slopes)

### Texture Mapping

- **Affine mapping** (simplified perspective)
- **Fixed-point arithmetic** for texture coordinates
- **Tiled texture support**

### Tilemap Rendering

- **Camera-based rendering** (only visible tiles rendered)
- **Tile flipping support** (horizontal/vertical)
- **Efficient** (only renders visible area)

---

## Dependencies

- `Math_Types` - For `Fixed16` type
- `Math_Fixed` - For fixed-point arithmetic

---

## Status

✅ **Complete** - All modules implemented:
- ✅ `types.pas` - Complete
- ✅ `line.pas` - Complete
- ✅ `polygon.pas` - Complete
- ✅ `circle.pas` - Complete
- ✅ `ellipse.pas` - Complete
- ✅ `texture.pas` - Complete
- ✅ `tilemap.pas` - Complete
- ✅ `mod.pas` - Complete

---

**Last Updated:** 2025-01-XX  
**Status:** Graphics library complete (8 modules: types, line, polygon, circle, ellipse, texture, tilemap, mod)

