# Graphics Algorithms

**Part of:** [Algorithms Appendix](../99_Algorithms_Appendix.md)

---

## Overview

Graphics algorithms for rendering, texture mapping, and visual effects. These algorithms are **generic** and work on **all platforms**.

**Source Material:** Mikro Documentation Archive

**See Also:**
- Book Chapter: [Chapter 20: Graphics Programming](../../book/20_GraphicsProgramming/README.md)
- Intrinsics: [Graphics Intrinsics](../intrinsicsAndDirectives/02_GraphicsIntrinsics.md)

---

## Line Drawing

**Source:** `docs/mikro_docs_archive/Coding/2/BRESENHA.TXT`

### Bresenham's Line Algorithm

**Algorithm:** Digital Differential Analyzer (DDA) for drawing lines on integer grid

**Key Features:**
- Integer-only arithmetic (no floating-point)
- Very fast
- Works for any slope

**Algorithm:**
```pascal
procedure DrawLine(x1, y1, x2, y2: integer; color: byte);
var
  deltaX, deltaY: integer;
  xChange, yChange: integer;
  error: integer;
  x, y, i, length: integer;
begin
  x := x1;
  y := y1;
  
  deltaX := x2 - x1;
  deltaY := y2 - y1;
  
  // Determine direction
  if deltaX < 0 then
  begin
    xChange := -1;
    deltaX := -deltaX;
  end
  else
    xChange := 1;
  
  if deltaY < 0 then
  begin
    yChange := -1;
    deltaY := -deltaY;
  end
  else
    yChange := 1;
  
  error := 0;
  i := 0;
  
  // Draw line
  if deltaX < deltaY then
  begin
    // Steep line (slope > 1)
    length := deltaY + 1;
    while i < length do
    begin
      PlotPixel(x, y, color);
      y := y + yChange;
      error := error + deltaX;
      
      if error > deltaY then
      begin
        x := x + xChange;
        error := error - deltaY;
      end;
      
      i := i + 1;
    end;
  end
  else
  begin
    // Shallow line (slope <= 1)
    length := deltaX + 1;
    while i < length do
    begin
      PlotPixel(x, y, color);
      x := x + xChange;
      error := error + deltaY;
      
      if error > deltaX then
      begin
        y := y + yChange;
        error := error - deltaX;
      end;
      
      i := i + 1;
    end;
  end;
end;
```

**Optimization:** Pre-calculate linear offset `y * width + x` and increment offset instead of recalculating each time.

---

## Polygon Rendering

**Source:** `docs/mikro_docs_archive/Coding/1/OTMPOLY.TXT`

### Scan Conversion

**Algorithm:** Determine edge coordinates for polygon filling

**Key Concept:** For horizontal scanlines, we need x value at each y coordinate

**Edge Equation:**
```
x = m(y - Y1) + X1
where m = (X2 - X1) / (Y2 - Y1)
```

**Incremental Calculation:**
```pascal
type
  TEdge = record
    X: Fixed16;      // Current X position
    XStep: Fixed16;  // X increment per scanline (dx/dy)
    YStart: integer;
    YEnd: integer;
  end;

procedure ScanConvertEdge(x1, y1, x2, y2: integer; var edge: TEdge);
var
  deltaX, deltaY: integer;
begin
  if y1 < y2 then
  begin
    edge.YStart := y1;
    edge.YEnd := y2;
    deltaX := x2 - x1;
    deltaY := y2 - y1;
  end
  else
  begin
    edge.YStart := y2;
    edge.YEnd := y1;
    deltaX := x1 - x2;
    deltaY := y1 - y2;
  end;
  
  if deltaY > 0 then
    edge.XStep := Fixed16Div(IntToFixed16(deltaX), IntToFixed16(deltaY))
  else
    edge.XStep := 0;
  
  edge.X := IntToFixed16(x1);
end;
```

### Polygon Filling

**Algorithm:** Fill polygon using edge table and scanline algorithm

```pascal
procedure FillPolygon(const vertices: array of TVector2; color: byte);
var
  edges: array of TEdge;
  edgeCount: integer;
  y, i: integer;
  leftX, rightX: Fixed16;
begin
  // Build edge table
  edgeCount := Length(vertices);
  SetLength(edges, edgeCount);
  
  for i := 0 to edgeCount - 1 do
  begin
    ScanConvertEdge(
      Round(vertices[i].X), Round(vertices[i].Y),
      Round(vertices[(i + 1) mod edgeCount].X),
      Round(vertices[(i + 1) mod edgeCount].Y),
      edges[i]
    );
  end;
  
  // Fill scanlines
  for y := 0 to ScreenHeight - 1 do
  begin
    // Find left and right edges for this scanline
    leftX := MaxFixed16;
    rightX := MinFixed16;
    
    for i := 0 to edgeCount - 1 do
    begin
      if (y >= edges[i].YStart) and (y < edges[i].YEnd) then
      begin
        if edges[i].X < leftX then
          leftX := edges[i].X;
        if edges[i].X > rightX then
          rightX := edges[i].X;
        
        // Update edge X for next scanline
        edges[i].X := edges[i].X + edges[i].XStep;
      end;
    end;
    
    // Draw horizontal line
    if leftX < rightX then
      DrawHorizontalLine(Round(leftX), Round(rightX), y, color);
  end;
end;
```

**Note:** This is a simplified version. Production code should use active edge table (AET) for efficiency.

---

## Texture Mapping

**Source:** `docs/mikro_docs_archive/Coding/1/TEXTURE.TXT`

### Basic Texture Mapping

**Algorithm:** Map texture coordinates to screen coordinates

**Key Concepts:**
- **Texture coordinates (u, v):** Range 0.0 to 1.0 (or 0 to texture width/height)
- **Perspective correction:** Required for 3D (not shown in basic version)
- **Bilinear filtering:** Smooth texture sampling (optional)

**Basic Algorithm:**
```pascal
procedure DrawTexturedPolygon(
  const vertices: array of TVector2;
  const texCoords: array of TVector2;
  const texture: array of byte;
  texWidth, texHeight: integer
);
var
  edges: array of TEdge;
  texEdges: array of TEdge;  // Texture coordinate edges
  y, i: integer;
  leftX, rightX: Fixed16;
  leftU, leftV, rightU, rightV: Fixed16;
  u, v, uStep, vStep: Fixed16;
  x: integer;
begin
  // Build edge tables (similar to polygon filling)
  // ... (omitted for brevity) ...
  
  // For each scanline
  for y := 0 to ScreenHeight - 1 do
  begin
    // Find left and right edges
    // ... (omitted) ...
    
    // Interpolate texture coordinates
    uStep := Fixed16Div(rightU - leftU, IntToFixed16(Round(rightX - leftX)));
    vStep := Fixed16Div(rightV - leftV, IntToFixed16(Round(rightX - leftX)));
    u := leftU;
    v := leftV;
    
    // Draw textured scanline
    for x := Round(leftX) to Round(rightX) do
    begin
      // Sample texture
      var texX := Round(Fixed16Mul(u, IntToFixed16(texWidth))) mod texWidth;
      var texY := Round(Fixed16Mul(v, IntToFixed16(texHeight))) mod texHeight;
      var texIndex := texY * texWidth + texX;
      
      PlotPixel(x, y, texture[texIndex]);
      
      u := u + uStep;
      v := v + vStep;
    end;
  end;
end;
```

### Tile Texture Mapping

**Source:** `docs/mikro_docs_archive/Coding/2/TILEMAP.TXT`

**Algorithm:** Optimized texture mapping using vertical cache tiles

**Key Optimization:** Organize texture as 4-pixel-wide vertical tiles for cache efficiency

```pascal
// Texture organized as vertical tiles (4 pixels wide)
// Each tile is texHeight pixels tall
// Total tiles = texWidth / 4

function SampleTiledTexture(
  u, v: Fixed16;
  const texture: array of byte;
  texWidth, texHeight: integer
): byte;
var
  tileIndex: integer;
  tileU, tileV: integer;
  pixelIndex: integer;
begin
  // Extract tile index (5 bits) and tile U (2 bits)
  tileIndex := (Round(u) shr 2) mod (texWidth div 4);
  tileU := Round(u) and 3;  // 0-3 within tile
  
  // V coordinate (7 bits for 128-pixel height)
  tileV := Round(v) mod texHeight;
  
  // Calculate pixel index: (tileIndex * 4 * texHeight) + (tileV * 4) + tileU
  pixelIndex := (tileIndex * 4 * texHeight) + (tileV * 4) + tileU;
  
  Result := texture[pixelIndex];
end;
```

---

## Tilemaps

**Source:** `docs/mikro_docs_archive/Coding/2/TILEMAP.TXT`

### Tilemap Rendering

**Algorithm:** Render 2D tile-based maps efficiently

```pascal
type
  TTileMap = record
    Tiles: array of byte;  // Tile indices
    Width, Height: integer;
    TileWidth, TileHeight: integer;
    TileSet: array of byte;  // Tile graphics data
  end;

procedure RenderTileMap(
  const map: TTileMap;
  cameraX, cameraY: integer;
  screenWidth, screenHeight: integer
);
var
  startTileX, startTileY: integer;
  endTileX, endTileY: integer;
  screenX, screenY: integer;
  tileX, tileY: integer;
  tileIndex: integer;
begin
  // Calculate visible tile range
  startTileX := cameraX div map.TileWidth;
  startTileY := cameraY div map.TileHeight;
  endTileX := (cameraX + screenWidth) div map.TileWidth + 1;
  endTileY := (cameraY + screenHeight) div map.TileHeight + 1;
  
  // Clamp to map bounds
  if startTileX < 0 then startTileX := 0;
  if startTileY < 0 then startTileY := 0;
  if endTileX > map.Width then endTileX := map.Width;
  if endTileY > map.Height then endTileY := map.Height;
  
  // Render visible tiles
  for tileY := startTileY to endTileY - 1 do
  begin
    for tileX := startTileX to endTileX - 1 do
    begin
      // Get tile index
      tileIndex := map.Tiles[tileY * map.Width + tileX];
      
      // Calculate screen position
      screenX := tileX * map.TileWidth - cameraX;
      screenY := tileY * map.TileHeight - cameraY;
      
      // Draw tile
      DrawTile(screenX, screenY, tileIndex, map.TileSet);
    end;
  end;
end;
```

### Scrolling Optimization

**Algorithm:** Use dirty rectangle or double buffering for smooth scrolling

```pascal
procedure ScrollTileMap(
  var map: TTileMap;
  deltaX, deltaY: integer
);
begin
  // Update camera position
  // Only redraw changed regions
  // Use dirty rectangle tracking for efficiency
end;
```

---

## Circle Drawing

### Midpoint Circle Algorithm

**Algorithm:** Draw circle using symmetry (8-way)

```pascal
procedure DrawCircle(centerX, centerY, radius: integer; color: byte);
var
  x, y: integer;
  d: integer;
begin
  x := 0;
  y := radius;
  d := 3 - 2 * radius;
  
  // Draw 8 symmetric points
  procedure Plot8Points(x, y: integer);
  begin
    PlotPixel(centerX + x, centerY + y, color);
    PlotPixel(centerX - x, centerY + y, color);
    PlotPixel(centerX + x, centerY - y, color);
    PlotPixel(centerX - x, centerY - y, color);
    PlotPixel(centerX + y, centerY + x, color);
    PlotPixel(centerX - y, centerY + x, color);
    PlotPixel(centerX + y, centerY - x, color);
    PlotPixel(centerX - y, centerY - x, color);
  end;
  
  while x <= y do
  begin
    Plot8Points(x, y);
    
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

---

## Performance Notes

**Line Drawing:**
- Bresenham: O(n) where n = line length
- Integer-only: Very fast
- No floating-point needed

**Polygon Rendering:**
- Scan conversion: O(edges × scanlines)
- Active Edge Table (AET): More efficient for complex polygons
- Consider back-face culling for 3D

**Texture Mapping:**
- Basic: O(pixels) with texture lookup
- Tiled: Better cache performance
- Bilinear filtering: 4× texture lookups (slower but smoother)

**Tilemaps:**
- Rendering: O(visible tiles)
- Scrolling: Use dirty rectangles for efficiency
- Consider tile caching for animated tiles

---

**Previous:** [Sorting Algorithms](./03_SortingAlgorithms.md)  
**Next:** [Collision Detection](./05_CollisionDetection.md)  
**Last Updated:** 2025-01-XX
