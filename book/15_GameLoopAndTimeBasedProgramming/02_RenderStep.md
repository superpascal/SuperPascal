# Render Step

**Part of:** [Chapter 11: Game Loop and Time-Based Programming](./README.md)

---

## Introduction

After updating game state, you need to draw everything to the screen. This section teaches you the **Render Step** — where you draw all graphics in the correct order.

**Key concepts:**
- **Render step** — Draw everything to screen
- **Drawing order** — What to draw first, what to draw last
- **Layers** — Background, sprites, foreground, HUD
- **Rendering pipeline** — The process of drawing a frame
- **Performance** — Efficient rendering techniques

---

## The Render Step

### Basic Structure

**The render step draws everything:**

```pascal
procedure RenderGame;
begin
  // 1. Clear screen
  ClearScreen(0);  // Black background
  
  // 2. Draw background
  DrawBackground;
  
  // 3. Draw game objects
  DrawPlayer;
  DrawEnemies;
  DrawBullets;
  
  // 4. Draw foreground
  DrawForeground;
  
  // 5. Draw HUD
  DrawHUD;
end;
```

### Why Separate from Update?

**Separating render from update:**

1. **Clear organization** — Display separate from logic
2. **Performance** — Can skip rendering if needed
3. **Debugging** — Can disable rendering to test logic
4. **Flexibility** — Can render multiple times per update

**Example:**
```pascal
// ✅ GOOD: Clear separation
procedure UpdateGame;
begin
  playerX := playerX + 1;
end;

procedure RenderGame;
begin
  DrawPlayer(playerX, playerY);
end;

// ❌ BAD: Mixed together
procedure UpdateAndRender;
begin
  playerX := playerX + 1;
  DrawPlayer(playerX, playerY);  // Logic and rendering mixed
end;
```

---

## Drawing Order

### Z-Order (Depth Order)

**Draw objects in the correct order:**

```
Layer 0 (Back):  Background
Layer 1:         Tiles (background layer)
Layer 2:         Sprites (behind foreground)
Layer 3:         Tiles (foreground layer)
Layer 4:         Sprites (in front)
Layer 5 (Front): HUD, UI, Text
```

**Example:**
```pascal
procedure RenderGame;
begin
  // Back to front order
  DrawBackground;        // Layer 0
  DrawBackgroundTiles;   // Layer 1
  DrawSpritesBehind;      // Layer 2
  DrawForegroundTiles;    // Layer 3
  DrawSpritesFront;       // Layer 4
  DrawHUD;                // Layer 5 (front)
end;
```

### Why Order Matters

**Objects drawn later appear in front:**

```pascal
// ✅ GOOD: Correct order
DrawBackground;   // Draw first (behind everything)
DrawPlayer;       // Draw second (in front of background)
DrawHUD;          // Draw last (in front of everything)

// ❌ BAD: Wrong order
DrawHUD;          // Draw first (will be behind player!)
DrawPlayer;
DrawBackground;   // Draw last (covers everything!)
```

---

## Rendering Pipeline

### Complete Render Pipeline

**Standard rendering pipeline:**

```pascal
procedure RenderGame;
begin
  // 1. Wait for VBlank (safe to update video memory)
  WaitVBlank;
  
  // 2. Clear screen (optional, depends on rendering method)
  // ClearScreen(0);  // May not be needed if drawing everything
  
  // 3. Draw background layers
  RenderBackground;
  RenderTilemapLayer0;
  
  // 4. Draw game objects (sorted by Z-order)
  RenderSprites;
  
  // 5. Draw foreground layers
  RenderTilemapLayer1;
  RenderForeground;
  
  // 6. Draw UI/HUD (always on top)
  RenderHUD;
  RenderMenu;
end;
```

### Background Rendering

**Draw background first:**

```pascal
procedure RenderBackground;
begin
  // Option 1: Solid color
  DrawRect(0, 0, 320, 240, BACKGROUND_COLOR);
  
  // Option 2: Tilemap
  RenderTilemap(backgroundTilemap);
  
  // Option 3: Scrolling background
  RenderScrollingBackground(scrollX, scrollY);
end;
```

### Sprite Rendering

**Draw sprites in Z-order:**

```pascal
procedure RenderSprites;
var
  i: byte;
  sortedSprites: array[0..127] of TSprite;
  spriteCount: byte;
begin
  // Sort sprites by Y position (painter's algorithm)
  SortSpritesByY(sortedSprites, spriteCount);
  
  // Draw from back to front
  for i := 0 to spriteCount - 1 do
  begin
    if sortedSprites[i].Visible then
      DrawSprite(sortedSprites[i]);
  end;
end;
```

**Painter's algorithm:**
- Draw objects from back to front
- Objects with larger Y values are drawn first (appear behind)
- Objects with smaller Y values are drawn last (appear in front)

### HUD Rendering

**Draw HUD last (always on top):**

```pascal
procedure RenderHUD;
begin
  // Score
  DrawText(10, 10, 'Score: ' + IntToStr(score));
  
  // Lives
  DrawText(10, 20, 'Lives: ' + IntToStr(lives));
  
  // Health bar
  DrawHealthBar(10, 30, playerHealth, MAX_HEALTH);
  
  // Minimap (if needed)
  DrawMinimap(280, 10);
end;
```

---

## Efficient Rendering

### Dirty Rectangle Rendering

**Only redraw what changed:**

```pascal
type
  TDirtyRect = record
    X, Y, W, H: integer;
    Dirty: boolean;
  end;

var
  dirtyRects: array[0..9] of TDirtyRect;
  dirtyCount: byte;

procedure MarkDirty(x, y, w, h: integer);
begin
  if dirtyCount < 10 then
  begin
    dirtyRects[dirtyCount].X := x;
    dirtyRects[dirtyCount].Y := y;
    dirtyRects[dirtyCount].W := w;
    dirtyRects[dirtyCount].H := h;
    dirtyRects[dirtyCount].Dirty := true;
    dirtyCount := dirtyCount + 1;
  end;
end;

procedure RenderDirtyRects;
var
  i: byte;
begin
  for i := 0 to dirtyCount - 1 do
  begin
    if dirtyRects[i].Dirty then
    begin
      // Redraw only this area
      RedrawArea(dirtyRects[i].X, dirtyRects[i].Y, 
                 dirtyRects[i].W, dirtyRects[i].H);
      dirtyRects[i].Dirty := false;
    end;
  end;
  dirtyCount := 0;
end;
```

### Sprite Batching

**Update all sprites at once:**

```pascal
procedure RenderAllSprites;
var
  i: byte;
begin
  // Update all sprite positions at once
  for i := 0 to spriteCount - 1 do
  begin
    if sprites[i].Visible then
      ZVB_SpriteSetFull(
        i,
        sprites[i].X,
        sprites[i].Y,
        sprites[i].Tile,
        sprites[i].Flags
      );
  end;
end;
```

### Tilemap Rendering

**Use hardware tilemap when possible:**

```pascal
procedure RenderTilemap;
begin
  // Update tilemap scroll
  ZVB_SetLayer0Scroll(scrollX, scrollY);
  
  // Hardware renders tilemap automatically
  // No CPU rendering needed!
end;
```

---

## Conditional Rendering

### State-Based Rendering

**Render different things based on game state:**

```pascal
procedure RenderGame;
begin
  case gameState of
    gsMenu:
      RenderMenu;
    gsPlaying:
    begin
      RenderBackground;
      RenderGameObjects;
      RenderHUD;
    end;
    gsPaused:
    begin
      RenderGame;  // Render game in background
      RenderPauseOverlay;
    end;
    gsGameOver:
      RenderGameOverScreen;
  end;
end;
```

### Culling (Don't Draw Off-Screen)

**Skip drawing objects that are off-screen:**

```pascal
function IsOnScreen(x, y, w, h: integer): boolean;
begin
  IsOnScreen := 
    (x + w >= 0) and (x < 320) and
    (y + h >= 0) and (y < 240);
end;

procedure RenderSprites;
var
  i: byte;
begin
  for i := 0 to spriteCount - 1 do
  begin
    if sprites[i].Visible and 
       IsOnScreen(sprites[i].X, sprites[i].Y, 16, 16) then
      DrawSprite(sprites[i]);
  end;
end;
```

---

## Rendering Techniques

### Double Buffering

**Render to off-screen buffer, then copy:**

```pascal
var
  frameBuffer: array[0..76799] of byte;  // 320x240 off-screen buffer

procedure RenderToBuffer;
begin
  // Draw everything to frameBuffer
  DrawBackgroundToBuffer;
  DrawSpritesToBuffer;
  DrawHUDToBuffer;
end;

procedure CopyBufferToScreen;
begin
  // Copy frameBuffer to video memory during VBlank
  WaitVBlank;
  DMA_Copy(frameBuffer, vram^, SizeOf(frameBuffer));
end;
```

**Note:** On retro systems, double buffering may not be necessary if you render during VBlank.

### Partial Updates

**Only update what changed:**

```pascal
procedure RenderGame;
begin
  if playerMoved then
  begin
    // Erase old position
    DrawSpriteAt(oldPlayerX, oldPlayerY, ERASE_SPRITE);
    // Draw new position
    DrawSpriteAt(playerX, playerY, PLAYER_SPRITE);
    playerMoved := false;
  end;
  
  if enemyMoved then
  begin
    UpdateEnemySprites;
    enemyMoved := false;
  end;
end;
```

---

## Complete Render Example

**Putting it all together:**

```pascal
procedure RenderGame;
begin
  // Wait for VBlank (safe to update)
  WaitVBlank;
  
  case gameState of
    gsMenu:
      RenderMenu;
      
    gsPlaying:
    begin
      // 1. Background
      RenderBackground;
      RenderTilemapLayer0;
      
      // 2. Game objects (sorted by Y)
      SortSpritesByY;
      RenderSprites;
      
      // 3. Foreground
      RenderTilemapLayer1;
      
      // 4. HUD (always on top)
      RenderHUD;
    end;
    
    gsPaused:
    begin
      // Render game in background
      RenderGameObjects;
      // Pause overlay on top
      RenderPauseOverlay;
    end;
    
    gsGameOver:
      RenderGameOverScreen;
  end;
end;

procedure RenderGameObjects;
begin
  RenderBackground;
  RenderPlayer;
  RenderEnemies;
  RenderBullets;
end;

procedure RenderHUD;
begin
  DrawText(10, 10, 'Score: ' + IntToStr(score));
  DrawText(10, 20, 'Lives: ' + IntToStr(lives));
  DrawText(10, 30, 'Level: ' + IntToStr(level));
end;
```

---

## Best Practices

### 1. Draw in Correct Order

**Always draw back to front:**

```pascal
// ✅ GOOD: Correct order
DrawBackground;
DrawSprites;
DrawHUD;

// ❌ BAD: Wrong order
DrawHUD;  // Will be behind sprites!
DrawSprites;
DrawBackground;
```

### 2. Wait for VBlank

**Always wait for VBlank before rendering:**

```pascal
// ✅ GOOD
WaitVBlank;
RenderGame;

// ❌ BAD
RenderGame;  // May cause flicker
WaitVBlank;
```

### 3. Batch Sprite Updates

**Update all sprites at once:**

```pascal
// ✅ GOOD: Batch updates
WaitVBlank;
UpdateAllSprites;

// ❌ BAD: Individual updates
UpdateSprite(0);
WaitVBlank;
UpdateSprite(1);
```

### 4. Cull Off-Screen Objects

**Don't draw what's not visible:**

```pascal
// ✅ GOOD: Check bounds
if IsOnScreen(sprite.X, sprite.Y, 16, 16) then
  DrawSprite(sprite);

// ❌ BAD: Draw everything
DrawSprite(sprite);  // Even if off-screen
```

### 5. Use Hardware Features

**Use hardware tilemaps, sprites, etc.:**

```pascal
// ✅ GOOD: Hardware tilemap
ZVB_SetLayer0Scroll(x, y);  // Hardware renders

// ❌ BAD: Software tilemap
for y := 0 to 29 do
  for x := 0 to 39 do
    DrawTile(x, y, tilemap[y, x]);  // Slow CPU rendering
```

---

## Exercises

### Exercise 1: Basic Render Loop

Write a program that:
1. Has a render step in the game loop
2. Draws background, player, and HUD
3. Draws in correct order
4. Renders every frame

### Exercise 2: Layered Rendering

Write a program that:
1. Has multiple layers (background, sprites, foreground, HUD)
2. Draws layers in correct order
3. Demonstrates Z-ordering (objects appear in front/behind)

### Exercise 3: Sprite Rendering

Write a program that:
1. Has multiple sprites
2. Sorts sprites by Y position
3. Renders sprites in correct order
4. Uses sprite batching

### Exercise 4: State-Based Rendering

Write a program that:
1. Has different game states (menu, playing, paused)
2. Renders different screens for each state
3. Handles pause overlay (game visible behind pause screen)
4. Renders HUD only during gameplay

---

**Previous Section:** [Update Step](./01_UpdateStep.md)  
**Next Section:** [Synchronizing with VBlank](./03_SynchronizingWithVBlank.md)  
**Language Specification:** See [Graphics Intrinsics](../../languageSpecification/intrinsicsAndDirectives/02_GraphicsIntrinsics.md)  
**Last Updated:** 2025-01-XX

