# Scrolling and Camera Movement

**Part of:** [Chapter 17: Tilemaps and Level Design](./README.md)

---

## Introduction

Scrolling allows you to display large tilemaps by moving the viewport. This section teaches you how to implement scrolling tilemaps, camera movement, and viewport management.

**Key concepts:**
- **Scrolling** — Moving the viewport over a larger tilemap
- **Camera** — The viewport position
- **Viewport** — The visible area of the screen
- **Following** — Camera follows player
- **Bounds** — Limiting camera movement

---

## Understanding Scrolling

### What is Scrolling?

**Scrolling moves the viewport over a larger tilemap:**

```
Tilemap (40x30 tiles):
[==========][==========][==========][==========]
[==========][==========][==========][==========]
[==========][==========][==========][==========]
            ^
            Viewport (20x15 tiles)
            Shows part of larger tilemap
```

**Benefits:**
- **Large levels** — Display levels larger than screen
- **Smooth movement** — Camera follows player
- **Efficient** — Only render visible tiles

### Viewport vs Tilemap

**Viewport is the visible area:**

```pascal
const
  SCREEN_WIDTH = 320;   // Screen width in pixels
  SCREEN_HEIGHT = 240;  // Screen height in pixels
  TILE_SIZE = 8;        // Tile size in pixels
  
  VIEWPORT_WIDTH_TILES = SCREEN_WIDTH div TILE_SIZE;   // 40 tiles
  VIEWPORT_HEIGHT_TILES = SCREEN_HEIGHT div TILE_SIZE;  // 30 tiles
```

**Tilemap is the full level:**

```pascal
const
  TILEMAP_WIDTH = 100;   // Full tilemap width
  TILEMAP_HEIGHT = 80;   // Full tilemap height
```

---

## Basic Scrolling

### Setting Scroll Position

**Set tilemap scroll position:**

```pascal
procedure SetTilemapScroll(scrollX, scrollY: word);
begin
  ZVB_SetLayer0Scroll(scrollX, scrollY);
end;
```

**Scroll values:**
- `scrollX` — Horizontal scroll (pixels)
- `scrollY` — Vertical scroll (pixels)
- Hardware automatically handles wrapping/clamping

### Simple Scrolling

**Scroll tilemap continuously:**

```pascal
var
  scrollX: word;
  
begin
  scrollX := 0;
  
  while true do
  begin
    // Scroll right
    scrollX := scrollX + 1;
    if scrollX >= TILEMAP_WIDTH * TILE_SIZE then
      scrollX := 0;  // Wrap around
    
    ZVB_SetLayer0Scroll(scrollX, 0);
    WaitVBlank;
  end;
end;
```

---

## Camera System

### Camera Position

**Track camera position:**

```pascal
type
  TCamera = record
    X, Y: integer;  // Camera position in world coordinates
  end;

var
  camera: TCamera;
  
procedure InitCamera;
begin
  camera.X := 0;
  camera.Y := 0;
end;
```

### World to Screen Conversion

**Convert world coordinates to screen coordinates:**

```pascal
function WorldToScreen(worldX, worldY: integer; const cam: TCamera): TPoint;
begin
  WorldToScreen.X := worldX - cam.X;
  WorldToScreen.Y := worldY - cam.Y;
end;

// Usage
var
  playerWorldX, playerWorldY: integer;
  playerScreenX, playerScreenY: integer;
  
begin
  EntityGetPosition(player, playerWorldX, playerWorldY);
  var screenPos := WorldToScreen(playerWorldX, playerWorldY, camera);
  playerScreenX := screenPos.X;
  playerScreenY := screenPos.Y;
  
  // Draw at screen position
  DrawSprite(playerScreenX, playerScreenY);
end;
```

### Screen to World Conversion

**Convert screen coordinates to world coordinates:**

```pascal
function ScreenToWorld(screenX, screenY: integer; const cam: TCamera): TPoint;
begin
  ScreenToWorld.X := screenX + cam.X;
  ScreenToWorld.Y := screenY + cam.Y;
end;
```

---

## Camera Following

### Follow Player

**Camera follows player position:**

```pascal
procedure UpdateCameraFollowPlayer(const player: TEntityID; var cam: TCamera);
var
  playerX, playerY: integer;
  targetX, targetY: integer;
begin
  EntityGetPosition(player, playerX, playerY);
  
  // Center camera on player
  targetX := playerX - SCREEN_WIDTH div 2;
  targetY := playerY - SCREEN_HEIGHT div 2;
  
  // Update camera
  cam.X := targetX;
  cam.Y := targetY;
  
  // Apply camera bounds
  ClampCamera(cam);
end;
```

### Smooth Following

**Smooth camera movement (interpolation):**

```pascal
const
  CAMERA_SPEED = 0.1;  // Interpolation speed (0.0 to 1.0)

procedure UpdateCameraSmooth(const player: TEntityID; var cam: TCamera);
var
  playerX, playerY: integer;
  targetX, targetY: integer;
begin
  EntityGetPosition(player, playerX, playerY);
  
  // Calculate target position
  targetX := playerX - SCREEN_WIDTH div 2;
  targetY := playerY - SCREEN_HEIGHT div 2;
  
  // Interpolate camera position
  cam.X := cam.X + Trunc((targetX - cam.X) * CAMERA_SPEED);
  cam.Y := cam.Y + Trunc((targetY - cam.Y) * CAMERA_SPEED);
  
  // Apply camera bounds
  ClampCamera(cam);
end;
```

### Look-Ahead

**Camera looks ahead of player:**

```pascal
const
  LOOK_AHEAD_X = 50;  // Look ahead distance

procedure UpdateCameraLookAhead(const player: TEntityID; var cam: TCamera);
var
  playerX, playerY: integer;
  playerVX, playerVY: integer;
  targetX, targetY: integer;
begin
  EntityGetPosition(player, playerX, playerY);
  EntityGetVelocity(player, playerVX, playerVY);
  
  // Look ahead in direction of movement
  if playerVX > 0 then
    targetX := playerX - SCREEN_WIDTH div 2 + LOOK_AHEAD_X
  else if playerVX < 0 then
    targetX := playerX - SCREEN_WIDTH div 2 - LOOK_AHEAD_X
  else
    targetX := playerX - SCREEN_WIDTH div 2;
  
  targetY := playerY - SCREEN_HEIGHT div 2;
  
  cam.X := targetX;
  cam.Y := targetY;
  
  ClampCamera(cam);
end;
```

---

## Camera Bounds

### Clamping Camera

**Limit camera to tilemap bounds:**

```pascal
procedure ClampCamera(var cam: TCamera);
begin
  // Clamp X
  if cam.X < 0 then
    cam.X := 0
  else if cam.X > (TILEMAP_WIDTH * TILE_SIZE - SCREEN_WIDTH) then
    cam.X := TILEMAP_WIDTH * TILE_SIZE - SCREEN_WIDTH;
  
  // Clamp Y
  if cam.Y < 0 then
    cam.Y := 0
  else if cam.Y > (TILEMAP_HEIGHT * TILE_SIZE - SCREEN_HEIGHT) then
    cam.Y := TILEMAP_HEIGHT * TILE_SIZE - SCREEN_HEIGHT;
end;
```

### Dead Zone

**Camera only moves when player is near edge:**

```pascal
const
  DEAD_ZONE_LEFT = 80;
  DEAD_ZONE_RIGHT = 240;
  DEAD_ZONE_TOP = 60;
  DEAD_ZONE_BOTTOM = 180;

procedure UpdateCameraDeadZone(const player: TEntityID; var cam: TCamera);
var
  playerX, playerY: integer;
  playerScreenX, playerScreenY: integer;
begin
  EntityGetPosition(player, playerX, playerY);
  
  // Convert to screen coordinates
  playerScreenX := playerX - cam.X;
  playerScreenY := playerY - cam.Y;
  
  // Move camera if player is outside dead zone
  if playerScreenX < DEAD_ZONE_LEFT then
    cam.X := playerX - DEAD_ZONE_LEFT
  else if playerScreenX > DEAD_ZONE_RIGHT then
    cam.X := playerX - DEAD_ZONE_RIGHT;
  
  if playerScreenY < DEAD_ZONE_TOP then
    cam.Y := playerY - DEAD_ZONE_TOP
  else if playerScreenY > DEAD_ZONE_BOTTOM then
    cam.Y := playerY - DEAD_ZONE_BOTTOM;
  
  ClampCamera(cam);
end;
```

---

## Rendering with Camera

### Rendering Entities

**Render entities using camera:**

```pascal
procedure RenderEntitiesWithCamera(const cam: TCamera);
var
  i: word;
  worldX, worldY: integer;
  screenX, screenY: integer;
begin
  for i := 0 to entityCount - 1 do
  begin
    if EntityValid(i) and HasComponent(i, COMPONENT_POSITION) then
    begin
      EntityGetPosition(i, worldX, worldY);
      
      // Convert to screen coordinates
      var screenPos := WorldToScreen(worldX, worldY, cam);
      screenX := screenPos.X;
      screenY := screenPos.Y;
      
      // Only render if on screen
      if (screenX >= -ENTITY_WIDTH) and (screenX < SCREEN_WIDTH) and
         (screenY >= -ENTITY_HEIGHT) and (screenY < SCREEN_HEIGHT) then
      begin
        RenderEntity(i, screenX, screenY);
      end;
    end;
  end;
end;
```

### Rendering Tilemap

**Render only visible tiles:**

```pascal
procedure RenderTilemapWithCamera(const tilemap: TTilemap; const cam: TCamera);
var
  startTileX, endTileX: word;
  startTileY, endTileY: word;
  tileX, tileY: word;
  screenX, screenY: integer;
begin
  // Calculate visible tile range
  startTileX := cam.X div TILE_SIZE;
  endTileX := (cam.X + SCREEN_WIDTH) div TILE_SIZE + 1;
  startTileY := cam.Y div TILE_SIZE;
  endTileY := (cam.Y + SCREEN_HEIGHT) div TILE_SIZE + 1;
  
  // Clamp to tilemap bounds
  if endTileX > tilemap.Width then endTileX := tilemap.Width;
  if endTileY > tilemap.Height then endTileY := tilemap.Height;
  
  // Render visible tiles
  for tileY := startTileY to endTileY - 1 do
    for tileX := startTileX to endTileX - 1 do
    begin
      // Calculate screen position
      screenX := tileX * TILE_SIZE - cam.X;
      screenY := tileY * TILE_SIZE - cam.Y;
      
      // Get tile
      var tile := GetTile(tilemap, tileX, tileY);
      
      // Draw tile
      DrawTile(screenX, screenY, tile);
    end;
end;
```

---

## Parallax Scrolling

### Multiple Layers

**Scroll layers at different speeds:**

```pascal
type
  TParallaxLayer = record
    ScrollX, ScrollY: integer;
    SpeedX, SpeedY: Q8.8;  // Scroll speed multiplier
  end;

var
  backgroundLayer: TParallaxLayer;
  foregroundLayer: TParallaxLayer;
  camera: TCamera;

procedure InitParallax;
begin
  // Background scrolls slower
  backgroundLayer.SpeedX := Q8_8(0.5);
  backgroundLayer.SpeedY := Q8_8(0.5);
  
  // Foreground scrolls faster
  foregroundLayer.SpeedX := Q8_8(1.5);
  foregroundLayer.SpeedY := Q8_8(1.5);
end;

procedure UpdateParallax(const cam: TCamera);
begin
  // Update layer scroll positions
  backgroundLayer.ScrollX := Trunc(cam.X * backgroundLayer.SpeedX);
  backgroundLayer.ScrollY := Trunc(cam.Y * backgroundLayer.SpeedY);
  
  foregroundLayer.ScrollX := Trunc(cam.X * foregroundLayer.SpeedX);
  foregroundLayer.ScrollY := Trunc(cam.Y * foregroundLayer.SpeedY);
  
  // Set hardware scroll
  ZVB_SetLayer0Scroll(backgroundLayer.ScrollX, backgroundLayer.ScrollY);
  ZVB_SetLayer1Scroll(foregroundLayer.ScrollX, foregroundLayer.ScrollY);
end;
```

---

## Complete Scrolling Example

**Putting it all together:**

```pascal
program ScrollingDemo;

type
  TCamera = record
    X, Y: integer;
  end;

var
  camera: TCamera;
  player: TEntityID;
  level1: TTilemap;

procedure InitCamera;
begin
  camera.X := 0;
  camera.Y := 0;
end;

procedure UpdateCamera;
var
  playerX, playerY: integer;
begin
  if EntityValid(player) then
  begin
    EntityGetPosition(player, playerX, playerY);
    
    // Center camera on player
    camera.X := playerX - SCREEN_WIDTH div 2;
    camera.Y := playerY - SCREEN_HEIGHT div 2;
    
    // Clamp camera
    if camera.X < 0 then camera.X := 0;
    if camera.Y < 0 then camera.Y := 0;
    if camera.X > (level1.Width * TILE_SIZE - SCREEN_WIDTH) then
      camera.X := level1.Width * TILE_SIZE - SCREEN_WIDTH;
    if camera.Y > (level1.Height * TILE_SIZE - SCREEN_HEIGHT) then
      camera.Y := level1.Height * TILE_SIZE - SCREEN_HEIGHT;
  end;
end;

procedure RenderGame;
begin
  // Set tilemap scroll
  ZVB_SetLayer0Scroll(camera.X, camera.Y);
  
  // Render entities (convert to screen coordinates)
  RenderEntitiesWithCamera(camera);
end;

begin
  InitGraphics;
  InitCamera;
  
  player := EntityCreate;
  EntitySetPosition(player, 160, 120);
  
  while true do
  begin
    UpdateGame;
    UpdateCamera;
    WaitVBlank;
    RenderGame;
  end;
end.
```

---

## Best Practices

### 1. Update Camera Before Rendering

**Update camera position before rendering:**

```pascal
// ✅ GOOD: Update then render
UpdateCamera;
WaitVBlank;
RenderGame;

// ❌ BAD: Render then update
RenderGame;
UpdateCamera;  // Camera updated after rendering
```

### 2. Clamp Camera to Bounds

**Always clamp camera to tilemap bounds:**

```pascal
// ✅ GOOD: Clamp camera
UpdateCamera;
ClampCamera(camera);

// ❌ BAD: No clamping
UpdateCamera;
// Camera may go out of bounds
```

### 3. Cull Off-Screen Entities

**Only render entities on screen:**

```pascal
// ✅ GOOD: Cull off-screen
if IsOnScreen(worldX, worldY, camera) then
  RenderEntity(entity);

// ❌ BAD: Render everything
RenderEntity(entity);  // Renders off-screen entities
```

### 4. Use Hardware Scrolling

**Use hardware scrolling when possible:**

```pascal
// ✅ GOOD: Hardware scroll
ZVB_SetLayer0Scroll(camera.X, camera.Y);

// ❌ BAD: Software scroll
for y := 0 to SCREEN_HEIGHT do
  for x := 0 to SCREEN_WIDTH do
    DrawTile(x, y, GetTile(camera.X + x, camera.Y + y));  // Slow
```

### 5. Smooth Camera Movement

**Use interpolation for smooth camera:**

```pascal
// ✅ GOOD: Smooth interpolation
cam.X := cam.X + Trunc((targetX - cam.X) * 0.1);

// ❌ BAD: Instant movement
cam.X := targetX;  // Jerky movement
```

---

## Exercises

### Exercise 1: Basic Scrolling

Write a program that:
1. Creates a large tilemap
2. Scrolls the tilemap continuously
3. Displays scrolling tilemap on screen
4. Wraps around when reaching edge

### Exercise 2: Camera Following

Write a program that:
1. Creates a player entity
2. Implements camera that follows player
3. Centers camera on player
4. Clamps camera to tilemap bounds

### Exercise 3: Smooth Camera

Write a program that:
1. Implements smooth camera interpolation
2. Camera follows player with delay
3. Camera looks ahead of player
4. Smooth camera movement

### Exercise 4: Parallax Scrolling

Write a program that:
1. Creates multiple tilemap layers
2. Scrolls layers at different speeds
3. Creates depth effect
4. Renders all layers correctly

---

**Previous Section:** [Tile Collision Rules](./02_TileCollisionRules.md)  
**Next Chapter:** [Chapter 18: Physics and Movement](../18_PhysicsAndMovement/README.md)  
**Language Specification:** See [ZVB Intrinsics](../../languageSpecification/intrinsicsAndDirectives/02A_ZVBIntrinsics.md)  
**Last Updated:** 2025-01-XX

