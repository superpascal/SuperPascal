# Animation Basics

**Part of:** [Chapter 09: Graphics](./README.md)

---

## Introduction

Animation brings graphics to life! This section teaches you how to:
- **Create animation loops** — Frame-based animation
- **Synchronize with VBlank** — Smooth, flicker-free animation
- **Animate sprites** — Sprite-based animation
- **Create smooth motion** — Interpolation and easing
- **Manage frame timing** — Consistent animation speed

**Key concepts:**
- **Frame-based animation** — Update each frame
- **VBlank synchronization** — Update during safe period
- **Animation state** — Track current frame, position, etc.
- **Timing** — Frame counters and delta time

---

## Frame-Based Animation

### The Animation Loop

**Basic animation loop structure:**

```pascal
program AnimationDemo;

procedure InitGraphics;
begin
  ZVB_SetVideoMode(ZVB_MODE_GFX_320_8BIT);
  ZVB_EnableScreen(true);
  WaitVBlank;
end;

procedure UpdateAnimation;
begin
  // Update animation state here
end;

procedure RenderFrame;
begin
  // Draw current frame here
end;

begin
  InitGraphics;
  
  // Main animation loop
  while true do
  begin
    UpdateAnimation;   // Update state
    RenderFrame;       // Draw frame
    WaitVBlank;        // Wait for next frame (60 FPS)
  end;
end.
```

### Frame Counter

**Track animation progress with a frame counter:**

```pascal
var
  frameCount: word;
begin
  frameCount := 0;
  
  while true do
  begin
    // Use frameCount for timing
    if (frameCount mod 60) = 0 then
      WriteLn('One second passed');
    
    frameCount := frameCount + 1;
    WaitVBlank;
  end;
end;
```

### Simple Animation Example

**Animate a moving rectangle:**

```pascal
var
  rectX: integer;
  rectSpeed: integer;
begin
  rectX := 0;
  rectSpeed := 2;
  
  while true do
  begin
    // Clear screen
    DrawRect(0, 0, 320, 240, 0);  // Black background
    
    // Draw rectangle at current position
    DrawRect(rectX, 100, 50, 50, 15);  // White rectangle
    
    // Update position
    rectX := rectX + rectSpeed;
    if (rectX > 320) or (rectX < 0) then
      rectSpeed := -rectSpeed;  // Bounce
    
    WaitVBlank;
  end;
end;
```

---

## Sprite Animation

### Frame Sequences

**Animate sprites by cycling through tiles:**

```pascal
type
  TAnimation = record
    StartTile: byte;    // First tile in sequence
    FrameCount: byte;   // Number of frames
    CurrentFrame: byte; // Current frame (0 to FrameCount-1)
    FrameDelay: byte;   // Frames to wait before next frame
    FrameCounter: byte; // Counter for frame delay
  end;

var
  walkAnimation: TAnimation;
begin
  // Set up walk animation: tiles 10-13, 4 frames, change every 8 frames
  walkAnimation.StartTile := 10;
  walkAnimation.FrameCount := 4;
  walkAnimation.CurrentFrame := 0;
  walkAnimation.FrameDelay := 8;
  walkAnimation.FrameCounter := 0;
end;
```

### Updating Animation

**Update animation state:**

```pascal
procedure UpdateAnimation(var anim: TAnimation);
begin
  anim.FrameCounter := anim.FrameCounter + 1;
  
  if anim.FrameCounter >= anim.FrameDelay then
  begin
    anim.FrameCounter := 0;
    anim.CurrentFrame := (anim.CurrentFrame + 1) mod anim.FrameCount;
  end;
end;

procedure RenderAnimation(const anim: TAnimation; spriteIndex: byte; x, y: word);
var
  currentTile: byte;
begin
  currentTile := anim.StartTile + anim.CurrentFrame;
  ZVB_SpriteSetFull(spriteIndex, x, y, currentTile, 0);
end;
```

**Usage:**
```pascal
while true do
begin
  UpdateAnimation(walkAnimation);
  RenderAnimation(walkAnimation, 0, 100, 50);
  WaitVBlank;
end;
```

### Multiple Animations

**Manage multiple sprite animations:**

```pascal
type
  TSpriteAnim = record
    SpriteIndex: byte;
    X, Y: word;
    Animation: TAnimation;
  end;

var
  sprites: array[0..9] of TSpriteAnim;

procedure UpdateAllAnimations;
var
  i: byte;
begin
  for i := 0 to 9 do
  begin
    UpdateAnimation(sprites[i].Animation);
    RenderAnimation(
      sprites[i].Animation,
      sprites[i].SpriteIndex,
      sprites[i].X,
      sprites[i].Y
    );
  end;
end;
```

---

## Smooth Motion

### Linear Interpolation

**Smooth movement between positions:**

```pascal
function Lerp(start, target: integer; t: Q8.8): integer;
begin
  // Linear interpolation: start + (target - start) * t
  // t is 0.0 to 1.0 in fixed-point
  Lerp := start + Trunc((target - start) * t);
end;
```

**Usage:**
```pascal
var
  startX, targetX: integer;
  t: Q8.8;
begin
  startX := 0;
  targetX := 300;
  t := 0;  // Start at 0.0
  
  while t < Q8_8(1.0) do
  begin
    var currentX := Lerp(startX, targetX, t);
    DrawRect(currentX, 100, 50, 50, 15);
    
    t := t + Q8_8(0.01);  // Increment by 0.01
    WaitVBlank;
  end;
end;
```

### Easing Functions

**Ease-in (slow start):**

```pascal
function EaseIn(t: Q8.8): Q8.8;
begin
  // t^2: slow start, fast end
  EaseIn := t * t;
end;
```

**Ease-out (fast start, slow end):**

```pascal
function EaseOut(t: Q8.8): Q8.8;
begin
  // 1 - (1-t)^2: fast start, slow end
  var oneMinusT: Q8.8;
  oneMinusT := Q8_8(1.0) - t;
  EaseOut := Q8_8(1.0) - (oneMinusT * oneMinusT);
end;
```

**Ease-in-out (slow start and end):**

```pascal
function EaseInOut(t: Q8.8): Q8.8;
begin
  // Smooth S-curve
  if t < Q8_8(0.5) then
    EaseInOut := 2 * t * t
  else
  begin
    var oneMinusT: Q8.8;
    oneMinusT := Q8_8(1.0) - t;
    EaseInOut := Q8_8(1.0) - 2 * oneMinusT * oneMinusT;
  end;
end;
```

**Usage:**
```pascal
var
  t: Q8.8;
  x: integer;
begin
  t := 0;
  while t < Q8_8(1.0) do
  begin
    var easedT := EaseInOut(t);
    x := Lerp(0, 300, easedT);
    DrawRect(x, 100, 50, 50, 15);
    
    t := t + Q8_8(0.01);
    WaitVBlank;
  end;
end;
```

---

## Frame Timing

### Fixed Frame Rate

**60 FPS (frames per second) with VBlank:**

```pascal
// VBlank occurs 60 times per second
// Each WaitVBlank = 1/60 second = ~16.67ms

while true do
begin
  UpdateGame;    // Update game state
  RenderFrame;   // Draw frame
  WaitVBlank;    // Wait for next frame (60 FPS)
end;
```

### Frame Rate Independence

**Use delta time for frame-rate independent motion:**

```pascal
const
  TARGET_FPS = 60;
  FRAME_TIME_MS = 1000 div TARGET_FPS;  // ~16.67ms

var
  lastFrameTime: word;
  deltaTime: Q8.8;
begin
  lastFrameTime := GetTime;  // Get current time (milliseconds)
  
  while true do
  begin
    var currentTime := GetTime;
    deltaTime := Q8_8(currentTime - lastFrameTime) / Q8_8(1000.0);  // Convert to seconds
    lastFrameTime := currentTime;
    
    // Update with delta time
    UpdateGame(deltaTime);
    RenderFrame;
    WaitVBlank;
  end;
end;
```

### Frame Skipping

**Skip frames if running too slow:**

```pascal
var
  frameCount: word;
  skipFrames: boolean;
begin
  frameCount := 0;
  skipFrames := false;
  
  while true do
  begin
    if not skipFrames then
    begin
      UpdateGame;
      RenderFrame;
    end;
    
    frameCount := frameCount + 1;
    
    // Skip every other frame if needed
    skipFrames := (frameCount mod 2) = 0;
    
    WaitVBlank;
  end;
end;
```

---

## Animation Patterns

### State Machine Animation

**Different animations for different states:**

```pascal
type
  TPlayerState = (psIdle, psWalking, psJumping, psFalling);
  
  TPlayer = record
    State: TPlayerState;
    IdleAnim: TAnimation;
    WalkAnim: TAnimation;
    JumpAnim: TAnimation;
    X, Y: word;
  end;

procedure UpdatePlayer(var player: TPlayer);
begin
  case player.State of
    psIdle:
      UpdateAnimation(player.IdleAnim);
    psWalking:
      UpdateAnimation(player.WalkAnim);
    psJumping:
      UpdateAnimation(player.JumpAnim);
    psFalling:
      // No animation, just falling
  end;
end;

procedure RenderPlayer(const player: TPlayer; spriteIndex: byte);
var
  currentTile: byte;
begin
  case player.State of
    psIdle:
      currentTile := player.IdleAnim.StartTile + player.IdleAnim.CurrentFrame;
    psWalking:
      currentTile := player.WalkAnim.StartTile + player.WalkAnim.CurrentFrame;
    psJumping:
      currentTile := player.JumpAnim.StartTile + player.JumpAnim.CurrentFrame;
    psFalling:
      currentTile := FALLING_TILE;
  end;
  
  ZVB_SpriteSetFull(spriteIndex, player.X, player.Y, currentTile, 0);
end;
```

### Ping-Pong Animation

**Animation that plays forward then backward:**

```pascal
type
  TPingPongAnim = record
    StartTile: byte;
    FrameCount: byte;
    CurrentFrame: byte;
    Direction: integer;  // 1 = forward, -1 = backward
    FrameDelay: byte;
    FrameCounter: byte;
  end;

procedure UpdatePingPongAnim(var anim: TPingPongAnim);
begin
  anim.FrameCounter := anim.FrameCounter + 1;
  
  if anim.FrameCounter >= anim.FrameDelay then
  begin
    anim.FrameCounter := 0;
    anim.CurrentFrame := anim.CurrentFrame + anim.Direction;
    
    // Reverse direction at ends
    if anim.CurrentFrame >= anim.FrameCount then
    begin
      anim.CurrentFrame := anim.FrameCount - 2;
      anim.Direction := -1;
    end
    else if anim.CurrentFrame < 0 then
    begin
      anim.CurrentFrame := 1;
      anim.Direction := 1;
    end;
  end;
end;
```

### Looping Animation

**Animation that loops continuously:**

```pascal
procedure UpdateLoopingAnim(var anim: TAnimation);
begin
  anim.FrameCounter := anim.FrameCounter + 1;
  
  if anim.FrameCounter >= anim.FrameDelay then
  begin
    anim.FrameCounter := 0;
    anim.CurrentFrame := (anim.CurrentFrame + 1) mod anim.FrameCount;
  end;
end;
```

---

## Complete Animation Example

**Putting it all together:**

```pascal
program AnimationDemo;

type
  TAnimation = record
    StartTile: byte;
    FrameCount: byte;
    CurrentFrame: byte;
    FrameDelay: byte;
    FrameCounter: byte;
  end;
  
  TSprite = record
    X, Y: word;
    Anim: TAnimation;
  end;

var
  sprite: TSprite;

procedure InitGraphics;
begin
  ZVB_SetVideoMode(ZVB_MODE_GFX_320_8BIT);
  ZVB_EnableScreen(true);
  WaitVBlank;
  
  // Load animation tiles
  LoadTile(10, TILE_WALK_0);
  LoadTile(11, TILE_WALK_1);
  LoadTile(12, TILE_WALK_2);
  LoadTile(13, TILE_WALK_3);
end;

procedure InitSprite;
begin
  sprite.X := 50;
  sprite.Y := 100;
  sprite.Anim.StartTile := 10;
  sprite.Anim.FrameCount := 4;
  sprite.Anim.CurrentFrame := 0;
  sprite.Anim.FrameDelay := 8;
  sprite.Anim.FrameCounter := 0;
end;

procedure UpdateAnimation(var anim: TAnimation);
begin
  anim.FrameCounter := anim.FrameCounter + 1;
  if anim.FrameCounter >= anim.FrameDelay then
  begin
    anim.FrameCounter := 0;
    anim.CurrentFrame := (anim.CurrentFrame + 1) mod anim.FrameCount;
  end;
end;

procedure UpdateSprite(var s: TSprite);
begin
  // Update animation
  UpdateAnimation(s.Anim);
  
  // Move sprite
  s.X := s.X + 1;
  if s.X > 320 then
    s.X := 0;
end;

procedure RenderSprite(const s: TSprite);
var
  currentTile: byte;
begin
  currentTile := s.Anim.StartTile + s.Anim.CurrentFrame;
  ZVB_SpriteSetFull(0, s.X, s.Y, currentTile, 0);
end;

begin
  InitGraphics;
  InitSprite;
  
  while true do
  begin
    UpdateSprite(sprite);
    RenderSprite(sprite);
    WaitVBlank;
  end;
end.
```

---

## Best Practices

### 1. Update During VBlank

**Always update animation during VBlank:**

```pascal
// ✅ GOOD
WaitVBlank;
UpdateAnimation;
RenderFrame;

// ❌ BAD
UpdateAnimation;  // May cause flicker
RenderFrame;
WaitVBlank;
```

### 2. Use Frame Delays

**Control animation speed with frame delays:**

```pascal
// ✅ GOOD: Smooth animation
anim.FrameDelay := 8;  // Change frame every 8 VBlanks

// ❌ BAD: Too fast
anim.FrameDelay := 1;  // Changes every frame (too fast)
```

### 3. Batch Updates

**Update all animations at once:**

```pascal
// ✅ GOOD
WaitVBlank;
UpdateAllAnimations;
RenderAllSprites;

// ❌ BAD
for i := 0 to 9 do
begin
  UpdateAnimation(sprites[i].Anim);
  WaitVBlank;  // Unnecessary waits
end;
```

### 4. Use State Machines

**Organize complex animations with state machines:**

```pascal
// ✅ GOOD: Clear state management
case player.State of
  psIdle: UpdateIdleAnimation;
  psWalking: UpdateWalkAnimation;
  psJumping: UpdateJumpAnimation;
end;

// ❌ BAD: Unclear state
if (someCondition) and (otherCondition) then
  UpdateSomeAnimation;
```

### 5. Pre-calculate Values

**Calculate expensive operations once:**

```pascal
// ✅ GOOD: Pre-calculate
const
  ANIM_TILES: array[0..3] of byte = (10, 11, 12, 13);
  
procedure RenderSprite;
begin
  currentTile := ANIM_TILES[anim.CurrentFrame];
end;

// ❌ BAD: Calculate every frame
currentTile := anim.StartTile + anim.CurrentFrame;
```

---

## Exercises

### Exercise 1: Simple Animation

Write a program that:
1. Draws a rectangle
2. Moves it across the screen
3. Bounces it off the edges
4. Runs at 60 FPS

### Exercise 2: Sprite Animation

Write a program that:
1. Loads a 4-frame walk animation
2. Displays a sprite
3. Animates it by cycling through frames
4. Moves the sprite while animating

### Exercise 3: Multiple Animations

Write a program that:
1. Creates 5 sprites with different animations
2. Updates all animations simultaneously
3. Renders all sprites
4. Each sprite moves independently

### Exercise 4: Easing Animation

Write a program that:
1. Animates a sprite moving from left to right
2. Uses ease-in-out interpolation
3. Loops the animation continuously
4. Smooth acceleration and deceleration

---

**Previous Section:** [Drawing Shapes and Tiles](./02_DrawingShapesAndTiles.md)  
**Next Chapter:** [Chapter 10: Input, State, and Interaction](../14_InputStateAndInteraction/README.md)  
**Language Specification:** See [Graphics Intrinsics](../../languageSpecification/intrinsicsAndDirectives/02_GraphicsIntrinsics.md)  
**Last Updated:** 2025-01-XX

