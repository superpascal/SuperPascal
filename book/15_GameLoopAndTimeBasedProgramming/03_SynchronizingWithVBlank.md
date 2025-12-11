# Synchronizing with VBlank

**Part of:** [Chapter 11: Game Loop and Time-Based Programming](./README.md)

---

## Introduction

Synchronizing with the video system is crucial for smooth, flicker-free graphics. This section teaches you how to:
- **Understand VBlank** — What it is and why it matters
- **Synchronize with VBlank** — Use `WaitVBlank` correctly
- **Manage frame timing** — Consistent frame rates
- **Use delta time** — Frame-rate independent motion
- **Handle timing** — Timers, delays, and time-based logic

**Key concepts:**
- **VBlank** — Vertical blanking period (safe update time)
- **Frame rate** — Frames per second (60 FPS typical)
- **Delta time** — Time since last frame
- **Frame timing** — Consistent update intervals
- **Time-based programming** — Motion based on time, not frames

---

## Understanding VBlank

### What is VBlank?

**VBlank (Vertical Blanking) is the period when the electron beam returns to the top:**

- **Duration**: ~1-2 milliseconds (depends on system)
- **Frequency**: 60 times per second (60 Hz)
- **Purpose**: Safe time to update video memory without flicker

**Visual representation:**
```
Frame N:  [==========]  ← Active display (drawing)
          [==========]
          [==========]
          ...
          [==========]
          [VBlank]     ← Safe to update (beam returning)
Frame N+1: [==========]
          ...
```

### Why VBlank Matters

**Updating video memory during active display causes problems:**

- **Flicker** — Screen tears, visible updates
- **Artifacts** — Half-drawn sprites, corrupted graphics
- **Inconsistent timing** — Updates happen at random times

**Updating during VBlank is safe:**

- **No flicker** — Screen is blank
- **Smooth updates** — Changes take effect on next frame
- **Consistent timing** — Updates happen at predictable intervals

---

## Using WaitVBlank

### Basic Usage

**Wait for VBlank before updating graphics:**

```pascal
procedure RenderGame;
begin
  WaitVBlank;  // Wait for safe update period
  
  // Now safe to update video memory
  UpdateSprites;
  UpdateTilemap;
  // Changes appear on next frame
end;
```

### Where to Wait

**Wait for VBlank at the right time:**

```pascal
// ✅ GOOD: Wait before updating graphics
procedure RenderGame;
begin
  WaitVBlank;  // Wait first
  UpdateGraphics;  // Then update
end;

// ❌ BAD: Wait after updating
procedure RenderGame;
begin
  UpdateGraphics;  // May cause flicker
  WaitVBlank;  // Too late!
end;
```

### Game Loop with VBlank

**Standard game loop pattern:**

```pascal
begin
  InitGame;
  
  while true do
  begin
    UpdateGame;   // Update logic (no VBlank needed)
    WaitVBlank;   // Wait for safe period
    RenderGame;   // Update graphics (safe now)
  end;
end.
```

**Why this order?**
1. **Update first** — Process logic, don't wait
2. **Wait for VBlank** — Synchronize with video
3. **Render** — Update graphics safely

---

## Frame Timing

### Fixed Frame Rate

**60 FPS (frames per second) with VBlank:**

```pascal
// VBlank occurs 60 times per second
// Each WaitVBlank = 1/60 second = ~16.67ms

var
  frameCount: word;
begin
  frameCount := 0;
  
  while true do
  begin
    UpdateGame;
    WaitVBlank;  // Exactly 60 FPS
    RenderGame;
    
    frameCount := frameCount + 1;
    // frameCount increments 60 times per second
  end;
end;
```

### Frame Counter

**Track time using frame counter:**

```pascal
var
  frameCount: word;
  seconds: word;
begin
  frameCount := 0;
  seconds := 0;
  
  while true do
  begin
    UpdateGame;
    WaitVBlank;
    RenderGame;
    
    frameCount := frameCount + 1;
    
    // 60 frames = 1 second
    if frameCount >= 60 then
    begin
      frameCount := 0;
      seconds := seconds + 1;
      WriteLn('Seconds: ', seconds);
    end;
  end;
end;
```

### Timed Events

**Trigger events at specific times:**

```pascal
var
  frameCount: word;
  spawnTimer: word;
begin
  frameCount := 0;
  spawnTimer := 0;
  
  while true do
  begin
    UpdateGame;
    WaitVBlank;
    RenderGame;
    
    frameCount := frameCount + 1;
    spawnTimer := spawnTimer + 1;
    
    // Spawn enemy every 2 seconds (120 frames)
    if spawnTimer >= 120 then
    begin
      SpawnEnemy;
      spawnTimer := 0;
    end;
  end;
end;
```

---

## Delta Time

### What is Delta Time?

**Delta time is the time elapsed since the last frame:**

- **Purpose**: Make motion frame-rate independent
- **Unit**: Usually seconds (or milliseconds)
- **Calculation**: Current time - previous time

**Why use delta time?**
- **Consistent speed** — Motion same speed regardless of FPS
- **Smooth on slow systems** — Slower but correct
- **Accurate timing** — Real time, not frame-based

### Calculating Delta Time

**Simple delta time calculation:**

```pascal
var
  lastFrameTime: word;  // Milliseconds
  deltaTime: Q8.8;      // Seconds (fixed-point)
  
function GetTime: word;
begin
  // Get current time in milliseconds
  // (Platform-specific implementation)
  GetTime := GetSystemTime;
end;

procedure GameLoop;
var
  currentTime: word;
begin
  lastFrameTime := GetTime;
  
  while true do
  begin
    currentTime := GetTime;
    deltaTime := Q8_8(currentTime - lastFrameTime) / Q8_8(1000.0);  // Convert to seconds
    lastFrameTime := currentTime;
    
    UpdateGame(deltaTime);
    WaitVBlank;
    RenderGame;
  end;
end;
```

### Using Delta Time

**Apply delta time to motion:**

```pascal
const
  PLAYER_SPEED = 100;  // Pixels per second

procedure UpdatePlayer(dt: Q8.8);
begin
  // Move based on time, not frames
  if InputPressed(BTN_LEFT) then
    playerX := playerX - Trunc(PLAYER_SPEED * dt);
  if InputPressed(BTN_RIGHT) then
    playerX := playerX + Trunc(PLAYER_SPEED * dt);
end;
```

**Without delta time (frame-dependent):**
```pascal
// ❌ BAD: Frame-dependent
if InputPressed(BTN_LEFT) then
  playerX := playerX - 2;  // Moves 2 pixels per frame
  // At 60 FPS: 120 pixels/second
  // At 30 FPS: 60 pixels/second (slower!)
```

**With delta time (frame-independent):**
```pascal
// ✅ GOOD: Frame-independent
if InputPressed(BTN_LEFT) then
  playerX := playerX - Trunc(PLAYER_SPEED * dt);  // Moves based on time
  // At 60 FPS: 100 pixels/second
  // At 30 FPS: 100 pixels/second (same speed!)
```

---

## Time-Based Programming

### Velocity and Acceleration

**Physics based on time:**

```pascal
const
  GRAVITY = 980;  // Pixels per second per second

procedure UpdatePhysics(dt: Q8.8);
begin
  // Apply gravity (acceleration)
  playerVelocityY := playerVelocityY + Trunc(GRAVITY * dt);
  
  // Apply velocity (movement)
  playerY := playerY + Trunc(playerVelocityY * dt);
end;
```

### Timers

**Timers that work at any frame rate:**

```pascal
type
  TTimer = record
    Duration: Q8.8;  // Duration in seconds
    Elapsed: Q8.8;   // Elapsed time in seconds
    Active: boolean;
  end;

procedure UpdateTimer(var timer: TTimer; dt: Q8.8);
begin
  if timer.Active then
  begin
    timer.Elapsed := timer.Elapsed + dt;
    if timer.Elapsed >= timer.Duration then
    begin
      timer.Active := false;
      OnTimerComplete;
    end;
  end;
end;

function IsTimerComplete(const timer: TTimer): boolean;
begin
  IsTimerComplete := not timer.Active and (timer.Elapsed >= timer.Duration);
end;
```

### Animation Speed

**Animation speed based on time:**

```pascal
const
  ANIMATION_FPS = 8;  // 8 frames per second

type
  TAnimation = record
    CurrentFrame: byte;
    FrameTime: Q8.8;  // Time in current frame
  end;

procedure UpdateAnimation(var anim: TAnimation; dt: Q8.8);
begin
  anim.FrameTime := anim.FrameTime + dt;
  
  // Change frame every 1/ANIMATION_FPS seconds
  if anim.FrameTime >= Q8_8(1.0 / ANIMATION_FPS) then
  begin
    anim.CurrentFrame := (anim.CurrentFrame + 1) mod FRAME_COUNT;
    anim.FrameTime := anim.FrameTime - Q8_8(1.0 / ANIMATION_FPS);
  end;
end;
```

---

## Frame Rate Management

### Target Frame Rate

**Maintain consistent frame rate:**

```pascal
const
  TARGET_FPS = 60;
  FRAME_TIME_MS = 1000 div TARGET_FPS;  // ~16.67ms

var
  frameStartTime: word;
  frameEndTime: word;
  frameDuration: word;
  
procedure GameLoop;
begin
  while true do
  begin
    frameStartTime := GetTime;
    
    UpdateGame;
    WaitVBlank;  // Blocks until VBlank (60 FPS)
    RenderGame;
    
    frameEndTime := GetTime;
    frameDuration := frameEndTime - frameStartTime;
    
    // Log if frame took too long
    if frameDuration > FRAME_TIME_MS then
      WriteLn('Frame took ', frameDuration, 'ms (target: ', FRAME_TIME_MS, 'ms)');
  end;
end;
```

### Frame Skipping

**Skip rendering if running too slow:**

```pascal
var
  frameCount: word;
  skipRender: boolean;
begin
  frameCount := 0;
  skipRender := false;
  
  while true do
  begin
    // Always update (logic is important)
    UpdateGame;
    
    // Skip rendering every other frame if needed
    if not skipRender then
    begin
      WaitVBlank;
      RenderGame;
    end
    else
      WaitVBlank;  // Still wait for timing
    
    frameCount := frameCount + 1;
    skipRender := (frameCount mod 2) = 0;  // Skip every other frame
  end;
end;
```

**Note:** Frame skipping reduces visual quality but maintains game logic speed.

---

## Complete Timing Example

**Putting it all together:**

```pascal
program TimingDemo;

var
  lastFrameTime: word;
  frameCount: word;
  gameTime: Q8.8;  // Total game time in seconds

procedure InitGame;
begin
  lastFrameTime := GetTime;
  frameCount := 0;
  gameTime := Q8_8(0.0);
end;

procedure UpdateGame(dt: Q8.8);
begin
  // Update game time
  gameTime := gameTime + dt;
  
  // Update with delta time
  UpdatePlayer(dt);
  UpdateEnemies(dt);
  UpdatePhysics(dt);
  UpdateAnimations(dt);
  UpdateTimers(dt);
end;

procedure RenderGame;
begin
  // Render everything
  RenderBackground;
  RenderPlayer;
  RenderEnemies;
  RenderHUD;
  
  // Show FPS
  var fps := Trunc(Q8_8(1.0) / dt);
  DrawText(10, 10, 'FPS: ' + IntToStr(fps));
  DrawText(10, 20, 'Time: ' + FloatToStr(gameTime));
end;

begin
  InitGame;
  InitGraphics;
  
  while true do
  begin
    // Calculate delta time
    var currentTime := GetTime;
    var dt := Q8_8(currentTime - lastFrameTime) / Q8_8(1000.0);
    lastFrameTime := currentTime;
    
    // Cap delta time (prevent huge jumps)
    if dt > Q8_8(0.1) then  // Max 100ms
      dt := Q8_8(0.1);
    
    // Update and render
    UpdateGame(dt);
    WaitVBlank;
    RenderGame;
    
    frameCount := frameCount + 1;
  end;
end.
```

---

## Best Practices

### 1. Always Wait for VBlank

**Wait before updating graphics:**

```pascal
// ✅ GOOD
WaitVBlank;
UpdateGraphics;

// ❌ BAD
UpdateGraphics;  // May cause flicker
WaitVBlank;
```

### 2. Use Delta Time for Motion

**Make motion frame-rate independent:**

```pascal
// ✅ GOOD: Time-based
playerX := playerX + Trunc(SPEED * dt);

// ❌ BAD: Frame-based
playerX := playerX + 2;  // Speed depends on FPS
```

### 3. Cap Delta Time

**Prevent huge time jumps:**

```pascal
// ✅ GOOD: Cap delta time
if dt > Q8_8(0.1) then
  dt := Q8_8(0.1);  // Max 100ms

// ❌ BAD: No cap
// dt could be huge if game was paused
```

### 4. Update Before Rendering

**Update logic, then render:**

```pascal
// ✅ GOOD: Update then render
UpdateGame;
WaitVBlank;
RenderGame;

// ❌ BAD: Render then update
RenderGame;
UpdateGame;  // Wrong order
```

### 5. Track Frame Time

**Monitor performance:**

```pascal
// ✅ GOOD: Track frame time
var frameTime := GetTime - lastFrameTime;
if frameTime > 20 then
  WriteLn('Slow frame: ', frameTime, 'ms');

// ❌ BAD: No monitoring
// Don't know if game is running slow
```

---

## Exercises

### Exercise 1: VBlank Synchronization

Write a program that:
1. Uses `WaitVBlank` correctly
2. Updates graphics during VBlank
3. Maintains 60 FPS
4. Shows no flicker

### Exercise 2: Frame Timing

Write a program that:
1. Tracks frame count
2. Displays FPS
3. Triggers events at specific frame counts
4. Shows time elapsed

### Exercise 3: Delta Time

Write a program that:
1. Calculates delta time each frame
2. Uses delta time for motion
3. Maintains consistent speed at any FPS
4. Displays current FPS

### Exercise 4: Time-Based Systems

Write a program that:
1. Uses timers based on real time
2. Animates sprites at consistent speed
3. Applies physics with delta time
4. Handles variable frame rates gracefully

---

**Previous Section:** [Render Step](./02_RenderStep.md)  
**Next Chapter:** [Chapter 16: Entity Component System](../16_EntityComponentSystem/README.md)  
**Language Specification:** See [Graphics Intrinsics](../../languageSpecification/intrinsicsAndDirectives/02_GraphicsIntrinsics.md)  
**Last Updated:** 2025-01-XX

