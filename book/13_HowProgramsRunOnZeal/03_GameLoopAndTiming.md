# Game Loop and Timing

**Part of:** [Chapter 13: How Programs Run on Zeal](./README.md)

---

> **For GCSE students:**  
> Games run in a loop that repeats over and over. Each time through the loop, the game updates (moves things, checks collisions) and then draws everything on the screen. The game waits for the right time (VBlank) before drawing to avoid flickering.
>
> **For A-Level students:**  
> The game loop is the fundamental pattern for interactive programs. It consists of update (logic) and render (graphics) phases, synchronized with the video system via VBlank. Understanding timing, frame rates, and synchronization is essential for smooth gameplay.
>
> **For University students:**  
> The game loop implements a discrete-time simulation with fixed or variable timesteps. VBlank synchronization ensures frame-accurate rendering and prevents visual artifacts. Frame timing, delta time, and interpolation enable smooth, frame-rate independent motion. *Detailed game loop architectures, timing algorithms, and synchronization techniques are covered in Part V: Game Development and Part IV: Computers in Depth.*

---

## Introduction

Games and interactive programs need to run continuously, updating and displaying content smoothly. The **game loop** is the pattern that makes this possible. Understanding game loops and timing helps you create smooth, responsive programs.

**What you'll learn:**
- What a game loop is and why it's needed
- The relationship between update and render phases
- How timing and VBlank synchronization work conceptually
- Why frame rates and timing matter

---

## What is a Game Loop?

### The Basic Concept

**A game loop repeats continuously:**
1. **Update** — Process input, update game state
2. **Render** — Draw everything on screen
3. **Wait** — Synchronize with video system
4. **Repeat** — Do it again

**Visual representation:**
```
Start
  ↓
Update (process input, move objects, check collisions)
  ↓
Render (draw everything)
  ↓
Wait (synchronize with video)
  ↓
Repeat ────────────────────┐
                            │
                            └─── (back to Update)
```

**Key insight:** Games are interactive—they need to respond to input and update continuously.

### Why a Loop?

**Games need to:**
- **Respond to input** — Keyboard, joystick, etc.
- **Update continuously** — Objects move, time passes
- **Display changes** — Show updated graphics
- **Run smoothly** — 60 frames per second (FPS)

**A loop makes this possible:**
- **Continuous execution** — Program keeps running
- **Regular updates** — Updates happen every frame
- **Smooth display** — Graphics update 60 times per second

---

## Game Loop Structure

### Basic Game Loop

**Simple game loop pattern:**
```pascal
begin
  InitializeGame;
  
  while true do  // Loop forever
  begin
    UpdateGame;    // Update game state
    RenderGame;    // Draw everything
    WaitVBlank;   // Synchronize with video
  end;
end.
```

**What happens:**
1. **InitializeGame** — Set up graphics, load resources
2. **UpdateGame** — Process input, update positions, check collisions
3. **RenderGame** — Draw sprites, backgrounds, UI
4. **WaitVBlank** — Wait for safe time to update video
5. **Repeat** — Do it all again (60 times per second)

### Update Phase

**The update phase handles logic:**
- **Input processing** — Read keyboard, joystick
- **Game state updates** — Move objects, update positions
- **Collision detection** — Check if objects collide
- **Game logic** — Rules, scoring, win/lose conditions

**Example:**
```pascal
procedure UpdateGame;
begin
  // Process input
  if KeyPressed(KEY_LEFT) then
    playerX := playerX - 1;
  
  // Update game state
  UpdatePlayer;
  UpdateEnemies;
  CheckCollisions;
  
  // Game logic
  if score >= 1000 then
    GameWin;
end;
```

**Key point:** Update happens every frame, regardless of rendering.

### Render Phase

**The render phase handles graphics:**
- **Clear screen** — Prepare for new frame
- **Draw background** — Tiles, scrolling layers
- **Draw objects** — Sprites, characters, items
- **Draw UI** — Score, health, menus

**Example:**
```pascal
procedure RenderGame;
begin
  ClearScreen;
  DrawBackground;
  DrawPlayer;
  DrawEnemies;
  DrawUI;
end;
```

**Key point:** Render draws the current state to screen.

---

## Timing and Synchronization

### Why Timing Matters

**Games need consistent timing:**
- **Smooth motion** — Objects move at consistent speed
- **Predictable behavior** — Same input = same result
- **Frame rate** — 60 FPS for smooth gameplay
- **Synchronization** — Graphics update at right time

**Without proper timing:**
- Motion is jerky
- Frame rate varies
- Graphics flicker
- Game feels unresponsive

**With proper timing:**
- Motion is smooth
- Consistent 60 FPS
- No flickering
- Game feels responsive

### VBlank Synchronization

**VBlank (Vertical Blanking) is the safe time to update graphics:**
- **Occurs 60 times per second** — Once per frame
- **Screen is blank** — Electron beam returning to top
- **Safe to update** — No flickering or tearing
- **Synchronization point** — Ensures consistent frame rate

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

**Why VBlank matters:**
- **During active display:** Updating video memory causes flicker
- **During VBlank:** Safe to update (screen is blank)
- **Synchronization:** `WaitVBlank` ensures updates happen at right time

### Using VBlank

**Wait for VBlank before rendering:**
```pascal
procedure GameLoop;
begin
  while true do
  begin
    UpdateGame;     // Update logic (no VBlank needed)
    WaitVBlank;     // Wait for safe update time
    RenderGame;     // Update graphics (safe now)
  end;
end;
```

**Why this order?**
1. **Update first** — Process logic, don't wait
2. **Wait for VBlank** — Synchronize with video
3. **Render** — Update graphics safely

**Result:** Smooth 60 FPS with no flickering.

---

## Frame Rate

### What is Frame Rate?

**Frame rate (FPS) = frames per second:**
- **60 FPS** — 60 frames per second (smooth)
- **30 FPS** — 30 frames per second (acceptable)
- **< 30 FPS** — Choppy, unresponsive

**Frame timing:**
- **60 FPS** = 16.67 milliseconds per frame
- **30 FPS** = 33.33 milliseconds per frame

**Visual:**
```
Time: 0ms    16.67ms   33.33ms   50ms     66.67ms
      │        │         │         │         │
Frame: 1       2         3         4         5
      └────────┴─────────┴─────────┴─────────┘
      60 FPS (16.67ms per frame)
```

### Maintaining Frame Rate

**VBlank ensures 60 FPS:**
```pascal
while true do
begin
  UpdateGame;
  WaitVBlank;  // Blocks until next VBlank (ensures 60 FPS)
  RenderGame;
end;
```

**How it works:**
- **WaitVBlank blocks** — Waits until next VBlank
- **VBlank occurs every 16.67ms** — Exactly 60 times per second
- **Result:** Consistent 60 FPS

**Benefits:**
- **Smooth gameplay** — Consistent frame rate
- **Predictable timing** — Each frame takes same time
- **No flickering** — Updates happen at safe time

---

## Conceptual Understanding

### The "Movie" Metaphor

**Think of a game like a movie:**
- **Frames** — Each frame is a still image
- **Frame rate** — How many frames per second (60 FPS)
- **Animation** — Rapid display of frames creates motion
- **Update** — Each frame shows slightly different state

**Visual:**
```
Frame 1: Player at (10, 20)
Frame 2: Player at (11, 20)  ← Moved right
Frame 3: Player at (12, 20)  ← Moved right
Frame 4: Player at (13, 20)  ← Moved right
...
Result: Smooth motion to the right
```

### The "Clock" Metaphor

**Think of timing like a clock:**
- **Ticks** — Each VBlank is a "tick"
- **60 ticks per second** — Like a clock with 60 ticks
- **Update on each tick** — Game updates every tick
- **Consistent timing** — Same time between ticks

---

## Summary

**Key Concepts:**
- **Game loop** — Continuous update and render cycle
- **Update phase** — Process input, update game state
- **Render phase** — Draw everything on screen
- **VBlank** — Safe time to update graphics (60 times per second)
- **Frame rate** — 60 FPS for smooth gameplay
- **Synchronization** — WaitVBlank ensures consistent timing

**What's Next:**
- Part V: Game Development covers game loop implementation in detail
- Part V includes update/render patterns, timing techniques, and optimization

**For Deeper Study:**
- Part V: Game Development — Detailed game loop implementation
- Part IV: Computers in Depth — Timing algorithms and synchronization techniques

---

**Next Chapter:** [Chapter 11: Graphics](../11_Graphics/README.md) — Learn to program graphics in detail

