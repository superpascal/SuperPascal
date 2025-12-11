# Video System (FPGA)

**Part of:** [Chapter 12: Inside the Zeal Computer](./README.md)

---

> **For GCSE students:**  
> The video system is what makes pictures appear on your screen. The Zeal computer uses an FPGA (a special chip that can be reprogrammed) to handle graphics very quickly. Think of it like having a dedicated artist that draws pictures while the CPU does other work.
>
> **For A-Level students:**  
> The Zeal Video Board (ZVB) is an FPGA-accelerated graphics subsystem. It handles sprite rendering, tilemaps, and video memory management in hardware, freeing the CPU for game logic. Understanding this separation of concerns is key to efficient game programming.
>
> **For University students:**  
> The ZVB implements a hardware-accelerated graphics pipeline using FPGA logic. It provides memory-mapped registers, DMA capabilities, and parallel processing for graphics operations. This offloads graphics work from the CPU, enabling real-time rendering at 60 FPS. *Detailed video system architecture is covered in Part IV: Computers in Depth.*

---

## Introduction

The **Video System** is responsible for displaying graphics on your screen. Unlike modern computers with abstract graphics APIs, the Zeal computer uses a dedicated **FPGA (Field-Programmable Gate Array)** to accelerate graphics operations. This makes graphics fast and efficient.

**What you'll learn:**
- What an FPGA is and why it's used for graphics
- How the Zeal Video Board (ZVB) works
- The relationship between CPU and video system
- Why hardware acceleration matters

---

## What is an FPGA?

### Field-Programmable Gate Array

**An FPGA is programmable hardware:**
- **Configurable logic** — Can be reprogrammed for different tasks
- **Hardware speed** — Runs at hardware speeds (very fast)
- **Parallel processing** — Can do multiple things simultaneously
- **Custom circuits** — Can implement specialized functions

**Think of it like this:**
- **CPU** = General-purpose worker (can do anything, but one task at a time)
- **FPGA** = Specialized worker (designed for specific tasks, very fast at those tasks)

**Why use FPGA for graphics?**
- **Speed** — Hardware is faster than software
- **Parallelism** — Can process multiple pixels simultaneously
- **Efficiency** — Frees CPU for game logic
- **Flexibility** — Can be updated/reconfigured

### FPGA vs CPU

**CPU approach (software rendering):**
```
CPU draws pixel 1 → CPU draws pixel 2 → CPU draws pixel 3 → ...
(Sequential, slow)
```

**FPGA approach (hardware rendering):**
```
FPGA draws pixels 1, 2, 3, 4, 5... simultaneously
(Parallel, fast)
```

**Result:**
- **CPU rendering:** ~1000 pixels per second (too slow for games)
- **FPGA rendering:** ~76,800 pixels per frame at 60 FPS (perfect for games)

---

## Zeal Video Board (ZVB)

### What is the ZVB?

**The ZVB is the Zeal Video Board:**
- **FPGA-based** — Uses programmable hardware
- **Hardware acceleration** — Graphics operations in hardware
- **Memory-mapped** — Appears as memory locations to the CPU
- **Multiple features** — Sprites, tiles, scrolling, text mode

**ZVB capabilities:**
- **Resolutions:** 320x240 or 640x480 pixels
- **Color modes:** 4-bit (16 colors) or 8-bit (256 colors)
- **Sprites:** Hardware sprite rendering
- **Tilemaps:** Background layers with scrolling
- **Text mode:** Character-based display
- **Palettes:** Color lookup tables

### How ZVB Works

**The ZVB operates independently:**
1. **CPU sets up graphics** — Tells ZVB what to display
2. **ZVB renders frame** — Draws everything automatically
3. **ZVB displays frame** — Sends to screen at 60 FPS
4. **CPU continues** — Can do other work while ZVB renders

**Visual representation:**
```
CPU:  [Setup sprites] → [Game logic] → [Update sprites] → [Game logic] → ...
ZVB:  [Render frame 1] → [Display] → [Render frame 2] → [Display] → ...
Time: ────────────────────────────────────────────────────────────────→
```

**Key insight:** CPU and ZVB work in parallel!

### ZVB Memory Model

**ZVB has its own video memory:**
- **Physical address:** 0x100000 (1MB mark)
- **Video RAM:** Stores sprites, tiles, palettes
- **Registers:** Control ZVB behavior (0x80-0xAF)
- **Memory-mapped:** CPU accesses like regular memory

**Memory layout:**
```
Video Memory (ZVB):
- Palette RAM (256 colors)
- Tile data (tileset graphics)
- Tilemap (background layout)
- Sprite attribute table (sprite positions)
- Font data (text characters)
```

**How CPU accesses ZVB:**
```pascal
// CPU writes to video memory
ZVB_WriteReg(ZVB_REG_SPRITE_X, 100);  // Set sprite X position
ZVB_WriteReg(ZVB_REG_SPRITE_Y, 50);   // Set sprite Y position
// ZVB automatically renders sprite on next frame
```

---

## CPU and Video System Interaction

### Division of Labor

**CPU handles:**
- **Game logic** — Update positions, check collisions, manage state
- **Input processing** — Read keyboard, joystick
- **Audio** — Play sounds, music
- **File I/O** — Load resources, save games

**ZVB handles:**
- **Graphics rendering** — Draw sprites, tiles, backgrounds
- **Frame timing** — 60 FPS display
- **Video memory** — Manage graphics data
- **Hardware acceleration** — Fast graphics operations

**Why this separation matters:**
- **Efficiency** — Each component does what it's best at
- **Performance** — Parallel processing (CPU + ZVB simultaneously)
- **Simplicity** — Clear responsibilities

### Communication Between CPU and ZVB

**CPU communicates with ZVB via:**
1. **Memory-mapped registers** — Control ZVB behavior
2. **Video memory** — Store graphics data
3. **DMA (Direct Memory Access)** — Fast bulk transfers

**Example workflow:**
```pascal
// 1. CPU updates game state
playerX := playerX + 1;

// 2. CPU tells ZVB to update sprite
ZVB_SpriteSetX(0, playerX);  // Update sprite position

// 3. ZVB renders frame automatically (in parallel with CPU)

// 4. CPU continues with other work
UpdateEnemies;
CheckCollisions;
```

**Key point:** CPU doesn't wait for ZVB—they work in parallel!

---

## Why Hardware Acceleration Matters

### Performance Comparison

**Software rendering (CPU only):**
- Draws pixels one at a time
- ~1000 pixels per second
- CPU is busy drawing, can't do game logic
- Result: Slow, choppy graphics

**Hardware acceleration (ZVB):**
- Draws pixels in parallel
- ~76,800 pixels per frame (320x240)
- CPU free for game logic
- Result: Smooth 60 FPS graphics

### Real-World Impact

**Without ZVB (CPU rendering):**
```
Frame 1: [Draw everything] ──────────────── 16ms (too slow!)
Frame 2: [Draw everything] ──────────────── 16ms
Result: 30 FPS, choppy
```

**With ZVB (hardware acceleration):**
```
Frame 1: [ZVB renders] ─── 1ms | [CPU logic] ─── 15ms
Frame 2: [ZVB renders] ─── 1ms | [CPU logic] ─── 15ms
Result: 60 FPS, smooth
```

**Benefits:**
- **Smooth graphics** — 60 FPS gameplay
- **Complex games** — CPU free for game logic
- **Better performance** — Hardware is faster than software
- **Professional results** — Games look and feel polished

---

## Summary

**Key Concepts:**
- **FPGA** is programmable hardware that runs very fast
- **ZVB** is the Zeal Video Board (FPGA-based graphics)
- **Hardware acceleration** makes graphics fast and efficient
- **CPU and ZVB work in parallel** — CPU does logic, ZVB does graphics
- **Understanding this separation** helps you write efficient games

**What's Next:**
- Learn about **Memory Paging** to understand how Zeal accesses more memory
- Explore **Communication Interfaces** (GPIO, I²C, UART)
- Understand how all components work together

**For Deeper Study:**
- Part V: Graphics covers ZVB programming in detail
- Part IV: Computers in Depth covers FPGA architecture and video system internals

---

**Next:** [Memory Paging](./03_MemoryPaging.md)

