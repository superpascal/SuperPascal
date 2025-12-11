# Memory Paging (Conceptual)

**Part of:** [Chapter 12: Inside the Zeal Computer](./README.md)

---

> **For GCSE students:**  
> The Z80 CPU can only see 64KB of memory at once, but the Zeal computer has 512KB of RAM. Memory paging is like having a big filing cabinet where you can only see one drawer at a time, but you can switch which drawer you're looking at.
>
> **For A-Level students:**  
> The Z80's 16-bit address bus limits direct addressing to 64KB. The Memory Management Unit (MMU) uses bank switching to map different 64KB pages of the 512KB RAM into the CPU's address space. This allows access to more memory than the CPU can directly address.
>
> **For University students:**  
> The MMU implements a bank-switching memory model using page tables. Physical memory is divided into 16KB pages that can be mapped into the CPU's 64KB logical address space. This extends the effective addressable memory while maintaining compatibility with the Z80's 16-bit addressing. *Detailed MMU architecture and paging algorithms are covered in Part IV: Computers in Depth.*

---

## Introduction

The Z80 CPU can only directly address 64KB of memory (because it has a 16-bit address bus). However, the Zeal computer has 512KB of RAM. **Memory paging** solves this problem by allowing the CPU to access different "pages" of memory at different times.

**What you'll learn:**
- Why memory paging is needed
- How memory paging works conceptually
- The relationship between physical and logical memory
- How the MMU (Memory Management Unit) manages paging

---

## The Problem: 64KB Limit

### Z80 Address Space

**The Z80 CPU has limitations:**
- **16-bit addresses** — Can address 2^16 = 65,536 bytes = 64KB
- **Direct addressing** — Can only see 64KB at once
- **Physical limitation** — Built into the CPU architecture

**Visual representation:**
```
Z80 CPU can see:
[0x0000 ──────────────────────────────────── 0xFFFF]
        64KB address space
```

**But Zeal has:**
```
Physical RAM:
[0x00000 ──────────────────────────────────── 0x7FFFF]
        512KB of RAM (8x more than CPU can see!)
```

**Problem:** How do we access all 512KB if the CPU can only see 64KB?

---

## The Solution: Memory Paging

### What is Memory Paging?

**Memory paging divides memory into "pages":**
- **Physical memory** — The actual 512KB RAM
- **Logical memory** — What the CPU sees (64KB)
- **Pages** — Chunks of memory that can be swapped in/out
- **MMU** — Memory Management Unit that handles paging

**Think of it like a book:**
- **Physical book** — 512 pages (512KB RAM)
- **Logical view** — You can only see 64 pages at once (64KB visible)
- **Page turning** — Switch which 64 pages you're looking at
- **Bookmark** — MMU remembers which pages are visible

### How Paging Works

**The MMU maps physical pages to logical addresses:**
```
Physical Memory (512KB):
Page 0: [0x00000-0x03FFF]  ──┐
Page 1: [0x04000-0x07FFF]  ──┤
Page 2: [0x08000-0x0BFFF]  ──┼─→ MMU maps to
Page 3: [0x0C000-0x0FFFF]  ──┤   Logical Memory
...                          │   (64KB visible)
Page 31: [0x7C000-0x7FFFF] ──┘

Logical Memory (64KB - what CPU sees):
[0x0000-0x3FFF] ← Page 0
[0x4000-0x7FFF] ← Page 1
[0x8000-0xBFFF] ← Page 2
[0xC000-0xFFFF] ← Page 3
```

**Key insight:** CPU always sees 64KB, but which 64KB changes!

### Page Switching

**To access different memory:**
1. **CPU requests** access to a memory location
2. **MMU checks** if that page is currently mapped
3. **If not mapped:** MMU switches pages (maps new page)
4. **CPU accesses** memory (now visible)

**Example:**
```pascal
// Access data in page 5 (physical 0x14000-0x17FFF)
// MMU automatically maps page 5 to logical 0x4000-0x7FFF
var data: byte;
data := Peek($5000);  // MMU maps this to physical page 5
```

**Note:** In SuperPascal, the compiler and runtime handle page switching automatically. You don't need to manage pages manually.

---

## MMU (Memory Management Unit)

### What is the MMU?

**The MMU is hardware that manages paging:**
- **Page tables** — Tracks which physical pages are mapped
- **Address translation** — Converts logical → physical addresses
- **Page switching** — Swaps pages in/out automatically
- **Transparent** — CPU doesn't know paging is happening

**MMU responsibilities:**
1. **Map pages** — Connect physical pages to logical addresses
2. **Translate addresses** — Convert CPU addresses to physical addresses
3. **Handle switching** — Swap pages when needed
4. **Maintain consistency** — Ensure correct mapping

### MMU Operation

**When CPU accesses memory:**
```
CPU: "I want address 0x5000"
    ↓
MMU: "0x5000 is in logical page 1"
    ↓
MMU: "Logical page 1 maps to physical page 5"
    ↓
MMU: "Physical address = 0x14000 + 0x1000 = 0x15000"
    ↓
RAM: Returns data from 0x15000
```

**Key point:** MMU does this automatically and transparently!

---

## Benefits of Memory Paging

### Access to More Memory

**Without paging:**
- CPU limited to 64KB
- Can't use full 512KB RAM
- Large programs won't fit

**With paging:**
- CPU can access all 512KB (by switching pages)
- Large programs can use more memory
- More flexibility for complex games

### Efficient Memory Use

**Paging allows:**
- **Code pages** — Store program code in different pages
- **Data pages** — Store game data in different pages
- **Resource pages** — Store graphics/audio in different pages
- **Dynamic loading** — Load pages as needed

**Example:**
```
Page 0-1: Program code
Page 2-3: Game data
Page 4-5: Graphics tiles
Page 6-7: Audio samples
... (can switch between them)
```

### Isolation and Protection

**Paging provides:**
- **Memory isolation** — Different pages for different purposes
- **Protection** — Can mark pages as read-only
- **Organization** — Clear separation of code/data/resources

---

## Conceptual Understanding

### The "Window" Metaphor

**Think of paging like windows:**
- **Physical memory** — The entire building (512KB)
- **Logical memory** — The window you're looking through (64KB)
- **MMU** — Moves the window to see different parts of the building
- **Page switching** — Changing which window you're looking through

**Visual:**
```
Building (512KB RAM):
┌─────────────────────────────────┐
│ Floor 0 (Page 0)                 │
│ Floor 1 (Page 1)                 │ ← Window (64KB visible)
│ Floor 2 (Page 2)                 │
│ ...                              │
│ Floor 31 (Page 31)               │
└─────────────────────────────────┘
```

### The "Book" Metaphor

**Think of paging like a book:**
- **Physical book** — 512 pages (512KB RAM)
- **Logical view** — 64 pages visible at once
- **MMU** — The bookmark that marks your current position
- **Page turning** — Switching which 64 pages are visible

---

## Summary

**Key Concepts:**
- **Z80 limitation** — Can only directly address 64KB
- **Zeal has 512KB** — 8x more memory than CPU can see
- **Memory paging** — Solution that allows access to all 512KB
- **MMU** — Hardware that manages paging automatically
- **Page switching** — Swapping which pages are visible to CPU
- **Transparent** — CPU doesn't know paging is happening

**What's Next:**
- Learn about **Communication Interfaces** (GPIO, I²C, UART)
- Understand how all components work together
- See how programs use memory in practice

**For Deeper Study:**
- Part IV: Computers in Depth covers MMU architecture in detail
- Part IV includes paging algorithms and memory management strategies

---

**Next:** [Communication Interfaces](./04_CommunicationInterfaces.md)

