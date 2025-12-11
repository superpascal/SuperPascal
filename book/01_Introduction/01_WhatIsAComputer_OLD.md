# What is a Computer?

**Part of:** [Chapter 01: Introduction to Programming on Zeal](./README.md)

---

> **For GCSE students:**  
> A computer is like a very fast calculator that can remember things. It has a brain (CPU) that does calculations, memory to store information, and ways to show you results (screen) and get your input (keyboard).
>
> **For A-Level students:**  
> A computer is a machine that processes data according to stored instructions. It consists of a CPU (processor), memory (RAM), storage, and I/O devices. Understanding these components helps you write efficient code.
>
> **For University students:**  
> A computer is a von Neumann architecture machine with a CPU, memory hierarchy, I/O subsystem, and storage. Understanding the architecture—including instruction execution, memory addressing, and platform-specific characteristics—is essential for systems programming and optimization.

---

## Introduction

Before we can write programs, we need to understand what a computer is and how it works. This knowledge will help you write better code and understand why SuperPascal is designed the way it is.

A computer is a machine that:
1. **Stores data** in memory
2. **Processes data** using a CPU
3. **Follows instructions** from programs
4. **Interacts** with the outside world through I/O

---

## The Basic Components

### CPU (Central Processing Unit)

The **CPU** is the "brain" of the computer. It:
- **Executes instructions** — Runs programs step by step
- **Performs calculations** — Arithmetic and logic operations
- **Controls flow** — Decides what to do next
- **Manages data** — Moves data between memory and registers

#### CPU Characteristics

**Clock Speed:**
- How fast the CPU runs (measured in MHz or GHz)
- Higher clock = faster execution (generally)
- Different platforms have different clock speeds

**Architecture:**
- **8-bit** — Processes 8 bits at a time (ZealZ80: Z80)
- **16-bit** — Processes 16 bits at a time (Foenix65C816: WDC65C816S)
- **32-bit** — Processes 32 bits at a time (FoenixA2560M: MC68LC060)

**Different Kinds of CPUs:**
- Many CPU designs exist (Z80, 6502, 65C816, 68000, etc.)
- All follow the same basic principles: fetch → decode → execute
- Simple 8-bit examples (Z80) vs more advanced 16-bit designs (65C816)
- We'll learn about different CPUs as we progress through the book

**Registers:**
- Small, fast storage inside the CPU
- Used for calculations and temporary data
- Limited in number (typically 8-16 registers)

### Memory (RAM)

**Memory** stores:
- **Program code** — The instructions to execute
- **Data** — Variables, arrays, structures
- **Stack** — Function calls and local variables
- **Heap** — Dynamically allocated memory

#### Memory Characteristics

**Size:**
- Measured in bytes (KB, MB, GB)
- Limited on retro platforms (512KB-2MB typical)
- Every byte matters on 8-bit systems

**Speed:**
- Much slower than CPU registers
- Accessing memory takes time (memory cycles)
- Cache helps (if available)

**Addressing:**
- Each byte has an **address** (like a house number)
- Programs access memory by address
- Address space depends on CPU (8-bit: 64KB, 16-bit: 16MB, 32-bit: 4GB)

### Storage

**Storage** (Flash, SD card) holds:
- **Programs** — Your SuperPascal programs
- **Data files** — Save games, assets, configurations
- **Operating system** — System software (if any)

**Differences from Memory:**
- **Non-volatile** — Data persists when power is off
- **Slower** — Much slower than RAM
- **Larger** — Typically more capacity than RAM

---

## SuperPascal Target Platforms

SuperPascal supports multiple retro computing platforms. Understanding these platforms helps you write efficient code.

### ZealZ80 — Zeal 8-bit Computer

**CPU:** Z80 @ 10 MHz
- **8-bit processor** — Classic 8-bit architecture
- **64KB address space** — Direct addressing
- **MMU (Memory Management Unit)** — Banks memory for larger programs

**Memory:**
- **512 KB SRAM** — Main memory
- **MMU banking** — Maps 512KB into 64KB address space

**Graphics:**
- **ZVB (Zeal Video Board)** — FPGA-accelerated graphics
- **Sprites, tilemaps, layers** — Hardware-accelerated rendering
- **VBlank synchronization** — Frame-based timing

**Audio:**
- **ZVB Sound Controller** — Hardware audio
- **Sound effects and music** — Built-in audio system

**Why ZealZ80?**
- **Classic 8-bit** — Learn true 8-bit programming
- **Modern hardware** — FPGA acceleration for graphics/audio
- **Educational** — Perfect for learning fundamentals

### Foenix65C816 — Foenix F256x Systems

**CPU:** WDC W65C816S @ 6.29 MHz
- **16-bit processor** — 16-bit internal, 8-bit external bus
- **24-bit addressing** — 16MB address space
- **Native and emulation modes** — 65C02 compatible

**Memory:**
- **512KB-2MB SRAM** — Model-dependent
- **Optional 256KB expansion** — Expansion slot
- **128MB DDR3** — On F256K2 model

**Graphics:**
- **VICKY graphics chips** — TinyVicky, TinyVicky II, or Vicky "The Fourth"
- **Text, graphics, sprites, tilemaps** — Multiple graphics modes
- **Model-dependent features** — Different capabilities per model

**Audio:**
- **PSG, SID, OPL3** — Multiple audio chips (FPGA emulation or real)
- **CODEC, PWM, SAM2695, VS1053b** — Additional audio options

**Why Foenix65C816?**
- **16-bit power** — More capable than 8-bit
- **Extended memory** — 16MB address space
- **Multiple models** — F256Jr, F256JrJr, F256K, F256K2

### FoenixA2560M — Foenix A2560M/A2560X

**CPU:** MC68LC060 @ 66 MHz
- **32-bit processor** — Modern 32-bit architecture
- **1GB DDR3** — Large memory capacity
- **High performance** — Fastest of the three platforms

**Graphics:**
- **VICKY III Mark II** — Advanced graphics engine
- **Multiple layers** — Complex graphics capabilities

**Audio:**
- **Multiple chips** — OPM, OPN2, PSG, OPL3, SID, SAM2695, VS1053B
- **Professional audio** — High-quality audio processing

**Why FoenixA2560M?**
- **32-bit power** — Modern computing capabilities
- **Large memory** — 1GB DDR3
- **Professional development** — Serious retro computing

---

## The Z80 and FPGA Overview

### Z80 CPU (ZealZ80)

The **Z80** is a classic 8-bit microprocessor from 1976:
- **8-bit data bus** — Processes 8 bits at a time
- **16-bit address bus** — Can address 64KB directly
- **Registers:** A, B, C, D, E, H, L, IX, IY, SP, PC
- **Instruction set** — ~250 instructions
- **Still used today** — In embedded systems and retro computers

**Why Z80?**
- **Classic architecture** — Learn fundamental 8-bit concepts
- **Well-documented** — Extensive resources available
- **Educational value** — Perfect for understanding computers

### FPGA (Field-Programmable Gate Array)

**FPGA** is programmable hardware:
- **Configurable logic** — Can be reprogrammed
- **Hardware acceleration** — Faster than software
- **Custom peripherals** — Graphics, audio, I/O controllers

**In ZealZ80:**
- **ZVB (Zeal Video Board)** — FPGA-accelerated graphics
- **Hardware sprites** — Sprite rendering in hardware
- **Audio controller** — Sound processing in hardware
- **DMA** — Direct memory access for fast transfers

**Benefits:**
- **Performance** — Hardware is faster than software
- **Flexibility** — Can be updated/reconfigured
- **Integration** — Custom hardware for specific needs

---

## What Makes Zeal Unique?

### Educational Focus

Zeal is designed for **learning**:
- **Offline-first** — No internet required
- **Complete toolchain** — Everything included
- **Clear documentation** — Understand how it works
- **Open hardware** — Learn from the design

### Modern Retro

Zeal combines:
- **Classic 8-bit CPU** — Z80 for authenticity
- **Modern FPGA** — Graphics/audio acceleration
- **Contemporary tools** — Modern development environment
- **Best of both** — Classic feel, modern capabilities

### Performance

Zeal offers:
- **10 MHz Z80** — Fast for 8-bit
- **FPGA acceleration** — Graphics/audio in hardware
- **512KB RAM** — Plenty for 8-bit programs
- **MMU banking** — Access to full memory

---

## How Computers Execute Programs

### The Execution Cycle

1. **Fetch** — CPU reads instruction from memory
2. **Decode** — CPU understands what the instruction does
3. **Execute** — CPU performs the operation
4. **Store** — CPU saves results (if any)
5. **Repeat** — Move to next instruction

### Program Flow

Programs execute **sequentially** (one instruction at a time):
- **Top to bottom** — Generally executes in order
- **Conditional jumps** — `if` statements change flow
- **Loops** — Repeat sections of code
- **Function calls** — Jump to other code, return later

### Memory Access

Programs access memory to:
- **Read data** — Get variable values
- **Write data** — Store variable values
- **Execute code** — CPU reads instructions from memory
- **Manage stack** — Function calls use stack memory

---

## Understanding Addresses

### Memory Addresses

Every byte in memory has an **address**:
- **Like house numbers** — Each location has a unique number
- **Hexadecimal notation** — Usually written in hex (e.g., `$8000`)
- **Platform-dependent** — Different platforms have different address spaces

**Example:**
```
Address    Value    Meaning
$0000      $3E      Instruction: LD A, (load into A register)
$0001      $42      Data: The value 42 (decimal 66)
$0002      $C9      Instruction: RET (return)
```

### Address Spaces

**ZealZ80 (8-bit):**
- **64KB address space** — $0000 to $FFFF
- **MMU banking** — Maps 512KB into 64KB windows
- **Direct addressing** — Can access any address in range

**Foenix65C816 (16-bit):**
- **16MB address space** — $000000 to $FFFFFF (24-bit addressing)
- **IO Page architecture** — Hardware registers in specific ranges
- **Extended memory** — Can access large memory space

**FoenixA2560M (32-bit):**
- **4GB address space** — Full 32-bit addressing
- **Large memory** — 1GB DDR3 available
- **Modern architecture** — Contemporary memory model

---

## Platform Comparison

| Feature | ZealZ80 | Foenix65C816 | FoenixA2560M |
|---------|---------|--------------|--------------|
| **CPU** | Z80 @ 10 MHz | WDC65C816S @ 6.29 MHz | MC68LC060 @ 66 MHz |
| **Architecture** | 8-bit | 16-bit | 32-bit |
| **Address Space** | 64KB (512KB via MMU) | 16MB | 4GB |
| **RAM** | 512KB SRAM | 512KB-2MB SRAM | 1GB DDR3 |
| **Graphics** | ZVB (FPGA) | VICKY chips | VICKY III Mark II |
| **Audio** | ZVB Sound | Multiple chips | Professional audio |
| **Best For** | Learning 8-bit | 16-bit development | High-performance |

---

## Why This Matters for Programming

Understanding hardware helps you:

1. **Write efficient code** — Know what's fast and what's slow
2. **Manage memory** — Understand limitations and constraints
3. **Optimize performance** — Make code run faster
4. **Debug problems** — Understand what's happening
5. **Choose algorithms** — Pick the right approach for the platform

### Example: Memory Awareness

**Inefficient (doesn't consider memory):**
```pascal
// Allocates large array on stack (might overflow)
var bigArray: array[0..9999] of integer;
```

**Efficient (memory-aware):**
```pascal
// Uses heap allocation for large data
var bigArray: ^array[0..9999] of integer;
New(bigArray);
// ... use bigArray^ ...
Dispose(bigArray);
```

---

## Summary

**Key Concepts:**
- **CPU** executes instructions and processes data
- **Memory** stores programs and data
- **Storage** holds programs and files persistently
- **Addresses** identify memory locations
- **Platforms** have different capabilities and constraints

**SuperPascal Platforms:**
- **ZealZ80** — 8-bit, 64KB address space, FPGA acceleration
- **Foenix65C816** — 16-bit, 16MB address space, multiple models
- **FoenixA2560M** — 32-bit, 4GB address space, high performance

**Next:** Learn what programs are and how algorithms work.

---

**Next Section:** [Programs and Algorithms](./02_ProgramsAndAlgorithms.md)  
**Language Specification:** See [Platform Specifications](../../platforms/README.md)  
**Last Updated:** 2025-01-XX

