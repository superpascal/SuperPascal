# Zeal CPU (Z80)

**Part of:** [Chapter 12: Inside the Zeal Computer](./README.md)

---

> **For GCSE students:**  
> The CPU is the "brain" of the computer. The Zeal computer uses a Z80 CPU, which is an 8-bit processor. Think of it like the engine of a car - it's what makes everything run.
>
> **For A-Level students:**  
> The Z80 is a classic 8-bit microprocessor that processes 8 bits of data at a time. It has a 16-bit address bus, allowing it to access 64KB of memory directly. Understanding the CPU's capabilities helps you write efficient code.
>
> **For University students:**  
> The Z80 is a CISC (Complex Instruction Set Computer) architecture with a rich instruction set (~250 instructions). It uses a multi-cycle instruction execution model and has specialized registers for different purposes. *Detailed architecture analysis is covered in Part IV: Computers in Depth.*

---

## Introduction

The **CPU (Central Processing Unit)** is the heart of the Zeal computer. It executes your programs, performs calculations, and coordinates all the other components. Understanding the CPU helps you understand how your programs run.

**What you'll learn:**
- What the Z80 CPU is and why Zeal uses it
- The CPU's role in executing programs
- Basic CPU characteristics (8-bit, registers, instruction set)
- How the CPU interacts with other components

---

## What is the Z80 CPU?

### The Z80 Processor

**The Z80 is a classic 8-bit microprocessor:**
- **8-bit data bus** — Processes 8 bits (1 byte) at a time
- **16-bit address bus** — Can address 64KB of memory directly
- **Clock speed** — 10 MHz on Zeal (10 million cycles per second)
- **Instruction set** — ~250 instructions
- **Registers** — Special storage locations inside the CPU

**Why Z80?**
- **Educational value** — Simple enough to understand, complex enough to learn
- **Classic architecture** — Used in many retro computers (ZX Spectrum, MSX, etc.)
- **Well-documented** — Extensive resources available
- **Real-world relevance** — Still used in embedded systems today

### CPU's Role in the System

**The CPU is responsible for:**
1. **Executing instructions** — Running your program code
2. **Performing calculations** — Arithmetic and logic operations
3. **Managing memory** — Reading from and writing to memory
4. **Controlling peripherals** — Communicating with video, audio, I/O devices
5. **Making decisions** — Conditional logic and branching

**Visual representation:**
```
Your Program (in memory)
    ↓
CPU fetches instruction
    ↓
CPU decodes instruction
    ↓
CPU executes instruction
    ↓
Results stored in memory/registers
    ↓
Next instruction...
```

---

## CPU Characteristics

### 8-bit Architecture

**What "8-bit" means:**
- Processes **8 bits** (1 byte) at a time
- Registers are 8 bits wide
- Most operations work on single bytes
- Can combine registers for 16-bit operations

**Example:**
```pascal
var x: byte;  // 8-bit value (0-255)
x := 100;     // CPU processes this as 8 bits
```

**Why 8-bit matters:**
- **Simplicity** — Easier to understand than 16-bit or 32-bit
- **Educational** — Learn fundamentals without complexity
- **Efficiency** — Perfect for retro-style games and applications
- **Memory** — Smaller data types use less memory

### Registers

**Registers are fast storage inside the CPU:**
- **Much faster** than memory access
- **Limited number** — Only a few registers available
- **Specialized** — Different registers for different purposes

**Main Z80 registers:**
- **A (Accumulator)** — Main register for arithmetic
- **B, C, D, E, H, L** — General-purpose 8-bit registers
- **BC, DE, HL** — Can be used as 16-bit register pairs
- **IX, IY** — Index registers (for arrays/structures)
- **SP (Stack Pointer)** — Points to the stack
- **PC (Program Counter)** — Points to current instruction

**Why registers matter:**
- **Speed** — Operations on registers are fastest
- **Efficiency** — Compiler uses registers for variables
- **Understanding** — Helps you understand code generation

> **Note:** You don't need to manage registers directly in SuperPascal—the compiler handles this automatically. Understanding registers helps you understand how your code executes.

### Instruction Set

**The Z80 has ~250 instructions:**
- **Arithmetic** — ADD, SUB, INC, DEC
- **Logic** — AND, OR, XOR, NOT
- **Data movement** — LD (load), PUSH, POP
- **Control flow** — JP (jump), CALL, RET
- **Bit operations** — SET, RES, BIT
- **I/O** — IN, OUT

**Example instruction flow:**
```pascal
x := 5 + 3;
```

**Becomes Z80 instructions:**
```
LD A, 5      ; Load 5 into accumulator
ADD A, 3     ; Add 3 to accumulator
LD (x), A    ; Store result to memory location x
```

> **Note:** You write Pascal code, and the compiler translates it to Z80 instructions. Understanding this translation helps you write efficient code.

---

## CPU and Memory

### Memory Access

**The CPU accesses memory to:**
- **Load instructions** — Fetch program code
- **Read data** — Get variable values
- **Write data** — Store results
- **Access peripherals** — Communicate with hardware

**Memory addressing:**
- **16-bit addresses** — Can address 64KB directly
- **Memory-mapped I/O** — Hardware registers appear as memory locations
- **Banked memory** — MMU allows access to more than 64KB (see Memory Paging section)

**Memory access speed:**
- **Registers** — 1 cycle (fastest)
- **Cache** — 1-2 cycles (if available)
- **RAM** — Multiple cycles (slower)
- **I/O** — Multiple cycles (slowest)

### CPU-Memory Interaction

**How CPU and memory work together:**
```
1. CPU needs data → Sends address to memory
2. Memory responds → Sends data back to CPU
3. CPU processes → Performs operation
4. CPU stores result → Sends data and address to memory
```

**Why this matters:**
- **Memory is slower** than CPU operations
- **Minimizing memory access** improves performance
- **Understanding this** helps you write efficient code

---

## CPU and Other Components

### CPU Coordinates Everything

**The CPU is the central coordinator:**
- **Video system** — CPU tells video chip what to display
- **Audio system** — CPU sends audio data to sound chip
- **Input devices** — CPU reads keyboard/joystick state
- **Storage** — CPU reads/writes files
- **Communication** — CPU manages UART, I²C, GPIO

**Data flow example:**
```
Keyboard input → CPU processes → Updates game state → CPU tells video chip → Screen updates
```

### CPU Limitations

**Understanding CPU limitations helps you:**
- **Write efficient code** — Minimize CPU-intensive operations
- **Use hardware acceleration** — Let FPGA handle graphics
- **Optimize algorithms** — Choose efficient approaches
- **Balance workload** — Distribute work across components

**Example:**
```pascal
// ❌ SLOW: CPU does all graphics work
for y := 0 to 239 do
  for x := 0 to 319 do
    DrawPixel(x, y, color);  // CPU draws each pixel

// ✅ FAST: Use hardware acceleration
ZVB_SpriteSet(0, x, y, tile);  // FPGA draws sprite
```

---

## Summary

**Key Concepts:**
- **Z80 CPU** is an 8-bit processor that executes your programs
- **Registers** are fast storage inside the CPU
- **Instruction set** provides operations the CPU can perform
- **CPU coordinates** all system components
- **Understanding CPU** helps you write efficient code

**What's Next:**
- Learn about the **Video System (FPGA)** in the next section
- Understand how the CPU and video system work together
- Explore **Memory Paging** to understand how Zeal accesses more than 64KB

**For Deeper Study:**
- Part IV: Computers in Depth covers Z80 architecture in detail
- Part IV includes instruction cycle analysis and optimization techniques

---

**Next:** [Video System (FPGA)](./02_VideoSystem.md)

