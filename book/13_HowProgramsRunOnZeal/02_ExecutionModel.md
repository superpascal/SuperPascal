# Execution Model

**Part of:** [Chapter 13: How Programs Run on Zeal](./README.md)

---

> **For GCSE students:**  
> Once your program is loaded, the CPU runs it step by step. The program interacts with the Zeal OS (operating system) to use hardware like the screen, keyboard, and storage. Think of the OS as a helper that manages all the hardware for your program.
>
> **For A-Level students:**  
> Programs execute in a runtime environment provided by Zeal OS. The OS manages memory, I/O, and hardware access. Programs interact with hardware through OS services and direct memory-mapped I/O. Understanding the execution model helps you write efficient code and debug runtime issues.
>
> **For University students:**  
> The execution model combines bare-metal execution with OS services. Programs run in supervisor mode with direct hardware access via memory-mapped I/O, while Zeal OS provides file system, memory management, and device abstraction services. The runtime manages stack frames, heap allocation, and exception handling. *Detailed execution model, memory layout, and ABI specifications are covered in Part IV: Computers in Depth.*

---

## Introduction

Once your program is compiled and loaded, it needs to **execute** on the Zeal computer. Understanding the **execution model** helps you understand how your program interacts with the system, uses hardware, and runs efficiently.

**What you'll learn:**
- How programs execute on Zeal
- The role of Zeal OS in program execution
- How programs interact with hardware
- Runtime environment and system integration

---

## Program Execution

### The Execution Cycle

**Programs execute in cycles:**
1. **CPU fetches** instruction from memory
2. **CPU decodes** instruction (understands what to do)
3. **CPU executes** instruction (performs operation)
4. **CPU stores** results (saves to memory/registers)
5. **Repeat** — Next instruction

**Visual representation:**
```
Memory:  [Instruction 1] [Instruction 2] [Instruction 3] ...
           ↓                ↓                ↓
CPU:    Fetch → Decode → Execute → Store
           ↓                ↓                ↓
        [Result 1]      [Result 2]      [Result 3]
```

**Key insight:** Programs run as a sequence of instructions executed one at a time.

### Instruction Execution

**Simple example:**
```pascal
x := 5 + 3;
```

**Execution steps:**
1. **Fetch:** CPU gets "load 5" instruction
2. **Execute:** CPU loads 5 into register A
3. **Fetch:** CPU gets "add 3" instruction
4. **Execute:** CPU adds 3 to register A (A = 8)
5. **Fetch:** CPU gets "store" instruction
6. **Execute:** CPU stores 8 to memory location x

**Each step takes time:**
- **Register operations:** 1 cycle (very fast)
- **Memory access:** Multiple cycles (slower)
- **I/O operations:** Many cycles (slowest)

---

## Zeal OS Role

### What is Zeal OS?

**Zeal OS is the operating system:**
- **Manages hardware** — Coordinates CPU, memory, I/O
- **Provides services** — File system, memory management
- **Loads programs** — Reads executables from storage
- **Runs programs** — Starts execution and manages runtime

**Think of Zeal OS as:**
- **Manager** — Coordinates all system components
- **Helper** — Provides services your program needs
- **Interface** — Connects your program to hardware

### OS Services

**Zeal OS provides:**
- **File system** — Read/write files from storage
- **Memory management** — Allocates RAM for programs
- **Device drivers** — Interfaces with hardware
- **Runtime support** — Libraries and system calls

**Example:**
```pascal
// Your program requests file read
ReadFile('data.txt', buffer);

// Zeal OS handles:
// 1. Finds file on storage
// 2. Reads data from storage
// 3. Copies to program memory
// 4. Returns to your program
```

### Program-OS Interaction

**How programs interact with OS:**
```
Your Program
    ↓ (requests service)
Zeal OS
    ↓ (accesses hardware)
Hardware (storage, I/O, etc.)
    ↓ (returns data)
Zeal OS
    ↓ (provides result)
Your Program
```

**Key point:** Programs don't access hardware directly—they go through Zeal OS (usually).

---

## Hardware Access

### Direct Hardware Access

**Some hardware is directly accessible:**
- **Memory-mapped I/O** — Hardware registers appear as memory
- **Video system** — ZVB registers at specific addresses
- **GPIO** — I/O ports directly accessible

**Example:**
```pascal
// Direct access to video register
ZVB_WriteReg($80, value);  // Write to ZVB register 0x80

// This writes directly to hardware (no OS involved)
```

### Memory-Mapped I/O

**Hardware registers as memory:**
```
Memory Address Space:
0x0000-0x7FFF  → Program RAM
0x8000-0x80FF  → ZVB Registers (video hardware)
0x8100-0x81FF  → Audio Registers
0x8200-0x82FF  → GPIO Registers
...
```

**Accessing hardware:**
```pascal
// Video register at 0x8000
var vreg: ^byte;
vreg := Pointer($8000);
vreg^ := $FF;  // Write directly to video hardware
```

**Why this matters:**
- **Fast** — Direct access (no OS overhead)
- **Low-level** — Full control over hardware
- **Efficient** — Essential for games and real-time applications

---

## Runtime Environment

### What is Runtime?

**Runtime provides:**
- **System libraries** — Standard functions (WriteLn, ReadLn, etc.)
- **Memory management** — Heap allocation, garbage collection (if any)
- **Exception handling** — Error management
- **Initialization** — Sets up program environment

**Runtime components:**
```
Your Program
    ↓ (calls)
Runtime Libraries
    ↓ (uses)
Zeal OS / Hardware
```

### Runtime Initialization

**When program starts:**
1. **OS loads program** — Reads executable from storage
2. **OS allocates memory** — Sets up RAM for program
3. **Runtime initializes** — Sets up libraries, I/O, etc.
4. **Program starts** — Your `begin...end` block executes

**Initialization sequence:**
```
OS loads program
    ↓
OS allocates memory
    ↓
Runtime initializes:
  - Standard libraries
  - I/O systems
  - Memory management
  - Exception handling
    ↓
Your program begins execution
```

### Runtime Services

**Common runtime services:**
- **Input/Output** — `WriteLn`, `ReadLn`, file operations
- **Memory allocation** — `New`, `Dispose`, heap management
- **String operations** — String manipulation functions
- **Math functions** — `Sin`, `Cos`, `Sqrt`, etc.
- **System calls** — Access to OS services

**Example:**
```pascal
WriteLn('Hello');  // Runtime provides WriteLn
var p: ^integer;
New(p);            // Runtime provides memory allocation
p^ := 42;
Dispose(p);        // Runtime frees memory
```

---

## System Integration

### How Components Work Together

**Complete execution model:**
```
Your Program Code
    ↓
Runtime Libraries
    ↓
Zeal OS Services
    ↓
Hardware (CPU, Memory, I/O, Video, Audio)
```

**Example: Drawing a sprite:**
```pascal
// 1. Your program code
ZVB_SpriteSet(0, x, y, tile);

// 2. Runtime/Compiler generates code
//    (direct hardware access, no OS call)

// 3. CPU executes
//    Writes to ZVB register

// 4. ZVB hardware renders
//    Sprite appears on screen
```

### Execution Flow

**Typical program execution:**
```
1. Program starts
   ↓
2. Initialize graphics
   ↓
3. Game loop:
   a. Process input
   b. Update game state
   c. Render graphics
   d. Wait for VBlank
   e. Repeat
   ↓
4. Program ends
```

**Key components:**
- **Your code** — Game logic, rendering
- **Runtime** — Libraries, memory management
- **OS** — File system, device management
- **Hardware** — CPU, video, audio, I/O

---

## Summary

**Key Concepts:**
- **Execution cycle** — Fetch → Decode → Execute → Store
- **Zeal OS** — Manages hardware and provides services
- **Hardware access** — Direct (memory-mapped) or through OS
- **Runtime** — Provides libraries and system support
- **System integration** — All components work together

**What's Next:**
- Learn about **game loops** and **timing** concepts
- Understand how programs coordinate with video system
- Explore the relationship between CPU, video, and timing

**For Deeper Study:**
- Part V: Game Development covers game loop implementation
- Part IV: Computers in Depth covers execution model, memory layout, and ABI

---

**Next:** [Game Loop and Timing](./03_GameLoopAndTiming.md)

