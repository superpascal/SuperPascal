# What Is a Computer?

**Part of:** [Chapter 01: Introduction to Programming on Zeal](./README.md)

---

> **For GCSE students:**  
> A computer is like a very fast calculator that can remember things. It has a brain (CPU) that does calculations, memory to store information, and ways to show you results (screen) and get your input (keyboard).
>
> **For A-Level students:**  
> A computer is a machine that processes data according to stored instructions. It consists of a CPU (processor), memory (RAM), storage, and I/O devices. Understanding these components helps you write efficient code.
>
> **For University students:**  
> A computer is a von Neumann architecture machine with a CPU, memory hierarchy, I/O subsystem, and storage. Understanding the architecture—including instruction execution, memory addressing, and platform-specific characteristics—is essential for systems programming and optimization. *Note: This chapter provides a conceptual introduction. Rigorous technical details are covered in Part IV: Computers in Depth.*

---

## Introduction

Before we can write programs, we need to understand what a computer is and how it works. This chapter introduces computers at a **conceptual level**—we'll build intuition first, then explore the technical details later.

**What you'll learn:**
- What computers do (input → processing → output)
- How the CPU works (simplified)
- What the ALU does (the calculator inside)
- How memory stores values
- Why Zeal uses an 8-bit CPU
- What programming really means

---

## What Does a Computer Do?

A computer is a machine that:

1. **Takes input** — Receives information (keyboard, mouse, sensors)
2. **Processes it** — Does calculations and makes decisions
3. **Produces output** — Shows results (screen, sound, lights)

### The Input → Processing → Output Cycle

**Simple example:**
```
You type: "5 + 3"
    ↓
Computer processes: Adds 5 and 3
    ↓
Computer shows: "8"
```

**Another example:**
```
You press: Arrow key
    ↓
Computer processes: Moves character on screen
    ↓
Computer shows: Character in new position
```

### Why Computers Need Instructions

Computers don't "know" what to do—they need **instructions** (programs) that tell them:
- What input to expect
- How to process it
- What output to produce

**This is what programming is:** Writing instructions for computers.

---

## CPU: The Computer's Brain

The **CPU (Central Processing Unit)** is the "brain" of the computer. It's a very fast decision-maker that follows instructions step by step.

### What the CPU Does

The CPU:
- **Reads instructions** from memory
- **Understands what to do** (decodes the instruction)
- **Does the work** (executes the instruction)
- **Repeats** for the next instruction

**Simple analogy:**
Think of the CPU like a chef following a recipe:
1. **Read** the next step
2. **Understand** what to do
3. **Do it** (chop, mix, cook)
4. **Repeat** for the next step

### The Fetch → Decode → Execute Cycle

Every instruction follows this cycle:

```
1. FETCH    → CPU gets the instruction from memory
2. DECODE   → CPU understands what to do
3. EXECUTE  → CPU does the work
4. REPEAT   → Move to next instruction
```

**Example:**
```
Instruction: "Add 5 and 3"

1. FETCH:    CPU reads "Add 5 and 3" from memory
2. DECODE:   CPU understands: "I need to add two numbers"
3. EXECUTE:  CPU calculates: 5 + 3 = 8
4. REPEAT:   CPU moves to next instruction
```

> **Note:** This is a simplified explanation. The technical details of how CPUs actually work—including instruction registers, control units, and machine code—are covered in **Part IV: Computers in Depth** for advanced students.

### Programs as Sequences of Steps

A **program** is a list of instructions:
```
Step 1: Get number from user
Step 2: Multiply by 2
Step 3: Show result
Step 4: Stop
```

The CPU follows these steps **one at a time**, very quickly (millions of times per second).

---

## The ALU: Arithmetic & Logic Unit

Inside the CPU is the **ALU (Arithmetic & Logic Unit)**. Think of it as "the calculator inside the computer."

### What the ALU Does

The ALU performs:
- **Arithmetic:** Add, subtract, multiply, divide
- **Logic:** Compare numbers (is 5 greater than 3?)
- **Bit operations:** Work with individual bits (we'll learn about bits later)

**Simple examples:**
```
ALU calculates: 5 + 3 = 8
ALU compares: 5 > 3? → Yes (true)
ALU calculates: 10 - 4 = 6
```

### The ALU in Action

When you write:
```pascal
result := 5 + 3;
```

**What happens:**
1. CPU sends 5 and 3 to the ALU
2. ALU adds them: 5 + 3 = 8
3. CPU gets the result (8) from the ALU
4. CPU stores 8 in memory (as `result`)

> **Note:** The technical details of how the ALU actually works—including addition circuits, subtraction as addition, and overflow detection—are covered in **Part IV: Computers in Depth** for advanced students.

---

## Memory: Where Values Live

**Memory (RAM)** is where the computer stores values while it's running. Think of memory as "many numbered boxes."

### Memory as Numbered Boxes

Imagine memory like a huge wall of mailboxes:
```
Box 0:   [empty]
Box 1:   [empty]
Box 2:   [empty]
Box 3:   [empty]
...
Box 100: [empty]
```

Each box has a **number** (address) and can hold a **value**.

### Variables Map to Memory Locations

When you write:
```pascal
var x: integer;
x := 42;
```

**What happens:**
1. Computer finds an empty memory box (e.g., Box 100)
2. Computer stores the value 42 in that box
3. Computer remembers: "x" means "Box 100"
4. When you use `x`, computer looks in Box 100

**Visual:**
```
Variable: x
    ↓
Memory Box 100: [42]
```

### Why Memory Matters

- **Variables** are stored in memory
- **Arrays** are stored in memory (many boxes in a row)
- **Program code** is stored in memory
- **Everything** the computer needs while running is in memory

> **Note:** The technical details of memory addressing, bytes, words, endianness, and how arrays map to memory are covered in **Part IV: Computers in Depth** for advanced students.

---

## Why Zeal Uses an 8-bit CPU

The Zeal computer uses a **Z80 CPU**, which is an **8-bit processor**. This might seem old-fashioned, but it's perfect for learning.

### What "8-bit" Means

**8-bit** means the CPU processes **8 bits at a time**. (We'll learn about bits in detail later.)

**Simple explanation:**
- 8-bit CPU: Works with small chunks of data
- 16-bit CPU: Works with larger chunks
- 32-bit CPU: Works with even larger chunks

### Why 8-bit is Good for Learning

**Simplicity:**
- Easier to understand
- Less complexity to hide
- You can see what's happening

**Predictability:**
- Behavior is straightforward
- No hidden optimizations
- What you write is what happens

**Educational value:**
- Learn fundamentals first
- Understand how computers really work
- Build strong foundation

### Why Modern Computers Hide Complexity

Modern computers (like your phone or laptop) are **very complex**:
- Multiple CPU cores
- Complex memory systems
- Many layers of abstraction
- Optimizations everywhere

**This complexity:**
- Makes them fast and powerful
- But hides how they actually work
- Makes learning harder

**Zeal's 8-bit CPU:**
- Shows you the fundamentals
- No hidden complexity
- Perfect for learning

> **Note:** We'll explore different CPU architectures (8-bit, 16-bit, 32-bit) and their differences in detail in **Part IV: Computers in Depth** for advanced students.

---

## What Programming Really Is

**Programming** is telling the computer **step-by-step** what to do.

### The Programming Process

**Step 1: You write code**
```pascal
program Hello;
begin
  WriteLn('Hello, World!');
end.
```

**Step 2: Compiler translates**
The compiler reads your code and converts it into instructions the CPU can understand.

**Step 3: CPU executes**
The CPU follows the instructions and runs your program.

### Humans Write → Compiler Translates → CPU Executes

**Visual flow:**
```
You write SuperPascal code
        ↓
Compiler translates to machine code
        ↓
CPU executes machine code
        ↓
Program runs!
```

### Why We Need a Compiler

**Humans write in SuperPascal:**
- Easy to read and understand
- Uses words like `begin`, `end`, `if`, `then`
- Organized and structured

**CPUs understand machine code:**
- Just numbers (0s and 1s)
- Very hard for humans to read
- Direct instructions for the CPU

**The compiler bridges the gap:**
- Reads SuperPascal code
- Converts to machine code
- CPU can execute it

> **Note:** The technical details of how compilers work—including lexers, parsers, code generation, and how SuperPascal code becomes Z80 assembly—are covered in **Part IV: Computers in Depth** and **Part V: Compiler Internals** for advanced students.

---

## Summary

**Key concepts:**
- **Computers** do: Input → Processing → Output
- **CPU** is the brain: Fetch → Decode → Execute
- **ALU** is the calculator inside the CPU
- **Memory** stores values in numbered boxes
- **8-bit CPU** is simple and perfect for learning
- **Programming** is writing step-by-step instructions

**What's next:**
- We'll write our first program
- We'll learn about variables and types
- We'll explore how expressions work
- Later, we'll dive deep into how computers really work

**Remember:**
- This chapter gave you **conceptual understanding**
- Technical details come later in **Part IV**
- Build intuition first, rigor later

---

**Next Section:** [Writing Your First Program](./02_ProgramsAndAlgorithms.md)  
**Language Specification:** See [Platform Specifications](../../platforms/README.md)  
**Last Updated:** 2025-01-XX

