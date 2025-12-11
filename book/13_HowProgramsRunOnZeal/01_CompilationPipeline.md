# Compilation Pipeline

**Part of:** [Chapter 13: How Programs Run on Zeal](./README.md)

---

> **For GCSE students:**  
> When you write a program, the computer can't understand it directly. A compiler translates your code into instructions the computer can understand. Think of it like translating a book from one language to another.
>
> **For A-Level students:**  
> The compilation process converts high-level Pascal source code into machine code that the Z80 CPU can execute. This involves lexical analysis, parsing, code generation, and linking. Understanding this process helps you write efficient code and debug compilation errors.
>
> **For University students:**  
> The SuperPascal compiler implements a multi-stage compilation pipeline: lexical analysis (tokenization), syntax analysis (parsing), semantic analysis (type checking), intermediate code generation, optimization, and target code generation (Z80 assembly). The assembler then converts assembly to object code, and the linker combines object files into an executable. *Detailed compiler architecture and optimization techniques are covered in Part IV: Computers in Depth.*

---

## Introduction

When you write a SuperPascal program, it goes through several stages before it can run on the Zeal computer. Understanding this **compilation pipeline** helps you understand how your code becomes a running program.

**What you'll learn:**
- The stages of compilation (Source → Assembly → Object → Executable)
- What happens at each stage
- How the compiler, assembler, and linker work together
- Why this process is necessary

---

## The Big Picture

### From Source Code to Running Program

**The complete journey:**
```
1. Source Code (Pascal)
   ↓
2. Compiler (translates to assembly)
   ↓
3. Assembly Code (Z80 instructions)
   ↓
4. Assembler (converts to object code)
   ↓
5. Object Files (machine code)
   ↓
6. Linker (combines into executable)
   ↓
7. Executable File (.bin)
   ↓
8. Zeal OS (loads and runs)
   ↓
9. Running Program
```

**Key insight:** Each stage transforms your code into a form closer to what the CPU can execute.

---

## Stage 1: Source Code

### What is Source Code?

**Source code is what you write:**
- **High-level language** — Pascal (human-readable)
- **Text file** — Stored as `.pas` file
- **Your program** — The code you write in ZealIDE

**Example:**
```pascal
program Hello;
begin
  WriteLn('Hello, World!');
end.
```

**Characteristics:**
- **Human-readable** — You can read and understand it
- **Platform-independent** — Same code can compile for different platforms
- **Abstract** — Doesn't specify exact CPU instructions

---

## Stage 2: Compiler

### What Does the Compiler Do?

**The compiler translates Pascal to assembly:**
- **Reads source code** — Parses your Pascal program
- **Checks syntax** — Ensures code is valid
- **Checks types** — Ensures types are correct
- **Generates assembly** — Creates Z80 assembly code

**Compiler stages:**
```
Source Code
    ↓
Lexical Analysis (tokenization)
    ↓
Syntax Analysis (parsing)
    ↓
Semantic Analysis (type checking)
    ↓
Code Generation (assembly)
    ↓
Assembly Code
```

### Lexical Analysis

**Tokenization — breaking code into tokens:**
```pascal
x := 5 + 3;
```

**Becomes tokens:**
```
IDENTIFIER(x) ASSIGN ADD NUMBER(5) ADD NUMBER(3) SEMICOLON
```

### Syntax Analysis

**Parsing — building syntax tree:**
```
      :=
     /  \
    x    +
        / \
       5   3
```

### Code Generation

**Generating assembly code:**
```pascal
x := 5 + 3;
```

**Becomes Z80 assembly:**
```assembly
LD A, 5      ; Load 5 into accumulator
ADD A, 3     ; Add 3
LD (x), A    ; Store result to x
```

---

## Stage 3: Assembly Code

### What is Assembly Code?

**Assembly is human-readable machine code:**
- **Z80 instructions** — Direct CPU instructions
- **Mnemonics** — Human-readable instruction names
- **Low-level** — Close to what CPU executes

**Example assembly:**
```assembly
LD A, 5      ; Load immediate value 5 into register A
ADD A, 3     ; Add immediate value 3 to register A
LD (x), A    ; Store register A to memory location x
```

**Characteristics:**
- **Platform-specific** — Z80 assembly only works on Z80
- **Low-level** — Direct CPU instructions
- **Efficient** — One assembly instruction = one CPU operation (usually)

---

## Stage 4: Assembler

### What Does the Assembler Do?

**The assembler converts assembly to object code:**
- **Reads assembly** — Parses assembly instructions
- **Converts to binary** — Creates machine code
- **Resolves addresses** — Calculates memory addresses
- **Creates object file** — Outputs `.o` file

**Assembler process:**
```
Assembly Code
    ↓
Instruction Encoding (binary)
    ↓
Address Resolution
    ↓
Object File (.o)
```

### Instruction Encoding

**Assembly → Binary:**
```assembly
LD A, 5
```

**Becomes binary:**
```
00111110 00000101
  ^^^^^^   ^^^^^^
   LD A    value 5
```

### Object File

**Object file contains:**
- **Machine code** — Binary instructions
- **Symbols** — Variable and function names
- **Relocations** — Addresses that need fixing
- **Metadata** — Debug info, etc.

---

## Stage 5: Linker

### What Does the Linker Do?

**The linker combines object files:**
- **Reads object files** — Multiple `.o` files
- **Resolves symbols** — Connects function calls to definitions
- **Fixes addresses** — Resolves all memory addresses
- **Creates executable** — Outputs `.bin` file

**Linking process:**
```
Object File 1 (main.o)
Object File 2 (utils.o)
Object File 3 (graphics.o)
    ↓
Symbol Resolution
Address Fixing
    ↓
Executable File (.bin)
```

### Symbol Resolution

**Connecting function calls:**
```pascal
// main.pas
procedure DoSomething;  // Defined here
begin
  // ...
end;

// utils.pas
procedure DoSomething; external;  // Used here
```

**Linker connects them:**
- Finds `DoSomething` definition in `main.o`
- Fixes call in `utils.o` to point to definition
- Creates single executable with all connections resolved

### Executable File

**Executable contains:**
- **Complete program** — All code combined
- **Resolved addresses** — All memory addresses fixed
- **Entry point** — Where program starts
- **Ready to run** — Can be loaded by Zeal OS

---

## Stage 6: Zeal OS

### What Does Zeal OS Do?

**Zeal OS loads and runs your program:**
- **Loads executable** — Reads `.bin` file from storage
- **Allocates memory** — Sets up program memory
- **Sets up environment** — Initializes runtime
- **Starts execution** — Jumps to program entry point

**OS responsibilities:**
- **Memory management** — Allocates RAM for program
- **File system** — Loads program from storage
- **Runtime setup** — Initializes libraries, I/O
- **Program execution** — Starts your program running

---

## Stage 7: Running Program

### Execution

**Once loaded, your program runs:**
- **CPU fetches instructions** — Gets instructions from memory
- **CPU executes** — Performs operations
- **Program runs** — Your code executes step by step
- **Interacts with hardware** — Uses video, audio, I/O

**Execution flow:**
```
Program Counter (PC) points to current instruction
    ↓
CPU fetches instruction from memory
    ↓
CPU decodes instruction
    ↓
CPU executes instruction
    ↓
PC moves to next instruction
    ↓
Repeat...
```

---

## Summary

**Key Concepts:**
- **Compilation pipeline** — Source → Assembly → Object → Executable
- **Compiler** — Translates Pascal to assembly
- **Assembler** — Converts assembly to object code
- **Linker** — Combines object files into executable
- **Zeal OS** — Loads and runs executable
- **Execution** — CPU runs your program

**What's Next:**
- Learn about the **execution model** — How programs run on Zeal
- Understand **runtime** and **system integration**
- Explore **game loop** and **timing** concepts

**For Deeper Study:**
- Part IV: Computers in Depth covers compiler architecture in detail
- Part IV includes code generation, optimization, and linking algorithms

---

**Next:** [Execution Model](./02_ExecutionModel.md)

