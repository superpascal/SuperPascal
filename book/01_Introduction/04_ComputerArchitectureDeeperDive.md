# Computer Architecture Deeper Dive

**Part of:** [Chapter 01: Introduction to Programming on Zeal](./README.md)

---

## Introduction

Understanding how computers work at a deeper level helps you write more efficient code and understand why certain programming patterns are important. This section explores computer architecture concepts that directly impact your programming.

**Target Levels:**
- **GCSE:** How code executes (simple, visual explanations)
- **A-Level:** Memory hierarchy, CPU basics, instruction execution
- **University:** Deep dive into architecture, performance implications

---

## How Code Executes

### The Execution Cycle

**Programs execute in cycles:**

1. **Fetch** — CPU gets instruction from memory
2. **Decode** — CPU understands what to do
3. **Execute** — CPU performs the operation
4. **Store** — CPU saves results
5. **Repeat** — Next instruction

**Visual Example:**
```
Memory:  [LOAD 5] [ADD 3] [STORE result]
           ↓        ↓        ↓
CPU:    Fetch → Decode → Execute → Store
```

### Instruction Cycle Differences Across Architectures

**Different CPUs execute instructions differently:**

**Z80 (8-bit):**
- Multi-cycle instructions (many instructions take 4-20 cycles)
- Simple pipeline (fetch, decode, execute)
- Some instructions can take many cycles (e.g., block operations)

**65C816 (16-bit):**
- More efficient instruction execution
- Some pipelined micro-operations
- Generally faster per instruction than Z80
- More orthogonal instruction set

**Key Insight:**
- All CPUs follow fetch → decode → execute
- Implementation details differ
- Understanding these differences helps optimize code

> **For University students:**  
> The 65C816's instruction set is more orthogonal than the Z80's, meaning similar operations use similar instruction formats. This makes the 65C816 easier to program in assembly and allows for more efficient code generation by compilers.

### Instruction Execution

**Simple instruction:**
```pascal
x := 5 + 3;
```

**What happens:**
1. CPU loads value 5 into register
2. CPU loads value 3 into register
3. CPU adds registers (5 + 3 = 8)
4. CPU stores result 8 to memory location x

**Each step takes time:**
- Register operations: Very fast (1 cycle)
- Memory access: Slower (multiple cycles)
- Calculations: Fast (1-2 cycles)

---

## Memory Hierarchy

### Memory Levels

**Memory is organized in levels:**

```
CPU Registers (fastest, smallest)
    ↓
L1 Cache (very fast, small)
    ↓
L2 Cache (fast, medium)
    ↓
RAM (slower, large)
    ↓
Storage (slowest, largest)
```

### Why Hierarchy Matters

**Speed differences:**
- **Register access:** 1 cycle (fastest)
- **Cache access:** 1-3 cycles
- **RAM access:** 10-100 cycles
- **Storage access:** 1000-10000 cycles

**Implications:**
- Keep frequently used data in registers/cache
- Minimize memory accesses
- Access patterns matter (sequential vs random)

### Cache Behavior

**Cache helps by:**
- Storing recently used data
- Predicting what you'll need next
- Reducing memory access time

**Cache-friendly code:**
```pascal
// ✅ GOOD: Sequential access
for i := 0 to 99 do
  arr[i] := arr[i] * 2;

// ❌ BAD: Random access (cache misses)
for i := 0 to 99 do
  arr[randomIndex[i]] := arr[randomIndex[i]] * 2;
```

---

## CPU Architecture Basics

### Registers

**Registers are:**
- Small, fast storage in CPU
- Used for calculations
- Limited in number (8-16 typical)

**Common registers:**
- **Accumulator** — Main calculation register
- **Index registers** — Array indexing
- **Stack pointer** — Function call stack
- **Program counter** — Current instruction

**Example:**
```pascal
x := 5 + 3;
// CPU uses accumulator register for calculation
```

### Instruction Pipeline

**Modern CPUs use pipelines:**
- Execute multiple instructions simultaneously
- While one instruction executes, next is decoded
- Improves performance

**Pipeline stages:**
```
Instruction 1: [Fetch] [Decode] [Execute] [Store]
Instruction 2:        [Fetch] [Decode] [Execute] [Store]
Instruction 3:               [Fetch] [Decode] [Execute] [Store]
```

### Branch Prediction

**CPU predicts branches:**
- If/else statements create branches
- CPU guesses which path to take
- Wrong guess = pipeline flush (slow)

**Implications:**
- Predictable branches are faster
- Unpredictable branches are slower
- Minimize branches in hot loops

---

## Memory Access Patterns

### Sequential Access

**Sequential access is fast:**
- Cache-friendly
- Predictable
- Efficient

**Example:**
```pascal
// ✅ GOOD: Sequential
for i := 0 to 99 do
  sum := sum + arr[i];
```

### Random Access

**Random access is slower:**
- Cache misses
- Unpredictable
- Less efficient

**Example:**
```pascal
// ⚠️ SLOWER: Random
for i := 0 to 99 do
  sum := sum + arr[randomIndex[i]];
```

### Locality of Reference

**Two types of locality:**

**Temporal locality:**
- Recently used data likely to be used again
- Cache exploits this

**Spatial locality:**
- Nearby data likely to be used
- Cache loads blocks of data

**Writing cache-friendly code:**
- Access data sequentially when possible
- Reuse data in loops
- Keep related data together

---

## Performance Implications

### Memory Access Costs

**Memory access is expensive:**
- Register: 1 cycle
- Cache: 1-3 cycles
- RAM: 10-100 cycles
- Storage: 1000-10000 cycles

**Minimize memory accesses:**
```pascal
// ❌ BAD: Multiple memory accesses
for i := 0 to 99 do
  arr[i] := arr[i] + 1;

// ✅ BETTER: Cache-friendly
for i := 0 to 99 do
  arr[i] := arr[i] + 1;  // Sequential access
```

### Branch Costs

**Branches can be expensive:**
- Predictable: Fast
- Unpredictable: Slow (pipeline flush)

**Minimize branches:**
```pascal
// ❌ BAD: Branch in hot loop
for i := 0 to 9999 do
  if arr[i] > 0 then
    sum := sum + arr[i];

// ✅ BETTER: Reduce branches
for i := 0 to 9999 do
  if arr[i] > 0 then
    sum := sum + arr[i];  // Still has branch, but consider alternatives
```

### Loop Optimization

**Loops are critical:**
- Most time spent in loops
- Optimize loop bodies
- Minimize work per iteration

**Example:**
```pascal
// ❌ BAD: Function call in loop
for i := 0 to 99 do
  arr[i] := CalculateValue(i);

// ✅ BETTER: Inline calculation
for i := 0 to 99 do
  arr[i] := i * 2 + 1;  // Direct calculation
```

---

## Memory Addressing Across Architectures

### Memory on Z80

**Z80 memory model:**
- **16-bit address bus** — Can address 64KB directly ($0000-$FFFF)
- **MMU banking** — Maps larger memory (512KB) into 64KB windows
- **Linear addressing** — Within a bank, addresses are linear
- **Bank switching** — Must switch banks to access different memory regions

**Addressing example:**
```
Bank 0: $0000-$FFFF (64KB window)
Bank 1: $0000-$FFFF (different 64KB window)
Switch banks to access different regions
```

### Memory on 65C816

**65C816 memory model:**
- **24-bit addressing** — Can address 16MB ($000000-$FFFFFF)
- **Bank + Offset** — Address = Bank (8 bits) + Offset (16 bits)
- **Direct Page (DP)** — Fast access to 64KB "zero page" region
- **Program Bank / Data Bank** — Separate banks for code and data

**24-bit addressing breakdown:**
```
Address: $01:2345
         │  │
         │  └─ Offset (16 bits, $0000-$FFFF)
         └──── Bank (8 bits, $00-$FF)

Total: 256 banks × 64KB = 16MB
```

**Direct Page (DP) register:**
- Points to a 64KB region
- Fast access using DP-relative addressing
- Can be relocated (not fixed at $0000)
- Similar to Z80's zero page but relocatable

**Stack addressing:**
- Stack pointer (S) is 16-bit in emulation mode
- Stack pointer is 16-bit in native mode (but can use bank)
- Stack-relative addressing available
- More flexible than Z80's fixed stack location

**Bank boundaries:**
- Each bank is 64KB
- Crossing bank boundaries requires bank register updates
- Important for large programs and data structures

### Comparison: Z80 vs 65C816 Memory

| Feature | Z80 | 65C816 |
|---------|-----|--------|
| **Address Space** | 16-bit (64KB) | 24-bit (16MB) |
| **Banking** | MMU banking (512KB total) | Native 24-bit addressing |
| **Zero Page** | Fixed at $0000-$00FF | Direct Page (relocatable) |
| **Stack** | Fixed location | Relocatable, stack-relative addressing |
| **Code/Data Separation** | Manual (programmer manages) | Program Bank / Data Bank registers |
| **Large Programs** | Requires bank switching | Direct access to 16MB |

**Key differences:**
- **Z80:** Simpler, but limited to 64KB per bank
- **65C816:** More complex, but can directly address 16MB
- **Z80:** Bank switching overhead
- **65C816:** No switching needed, but must manage bank registers

---

## Platform-Specific Considerations

### ZealZ80 (8-bit)

**Characteristics:**
- 8-bit CPU, limited registers
- 64KB address space (with MMU banking)
- Memory access is expensive
- Cache: None (direct memory access)

**Optimization strategies:**
- Minimize memory accesses
- Use registers efficiently
- Consider MMU banking overhead
- Keep frequently used data in zero page ($0000-$00FF)

### Foenix65C816 (16-bit)

**Characteristics:**
- 16-bit CPU, more registers
- 16MB address space (24-bit addressing)
- Direct Page for fast access
- Better memory access patterns
- Cache: Limited (if any)

**Optimization strategies:**
- Use Direct Page for frequently accessed data
- Leverage 16MB address space (no bank switching)
- Better register utilization (16-bit operations)
- Consider cache behavior
- Use Program Bank / Data Bank separation

### FoenixA2560M (32-bit)

**Characteristics:**
- 32-bit CPU, many registers
- Large address space
- Modern memory architecture
- Cache: Likely present

**Optimization strategies:**
- Cache-friendly code patterns
- Register optimization
- Pipeline considerations

---

## Practical Guidelines

### 1. Understand Your Platform

**Know your target:**
- CPU architecture
- Memory constraints
- Cache behavior
- Performance characteristics

### 2. Write Cache-Friendly Code

**Sequential access:**
- Prefer sequential over random
- Access related data together
- Reuse data in loops

### 3. Minimize Memory Accesses

**Reduce memory operations:**
- Use local variables (registers)
- Cache frequently used values
- Batch operations

### 4. Optimize Hot Paths

**Focus on:**
- Loops that run many times
- Frequently called functions
- Critical performance paths

### 5. Measure, Don't Guess

**Profile your code:**
- Measure actual performance
- Identify bottlenecks
- Optimize what matters

---

## Exercises

### Exercise 1: Memory Access

Compare sequential vs random access:
1. Create array of 1000 integers
2. Sum sequentially
3. Sum randomly
4. Measure time difference

### Exercise 2: Cache Behavior

Test cache effects:
1. Access small array repeatedly
2. Access large array repeatedly
3. Compare performance

### Exercise 3: Loop Optimization

Optimize a loop:
1. Write simple loop
2. Identify optimizations
3. Measure improvements

---

**Previous Section:** [Running Your First Program](./03_RunningYourFirstProgram.md)  
**Next Chapter:** [Chapter 02: Computational Thinking](../02_ComputationalThinking/README.md)  
**Last Updated:** 2025-01-XX

