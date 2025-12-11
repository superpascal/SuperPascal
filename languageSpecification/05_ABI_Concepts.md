# SuperPascal Language Specification — ABI Concepts and Principles

## General ABI Concepts (Platform-Agnostic)

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

---

## Overview

This document defines **general ABI concepts and principles** that apply across all SuperPascal platforms. These concepts are platform-agnostic and describe the **what** and **why** of ABI design.

**Platform-specific implementations** (the **how**) are documented in:
- **[ZealZ80 ABI](../platforms/ZealZ80/ABI.md)** - Z80-specific calling conventions
- **[Platform Specifications](../platforms/README.md)** - All platform-specific ABI implementations

---

## 1. What is an ABI?

An **Application Binary Interface (ABI)** defines the low-level contract between:
- **Compiler-generated code** and **runtime libraries**
- **Object files** and **linkers**
- **Different compilation units** (modules, units)

The ABI ensures that code compiled separately can interoperate correctly.

### ABI Components

1. **Calling Conventions** - How functions are called and return
2. **Register Usage** - Which registers are preserved vs. scratch
3. **Stack Frame Layout** - How local variables and parameters are organized
4. **Parameter Passing** - How arguments are passed to functions
5. **Return Value Conventions** - How function results are returned
6. **Data Layout** - How records, classes, arrays are laid out in memory
7. **Name Mangling** - How symbols are encoded in object files
8. **Exception Handling** - How exceptions are represented and unwound
9. **Object File Format** - Binary format for compiled code

---

## 2. Parameter Passing Strategies

### 2.1 Value Parameters

**Concept:** Pass a copy of the value to the function.

**Characteristics:**
- Callee receives a copy, modifications don't affect caller
- Size determines passing strategy (register vs. stack)
- May require promotion (e.g., byte → word for alignment)

**Platform-specific:** Register choice, stack alignment, promotion rules

### 2.2 Var Parameters

**Concept:** Pass a reference (pointer) to the variable.

**Characteristics:**
- Callee receives address, modifications affect caller
- Always passed as pointer (size independent)
- Callee must dereference to access value

**Platform-specific:** Pointer size, addressing mode

### 2.3 Const Parameters

**Concept:** Pass value with read-only guarantee.

**Characteristics:**
- Semantically like value parameters
- Compiler enforces immutability
- May be optimized (passed by reference if large)

**Platform-specific:** Optimization strategies

### 2.4 Hidden Parameters

**Concept:** Parameters added by compiler, not visible in source.

**Common hidden parameters:**
- **Self pointer** - For method calls (object instance)
- **Return buffer** - For large return values (records, arrays)
- **Type information** - For generic/runtime type checks

**Platform-specific:** Parameter order, passing mechanism

---

## 3. Return Value Strategies

### 3.1 Scalar Returns

**Concept:** Small values returned in registers.

**Characteristics:**
- Fast (no memory access)
- Limited by register size
- Common for: integers, pointers, booleans, chars

**Platform-specific:** Which register(s), size limits

### 3.2 Large Returns

**Concept:** Large values returned via hidden buffer.

**Characteristics:**
- Caller allocates buffer
- Callee writes result to buffer
- Pointer passed as hidden parameter

**Platform-specific:** Size threshold, buffer allocation strategy

---

## 4. Data Layout Principles

### 4.1 Record Layout

**General principles:**
- Fields stored sequentially in memory
- Alignment may be required (platform-specific)
- Field offsets calculated from start of record
- Size = sum of field sizes (plus padding if aligned)

**Platform-specific:** Alignment rules, padding strategy, endianness

### 4.2 Class Layout

**General principles:**
- Instance data (like records)
- VTable pointer (for virtual methods)
- Inheritance: base class data first, then derived
- Virtual method dispatch via VTable

**Platform-specific:** VTable format, method pointer size, inheritance model

### 4.3 Array Layout

**General principles:**
- Elements stored contiguously
- Row-major order (for multi-dimensional)
- Bounds may be stored (for bounds checking)

**Platform-specific:** Element alignment, bounds storage strategy

---

## 5. Name Mangling Concepts

**Purpose:** Encode function/method signatures in symbol names for:
- Overload resolution
- Type safety
- Linker symbol matching

**General approach:**
- Function name + parameter types + return type
- Encoded in ASCII (or platform-specific encoding)
- Unique identifier for each overload

**Platform-specific:** Encoding format, character set, length limits

---

## 6. Exception Handling Model

### 6.1 Exception Frame Concept

**General principles:**
- Each function with exception handling has an exception frame
- Frames linked in a chain (stack order)
- Frame contains: handler address, cleanup code, previous frame pointer
- Unwinding traverses frame chain

**Platform-specific:** Frame structure, unwinding mechanism, register preservation

### 6.2 Stack Unwinding

**Concept:** When exception is raised, unwind stack to find handler.

**Process:**
1. Start at current frame
2. Execute cleanup code (finally blocks)
3. Move to previous frame
4. Repeat until handler found or stack exhausted

**Platform-specific:** Unwinding implementation, performance characteristics

---

## 7. ABI Stability and Versioning

### 7.1 Versioning Strategy

**General principles:**
- ABI version encoded in object files
- Breaking changes require new version
- Non-breaking changes maintain compatibility
- Version compatibility rules enforced by linker

**Platform-specific:** Version encoding format, compatibility rules

### 7.2 Breaking vs. Non-Breaking Changes

**Breaking changes** (require new ABI version):
- Register usage changes
- Stack frame layout changes
- Calling convention changes
- VTable layout changes
- Parameter passing order changes

**Non-breaking changes** (same ABI version):
- New optional features
- Extended symbol table formats
- Additional debug information
- New optional intrinsics

---

## 8. Compiler-Runtime-Linker Responsibilities

### 8.1 Compiler Responsibilities

**General:**
- Generate code conforming to ABI
- Emit correct frame prologues/epilogues
- Use correct parameter passing order
- Generate proper name mangling
- Emit relocation entries for external symbols

**Platform-specific:** Code generation details

### 8.2 Runtime Responsibilities

**General:**
- Provide library routines (arithmetic helpers, memory management)
- Manage heap allocation
- Handle exception unwinding
- Provide system call wrappers

**Platform-specific:** Runtime implementation details

### 8.3 Linker Responsibilities

**General:**
- Resolve external symbols
- Apply relocations
- Merge object files
- Generate final binary

**Platform-specific:** Object file format, relocation types

---

## 9. Cross-Platform ABI Considerations

### 9.1 Common Patterns

While ABIs are platform-specific, common patterns emerge:

- **Pascal-style calling convention** - Callee cleans stack (common in Pascal)
- **Frame pointer** - Dedicated register for stack frame access
- **Register preservation** - Some registers preserved across calls
- **Stack growth** - Typically downward (toward smaller addresses)

### 9.2 Platform Differences

Key differences between platforms:

- **Register count and size** - 8-bit vs. 16-bit vs. 32-bit registers
- **Address space** - 16-bit vs. 24-bit vs. 32-bit addressing
- **Stack alignment** - Word-aligned vs. byte-aligned
- **Parameter passing** - Register-based vs. stack-based
- **Return values** - Single register vs. multiple registers vs. memory

---

## 10. ABI Design Principles

### 10.1 Efficiency

- Minimize register spills
- Optimize common cases (small parameters, scalar returns)
- Minimize stack frame overhead

### 10.2 Compatibility

- Maintain backward compatibility when possible
- Version breaking changes clearly
- Document migration paths

### 10.3 Clarity

- Clear, unambiguous rules
- Well-documented edge cases
- Examples for common patterns

### 10.4 Debuggability

- Support stack traces
- Preserve frame information
- Enable exception unwinding

---

## See Also

- **[Platform-Specific ABI Implementations](../platforms/README.md)** - CPU-specific calling conventions
- **[ZealZ80 ABI](../platforms/ZealZ80/ABI.md)** - Complete Z80 ABI specification
- **[Language Specification Overview](./00_Overview.md)** - Complete specification index

---

**End of ABI Concepts Specification**

