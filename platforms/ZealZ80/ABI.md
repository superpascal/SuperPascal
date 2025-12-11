# SuperPascal — ZealZ80 Platform ABI Implementation

## Platform-Specific ABI Specification

**Version:** 1.0  
**Platform:** ZealZ80 (Zeal 8-bit Computer, Z80 @ 10 MHz)  
**Part of:** SuperPascal Platform-Specific Specifications

**See also:** [General ABI Concepts](../../languageSpecification/05_ABI_Concepts.md) for platform-agnostic ABI principles.

---

**Note:** This document specifies the **platform-specific implementation** of the ABI for ZealZ80. For general ABI concepts and principles that apply across all platforms, see [05_ABI_Concepts.md](../../languageSpecification/05_ABI_Concepts.md).

---

## 1. Target Architecture

- **CPU**: Zilog Z80 @ 10 MHz
- **Endianness**: Little-endian
- **Address Space**: 16-bit logical (banked via MMU)
- **Calling Convention**: Pascal-style (callee cleans stack)
- **Stack**: Grows downward (toward smaller addresses)

---

## 2. Register Usage

### 2.1 Register Conventions

| Register | Purpose | Preserved? |
|----------|---------|-----------|
| `AF` | Condition codes, accumulator | No (scratch) |
| `BC` | Temporary, block operations | No (scratch) |
| `DE` | Temporary, address math | No (scratch) |
| `HL` | Primary data pointer, return value | No (scratch) |
| `IX` | Frame pointer | Yes (callee-saved) |
| `IY` | Runtime system (exception frames) | Yes (runtime) |
| `SP` | Stack pointer | Yes (managed) |

### 2.2 Scratch Registers

`AF`, `BC`, `DE`, `HL` may be clobbered by function calls. Callers must save if needed.

---

## 3. Stack Frame Layout

### 3.1 Frame Structure

```
High addresses
+-------------------+
| Caller locals     |
+-------------------+
| Parameter N       |  ← ix + (N*2 + 6)
+-------------------+
| ...               |
+-------------------+
| Parameter 1       |  ← ix + 8
+-------------------+
| Self (if method)  |  ← ix + 6
+-------------------+
| Return buffer     |  ← ix + 4 (if large return)
+-------------------+
| Return address    |  ← ix + 2
+-------------------+
| Old IX            |  ← ix + 0
+-------------------+
| Local variables   |
+-------------------+
| ...               |
+-------------------+
| Temporary space   |  ← sp (current)
+-------------------+
Low addresses
```

### 3.2 Function Prologue

```asm
; On entry: return address on stack
push ix              ; Save old frame pointer
ld ix, 0
add ix, sp          ; IX = current SP
ld hl, -LocalSize   ; Calculate new SP
add hl, sp
ld sp, hl            ; Allocate locals
```

### 3.3 Function Epilogue

```asm
ld sp, ix            ; Restore SP
pop ix               ; Restore old IX
ret                  ; Return
```

**Note**: Callee removes parameters from stack (Pascal convention).

---

## 4. Parameter Passing

### 4.1 Value Parameters

- Passed **right-to-left** on stack
- Scalars (integer, byte, word, boolean, char) use **2 bytes** (word-aligned)
- Byte/char promoted to word for stack alignment

**Example:**
```pascal
procedure Foo(a: integer; b: byte; c: word);
```

Stack layout (high to low):
```
+4: c (word, 2 bytes)
+6: b (word, 2 bytes, byte promoted)
+8: a (word, 2 bytes)
```

### 4.2 Var Parameters

- Passed as **2-byte addresses** (pointers)
- Callee dereferences to access value
- Modifications affect caller's variable

### 4.3 Const Parameters

- Passed same as value parameters (may be optimized)
- Callee cannot modify (compiler enforces)

### 4.4 Hidden Parameters

**Self pointer** (for methods):
- First hidden parameter (before user parameters)
- 2-byte pointer to instance
- Offset: `ix + 6`

**Return buffer** (for large returns):
- First hidden parameter (before Self if both present)
- 2-byte pointer to caller-allocated buffer
- Offset: `ix + 4`

---

## 5. Return Values

### 5.1 Scalar Returns

Returned in **`HL`** register:

- `integer` (16-bit signed)
- `word` (16-bit unsigned)
- `byte` (8-bit, zero-extended to 16-bit in `L`)
- `boolean` (0 or 1 in `L`)
- `char` (8-bit in `L`)
- Pointers (16-bit address)
- Class references (16-bit address)

### 5.2 Large Returns

Returned via **hidden return buffer**:

- Records (any size)
- Arrays (any size)
- Sets (any size)

**Caller responsibility:**
1. Allocate buffer for return value
2. Pass buffer address as hidden first parameter
3. Callee writes result to buffer

**Callee responsibility:**
1. Write result to `ReturnPtr^`
2. Return (no value in `HL`)

---

## 6. Record Layout

### 6.1 Memory Layout

Records stored with fields in **declared order**:

```pascal
type TVec2 = record
  X, Y: integer;
end;
```

Layout:
```
Offset 0: X (2 bytes)
Offset 2: Y (2 bytes)
Total:   4 bytes
```

### 6.2 Field Access

Field offset = sum of previous field sizes:

```pascal
type TPlayer = record
  Name: string[20];    // Offset 0, size 21
  Score: integer;      // Offset 21, size 2
  Position: TVec2;    // Offset 23, size 4
end;
```

### 6.3 Record Methods

Record methods compile to free functions with `var` parameter:

```pascal
procedure TVec2.Add(const B: TVec2);
```

Becomes:
```pascal
procedure TVec2_Add(var self: TVec2; const B: TVec2);
```

`self` passed as first parameter (hidden `var` parameter).

---

## 7. Class Layout

### 7.1 Instance Layout

```
Offset 0: VTable pointer (2 bytes)
Offset 2: Field1
Offset N: FieldN
```

### 7.2 VTable Layout

```
Offset 0: Parent VTable pointer (2 bytes) or 0
Offset 2: RTTI pointer (2 bytes) or 0
Offset 4: Method slot 0 (2 bytes)
Offset 6: Method slot 1 (2 bytes)
...
```

### 7.3 Virtual Method Dispatch

**Code sequence:**
```asm
; obj.VirtualMethod
ld hl, (obj)         ; Load object pointer
ld de, (hl)          ; Load vtable pointer
ld hl, (de+SlotIndex); Load method address
push args...
jp (hl)              ; Jump to method
```

**Slot assignment:**
- Virtual methods assigned slots in declaration order
- Overridden methods replace parent slots
- Slot index determined at compile time

---

## 8. Name Mangling

### 8.1 Global Symbols

Format: `UnitName_SymbolName`

**Examples:**
- `Math_Sin`
- `Player_Init`
- `System_Halt`

### 8.2 Methods

**Record methods:**
- Format: `TypeName_MethodName`
- Example: `TVec2_Add`

**Class methods:**
- Format: `ClassName_MethodName`
- Example: `TEntity_Update`

**Virtual methods:**
- Format: `ClassName_MethodName_V<slot>`
- Example: `TEntity_Update_V0`

### 8.3 Overloaded Functions

Future: May use type suffixes:
- `_I` for integer
- `_B` for byte
- `_S` for string

---

## 9. Object File Format

### 9.1 .ZOF (Zeal Object File)

**Sections:**
- **CODE**: Machine code (relocatable)
- **DATA**: Initialized data
- **BSS**: Zero-initialized data
- **SYMBOLS**: Symbol table
- **RELOC**: Relocation entries
- **VTABLES**: VTable definitions

### 9.2 .ZOU (Zeal Unit File)

**Contents:**
- Exported symbol table
- Type metadata
- VTable definitions
- Unit initialization address
- Dependency list

---

## 10. Exception Frames

### 10.1 Exception Frame Structure

```
Offset 0: Previous frame pointer (2 bytes)
Offset 2: Handler address (2 bytes)
Offset 4: Saved SP (2 bytes)
Offset 5: Saved IX (2 bytes)
Offset 6: Flags (1 byte)
Offset 7: Reserved (1 byte)
Total:   8 bytes
```

### 10.2 Exception Frame Chain

Frames linked via `IY` register:

```
IY → Current frame
     ↓
     Previous frame
     ↓
     Older frame
     ↓
     nil
```

### 10.3 Unwinding

When exception raised:

1. Read current frame from `IY`
2. Restore `SP` from frame
3. Restore `IX` from frame
4. Jump to handler address
5. Continue unwinding if handler re-raises

---

## 11. Code Generation Guidelines

### 11.1 General Principles

- Use `HL` as primary accumulator
- Use `IX` for frame pointer only (no arithmetic)
- Minimize stack operations
- Prefer short instruction sequences
- Use library routines for complex operations (MUL, DIV)

### 11.2 Arithmetic

**16-bit addition:**
```asm
add hl, de    ; HL = HL + DE
```

**16-bit subtraction:**
```asm
or a          ; Clear carry
sbc hl, de    ; HL = HL - DE
```

**Multiplication/Division:**
- Use runtime library routines
- Pass operands on stack
- Result in `HL`

### 11.3 Branching

**Conditional jumps:**
```asm
cp value      ; Compare A with value
jr z, label   ; Jump if zero
jr nz, label  ; Jump if not zero
```

**Boolean short-circuit:**
- Evaluate left operand first
- Skip right if result determined

---

## 12. ABI Stability

### 12.1 Versioning

ABI version encoded in object files:
- **v1.0**: Initial ABI (this specification)
- Future versions must maintain backward compatibility or use new file format

### 12.2 Compatibility Rules

**Breaking changes:**
- Register usage changes
- Stack frame layout changes
- Calling convention changes
- VTable layout changes

**Non-breaking changes:**
- New optional features
- Extended symbol table formats
- Additional debug information

---

## 13. Implementation Notes

### 13.1 Compiler Responsibilities

- Generate code conforming to this ABI
- Emit correct frame prologues/epilogues
- Use correct parameter passing order
- Generate proper name mangling
- Emit relocation entries for external symbols

### 13.2 Runtime Responsibilities

- Provide library routines (MUL, DIV, exception handling)
- Manage heap allocation
- Handle exception unwinding
- Provide system call wrappers

### 13.3 Linker Responsibilities

- Resolve external symbols
- Apply relocations
- Merge object files
- Generate final binary

---

**End of ABI Specification**

