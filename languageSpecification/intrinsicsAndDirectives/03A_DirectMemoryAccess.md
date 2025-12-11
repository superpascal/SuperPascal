# Direct Memory Access Intrinsics

**Part of:** [06_IntrinsicsAndDirectives.md](../06_IntrinsicsAndDirectives.md)

---

SuperPascal provides direct memory access functions similar to BASIC's PEEK and POKE, allowing low-level access to RAM and memory-mapped hardware.

### 3A.1 Peek (Read Byte from Memory)

**Syntax:**
```pascal
function Peek(Addr: word): byte;
```

**Purpose**: Read a byte from memory address (PEEK equivalent).

**Parameters:**
- `Addr`: 16-bit memory address (logical address, MMU-mapped)

**Returns**: Byte value at address

**Codegen**: Direct memory read instruction sequence.

**Usage:**
```pascal
var value: byte;
value := Peek($8000);  // Read byte from address $8000
```

**Note**: Address is logical (MMU-mapped), not physical. Physical RAM starts at 0x80000.

### 3A.2 PeekW (Read Word from Memory)

**Syntax:**
```pascal
function PeekW(Addr: word): word;
```

**Purpose**: Read a 16-bit word from memory address.

**Parameters:**
- `Addr`: 16-bit memory address (must be word-aligned)

**Returns**: Word value at address (little-endian)

**Usage:**
```pascal
var value: word;
value := PeekW($8000);  // Read word from address $8000
```

### 3A.3 PeekL (Read Long from Memory)

**Syntax:**
```pascal
function PeekL(Addr: word): dword;
```

**Purpose**: Read a 32-bit value from memory (for future use).

**Parameters:**
- `Addr`: 16-bit memory address (must be aligned)

**Returns**: 32-bit value (little-endian)

**Status**: Future feature, not in v1.0.

### 3A.4 Poke (Write Byte to Memory)

**Syntax:**
```pascal
procedure Poke(Addr: word; Value: byte);
```

**Purpose**: Write a byte to memory address (POKE equivalent).

**Parameters:**
- `Addr`: 16-bit memory address
- `Value`: Byte value to write

**Codegen**: Direct memory write instruction sequence.

**Usage:**
```pascal
Poke($8000, $FF);  // Write $FF to address $8000
```

### 3A.5 PokeW (Write Word to Memory)

**Syntax:**
```pascal
procedure PokeW(Addr: word; Value: word);
```

**Purpose**: Write a 16-bit word to memory address.

**Parameters:**
- `Addr`: 16-bit memory address (must be word-aligned)
- `Value`: Word value to write (little-endian)

**Usage:**
```pascal
PokeW($8000, $1234);  // Write word $1234 to address $8000
```

### 3A.6 PokeL (Write Long to Memory)

**Syntax:**
```pascal
procedure PokeL(Addr: word; Value: dword);
```

**Purpose**: Write a 32-bit value to memory (for future use).

**Status**: Future feature, not in v1.0.

### 3A.7 Memory Access Safety

**Warnings:**
- Direct memory access bypasses type safety
- Writing to invalid addresses causes undefined behavior
- Accessing MMU-unmapped addresses may cause hardware faults
- Stack and code areas should not be modified directly

**Best Practices:**
- Use for hardware register access
- Use for memory-mapped I/O
- Avoid for general programming (use typed variables)
- Document memory addresses used

---

**See also:**
- [Memory Management](./03_MemoryIntrinsics.md)
- [I/O Port Access](./03B_IOPortAccess.md)
- [Absolute Addressing](./03C_AbsoluteAddressing.md)

