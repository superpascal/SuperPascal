# Absolute Addressing

**Part of:** [06_IntrinsicsAndDirectives.md](../06_IntrinsicsAndDirectives.md)

---

SuperPascal supports **absolute addressing** for variables at fixed memory locations, similar to Turbo Pascal's `absolute` directive.

### 3C.1 Absolute Variable Declaration

**Syntax:**
```pascal
var
  VariableName: Type absolute Address;
```

**Purpose**: Declare variable at fixed memory address.

**Parameters:**
- `VariableName`: Variable identifier
- `Type`: Variable type (byte, word, integer, record, etc.)
- `Address`: Memory address (word constant or expression)

**Examples:**
```pascal
var
  VideoRAM: array[0..1023] of byte absolute $C000;
  StatusReg: byte absolute $8000;
  Counter: word absolute $8001;
  Config: record
    Flags: byte;
    Value: word;
  end absolute $8100;
```

### 3C.2 Absolute Addressing Rules

**Rules:**
- Address must be compile-time constant
- Address must be valid for variable size
- Multiple variables can share addresses (union-like behavior)
- No initialization allowed (memory already exists)
- Type must fit at address (no overflow)

**Memory Layout:**
```pascal
var
  LowByte: byte absolute $8000;
  HighByte: byte absolute $8001;
  WordValue: word absolute $8000;  // Shares address with LowByte/HighByte
```

**Usage:**
```pascal
WordValue := $1234;  // Writes to $8000 and $8001
// LowByte = $34, HighByte = $12 (little-endian)
```

### 3C.3 Absolute Addressing for Hardware Registers

**Common Pattern:**
```pascal
// ZVB Video Board registers
var
  ZVB_MODE: byte absolute $8000;
  ZVB_CTRL: byte absolute $8001;
  ZVB_PALETTE: array[0..255] of word absolute $9000;

// PIO registers
var
  PIO_DATA_A: byte absolute $D0;  // Note: I/O ports use absolute differently
  PIO_CTRL_A: byte absolute $D2;
```

**Note**: For I/O ports, use `PortIn`/`PortOut` intrinsics instead of absolute addressing, as I/O ports are accessed via IN/OUT instructions, not memory mapping.

### 3C.4 Absolute Addressing Limitations

**Not Allowed:**
- Absolute addressing for I/O ports (use `PortIn`/`PortOut`)
- Absolute addressing for stack variables
- Absolute addressing for parameters
- Dynamic addresses (must be compile-time constant)

**Best Practices:**
- Use for memory-mapped hardware registers
- Use for fixed memory locations (BIOS data, etc.)
- Document hardware addresses used
- Group related registers in records

---

**See also:**
- [Memory Management](./03_MemoryIntrinsics.md)
- [Direct Memory Access](./03A_DirectMemoryAccess.md)
- [I/O Port Access](./03B_IOPortAccess.md)

