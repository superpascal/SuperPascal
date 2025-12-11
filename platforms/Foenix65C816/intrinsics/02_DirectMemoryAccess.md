# Foenix65C816 Platform — Direct Memory Access Intrinsics

**Platform:** Foenix65C816 (Foenix F256x systems)  
**Part of:** [Foenix65C816 Platform Intrinsics](./README.md)

---

SuperPascal provides intrinsics for direct memory read/write operations, similar to MS-BASIC's PEEK and POKE functions. These intrinsics provide low-level memory access for hardware control and system programming.

---

## 1. Memory Read Operations

### Peek

**Syntax:**
```pascal
function Peek(Address: dword): byte;
```

**Purpose**: Read a byte from memory.

**Parameters:**
- `Address`: Memory address (24-bit address space: 0x000000-0xFFFFFF)

**Returns**: Byte value at the specified address

**Codegen**: Direct memory read instruction (65C816 `LDA` with appropriate addressing mode).

**Usage:**
```pascal
var value: byte;
value := Peek($D000);  // Read from VICKY register
```

### PeekW

**Syntax:**
```pascal
function PeekW(Address: dword): word;
```

**Purpose**: Read a word (16-bit) from memory.

**Parameters:**
- `Address`: Memory address (24-bit address space)

**Returns**: Word value at the specified address (little-endian)

**Codegen**: Direct memory read instruction (65C816 `LDA` in 16-bit mode).

**Usage:**
```pascal
var value: word;
value := PeekW($D014);  // Read 16-bit cursor X position
```

**Note**: Address should be word-aligned for optimal performance.

---

## 2. Memory Write Operations

### Poke

**Syntax:**
```pascal
procedure Poke(Address: dword; Value: byte);
```

**Purpose**: Write a byte to memory.

**Parameters:**
- `Address`: Memory address (24-bit address space)
- `Value`: Byte value to write

**Codegen**: Direct memory write instruction (65C816 `STA` with appropriate addressing mode).

**Usage:**
```pascal
Poke($D000, $01);  // Write to VICKY master control register
```

### PokeW

**Syntax:**
```pascal
procedure PokeW(Address: dword; Value: word);
```

**Purpose**: Write a word (16-bit) to memory.

**Parameters:**
- `Address`: Memory address (24-bit address space)
- `Value`: Word value to write (little-endian)

**Codegen**: Direct memory write instruction (65C816 `STA` in 16-bit mode).

**Usage:**
```pascal
PokeW($D014, 100);  // Write 16-bit cursor X position
```

**Note**: Address should be word-aligned for optimal performance.

---

## 3. Memory Access Patterns

### Reading Hardware Registers

```pascal
// Read VICKY master control register
var ctrl: byte;
ctrl := Peek($D000);

// Read cursor position (16-bit)
var cursorX: word;
cursorX := PeekW($D014);
```

### Writing Hardware Registers

```pascal
// Write to VICKY master control
Poke($D000, $01);  // Enable text mode

// Write cursor position
PokeW($D014, 100);  // Set cursor X to 100
```

### Memory-Mapped I/O

Foenix systems use memory-mapped I/O. The IO window is typically $C000-$DFFF, with VICKY registers starting at $D000.

```pascal
// Access IO Page 0 registers directly
var status: byte;
status := Peek($D000);  // VICKY master control
```

---

## 4. Safety Considerations

1. **Address Validity**: Ensure addresses are within valid memory ranges (0x000000-0xFFFFFF for 24-bit addressing).

2. **Hardware Registers**: Writing to hardware registers can have immediate effects. Read hardware documentation before modifying registers.

3. **Alignment**: For optimal performance, word operations should use word-aligned addresses.

4. **Volatile Memory**: Hardware registers are volatile. Values may change between reads.

5. **MMU Mapping**: In native mode, ensure proper MMU mapping for addresses outside the direct address space.

---

## 5. Platform-Specific Notes

### Address Space

- **24-bit Addressing**: Foenix65C816 supports 24-bit addressing (16MB address space)
- **IO Page**: Hardware registers are in IO Page 0 ($C000-$DFFF)
- **Memory Maps**: Four concurrent memory maps available (kernel uses map 0, user programs use map 3)

### Memory Model

- **Direct Access**: Addresses 0x0000-0xBFFF are directly accessible
- **MMU Banking**: Addresses can be banked using MMU for extended memory access
- **Expansion Slot**: Optional 256KB expansion slot at $100000-$13FFFF

---

## 6. Usage Examples

### Example: Read/Write VICKY Registers

```pascal
program VICKYDirectAccess;
var
  ctrl: byte;
begin
  // Read current master control
  ctrl := Peek($D000);
  
  // Enable text mode (set bit 0)
  ctrl := ctrl or $01;
  Poke($D000, ctrl);
  
  // Set border color
  Poke($D005, 0);   // Blue = 0
  Poke($D006, 0);   // Green = 0
  Poke($D007, 255); // Red = 255 (red border)
end.
```

### Example: Read Cursor Position

```pascal
program ReadCursor;
var
  cursorX, cursorY: word;
begin
  // Read 16-bit cursor position
  cursorX := PeekW($D014);  // Cursor X (low/high bytes)
  cursorY := PeekW($D016);  // Cursor Y (low/high bytes)
  
  WriteLn('Cursor at: ', cursorX, ', ', cursorY);
end.
```

### Example: Memory Copy

```pascal
program MemoryCopy;
var
  i: word;
  src, dst: dword;
begin
  src := $1000;
  dst := $2000;
  
  // Copy 256 bytes
  for i := 0 to 255 do
    Poke(dst + i, Peek(src + i));
end.
```

---

**Reference:** [SuperPascal Memory and I/O Specification](../../../languageSpecification/12_MemoryAndIO.md)  
**Last Updated:** 2025-01-XX  
**Platform:** Foenix65C816 (WDC W65C816S @ 6.29 MHz)

