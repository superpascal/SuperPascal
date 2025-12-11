# Foenix65C816 Platform — Absolute Addressing

**Platform:** Foenix65C816 (Foenix F256x systems)  
**Part of:** [Foenix65C816 Platform Intrinsics](./README.md)

---

SuperPascal supports absolute addressing, allowing variables to be placed at specific memory addresses. This is useful for hardware register access, zero-page variables, and memory-mapped data structures.

---

## 1. Absolute Directive

### Syntax

```pascal
var
  VariableName: Type absolute Address;
```

**Purpose**: Place a variable at a specific memory address.

**Parameters:**
- `VariableName`: Name of the variable
- `Type`: Variable type (byte, word, dword, record, array, etc.)
- `Address`: Memory address (24-bit address space: 0x000000-0xFFFFFF)

---

## 2. Usage Examples

### Hardware Register Access

```pascal
program HardwareRegisters;
var
  // VICKY master control register
  VICKY_MasterCtrl: byte absolute $D000;
  
  // VICKY border control
  VICKY_BorderCtrl: byte absolute $D004;
  VICKY_BorderColorB: byte absolute $D005;
  VICKY_BorderColorG: byte absolute $D006;
  VICKY_BorderColorR: byte absolute $D007;
  
begin
  // Direct access to hardware registers
  VICKY_MasterCtrl := $01;  // Enable text mode
  VICKY_BorderColorR := 255; // Red border
end.
```

### Zero-Page Variables

```pascal
program ZeroPageVars;
var
  // Zero-page variables for fast access
  TempByte: byte absolute $00;
  TempWord: word absolute $02;
  
begin
  TempByte := 42;
  TempWord := $1234;
end.
```

### Memory-Mapped Structures

```pascal
program MemoryMappedStruct;
type
  TCursorRegs = record
    Ctrl: byte;
    StartAddr: byte;
    Char: byte;
    Color: byte;
    X: word;  // 16-bit at $D014-$D015
    Y: word;  // 16-bit at $D016-$D017
  end;
  
var
  Cursor: TCursorRegs absolute $D010;
  
begin
  // Access cursor registers as a structure
  Cursor.Ctrl := $01;  // Enable cursor
  Cursor.X := 10;
  Cursor.Y := 5;
  Cursor.Char := 95;  // Underscore cursor
end.
```

### Interrupt Controller Registers

```pascal
program InterruptController;
var
  // Interrupt pending registers
  IntPending0: byte absolute $D660;
  IntPending1: byte absolute $D661;
  
  // Interrupt mask registers
  IntMask0: byte absolute $D66C;
  IntMask1: byte absolute $D66D;
  
begin
  // Check pending interrupts
  if (IntPending0 and $01) <> 0 then
    WriteLn('Start of Frame interrupt');
    
  // Enable interrupt
  IntMask0 := IntMask0 or $01;
end.
```

---

## 3. Address Space Considerations

### Valid Address Ranges

- **0x000000-0x00FFFF**: Zero page and direct page (fast access)
- **0x010000-0xBFFFFF**: Extended memory (may require MMU mapping)
- **0xC000-0xDFFF**: IO Page 0 (hardware registers)
- **0xE000-0xFFFF**: Kernel space (reserved)

### MMU Mapping

For addresses outside the direct address space, ensure proper MMU mapping:

```pascal
// Addresses in extended memory may require MMU setup
var
  ExtendedVar: byte absolute $100000;  // May require MMU mapping
```

---

## 4. Type Compatibility

### Supported Types

- **Primitive Types**: `byte`, `word`, `dword`, `integer`, `cardinal`, etc.
- **Records**: Structures can be placed at absolute addresses
- **Arrays**: Arrays can be placed at absolute addresses
- **Pointers**: Pointer types can be placed at absolute addresses

### Alignment

- **Byte**: No alignment required
- **Word**: Should be word-aligned (even address) for optimal performance
- **Dword**: Should be dword-aligned (address divisible by 4) for optimal performance
- **Records**: Alignment depends on largest member

---

## 5. Safety Considerations

1. **Address Validity**: Ensure addresses are within valid memory ranges.

2. **Hardware Registers**: Writing to hardware registers can have immediate effects. Read hardware documentation before modifying.

3. **Kernel Space**: Avoid placing variables in kernel space ($E000-$FFFF).

4. **MMU Mapping**: Extended memory addresses may require MMU setup.

5. **Volatile Memory**: Hardware registers are volatile. Values may change between accesses.

---

## 6. Platform-Specific Notes

### Foenix Memory Model

- **24-bit Addressing**: Full 16MB address space available
- **IO Page**: Hardware registers in IO Page 0 ($C000-$DFFF)
- **Zero Page**: Fast access variables in zero page ($0000-$00FF)
- **MMU**: Four concurrent memory maps for extended memory access

### Common Absolute Addresses

```pascal
const
  // VICKY Graphics Registers
  VICKY_MASTER_CTRL = $D000;
  VICKY_BORDER_CTRL = $D004;
  VICKY_CURSOR_CTRL = $D010;
  
  // Interrupt Controller
  INT_PENDING_REG0 = $D660;
  INT_PENDING_REG1 = $D661;
  INT_MASK_REG0 = $D66C;
  INT_MASK_REG1 = $D66D;
```

---

## 7. Advanced Usage

### Overlapping Structures

```pascal
program OverlappingStructs;
type
  TLineInterruptWrite = record
    Ctrl: byte;      // $D018 (write)
    CompareLo: byte;  // $D019 (write)
    CompareHi: byte;  // $D01A (write)
  end;
  
  TRasterRead = record
    PixelXLo: byte;   // $D018 (read)
    PixelXHi: byte;   // $D019 (read)
    LineYLo: byte;    // $D01A (read)
    LineYHi: byte;    // $D01B (read)
  end;
  
var
  LineInt: TLineInterruptWrite absolute $D018;
  Raster: TRasterRead absolute $D018;  // Same address, different interpretation
  
begin
  // Write: Set line interrupt
  LineInt.CompareLo := 240;
  LineInt.CompareHi := 0;
  LineInt.Ctrl := $01;
  
  // Read: Get raster position
  // Note: Reading and writing use different register meanings
end.
```

---

**Reference:** [SuperPascal Memory and I/O Specification](../../../languageSpecification/12_MemoryAndIO.md)  
**Last Updated:** 2025-01-XX  
**Platform:** Foenix65C816 (WDC W65C816S @ 6.29 MHz)

