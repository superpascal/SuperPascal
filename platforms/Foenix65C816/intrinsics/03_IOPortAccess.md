# Foenix65C816 Platform — I/O Port Access Intrinsics

**Platform:** Foenix65C816 (Foenix F256x systems)  
**Part of:** [Foenix65C816 Platform Intrinsics](./README.md)

---

SuperPascal provides intrinsics for direct I/O port access. Foenix systems primarily use memory-mapped I/O, but some hardware may use dedicated I/O ports.

**Note**: Foenix systems use memory-mapped I/O for most hardware. These port access intrinsics are provided for compatibility and for any hardware that uses dedicated I/O ports.

---

## 1. I/O Port Read Operations

### PortIn

**Syntax:**
```pascal
function PortIn(Port: word): byte;
```

**Purpose**: Read a byte from an I/O port.

**Parameters:**
- `Port`: I/O port address

**Returns**: Byte value from the port

**Codegen**: 65C816 I/O port read instruction (if dedicated I/O ports are used) or memory-mapped I/O read.

**Usage:**
```pascal
var value: byte;
value := PortIn($D600);  // Read from audio port
```

### PortInW

**Syntax:**
```pascal
function PortInW(Port: word): word;
```

**Purpose**: Read a word (16-bit) from an I/O port.

**Parameters:**
- `Port`: I/O port address

**Returns**: Word value from the port (little-endian)

**Codegen**: 65C816 I/O port read instruction in 16-bit mode.

**Usage:**
```pascal
var value: word;
value := PortInW($D600);  // Read 16-bit from port
```

---

## 2. I/O Port Write Operations

### PortOut

**Syntax:**
```pascal
procedure PortOut(Port: word; Value: byte);
```

**Purpose**: Write a byte to an I/O port.

**Parameters:**
- `Port`: I/O port address
- `Value`: Byte value to write

**Codegen**: 65C816 I/O port write instruction (if dedicated I/O ports are used) or memory-mapped I/O write.

**Usage:**
```pascal
PortOut($D600, $80);  // Write to audio port
```

### PortOutW

**Syntax:**
```pascal
procedure PortOutW(Port: word; Value: word);
```

**Purpose**: Write a word (16-bit) to an I/O port.

**Parameters:**
- `Port`: I/O port address
- `Value`: Word value to write (little-endian)

**Codegen**: 65C816 I/O port write instruction in 16-bit mode.

**Usage:**
```pascal
PortOutW($D600, $8080);  // Write 16-bit to port
```

---

## 3. Memory-Mapped I/O vs. Port I/O

Foenix systems primarily use **memory-mapped I/O**, where hardware registers appear as memory addresses. For memory-mapped I/O, use `Peek`/`Poke` instead of `PortIn`/`PortOut`.

### Memory-Mapped I/O (Preferred)

```pascal
// VICKY registers are memory-mapped
var ctrl: byte;
ctrl := Peek($D000);  // Read VICKY master control
Poke($D000, $01);     // Write to VICKY master control
```

### Port I/O (If Available)

```pascal
// Some hardware may use dedicated I/O ports
var value: byte;
value := PortIn($D600);  // Read from port
PortOut($D600, $80);     // Write to port
```

---

## 4. Platform-Specific Notes

### Foenix I/O Architecture

- **Memory-Mapped I/O**: Primary I/O method (IO Page 0: $C000-$DFFF)
- **Port I/O**: Limited use, primarily for compatibility
- **IO Page Registers**: Hardware registers accessible via IO Page architecture

### Common I/O Addresses

```pascal
const
  // VICKY Graphics (memory-mapped)
  VICKY_BASE = $D000;
  
  // Audio (memory-mapped, example)
  AUDIO_PORT = $D600;
  
  // Interrupt Controller (memory-mapped)
  INT_PENDING_REG0 = $D660;
  INT_PENDING_REG1 = $D661;
```

---

## 5. Usage Examples

### Example: Audio Port Access

```pascal
program AudioPortAccess;
var
  audioValue: byte;
begin
  // Read from audio port
  audioValue := PortIn($D600);
  
  // Write to audio port
  PortOut($D600, $80);
end.
```

### Example: Memory-Mapped I/O (Preferred)

```pascal
program MemoryMappedIO;
begin
  // Use Peek/Poke for memory-mapped I/O
  // This is the preferred method for Foenix systems
  Poke($D000, $01);  // VICKY master control
end.
```

---

## 6. Safety Considerations

1. **Port Validity**: Ensure port addresses are valid for the target hardware.

2. **Hardware Effects**: Writing to I/O ports can have immediate hardware effects. Read hardware documentation before modifying ports.

3. **Timing**: Some I/O operations may have timing requirements.

4. **Interrupts**: I/O operations may trigger interrupts. Ensure proper interrupt handling.

---

**Reference:** [SuperPascal Memory and I/O Specification](../../../languageSpecification/12_MemoryAndIO.md)  
**Last Updated:** 2025-01-XX  
**Platform:** Foenix65C816 (WDC W65C816S @ 6.29 MHz)

