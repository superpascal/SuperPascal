# I/O Port Access Intrinsics

**Part of: [ZealZ80 Platform Intrinsics](./README.md)

---

SuperPascal provides direct I/O port access for hardware communication, similar to Turbo Pascal's `Port` and `PortW` functions.

### 3B.1 PortIn (Read from I/O Port)

**Syntax:**
```pascal
function PortIn(Port: byte): byte;
```

**Purpose**: Read a byte from I/O port (Z80 IN instruction).

**Parameters:**
- `Port`: 8-bit I/O port address

**Returns**: Byte value read from port

**Codegen**: Z80 `IN A, (port)` instruction sequence.

**Usage:**
```pascal
var status: byte;
status := PortIn($D1);  // Read from PIO port B data register
```

**Hardware Reference:**
- PIO Port A Data: `$D0`
- PIO Port A Control: `$D2`
- PIO Port B Data: `$D1`
- PIO Port B Control: `$D3`

### 3B.2 PortOut (Write to I/O Port)

**Syntax:**
```pascal
procedure PortOut(Port: byte; Value: byte);
```

**Purpose**: Write a byte to I/O port (Z80 OUT instruction).

**Parameters:**
- `Port`: 8-bit I/O port address
- `Value`: Byte value to write

**Codegen**: Z80 `OUT (port), A` instruction sequence.

**Usage:**
```pascal
PortOut($D1, $FF);  // Write $FF to PIO port B data register
```

### 3B.3 PortInW (Read Word from I/O Port)

**Syntax:**
```pascal
function PortInW(Port: byte): word;
```

**Purpose**: Read a 16-bit word from I/O port (two consecutive reads).

**Parameters:**
- `Port`: 8-bit I/O port address (reads from Port and Port+1)

**Returns**: Word value (little-endian)

**Usage:**
```pascal
var value: word;
value := PortInW($D0);  // Read word from ports $D0 and $D1
```

### 3B.4 PortOutW (Write Word to I/O Port)

**Syntax:**
```pascal
procedure PortOutW(Port: byte; Value: word);
```

**Purpose**: Write a 16-bit word to I/O port (two consecutive writes).

**Parameters:**
- `Port`: 8-bit I/O port address (writes to Port and Port+1)
- `Value`: Word value to write (little-endian)

**Usage:**
```pascal
PortOutW($D0, $1234);  // Write word to ports $D0 and $D1
```

### 3B.5 I/O Port Constants

**Standard I/O Ports:**
```pascal
const
  IO_PIO_DATA_A = $D0;
  IO_PIO_CTRL_A = $D2;
  IO_PIO_DATA_B = $D1;
  IO_PIO_CTRL_B = $D3;
  IO_UART_DATA  = $E0;  // UART data register (example)
  IO_UART_STATUS = $E1; // UART status register (example)
```

### 3B.6 I/O Port Access Safety

**Warnings:**
- Writing to wrong ports may damage hardware
- Reading from invalid ports returns undefined values
- I/O port timing may be critical for some devices
- Interrupts may be affected by port access

**Best Practices:**
- Use constants for port addresses
- Document port usage
- Follow hardware timing requirements
- Use proper initialization sequences

---

**See also:**
- [Memory Management](./03_MemoryIntrinsics.md)
- [Direct Memory Access](./03A_DirectMemoryAccess.md)
- [Absolute Addressing](./03C_AbsoluteAddressing.md)

