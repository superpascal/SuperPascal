# SuperPascal Language Specification — Memory and I/O Access

## Direct Memory and I/O Port Access

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

---

## 1. Overview

SuperPascal provides direct access to memory and I/O ports, similar to BASIC's PEEK and POKE functions. This enables:

- Low-level hardware control
- Memory-mapped I/O access
- Direct RAM manipulation
- Hardware register access
- System programming

Based on the [Zeal 8-bit Computer hardware](https://zeal8bit.com/docs/ram/), RAM is directly accessible at physical addresses 0x80000-0xFFFFF, and I/O ports are accessible via the Z80 PIO chip.

---

## 2. Historical Context

### 2.1 BASIC's PEEK and POKE

Classic BASIC provided:
- `PEEK(address)` - Read byte from memory
- `POKE address, value` - Write byte to memory

### 2.2 Turbo Pascal's Approach

Turbo Pascal provided:
- `Mem[segment:offset]` - Read byte from memory
- `MemW[segment:offset]` - Read word from memory
- `Port[port]` - Read/write I/O port
- `PortW[port]` - Read/write word I/O port
- `absolute` directive - Variables at fixed addresses

### 2.3 SuperPascal's Design

SuperPascal combines both approaches:
- **Intrinsics**: `Peek`, `PeekW`, `Poke`, `PokeW` (function/procedure style)
- **I/O Ports**: `PortIn`, `PortOut`, `PortInW`, `PortOutW`
- **Absolute addressing**: `absolute` clause for fixed addresses

---

## 3. Memory Access Functions

### 3.1 Peek (Read Byte)

**Syntax:**
```pascal
function Peek(Addr: word): byte;
```

**Purpose**: Read a byte from memory address (PEEK equivalent).

**Parameters:**
- `Addr`: 16-bit logical memory address (MMU-mapped)

**Returns**: Byte value at address

**Code Generation:**
```asm
ld hl, (Addr)    ; Load address
ld a, (hl)       ; Read byte
ld l, a          ; Return in L
```

**Usage:**
```pascal
var value: byte;
value := Peek($8000);  // Read byte from address $8000
```

**Memory Mapping:**
- Addresses are **logical** (MMU-mapped), not physical
- Physical RAM starts at 0x80000 (512 KB offset)
- MMU maps physical pages to 16-bit logical addresses
- Accessing unmapped addresses causes undefined behavior

### 3.2 PeekW (Read Word)

**Syntax:**
```pascal
function PeekW(Addr: word): word;
```

**Purpose**: Read a 16-bit word from memory.

**Parameters:**
- `Addr`: 16-bit memory address (should be word-aligned)

**Returns**: Word value (little-endian)

**Code Generation:**
```asm
ld hl, (Addr)    ; Load address
ld a, (hl)        ; Read low byte
inc hl
ld h, (hl)        ; Read high byte
ld l, a           ; Return in HL
```

**Usage:**
```pascal
var value: word;
value := PeekW($8000);  // Read word from $8000-$8001
```

**Alignment:**
- Word reads are more efficient when address is even
- Unaligned reads work but may be slower
- Little-endian byte order (Z80 native)

### 3.3 Poke (Write Byte)

**Syntax:**
```pascal
procedure Poke(Addr: word; Value: byte);
```

**Purpose**: Write a byte to memory address (POKE equivalent).

**Parameters:**
- `Addr`: 16-bit logical memory address
- `Value`: Byte value to write

**Code Generation:**
```asm
ld hl, (Addr)    ; Load address
ld a, (Value)     ; Load value
ld (hl), a        ; Write byte
```

**Usage:**
```pascal
Poke($8000, $FF);  // Write $FF to address $8000
```

### 3.4 PokeW (Write Word)

**Syntax:**
```pascal
procedure PokeW(Addr: word; Value: word);
```

**Purpose**: Write a 16-bit word to memory.

**Parameters:**
- `Addr`: 16-bit memory address (should be word-aligned)
- `Value`: Word value to write (little-endian)

**Code Generation:**
```asm
ld hl, (Addr)    ; Load address
ld a, (Value)     ; Load low byte
ld (hl), a        ; Write low byte
inc hl
ld a, (Value+1)   ; Load high byte
ld (hl), a        ; Write high byte
```

**Usage:**
```pascal
PokeW($8000, $1234);  // Write $34 to $8000, $12 to $8001
```

---

## 4. I/O Port Access

Based on the [Zeal PIO documentation](https://zeal8bit.com/docs/pio/#overview), I/O ports are accessed via the Z80 PIO chip using IN/OUT instructions.

### 4.1 PortIn (Read I/O Port)

**Syntax:**
```pascal
function PortIn(Port: byte): byte;
```

**Purpose**: Read a byte from I/O port (Z80 IN instruction).

**Parameters:**
- `Port`: 8-bit I/O port address

**Returns**: Byte value read from port

**Code Generation:**
```asm
ld a, (Port)      ; Load port number
ld c, a           ; Port in C
in a, (c)         ; Read from port
ld l, a           ; Return in L
```

**Usage:**
```pascal
var status: byte;
status := PortIn($D1);  // Read from PIO port B data register
```

### 4.2 PortOut (Write I/O Port)

**Syntax:**
```pascal
procedure PortOut(Port: byte; Value: byte);
```

**Purpose**: Write a byte to I/O port (Z80 OUT instruction).

**Parameters:**
- `Port`: 8-bit I/O port address
- `Value`: Byte value to write

**Code Generation:**
```asm
ld a, (Port)      ; Load port number
ld c, a           ; Port in C
ld a, (Value)      ; Load value
out (c), a        ; Write to port
```

**Usage:**
```pascal
PortOut($D1, $FF);  // Write $FF to PIO port B data register
```

### 4.3 PortInW (Read Word from I/O Port)

**Syntax:**
```pascal
function PortInW(Port: byte): word;
```

**Purpose**: Read a 16-bit word from consecutive I/O ports.

**Parameters:**
- `Port`: 8-bit I/O port address (reads from Port and Port+1)

**Returns**: Word value (little-endian)

**Usage:**
```pascal
var value: word;
value := PortInW($D0);  // Read from ports $D0 (low) and $D1 (high)
```

### 4.4 PortOutW (Write Word to I/O Port)

**Syntax:**
```pascal
procedure PortOutW(Port: byte; Value: word);
```

**Purpose**: Write a 16-bit word to consecutive I/O ports.

**Parameters:**
- `Port`: 8-bit I/O port address (writes to Port and Port+1)
- `Value`: Word value to write (little-endian)

**Usage:**
```pascal
PortOutW($D0, $1234);  // Write $34 to $D0, $12 to $D1
```

---

## 5. Standard I/O Port Addresses

Based on Zeal hardware documentation:

### 5.1 Z80 PIO Ports

```pascal
const
  IO_PIO_DATA_A = $D0;  // PIO Port A Data Register
  IO_PIO_CTRL_A = $D2;  // PIO Port A Control Register
  IO_PIO_DATA_B = $D1;  // PIO Port B Data Register (System Port)
  IO_PIO_CTRL_B = $D3;  // PIO Port B Control Register
```

### 5.2 PIO System Port (Port B) Bit Assignments

```pascal
const
  PIO_BIT_KEYBOARD = 7;   // Keyboard data line
  PIO_BIT_VBLANK  = 6;    // VBlank signal
  PIO_BIT_HBLANK  = 5;    // HBlank signal
  PIO_BIT_UART_RX = 3;    // UART receive
  PIO_BIT_I2C_SDA_IN = 2; // I2C SDA input
  PIO_BIT_I2C_SCL   = 1;  // I2C SCL
  PIO_BIT_I2C_SDA_OUT = 0; // I2C SDA output
```

### 5.3 Example: Reading PIO Status

```pascal
var pioStatus: byte;
pioStatus := PortIn(IO_PIO_DATA_B);

if (pioStatus and (1 shl PIO_BIT_VBLANK)) = 0 then
  // VBlank active (active low)
  
if (pioStatus and (1 shl PIO_BIT_KEYBOARD)) = 0 then
  // Keyboard data available
```

---

## 6. Absolute Addressing

### 6.1 Syntax

**Format:**
```pascal
var
  VariableName: Type absolute Address;
```

**Purpose**: Declare variable at fixed memory address.

### 6.2 Examples

```pascal
var
  // Memory-mapped hardware
  VideoRAM: array[0..1023] of byte absolute $C000;
  StatusReg: byte absolute $8000;
  Counter: word absolute $8001;
  
  // Hardware registers (if memory-mapped)
  ZVB_MODE: byte absolute $8000;
  ZVB_CTRL: byte absolute $8001;
```

### 6.3 Rules

**Requirements:**
- Address must be compile-time constant
- Address expression must evaluate to word value
- Variable type must fit at address

**Limitations:**
- Cannot use for I/O ports (use `PortIn`/`PortOut`)
- Cannot use for stack variables
- Cannot use for parameters
- No initialization (memory already exists)

**Union-like Behavior:**
```pascal
var
  LowByte: byte absolute $8000;
  HighByte: byte absolute $8001;
  WordValue: word absolute $8000;  // Shares address
  
// Writing to WordValue affects both bytes
WordValue := $1234;
// LowByte = $34, HighByte = $12
```

### 6.4 Memory Layout Considerations

**Physical RAM Layout:**
- Physical RAM: 0x80000 to 0xFFFFF (512 KB)
- Logical addresses: 0x0000 to 0xFFFF (16-bit, MMU-mapped)
- MMU maps physical pages to logical addresses

**Absolute Addresses:**
- Absolute addresses are **logical** (MMU-mapped)
- Compiler/runtime must ensure proper MMU mapping
- Accessing unmapped addresses causes undefined behavior

---

## 7. Memory-Mapped Hardware Access

### 7.1 ZVB Video Board Registers

If ZVB registers are memory-mapped (depends on hardware design):

```pascal
var
  ZVB_MODE: byte absolute $8000;
  ZVB_CTRL: byte absolute $8001;
  ZVB_PALETTE: array[0..255] of word absolute $9000;
```

**Usage:**
```pascal
ZVB_MODE := ZVB_MODE_TILE;
ZVB_CTRL := ZVB_CTRL_ENABLE;
ZVB_PALETTE[0] := $1F00;  // Color 0 = red
```

### 7.2 Alternative: Register Access Intrinsics

If registers are I/O-mapped, use intrinsics:

```pascal
WriteReg($8000, ZVB_MODE_TILE);  // Write to register
var mode := ReadReg($8000);       // Read from register
```

---

## 8. Safety and Best Practices

### 8.1 Educational Philosophy: "Razorblades + Teaching"

SuperPascal provides powerful low-level tools (the "razorblades") but expects students to learn safe usage (not "clenching their fist"). The language design philosophy is:

- **Give students the tools**: Direct memory access, pointer arithmetic, I/O ports
- **Teach safe usage**: Bounds checking in debug mode, clear error messages
- **Learn by doing**: Students will make mistakes and learn to debug them
- **No automatic seatbelts**: Beyond bounds checking, no automatic safety features

This approach teaches:
- How memory actually works
- Consequences of unsafe operations
- Debugging skills
- Systems programming understanding
- When to use unsafe operations vs safe alternatives

### 8.2 Bounds Checking

**Bounds checking is the primary safety mechanism:**

```pascal
{$RANGE_CHECK ON}   // Default in debug mode
{$RANGE_CHECK OFF}  // Can disable for performance in release mode
```

**What bounds checking protects:**
- Array index bounds: `array[i]` where `i` is out of range
- Subrange bounds: Assigning value outside subrange type
- String bounds: String index out of range

**Bounds checking behavior:**
- **Debug mode**: Always ON by default (cannot be disabled)
- **Release mode**: Can be disabled with `{$RANGE_CHECK OFF}` for performance
- **Error messages**: Clear, educational messages showing bounds and actual index
  - Example: `Array index 100 out of bounds [0..9]`

**When to disable bounds checking:**
- Performance-critical code (release builds)
- After thorough testing in debug mode
- When you've verified bounds manually

**Educational value:**
- Teaches students about buffer overflows
- Shows importance of bounds validation
- Demonstrates performance vs safety trade-offs

### 8.3 Memory Access Safety

**Direct Memory Access (Peek/Poke):**
- **No automatic bounds checking**: `Peek`/`Poke` can access any address
- **Undefined behavior**: Accessing invalid addresses causes crashes or corruption
- **Stack corruption**: Writing to stack area can corrupt return addresses
- **Code corruption**: Writing to code area can corrupt program execution

**Pointer Arithmetic:**
- **No automatic bounds checking**: Pointer arithmetic can access invalid memory
- **Type-aware scaling**: Arithmetic is scaled by type size (automatic)
- **Undefined behavior**: Dereferencing invalid pointers causes crashes
- **Common mistakes**: Off-by-one errors, accessing freed memory, null pointer dereference

**Safe Practices:**
- Use bounds checking in debug mode during development
- Validate addresses before using `Peek`/`Poke`
- Use array indexing instead of pointer arithmetic when possible
- Document memory addresses and pointer usage
- Test thoroughly in debug mode before disabling checks

**Common Mistakes Students Will Learn From:**
```pascal
// Buffer overflow
var arr: array[0..9] of integer;
var i: integer;
for i := 0 to 10 do  // Off-by-one: should be 9
  arr[i] := 0;       // Crashes when i=10 (bounds check catches this)

// Invalid pointer arithmetic
var p: ^integer;
p := @arr[0];
p := p + 20;         // Points beyond array end
p^ := 42;            // Undefined behavior (no bounds check on pointers)

// Invalid memory access
Poke($0000, $FF);    // Writing to address 0 (likely invalid)
```

### 8.4 I/O Port Safety

**Warnings:**
- Writing to wrong ports may damage hardware
- Reading from invalid ports returns undefined values
- Port timing may be critical
- Interrupts may be affected

**Safe Practices:**
- Use constants for port addresses
- Follow hardware initialization sequences
- Respect hardware timing requirements
- Document port usage
- Test on hardware (not just emulator)

### 8.5 Debug Mode vs Release Mode

**Debug Mode (`{$DEBUG}`):**
- Bounds checking: **Always ON** (cannot disable)
- Overflow checking: ON by default
- Clear error messages with context
- Symbol information for debugging
- Slower execution (safety checks)

**Release Mode (`{$RELEASE}`):**
- Bounds checking: **Can be disabled** with `{$RANGE_CHECK OFF}`
- Overflow checking: Can be disabled with `{$OVERFLOW_CHECK OFF}`
- Optimized code generation
- No symbol information
- Faster execution (no safety checks)

**Educational progression:**
1. **Learn in debug mode**: All safety checks enabled
2. **Understand consequences**: See what happens when checks are disabled
3. **Optimize carefully**: Disable checks only after thorough testing
4. **Learn trade-offs**: Performance vs safety, when each is appropriate

---

## 9. Standard Library Integration

### 9.1 System Unit Extensions

The `System` unit should provide:

```pascal
unit System;

interface
  // Memory access
  function Peek(Addr: word): byte;
  function PeekW(Addr: word): word;
  procedure Poke(Addr: word; Value: byte);
  procedure PokeW(Addr: word; Value: word);
  
  // I/O port access
  function PortIn(Port: byte): byte;
  function PortInW(Port: byte): word;
  procedure PortOut(Port: byte; Value: byte);
  procedure PortOutW(Port: byte; Value: word);
  
  // I/O port constants
  const
    IO_PIO_DATA_A = $D0;
    IO_PIO_CTRL_A = $D2;
    IO_PIO_DATA_B = $D1;
    IO_PIO_CTRL_B = $D3;
    
implementation
end.
```

### 9.2 Hardware Unit

Optional hardware abstraction unit:

```pascal
unit Hardware;

interface
  // PIO access
  function ReadPIO(Port: byte): byte;
  procedure WritePIO(Port: byte; Value: byte);
  
  // Memory-mapped hardware
  procedure WriteHardwareReg(Addr: word; Value: byte);
  function ReadHardwareReg(Addr: word): byte;
  
implementation
end.
```

---

## 10. Examples

### 10.1 Basic Memory Access

```pascal
program MemoryDemo;
uses System;

var
  addr: word;
  value: byte;
  
begin
  // Read from memory
  value := Peek($8000);
  WriteLn('Value at $8000: ', value);
  
  // Write to memory
  Poke($8000, $42);
  
  // Read word
  var wordVal: word;
  wordVal := PeekW($8000);
  
  // Write word
  PokeW($8000, $1234);
end.
```

### 10.2 I/O Port Access

```pascal
program IODemo;
uses System;

begin
  // Initialize PIO
  PortOut(IO_PIO_CTRL_B, $CF);  // Set bit mode
  PortOut(IO_PIO_CTRL_B, $EC);  // Set directions
  PortOut(IO_PIO_DATA_B, $FF);  // Set default values
  
  // Read status
  var status: byte;
  status := PortIn(IO_PIO_DATA_B);
  
  // Check VBlank
  if (status and $40) = 0 then
    WriteLn('VBlank active');
end.
```

### 10.3 Absolute Addressing

```pascal
program HardwareDemo;

var
  // Memory-mapped video registers
  VideoMode: byte absolute $8000;
  VideoCtrl: byte absolute $8001;
  VideoPalette: array[0..15] of word absolute $9000;
  
begin
  // Configure video
  VideoMode := 1;  // Tile mode
  VideoCtrl := $80;  // Enable display
  
  // Set palette
  VideoPalette[0] := $0000;  // Black
  VideoPalette[1] := $1F00;  // Red
  VideoPalette[2] := $001F;  // Blue
end.
```

### 10.4 Combined Memory and I/O

```pascal
program HardwareControl;

var
  // Memory-mapped data
  FrameBuffer: array[0..7679] of byte absolute $C000;  // 320x24 bytes
  
begin
  // Initialize PIO
  PortOut(IO_PIO_CTRL_B, $CF);
  PortOut(IO_PIO_CTRL_B, $EC);
  
  // Wait for VBlank
  while (PortIn(IO_PIO_DATA_B) and $40) <> 0 do
    ;  // Wait
  
  // Write to frame buffer
  FrameBuffer[0] := $FF;
  
  // Use Poke for direct access
  Poke($C000, $AA);
end.
```

---

## 11. Performance Considerations

### 11.1 Memory Access Speed

**Fastest Access:**
- Direct variable access: 1-2 T-states
- `Peek`/`Poke`: ~7 T-states (Z80 memory access)
- `PeekW`/`PokeW`: ~14 T-states (two memory accesses)

**Optimization:**
- Use absolute addressing for frequently accessed locations
- Cache port values in variables
- Batch memory operations when possible

### 11.2 I/O Port Speed

**Port Access:**
- `PortIn`/`PortOut`: ~11 T-states (Z80 I/O access)
- Slower than memory access (I/O wait states)

**Best Practices:**
- Minimize port accesses in tight loops
- Read port once, cache in variable
- Use interrupts instead of polling when possible

---

## 12. Integration with Zeal Hardware

### 12.1 RAM Access

Based on [Zeal RAM documentation](https://zeal8bit.com/docs/ram/):
- Physical RAM: 0x80000 to 0xFFFFF (512 KB)
- Access via MMU mapping to 16-bit logical addresses
- No memory protection (can read/write/execute)
- Fast access: 55ns propagation delay

**Usage:**
```pascal
// Access RAM via logical address (MMU handles mapping)
var data: byte;
data := Peek($4000);  // Logical address, MMU maps to physical
```

### 12.2 PIO Access

Based on [Zeal PIO documentation](https://zeal8bit.com/docs/pio/#overview):
- Port A: Data $D0, Control $D2
- Port B (System): Data $D1, Control $D3
- Bit-control mode for system port
- Interrupt support available

**Initialization:**
```pascal
procedure InitPIO;
begin
  // Disable interrupts
  PortOut(IO_PIO_CTRL_B, $07);  // Disable interrupt
  
  // Set bit mode
  PortOut(IO_PIO_CTRL_B, $CF);
  
  // Set directions (1=input, 0=output)
  PortOut(IO_PIO_CTRL_B, $EC);  // Keyboard, VBlank, HBlank, UART RX, I2C SDA IN are inputs
  
  // Set default output values
  PortOut(IO_PIO_DATA_B, $FF);
end;
```

---

## 13. Comparison with Other Languages

### 13.1 BASIC

**BASIC:**
```basic
PEEK(32768)    ' Read byte
POKE 32768, 255 ' Write byte
```

**SuperPascal:**
```pascal
Peek($8000)      // Read byte
Poke($8000, $FF) // Write byte
```

### 13.2 Turbo Pascal

**Turbo Pascal:**
```pascal
Mem[0:$8000]    // Read byte
MemW[0:$8000]   // Read word
Port[$D1]       // I/O port
```

**SuperPascal:**
```pascal
Peek($8000)      // Read byte
PeekW($8000)     // Read word
PortIn($D1)     // I/O port read
PortOut($D1, $FF) // I/O port write
```

### 13.3 C

**C:**
```c
*(unsigned char*)0x8000 = 0xFF;  // Write byte
unsigned char val = *(unsigned char*)0x8000;  // Read byte
```

**SuperPascal:**
```pascal
Poke($8000, $FF);  // Write byte
var val := Peek($8000);  // Read byte
```

---

## 14. Summary

SuperPascal provides comprehensive memory and I/O access:

**Memory Access:**
- `Peek` / `PeekW` - Read bytes/words (PEEK equivalent)
- `Poke` / `PokeW` - Write bytes/words (POKE equivalent)
- `absolute` directive - Variables at fixed addresses

**I/O Port Access:**
- `PortIn` / `PortInW` - Read from I/O ports
- `PortOut` / `PortOutW` - Write to I/O ports

**Features:**
- Direct hardware control
- Memory-mapped I/O support
- Type-safe wrappers available
- Educational clarity (PEEK/POKE familiar to BASIC users)

This enables low-level system programming while maintaining Pascal's clarity and safety features.

---

**End of Memory and I/O Access Specification**

