# Foenix65C816 Platform — Interrupt Control Intrinsics

**Platform:** Foenix65C816 (Foenix F256x systems)  
**Part of:** [Foenix65C816 Platform Intrinsics](./README.md)

---

Based on the [Foenix MicroKernel interrupt definitions](../../../../SuperPascal/Foenix/f256-microkernel/f256/interrupt_def.asm), SuperPascal provides intrinsics for interrupt controller management.

---

## 1. Interrupt Register Access

### INT_ReadPending

**Syntax:**
```pascal
function INT_ReadPending(Reg: byte): byte;
```

**Purpose**: Read interrupt pending register (read and write back to clear).

**Parameters:**
- `Reg`: Register number (0 or 1)

**Returns**: Pending interrupt bits

**Interrupt Sources (Register 0):**
```pascal
const
  INT00_SOF    = $01;  // Start of Frame @ 60FPS
  INT01_SOL    = $02;  // Start of Line (Programmable)
  INT02_KBD    = $04;  // Keyboard
  INT03_MOUSE  = $08;  // Mouse
  INT04_TMR0   = $10;  // Timer 0
  INT05_TMR1   = $20;  // Real-Time Clock Interrupt
  INT06_DMA    = $40;  // Floppy Disk Controller
  INT07_TBD    = $80;  // Reserved
```

**Interrupt Sources (Register 1):**
```pascal
const
  INT00_UART   = $01;  // UART
  INT01_COL0   = $02;  // VICKY Collision 0
  INT02_COL1   = $04;  // VICKY Collision 1
  INT03_COL2   = $08;  // VICKY Collision 2
  INT04_RTC    = $10;  // RTC
  INT05_VIA    = $20;  // VIA
  INT06_IEC    = $40;  // IEC Bus
  INT07_SDCARD = $80;  // SD Card Insert
```

**Usage:**
```pascal
var pending: byte;
pending := INT_ReadPending(0);  // Read register 0
if (pending and INT00_SOF) <> 0 then
  WriteLn('Start of Frame interrupt');
```

### INT_WritePending

**Syntax:**
```pascal
procedure INT_WritePending(Reg: byte; Value: byte);
```

**Purpose**: Write interrupt pending register (clears pending bits).

**Parameters:**
- `Reg`: Register number (0 or 1)
- `Value`: Bits to clear (write 1 to clear)

**Usage:**
```pascal
// Clear Start of Frame interrupt
INT_WritePending(0, INT00_SOF);
```

### INT_GetPolarity

**Syntax:**
```pascal
function INT_GetPolarity(Reg: byte): byte;
```

**Purpose**: Read interrupt polarity register.

**Parameters:**
- `Reg`: Register number (0 or 1)

**Returns**: Polarity bits (0 = active low, 1 = active high)

### INT_SetPolarity

**Syntax:**
```pascal
procedure INT_SetPolarity(Reg: byte; Value: byte);
```

**Purpose**: Set interrupt polarity register.

**Parameters:**
- `Reg`: Register number (0 or 1)
- `Value`: Polarity bits

### INT_GetEdge

**Syntax:**
```pascal
function INT_GetEdge(Reg: byte): byte;
```

**Purpose**: Read edge detection enable register.

**Parameters:**
- `Reg`: Register number (0 or 1)

**Returns**: Edge detection bits (1 = edge-triggered, 0 = level-triggered)

### INT_SetEdge

**Syntax:**
```pascal
procedure INT_SetEdge(Reg: byte; Value: byte);
```

**Purpose**: Set edge detection enable register.

**Parameters:**
- `Reg`: Register number (0 or 1)
- `Value`: Edge detection bits

### INT_GetMask

**Syntax:**
```pascal
function INT_GetMask(Reg: byte): byte;
```

**Purpose**: Read interrupt mask register (1 = enabled, 0 = masked).

**Parameters:**
- `Reg`: Register number (0 or 1)

**Returns**: Mask bits

### INT_SetMask

**Syntax:**
```pascal
procedure INT_SetMask(Reg: byte; Value: byte);
```

**Purpose**: Set interrupt mask register (enable/disable interrupts).

**Parameters:**
- `Reg`: Register number (0 or 1)
- `Value`: Mask bits (1 = enable, 0 = disable)

**Usage:**
```pascal
// Enable Start of Frame interrupt
var mask: byte;
mask := INT_GetMask(0);
mask := mask or INT00_SOF;
INT_SetMask(0, mask);
```

---

## 2. Convenience Functions

### INT_Enable

**Syntax:**
```pascal
procedure INT_Enable(Reg: byte; Interrupt: byte);
```

**Purpose**: Enable a specific interrupt.

**Parameters:**
- `Reg`: Register number (0 or 1)
- `Interrupt`: Interrupt bit to enable

**Usage:**
```pascal
INT_Enable(0, INT00_SOF);  // Enable Start of Frame interrupt
```

### INT_Disable

**Syntax:**
```pascal
procedure INT_Disable(Reg: byte; Interrupt: byte);
```

**Purpose**: Disable a specific interrupt.

**Parameters:**
- `Reg`: Register number (0 or 1)
- `Interrupt`: Interrupt bit to disable

**Usage:**
```pascal
INT_Disable(0, INT00_SOF);  // Disable Start of Frame interrupt
```

### INT_Clear

**Syntax:**
```pascal
procedure INT_Clear(Reg: byte; Interrupt: byte);
```

**Purpose**: Clear a pending interrupt.

**Parameters:**
- `Reg`: Register number (0 or 1)
- `Interrupt`: Interrupt bit to clear

**Usage:**
```pascal
INT_Clear(0, INT00_SOF);  // Clear Start of Frame interrupt
```

### INT_IsPending

**Syntax:**
```pascal
function INT_IsPending(Reg: byte; Interrupt: byte): boolean;
```

**Purpose**: Check if an interrupt is pending.

**Parameters:**
- `Reg`: Register number (0 or 1)
- `Interrupt`: Interrupt bit to check

**Returns**: `true` if interrupt is pending, `false` otherwise

**Usage:**
```pascal
if INT_IsPending(0, INT00_SOF) then
  WriteLn('Start of Frame interrupt pending');
```

---

## 3. Register Map

| Address | Register | Read/Write | Description |
|---------|----------|------------|-------------|
| $D660 | INT_PENDING_REG0 | R/W | Pending interrupts register 0 (write to clear) |
| $D661 | INT_PENDING_REG1 | R/W | Pending interrupts register 1 (write to clear) |
| $D662 | INT_PENDING_REG2 | - | Reserved (not used) |
| $D663 | INT_PENDING_REG3 | - | Reserved (not used) |
| $D664 | INT_POL_REG0 | R/W | Interrupt polarity register 0 |
| $D665 | INT_POL_REG1 | R/W | Interrupt polarity register 1 |
| $D666 | INT_POL_REG2 | - | Reserved (not used) |
| $D667 | INT_POL_REG3 | - | Reserved (not used) |
| $D668 | INT_EDGE_REG0 | R/W | Edge detection enable register 0 |
| $D669 | INT_EDGE_REG1 | R/W | Edge detection enable register 1 |
| $D66A | INT_EDGE_REG2 | - | Reserved (not used) |
| $D66B | INT_EDGE_REG3 | - | Reserved (not used) |
| $D66C | INT_MASK_REG0 | R/W | Interrupt mask register 0 |
| $D66D | INT_MASK_REG1 | R/W | Interrupt mask register 1 |
| $D66E | INT_MASK_REG2 | - | Reserved (not used) |
| $D66F | INT_MASK_REG3 | - | Reserved (not used) |

---

## 4. Usage Examples

### Example: Enable Start of Frame Interrupt

```pascal
program EnableSOF;
begin
  // Enable Start of Frame interrupt
  INT_Enable(0, INT00_SOF);
  
  // Check if interrupt is pending
  if INT_IsPending(0, INT00_SOF) then
    WriteLn('SOF interrupt pending');
    
  // Clear interrupt
  INT_Clear(0, INT00_SOF);
end.
```

### Example: Poll Interrupts

```pascal
program PollInterrupts;
var
  pending: byte;
begin
  // Read pending interrupts
  pending := INT_ReadPending(0);
  
  // Check each interrupt source
  if (pending and INT00_SOF) <> 0 then
    WriteLn('Start of Frame');
    
  if (pending and INT02_KBD) <> 0 then
    WriteLn('Keyboard interrupt');
    
  if (pending and INT03_MOUSE) <> 0 then
    WriteLn('Mouse interrupt');
    
  // Clear all pending interrupts
  INT_WritePending(0, pending);
end.
```

### Example: Configure Interrupt

```pascal
program ConfigureInterrupt;
begin
  // Set edge-triggered mode for keyboard interrupt
  var edge: byte;
  edge := INT_GetEdge(0);
  edge := edge or INT02_KBD;
  INT_SetEdge(0, edge);
  
  // Set active high polarity
  var pol: byte;
  pol := INT_GetPolarity(0);
  pol := pol or INT02_KBD;
  INT_SetPolarity(0, pol);
  
  // Enable interrupt
  INT_Enable(0, INT02_KBD);
end.
```

---

## 5. Platform-Specific Notes

### Kernel Ownership

The Foenix MicroKernel reserves certain interrupts:
- **RTC**: Kernel-owned
- **SOF (Start of Frame)**: Kernel-owned
- **PS2**: Kernel-owned (keyboard/mouse)
- **Serial**: Kernel-owned (if SLIP or Feather DIP switches are set)

User programs should not enable interrupts that are owned by the kernel.

### IRQ Events

Starting with January 2025 kernel releases, programs can receive IRQ events for non-kernel interrupts. See the MicroKernel documentation for details.

---

**Reference:** [Foenix MicroKernel - interrupt_def.asm](../../../../SuperPascal/Foenix/f256-microkernel/f256/interrupt_def.asm)  
**Last Updated:** 2025-01-XX  
**Platform:** Foenix65C816 (WDC W65C816S @ 6.29 MHz)

