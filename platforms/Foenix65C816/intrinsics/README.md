# Foenix65C816 Platform — Intrinsics Index

## Overview

This directory contains all platform-specific intrinsics for the Foenix65C816 platform (Foenix F256x systems, WDC W65C816S @ 6.29 MHz).

Intrinsics are built-in procedures and functions that map directly to hardware operations or runtime services. They provide efficient, low-level access to Foenix platform features.

**Status:** Planned — Structure defined, implementation pending

---

## Intrinsics by Category

### 1. [VICKY Graphics Intrinsics](./01_VICKYIntrinsics.md)
VICKY graphics chip hardware control (TinyVicky, TinyVicky II, Vicky "The Fourth"):
- Register access (`VICKY_ReadReg`, `VICKY_WriteReg`)
- Master control registers
- Border control and colors
- Text mode and cursor control
- Graphics mode and background colors
- Line interrupt control
- Raster position queries
- Sprite control (extended - pending)
- Tilemap control (extended - pending)
- Bitmap control (extended - pending)

**Reference:** [Foenix MicroKernel - TinyVicky_Def.asm](../../../../SuperPascal/Foenix/f256-microkernel/f256/TinyVicky_Def.asm)

### 2. [Direct Memory Access](./02_DirectMemoryAccess.md)
Direct memory read/write:
- `Peek` - Read byte from memory
- `PeekW` - Read word from memory
- `Poke` - Write byte to memory
- `PokeW` - Write word to memory

### 3. [I/O Port Access](./03_IOPortAccess.md)
Direct I/O port access:
- `PortIn` - Read byte from I/O port
- `PortInW` - Read word from I/O port
- `PortOut` - Write byte to I/O port
- `PortOutW` - Write word to I/O port

### 4. [Absolute Addressing](./04_AbsoluteAddressing.md)
Absolute variable addressing:
- `absolute` directive - Place variable at specific memory address

### 5. [Interrupt Control](./05_InterruptControl.md)
Interrupt controller management:
- Interrupt pending registers
- Interrupt polarity control
- Edge detection enable
- Interrupt masking
- Interrupt sources: SOF, SOL, Keyboard, Mouse, Timer, RTC, DMA, UART, VIA, IEC, SDCard

**Reference:** [Foenix MicroKernel - interrupt_def.asm](../../../../SuperPascal/Foenix/f256-microkernel/f256/interrupt_def.asm)

### 6. Memory Intrinsics (Planned)
MMU page mapping and memory management:
- IO Page architecture
- Memory bank switching
- Expansion slot access

### 7. Audio Intrinsics (Planned)
Audio hardware control:
- PSG (Programmable Sound Generator) - FPGA emulation
- SID (Sound Interface Device) - FPGA emulation
- OPL3 (Yamaha FM synthesis) - FPGA emulation or real chip
- CODEC (WM8776)
- PWM audio
- SAM2695
- VS1053b

**Reference:** [Foenix MicroKernel - audio.asm](../../../../SuperPascal/Foenix/f256-microkernel/f256/audio.asm)

### 8. Input/Output Intrinsics (Planned)
Input and output device control:
- Keyboard (PS/2, CBM, F256K, F256K2)
- Mouse (PS/2)
- Joystick (Atari-style, SNES/NES)
- Serial ports
- SD Card
- IEC bus

### 9. Game Engine Intrinsics (Planned)
Game engine hardware acceleration:
- Entity management
- Component access
- Collision detection
- Animation
- Physics
- Platform-specific hardware acceleration

---

## Platform-Specific Notes

### VICKY Graphics Chip Variants

The Foenix F256 series uses different VICKY graphics chips depending on the model:

- **F256Jr, F256K:** TinyVicky
- **F256JrJr (Jr2):** TinyVicky II
- **F256K2:** Vicky "The Fourth"

Intrinsics should abstract these differences where possible, with platform-specific variants where necessary.

### IO Page Architecture

Foenix systems use an IO Page architecture where hardware registers are mapped into specific address ranges. The standard IO window is $C000-$DFFF, with additional registers accessible via IO Page registers.

### Memory Model

- **Address Space:** 16MB (24-bit addressing)
- **RAM:** 512KB-2MB SRAM (model-dependent), optional 256KB expansion, DDR3 on K2 (128MB)
- **Flash:** 512KB per context (4 contexts on K2)
- **MMU:** Four concurrent memory maps (kernel reserves map 0, user programs use map 3)

---

## SDK References

### Complete Foenix SDK Available

**Local SDK Location:** `/Users/casibbald/Workspace/casibbald/SuperPascal/Foenix/`

#### Core Resources

- **f256-microkernel/** - MicroKernel with hardware definitions
  - `f256/TinyVicky_Def.asm` - **Complete VICKY register definitions**
  - `f256/interrupt_def.asm` - **Complete interrupt controller definitions**
  - `f256/audio.asm` - Audio chip definitions
  - `f256/keyboard.asm` - Keyboard handling
  - `f256/serial.asm` - Serial port handling
  - `f256/iec.asm` - IEC bus handling
  - `hardware/` - Hardware driver implementations (RTC, UART, PS/2, etc.)

#### Additional Resources

- **BASIC816/** - BASIC interpreter for 65C816
  - `src/macros.s` - **Calling convention macros (ENTER/LEAVE) - GOLD for ABI!**
  - `src/basic816.s` - Complete BASIC implementation
  - Shows 65C816 calling conventions in practice

- **f256-superbasic/** - Another BASIC implementation
  - Reference implementation for language features

- **F256-FileManager/** - File manager application
  - Shows I/O patterns and API usage

- **FoenixIDE/Manuals/** - PDF documentation
  - `65816 Programmanual.pdf` - 65C816 programming manual
  - `65816 Quick Reference.pdf` - Quick reference
  - `C256_Foenix_Memory_Map_RevA.pdf` - Memory map
  - `c256-foenix-devguide.pdf` - Developer guide

#### Online References

- **Foenix Hardware Wiki:** https://wiki.f256foenix.com/index.php?title=Hardware

---

## Implementation Status

| Category | Status | Notes |
|----------|--------|-------|
| VICKY Graphics | ✅ Documented | Core features documented, extended features pending |
| Direct Memory Access | ✅ Documented | Peek/Poke intrinsics documented |
| I/O Port Access | ✅ Documented | PortIn/PortOut intrinsics documented |
| Absolute Addressing | ✅ Documented | Absolute directive documented |
| Interrupt Control | ✅ Documented | Interrupt controller intrinsics documented |
| Memory | Planned | IO Page architecture documented |
| Audio | Planned | Multiple audio chips supported |
| Input/Output | Planned | Multiple device types supported |
| Game Engine | Planned | Platform-specific acceleration pending |

---

**Last Updated:** 2025-01-XX  
**Platform:** Foenix65C816 (WDC W65C816S @ 6.29 MHz)

