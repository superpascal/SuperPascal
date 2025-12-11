# Commander X16 Platform — SuperPascal for Commander X16

## Overview

**Platform:** CommanderX16  
**CPU:** WDC 65C02 @ 8 MHz  
**System:** Commander X16 Retro Computer  
**Status:** 🔄 **Planned** (Tier 1 Platform)

The Commander X16 is a modern retro computer designed by 8-Bit Guy (David Murray) that combines classic 8-bit computing with modern features. It uses the WDC 65C02 CPU (6502-compatible) and features the custom VERA graphics chip for advanced video capabilities.

---

## Platform Features

### Hardware
- **CPU:** WDC 65C02S @ 8 MHz
  - **Architecture:** 8-bit, 6502-compatible
  - **Instruction Set:** 65C02 (enhanced 6502)
  - **Note:** Future upgrade path to 65C816 (avoid BBRx, BBSx, RMBx, SMBx instructions)
- **Memory:** 
  - 512 KB banked RAM (upgradeable to 2 MB on Developer Edition)
  - 512 KB ROM (KERNAL, BASIC, CMDR-DOS, character sets, etc.)
  - Up to 3.5 MB additional RAM/ROM via cartridges
- **Graphics:** VERA (Versatile Embedded Retro Adapter)
  - **Resolution:** Up to 640x480 @ 60 Hz (fixed resolution)
  - **Colors:** 256 colors from 4096-color palette
  - **Sprites:** 128 hardware sprites, up to 64x64 pixels each
  - **Layers:** 2 layers (bitmap or tilemap modes)
  - **VRAM:** 128 KB embedded video RAM
  - **Output:** VGA, NTSC Composite, NTSC S-Video, RGB
  - **FPGA:** Lattice ICE40UP5K
- **Audio:** 
  - **YM2151 (OPM):** 8 channels, 4-operator FM synthesis
  - **VERA PSG:** 16 channels, 4 waveforms (Pulse, Sawtooth, Triangle, Noise)
  - **VERA PCM:** Up to 48 kHz, 16-bit, stereo (4 KB FIFO buffer)
- **Storage:**
  - SD card slot (FAT32 via CMDR-DOS)
  - IEC serial bus (Commodore-style, supports Fast IEC)
- **I/O:**
  - PS/2 keyboard port
  - PS/2 mouse port
  - 4x SNES controller ports (2 on-board, 2 via header)
  - IEC serial port (6-pin DIN 45322)
  - User port header (VIA 2, parallel/serial I/O)
  - Expansion slots (Gen 1: 4 slots, 60-pin edge connector)
  - Cartridge slot (external expansion cards)

### Software
- **OS:** KERNAL (Commodore-style operating system)
- **BASIC:** X16 BASIC (enhanced Commodore BASIC)
- **File System:** FAT32 on SD card

---

## Architecture Details

### CPU Architecture
- **ISA:** 65C02 (enhanced 6502)
- **Endianness:** Little-endian
- **Register Width:** 8-bit
- **Address Width:** 16-bit (64 KB logical, banked to 512 KB physical)
- **Calling Convention:** To be specified (6502-style stack-based)
- **Floating Point:** Software (no hardware FPU)

### Memory Model
- **Logical Address Space:** 64 KB (16-bit addressing)
- **Physical Memory:** 512 KB RAM (banked), 512 KB ROM, up to 2 MB RAM on Developer Edition
- **Memory Map:**
  - **$0000-$9EFF:** Fixed RAM (40 KB minus 256 bytes)
  - **$9F00-$9FFF:** I/O Area (256 bytes)
    - $9F00-$9F0F: VIA I/O controller #1
    - $9F10-$9F1F: VIA I/O controller #2
    - $9F20-$9F3F: VERA video controller
    - $9F40-$9F41: YM2151 audio controller
    - $9F60-$9FFF: Expansion Card Memory Mapped IO (IO3-IO7)
  - **$A000-$BFFF:** Banked RAM (8 KB window into one of 256 banks, total 2 MB)
  - **$C000-$FFFF:** Banked System ROM and Cartridge ROM/RAM (16 KB window into one of 256 banks)
- **Banking Registers:**
  - **$0000:** Current RAM bank (0-255)
  - **$0001:** Current ROM/Cartridge bank (0-31 = ROM, 32-255 = Cartridge)

### Register Set
- **Accumulator:** A (8-bit)
- **Index Registers:** X, Y (8-bit)
- **Stack Pointer:** SP (8-bit, points to $0100-$01FF)
- **Program Counter:** PC (16-bit)
- **Status Register:** P (8-bit, flags: N, V, B, D, I, Z, C)

### VERA Graphics Chip
- **Video Modes:** Multiple modes (text, bitmap, tilemap)
- **Resolution:** Up to 640x480 @ 60 Hz
- **Colors:** 256 colors from 4096-color palette
- **Sprites:** 128 hardware sprites, 64 colors each
- **Tilemaps:** 2 layers with hardware scrolling
- **Memory:** 128 KB VRAM (banked)

---

## Documentation Structure

### [ABI.md](./ABI.md) (To Be Created)
Application Binary Interface specification:
- 65C02 calling conventions
- Register usage conventions
- Stack frame layout (6502-style)
- Parameter passing (value, var, const)
- Return value conventions
- Record and class memory layout
- VTable structure
- Exception frame layout
- Object file format

### [intrinsics/](./intrinsics/) (To Be Created)
Platform-specific intrinsics:
- VERA graphics intrinsics
- YM2151 audio intrinsics
- Memory banking intrinsics
- Direct memory access (Peek/Poke)
- I/O port access
- Interrupt control

### [runtime/](./runtime/) (To Be Created)
Runtime specification:
- KERNAL integration
- Memory banking support
- Interrupt handling
- System call interface

### [stdlib/](./stdlib/) (To Be Created)
Platform-specific standard library units:
- `VERA_Graphics` - VERA graphics API
- `VERA_Sprites` - Sprite management
- `VERA_Tilemap` - Tilemap operations
- `YM2151_Audio` - FM synthesis audio
- `X16_System` - KERNAL system calls
- `X16_IEC` - IEC serial bus interface

---

## Quick Start

### Hello World

```pascal
program HelloWorld;
uses System;

begin
  WriteLn('Hello, Commander X16!');
end.
```

### Graphics Example (Future)

```pascal
program GraphicsDemo;
uses System, VERA_Graphics;

begin
  // Set VERA to 320x240 mode
  VERA_SetMode(VERA_MODE_320x240);
  
  // Set sprite
  VERA_SpriteSet(0, 100, 100, 1);
  
  // Wait for vblank
  WaitVBlank;
end.
```

### Audio Example (Future)

```pascal
program AudioDemo;
uses System, YM2151_Audio;

begin
  YM2151_Initialize;
  YM2151_SetFrequency(0, 440);  // A4 note
  YM2151_SetVolume(0, 127);
end.
```

---

## Compiler Usage

```bash
# Compile for Commander X16 platform
spc --platform=commanderx16 program.pas

# With optimizations
spc --platform=commanderx16 -O2 program.pas

# Generate assembly
spc --platform=commanderx16 -S program.pas
```

---

## Platform-Specific Constants

```pascal
const
  // Banking Registers
  RAM_BANK         = $0000;  // Current RAM bank (0-255)
  ROM_BANK         = $0001;  // Current ROM/Cartridge bank (0-31=ROM, 32-255=Cartridge)
  
  // VERA Registers ($9F20-$9F3F)
  VERA_ADDR_LOW    = $9F20;  // VRAM Address (7:0)
  VERA_ADDR_MID    = $9F21;  // VRAM Address (15:8)
  VERA_ADDR_HIGH   = $9F22;  // VRAM Address (16) + increment settings
  VERA_DATA0       = $9F23;  // VRAM Data port 0
  VERA_DATA1       = $9F24;  // VRAM Data port 1
  VERA_CTRL        = $9F25;  // Control register
  VERA_IEN         = $9F26;  // Interrupt enable
  VERA_ISR         = $9F27;  // Interrupt status
  VERA_IRQLINE_L   = $9F28;  // IRQ line (7:0)
  VERA_IRQLINE_H   = $9F29;  // IRQ line (8)
  // ... (see VERA Programmer's Reference for complete register map)
  
  // YM2151 Registers
  YM2151_ADDR      = $9F40;  // YM2151 address register
  YM2151_DATA      = $9F41;  // YM2151 data register
  
  // VIA I/O Controllers
  VIA1_BASE        = $9F00;  // VIA I/O controller #1
  VIA2_BASE        = $9F10;  // VIA I/O controller #2 (user port)
  
  // ROM Bank Allocations
  ROM_BANK_KERNAL  = 0;      // KERNAL operating system
  ROM_BANK_KEYBD   = 1;      // Keyboard layout tables
  ROM_BANK_CMDRDOS = 2;      // CMDR-DOS for FAT32 SD cards
  ROM_BANK_FAT32   = 3;      // FAT32 driver
  ROM_BANK_BASIC   = 4;      // BASIC interpreter
  ROM_BANK_MONITOR = 5;      // Machine Language Monitor
  ROM_BANK_CHARSET = 6;      // PETSCII and ISO character sets
  ROM_BANK_DIAG    = 7;      // Memory diagnostic
  // ... (see Memory Map chapter for complete ROM bank allocation)
```

---

## Memory Map

```
Logical Address Space (16-bit, banked):
- $0000-$9EFF: Fixed RAM (40 KB minus 256 bytes)
  - $0000-$00FF: Zero page (banking registers at $0000-$0001)
  - $0100-$01FF: CPU stack
  - $0200-$03FF: KERNAL and BASIC variables
  - $0400-$07FF: Available for machine code programs
  - $0800-$9EFF: BASIC program/variables
- $9F00-$9FFF: I/O Area (256 bytes)
  - $9F00-$9F0F: VIA I/O controller #1
  - $9F10-$9F1F: VIA I/O controller #2
  - $9F20-$9F3F: VERA video controller
  - $9F40-$9F41: YM2151 audio controller
  - $9F42-$9F5F: Unavailable
  - $9F60-$9F7F: Expansion Card MMIO IO3
  - $9F80-$9F9F: Expansion Card MMIO IO4
  - $9FA0-$9FBF: Expansion Card MMIO IO5
  - $9FC0-$9FDF: Expansion Card MMIO IO6
  - $9FE0-$9FFF: Cartridge/Expansion MMIO IO7
- $A000-$BFFF: Banked RAM (8 KB window, 256 banks = 2 MB total)
- $C000-$FFFF: Banked System ROM/Cartridge (16 KB window, 256 banks)

Physical Memory:
- 512 KB RAM (standard) or 2 MB RAM (Developer Edition)
- 512 KB ROM (KERNAL, BASIC, CMDR-DOS, character sets, etc.)
- Up to 3.5 MB additional RAM/ROM via cartridges
```

---

## Standard Library Units

Platform-specific standard library units:

- `VERA_Graphics` - High-level VERA graphics API
- `VERA_Sprites` - Sprite management
- `VERA_Tilemap` - Tilemap operations
- `YM2151_Audio` - FM synthesis audio
- `X16_System` - KERNAL system calls
- `X16_IEC` - IEC serial bus interface
- `X16_Joystick` - Joystick input

---

## Educational Value (Tier 1)

The Commander X16 platform is ideal for Tier 1 education because:

1. **Classic 8-bit Architecture:** 65C02 is a classic, well-understood CPU
2. **6502 Compatibility:** Shares heritage with Commodore 64, Apple II, NES
3. **Modern Features:** VERA graphics chip provides modern capabilities
4. **Active Community:** Active development and community support
5. **Retro Computing:** Perfect for learning retro computing concepts

### Tier 1 Use Cases
- Basic programming concepts
- 8-bit computing fundamentals
- Graphics programming (VERA)
- Audio programming (YM2151)
- Memory banking concepts
- Retro game development

---

## References

### Complete SDK Available

**Local SDK Location:** `/Users/casibbald/Workspace/casibbald/SuperPascal/CommandX16/`

#### Documentation (`x16-docs/`)
- ✅ **Complete Programmer's Reference Guide** - All chapters available
  - `X16 Reference - 05 - KERNAL.md` - **KERNAL API with calling conventions!**
  - `X16 Reference - 08 - Memory Map.md` - Complete memory map and banking
  - `X16 Reference - 09 - VERA Programmer's Reference.md` - VERA graphics chip complete reference
  - `X16 Reference - 10 - VERA FX Reference.md` - VERA FX features
  - `X16 Reference - 11 - Sound Programming.md` - Audio programming (YM2151, VERA PSG, PCM)
  - `X16 Reference - 12 - IO Programming.md` - I/O programming (VIA, user port)
  - `X16 Reference - 14 - Hardware.md` - Hardware pinouts and connectors
  - `X16 Reference - Appendix C - 65C02 Processor.md` - 65C02 instruction reference
  - `X16 Reference - Appendix F - 65C816 Processor.md` - 65C816 reference (for future upgrade)

#### ROM Source Code (`x16-rom/`) ⭐ **EXTREMELY VALUABLE**
- ✅ **KERNAL Source** (`kernal/`) - **Can extract calling conventions from assembly!**
  - `kernal/cbm/` - Commodore-compatible KERNAL routines
  - `kernal/drivers/x16/` - X16-specific drivers (keyboard, mouse, joystick, etc.)
  - `kernal/graph/` - Graphics routines
  - `kernal/x16/extapi.s` - Extended API implementation
- ✅ **Audio Routines** (`audio/`) - YM2151 and PSG implementation
- ✅ **Graphics Routines** (`graphics/`) - VERA graphics implementation
- ✅ **BASIC Interpreter** (`basic/`) - BASIC implementation
- ✅ **Math Library** (`math/`) - Math routines

#### Emulator Source (`x16-emulator/`)
- ✅ Complete emulator implementation (C/C++)
- Useful for understanding hardware behavior

#### Bootloader (`x16-smc-bootloader/`)
- ✅ System Management Controller bootloader source

### Online References

- [Commander X16 Official Website](https://www.commanderx16.com/)
- [Commander X16 Documentation](https://github.com/commanderx16/x16-docs)
- [VERA Original Documentation](https://github.com/fvdhoef/vera-module/blob/rev4/doc/VERA%20Programmer's%20Reference.md)
- [WDC 65C02 Datasheet](https://www.westerndesigncenter.com/wdc/documentation/w65c02s.pdf)

### Key Files for ABI/Intrinsics Development

**For ABI (Calling Conventions):**
- `x16-docs/X16 Reference - 05 - KERNAL.md` - Documents KERNAL calling conventions
- `x16-rom/kernal/` - Source code showing actual calling patterns
- `x16-rom/kernal/x16/extapi.s` - Extended API implementation

**For Intrinsics:**
- `x16-docs/X16 Reference - 09 - VERA Programmer's Reference.md` - Complete VERA API
- `x16-docs/X16 Reference - 11 - Sound Programming.md` - Complete audio API
- `x16-rom/graphics/` - Graphics implementation reference
- `x16-rom/audio/` - Audio implementation reference

---

## See Also

- [Platform Overview](../README.md) - All supported platforms
- [Language Specification](../../languageSpecification/00_Overview.md) - Platform-agnostic language core
- [Multi-Platform Architecture](../../MULTI_PLATFORM_ARCHITECTURE.md) - Architecture details
- [Target Architectures](../../docs/TARGET_ARCHITECTURES.md) - Complete architecture inventory

---

**Platform Maintainer:** SuperPascal Team  
**Last Updated:** 2024  
**Tier:** Tier 1 (Beginner)

