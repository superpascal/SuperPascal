# ZealZ80 Platform — SuperPascal for Zeal 8-bit Computer

## Overview

**Platform:** ZealZ80  
**CPU:** Zilog Z80 @ 10 MHz  
**System:** Zeal 8-bit Computer  
**Status:** ✅ Fully Specified

The ZealZ80 platform provides comprehensive support for the Zeal 8-bit Computer, including direct access to the ZVB (Zeal Video Board) graphics and audio hardware, MMU-based memory management, and a complete game engine API.

---

## Platform Features

### Hardware
- **CPU:** Zilog Z80 @ 10 MHz
- **Memory:** 512 KB SRAM (banked via MMU)
- **Graphics:** ZVB (Zeal Video Board) with FPGA-accelerated rendering
- **Audio:** ZVB audio controller (4 voices + sample table)
- **Storage:** TF card via SPI
- **I/O:** PIO (Parallel I/O) ports

### Software
- **OS:** ZealOS
- **File System:** ZealOS filesystem
- **Development:** ZealIDE integration

---

## Documentation Structure

### [ABI.md](./ABI.md)
Application Binary Interface specification:
- Z80 register usage conventions
- Stack frame layout
- Parameter passing (value, var, const)
- Return value conventions
- Record and class memory layout
- VTable structure
- Exception frame layout
- Object file format

### [gameEngine/](./gameEngine/)
Platform-specific game engine implementation:
- **[Game Engine Implementation](./gameEngine/README.md)** - ZVB hardware integration, sprite management, rendering pipeline

### [intrinsics/](./intrinsics/)
Platform-specific intrinsics:

1. **[ZVB Intrinsics](./intrinsics/01_ZVBIntrinsics.md)** - Zeal Video Board hardware control
   - Register access
   - Video mode configuration
   - Sprite control
   - Text controller
   - DMA operations
   - SPI controller
   - CRC32 controller
   - Sound controller

2. **[Memory Intrinsics](./intrinsics/02_MemoryIntrinsics.md)** - MMU page mapping

3. **[Direct Memory Access](./intrinsics/03_DirectMemoryAccess.md)** - Peek/Poke functions

4. **[I/O Port Access](./intrinsics/04_IOPortAccess.md)** - PortIn/PortOut functions

5. **[Absolute Addressing](./intrinsics/05_AbsoluteAddressing.md)** - Absolute variable addressing

6. **[Game Engine Intrinsics](./intrinsics/06_GameEngineIntrinsics.md)** - Entity Component System API

7. **[Extended Audio Intrinsics](./intrinsics/07_ExtendedAudioIntrinsics.md)** - High-level audio API

---

## Quick Start

### Hello World

```pascal
program HelloWorld;
uses System;

begin
  WriteLn('Hello, Zeal!');
end.
```

### Graphics Example

```pascal
program GraphicsDemo;
uses System, ZVB_Graphics;

begin
  ZVB_SetVideoMode(ZVB_MODE_256);
  ZVB_EnableScreen(true);
  
  // Set sprite
  ZVB_SpriteSetFull(0, 100, 100, 1, 0);
  
  // Wait for vblank
  WaitVBlank;
end.
```

### Audio Example

```pascal
program AudioDemo;
uses System, ZVB_Audio;

begin
  ZVB_Sound_Initialize(true);
  ZVB_Sound_SetFrequency(0, 440);  // A4 note
  ZVB_Sound_SetWaveform(0, ZVB_SOUND_SQUARE, ZVB_SOUND_DUTY_50);
  ZVB_Sound_SetVolume(0, ZVB_SOUND_VOL_50);
end.
```

---

## Compiler Usage

```bash
# Compile for ZealZ80 platform
spc --platform=zealz80 program.pas

# With optimizations
spc --platform=zealz80 -O2 program.pas

# Generate assembly
spc --platform=zealz80 -S program.pas
```

---

## Platform-Specific Constants

```pascal
const
  // ZVB Peripheral Indices
  ZVB_PERI_TEXT_IDX   = 0;
  ZVB_PERI_SPI_IDX     = 1;
  ZVB_PERI_CRC_IDX     = 2;
  ZVB_PERI_SOUND_IDX   = 3;
  
  // Video Modes
  ZVB_MODE_256         = 0;
  ZVB_MODE_16          = 1;
  
  // Sound Waveforms
  ZVB_SOUND_SQUARE     = 0;
  ZVB_SOUND_TRIANGLE   = 1;
  ZVB_SOUND_SAWTOOTH   = 2;
  ZVB_SOUND_NOISE      = 3;
```

---

## Memory Map

```
Logical Address Space (16-bit, banked via MMU):
- $0000-$3FFF: ROM (bank 0)
- $4000-$7FFF: RAM (banked, 512 KB total)
- $8000-$BFFF: I/O space (ZVB registers, PIO, etc.)
- $C000-$FFFF: Video RAM (mapped via MMU)
```

---

## Standard Library Units

Platform-specific standard library units:

- `ZVB_Graphics` - High-level graphics API
- `ZVB_Audio` - High-level audio API
- `ZVB_Sprites` - Sprite management
- `ZVB_Tilemap` - Tilemap operations
- `ZealOS` - Operating system interface
- `GameEngine` - ECS game engine

---

## See Also

- [Platform Overview](../README.md) - All supported platforms
- [Language Specification](../../languageSpecification/00_Overview.md) - Platform-agnostic language core
- [Multi-Platform Architecture](../../MULTI_PLATFORM_ARCHITECTURE.md) - Architecture details

---

**Platform Maintainer:** ZealPascal Team  
**Last Updated:** 2024

