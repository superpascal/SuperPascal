# ZealZ80 Platform — Intrinsics Index

## Overview

This directory contains all platform-specific intrinsics for the ZealZ80 platform (Zeal 8-bit Computer, Z80).

Intrinsics are built-in procedures and functions that map directly to hardware operations or runtime services. They provide efficient, low-level access to Zeal platform features.

---

## Intrinsics by Category

### 1. [ZVB Intrinsics](./01_ZVBIntrinsics.md)
Comprehensive Zeal Video Board hardware control:
- Register access (`ZVB_ReadReg`, `ZVB_WriteReg`)
- Peripheral mapping (`ZVB_MapPeripheral`)
- Video mode control (`ZVB_SetVideoMode`, `ZVB_EnableScreen`)
- Raster position (`ZVB_GetVPos`, `ZVB_GetHPos`)
- Scrolling (`ZVB_SetLayer0Scroll`, `ZVB_SetLayer1Scroll`)
- Video status (`ZVB_IsHBlank`, `ZVB_IsVBlank`)
- Sprite control (`ZVB_SpriteSetFull`, `ZVB_SpriteSetX`, etc.)
- Text controller (`ZVB_TextPrintChar`, `ZVB_TextSetCursor`, etc.)
- DMA operations (`ZVB_DMAStart`, `ZVB_DMA_VirtToPhys`, etc.)
- SPI controller (`ZVB_SPI_Initialize`, `ZVB_SPI_Start`, etc.)
- CRC32 controller (`ZVB_CRC_Initialize`, `ZVB_CRC_Update`, etc.)
- Sound controller (`ZVB_Sound_Initialize`, `ZVB_Sound_SetFrequency`, etc.)

### 2. [Memory Intrinsics](./02_MemoryIntrinsics.md)
MMU page mapping:
- `MapPage` - Map physical page to MMU slot

### 3. [Direct Memory Access](./03_DirectMemoryAccess.md)
Direct memory read/write:
- `Peek` - Read byte from memory
- `PeekW` - Read word from memory
- `Poke` - Write byte to memory
- `PokeW` - Write word to memory

### 4. [I/O Port Access](./04_IOPortAccess.md)
Direct I/O port access:
- `PortIn` - Read byte from I/O port
- `PortInW` - Read word from I/O port
- `PortOut` - Write byte to I/O port
- `PortOutW` - Write word to I/O port

### 5. [Absolute Addressing](./05_AbsoluteAddressing.md)
Absolute variable addressing:
- `absolute` directive - Place variable at specific memory address

### 6. [Game Engine Intrinsics](./06_GameEngineIntrinsics.md)
Entity Component System API:
- Entity management (`EntityCreate`, `EntityDestroy`, `EntityValid`)
- Position and velocity (`EntitySetPosition`, `EntitySetVelocity`)
- Sprite assignment (`EntitySetSprite`)
- Collision detection (`CollisionCheck`, `CollisionCheckTile`)
- Animation (`EntitySetAnimation`, `AnimationUpdate`)
- Physics (`PhysicsApplyGravity`, `PhysicsApplyVelocity`)

### 7. [Extended Audio Intrinsics](./07_ExtendedAudioIntrinsics.md)
High-level audio API:
- Low-level hardware (`AudioRegWrite`, `AudioRegRead`)
- Tone and waveform (`SetTone`, `SetWaveform`)
- Sound effects (`PlaySFX`, `StopSFX`, `IsSFXPlaying`)
- Music playback (`PlayMusic`, `StopMusic`, `PauseMusic`)
- Music fading (`MusicFadeOut`, `MusicFadeIn`)
- Audio streaming (`StreamMusicStart`, `StreamMusicStop`)
- Sample playback (`PlaySample`, `PlaySampleLoop`)

---

## Platform-Agnostic Intrinsics

Some intrinsics are platform-agnostic and defined in the main language specification:

- `WaitVBlank` - Wait for vertical blank (concept is universal, implementation is platform-specific)
- Basic graphics operations (see [Language Specification](../../../languageSpecification/intrinsicsAndDirectives/))

---

## Usage Examples

### ZVB Graphics
```pascal
ZVB_SetVideoMode(ZVB_MODE_256);
ZVB_EnableScreen(true);
ZVB_SpriteSetFull(0, 100, 100, 1, 0);
```

### Memory Access
```pascal
var value: byte;
value := Peek($8000);
Poke($8000, $FF);
```

### I/O Ports
```pascal
var status: byte;
status := PortIn($D1);
PortOut($D1, $FF);
```

### Game Engine
```pascal
var entity: TEntity;
entity := EntityCreate;
EntitySetPosition(entity, 100, 100);
EntitySetSprite(entity, 1);
```

---

## See Also

- [ZealZ80 Platform Overview](../README.md)
- [Platform Specifications](../../README.md)
- [Language Specification](../../../languageSpecification/00_Overview.md)

