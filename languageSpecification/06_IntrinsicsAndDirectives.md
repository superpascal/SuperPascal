# SuperPascal Language Specification — Intrinsics and Compiler Directives

## Language Extensions (Platform-Agnostic and Platform-Specific)

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

---

## Overview

This document serves as an **index** to the complete Intrinsics and Directives specification, which has been organized into separate files for better maintainability and navigation.

**Intrinsics** are built-in procedures and functions that map directly to hardware operations or runtime services. They provide efficient access to platform features.

**Note:** This document covers **platform-agnostic intrinsics** (concepts that apply across platforms). For **platform-specific intrinsics** (hardware-specific operations), see the [Platform Specifications](../platforms/README.md).

**Compiler Directives** are special comments that control compilation behavior, optimization, and conditional compilation.

---

## Document Structure

The specification is organized into the following sections:

### Intrinsics

### Platform-Agnostic Intrinsics

1. **[Overview](./intrinsicsAndDirectives/01_Overview.md)** - Introduction to intrinsics
2. **[Graphics Intrinsics](./intrinsicsAndDirectives/02_GraphicsIntrinsics.md)** - Basic graphics operations (concepts)
8. **[Audio Intrinsics](./intrinsicsAndDirectives/04_AudioIntrinsics.md)** - Basic audio operations (concepts)
9. **[Input Intrinsics](./intrinsicsAndDirectives/05_InputIntrinsics.md)** - Input reading (concepts)

### Platform-Specific Intrinsics

**ZealZ80 Platform:**
- **[ZVB Intrinsics](../platforms/ZealZ80/intrinsics/01_ZVBIntrinsics.md)** - Zeal Video Board hardware control (includes SPI, CRC32, Sound)
- **[Memory Management](../platforms/ZealZ80/intrinsics/02_MemoryIntrinsics.md)** - MMU page mapping
- **[Direct Memory Access](../platforms/ZealZ80/intrinsics/03_DirectMemoryAccess.md)** - Peek/Poke functions
- **[I/O Port Access](../platforms/ZealZ80/intrinsics/04_IOPortAccess.md)** - PortIn/PortOut functions
- **[Absolute Addressing](../platforms/ZealZ80/intrinsics/05_AbsoluteAddressing.md)** - Fixed memory addresses
- **[Game Engine Intrinsics](../platforms/ZealZ80/intrinsics/06_GameEngineIntrinsics.md)** - Entity and component operations
- **[Extended Audio Intrinsics](../platforms/ZealZ80/intrinsics/07_ExtendedAudioIntrinsics.md)** - Complete audio system

**See [Platform Specifications](../platforms/README.md) for all platforms.**

### Directives

6. **[Compiler Directives](./intrinsicsAndDirectives/06_CompilerDirectives.md)** - Directive syntax and types
7. **[Tilemap DSL](./intrinsicsAndDirectives/07_TilemapDSL.md)** - Tilemap domain-specific language
8. **[Intrinsic Implementation](./intrinsicsAndDirectives/08_IntrinsicImplementation.md)** - How intrinsics are processed
9. **[Directive Processing](./intrinsicsAndDirectives/09_DirectiveProcessing.md)** - How directives are processed
13. **[Game Engine Directives](./intrinsicsAndDirectives/13_GameEngineDirectives.md)** - ECS and physics directives

### Reference

12. **[Standard Intrinsics Summary](./intrinsicsAndDirectives/12_StandardIntrinsicsSummary.md)** - Quick reference table
14. **[Standard Directives Summary](./intrinsicsAndDirectives/14_StandardDirectivesSummary.md)** - Quick reference table

---

## Quick Navigation

### By Category

**Graphics & Video:**
- [Graphics Intrinsics](./intrinsicsAndDirectives/02_GraphicsIntrinsics.md) - Platform-agnostic concepts
- **[ZealZ80: ZVB Intrinsics](../platforms/ZealZ80/intrinsics/01_ZVBIntrinsics.md)** - Complete ZVB hardware API

**Memory & I/O:**
- **[ZealZ80: Memory Management](../platforms/ZealZ80/intrinsics/02_MemoryIntrinsics.md)** - `MapPage`
- **[ZealZ80: Direct Memory Access](../platforms/ZealZ80/intrinsics/03_DirectMemoryAccess.md)** - `Peek`, `PeekW`, `Poke`, `PokeW`
- **[ZealZ80: I/O Port Access](../platforms/ZealZ80/intrinsics/04_IOPortAccess.md)** - `PortIn`, `PortOut`, `PortInW`, `PortOutW`
- **[ZealZ80: Absolute Addressing](../platforms/ZealZ80/intrinsics/05_AbsoluteAddressing.md)** - `absolute` directive

**Audio:**
- [Audio Intrinsics](./intrinsicsAndDirectives/04_AudioIntrinsics.md) - Platform-agnostic concepts
- **[ZealZ80: Extended Audio Intrinsics](../platforms/ZealZ80/intrinsics/07_ExtendedAudioIntrinsics.md)** - Complete audio system

**Input:**
- [Input Intrinsics](./intrinsicsAndDirectives/05_InputIntrinsics.md) - Platform-agnostic concepts

**Game Engine:**
- **[ZealZ80: Game Engine Intrinsics](../platforms/ZealZ80/intrinsics/06_GameEngineIntrinsics.md)** - Entity, component, collision, animation, physics
- [Game Engine Directives](./intrinsicsAndDirectives/13_GameEngineDirectives.md) - ECS and physics optimization

**Compiler Features:**
- [Compiler Directives](./intrinsicsAndDirectives/06_CompilerDirectives.md) - All directive types
- [Tilemap DSL](./intrinsicsAndDirectives/07_TilemapDSL.md) - Tilemap syntax
- [Intrinsic Implementation](./intrinsicsAndDirectives/08_IntrinsicImplementation.md) - How intrinsics work
- [Directive Processing](./intrinsicsAndDirectives/09_DirectiveProcessing.md) - How directives work

**Reference:**
- [Standard Intrinsics Summary](./intrinsicsAndDirectives/12_StandardIntrinsicsSummary.md) - Complete table
- [Standard Directives Summary](./intrinsicsAndDirectives/14_StandardDirectivesSummary.md) - Complete table

---

## Quick Reference

### Most Common Intrinsics

**Graphics:**
```pascal
WaitVBlank;                    // Frame sync
SpriteSet(0, 100, 50, 42);     // Set sprite
SpriteShow(0, true);           // Show sprite
```

**ZVB:**
```pascal
ZVB_SetVideoMode(ZVB_MODE_GFX_320_8BIT);
ZVB_EnableScreen(true);
ZVB_SetLayer0Scroll(0, 0);
```

**Memory:**
```pascal
value := Peek($8000);          // Read byte
Poke($8000, $FF);              // Write byte
MapPage(2, 10);                // Map MMU page
```

**I/O:**
```pascal
status := PortIn($D1);         // Read port
PortOut($D1, $FF);             // Write port
```

**Audio:**
```pascal
PlaySFX(SFX_JUMP);
PlayMusic(@ThemeMusic);
MusicFadeOut(60);
```

**Input:**
```pascal
input := ReadInput;
ClearScreen(0);
```

### Most Common Directives

```pascal
{$INLINE}                      // Inline function
{$UNROLL}                      // Unroll loop
{$VBLANK_AWARE}                // VBlank optimization
{$RANGE_CHECK ON}              // Enable bounds checking
{$DEBUG}                       // Debug mode
{$IFDEF DEBUG}                 // Conditional compile
{$INCLUDE 'file.pas'}          // Include file
```

---

## Cross-References

- **Game Engine Types**: See [09_GameEngine.md](./09_GameEngine.md)
- **Audio System Types**: See [10_AudioSystem.md](./10_AudioSystem.md)
- **Memory and I/O**: See [12_MemoryAndIO.md](./12_MemoryAndIO.md)
- **Platform Specifications**: See [Platform Specifications](../platforms/README.md)
- **ZealZ80 Platform**: See [ZealZ80 Platform](../platforms/ZealZ80/README.md)
- **ZVB Hardware**: See [Zeal 8-bit Computer documentation](https://zeal8bit.com/docs/foreword/)
- **ZVB Video Port**: See [Zeal Video Port documentation](https://zeal8bit.com/docs/video_port/)
- **ZVB SDK**: See Zeal-VideoBoard-SDK/include/ for complete register definitions

---

**End of Intrinsics and Directives Index**
