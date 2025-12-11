# SuperPascal Platform Specifications

## Overview

SuperPascal is a modern Pascal superset compiler supporting multiple retro computing platforms (8-bit, 16-bit, 32-bit) and modern platforms (64-bit ARM). This directory contains platform-specific specifications for each supported target.

**Note:** Retro computing platforms (ZealZ80, CommanderX16, Foenix65C816, FoenixA2560M) are suitable for Tier 1 and Tier 2. Modern platforms (RaspberryPi5) are Tier 2 only.

---

## Supported Platforms

### [ZealZ80](./ZealZ80/) ⭐ (Primary Platform)
- **CPU:** Zilog Z80 @ 10 MHz
- **System:** Zeal 8-bit Computer
- **Status:** Fully Specified
- **Features:**
  - ZVB (Zeal Video Board) graphics and audio
  - MMU-based memory management
  - Direct memory and I/O port access
  - Game engine intrinsics
  - Complete ABI specification

### [CommanderX16](./CommanderX16/) 🔄 (Tier 1 Platform)
- **CPU:** WDC 65C02 @ 8 MHz
- **System:** Commander X16 Retro Computer
- **Status:** Planned (Tier 1)
- **CPU Specifications:**
  - **Architecture:** 8-bit, 6502-compatible
  - **Instruction Set:** 65C02 (enhanced 6502)
  - **Clock Speed:** 8 MHz
- **Platform Features:**
  - **Graphics:** VERA (Versatile Embedded Retro Adapter)
    - 640x480 @ 60 Hz (or 320x240 @ 60 Hz)
    - 256 colors from 4096-color palette
    - 128 hardware sprites, 64 colors each
    - 2 tilemap layers with hardware scrolling
  - **Audio:** YM2151 (OPM) FM synthesis, VERA PSG, PCM playback
  - **Memory:** 512 KB RAM (banked), 32 KB ROM
  - **Storage:** SD card slot, IEC serial bus
  - **I/O:** PS/2 keyboard/mouse, 2x joystick ports, user port
  - **OS:** KERNAL (Commodore-style), X16 BASIC
- **Educational Value:** Classic 6502 architecture with modern VERA graphics
- **References:**
  - [Commander X16 Official Website](https://www.commanderx16.com/)
  - [Commander X16 Documentation](https://github.com/commanderx16/x16-docs)
  - **Local Documentation:** `/Users/casibbald/Workspace/casibbald/SuperPascal/CommandX16/x16-docs/`
    - Complete Programmer's Reference Guide with Memory Map, VERA, Sound, I/O, Hardware specs
  - [WDC 65C02 Datasheet](https://www.westerndesigncenter.com/wdc/documentation/w65c02s.pdf)

### Foenix65C816 (Planned)
- **CPU:** WDC W65C816S @ 6.29 MHz (16-bit microprocessor with 24-bit addressing, 16MB address space)
- **System:** Foenix F256x retrocomputing systems (F256Jr, F256JrJr, F256K, F256K2)
- **Status:** Planned
- **CPU Specifications:**
  - **Data width:** 8-bit external, 16-bit internal
  - **Address width:** 24 bits (16MB address space)
  - **Modes:** Emulation mode (65C02 compatible) and Native mode (16-bit)
  - **Registers:** 16-bit in native mode (A, X, Y, SP)
  - **Clock:** 6.29 MHz (6,293,750 Hz, derived from 25.175 MHz / 4)
  - **Reference:** [WDC 65C816 Wikipedia](https://en.wikipedia.org/wiki/WDC_65C816), [WDC 65C816S Datasheet](https://www.westerndesigncenter.com/wdc/documentation/w65c816s.pdf)
- **Platform Features:**
  - **Graphics:** VICKY graphics chips (TinyVicky, TinyVicky II, Vicky "The Fourth" depending on model)
  - **Audio:** PSG (FPGA emulation), SID (FPGA emulation), OPL3 (FPGA emulation or real), CODEC, PWM, SAM2695, VS1053b
  - **Memory:** 512KB-2MB SRAM (model-dependent), optional 256KB expansion, DDR3 on K2 (128MB)
  - **I/O:** IO Page architecture, PS/2 keyboard/mouse, SD card, serial ports, joystick ports, MIDI
  - **Networking:** WiFi (Wiznet or ESP32), Ethernet (Wiznet on K2)
  - **Reference:** [Foenix Hardware Wiki](https://wiki.f256foenix.com/index.php?title=Hardware) for complete hardware specifications

### Foenix65C816 (Planned)
- **Status:** Platform structure created, intrinsics planned
- **Intrinsics:** See [`platforms/Foenix65C816/intrinsics/`](./Foenix65C816/intrinsics/README.md)
- **SDK Location:** `/Users/casibbald/Workspace/casibbald/SuperPascal/Foenix/`

### FoenixA2560M (Planned)
- **CPU:** MC68LC060RC66 @ 66 MHz (32-bit Motorola 68060 variant)
- **System:** Foenix A2560M/A2560X Mini-ITX retro computer
- **Status:** Planned
- **Features:**
  - 8MB Static RAM
  - 4MB Flash memory
  - 1GB DDR3 memory
  - PCI Express (Gen 2, 4 lanes)
  - USB OTG (2x ports)
  - VICKY III Mark II graphics engine
  - Multiple audio chips (OPM, OPN2, PSG, OPL3, SID, SAM2695, VS1053B)
  - Ethernet (Wiznet W6100) and WiFi (WizFi360)
  - See [A2560M Specifications](https://c256foenix.com/a2560m/) for complete hardware specs

### [RaspberryPi5](./RaspberryPi5/) 🔄 (Tier 2 Platform)
- **CPU:** Broadcom BCM2712 (ARM Cortex-A76 @ 2.4 GHz)
- **System:** Raspberry Pi 5 / Raspberry Pi 500
- **Status:** Planned (Tier 2)
- **CPU Specifications:**
  - **Cores:** 4x ARM Cortex-A76
  - **Architecture:** ARMv8-A (64-bit)
  - **Instruction Set:** AArch64
  - **Clock Speed:** 2.4 GHz
- **Platform Features:**
  - **Memory:** 4 GB or 8 GB LPDDR4X-4267 SDRAM
  - **GPU:** VideoCore VII (OpenGL ES 3.1, Vulkan 1.2)
  - **Storage:** MicroSD card slot, optional NVMe SSD via PCIe 2.0
  - **Connectivity:** Gigabit Ethernet, Wi-Fi 5 (802.11ac), Bluetooth 5.0/BLE
  - **I/O:** 2x USB 3.0, 2x USB 2.0, 2x MIPI camera/display, PCIe 2.0, 40-pin GPIO
  - **OS:** Raspberry Pi OS (Debian-based Linux)
- **Educational Value:** Modern 64-bit ARM platform for Tier 2 advanced programming
- **References:**
  - [Raspberry Pi 5 Product Page](https://www.raspberrypi.com/products/raspberry-pi-5/)
  - [Raspberry Pi 5 Product Brief](https://datasheets.raspberrypi.com/rpi5/raspberry-pi-5-product-brief.pdf)
  - [ARM64 Procedure Call Standard](https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst)

---

## Platform Structure

Each platform directory contains:

```
{Platform}/
├── README.md          # Platform overview and quick start
├── ABI.md             # Application Binary Interface (calling conventions)
├── intrinsics/        # Platform-specific intrinsics
│   ├── README.md      # Intrinsics index
│   └── *.md           # Individual intrinsic specifications
├── gameEngine/        # Platform-specific game engine implementation
│   └── README.md      # Game engine hardware integration
├── runtime/           # Runtime specification (future)
└── stdlib/            # Platform-specific standard library units (future)
```

---

## Platform Selection

When writing SuperPascal code, platform-specific features are accessed through:

1. **Platform-specific intrinsics** - Direct hardware access
2. **Conditional compilation** - `{$IFDEF PLATFORM_NAME}`
3. **Platform-specific standard library units** - Imported via `uses` clause

### Example

```pascal
program Example;
{$IFDEF ZEALZ80}
uses ZVB_Graphics, ZVB_Audio;
{$ELSEIF DEFINED(FOENIX65C816)}
uses Foenix_Graphics, Foenix_Audio;
{$ENDIF}

begin
  {$IFDEF ZEALZ80}
    ZVB_SpriteSet(0, 100, 100, 1);
  {$ELSEIF DEFINED(FOENIX65C816)}
    Foenix_SpriteSet(0, 100, 100, 1);
  {$ENDIF}
end.
```

---

## Adding a New Platform

To add support for a new platform:

1. Create `platforms/{PlatformName}/` directory
2. Create `ABI.md` with CPU-specific calling conventions
3. Create `intrinsics/` directory with platform intrinsics
4. Implement backend in `compiler-rs/src/backend/{platform}/`
5. Add platform-specific runtime in `runtime/{platform}/`
6. Create platform-specific stdlib units in `stdlib/platforms/{platform}/`

See [MULTI_PLATFORM_ARCHITECTURE.md](../MULTI_PLATFORM_ARCHITECTURE.md) for detailed architecture.

---

## Cross-Platform Development

The core SuperPascal language specification is platform-agnostic:
- Lexical structure
- Grammar (EBNF)
- Type system
- Semantics
- Language extensions (struct syntax, etc.)

Only intrinsics, ABI, and runtime are platform-specific.

---

**See also:**
- [Language Specification](../languageSpecification/00_Overview.md) - Platform-agnostic language core
- [Multi-Platform Architecture](../MULTI_PLATFORM_ARCHITECTURE.md) - Detailed architecture proposal

