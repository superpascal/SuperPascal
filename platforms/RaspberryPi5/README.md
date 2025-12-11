# Raspberry Pi 5 Platform — SuperPascal for Raspberry Pi 5 (Pi 500)

## Overview

**Platform:** RaspberryPi5  
**CPU:** Broadcom BCM2712 (ARM Cortex-A76 @ 2.4 GHz)  
**System:** Raspberry Pi 5 / Raspberry Pi 500  
**Status:** 🔄 **Planned** (Tier 2 Platform)

The Raspberry Pi 5 platform provides support for modern 64-bit ARM computing, suitable for Tier 2 advanced programming education. This platform offers significantly more computational power than the retro computing platforms, making it ideal for teaching modern systems programming, advanced algorithms, and real-world application development.

---

## Platform Features

### Hardware
- **CPU:** Broadcom BCM2712 SoC
  - **Cores:** 4x ARM Cortex-A76 @ 2.4 GHz
  - **Architecture:** ARMv8-A (64-bit)
  - **Instruction Set:** AArch64
- **Memory:** 4 GB or 8 GB LPDDR4X-4267 SDRAM
- **GPU:** VideoCore VII (OpenGL ES 3.1, Vulkan 1.2)
- **Storage:** 
  - MicroSD card slot (UHS-I)
  - Optional NVMe SSD via PCIe 2.0 (via M.2 HAT)
- **Connectivity:**
  - Gigabit Ethernet (with PoE+ support via HAT)
  - Dual-band 802.11ac Wi-Fi (Wi-Fi 5)
  - Bluetooth 5.0 / Bluetooth Low Energy (BLE)
- **I/O:**
  - 2x USB 3.0 ports (5 Gbps)
  - 2x USB 2.0 ports
  - 2x 4-lane MIPI camera/display interfaces
  - PCIe 2.0 interface (single lane)
  - 40-pin GPIO header (compatible with previous models)
  - 5V/5A USB-C power supply

### Software
- **OS:** Raspberry Pi OS (Debian-based Linux)
- **Boot:** UEFI-compatible bootloader
- **File System:** Standard Linux filesystems (ext4, etc.)

---

## Architecture Details

### CPU Architecture
- **ISA:** ARMv8-A (AArch64)
- **Endianness:** Little-endian (configurable, but little-endian is standard)
- **Register Width:** 64-bit
- **Address Space:** 48-bit virtual, 48-bit physical (256 TB virtual, 256 TB physical)
- **Calling Convention:** ARM64 Procedure Call Standard (AAPCS64)
- **Floating Point:** IEEE 754 compliant (hardware FPU)

### Memory Model
- **Virtual Memory:** 48-bit address space (256 TB)
- **Physical Memory:** Up to 8 GB LPDDR4X
- **Page Size:** 4 KB (standard), 16 KB, 64 KB (optional)
- **Cache:** 
  - L1 I-cache: 64 KB per core
  - L1 D-cache: 64 KB per core
  - L2 cache: 1 MB shared
  - L3 cache: None (SoC-level)

### Register Set
- **General Purpose:** 31 registers (X0-X30, 64-bit)
- **Stack Pointer:** SP (X31 alias)
- **Program Counter:** PC (64-bit)
- **SIMD/FP:** 32 registers (V0-V31, 128-bit)
- **Special:** NZCV (condition flags), FPCR, FPSR

---

## Documentation Structure

### [ABI.md](./ABI.md) (To Be Created)
Application Binary Interface specification:
- ARM64 Procedure Call Standard (AAPCS64)
- Register usage conventions
- Stack frame layout
- Parameter passing (value, var, const)
- Return value conventions
- Record and class memory layout
- VTable structure
- Exception frame layout (Linux ABI)
- ELF object file format

### [intrinsics/](./intrinsics/) (To Be Created)
Platform-specific intrinsics:
- GPIO access
- I2C/SPI/UART interfaces
- VideoCore GPU access (future)
- Hardware timers
- Interrupt control
- Memory-mapped I/O

### [runtime/](./runtime/) (To Be Created)
Runtime specification:
- Linux system call interface
- Standard C library integration
- Threading support
- Memory management
- Exception handling (Linux signals)

### [stdlib/](./stdlib/) (To Be Created)
Platform-specific standard library units:
- `RPi5_GPIO` - GPIO pin control
- `RPi5_I2C` - I2C bus interface
- `RPi5_SPI` - SPI bus interface
- `RPi5_UART` - Serial communication
- `RPi5_System` - System-level operations

---

## Quick Start

### Hello World

```pascal
program HelloWorld;
uses System;

begin
  WriteLn('Hello, Raspberry Pi 5!');
end.
```

### GPIO Example (Future)

```pascal
program GPIOExample;
uses System, RPi5_GPIO;

begin
  // Configure GPIO pin 18 as output
  RPi5_GPIO_SetMode(18, GPIO_OUTPUT);
  
  // Set pin high
  RPi5_GPIO_Write(18, true);
  
  // Wait
  Delay(1000);
  
  // Set pin low
  RPi5_GPIO_Write(18, false);
end.
```

---

## Compiler Usage

```bash
# Compile for Raspberry Pi 5 platform
spc --platform=raspberrypi5 program.pas

# Cross-compile from x86_64
spc --platform=raspberrypi5 --target=aarch64-linux-gnu program.pas

# Generate assembly
spc --platform=raspberrypi5 -S program.pas

# With optimizations
spc --platform=raspberrypi5 -O2 program.pas
```

---

## Platform-Specific Constants

```pascal
const
  // GPIO Pin Numbers
  RPI5_GPIO_PIN_2  = 2;
  RPI5_GPIO_PIN_3  = 3;
  // ... (all 40 GPIO pins)
  
  // GPIO Modes
  GPIO_INPUT       = 0;
  GPIO_OUTPUT      = 1;
  GPIO_ALT_FUNC    = 2;
  
  // I2C Buses
  RPI5_I2C_BUS_0   = 0;
  RPI5_I2C_BUS_1   = 1;
  
  // SPI Buses
  RPI5_SPI_BUS_0   = 0;
  RPI5_SPI_BUS_1   = 1;
```

---

## Memory Map

```
Virtual Address Space (48-bit, Linux):
- 0x0000000000000000 - 0x00007FFFFFFFFFFF: User space (128 TB)
- 0x0000800000000000 - 0x0000FFFFFFFFFFFF: Kernel space (128 TB)

Physical Memory:
- 0x00000000 - 0x1FFFFFFF: Up to 512 MB (model-dependent)
- 0x20000000 - 0x3FFFFFFF: Peripheral registers (1 GB)
- 0x40000000 - 0xFFFFFFFF: Extended memory (model-dependent)
```

---

## Standard Library Units

Platform-specific standard library units:

- `RPi5_GPIO` - GPIO pin control
- `RPi5_I2C` - I2C bus interface
- `RPi5_SPI` - SPI bus interface
- `RPi5_UART` - Serial communication
- `RPi5_System` - System-level operations
- `Linux` - Linux system call interface (future)

---

## Educational Value (Tier 2)

The Raspberry Pi 5 platform is ideal for Tier 2 education because:

1. **Modern Architecture:** 64-bit ARM is the dominant mobile/server architecture
2. **Real-World Applications:** Students can build actual applications
3. **Rich Ecosystem:** Vast hardware and software ecosystem
4. **Performance:** Enough power for advanced algorithms and data structures
5. **Industry Relevance:** ARM64 is used in servers, mobile devices, embedded systems

### Tier 2 Use Cases
- Advanced algorithms and data structures
- Systems programming (Linux system calls)
- Network programming
- Multi-threading and concurrency
- Hardware interfacing (GPIO, I2C, SPI)
- Real-world application development

---

## References

### Bare-Metal ARM64 Examples Available

**Local SDK Location:** `/Users/casibbald/Workspace/casibbald/SuperPascal/RaspberryPi/spectrum4/`

**Spectrum +4 Project** - Complete bare-metal ARM64 kernel for Raspberry Pi:
- **kernel/entry.s** - **Exception vectors and kernel entry/exit macros showing ARM64 register usage!**
  - `kernel_entry` macro - Shows how to save all 31 general-purpose registers (X0-X30)
  - `kernel_exit` macro - Shows how to restore all registers
  - Exception vector table setup
- **kernel/kernel.s** - Kernel initialization and ARM64 register setup
  - Shows ARM64 system register initialization
  - Shows MMU setup patterns
- **kernel/macros.s** - ARM64 assembly macros
- **kernel/uart.s** - UART driver implementation (I/O example)
  - Shows GPIO configuration
  - Shows UART register access
- **kernel/timer.s** - Timer driver implementation (interrupt example)
  - Shows ARM64 timer register usage
- **kernel/irq.s** - IRQ handling implementation
  - Shows interrupt controller setup (BCM283x and BCM2711)
- **kernel/mmu.s** - MMU setup
  - Shows ARM64 page table setup
- **kernel/spectrum4.ld** - Linker script example
  - Shows ARM64 linker script format

**Note:** This is a Spectrum +4 emulator/OS written in ARM64 assembly, providing excellent examples of bare-metal ARM64 programming on Raspberry Pi. While it targets Raspberry Pi 3/4, the ARM64 patterns apply to Raspberry Pi 5.

### Online References

- [Raspberry Pi 5 Product Page](https://www.raspberrypi.com/products/raspberry-pi-5/)
- [Raspberry Pi 5 Product Brief](https://datasheets.raspberrypi.com/rpi5/raspberry-pi-5-product-brief.pdf)
- [Raspberry Pi Documentation](https://www.raspberrypi.com/documentation/computers/raspberry-pi.html)
- [ARM Architecture Reference Manual](https://developer.arm.com/documentation/ddi0487/latest/)
- [ARM64 Procedure Call Standard](https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst)

### Key Files for ABI/Intrinsics Development

**For ABI (Calling Conventions):**
- `spectrum4/kernel/entry.s` - `kernel_entry`/`kernel_exit` macros showing register save/restore patterns
- `spectrum4/kernel/kernel.s` - ARM64 register initialization
- FPC `aarch64/cpupara.pas` - FPC calling conventions

**For Intrinsics:**
- `spectrum4/kernel/uart.s` - UART driver (I/O example, GPIO configuration)
- `spectrum4/kernel/timer.s` - Timer driver (interrupt example)
- `spectrum4/kernel/irq.s` - IRQ handling (interrupt controller)

---

## See Also

- [Platform Overview](../README.md) - All supported platforms
- [Language Specification](../../languageSpecification/00_Overview.md) - Platform-agnostic language core
- [Multi-Platform Architecture](../../MULTI_PLATFORM_ARCHITECTURE.md) - Architecture details
- [Target Architectures](../../docs/TARGET_ARCHITECTURES.md) - Complete architecture inventory

---

**Platform Maintainer:** SuperPascal Team  
**Last Updated:** 2024  
**Tier:** Tier 2 (Advanced)

