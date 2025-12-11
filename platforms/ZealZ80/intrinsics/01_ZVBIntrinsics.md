# ZealZ80 Platform — ZVB (Zeal Video Board) Intrinsics

**Platform:** ZealZ80 (Zeal 8-bit Computer)  
**Part of:** [ZealZ80 Platform Intrinsics](../README.md)

---

Based on the [Zeal Video Board SDK](https://zeal8bit.com/docs/video_port/) and hardware specification, SuperPascal provides comprehensive intrinsics for direct ZVB hardware control.

### 2A.1 ZVB Register Access

#### ReadReg

**Syntax:**
```pascal
function ReadReg(Reg: byte): byte;
```

**Purpose**: Read from ZVB I/O register.

**Parameters:**
- `Reg`: I/O register address (0x80-0xAF range)

**Returns**: Byte value from register

**Codegen**: Z80 `IN A, (port)` instruction.

**Usage:**
```pascal
var status: byte;
status := ReadReg($90);  // Read from ZVB control base
```

#### WriteReg

**Syntax:**
```pascal
procedure WriteReg(Reg: byte; Value: byte);
```

**Purpose**: Write to ZVB I/O register.

**Parameters:**
- `Reg`: I/O register address (0x80-0xAF range)
- `Value`: Byte value to write

**Codegen**: Z80 `OUT (port), A` instruction.

**Usage:**
```pascal
WriteReg($90, $80);  // Write to ZVB control register
```

**Register Base Addresses:**
```pascal
const
  ZVB_CONFIG_BASE = $80;  // Config/mapper registers
  ZVB_CTRL_BASE   = $90;  // Control/status registers (non-banked)
  ZVB_PERI_BASE   = $A0;  // Peripheral registers (banked)
```

### 2A.2 ZVB Configuration

#### ZVB_MapPeripheral

**Syntax:**
```pascal
procedure ZVB_MapPeripheral(Periph: byte);
```

**Purpose**: Map peripheral to banked I/O space (0xA0-0xAF).

**Parameters:**
- `Periph`: Peripheral index (ZVB_PERI_TEXT_IDX, ZVB_PERI_SPI_IDX, etc.)

**Peripheral Indices:**
```pascal
const
  ZVB_PERI_TEXT_IDX  = 0;  // Text controller
  ZVB_PERI_SPI_IDX   = 1;  // SPI controller
  ZVB_PERI_CRC_IDX   = 2;  // CRC32 controller
  ZVB_PERI_SOUND_IDX = 3;  // Sound controller
  ZVB_PERI_DMA_IDX   = 4;  // DMA controller
```

**Usage:**
```pascal
ZVB_MapPeripheral(ZVB_PERI_TEXT_IDX);  // Map text controller
```

### 2A.3 Video Mode Control

#### ZVB_SetVideoMode

**Syntax:**
```pascal
procedure ZVB_SetVideoMode(Mode: byte);
```

**Purpose**: Set video mode (takes effect at next VBlank).

**Parameters:**
- `Mode`: Video mode constant

**Video Modes:**
```pascal
const
  ZVB_MODE_TEXT_640      = 0;  // Text mode 640x480
  ZVB_MODE_TEXT_320      = 1;  // Text mode 320x240
  ZVB_MODE_BITMAP_256    = 2;  // Bitmap 256-color
  ZVB_MODE_BITMAP_320    = 3;  // Bitmap 320-color
  ZVB_MODE_GFX_640_8BIT  = 4;  // GFX mode 640x480 8bpp
  ZVB_MODE_GFX_320_8BIT  = 5;  // GFX mode 320x240 8bpp
  ZVB_MODE_GFX_640_4BIT  = 6;  // GFX mode 640x480 4bpp
  ZVB_MODE_GFX_320_4BIT  = 7;  // GFX mode 320x240 4bpp
```

**Usage:**
```pascal
ZVB_SetVideoMode(ZVB_MODE_GFX_320_8BIT);
```

#### ZVB_EnableScreen

**Syntax:**
```pascal
procedure ZVB_EnableScreen(Enable: boolean);
```

**Purpose**: Enable or disable screen display.

**Parameters:**
- `Enable`: `true` to enable, `false` to disable (black screen)

**Usage:**
```pascal
ZVB_EnableScreen(true);   // Enable display
ZVB_EnableScreen(false);  // Disable (black screen)
```

### 2A.4 Raster Position

#### ZVB_GetVPos

**Syntax:**
```pascal
function ZVB_GetVPos: word;
```

**Purpose**: Get current vertical raster position (latched on LSB read).

**Returns**: Vertical position (0-479 for 640 modes, 0-239 for 320 modes)

**Usage:**
```pascal
var vpos: word;
vpos := ZVB_GetVPos;  // Get vertical position
```

#### ZVB_GetHPos

**Syntax:**
```pascal
function ZVB_GetHPos: word;
```

**Purpose**: Get current horizontal raster position (latched on LSB read).

**Returns**: Horizontal position (0-639 for 640 modes, 0-319 for 320 modes)

**Usage:**
```pascal
var hpos: word;
hpos := ZVB_GetHPos;  // Get horizontal position
```

### 2A.5 Scrolling Control

#### ZVB_SetLayer0Scroll

**Syntax:**
```pascal
procedure ZVB_SetLayer0Scroll(X, Y: word);
```

**Purpose**: Set Layer 0 scrolling (GFX modes only, latched on MSB write).

**Parameters:**
- `X`: Horizontal scroll value
- `Y`: Vertical scroll value

**Usage:**
```pascal
ZVB_SetLayer0Scroll(100, 50);  // Scroll layer 0
```

#### ZVB_SetLayer1Scroll

**Syntax:**
```pascal
procedure ZVB_SetLayer1Scroll(X, Y: word);
```

**Purpose**: Set Layer 1 scrolling (GFX modes only, latched on MSB write).

**Parameters:**
- `X`: Horizontal scroll value
- `Y`: Vertical scroll value

**Usage:**
```pascal
ZVB_SetLayer1Scroll(0, 0);  // Reset layer 1 scroll
```

### 2A.6 Video Status

#### ZVB_IsHBlank

**Syntax:**
```pascal
function ZVB_IsHBlank: boolean;
```

**Purpose**: Check if currently in horizontal blank.

**Returns**: `true` if in H-blank, `false` otherwise

**Usage:**
```pascal
if ZVB_IsHBlank then
  // Safe to update video memory
```

#### ZVB_IsVBlank

**Syntax:**
```pascal
function ZVB_IsVBlank: boolean;
```

**Purpose**: Check if currently in vertical blank.

**Returns**: `true` if in V-blank, `false` otherwise

**Usage:**
```pascal
if ZVB_IsVBlank then
  // Safe to update video registers
```

### 2A.7 Sprite Control (Extended)

#### ZVB_SpriteSetFull

**Syntax:**
```pascal
procedure ZVB_SpriteSetFull(Index: byte; X, Y: word; Tile: byte; Flags: byte);
```

**Purpose**: Set complete sprite attributes (128 sprites available).

**Parameters:**
- `Index`: Sprite index (0-127)
- `X`, `Y`: Screen coordinates (X-16, Y-16 format: write 16 to show at top-left)
- `Tile`: Tile index (0-255, bit 9 in flags for 4bpp)
- `Flags`: Sprite flags (palette, flip, behind foreground, etc.)

**Sprite Flags:**
```pascal
const
  SPRITE_FLAG_TILE_BIT9  = $01;  // 9th bit of tile index (4bpp mode)
  SPRITE_FLAG_BEHIND_FG  = $02;  // Render behind foreground layer
  SPRITE_FLAG_FLIP_Y     = $04;  // Flip vertically
  SPRITE_FLAG_FLIP_X     = $08;  // Flip horizontally
  // Upper nibble (bits 4-7): Palette index (0-15) for 4bpp mode
```

**Usage:**
```pascal
ZVB_SpriteSetFull(0, 100, 50, 42, SPRITE_FLAG_FLIP_X);
```

#### ZVB_SpriteSetX

**Syntax:**
```pascal
procedure ZVB_SpriteSetX(Index: byte; X: word);
```

**Purpose**: Set sprite X coordinate only.

**Usage:**
```pascal
ZVB_SpriteSetX(0, 100);  // Move sprite 0 to X=100
```

#### ZVB_SpriteSetY

**Syntax:**
```pascal
procedure ZVB_SpriteSetY(Index: byte; Y: word);
```

**Purpose**: Set sprite Y coordinate only.

**Usage:**
```pascal
ZVB_SpriteSetY(0, 50);  // Move sprite 0 to Y=50
```

#### ZVB_SpriteSetTile

**Syntax:**
```pascal
procedure ZVB_SpriteSetTile(Index: byte; Tile: byte);
```

**Purpose**: Set sprite tile index only.

**Usage:**
```pascal
ZVB_SpriteSetTile(0, 42);  // Change sprite 0 tile
```

#### ZVB_SpriteSetFlags

**Syntax:**
```pascal
procedure ZVB_SpriteSetFlags(Index: byte; Flags: byte);
```

**Purpose**: Set sprite flags only.

**Usage:**
```pascal
ZVB_SpriteSetFlags(0, SPRITE_FLAG_FLIP_X or SPRITE_FLAG_FLIP_Y);
```

### 2A.8 Video Memory Access

Based on the [ZVB hardware specification](https://zeal8bit.com/docs/video_port/), video memory starts at physical address 0x100000. The following intrinsics provide access to video memory regions:

#### ZVB_GetVideoMemBase

**Syntax:**
```pascal
function ZVB_GetVideoMemBase: dword;
```

**Purpose**: Get physical base address of video memory.

**Returns**: Physical address (0x100000)

**Note**: This is a physical address. Use MMU mapping to access from logical address space.

**Video Memory Layout:**
```pascal
const
  VID_MEM_PHYS_BASE     = $100000;  // Physical base address
  VID_MEM_LAYER0_OFFSET = $0000;    // Layer 0 tilemap
  VID_MEM_PALETTE_OFFSET = $0E00;   // Palette RAM (256 colors, RGB565)
  VID_MEM_LAYER1_OFFSET = $1000;    // Layer 1 tilemap
  VID_MEM_SPRITE_OFFSET = $2800;    // Sprite attribute table
  VID_MEM_FONT_OFFSET   = $3000;    // Font data
  VID_MEM_TILESET_OFFSET = $10000;  // Tileset data
```

### 2A.9 Text Controller

#### ZVB_TextPrintChar

**Syntax:**
```pascal
procedure ZVB_TextPrintChar(Ch: char);
```

**Purpose**: Print character at current cursor position (text modes only).

**Parameters:**
- `Ch`: Character to print (from font table)

**Usage:**
```pascal
ZVB_TextPrintChar('A');
```

#### ZVB_TextSetCursor

**Syntax:**
```pascal
procedure ZVB_TextSetCursor(X, Y: byte);
```

**Purpose**: Set text cursor position.

**Parameters:**
- `X`: Column (0-79 for 640 mode, 0-39 for 320 mode)
- `Y`: Row (0-59 for 640 mode, 0-29 for 320 mode)

**Usage:**
```pascal
ZVB_TextSetCursor(10, 5);  // Set cursor to column 10, row 5
```

#### ZVB_TextSetColor

**Syntax:**
```pascal
procedure ZVB_TextSetColor(Foreground, Background: byte);
```

**Purpose**: Set text color (uses first 16 palette entries).

**Parameters:**
- `Foreground`: Foreground color index (0-15)
- `Background`: Background color index (0-15)

**Usage:**
```pascal
ZVB_TextSetColor(15, 0);  // White on black
```

### 2A.10 DMA Operations

#### ZVB_DMAStart

**Syntax:**
```pascal
procedure ZVB_DMAStart(DescAddr: word);
```

**Purpose**: Start DMA transfer using descriptor.

**Parameters:**
- `DescAddr`: Address of DMA descriptor structure

**DMA Descriptor Structure:**
```pascal
type
  TDMADescriptor = record
    RdAddrLo: word;   // Source address low
    RdAddrHi: byte;   // Source address high
    WrAddrLo: word;   // Destination address low
    WrAddrHi: byte;  // Destination address high
    Length: word;     // Transfer length in bytes
    Flags: byte;      // Descriptor flags
    Reserved: array[0..2] of byte;  // Padding
  end;
```

**Usage:**
```pascal
var desc: TDMADescriptor;
// ... fill descriptor ...
ZVB_DMAStart(word(@desc));  // Start DMA transfer
```

#### ZVB_DMA_VirtToPhys

**Syntax:**
```pascal
function ZVB_DMA_VirtToPhys(Ptr: pointer): dword;
```

**Purpose**: Convert virtual (logical) address to physical address for DMA.

**Parameters:**
- `Ptr`: Virtual address pointer

**Returns**: Physical address (32-bit)

**Note**: Required because DMA operates on physical addresses, not MMU-mapped logical addresses.

**Usage:**
```pascal
var physAddr: dword;
physAddr := ZVB_DMA_VirtToPhys(@Buffer);
```

#### ZVB_DMA_SetRead

**Syntax:**
```pascal
procedure ZVB_DMA_SetRead(var Desc: TDMADescriptor; Addr: dword);
```

**Purpose**: Set source address in DMA descriptor.

**Parameters:**
- `Desc`: DMA descriptor (modified)
- `Addr`: Physical source address

**Usage:**
```pascal
ZVB_DMA_SetRead(desc, $100000);  // Set source to video memory
```

#### ZVB_DMA_SetReadVirt

**Syntax:**
```pascal
procedure ZVB_DMA_SetReadVirt(var Desc: TDMADescriptor; Ptr: pointer);
```

**Purpose**: Set source address from virtual pointer (converts automatically).

**Parameters:**
- `Desc`: DMA descriptor (modified)
- `Ptr`: Virtual address pointer

**Usage:**
```pascal
ZVB_DMA_SetReadVirt(desc, @SourceBuffer);
```

#### ZVB_DMA_SetWrite

**Syntax:**
```pascal
procedure ZVB_DMA_SetWrite(var Desc: TDMADescriptor; Addr: dword);
```

**Purpose**: Set destination address in DMA descriptor.

**Parameters:**
- `Desc`: DMA descriptor (modified)
- `Addr`: Physical destination address

**Usage:**
```pascal
ZVB_DMA_SetWrite(desc, $100000);  // Set destination to video memory
```

#### ZVB_DMA_SetWriteVirt

**Syntax:**
```pascal
procedure ZVB_DMA_SetWriteVirt(var Desc: TDMADescriptor; Ptr: pointer);
```

**Purpose**: Set destination address from virtual pointer (converts automatically).

**Parameters:**
- `Desc`: DMA descriptor (modified)
- `Ptr`: Virtual address pointer

**Usage:**
```pascal
ZVB_DMA_SetWriteVirt(desc, @DestBuffer);
```

#### ZVB_DMA_PrepareDescriptor

**Syntax:**
```pascal
function ZVB_DMA_PrepareDescriptor(var Desc: TDMADescriptor; 
                                   ReadAddr, WriteAddr: dword; 
                                   Length: word; 
                                   Flags: byte): boolean;
```

**Purpose**: Prepare complete DMA descriptor.

**Parameters:**
- `Desc`: DMA descriptor (modified)
- `ReadAddr`: Physical source address
- `WriteAddr`: Physical destination address
- `Length`: Transfer length in bytes
- `Flags`: Descriptor flags

**Returns**: `true` on success, `false` on error

**DMA Flags:**
```pascal
const
  ZVB_DMA_OP_INC = 0;  // Increment address
  ZVB_DMA_OP_DEC = 1;  // Decrement address
  ZVB_DMA_DESC_LAST = 1;  // Last descriptor in chain
```

**Usage:**
```pascal
var desc: TDMADescriptor;
var success: boolean;
success := ZVB_DMA_PrepareDescriptor(desc, $100000, $200000, 1024, 0);
if success then
  ZVB_DMAStart(word(@desc));
```

### 2A.11 ZVB Constants Summary

**I/O Register Bases:**
```pascal
const
  ZVB_CONFIG_BASE = $80;  // Config/mapper (16 registers)
  ZVB_CTRL_BASE   = $90;  // Control/status (non-banked)
  ZVB_PERI_BASE   = $A0;  // Peripherals (banked, 16 registers)
```

**Video Memory Physical Addresses:**
```pascal
const
  VID_MEM_BASE        = $100000;  // Physical base
  VID_MEM_LAYER0      = $100000;  // Layer 0
  VID_MEM_PALETTE     = $100E00;  // Palette (256 colors)
  VID_MEM_LAYER1      = $101000;  // Layer 1
  VID_MEM_SPRITE      = $102800;  // Sprite table
  VID_MEM_FONT        = $103000;  // Font data
  VID_MEM_TILESET     = $110000;  // Tileset data
```

**Note**: Video memory is at physical address 0x100000-0x1FFFFF (1MB). Access requires MMU mapping to logical address space. The first 1MB of physical address space (0x000000-0x0FFFFF) is reserved for ROM (0x000000-0x07FFFF) and RAM (0x080000-0x0FFFFF).

### 2A.12 SPI Controller

The SPI controller is used for communication with external devices, primarily the TF (SD) card.

#### ZVB_SPI_Initialize

**Syntax:**
```pascal
procedure ZVB_SPI_Initialize(Reset: boolean);
```

**Purpose**: Initialize SPI peripheral and map to I/O space.

**Parameters:**
- `Reset`: If `true`, reset the SPI controller

**Usage:**
```pascal
ZVB_SPI_Initialize(false);  // Initialize without reset
```

#### ZVB_SPI_Start

**Syntax:**
```pascal
procedure ZVB_SPI_Start;
```

**Purpose**: Start SPI transaction.

**Usage:**
```pascal
ZVB_SPI_Start;  // Begin SPI transfer
```

#### ZVB_SPI_IsIdle

**Syntax:**
```pascal
function ZVB_SPI_IsIdle: boolean;
```

**Purpose**: Check if SPI controller is idle.

**Returns**: `true` if idle, `false` if transfer in progress

**Usage:**
```pascal
while not ZVB_SPI_IsIdle do
  WaitVBlank;  // Wait for transfer to complete
```

#### ZVB_SPI_CS_Start

**Syntax:**
```pascal
procedure ZVB_SPI_CS_Start;
```

**Purpose**: Assert chip select (CS goes low).

**Usage:**
```pascal
ZVB_SPI_CS_Start;  // Select device
```

#### ZVB_SPI_CS_Stop

**Syntax:**
```pascal
procedure ZVB_SPI_CS_Stop;
```

**Purpose**: Deassert chip select (CS goes high).

**Usage:**
```pascal
ZVB_SPI_CS_Stop;  // Deselect device
```

#### ZVB_SPI_SetClockDiv

**Syntax:**
```pascal
procedure ZVB_SPI_SetClockDiv(Divider: byte);
```

**Purpose**: Set SPI clock divider. Output frequency = 50/(2*div) MHz.

**Parameters:**
- `Divider`: Clock divider value

**Usage:**
```pascal
ZVB_SPI_SetClockDiv(10);  // Set clock divider
```

#### ZVB_SPI_WriteByte

**Syntax:**
```pascal
procedure ZVB_SPI_WriteByte(Value: byte);
```

**Purpose**: Write byte to SPI FIFO.

**Usage:**
```pascal
ZVB_SPI_WriteByte($FF);  // Write byte
```

#### ZVB_SPI_ReadByte

**Syntax:**
```pascal
function ZVB_SPI_ReadByte: byte;
```

**Purpose**: Read byte from SPI FIFO.

**Returns**: Byte value read

**Usage:**
```pascal
var data: byte;
data := ZVB_SPI_ReadByte;
```

#### ZVB_SPI_SetLength

**Syntax:**
```pascal
procedure ZVB_SPI_SetLength(Length: byte; ResetIndex: boolean);
```

**Purpose**: Set number of bytes for SPI transaction.

**Parameters:**
- `Length`: Number of bytes (0-7, uses bits 0-2)
- `ResetIndex`: If `true`, reset FIFO index (uses bit 7)

**Usage:**
```pascal
ZVB_SPI_SetLength(4, true);  // 4 bytes, reset index
```

**SPI Constants:**
```pascal
const
  ZVB_SPI_CTRL_START_BIT    = 7;  // Start transaction
  ZVB_SPI_CTRL_RESET_BIT    = 6;  // Reset controller
  ZVB_SPI_CTRL_CS_START_BIT = 5;  // CS low
  ZVB_SPI_CTRL_CS_STOP_BIT  = 4;  // CS high
  ZVB_SPI_CTRL_CS_BIT       = 3;  // CS select (0=TF Card)
  ZVB_SPI_CTRL_IDLE_BIT     = 0;  // Idle status
  ZVB_SPI_ARRAY_LEN         = 8;  // FIFO array length
```

### 2A.13 CRC32 Controller

The CRC32 controller calculates CRC32 checksums using polynomial 0x04C11DB7.

#### ZVB_CRC_Initialize

**Syntax:**
```pascal
procedure ZVB_CRC_Initialize(Reset: boolean);
```

**Purpose**: Initialize CRC32 peripheral and map to I/O space.

**Parameters:**
- `Reset`: If `true`, reset CRC calculation

**Usage:**
```pascal
ZVB_CRC_Initialize(true);  // Initialize and reset
```

#### ZVB_CRC_Reset

**Syntax:**
```pascal
procedure ZVB_CRC_Reset;
```

**Purpose**: Reset CRC32 calculation.

**Usage:**
```pascal
ZVB_CRC_Reset;  // Clear CRC state
```

#### ZVB_CRC_Update

**Syntax:**
```pascal
function ZVB_CRC_Update(const Buffer; Size: word): dword;
```

**Purpose**: Update CRC32 calculation with buffer data.

**Parameters:**
- `Buffer`: Data buffer (any type)
- `Size`: Number of bytes

**Returns**: CRC32 result (32-bit value)

**Usage:**
```pascal
var crc: dword;
crc := ZVB_CRC_Update(DataBuffer, SizeOf(DataBuffer));
```

#### ZVB_CRC_GetResult

**Syntax:**
```pascal
function ZVB_CRC_GetResult: dword;
```

**Purpose**: Get current CRC32 result without updating.

**Returns**: Current CRC32 value

**Usage:**
```pascal
var crc: dword;
crc := ZVB_CRC_GetResult;
```

**CRC32 Constants:**
```pascal
const
  ZVB_CRC_CTRL_RESET_BIT = 0;  // Reset CRC calculation
  ZVB_CRC_POLY = $04C11DB7;    // CRC32 polynomial
```

### 2A.14 ZVB Sound Controller (Low-Level)

The ZVB sound controller provides 4 waveform voices and 1 sample table voice. This section covers low-level hardware access; see [Extended Audio Intrinsics](./11_ExtendedAudioIntrinsics.md) for high-level audio APIs.

#### ZVB_Sound_Initialize

**Syntax:**
```pascal
procedure ZVB_Sound_Initialize(Reset: boolean);
```

**Purpose**: Initialize sound peripheral and map to I/O space.

**Parameters:**
- `Reset`: If `true`, reset all sound voices

**Usage:**
```pascal
ZVB_Sound_Initialize(true);  // Initialize and reset
```

#### ZVB_Sound_Reset

**Syntax:**
```pascal
procedure ZVB_Sound_Reset;
```

**Purpose**: Reset all sound voices.

**Usage:**
```pascal
ZVB_Sound_Reset;
```

#### ZVB_Sound_SetFrequency

**Syntax:**
```pascal
procedure ZVB_Sound_SetFrequency(Voice: byte; Freq: word);
```

**Purpose**: Set voice frequency (voices 0-3 only).

**Parameters:**
- `Voice`: Voice index (0-3)
- `Freq`: 16-bit frequency value

**Frequency Calculation:**
- Resulting frequency (Hz) = (44091 * freq) / 65536

**Usage:**
```pascal
ZVB_Sound_SetFrequency(0, 440);  // Set voice 0 frequency
```

#### ZVB_Sound_SetWaveform

**Syntax:**
```pascal
procedure ZVB_Sound_SetWaveform(Voice: byte; Waveform: byte; DutyCycle: byte);
```

**Purpose**: Set voice waveform and duty cycle (voices 0-3 only).

**Parameters:**
- `Voice`: Voice index (0-3)
- `Waveform`: Waveform type (ZVB_SOUND_SQUARE, ZVB_SOUND_TRIANGLE, ZVB_SOUND_SAWTOOTH, ZVB_SOUND_NOISE)
- `DutyCycle`: Duty cycle for square wave (0-7, bits 5-7)

**Waveform Types:**
```pascal
const
  ZVB_SOUND_SQUARE    = 0;  // Square wave
  ZVB_SOUND_TRIANGLE  = 1;  // Triangle wave
  ZVB_SOUND_SAWTOOTH  = 2;  // Sawtooth wave
  ZVB_SOUND_NOISE     = 3;  // Noise
```

**Duty Cycle Values:**
```pascal
const
  ZVB_SOUND_DUTY_0    = 0 shl 5;  // 0%
  ZVB_SOUND_DUTY_12_5 = 1 shl 5;  // 12.5%
  ZVB_SOUND_DUTY_25   = 2 shl 5;  // 25%
  ZVB_SOUND_DUTY_37_5 = 3 shl 5;  // 37.5%
  ZVB_SOUND_DUTY_50   = 4 shl 5;  // 50%
  ZVB_SOUND_DUTY_62_5 = 5 shl 5;  // 62.5%
  ZVB_SOUND_DUTY_75   = 6 shl 5;  // 75%
  ZVB_SOUND_DUTY_87_5 = 7 shl 5;  // 87.5%
```

**Usage:**
```pascal
ZVB_Sound_SetWaveform(0, ZVB_SOUND_SQUARE, ZVB_SOUND_DUTY_50);
```

#### ZVB_Sound_SetVolume

**Syntax:**
```pascal
procedure ZVB_Sound_SetVolume(Voice: byte; Volume: byte);
```

**Purpose**: Set per-voice volume (voices 0-3 only).

**Parameters:**
- `Voice`: Voice index (0-3)
- `Volume`: Volume level (0-255, or use ZVB_SOUND_VOL_* constants)

**Volume Constants:**
```pascal
const
  ZVB_SOUND_VOL_DISABLE = $80;  // Disable (0%)
  ZVB_SOUND_VOL_25      = $00;  // 25%
  ZVB_SOUND_VOL_50      = $01;  // 50%
  ZVB_SOUND_VOL_75      = $02;  // 75%
  ZVB_SOUND_VOL_100     = $03;  // 100%
```

**Usage:**
```pascal
ZVB_Sound_SetVolume(0, ZVB_SOUND_VOL_50);  // 50% volume
```

#### ZVB_Sound_SetMasterVolume

**Syntax:**
```pascal
procedure ZVB_Sound_SetMasterVolume(Left, Right: byte);
```

**Purpose**: Set master volume for left and right channels.

**Parameters:**
- `Left`: Left channel volume (ZVB_SOUND_VOL_*)
- `Right`: Right channel volume (ZVB_SOUND_VOL_*)

**Usage:**
```pascal
ZVB_Sound_SetMasterVolume(ZVB_SOUND_VOL_100, ZVB_SOUND_VOL_100);
```

#### ZVB_Sound_SetChannels

**Syntax:**
```pascal
procedure ZVB_Sound_SetChannels(LeftVoices, RightVoices: byte);
```

**Purpose**: Assign voices to left/right channels.

**Parameters:**
- `LeftVoices`: Bitmask of voices for left channel (VOICE0, VOICE1, VOICE2, VOICE3, SAMPTAB)
- `RightVoices`: Bitmask of voices for right channel

**Voice Constants:**
```pascal
const
  ZVB_SOUND_VOICE0 = 1 shl 0;  // Voice 0
  ZVB_SOUND_VOICE1 = 1 shl 1;  // Voice 1
  ZVB_SOUND_VOICE2 = 1 shl 2;  // Voice 2
  ZVB_SOUND_VOICE3 = 1 shl 3;  // Voice 3
  ZVB_SOUND_SAMPTAB = 1 shl 7; // Sample table voice
```

**Usage:**
```pascal
ZVB_Sound_SetChannels(ZVB_SOUND_VOICE0 or ZVB_SOUND_VOICE2, 
                      ZVB_SOUND_VOICE1 or ZVB_SOUND_VOICE3);
```

#### ZVB_Sound_SetHold

**Syntax:**
```pascal
procedure ZVB_Sound_SetHold(Voices: byte; Hold: boolean);
```

**Purpose**: Hold (mute) or unhold (start) voices.

**Parameters:**
- `Voices`: Bitmask of voices to control
- `Hold`: `true` to hold (mute), `false` to unhold (start)

**Usage:**
```pascal
ZVB_Sound_SetHold(ZVB_SOUND_VOICE0, true);   // Mute voice 0
ZVB_Sound_SetHold(ZVB_SOUND_VOICE0, false); // Start voice 0
```

#### ZVB_Sound_PlaySamples

**Syntax:**
```pascal
procedure ZVB_Sound_PlaySamples(Divider: byte; Mode: byte; const Samples; Length: word);
```

**Purpose**: Play samples on sample table voice (voice 7).

**Parameters:**
- `Divider`: Sample rate divider (resulting rate = 44091/(divider+1))
- `Mode`: Sample mode (ZVB_SAMPLE_UINT8, ZVB_SAMPLE_UINT16, ZVB_SAMPLE_SINT16)
- `Samples`: Sample data buffer
- `Length`: Number of samples

**Sample Mode Constants:**
```pascal
const
  ZVB_SAMPLE_UINT8  = 1;  // Unsigned 8-bit samples
  ZVB_SAMPLE_UINT16 = 0;  // Unsigned 16-bit samples
  ZVB_SAMPLE_SINT16 = 0;  // Signed 16-bit samples (with sign bit)
```

**Usage:**
```pascal
ZVB_Sound_PlaySamples(0, ZVB_SAMPLE_UINT8, @SampleData, SizeOf(SampleData));
```

#### ZVB_Sound_IsSampleReady

**Syntax:**
```pascal
function ZVB_Sound_IsSampleReady: boolean;
```

**Purpose**: Check if sample table voice is ready (FIFO empty/all samples played).

**Returns**: `true` if ready, `false` if still playing

**Usage:**
```pascal
if ZVB_Sound_IsSampleReady then
  // Samples finished playing
```

#### ZVB_Sound_IsSampleFull

**Syntax:**
```pascal
function ZVB_Sound_IsSampleFull: boolean;
```

**Purpose**: Check if sample table FIFO is full.

**Returns**: `true` if FIFO full, `false` otherwise

**Usage:**
```pascal
if not ZVB_Sound_IsSampleFull then
  // Can write more samples
```

---

**See also:**
- [Graphics Intrinsics](./02_GraphicsIntrinsics.md)
- [Memory Intrinsics](./03_MemoryIntrinsics.md)
- [Extended Audio Intrinsics](./11_ExtendedAudioIntrinsics.md) for high-level audio APIs

