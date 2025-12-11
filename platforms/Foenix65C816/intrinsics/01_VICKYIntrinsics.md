# Foenix65C816 Platform — VICKY Graphics Intrinsics

**Platform:** Foenix65C816 (Foenix F256x systems)  
**Part of:** [Foenix65C816 Platform Intrinsics](./README.md)

---

Based on the [Foenix MicroKernel TinyVicky definitions](../../../../SuperPascal/Foenix/f256-microkernel/f256/TinyVicky_Def.asm) and hardware specification, SuperPascal provides comprehensive intrinsics for direct VICKY graphics chip hardware control.

**Note:** VICKY chip variants differ by model:
- **F256Jr, F256K:** TinyVicky
- **F256JrJr (Jr2):** TinyVicky II  
- **F256K2:** Vicky "The Fourth"

Intrinsics abstract these differences where possible, with platform-specific variants where necessary.

---

## 1. VICKY Register Access

### VICKY_ReadReg

**Syntax:**
```pascal
function VICKY_ReadReg(Reg: word): byte;
```

**Purpose**: Read from VICKY I/O register.

**Parameters:**
- `Reg`: I/O register address (0xD000-0xDFFF range, IO Page 0)

**Returns**: Byte value from register

**Codegen**: 65C816 memory-mapped I/O read.

**Usage:**
```pascal
var status: byte;
status := VICKY_ReadReg($D000);  // Read master control register
```

### VICKY_WriteReg

**Syntax:**
```pascal
procedure VICKY_WriteReg(Reg: word; Value: byte);
```

**Purpose**: Write to VICKY I/O register.

**Parameters:**
- `Reg`: I/O register address (0xD000-0xDFFF range, IO Page 0)
- `Value`: Byte value to write

**Codegen**: 65C816 memory-mapped I/O write.

**Usage:**
```pascal
VICKY_WriteReg($D000, $01);  // Enable text mode
```

**Register Base Addresses:**
```pascal
const
  VICKY_BASE = $D000;  // VICKY registers start at $D000 (IO Page 0)
```

---

## 2. Master Control Registers

### VICKY_SetMasterControl

**Syntax:**
```pascal
procedure VICKY_SetMasterControl(Flags: byte);
```

**Purpose**: Set master control register flags (enables/disables video features).

**Parameters:**
- `Flags`: Bit flags for video features

**Control Flags:**
```pascal
const
  VICKY_CTRL_TEXT_MODE_EN  = $01;  // Enable Text Mode
  VICKY_CTRL_TEXT_OVERLAY  = $02;  // Enable Text Overlay on Graphics
  VICKY_CTRL_GRAPH_MODE_EN = $04;  // Enable Graphic Mode
  VICKY_CTRL_BITMAP_EN     = $08;  // Enable Bitmap Module
  VICKY_CTRL_TILEMAP_EN    = $10;  // Enable Tile Module
  VICKY_CTRL_SPRITE_EN     = $20;  // Enable Sprite Module
  VICKY_CTRL_GAMMA_EN      = $40;  // Enable GAMMA correction
  VICKY_CTRL_DISABLE_VID   = $80;  // Disable video scanning (100% CPU bandwidth)
```

**Usage:**
```pascal
// Enable text mode and graphics mode simultaneously
VICKY_SetMasterControl(VICKY_CTRL_TEXT_MODE_EN or VICKY_CTRL_GRAPH_MODE_EN);

// Enable sprites and tilemap
VICKY_SetMasterControl(VICKY_CTRL_SPRITE_EN or VICKY_CTRL_TILEMAP_EN);
```

### VICKY_GetMasterControl

**Syntax:**
```pascal
function VICKY_GetMasterControl: byte;
```

**Purpose**: Read current master control register value.

**Returns**: Current master control flags

---

## 3. Border Control

### VICKY_SetBorderControl

**Syntax:**
```pascal
procedure VICKY_SetBorderControl(Enable: boolean; XScroll: byte);
```

**Purpose**: Configure border display and horizontal scroll offset.

**Parameters:**
- `Enable`: Enable border (default: true)
- `XScroll`: Horizontal scroll offset (0-7, scrolls left)

**Usage:**
```pascal
VICKY_SetBorderControl(true, 0);  // Enable border, no scroll
```

### VICKY_SetBorderColor

**Syntax:**
```pascal
procedure VICKY_SetBorderColor(R, G, B: byte);
```

**Purpose**: Set border color (RGB).

**Parameters:**
- `R`: Red component (0-255)
- `G`: Green component (0-255)
- `B`: Blue component (0-255)

**Usage:**
```pascal
VICKY_SetBorderColor(255, 0, 0);  // Red border
```

### VICKY_SetBorderSize

**Syntax:**
```pascal
procedure VICKY_SetBorderSize(XSize, YSize: byte);
```

**Purpose**: Set border size in pixels.

**Parameters:**
- `XSize`: Horizontal border size (0-32, default: 32)
- `YSize`: Vertical border size (0-32, default: 32)

**Usage:**
```pascal
VICKY_SetBorderSize(32, 32);  // Full border
```

---

## 4. Background Color (Graphics Mode)

### VICKY_SetBackgroundColor

**Syntax:**
```pascal
procedure VICKY_SetBackgroundColor(R, G, B: byte);
```

**Purpose**: Set background color for graphics mode (used when pixel value is 0).

**Parameters:**
- `R`: Red component (0-255)
- `G`: Green component (0-255)
- `B`: Blue component (0-255)

**Usage:**
```pascal
VICKY_SetBackgroundColor(0, 0, 0);  // Black background
```

---

## 5. Text Mode Control

### VICKY_TextEnable

**Syntax:**
```pascal
procedure VICKY_TextEnable(Enable: boolean);
```

**Purpose**: Enable or disable text mode.

**Parameters:**
- `Enable`: Enable text mode

### VICKY_TextSetStartAddress

**Syntax:**
```pascal
procedure VICKY_TextSetStartAddress(Offset: byte);
```

**Purpose**: Set starting address offset for text mode buffer (horizontal offset).

**Parameters:**
- `Offset`: Starting address offset in X direction

---

## 6. Cursor Control

### VICKY_CursorEnable

**Syntax:**
```pascal
procedure VICKY_CursorEnable(Enable: boolean);
```

**Purpose**: Enable or disable text cursor.

**Parameters:**
- `Enable`: Enable cursor

### VICKY_CursorSetFlashRate

**Syntax:**
```pascal
procedure VICKY_CursorSetFlashRate(Rate0, Rate1: boolean);
```

**Purpose**: Set cursor flash rate.

**Parameters:**
- `Rate0`: Flash rate bit 0
- `Rate1`: Flash rate bit 1

### VICKY_CursorSetChar

**Syntax:**
```pascal
procedure VICKY_CursorSetChar(Char: byte);
```

**Purpose**: Set cursor character.

**Parameters:**
- `Char`: Character code for cursor

### VICKY_CursorSetColor

**Syntax:**
```pascal
procedure VICKY_CursorSetColor(Color: byte);
```

**Purpose**: Set cursor color.

**Parameters:**
- `Color`: Color value

### VICKY_CursorSetPosition

**Syntax:**
```pascal
procedure VICKY_CursorSetPosition(X, Y: word);
```

**Purpose**: Set cursor position.

**Parameters:**
- `X`: X coordinate (16-bit)
- `Y`: Y coordinate (16-bit)

**Usage:**
```pascal
VICKY_CursorSetPosition(10, 5);  // Set cursor to column 10, row 5
```

---

## 7. Raster Position Queries

### VICKY_GetPixelX

**Syntax:**
```pascal
function VICKY_GetPixelX: word;
```

**Purpose**: Get current pixel X position on video line (read-only register).

**Returns**: Current pixel X position (16-bit)

**Note**: This register is read-only and shows the pixel being displayed when read.

### VICKY_GetLineY

**Syntax:**
```pascal
function VICKY_GetLineY: word;
```

**Purpose**: Get current raster line Y position (read-only register).

**Returns**: Current raster line Y position (16-bit)

**Usage:**
```pascal
var line: word;
line := VICKY_GetLineY;  // Get current scanline
```

---

## 8. Line Interrupt Control

### VICKY_LineInterruptEnable

**Syntax:**
```pascal
procedure VICKY_LineInterruptEnable(Enable: boolean);
```

**Purpose**: Enable line interrupt (write-only register).

**Parameters:**
- `Enable`: Enable line interrupt

### VICKY_LineInterruptSetCompare

**Syntax:**
```pascal
procedure VICKY_LineInterruptSetCompare(Line: word);
```

**Purpose**: Set line interrupt compare value (triggers interrupt at specified line).

**Parameters:**
- `Line`: Line number to trigger interrupt (12-bit: 0-4095)

**Usage:**
```pascal
VICKY_LineInterruptSetCompare(240);  // Trigger interrupt at line 240
```

**Note**: This is a write-only register. The compare value is split across two registers (low and high bytes).

---

## 9. Platform-Specific Variants

Different Foenix models use different VICKY chip variants with varying capabilities:

### TinyVicky (F256Jr, F256K)
- Basic graphics features
- Text mode
- Sprites (limited)
- Tilemap (limited)

### TinyVicky II (F256JrJr)
- Enhanced graphics features
- Improved sprite capabilities
- Enhanced tilemap support

### Vicky "The Fourth" (F256K2)
- Full feature set
- Advanced sprite capabilities
- Enhanced tilemap with multiple layers
- Additional graphics features

**Implementation Note:** The compiler should detect the target model and generate appropriate intrinsics. Platform-specific intrinsics may be named with a suffix (e.g., `VICKY_SpriteSetFull_F256K2` for K2-specific features).

---

## 10. Register Map Summary

| Address | Register | Read/Write | Description |
|---------|----------|------------|-------------|
| $D000 | MASTER_CTRL_REG_L | R/W | Master control register (low byte) |
| $D001 | MASTER_CTRL_REG_H | R/W | Master control register (high byte) |
| $D002-$D003 | Reserved | - | Reserved for future use |
| $D004 | BORDER_CTRL_REG | R/W | Border control and X scroll |
| $D005 | BORDER_COLOR_B | R/W | Border color blue |
| $D006 | BORDER_COLOR_G | R/W | Border color green |
| $D007 | BORDER_COLOR_R | R/W | Border color red |
| $D008 | BORDER_X_SIZE | R/W | Border X size (0-32) |
| $D009 | BORDER_Y_SIZE | R/W | Border Y size (0-32) |
| $D00A-$D00C | Reserved | - | Reserved for future use |
| $D00D | BACKGROUND_COLOR_B | R/W | Background color blue (graphics mode) |
| $D00E | BACKGROUND_COLOR_G | R/W | Background color green (graphics mode) |
| $D00F | BACKGROUND_COLOR_R | R/W | Background color red (graphics mode) |
| $D010 | VKY_TXT_CURSOR_CTRL_REG | R/W | Text cursor control |
| $D011 | VKY_TXT_START_ADD_PTR | R/W | Text mode start address offset |
| $D012 | VKY_TXT_CURSOR_CHAR_REG | R/W | Cursor character |
| $D013 | VKY_TXT_CURSOR_COLR_REG | R/W | Cursor color |
| $D014-$D015 | VKY_TXT_CURSOR_X_REG | R/W | Cursor X position (16-bit) |
| $D016-$D017 | VKY_TXT_CURSOR_Y_REG | R/W | Cursor Y position (16-bit) |
| $D018 | VKY_LINE_IRQ_CTRL_REG / VKY_PIXEL_X_POS_LO | W/R | Line interrupt control (write) / Pixel X position (read) |
| $D019 | VKY_LINE_CMP_VALUE_LO / VKY_PIXEL_X_POS_HI | W/R | Line compare low (write) / Pixel X position high (read) |
| $D01A | VKY_LINE_CMP_VALUE_HI / VKY_LINE_Y_POS_LO | W/R | Line compare high (write) / Line Y position low (read) |
| $D01B | VKY_LINE_Y_POS_HI | R | Line Y position high (read) |

---

## 11. Usage Examples

### Example: Initialize VICKY for Text Mode

```pascal
program TextModeDemo;
begin
  // Enable text mode
  VICKY_SetMasterControl(VICKY_CTRL_TEXT_MODE_EN);
  
  // Enable cursor
  VICKY_CursorEnable(true);
  VICKY_CursorSetPosition(0, 0);
  
  // Set border
  VICKY_SetBorderControl(true, 0);
  VICKY_SetBorderColor(64, 64, 64);  // Gray border
end.
```

### Example: Query Raster Position

```pascal
program RasterQuery;
var
  x, y: word;
begin
  // Wait for specific line
  repeat
    y := VICKY_GetLineY;
  until y >= 240;
  
  // Get pixel position
  x := VICKY_GetPixelX;
  
  // Do something at line 240, pixel x
end.
```

### Example: Line Interrupt Setup

```pascal
program LineInterruptDemo;
begin
  // Enable line interrupt at line 200
  VICKY_LineInterruptSetCompare(200);
  VICKY_LineInterruptEnable(true);
  
  // Interrupt handler will be called at line 200
  // (interrupt handling is platform-specific)
end.
```

---

## 12. Implementation Notes

1. **IO Page Architecture:** VICKY registers are in IO Page 0, starting at $D000. The compiler must ensure proper IO page mapping.

2. **Register Aliasing:** Some registers have different meanings when read vs. written (e.g., $D018-$D01A). The compiler must generate appropriate read/write operations.

3. **Model Detection:** The compiler should detect the target Foenix model and generate appropriate intrinsics for the specific VICKY variant.

4. **Performance:** Direct register access is fast. Higher-level intrinsics may add minimal overhead for convenience.

5. **Sprite/Tilemap/Bitmap:** Additional registers for sprites, tilemaps, and bitmaps are defined in extended VICKY documentation. These will be documented in separate sections as the SDK is analyzed.

---

**Reference:** [Foenix MicroKernel - TinyVicky_Def.asm](../../../../SuperPascal/Foenix/f256-microkernel/f256/TinyVicky_Def.asm)  
**Last Updated:** 2025-01-XX  
**Platform:** Foenix65C816 (WDC W65C816S @ 6.29 MHz)

