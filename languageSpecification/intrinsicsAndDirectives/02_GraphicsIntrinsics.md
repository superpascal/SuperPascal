# Graphics Intrinsics

**Part of:** [06_IntrinsicsAndDirectives.md](../06_IntrinsicsAndDirectives.md)

---

### 2.1 WaitVBlank

**Syntax:**
```pascal
procedure WaitVBlank;
```

**Purpose**: Synchronize with vertical blank interrupt.

**Behavior**: Blocks until next VBlank (60 Hz on Zeal).

**Codegen**: Calls runtime routine that waits for VBlank signal.

**Usage:**
```pascal
WaitVBlank;  // Wait for frame sync
```

### 2.2 WriteReg

**Syntax:**
```pascal
procedure WriteReg(Reg: byte; Value: byte);
```

**Purpose**: Write to ZVB (Zeal Video Board) I/O register.

**Parameters:**
- `Reg`: I/O register address (0x80-0xAF range)
- `Value`: Register value (8-bit)

**Codegen**: Z80 `OUT (port), A` instruction.

**Usage:**
```pascal
WriteReg($90, $80);  // Write to ZVB control register
```

**Note**: For ZVB-specific operations, prefer the dedicated intrinsics (e.g., `ZVB_SetVideoMode`, `ZVB_EnableScreen`) which provide type safety and clearer semantics. `WriteReg` is for low-level register access when needed.

### 2.3 DMA_Copy

**Syntax:**
```pascal
procedure DMA_Copy(const Source; var Dest; Size: word);
```

**Purpose**: Bulk memory copy via DMA engine.

**Parameters:**
- `Source`: Source address (any type)
- `Dest`: Destination address (any type)
- `Size`: Number of bytes to copy

**Codegen**: Sets up DMA command block and triggers transfer.

**Usage:**
```pascal
DMA_Copy(SpriteData, VRAM, SizeOf(SpriteData));
```

### 2.4 SpriteSet

**Syntax:**
```pascal
procedure SpriteSet(Index: byte; X, Y: integer; Tile: word);
```

**Purpose**: Configure sprite position and tile.

**Parameters:**
- `Index`: Sprite index (0-127)
- `X`, `Y`: Screen coordinates
- `Tile`: Tile index

**Codegen**: Writes to sprite attribute table.

**Usage:**
```pascal
SpriteSet(0, 100, 50, 42);  // Set sprite 0
```

### 2.5 SpriteShow

**Syntax:**
```pascal
procedure SpriteShow(Index: byte; Visible: boolean);
```

**Purpose**: Show or hide sprite.

**Parameters:**
- `Index`: Sprite index
- `Visible`: Visibility flag

**Usage:**
```pascal
SpriteShow(0, true);   // Show sprite 0
SpriteShow(0, false); // Hide sprite 0
```

---

**See also:**
- [ZVB Intrinsics](./02A_ZVBIntrinsics.md) for extended ZVB functionality

