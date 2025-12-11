# Input Intrinsics

**Part of:** [06_IntrinsicsAndDirectives.md](../06_IntrinsicsAndDirectives.md)

---

### 5.1 ReadInput

**Syntax:**
```pascal
function ReadInput: word;
```

**Purpose**: Read input state (keyboard, gamepad).

**Returns**: Input state word (bit flags).

**Usage:**
```pascal
var input: word;
input := ReadInput;
if (input and $01) <> 0 then
  // Button A pressed
```

### 5.2 ClearScreen

**Syntax:**
```pascal
procedure ClearScreen(Color: byte);
```

**Purpose**: Clear screen with color.

**Parameters:**
- `Color`: Palette color index

**Usage:**
```pascal
ClearScreen(0);  // Clear to color 0 (black)
```

---

**See also:**
- [Graphics Intrinsics](./02_GraphicsIntrinsics.md)
- [ZVB Intrinsics](./02A_ZVBIntrinsics.md)

