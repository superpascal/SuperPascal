# Polling Input

**Part of:** [Chapter 10: Input, State, and Interaction](./README.md)

---

## Introduction

Interactive programs need to respond to user input. This section teaches you how to:
- **Read input** — Get keyboard and gamepad state
- **Understand bit flags** — How input is encoded
- **Handle multiple inputs** — Check multiple buttons at once
- **Detect input changes** — Know when buttons are pressed vs held

**Key concepts:**
- **Polling** — Checking input state each frame
- **Bit flags** — Encoding multiple inputs in one value
- **Input state** — Current state of all inputs
- **Edge detection** — Detecting button presses vs holds

---

## Understanding Input

### Polling vs Events

**Two ways to handle input:**

1. **Polling** — Check input state each frame (what we use)
2. **Events** — Respond to input events (not covered here)

**Polling is simpler and more predictable:**
- Check input once per frame
- Consistent behavior
- Easy to understand
- Works well for games

**Example:**
```pascal
while true do
begin
  var input := ReadInput;  // Poll input
  HandleInput(input);      // Process input
  UpdateGame;             // Update game state
  RenderFrame;            // Draw frame
  WaitVBlank;             // Wait for next frame
end;
```

### The ReadInput Function

**ReadInput returns the current input state:**

```pascal
function ReadInput: word;
```

**Returns:** 16-bit word with bit flags for each input

**Usage:**
```pascal
var input: word;
input := ReadInput;
```

**What it reads:**
- Keyboard keys
- Gamepad buttons
- Directional pad (D-pad)
- Other input devices

---

## Understanding Bit Flags

### Binary Representation

**Input state is encoded as bits:**

```
Bit:  15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
Flag:  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?  ?
```

**Each bit represents one input:**
- `1` = pressed/active
- `0` = not pressed/inactive

**Example:**
```
Input word: $0005
Binary:     0000 0000 0000 0101
            Bit 0: Button A (pressed)
            Bit 2: Button B (pressed)
            Others: Not pressed
```

### Checking Individual Bits

**Use bitwise AND to check if a bit is set:**

```pascal
const
  BUTTON_A = $0001;  // Bit 0
  BUTTON_B = $0002;  // Bit 1
  BUTTON_X = $0004;  // Bit 2
  BUTTON_Y = $0008;  // Bit 3
  BUTTON_UP = $0010;    // Bit 4
  BUTTON_DOWN = $0020;  // Bit 5
  BUTTON_LEFT = $0040;  // Bit 6
  BUTTON_RIGHT = $0080; // Bit 7

var input: word;
input := ReadInput;

// Check if Button A is pressed
if (input and BUTTON_A) <> 0 then
  WriteLn('Button A pressed');

// Check if Button B is pressed
if (input and BUTTON_B) <> 0 then
  WriteLn('Button B pressed');
```

### Checking Multiple Bits

**Check multiple buttons at once:**

```pascal
// Check if A or B is pressed
if ((input and BUTTON_A) <> 0) or ((input and BUTTON_B) <> 0) then
  WriteLn('A or B pressed');

// Check if A and B are both pressed
if ((input and BUTTON_A) <> 0) and ((input and BUTTON_B) <> 0) then
  WriteLn('A and B both pressed');

// Check if any direction button is pressed
if (input and ($0010 or $0020 or $0040 or $0080)) <> 0 then
  WriteLn('Direction pressed');
```

---

## Standard Input Constants

### Gamepad Buttons

**Common gamepad button constants:**

```pascal
const
  // Action buttons
  BTN_A      = $0001;  // Button A (primary action)
  BTN_B      = $0002;  // Button B (secondary action)
  BTN_X      = $0004;  // Button X
  BTN_Y      = $0008;  // Button Y
  BTN_L      = $0100;  // Left shoulder button
  BTN_R      = $0200;  // Right shoulder button
  BTN_START  = $0400;  // Start button
  BTN_SELECT = $0800;  // Select button
  
  // Directional pad
  BTN_UP    = $0010;  // D-pad up
  BTN_DOWN  = $0020;  // D-pad down
  BTN_LEFT  = $0040;  // D-pad left
  BTN_RIGHT = $0080;  // D-pad right
```

### Keyboard Keys

**Common keyboard key constants:**

```pascal
const
  // Arrow keys
  KEY_UP    = $0010;  // Up arrow
  KEY_DOWN  = $0020;  // Down arrow
  KEY_LEFT  = $0040;  // Left arrow
  KEY_RIGHT = $0080;  // Right arrow
  
  // Letter keys
  KEY_A = $1000;  // A key
  KEY_B = $2000;  // B key
  KEY_C = $3000;  // C key
  // ... etc
  
  // Special keys
  KEY_SPACE = $4000;  // Spacebar
  KEY_ENTER = $8000;  // Enter key
  KEY_ESC   = $0100;  // Escape key
```

**Note:** Actual key codes depend on the platform and hardware. Check platform documentation for exact values.

---

## Reading Input in a Game Loop

### Basic Input Reading

**Read input each frame:**

```pascal
program InputDemo;

var
  input: word;
  playerX, playerY: integer;

procedure UpdatePlayer;
begin
  input := ReadInput;
  
  // Move player based on input
  if (input and BTN_LEFT) <> 0 then
    playerX := playerX - 2;
  if (input and BTN_RIGHT) <> 0 then
    playerX := playerX + 2;
  if (input and BTN_UP) <> 0 then
    playerY := playerY - 2;
  if (input and BTN_DOWN) <> 0 then
    playerY := playerY + 2;
end;

begin
  playerX := 160;
  playerY := 120;
  
  while true do
  begin
    UpdatePlayer;
    RenderPlayer(playerX, playerY);
    WaitVBlank;
  end;
end.
```

### Helper Functions

**Create helper functions for cleaner code:**

```pascal
function IsButtonPressed(input: word; button: word): boolean;
begin
  IsButtonPressed := (input and button) <> 0;
end;

function IsAnyDirectionPressed(input: word): boolean;
begin
  IsAnyDirectionPressed := (input and (BTN_UP or BTN_DOWN or BTN_LEFT or BTN_RIGHT)) <> 0;
end;

// Usage
var input: word;
input := ReadInput;

if IsButtonPressed(input, BTN_A) then
  WriteLn('A pressed');

if IsAnyDirectionPressed(input) then
  WriteLn('Direction pressed');
```

---

## Detecting Input Changes

### Edge Detection

**Detect button presses (not holds):**

**Problem:** `ReadInput` tells you if a button is currently pressed, but not if it was just pressed this frame.

**Solution:** Store previous input state and compare.

```pascal
var
  currentInput: word;
  previousInput: word;

procedure UpdateInput;
begin
  previousInput := currentInput;  // Save previous state
  currentInput := ReadInput;      // Read new state
end;

function WasButtonJustPressed(button: word): boolean;
begin
  // Button is pressed now, but wasn't pressed before
  WasButtonJustPressed := 
    ((currentInput and button) <> 0) and 
    ((previousInput and button) = 0);
end;

function WasButtonJustReleased(button: word): boolean;
begin
  // Button was pressed before, but isn't pressed now
  WasButtonJustReleased := 
    ((previousInput and button) <> 0) and 
    ((currentInput and button) = 0);
end;
```

**Usage:**
```pascal
UpdateInput;

// Only trigger once when button is first pressed
if WasButtonJustPressed(BTN_A) then
begin
  WriteLn('A was just pressed!');
  // Do something once
end;

// Check if button is held
if IsButtonPressed(currentInput, BTN_B) then
begin
  WriteLn('B is being held');
  // Do something continuously
end;
```

### Complete Input Handler

**Full input handling example:**

```pascal
type
  TInputState = record
    Current: word;
    Previous: word;
  end;

var
  input: TInputState;

procedure UpdateInput(var state: TInputState);
begin
  state.Previous := state.Current;
  state.Current := ReadInput;
end;

function IsPressed(const state: TInputState; button: word): boolean;
begin
  IsPressed := (state.Current and button) <> 0;
end;

function WasJustPressed(const state: TInputState; button: word): boolean;
begin
  WasJustPressed := 
    ((state.Current and button) <> 0) and 
    ((state.Previous and button) = 0);
end;

function WasJustReleased(const state: TInputState; button: word): boolean;
begin
  WasJustReleased := 
    ((state.Previous and button) <> 0) and 
    ((state.Current and button) = 0);
end;

// Usage
UpdateInput(input);

if WasJustPressed(input, BTN_A) then
  WriteLn('A just pressed');

if IsPressed(input, BTN_B) then
  WriteLn('B held');
```

---

## Input Patterns

### Character Movement

**Move character with input:**

```pascal
var
  playerX, playerY: integer;
  input: word;

procedure UpdatePlayer;
begin
  input := ReadInput;
  
  // Move player
  if (input and BTN_LEFT) <> 0 then
    playerX := playerX - 2;
  if (input and BTN_RIGHT) <> 0 then
    playerX := playerX + 2;
  if (input and BTN_UP) <> 0 then
    playerY := playerY - 2;
  if (input and BTN_DOWN) <> 0 then
    playerY := playerY + 2;
  
  // Keep player on screen
  if playerX < 0 then playerX := 0;
  if playerX > 320 then playerX := 320;
  if playerY < 0 then playerY := 0;
  if playerY > 240 then playerY := 240;
end;
```

### Menu Navigation

**Navigate menu with input:**

```pascal
type
  TMenuState = (msMain, msOptions, msQuit);
  
var
  menuState: TMenuState;
  selectedItem: byte;
  input: TInputState;

procedure UpdateMenu;
begin
  UpdateInput(input);
  
  case menuState of
    msMain:
    begin
      // Navigate menu
      if WasJustPressed(input, BTN_UP) then
        selectedItem := (selectedItem - 1) mod 3;
      if WasJustPressed(input, BTN_DOWN) then
        selectedItem := (selectedItem + 1) mod 3;
      
      // Select item
      if WasJustPressed(input, BTN_A) then
      begin
        case selectedItem of
          0: menuState := msOptions;
          1: menuState := msQuit;
          2: // Exit game
        end;
      end;
    end;
    // ... other menu states
  end;
end;
```

### Action on Press

**Trigger action only when button is first pressed:**

```pascal
var
  input: TInputState;
  jumpPressed: boolean;

procedure UpdatePlayer;
begin
  UpdateInput(input);
  
  // Jump only when button is first pressed
  if WasJustPressed(input, BTN_A) and not jumpPressed then
  begin
    Jump;  // Trigger jump
    jumpPressed := true;
  end;
  
  // Reset jump flag when button is released
  if WasJustReleased(input, BTN_A) then
    jumpPressed := false;
end;
```

---

## Platform-Specific Input

### ZealZ80 Input

**ZealZ80 uses PIO (Parallel I/O) for input:**

```pascal
// Read from PIO port
var
  input: byte;
begin
  input := PortIn($D0);  // PIO Port A Data
  
  // Check bits
  if (input and $01) <> 0 then
    WriteLn('Button 0 pressed');
end;
```

### Foenix65C816 Input

**Foenix65C816 uses memory-mapped I/O:**

```pascal
// Read keyboard state
var
  keyboardState: byte;
begin
  keyboardState := VICKY_ReadReg(KEYBOARD_REG);
  
  // Check keys
  if (keyboardState and KEY_SPACE) <> 0 then
    WriteLn('Space pressed');
end;
```

**Note:** Platform-specific input details are in platform documentation. Use `ReadInput` for portable code.

---

## Best Practices

### 1. Read Input Once Per Frame

**Read input at the start of each frame:**

```pascal
// ✅ GOOD
while true do
begin
  var input := ReadInput;  // Read once
  UpdateGame(input);
  RenderFrame;
  WaitVBlank;
end;

// ❌ BAD
while true do
begin
  UpdateGame(ReadInput);  // Reading multiple times
  RenderFrame;
  UpdateGame(ReadInput);  // Inconsistent state
  WaitVBlank;
end;
```

### 2. Use Constants for Button Codes

**Don't hardcode bit values:**

```pascal
// ✅ GOOD
const
  BTN_A = $0001;
  
if (input and BTN_A) <> 0 then
  // ...

// ❌ BAD
if (input and $0001) <> 0 then  // What does $0001 mean?
  // ...
```

### 3. Use Edge Detection for Actions

**Use edge detection for one-time actions:**

```pascal
// ✅ GOOD: Jump only once
if WasJustPressed(input, BTN_A) then
  Jump;

// ❌ BAD: Jump every frame while held
if IsPressed(input, BTN_A) then
  Jump;  // Jumps continuously
```

### 4. Store Previous Input State

**Track previous state for edge detection:**

```pascal
// ✅ GOOD
var
  currentInput, previousInput: word;
  
UpdateInput;  // Updates both
if WasJustPressed(BTN_A) then
  // ...

// ❌ BAD
var input: word;
input := ReadInput;
// No way to detect changes
```

### 5. Handle Multiple Inputs

**Check all relevant inputs:**

```pascal
// ✅ GOOD: Check all directions
if (input and BTN_LEFT) <> 0 then MoveLeft;
if (input and BTN_RIGHT) <> 0 then MoveRight;
if (input and BTN_UP) <> 0 then MoveUp;
if (input and BTN_DOWN) <> 0 then MoveDown;

// ❌ BAD: Only check one
if (input and BTN_LEFT) <> 0 then MoveLeft;
// What about other directions?
```

---

## Exercises

### Exercise 1: Basic Input Reading

Write a program that:
1. Reads input each frame
2. Displays which buttons are pressed
3. Updates display every frame
4. Shows button state on screen

### Exercise 2: Character Movement

Write a program that:
1. Displays a character sprite
2. Moves character with arrow keys/D-pad
3. Keeps character on screen
4. Updates position smoothly

### Exercise 3: Edge Detection

Write a program that:
1. Tracks previous input state
2. Detects when buttons are first pressed
3. Counts how many times each button was pressed
4. Displays counts on screen

### Exercise 4: Menu System

Write a program that:
1. Creates a simple menu (3-4 items)
2. Navigates menu with up/down keys
3. Selects items with A button
4. Changes menu state based on selection

---

**Previous Chapter:** [Chapter 09: Graphics](../11_Graphics/README.md)  
**Next Section:** [Managing Program State](./02_ManagingProgramState.md)  
**Language Specification:** See [Input Intrinsics](../../languageSpecification/intrinsicsAndDirectives/05_InputIntrinsics.md)  
**Last Updated:** 2025-01-XX

