# Text Formatting

**Part of:** [Chapter 07: Strings and Text Processing](./README.md)

---

**Previous Section:** [String Interpolation](./02_StringInterpolation.md)

---

## Introduction

**Text formatting** makes output readable and professional. It's essential for:
- **User interfaces** — Menus, status displays, HUD
- **Game feedback** — Scores, messages, instructions
- **Data display** — Tables, reports, debug output
- **Professional output** — Clean, organized text

This chapter teaches you how to format text effectively in SuperPascal.

---

## WriteLn Formatting

### Basic Output

**WriteLn outputs text and moves to next line:**
```pascal
WriteLn('Hello, World!');
WriteLn('This is on a new line');
```

### Multiple Values

**WriteLn can output multiple values:**
```pascal
var name: string;
var score: integer;
begin
  name := 'Alice';
  score := 1000;
  WriteLn('Player: ', name, ', Score: ', score);
end.
```

**Output:**
```
Player: Alice, Score: 1000
```

### Write vs. WriteLn

**`Write` outputs without newline:**
```pascal
Write('Hello');
Write(' World');
WriteLn('!');  // Now move to next line
```

**Output:**
```
Hello World!
```

**`WriteLn` outputs and adds newline:**
```pascal
WriteLn('Line 1');
WriteLn('Line 2');
```

**Output:**
```
Line 1
Line 2
```

---

## Formatting Numbers

### Integer Formatting

**Integers are formatted automatically:**
```pascal
var score: integer;
begin
  score := 1000;
  WriteLn('Score: ', score);  // Score: 1000
end.
```

### Fixed-Point Formatting

**Fixed-point types need conversion for display:**
```pascal
var x: Q8.8;
begin
  x := 10.5;
  // Convert to string for display (if conversion function exists)
  WriteLn('Position: ', Q8_8ToStr(x));  // Assuming conversion function
end.
```

**Note:** Check SuperPascal specification for fixed-point to string conversion functions.

### Hexadecimal Output

**Output integers in hexadecimal:**
```pascal
var value: integer;
begin
  value := 255;
  WriteLn('Decimal: ', value);        // 255
  WriteLn('Hex: $', IntToHex(value)); // $FF (if IntToHex exists)
end.
```

**Note:** Check SuperPascal specification for `IntToHex` or similar functions.

---

## String Formatting Patterns

### Pattern 1: Score Display

```pascal
var playerName: string;
var score: integer;
var level: integer;
begin
  playerName := 'Alice';
  score := 1000;
  level := 5;
  
  WriteLn('=== Game Status ===');
  WriteLn('Player: ', playerName);
  WriteLn('Score:  ', score);
  WriteLn('Level:  ', level);
end.
```

**Output:**
```
=== Game Status ===
Player: Alice
Score:  1000
Level:  5
```

### Pattern 2: Menu Formatting

```pascal
procedure ShowMenu;
begin
  WriteLn('====================');
  WriteLn('   GAME MENU');
  WriteLn('====================');
  WriteLn('1. New Game');
  WriteLn('2. Load Game');
  WriteLn('3. Settings');
  WriteLn('4. Quit');
  WriteLn('====================');
end;
```

### Pattern 3: Table Formatting

```pascal
var players: array[0..2] of string;
var scores: array[0..2] of integer;
var i: integer;
begin
  players[0] := 'Alice'; scores[0] := 1000;
  players[1] := 'Bob';   scores[1] := 850;
  players[2] := 'Charlie'; scores[2] := 1200;
  
  WriteLn('Player        Score');
  WriteLn('--------------------');
  for i := 0 to 2 do
    WriteLn(players[i], '        ', scores[i]);
end.
```

**Output:**
```
Player        Score
--------------------
Alice        1000
Bob          850
Charlie      1200
```

---

## Text Alignment

### Left Alignment (Default)

**Strings are left-aligned by default:**
```pascal
WriteLn('Left aligned text');
```

### Manual Right Alignment

**Pad with spaces for right alignment:**
```pascal
function RightAlign(const text: string; width: integer): string;
var padding: integer;
var i: integer;
begin
  padding := width - Length(text);
  if padding > 0 then
  begin
    RightAlign := '';
    for i := 1 to padding do
      RightAlign := RightAlign + ' ';
    RightAlign := RightAlign + text;
  end
  else
    RightAlign := text;
end;

var score: integer;
begin
  score := 1000;
  WriteLn('Score: ', RightAlign(IntToStr(score), 10));
end.
```

### Manual Center Alignment

```pascal
function CenterAlign(const text: string; width: integer): string;
var padding: integer;
var leftPad, rightPad: integer;
var i: integer;
begin
  padding := width - Length(text);
  if padding > 0 then
  begin
    leftPad := padding div 2;
    rightPad := padding - leftPad;
    CenterAlign := '';
    for i := 1 to leftPad do
      CenterAlign := CenterAlign + ' ';
    CenterAlign := CenterAlign + text;
    for i := 1 to rightPad do
      CenterAlign := CenterAlign + ' ';
  end
  else
    CenterAlign := text;
end;
```

---

## Game UI Text

### HUD (Heads-Up Display)

```pascal
procedure DrawHUD(score: integer; health: integer; level: integer);
begin
  WriteLn('Score: ', score, ' | Health: ', health, ' | Level: ', level);
end;
```

### Status Messages

```pascal
procedure ShowStatus(const message: string);
begin
  WriteLn('[STATUS] ', message);
end;

procedure ShowError(const message: string);
begin
  WriteLn('[ERROR] ', message);
end;

begin
  ShowStatus('Game loaded successfully');
  ShowError('Invalid input');
end.
```

### Progress Indicators

```pascal
procedure ShowProgress(current, total: integer);
var percent: integer;
begin
  percent := (current * 100) div total;
  Write('Progress: [');
  // Draw progress bar (simplified)
  Write('=');
  Write('] ', percent, '%');
  WriteLn;
end;
```

---

## Formatting Best Practices

### 1. Consistent Spacing

**Use consistent spacing:**
```pascal
WriteLn('Player: ', name, ' | Score: ', score);
// Clear separation with | or spaces
```

### 2. Clear Labels

**Always label values:**
```pascal
// Bad
WriteLn(score);

// Good
WriteLn('Score: ', score);
```

### 3. Organize Output

**Group related information:**
```pascal
WriteLn('=== Player Info ===');
WriteLn('Name: ', name);
WriteLn('Score: ', score);
WriteLn('==================');
```

### 4. Handle Long Text

**Truncate if needed:**
```pascal
function Truncate(const text: string; maxLen: integer): string;
begin
  if Length(text) > maxLen then
    Truncate := Copy(text, 1, maxLen - 3) + '...'
  else
    Truncate := text;
end;
```

---

## Platform Considerations

### Display Limitations

**On retro platforms:**
- **Screen size** — Limited characters per line
- **Character set** — May have limited characters
- **Colors** — May support limited colors
- **Font** — Fixed-width fonts typical

**Design for:**
- **40-80 characters per line** — Typical retro display
- **Simple formatting** — Avoid complex layouts
- **Clear text** — Readable at low resolution

### Memory Efficiency

**String formatting:**
- **Temporary strings** — Format operations may create temporary strings
- **Memory usage** — Be mindful of string length
- **On ZealZ80** — Keep formatted strings short

---

## Common Patterns

### Pattern 1: Score Display

```pascal
procedure DisplayScore(score: integer);
begin
  WriteLn('==========');
  WriteLn('  SCORE');
  WriteLn('==========');
  WriteLn('  ', score);
  WriteLn('==========');
end;
```

### Pattern 2: Menu Item

```pascal
procedure DisplayMenuItem(number: integer; const text: string);
begin
  WriteLn(number, '. ', text);
end;
```

### Pattern 3: Debug Output

```pascal
procedure DebugPrint(const label: string; value: integer);
begin
  WriteLn('[DEBUG] ', label, ': ', value);
end;
```

---

## Summary

**Key Concepts:**
- **WriteLn** outputs text with newline
- **Write** outputs without newline
- **Multiple values** can be output in one WriteLn
- **Formatting** makes output readable
- **Alignment** can be done manually with padding

**Formatting Techniques:**
- Consistent spacing
- Clear labels
- Organized output
- Truncation for long text

**Game Applications:**
- HUD displays
- Status messages
- Menus
- Progress indicators
- Debug output

**Best Practices:**
- Label all values
- Use consistent spacing
- Organize related information
- Design for platform limitations

**Next:** Learn about units and modular programming.

---

**Next Section:** [Regular Expressions](./04_RegularExpressions.md)  
**Next Chapter:** [Chapter 08: Units and Modular Programming](../10_UnitsAndModularProgramming/README.md)  
**Language Specification:** See [03_TypeSystem.md](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

