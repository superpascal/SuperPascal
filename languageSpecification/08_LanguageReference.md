# SuperPascal Language Specification — Quick Reference

## Language Reference Guide

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

---

## 1. Keywords Reference

### 1.1 Core Keywords

| Keyword | Purpose | Tier |
|---------|---------|------|
| `and` | Logical AND | 1 |
| `array` | Array type | 1 |
| `begin` | Block start | 1 |
| `boolean` | Boolean type | 1 |
| `byte` | Byte type | 1 |
| `case` | Case statement | 1 |
| `char` | Character type | 1 |
| `const` | Constant declaration | 1 |
| `div` | Integer division | 1 |
| `do` | Loop/if keyword | 1 |
| `downto` | For loop direction | 1 |
| `else` | If/except else | 1 |
| `end` | Block end | 1 |
| `false` | Boolean false | 1 |
| `for` | For loop | 1 |
| `function` | Function declaration | 1 |
| `goto` | Goto statement | 1 |
| `if` | If statement | 1 |
| `integer` | Integer type | 1 |
| `mod` | Modulo operator | 1 |
| `not` | Logical NOT | 1 |
| `of` | Case/array of | 1 |
| `or` | Logical OR | 1 |
| `procedure` | Procedure declaration | 1 |
| `program` | Program declaration | 1 |
| `record` | Record type | 1 |
| `repeat` | Repeat loop | 1 |
| `set` | Set type | 1 |
| `then` | If then | 1 |
| `to` | For loop direction | 1 |
| `true` | Boolean true | 1 |
| `type` | Type declaration | 1 |
| `until` | Repeat until | 1 |
| `var` | Variable declaration | 1 |
| `while` | While loop | 1 |
| `word` | Word type | 1 |

### 1.2 Unit Keywords

| Keyword | Purpose | Tier |
|---------|---------|------|
| `implementation` | Unit implementation | 2 |
| `interface` | Unit interface | 2 |
| `unit` | Unit declaration | 2 |
| `uses` | Unit imports | 2 |

### 1.3 Object Pascal Keywords

| Keyword | Purpose | Tier |
|---------|---------|------|
| `class` | Class type | 3 |
| `constructor` | Constructor method | 3 |
| `destructor` | Destructor method | 3 |
| `override` | Override method | 3 |
| `private` | Private visibility | 3 |
| `protected` | Protected visibility | 3 |
| `public` | Public visibility | 3 |
| `virtual` | Virtual method | 3 |

### 1.4 Exception Keywords

| Keyword | Purpose | Tier |
|---------|---------|------|
| `except` | Exception handler | 3 |
| `finally` | Finally block | 3 |
| `raise` | Raise exception | 3 |
| `try` | Try block | 3 |

### 1.5 Special Keywords

| Keyword | Purpose | Notes |
|---------|---------|-------|
| `nil` | Null pointer | For pointers and classes |
| `self` | Self reference | Implicit in methods |
| `inherited` | Parent method call | In class methods |

---

## 2. Operator Precedence

From highest to lowest:

| Precedence | Operators | Associativity |
|------------|-----------|---------------|
| 1 | `@`, `not`, `-` (unary), `+` (unary) | Right |
| 2 | `.`, `[ ]`, `^`, `()` | Left |
| 3 | `*`, `/`, `div`, `mod`, `and` | Left |
| 4 | `+`, `-`, `or` | Left |
| 5 | `=`, `<>`, `<`, `<=`, `>`, `>=`, `in` | Left |
| 6 | `:=` | Right (statement) |

---

## 3. Type Compatibility Matrix

| Type 1 | Type 2 | Compatible? | Notes |
|--------|--------|-------------|-------|
| `integer` | `integer` | Yes | Same type |
| `integer` | `word` | No | Explicit cast required |
| `integer` | `byte` | No | Explicit cast required |
| Subrange | Base type | Yes | If value fits |
| Enum | Enum | No | Even if same values |
| Record | Record | No | Unless same name |
| Class | Class | Yes | If same or inheritance |
| Array | Array | Yes | If same bounds and element type |
| Set | Set | Yes | If same base type |
| Pointer | Pointer | No | Unless same target type |
| `nil` | Any pointer | Yes | Null pointer |
| `nil` | Any class | Yes | Null reference |

---

## 4. Common Patterns

### 4.1 Variable Declaration

```pascal
var
  x: integer;
  y, z: word;
  name: string;
```

### 4.2 Procedure Declaration

```pascal
procedure DoSomething(x: integer; var y: integer);
begin
  y := x * 2;
end;
```

### 4.3 Function Declaration

```pascal
function Add(a, b: integer): integer;
begin
  Result := a + b;
end;
```

### 4.4 Record Declaration

**Pascal-style:**
```pascal
type
  TVec2 = record
    X, Y: integer;
  end;
```

**C-style (SuperPascal extension):**
```pascal
type
  TVec2 = struct {
    integer X, Y;
  };
```

Both syntaxes are equivalent and can be mixed in the same program.

### 4.5 Class Declaration

```pascal
type
  TObject = class
  public
    procedure Update; virtual;
  end;
```

### 4.6 Array Declaration

```pascal
type
  IntArray = array[0..9] of integer;
```

### 4.7 Set Declaration

```pascal
type
  CharSet = set of char;
```

---

## 5. Control Flow Patterns

### 5.1 If Statement

```pascal
if x > 0 then
  WriteLn('Positive')
else
  WriteLn('Non-positive');
```

### 5.2 While Loop

```pascal
while i < 10 do
begin
  Process(i);
  i := i + 1;
end;
```

### 5.3 For Loop

```pascal
for i := 1 to 10 do
  Process(i);
```

### 5.4 Repeat Loop

```pascal
repeat
  ReadInput;
until Done;
```

### 5.5 Case Statement

```pascal
case x of
  1: WriteLn('One');
  2, 3: WriteLn('Two or Three');
  4..6: WriteLn('Four to Six');
else
  WriteLn('Other');
end;
```

---

## 6. Exception Patterns

### 6.1 Try-Except

```pascal
try
  DangerousOperation;
except
  HandleError;
end;
```

### 6.2 Try-Finally

```pascal
try
  UseResource;
finally
  CleanupResource;
end;
```

### 6.3 Resource Management

```pascal
AcquireResource;
try
  UseResource;
finally
  ReleaseResource;
end;
```

---

## 7. Type Conversion

### 7.1 Explicit Casts

```pascal
var i: integer;
var w: word;
w := word(i);  // Explicit conversion
```

### 7.2 Conversion Functions

```pascal
IntToStr(42)        // "42"
StrToInt("42")      // 42
Ord('A')            // 65
Chr(65)             // 'A'
```

---

## 8. Memory Management

### 8.1 Stack Variables

```pascal
procedure Test;
var x: integer;  // On stack
begin
  x := 5;
end;
```

### 8.2 Heap Allocation

```pascal
var ptr: ^TRecord;
New(ptr);
// Use ptr^
Dispose(ptr);
```

### 8.3 Class Instances

```pascal
var obj: TEntity;
obj := TEntity.Create;
// Use obj
obj.Destroy;
```

### 8.4 Absolute Addressing

```pascal
var
  VideoRAM: array[0..1023] of byte absolute $C000;
  StatusReg: byte absolute $8000;
  Counter: word absolute $8001;
```

Variables at fixed memory addresses for hardware access.

---

## 9. Common Intrinsics

### 9.1 Graphics Intrinsics

```pascal
WaitVBlank;                    // Frame sync
WriteReg($1234, $56);          // Write register
DMA_Copy(src, dest, size);     // Bulk copy
SpriteSet(0, 100, 50, 42);     // Set sprite
SpriteShow(0, true);           // Show sprite
ClearScreen(0);                // Clear screen
```

### 9.2 Memory Access (PEEK/POKE)

```pascal
var value: byte;
value := Peek($8000);          // Read byte (PEEK)
PeekW($8000);                  // Read word
Poke($8000, $FF);             // Write byte (POKE)
PokeW($8000, $1234);          // Write word
```

### 9.3 I/O Port Access

```pascal
var status: byte;
status := PortIn($D1);         // Read from I/O port
PortOut($D1, $FF);             // Write to I/O port
PortInW($D0);                  // Read word from port
PortOutW($D0, $1234);          // Write word to port
```

### 9.4 Memory Management

```pascal
MapPage(2, 10);                // Map MMU page
input := ReadInput;            // Read input
```

---

## 10. Compiler Directives

```pascal
{$INLINE}              // Inline function
{$UNROLL}              // Unroll loop
{$RANGE_CHECK ON}      // Enable bounds checking
{$OVERFLOW_CHECK ON}   // Enable overflow checking
{$DEBUG}               // Debug mode
{$RELEASE}             // Release mode
{$IFDEF DEBUG}         // Conditional compile
  WriteLn('Debug');
{$ENDIF}
```

---

## 11. Quick Syntax Reference

### 11.1 Program Structure

```pascal
program Name;
uses Unit1, Unit2;
var
  // variables
begin
  // statements
end.
```

### 11.2 Unit Structure

```pascal
unit Name;
interface
  // exports
implementation
  // implementation
end.
```

### 11.3 Type Declaration

```pascal
type
  TName = type-spec;
```

### 11.4 Constant Declaration

```pascal
const
  Name = value;
```

### 11.5 Variable Declaration

```pascal
var
  Name: Type;
```

---

## 12. Common Errors and Solutions

### 12.1 Type Mismatch

**Error**: `Type mismatch`
**Solution**: Check types, use explicit cast if needed

### 12.2 Undefined Identifier

**Error**: `Identifier 'X' not found`
**Solution**: Check spelling, scope, `uses` clause

### 12.3 Array Index Out of Bounds

**Error**: `Array index out of bounds`
**Solution**: Check index range, enable `{$RANGE_CHECK}`

### 12.4 Nil Pointer Dereference

**Error**: `Nil pointer dereference`
**Solution**: Check pointer before use: `if ptr <> nil then`

### 12.5 Missing Semicolon

**Error**: `';' expected`
**Solution**: Add semicolon between statements

---

## 13. Language Limits

- **String length**: 255 characters
- **Array dimensions**: Unlimited (practical limit)
- **Set size**: 256 elements (0..255)
- **Identifier length**: 255 characters (practical)
- **Integer range**: -32768 to 65535
- **Nesting depth**: No hard limit

---

## 14. Struct Syntax Examples

### 14.1 Pascal-Style Record

```pascal
type
  TVec2 = record
    X, Y: integer;
  end;
```

### 14.2 C-Style Struct

```pascal
type
  TVec2 = struct {
    integer X, Y;
  };
```

### 14.3 C-Style Initialization

```pascal
var v: TVec2 = {10, 20};
var v2: TVec2 = {X: 10, Y: 20};
```

### 14.4 Anonymous Struct

```pascal
var point = struct {
  integer X, Y;
};
point.X := 10;
```

---

## 15. Game Engine Patterns

### 14.1 Entity Creation

```pascal
var player: TEntityID;
player := EntityCreate;
EntitySetPosition(player, 100, 50);
EntitySetSprite(player, 0, 42, 1);
```

### 14.2 ECS System

```pascal
procedure MovementSystem;
var i: word;
    id: TEntityID;
begin
  for i := 0 to DynamicCount - 1 do
  begin
    id := DynamicEntities[i];
    PositionX[id] := PositionX[id] + VelocityX[id];
    PositionY[id] := PositionY[id] + VelocityY[id];
  end;
end;
```

### 14.3 Collision Detection

```pascal
if CollisionCheck(player, enemy) then
  HandleCollision;
```

### 14.4 Animation

```pascal
EntitySetAnimation(player, @PlayerRunAnim);
AnimationUpdate(player);
```

---

## 16. Audio Patterns

### 15.1 Basic Audio Setup

```pascal
Audio.Init;
Audio.SetMasterVolume(255);
Audio.PlayMusic(@ThemeMusic);
```

### 15.2 Sound Effects

```pascal
PlaySFX(SFX_JUMP);
if IsSFXPlaying(SFX_JUMP) then
  // Still playing
```

### 15.3 Music Control

```pascal
PlayMusic(@ThemeMusic);
MusicFadeOut(60);
Crossfade(@NewTheme, 120);
```

### 15.4 Audio Streaming

```pascal
StreamMusicStart(STREAM_NARRATION);
while IsStreaming do
begin
  StreamMusicUpdate;
  WaitVBlank;
end;
```

---

## 17. Cross-References

- **Lexical Structure**: See [01_LexicalStructure.md](./01_LexicalStructure.md)
- **Grammar**: See [02_Grammar.md](./02_Grammar.md)
- **Type System**: See [03_TypeSystem.md](./03_TypeSystem.md)
- **Semantics**: See [04_Semantics.md](./04_Semantics.md)
- **ABI**: See [Platform Specifications](../platforms/README.md) - ABI is platform-specific
- **Intrinsics**: See [06_IntrinsicsAndDirectives.md](./06_IntrinsicsAndDirectives.md)
- **Exceptions**: See [07_Exceptions.md](./07_Exceptions.md)
- **Game Engine**: See [09_GameEngine.md](./09_GameEngine.md)
- **Audio System**: See [10_AudioSystem.md](./10_AudioSystem.md)
- **Language Extensions**: See [11_LanguageExtensions.md](./11_LanguageExtensions.md)
- **Memory and I/O**: See [12_MemoryAndIO.md](./12_MemoryAndIO.md)

---

**End of Language Reference**

