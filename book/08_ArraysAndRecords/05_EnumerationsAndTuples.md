# Enumerations and Tuples

**Part of:** [Chapter 06: Arrays and Records](./README.md)

---

## Introduction

**Enumerations (enums)** and **tuples** are powerful type system features that make code more readable, type-safe, and expressive.

**Enumerations:**
- **Type-safe constants** — Named values instead of magic numbers
- **Readable code** — `Color.Red` instead of `0`
- **Compiler checking** — Type errors caught at compile time
- **Case statements** — Perfect for multiway decisions

**Tuples:**
- **Multiple values** — Return or pass multiple values together
- **Lightweight** — No need for full record when you just need a few values
- **Convenient** — Quick way to group related values
- **Type-safe** — Compiler checks tuple types

---

## Enumerations (Enums)

### What is an Enumeration?

An **enumeration** (enum) is a type that defines a set of named constants:

```pascal
type Color = (Red, Green, Blue);
```

**This creates:**
- A new type called `Color`
- Three constants: `Red`, `Green`, `Blue`
- Ordinal values: `Red = 0`, `Green = 1`, `Blue = 2`

**Visual representation:**
```
Color enum:
  Red   = 0
  Green = 1
  Blue  = 2
```

### Basic Syntax

**Enum declaration:**
```pascal
type EnumName = (Value1, Value2, Value3, ...);
```

**Examples:**
```pascal
type
  Direction = (North, South, East, West);
  GameState = (Menu, Playing, Paused, GameOver);
  Difficulty = (Easy, Normal, Hard, Expert);
```

### Using Enums

**Variable declaration:**
```pascal
var direction: Direction;
var state: GameState;
begin
  direction := North;
  state := Playing;
end.
```

**Comparison:**
```pascal
var color: Color;
begin
  color := Red;
  
  if color = Red then
    WriteLn('Red color');
  
  if color <> Blue then
    WriteLn('Not blue');
end.
```

**Ordinal values:**
```pascal
var color: Color;
var ordValue: integer;
begin
  color := Green;
  ordValue := Ord(color);  // ordValue = 1
  WriteLn('Green is value: ', ordValue);
end.
```

### Enum in Case Statements

**Enums work perfectly with case:**
```pascal
type GameState = (Menu, Playing, Paused, GameOver);
var state: GameState;
begin
  state := Playing;
  
  case state of
    Menu:     WriteLn('Show menu');
    Playing:  WriteLn('Game running');
    Paused:   WriteLn('Game paused');
    GameOver: WriteLn('Game over');
  end;
end.
```

### Enum Operations

**Ord (Ordinal Value):**
```pascal
function Ord(x): integer;
```

**Get numeric value of enum:**
```pascal
var color: Color;
var value: integer;
begin
  color := Blue;
  value := Ord(color);  // value = 2
end.
```

**Pred (Predecessor):**
```pascal
function Pred(x): T;
```

**Get previous enum value:**
```pascal
var color: Color;
begin
  color := Green;
  color := Pred(color);  // color is now Red
end.
```

**Succ (Successor):**
```pascal
function Succ(x): T;
```

**Get next enum value:**
```pascal
var color: Color;
begin
  color := Green;
  color := Succ(color);  // color is now Blue
end.
```

**Low and High:**
```pascal
var first, last: Color;
begin
  first := Low(Color);  // Red (first value)
  last := High(Color);  // Blue (last value)
end.
```

### Enum Comparison

**Enums can be compared:**
```pascal
var c1, c2: Color;
begin
  c1 := Red;
  c2 := Green;
  
  if c1 < c2 then   // true (0 < 1)
    WriteLn('Red comes before Green');
  
  if c1 <= c2 then  // true
    WriteLn('Red <= Green');
end.
```

**Enum ranges:**
```pascal
var color: Color;
begin
  for color := Low(Color) to High(Color) do
    WriteLn('Color: ', Ord(color));
  // Output: 0, 1, 2
end.
```

### Type Safety

**Enums are strongly typed:**
```pascal
type
  Color = (Red, Green, Blue);
  Mood = (Happy, Sad, Angry);

var c: Color;
var m: Mood;
begin
  c := Red;   // OK
  m := Happy; // OK
  
  // c := Happy;  // ERROR: incompatible types!
  // m := Red;     // ERROR: incompatible types!
end.
```

**Even with same values, different enums are incompatible:**
```pascal
type
  Color1 = (Red, Green, Blue);
  Color2 = (Red, Green, Blue);  // Same values, but different type!

var c1: Color1;
var c2: Color2;
begin
  c1 := Red;
  c2 := Red;
  
  // c1 := c2;  // ERROR: Color1 and Color2 are different types!
end.
```

### Common Patterns

**Pattern 1: State Machine**
```pascal
type GameState = (Menu, Playing, Paused, GameOver);
var state: GameState;

procedure UpdateGame;
begin
  case state of
    Menu:     UpdateMenu;
    Playing:  UpdateGameplay;
    Paused:   UpdatePaused;
    GameOver: UpdateGameOver;
  end;
end;
```

**Pattern 2: Direction**
```pascal
type Direction = (North, South, East, West);
var dir: Direction;

function MoveDirection(x, y: integer; dir: Direction): TPoint;
begin
  case dir of
    North: begin MoveDirection.X := x; MoveDirection.Y := y - 1; end;
    South: begin MoveDirection.X := x; MoveDirection.Y := y + 1; end;
    East:  begin MoveDirection.X := x + 1; MoveDirection.Y := y; end;
    West:  begin MoveDirection.X := x - 1; MoveDirection.Y := y; end;
  end;
end;
```

**Pattern 3: Flags (using sets)**
```pascal
type StatusFlag = (Active, Visible, Enabled, Selected);
type StatusSet = set of StatusFlag;

var status: StatusSet;
begin
  status := [Active, Visible];
  
  if Active in status then
    WriteLn('Active');
  
  status := status + [Enabled];  // Add flag
  status := status - [Visible];   // Remove flag
end.
```

---

## Tuples

### What is a Tuple?

A **tuple** is an ordered collection of values, similar to a record but without named fields. Tuples are useful for:
- **Multiple return values** — Return several values from a function
- **Temporary grouping** — Group values without creating a record type
- **Lightweight data** — When a full record is overkill

**SuperPascal tuple syntax:**
```pascal
var point: (integer, integer);  // Tuple of two integers
```

### Basic Syntax

**Tuple type declaration:**
```pascal
type TupleType = (Type1, Type2, Type3, ...);
```

**Examples:**
```pascal
type
  Point2D = (integer, integer);        // (x, y)
  Point3D = (integer, integer, integer); // (x, y, z)
  Result = (boolean, integer);          // (success, value)
  ColorRGB = (byte, byte, byte);        // (r, g, b)
```

**Variable declaration:**
```pascal
var point: (integer, integer);
var result: (boolean, integer);
var color: (byte, byte, byte);
```

### Tuple Assignment

**Assign tuple values:**
```pascal
var point: (integer, integer);
begin
  point := (10, 20);  // x = 10, y = 20
end.
```

**Named tuple assignment (SuperPascal extension):**
```pascal
var point: (integer, integer);
begin
  point := (X: 10, Y: 20);  // Named fields (optional)
end.
```

### Accessing Tuple Elements

**Access by position:**
```pascal
var point: (integer, integer);
var x, y: integer;
begin
  point := (10, 20);
  x := point[0];  // First element (x)
  y := point[1];  // Second element (y)
end.
```

**Destructuring assignment (SuperPascal extension):**
```pascal
var point: (integer, integer);
var x, y: integer;
begin
  point := (10, 20);
  (x, y) := point;  // Destructure tuple
  // x = 10, y = 20
end.
```

### Multiple Return Values

**Return multiple values from function:**
```pascal
function Divide(a, b: integer): (boolean, integer);
var success: boolean;
var result: integer;
begin
  if b <> 0 then
  begin
    success := true;
    result := a div b;
  end
  else
  begin
    success := false;
    result := 0;
  end;
  
  Divide := (success, result);
end;
```

**Using tuple return:**
```pascal
var result: (boolean, integer);
var success: boolean;
var value: integer;
begin
  result := Divide(10, 2);
  success := result[0];  // Get success flag
  value := result[1];    // Get result value
  
  if success then
    WriteLn('Result: ', value)
  else
    WriteLn('Division by zero!');
end.
```

**With destructuring:**
```pascal
var success: boolean;
var value: integer;
begin
  (success, value) := Divide(10, 2);
  
  if success then
    WriteLn('Result: ', value);
end.
```

### Tuple Comparison

**Tuples can be compared element-wise:**
```pascal
var p1, p2: (integer, integer);
begin
  p1 := (10, 20);
  p2 := (10, 20);
  
  if p1 = p2 then
    WriteLn('Points are equal');
  
  p2 := (10, 21);
  if p1 < p2 then  // Compares element by element
    WriteLn('p1 < p2');
end.
```

### Tuple Membership (`in` Operator)

**Check if value is in tuple (Python-like):**
```pascal
var point: (integer, integer);
begin
  point := (10, 20);
  
  if 10 in point then
    WriteLn('10 is in tuple');
  
  if 30 in point then
    WriteLn('30 is in tuple')
  else
    WriteLn('30 is not in tuple');
end.
```

**Find index in tuple:**
```pascal
var point: (integer, integer);
var index: integer;
begin
  point := (10, 20);
  
  // Find index (returns -1 if not found)
  index := Find(point, 10);  // index = 0
  index := Find(point, 20);  // index = 1
  index := Find(point, 30);  // index = -1 (not found)
  
  if index >= 0 then
    WriteLn('Found at index: ', index)
  else
    WriteLn('Not found');
end.
```

**Performance:**
- `in` operator: O(n) linear time (sequential search, returns boolean)
- `Find` function: O(n) linear time (sequential search, returns index)
- Where n is the number of elements in the tuple

**Comparison rules:**
- Compare first elements
- If equal, compare second elements
- Continue until difference found or all equal

### Tuple in Function Parameters

**Pass tuple as parameter:**
```pascal
function Distance(p1, p2: (integer, integer)): Q8.8;
var dx, dy: integer;
begin
  dx := p2[0] - p1[0];
  dy := p2[1] - p1[1];
  Distance := Sqrt(Q8.8(dx * dx + dy * dy));
end;
```

**Usage:**
```pascal
var p1, p2: (integer, integer);
var dist: Q8.8;
begin
  p1 := (0, 0);
  p2 := (3, 4);
  dist := Distance(p1, p2);  // dist = 5.0
end.
```

### Tuple Arrays

**Array of tuples:**
```pascal
var points: array[0..9] of (integer, integer);
var i: integer;
begin
  for i := 0 to 9 do
    points[i] := (i * 10, i * 20);
  
  // Access
  WriteLn('Point 0: (', points[0][0], ', ', points[0][1], ')');
end.
```

### Anonymous Tuples

**Use tuples without type declaration:**
```pascal
var point: (integer, integer);  // Anonymous tuple type
begin
  point := (10, 20);
end.
```

**Or with explicit type:**
```pascal
type Point = (integer, integer);
var point: Point;
begin
  point := (10, 20);
end.
```

### Tuple vs Record

**When to use tuple:**
- **Simple grouping** — Just a few related values
- **Temporary data** — Don't need a named type
- **Multiple return values** — Function returns
- **Lightweight** — No methods or complex structure

**When to use record:**
- **Named fields** — Need field names for clarity
- **Methods** — Need to attach methods
- **Complex structure** — Many fields, nested structures
- **Reusable type** — Used in multiple places

**Example comparison:**
```pascal
// Tuple: Simple, temporary
var point: (integer, integer);
point := (10, 20);

// Record: Named fields, reusable
type TPoint = record
  X, Y: integer;
end;
var point: TPoint;
point.X := 10;
point.Y := 20;
```

---

## Common Patterns

### Pattern 1: Function with Multiple Returns

**Return success and value:**
```pascal
function TryParseInt(s: string): (boolean, integer);
var value: integer;
var success: boolean;
begin
  success := TryStrToInt(s, value);
  TryParseInt := (success, value);
end;

var result: (boolean, integer);
var success: boolean;
var value: integer;
begin
  result := TryParseInt('42');
  (success, value) := result;
  
  if success then
    WriteLn('Parsed: ', value)
  else
    WriteLn('Parse failed');
end.
```

### Pattern 2: Coordinate Pairs

**Use tuples for coordinates:**
```pascal
function GetPlayerPosition: (integer, integer);
begin
  GetPlayerPosition := (playerX, playerY);
end;

var pos: (integer, integer);
var x, y: integer;
begin
  pos := GetPlayerPosition;
  (x, y) := pos;
  WriteLn('Player at: (', x, ', ', y, ')');
end.
```

### Pattern 3: Key-Value Pairs

**Simple key-value:**
```pascal
type KeyValue = (string, integer);
var kv: KeyValue;
begin
  kv := ('Score', 1000);
  WriteLn(kv[0], ': ', kv[1]);  // "Score: 1000"
end.
```

### Pattern 4: Enum with Associated Data

**Combine enum with tuple:**
```pascal
type
  ItemType = (Weapon, Armor, Potion);
  Item = record
    ItemType: ItemType;
    Data: (integer, integer);  // Tuple for item-specific data
  end;

var item: Item;
begin
  item.ItemType := Weapon;
  item.Data := (10, 5);  // (damage, durability)
end.
```

### Pattern 5: Error Types with Result<T, E>

**Use enums for type-safe error codes:**
```pascal
type
  FileError = (NotFound, PermissionDenied, DiskFull, InvalidPath);
  
function OpenFile(path: string): Result<FileHandle, FileError>;
begin
  if not FileExists(path) then
    OpenFile := Result<FileHandle, FileError>.Error(NotFound)
  else
    OpenFile := Result<FileHandle, FileError>.Ok(CreateHandle(path));
end;

var result: Result<FileHandle, FileError>;
var handle: FileHandle;
begin
  result := OpenFile('data.txt');
  
  case result.IsOk of
    true:
      begin
        handle := result.Value;
        ProcessFile(handle);
      end;
    false:
      case result.Error of
        NotFound:         WriteLn('File not found');
        PermissionDenied: WriteLn('Permission denied');
        DiskFull:         WriteLn('Disk full');
        InvalidPath:      WriteLn('Invalid path');
      end;
  end;
end.
```

**Benefits:**
- **Type-safe** — Compiler checks error handling
- **Explicit** — Must handle errors (no silent failures)
- **Zero-cost** — No exception overhead
- **Clear** — Error types are self-documenting

---

## Best Practices

### 1. Use Enums for Constants

**Prefer enums over magic numbers:**
```pascal
// Good: Type-safe, readable
type GameState = (Menu, Playing, Paused);
var state: GameState;
state := Playing;

// Bad: Magic numbers
var state: integer;
state := 1;  // What does 1 mean?
```

### 2. Use Descriptive Enum Names

**Make enum values clear:**
```pascal
// Good: Clear and descriptive
type Direction = (North, South, East, West);

// Avoid: Unclear
type Dir = (N, S, E, W);  // Less readable
```

### 3. Use Tuples for Simple Grouping

**Tuples for temporary grouping:**
```pascal
// Good: Simple tuple for coordinates
var pos: (integer, integer);
pos := (10, 20);

// Use record if you need named fields
type TPoint = record
  X, Y: integer;
end;
```

### 4. Destructure Tuples

**Use destructuring for clarity:**
```pascal
// Good: Clear what values are
var success: boolean;
var value: integer;
(success, value) := TryParse('42');

// Less clear: Access by index
var result: (boolean, integer);
result := TryParse('42');
if result[0] then  // What is result[0]?
  WriteLn(result[1]);
```

### 5. Type Safety

**Leverage type safety:**
```pascal
// Enums catch errors at compile time
type Color = (Red, Green, Blue);
var color: Color;
color := Red;     // OK
// color := 0;    // ERROR: type mismatch
```

---

## Platform Considerations

### Memory

**Enums:**
- **Small** — Typically 1-2 bytes (ordinal value)
- **Efficient** — Stored as integers
- **No overhead** — Just the ordinal value

**Tuples:**
- **Size** — Sum of element sizes
- **Contiguous** — Elements stored together
- **No padding** — Elements packed (platform-dependent)

### Performance

**Enums:**
- **Fast** — Just integer operations
- **No overhead** — Direct value comparison
- **Case statements** — Compiler optimizes

**Tuples:**
- **Fast** — Direct value access
- **No overhead** — Like records but without names
- **Efficient** — Contiguous memory layout

---

## Summary

**Key Concepts:**
- **Enumerations** — Type-safe named constants
- **Tuples** — Lightweight grouping of multiple values
- **Type safety** — Compiler catches type errors
- **Readability** — Code is more expressive

**Enumerations:**
- `type Enum = (Value1, Value2, ...)` — Declare enum
- `Ord()`, `Pred()`, `Succ()` — Enum operations
- **Type-safe** — Different enums are incompatible
- **Perfect for** — State machines, directions, flags

**Tuples:**
- `(Type1, Type2, ...)` — Tuple type
- `(value1, value2)` — Tuple literal
- `tuple[0]`, `tuple[1]` — Access elements
- `(x, y) := tuple` — Destructure
- **Perfect for** — Multiple return values, simple grouping

**When to Use:**
- **Enums** — When you have a fixed set of named constants
- **Tuples** — When you need to group a few values temporarily
- **Records** — When you need named fields or methods

**Next:** Learn about lists and linked lists for dynamic data structures.

---

**Next Section:** [Sets and Unions](./06_SetsAndUnions.md)  
**Language Specification:** See [03_TypeSystem.md](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

