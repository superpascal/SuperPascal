# Sets and Unions

**Part of:** [Chapter 06: Arrays and Records](./README.md)

---

## Introduction

**Sets and unions** are powerful type system features that enable efficient and type-safe data modeling:

**Sets:**
- **Efficient membership testing** — Check if value is in set (O(1))
- **Set operations** — Union, intersection, difference
- **Flag management** — Perfect for status flags and options
- **Bitset implementation** — Memory efficient (1 bit per element)

**Variant Records (Tagged Unions):**
- **Multiple representations** — One type with different field layouts
- **Memory efficient** — Variants share memory space
- **Type-safe** — Discriminator ensures correct field access
- **Pattern matching** — Use case statements with discriminator

---

## Sets

### What is a Set?

A **set** is a collection of distinct values from an ordinal type (integer, char, enum, subrange). Sets are implemented as bitsets for efficiency.

**Key properties:**
- **No duplicates** — Each element appears at most once
- **Fast membership** — O(1) check if element is in set
- **Efficient operations** — Union, intersection, difference
- **Memory efficient** — 1 bit per element

**Example:**
```pascal
type StatusFlag = (Active, Visible, Enabled, Selected);
type StatusSet = set of StatusFlag;

var status: StatusSet;
begin
  status := [Active, Visible];
  
  if Active in status then
    WriteLn('Active');
end.
```

### Set Declaration

**Set type declaration:**
```pascal
type SetName = set of BaseType;
```

**Base types:**
- **Integer subrange**: `set of 0..31`
- **Character**: `set of char`
- **Enum**: `set of Color`
- **Subrange**: `set of 'A'..'Z'`

**Examples:**
```pascal
type
  CharSet = set of char;
  SmallSet = set of 0..31;
  ColorSet = set of Color;
  LetterSet = set of 'A'..'Z';
```

### Set Literals

**Empty set:**
```pascal
var s: set of 0..31;
begin
  s := [];  // Empty set
end.
```

**Set with elements:**
```pascal
var s: set of 0..31;
begin
  s := [1, 3, 5];  // Set containing 1, 3, and 5
end.
```

**Set with range:**
```pascal
var s: set of 0..31;
begin
  s := [1..10];  // Set containing 1 through 10
end.
```

**Mixed elements and ranges:**
```pascal
var s: set of 0..31;
begin
  s := [1, 3, 5..10, 15];  // Elements 1, 3, 5-10, and 15
end.
```

### Set Operations

**Union (`+` or `|`):**
```pascal
var s1, s2, s3: set of 0..31;
begin
  s1 := [1, 2, 3];
  s2 := [3, 4, 5];
  s3 := s1 + s2;  // s3 = [1, 2, 3, 4, 5] (Pascal style)
  s3 := s1 | s2;  // s3 = [1, 2, 3, 4, 5] (Python style)
end.
```

**Intersection (`*` or `&`):**
```pascal
var s1, s2, s3: set of 0..31;
begin
  s1 := [1, 2, 3];
  s2 := [3, 4, 5];
  s3 := s1 * s2;  // s3 = [3] (Pascal style)
  s3 := s1 & s2;  // s3 = [3] (Python style)
end.
```

**Difference (`-`):**
```pascal
var s1, s2, s3: set of 0..31;
begin
  s1 := [1, 2, 3];
  s2 := [3, 4, 5];
  s3 := s1 - s2;  // s3 = [1, 2] (elements in s1 but not s2)
end.
```

**Symmetric Difference (`^`):**
```pascal
var s1, s2, s3: set of 0..31;
begin
  s1 := [1, 2, 3];
  s2 := [3, 4, 5];
  s3 := s1 ^ s2;  // s3 = [1, 2, 4, 5] (elements in either set, but not both)
end.
```

**Membership (`in`):**
```pascal
var s: set of 0..31;
begin
  s := [1, 2, 3];
  
  if 2 in s then
    WriteLn('2 is in set');
  
  if 5 in s then
    WriteLn('5 is in set')  // This won't execute
  else
    WriteLn('5 is not in set');
end.
```

**Note:** Sets don't have a `Find` function because sets are unordered. Use `in` operator for membership testing (O(1) constant time).

**Equality (`=`, `<>`):**
```pascal
var s1, s2: set of 0..31;
begin
  s1 := [1, 2, 3];
  s2 := [1, 2, 3];
  
  if s1 = s2 then
    WriteLn('Sets are equal');
  
  s2 := [1, 2, 4];
  if s1 <> s2 then
    WriteLn('Sets are different');
end.
```

**Subset (`<=`, `>=`):**
```pascal
var s1, s2: set of 0..31;
begin
  s1 := [1, 2];
  s2 := [1, 2, 3];
  
  if s1 <= s2 then  // s1 is subset of s2
    WriteLn('s1 is subset of s2');
  
  if s2 >= s1 then  // s2 is superset of s1
    WriteLn('s2 is superset of s1');
end.
```

### Sets with Enums

**Enum sets are perfect for flags:**
```pascal
type
  StatusFlag = (Active, Visible, Enabled, Selected);
  StatusSet = set of StatusFlag;

var status: StatusSet;
begin
  status := [Active, Visible];
  
  // Check flags
  if Active in status then
    WriteLn('Active');
  
  if Visible in status then
    WriteLn('Visible');
  
  // Add flag
  status := status + [Enabled];
  
  // Remove flag
  status := status - [Visible];
  
  // Toggle flag
  if Active in status then
    status := status - [Active]
  else
    status := status + [Active];
end.
```

### Common Patterns

**Pattern 1: Status Flags**
```pascal
type
  GameStateFlag = (Paused, Muted, DebugMode, CheatMode);
  GameStateSet = set of GameStateFlag;

var gameState: GameStateSet;
begin
  gameState := [];
  
  // Set flags
  gameState := gameState + [Paused, Muted];
  
  // Check flags
  if Paused in gameState then
    WriteLn('Game is paused');
  
  // Clear all flags
  gameState := [];
end.
```

**Pattern 2: Character Classification**
```pascal
type
  CharClass = set of char;
  
var digits, letters: CharClass;
var c: char;
begin
  digits := ['0'..'9'];
  letters := ['a'..'z', 'A'..'Z'];
  
  c := '5';
  if c in digits then
    WriteLn('Digit');
  
  c := 'A';
  if c in letters then
    WriteLn('Letter');
end.
```

**Pattern 3: Valid Values**
```pascal
type
  ValidKeys = set of char;
  
var validKeys: ValidKeys;
var key: char;
begin
  validKeys := ['w', 'a', 's', 'd', 'q'];
  
  key := ReadKey;
  if key in validKeys then
    ProcessKey(key)
  else
    WriteLn('Invalid key');
end.
```

**Pattern 4: Set Operations**
```pascal
type
  Permission = (Read, Write, Execute);
  PermissionSet = set of Permission;

var userPerms, requiredPerms: PermissionSet;
begin
  userPerms := [Read, Write];
  requiredPerms := [Read, Execute];
  
  // Check if user has all required permissions
  if requiredPerms <= userPerms then
    WriteLn('Access granted')
  else
    WriteLn('Access denied');
  
  // Get common permissions
  var common: PermissionSet;
  common := userPerms * requiredPerms;  // [Read]
end.
```

### Python-Style Set Operations

**SuperPascal supports both Pascal and Python-style set operators:**

| Operation | Pascal | Python | Result |
|-----------|--------|--------|--------|
| Union | `s1 + s2` | `s1 \| s2` | Elements in either set |
| Intersection | `s1 * s2` | `s1 & s2` | Elements in both sets |
| Difference | `s1 - s2` | `s1 - s2` | Elements in s1 but not s2 |
| Symmetric Difference | `s1 ^ s2` | `s1 ^ s2` | Elements in either set, but not both |

**Both styles are supported** — Use whichever you prefer:
```pascal
var s1, s2, s3: set of 0..31;
begin
  s1 := [1, 2, 3];
  s2 := [3, 4, 5];
  
  // Pascal style
  s3 := s1 + s2;  // Union
  s3 := s1 * s2;  // Intersection
  
  // Python style
  s3 := s1 | s2;  // Union
  s3 := s1 & s2;  // Intersection
  s3 := s1 ^ s2;  // Symmetric difference
end.
```

### Set Methods (Python-like)

**SuperPascal provides Python-like set methods:**

| Method | Signature | Description |
|--------|-----------|-------------|
| `Add` | `Add(var s: set of T; value: T)` | Add element to set |
| `Remove` | `Remove(var s: set of T; value: T)` | Remove element (raises error if not found) |
| `Discard` | `Discard(var s: set of T; value: T)` | Remove element (no error if not found) |
| `Clear` | `Clear(var s: set of T)` | Remove all elements |
| `Copy` | `Copy(s: set of T): set of T` | Create copy of set |
| `IsSubset` | `IsSubset(s1, s2: set of T): boolean` | Check if s1 ⊆ s2 |
| `IsSuperset` | `IsSuperset(s1, s2: set of T): boolean` | Check if s1 ⊇ s2 |
| `IsDisjoint` | `IsDisjoint(s1, s2: set of T): boolean` | Check if sets have no common elements |

**Examples:**
```pascal
var s: set of 0..31;
begin
  s := [1, 2, 3];
  
  Add(s, 4);        // s = [1, 2, 3, 4]
  Remove(s, 2);     // s = [1, 3, 4]
  Discard(s, 5);    // s = [1, 3, 4] (no error, 5 wasn't in set)
  
  if IsSubset([1, 3], s) then
    WriteLn('[1, 3] is subset of s');
end.
```

---

## Membership Testing (`in` Operator)

### Python-like `in` Operator

**SuperPascal's `in` operator works with multiple types (Python-like):**

**Sets:**
```pascal
var s: set of 0..31;
begin
  s := [1, 2, 3];
  if 2 in s then
    WriteLn('2 is in set');
end.
```

**Arrays:**
```pascal
var arr: array[0..9] of integer;
var count: integer;
begin
  arr[0] := 10; arr[1] := 20; arr[2] := 30;
  count := 3;
  
  if 20 in arr[:count] then  // Check if 20 is in array
    WriteLn('20 is in array');
end.
```

**Lists:**
```pascal
var myList: list;
begin
  myList := [10, 'Hello', true];
  
  if 10 in myList then
    WriteLn('10 is in list');
  
  if 'Hello' in myList then
    WriteLn('Hello is in list');
end.
```

**Strings:**
```pascal
var text: string;
begin
  text := 'Hello, World!';
  
  if 'World' in text then
    WriteLn('World is in string');
  
  if 'X' in text then
    WriteLn('X is in string')
  else
    WriteLn('X is not in string');
end.
```

**Tuples:**
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

### Performance

**Membership testing performance:**
- **Sets**: O(1) constant time (bitset lookup)
- **Arrays**: O(n) linear time (sequential search)
- **Lists**: O(n) linear time (sequential search)
- **Strings**: O(n) linear time (substring search)
- **Tuples**: O(n) linear time (sequential search)

**Best practice:** Use sets for frequent membership tests.

---

## Variant Records (Tagged Unions)

### What is a Variant Record?

A **variant record** (also called a **tagged union**) is a record that can have different field layouts depending on a discriminator value. All variants share the same memory space.

**Key properties:**
- **Multiple representations** — Different field layouts for different cases
- **Memory efficient** — Variants share memory (only largest variant's size)
- **Type-safe** — Discriminator ensures correct field access
- **Pattern matching** — Use case statements with discriminator

**Example:**
```pascal
type
  ShapeKind = (Circle, Rectangle, Triangle);
  
  TShape = record
    case Kind: ShapeKind of
      Circle:    (X, Y, Radius: integer);
      Rectangle: (X1, Y1, X2, Y2: integer);
      Triangle:  (X1, Y1, X2, Y2, X3, Y3: integer);
  end;
```

### Variant Record Syntax

**Basic syntax:**
```pascal
type
  TRecord = record
    CommonField1: Type1;
    CommonField2: Type2;
    case Discriminator: DiscriminatorType of
      Value1: (Field1: Type1; Field2: Type2);
      Value2: (Field3: Type3; Field4: Type4);
      Value3: (Field5: Type5);
  end;
```

**Rules:**
- **Discriminator field** — Determines which variant is active
- **Common fields** — Shared by all variants (optional)
- **Variant fields** — Different for each discriminator value
- **Memory sharing** — All variants share same memory offset

### Variant Records with Enums

**Using enums as discriminator:**
```pascal
type
  ShapeKind = (Circle, Rectangle, Triangle);
  
  TShape = record
    case Kind: ShapeKind of
      Circle:    (X, Y, Radius: integer);
      Rectangle: (X1, Y1, X2, Y2: integer);
      Triangle:  (X1, Y1, X2, Y2, X3, Y3: integer);
  end;

var shape: TShape;
begin
  // Create circle
  shape.Kind := Circle;
  shape.X := 10;
  shape.Y := 20;
  shape.Radius := 5;
  
  // Process shape
  case shape.Kind of
    Circle:
      WriteLn('Circle at (', shape.X, ', ', shape.Y, ') radius ', shape.Radius);
    Rectangle:
      WriteLn('Rectangle');
    Triangle:
      WriteLn('Triangle');
  end;
end.
```

### Variant Records with Common Fields

**Common fields shared by all variants:**
```pascal
type
  ItemType = (Weapon, Armor, Potion);
  
  TItem = record
    Name: string;
    Value: integer;
    case ItemType: ItemType of
      Weapon: (Damage, Durability: integer);
      Armor:  (Defense, Weight: integer);
      Potion: (HealAmount: integer);
  end;

var item: TItem;
begin
  item.Name := 'Sword';
  item.Value := 100;
  item.ItemType := Weapon;
  item.Damage := 10;
  item.Durability := 50;
end.
```

### Type Safety

**Always check discriminator before accessing variant fields:**
```pascal
var shape: TShape;
begin
  shape.Kind := Circle;
  
  // Safe: Check discriminator first
  case shape.Kind of
    Circle:    WriteLn('Radius: ', shape.Radius);
    Rectangle: WriteLn('Width: ', shape.X2 - shape.X1);
    Triangle:  WriteLn('Triangle');
  end;
  
  // Unsafe: Accessing wrong variant (undefined behavior)
  // shape.Radius;  // ERROR if shape.Kind is not Circle
end.
```

### Common Patterns

**Pattern 1: Shape System**
```pascal
type
  ShapeKind = (Circle, Rectangle, Triangle);
  
  TShape = record
    case Kind: ShapeKind of
      Circle:    (X, Y, Radius: integer);
      Rectangle: (X1, Y1, X2, Y2: integer);
      Triangle:  (X1, Y1, X2, Y2, X3, Y3: integer);
  end;

function DrawShape(shape: TShape);
begin
  case shape.Kind of
    Circle:
      DrawCircle(shape.X, shape.Y, shape.Radius);
    Rectangle:
      DrawRect(shape.X1, shape.Y1, shape.X2, shape.Y2);
    Triangle:
      DrawTriangle(shape.X1, shape.Y1, shape.X2, shape.Y2, shape.X3, shape.Y3);
  end;
end;
```

**Pattern 2: Message System**
```pascal
type
  MessageType = (Text, Number, Command);
  
  TMessage = record
    case MsgType: MessageType of
      Text:    (TextValue: string);
      Number:  (NumberValue: integer);
      Command: (CommandCode: byte; Param1, Param2: integer);
  end;

procedure ProcessMessage(msg: TMessage);
begin
  case msg.MsgType of
    Text:
      WriteLn('Text: ', msg.TextValue);
    Number:
      WriteLn('Number: ', msg.NumberValue);
    Command:
      ExecuteCommand(msg.CommandCode, msg.Param1, msg.Param2);
  end;
end;
```

**Pattern 3: Event System**
```pascal
type
  EventType = (KeyPress, MouseClick, Timer);
  
  TEvent = record
    Timestamp: integer;
    case EventType: EventType of
      KeyPress:  (Key: char);
      MouseClick: (X, Y: integer; Button: byte);
      Timer:     (TimerID: integer);
  end;

procedure HandleEvent(event: TEvent);
begin
  case event.EventType of
    KeyPress:
      ProcessKey(event.Key);
    MouseClick:
      ProcessClick(event.X, event.Y, event.Button);
    Timer:
      ProcessTimer(event.TimerID);
  end;
end;
```

### Variant Records vs Result<T, E>

**Variant records:**
- Multiple variants of same "kind" (all shapes, all events)
- Use when variants are equally valid
- Discriminator determines which variant is active

**Result<T, E>:**
- Success/error dichotomy
- Use for error handling
- `IsOk` determines if value or error

**Example comparison:**
```pascal
// Variant record: Multiple valid shapes
type
  TShape = record
    case Kind: ShapeKind of
      Circle: (X, Y, Radius: integer);
      Rectangle: (X1, Y1, X2, Y2: integer);
  end;

// Result: Success or error
type
  ParseResult = Result<integer, ParseError>;
```

---

## Best Practices

### 1. Use Sets for Flags

**Sets are perfect for status flags:**
```pascal
type
  StatusFlag = (Active, Visible, Enabled);
  StatusSet = set of StatusFlag;

var status: StatusSet;
begin
  status := [Active, Visible];
  
  // Check flag
  if Active in status then
    ProcessActive;
end.
```

### 2. Always Check Discriminator

**Always check variant discriminator before accessing fields:**
```pascal
// Good: Check discriminator
case shape.Kind of
  Circle: ProcessCircle(shape.Radius);
  Rectangle: ProcessRectangle(shape.X1, shape.Y1);
end;

// Bad: Access without check (undefined behavior)
// ProcessCircle(shape.Radius);  // ERROR if shape.Kind is not Circle
```

### 3. Use Enums for Discriminators

**Enums make discriminators type-safe:**
```pascal
// Good: Enum discriminator
type
  ShapeKind = (Circle, Rectangle);
  TShape = record
    case Kind: ShapeKind of
      Circle: (Radius: integer);
      Rectangle: (Width, Height: integer);
  end;

// Less clear: Integer discriminator
type
  TShape = record
    case Kind: integer of
      0: (Radius: integer);
      1: (Width, Height: integer);
  end;
```

### 4. Keep Variants Simple

**Avoid deeply nested variants:**
```pascal
// Good: Simple variants
type
  TShape = record
    case Kind: ShapeKind of
      Circle: (X, Y, Radius: integer);
      Rectangle: (X1, Y1, X2, Y2: integer);
  end;

// Complex: Hard to understand
type
  TShape = record
    case Kind: ShapeKind of
      Circle: (CircleData: TCircleData);
      Rectangle: (RectData: TRectData);
      // Where TCircleData and TRectData are also variant records...
  end;
```

---

## Platform Considerations

### Memory

**Sets:**
- **Bitset implementation** — 1 bit per element
- **Maximum 256 elements** — Limited by 8-bit architecture
- **Efficient** — Up to 32 bytes for full set (256 elements)

**Variant records:**
- **Shared memory** — Only largest variant's size matters
- **Discriminator overhead** — One field for discriminator
- **Efficient** — No wasted space for unused variants

### Performance

**Sets:**
- **Membership test** — O(1) constant time (bit test)
- **Set operations** — O(n) where n is set size in bytes
- **Very fast** — Bitwise operations are efficient

**Variant records:**
- **Field access** — O(1) constant time
- **Discriminator check** — O(1) constant time
- **Memory efficient** — No overhead for unused variants

---

## Summary

**Key Concepts:**
- **Sets** — Efficient collections of distinct ordinal values
- **Set operations** — Union, intersection, difference, membership
- **Variant records** — Records with different field layouts
- **Tagged unions** — Variant records with discriminator
- **Type safety** — Always check discriminator before accessing variants

**Sets:**
- Bitset implementation (1 bit per element)
- Fast membership testing (O(1))
- Perfect for flags and status values
- Maximum 256 elements

**Variant Records:**
- Multiple field layouts in one type
- Memory efficient (variants share space)
- Type-safe (discriminator ensures correct access)
- Use with enums for type-safe discriminators

**When to Use:**
- **Sets** — Flags, status values, valid value sets, character classification
- **Variant records** — Multiple representations of same concept (shapes, events, messages)

**Next:** Learn about lists and linked lists for dynamic data structures.

---

**Previous Section:** [Enumerations and Tuples](./05_EnumerationsAndTuples.md)  
**Next Section:** [Lists and Linked Lists](./07_ListsAndLinkedLists.md)  
**Language Specification:** See [03_TypeSystem.md](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

