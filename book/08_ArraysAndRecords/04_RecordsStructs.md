# Records and Structs

**Part of:** [Chapter 06: Arrays and Records](./README.md)

---

## Introduction

**Records** (also called **structs**) let you group related data together. They're essential for:
- **Structured data** — Organize related information
- **Game objects** — Player data, entity properties
- **Data organization** — Group fields that belong together
- **ECS components** — Entity Component System (covered later)

SuperPascal supports **both Pascal-style records and C-style structs**, making it a superset of standard Pascal.

---

## What is a Record?

A **record** (or **struct**) is:
- **A collection of fields** — Multiple named values
- **Different types** — Each field can have a different type
- **Grouped data** — Related information together
- **Value type** — Copied when assigned (in Pascal)

**Analogy:** Think of a record like a form:
- **Fields** — Name, age, address (different types)
- **One record** — One person's information
- **Multiple records** — Multiple people's information

---

## Pascal-Style Records

### Basic Syntax

**Record declaration:**
```pascal
type
  RecordName = record
    field1: type1;
    field2: type2;
    field3: type3;
  end;
```

**Example:**
```pascal
type
  TPlayer = record
    Name: string;
    Score: integer;
    Health: integer;
    IsAlive: boolean;
  end;
```

### Using Records

**Declare variables:**
```pascal
var player: TPlayer;
begin
  player.Name := 'Alice';
  player.Score := 100;
  player.Health := 50;
  player.IsAlive := true;
end.
```

**Access fields:**
```pascal
WriteLn('Player: ', player.Name);
WriteLn('Score: ', player.Score);
```

---

## C-Style Structs (SuperPascal Extension)

### Basic Syntax

**Struct declaration:**
```pascal
type
  StructName = struct {
    type1 field1;
    type2 field2;
    type3 field3;
  };
```

**Note:** C-style uses `struct` keyword, `{ }` braces, and `type name;` field syntax.

### Example

```pascal
type
  TPlayer = struct {
    string Name;
    integer Score;
    integer Health;
    boolean IsAlive;
  };
```

### Using Structs

**Same usage as records:**
```pascal
var player: TPlayer;
begin
  player.Name := 'Alice';
  player.Score := 100;
  player.Health := 50;
  player.IsAlive := true;
end.
```

**Field access is identical:**
```pascal
WriteLn('Player: ', player.Name);
```

---

## Pascal vs C-Style Comparison

### Syntax Differences

| Feature | Pascal-Style | C-Style |
|---------|--------------|---------|
| **Keyword** | `record` | `struct` |
| **Delimiters** | `record ... end` | `{ ... }` |
| **Field syntax** | `name: type;` | `type name;` |
| **Semicolon** | After `end` | After `}` |

### Example Comparison

**Pascal-style:**
```pascal
type
  TVec2 = record
    X, Y: integer;
  end;
```

**C-style:**
```pascal
type
  TVec2 = struct {
    integer X, Y;
  };
```

**Both are semantically identical!**

### When to Use Which?

**Use Pascal-style when:**
- **Familiar with Pascal** — Traditional Pascal syntax
- **Learning Pascal** — Standard Pascal approach
- **Consistency** — Matching existing Pascal code

**Use C-style when:**
- **Familiar with C/C++** — C/C++ background
- **Team preference** — Team uses C-style
- **Mixed codebase** — Interoperating with C code

**Both work the same** — Choose based on preference or team standards.

---

## Record Examples

### Example 1: 2D Point

```pascal
type
  TPoint = record
    X, Y: integer;
  end;

var point: TPoint;
begin
  point.X := 10;
  point.Y := 20;
  WriteLn('Point: (', point.X, ', ', point.Y, ')');
end.
```

**C-style equivalent:**
```pascal
type
  TPoint = struct {
    integer X, Y;
  };
```

### Example 2: Player Data

```pascal
type
  TPlayer = record
    Name: string;
    Score: integer;
    Health: integer;
    Position: TPoint;
    IsAlive: boolean;
  end;

var player: TPlayer;
begin
  player.Name := 'Alice';
  player.Score := 100;
  player.Health := 50;
  player.Position.X := 10;
  player.Position.Y := 20;
  player.IsAlive := true;
end.
```

### Example 3: Game Entity

```pascal
type
  TEntity = record
    ID: integer;
    X, Y: Q8.8;  // Position (fixed-point)
    VX, VY: Q8.8;  // Velocity (fixed-point)
    SpriteID: byte;
    IsActive: boolean;
  end;

var entity: TEntity;
begin
  entity.ID := 1;
  entity.X := 10.5;
  entity.Y := 20.25;
  entity.VX := 1.0;
  entity.VY := 0.0;
  entity.SpriteID := 5;
  entity.IsActive := true;
end.
```

---

## Record Assignment

### Value Semantics

**Records are value types** — Assignment copies the entire record:

```pascal
var player1, player2: TPlayer;
begin
  player1.Name := 'Alice';
  player1.Score := 100;
  
  player2 := player1;  // Copies all fields
  
  player2.Score := 200;  // Only changes player2
  
  WriteLn('Player1 score: ', player1.Score);  // Still 100
  WriteLn('Player2 score: ', player2.Score);  // Now 200
end.
```

### Field-by-Field Assignment

**You can assign fields individually:**
```pascal
player2.Name := player1.Name;
player2.Score := player1.Score;
```

**Or copy entire record:**
```pascal
player2 := player1;  // Copies all fields at once
```

---

## Arrays of Records

### Declaring Arrays of Records

```pascal
type
  TPlayer = record
    Name: string;
    Score: integer;
  end;

var players: array[0..9] of TPlayer;
var i: integer;
begin
  // Initialize players
  for i := 0 to 9 do
  begin
    players[i].Name := 'Player' + IntToStr(i);
    players[i].Score := i * 10;
  end;
  
  // Print all players
  for i := 0 to 9 do
    WriteLn(players[i].Name, ': ', players[i].Score);
end.
```

### Example: Entity Array

```pascal
type
  TEntity = record
    X, Y: integer;
    IsActive: boolean;
  end;

var entities: array[0..99] of TEntity;
var i: integer;
begin
  // Initialize all entities
  for i := 0 to 99 do
  begin
    entities[i].X := 0;
    entities[i].Y := 0;
    entities[i].IsActive := false;
  end;
  
  // Activate first 10 entities
  for i := 0 to 9 do
  begin
    entities[i].X := i * 10;
    entities[i].Y := i * 10;
    entities[i].IsActive := true;
  end;
end.
```

---

## Nested Records

### Records Inside Records

**Records can contain other records:**
```pascal
type
  TPoint = record
    X, Y: integer;
  end;
  
  TRectangle = record
    TopLeft: TPoint;
    BottomRight: TPoint;
  end;

var rect: TRectangle;
begin
  rect.TopLeft.X := 10;
  rect.TopLeft.Y := 20;
  rect.BottomRight.X := 50;
  rect.BottomRight.Y := 60;
end.
```

### Accessing Nested Fields

**Use dot notation:**
```pascal
rect.TopLeft.X := 10;  // Nested field access
```

---

## Anonymous Structs (C-Style Only)

### What are Anonymous Structs?

**Anonymous structs** have no type name — declared inline:

```pascal
var point = struct {
  integer X, Y;
};
begin
  point.X := 10;
  point.Y := 20;
end.
```

**Use cases:**
- **One-off structures** — Only used once
- **Local data** — Temporary grouping
- **Quick prototypes** — Quick development

**Note:** Pascal-style records always require a type name.

---

## The Hybrid OOP + Struct Model

### SuperPascal's Unique Approach

**SuperPascal uses a hybrid model:**
- **Records (Structs)** — Value types for **data**
- **Classes** — Reference types for **behavior**

### Records for Data

**Records are perfect for:**
- **ECS components** — Position, Velocity, Sprite (data only)
- **Game state** — Player data, entity properties
- **Mathematical types** — Vectors, points, matrices
- **Efficient storage** — Value types, no overhead

**Example:**
```pascal
type
  TPosition = record
    X, Y: Q8.8;
  end;
  
  TVelocity = record
    VX, VY: Q8.8;
  end;
  
  TSprite = record
    ID: byte;
    Frame: byte;
  end;
```

### Classes for Behavior (Preview)

**Classes are for:**
- **Behavior orchestration** — Game systems, managers
- **Code organization** — Group related functions
- **Inheritance** — Code reuse and polymorphism

**Example (preview):**
```pascal
type
  TEntityManager = class
    procedure Update;
    procedure Draw;
  end;
```

**Note:** Classes are covered in detail in Chapter 22 (Object-Oriented Programming).

### Why Hybrid?

**Benefits:**
- **Data-oriented** — Records for fast data access (ECS-friendly)
- **Object-oriented** — Classes for code organization
- **Best of both** — Performance + organization
- **Game development** — Perfect for ECS architecture

---

## Record Methods (Preview)

### Records Can Have Methods

**Records can have procedures/functions:**
```pascal
type
  TVec2 = record
    X, Y: integer;
    
    function Length: integer;
    procedure Add(const Other: TVec2);
  end;

function TVec2.Length: integer;
begin
  Length := X * X + Y * Y;  // Simplified (actual would use sqrt)
end;

procedure TVec2.Add(const Other: TVec2);
begin
  X := X + Other.X;
  Y := Y + Other.Y;
end;
```

**Note:** Methods compile to free functions with `var` parameter for `self`.

**Usage:**
```pascal
var vec: TVec2;
begin
  vec.X := 3;
  vec.Y := 4;
  WriteLn('Length: ', vec.Length);
end.
```

---

## Common Patterns

### Pattern 1: Vector Math

```pascal
type
  TVec2 = record
    X, Y: Q8.8;
  end;

procedure Vec2Add(const A, B: TVec2; var Result: TVec2);
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

var pos, vel, newPos: TVec2;
begin
  pos.X := 10.0;
  pos.Y := 20.0;
  vel.X := 1.0;
  vel.Y := 0.5;
  
  Vec2Add(pos, vel, newPos);
  // newPos = (11.0, 20.5)
end.
```

### Pattern 2: Configuration Data

```pascal
type
  TGameConfig = record
    ScreenWidth: integer;
    ScreenHeight: integer;
    MaxFPS: integer;
    SoundEnabled: boolean;
  end;

var config: TGameConfig;
begin
  config.ScreenWidth := 320;
  config.ScreenHeight := 240;
  config.MaxFPS := 60;
  config.SoundEnabled := true;
end.
```

### Pattern 3: State Data

```pascal
type
  TGameState = record
    Score: integer;
    Level: integer;
    Lives: integer;
    IsPaused: boolean;
  end;

var state: TGameState;
begin
  state.Score := 1000;
  state.Level := 5;
  state.Lives := 3;
  state.IsPaused := false;
end.
```

---

## Best Practices

### 1. Group Related Data

**Bad:**
```pascal
var playerX, playerY, playerScore, playerHealth: integer;
```

**Good:**
```pascal
type
  TPlayer = record
    X, Y: integer;
    Score: integer;
    Health: integer;
  end;
var player: TPlayer;
```

### 2. Use Meaningful Names

**Bad:**
```pascal
type
  TData = record
    A, B: integer;
  end;
```

**Good:**
```pascal
type
  TPoint = record
    X, Y: integer;
  end;
```

### 3. Choose Appropriate Style

**Use Pascal-style for:**
- Traditional Pascal code
- Learning Pascal
- Consistency with existing code

**Use C-style for:**
- C/C++ background
- Team preference
- C interop

### 4. Keep Records Focused

**Bad:**
```pascal
type
  TEverything = record
    // 50 fields mixing unrelated data
  end;
```

**Good:**
```pascal
type
  TPosition = record
    X, Y: integer;
  end;
  
  TVelocity = record
    VX, VY: integer;
  end;
```

---

## Platform Considerations

### Memory Layout

**Records are stored contiguously:**
```pascal
type
  TPoint = record
    X: integer;  // 2 bytes
    Y: integer;  // 2 bytes
  end;
// Total: 4 bytes, stored as [X][Y]
```

**Field order matters for:**
- **Memory layout** — Fields stored in declaration order
- **Size** — Total size is sum of field sizes
- **Alignment** — Compiler may add padding (platform-specific)

### Efficiency

**Records are efficient:**
- **Value types** — No pointer overhead
- **Contiguous** — Cache-friendly access
- **Small size** — Typically small (few bytes to few hundred bytes)
- **On ZealZ80** — Very efficient for game data

---

## Common Mistakes

### Mistake 1: Forgetting Field Access

**Wrong:**
```pascal
var player: TPlayer;
player := 'Alice';  // ERROR: Can't assign string to record
```

**Correct:**
```pascal
var player: TPlayer;
player.Name := 'Alice';  // Access field
```

### Mistake 2: Wrong Field Name

**Wrong:**
```pascal
var point: TPoint;
point.x := 10;  // ERROR: Field names are case-sensitive (usually)
```

**Correct:**
```pascal
var point: TPoint;
point.X := 10;  // Correct field name
```

### Mistake 3: Uninitialized Fields

**Wrong:**
```pascal
var player: TPlayer;
WriteLn(player.Score);  // ERROR: Score is uninitialized
```

**Correct:**
```pascal
var player: TPlayer;
player.Score := 0;  // Initialize
WriteLn(player.Score);  // OK
```

---

## Summary

**Key Concepts:**
- **Records (Structs)** group related data together
- **Pascal-style** — `record ... end` with `name: type;` syntax
- **C-style** — `struct { }` with `type name;` syntax
- **Both are identical** — Semantically the same
- **Value types** — Copied when assigned
- **Hybrid model** — Records for data, classes for behavior

**Syntax:**

**Pascal-style:**
```pascal
type
  TName = record
    field: type;
  end;
```

**C-style:**
```pascal
type
  TName = struct {
    type field;
  };
```

**Usage:**
```pascal
var varName: TName;
varName.field := value;
```

**Best Practices:**
- Group related data
- Use meaningful names
- Choose style based on preference/team
- Keep records focused
- Initialize fields before use

**Next:** Learn about strings and text processing.

---

**Next Section:** [Enumerations and Tuples](./05_EnumerationsAndTuples.md)  
**Next Chapter:** [Chapter 07: Strings and Text Processing](../09_StringsAndTextProcessing/README.md)  
**Language Specification:** See [03_TypeSystem.md](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

