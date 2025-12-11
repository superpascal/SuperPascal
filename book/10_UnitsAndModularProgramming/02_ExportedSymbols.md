# Exported Symbols

**Part of:** [Chapter 08: Units and Modular Programming](./README.md)

---

## Introduction

When you declare something in a unit's **interface section**, it becomes **exported** — meaning other code can use it. Understanding what can be exported and how to use exported symbols is essential for building modular programs.

**Key concepts:**
- **Exported symbols** — Public API of a unit
- **Visibility rules** — What can be accessed from where
- **Qualified names** — Using `UnitName.SymbolName`
- **Name conflicts** — What happens when names collide

---

## What Can Be Exported?

### Types

**All type declarations in the interface are exported:**

```pascal
unit GameTypes;
interface
  // Exported types
  type
    TPoint = record
      X, Y: integer;
    end;
    
    TColor = (Red, Green, Blue);
    
    TPlayer = record
      Name: string;
      Position: TPoint;
      Color: TColor;
    end;
    
    PPlayer = ^TPlayer;  // Pointer types can be exported too
implementation
  // Implementation section
end.
```

**Using exported types:**
```pascal
program MyGame;
uses GameTypes;

var
  player: TPlayer;      // Can use TPlayer
  pos: TPoint;          // Can use TPoint
  color: TColor;       // Can use TColor
  ptr: PPlayer;        // Can use pointer types
begin
  player.Position.X := 10;
  player.Color := Red;
end.
```

### Constants

**Constants declared in interface are exported:**

```pascal
unit GameConstants;
interface
  const
    SCREEN_WIDTH = 320;
    SCREEN_HEIGHT = 240;
    MAX_PLAYERS = 4;
    DEFAULT_COLOR = $1F00;  // RGB color value
    
    // Typed constants
    PI: Q8.8 = 3.14159;
    GRAVITY: Q8.8 = 9.81;
implementation
end.
```

**Using exported constants:**
```pascal
program MyGame;
uses GameConstants;

var
  x, y: integer;
begin
  x := SCREEN_WIDTH div 2;   // Use exported constant
  y := SCREEN_HEIGHT div 2;
  
  if playerCount > MAX_PLAYERS then
    WriteLn('Too many players!');
end.
```

### Procedures and Functions

**Procedures and functions declared in interface are exported:**

```pascal
unit MathUtils;
interface
  // Exported functions
  function Add(a, b: integer): integer;
  function Multiply(a, b: integer): integer;
  function Distance(x1, y1, x2, y2: integer): integer;
  
  // Exported procedures
  procedure Swap(var a, b: integer);
  procedure DrawCircle(x, y, radius: integer);
implementation
  // Implementations (can be private)
  function Add(a, b: integer): integer;
  begin
    Add := a + b;
  end;
  
  // ... other implementations
end.
```

**Using exported procedures and functions:**
```pascal
program Calculator;
uses MathUtils;

var
  sum, product: integer;
begin
  sum := Add(5, 3);              // Call exported function
  product := Multiply(4, 7);
  
  Swap(sum, product);            // Call exported procedure
  
  DrawCircle(160, 120, 50);      // Call exported procedure
end.
```

### Variables (Global Variables)

**Variables can be exported, but use them carefully:**

```pascal
unit GameState;
interface
  var
    Score: integer;           // Exported global variable
    Level: integer;           // Exported global variable
    PlayerName: string;       // Exported global variable
implementation
end.
```

**Using exported variables:**
```pascal
program MyGame;
uses GameState;

begin
  Score := 0;                 // Can access exported variable
  Level := 1;
  PlayerName := 'Player1';
  
  WriteLn('Score: ', Score);
end.
```

**⚠️ Warning: Global variables are generally discouraged:**
- Make code harder to understand
- Can lead to bugs (unexpected changes)
- Make testing difficult
- Better alternatives: Use functions to access/modify state

**Better approach:**
```pascal
unit GameState;
interface
  // Export functions instead of variables
  function GetScore: integer;
  procedure SetScore(value: integer);
  function GetLevel: integer;
  procedure SetLevel(value: integer);
implementation
  var
    Score: integer;    // Private variable
    Level: integer;    // Private variable
    
  function GetScore: integer;
  begin
    GetScore := Score;
  end;
  
  procedure SetScore(value: integer);
  begin
    Score := value;
  end;
  
  // ... other functions
end.
```

---

## Visibility Rules

### Public (Exported) vs Private

**Rule: Everything in `interface` is public, everything in `implementation` is private.**

```pascal
unit Example;
interface
  // PUBLIC (exported)
  type
    TPoint = record
      X, Y: integer;
    end;
  
  function PublicFunction(x: integer): integer;
  
implementation
  // PRIVATE (not exported)
  type
    TInternal = record
      Data: integer;
    end;
  
  function PrivateHelper(x: integer): integer;
  begin
    PrivateHelper := x * 2;
  end;
  
  function PublicFunction(x: integer): integer;
  begin
    // Can use private helper
    PublicFunction := PrivateHelper(x) + 1;
  end;
end.
```

**What other code can access:**
- ✅ `TPoint` — Exported type
- ✅ `PublicFunction` — Exported function
- ❌ `TInternal` — Private type (not accessible)
- ❌ `PrivateHelper` — Private function (not accessible)

### Using Exported Symbols

**Basic usage (unqualified names):**

```pascal
program MyProgram;
uses MathUtils, GameTypes;

var
  result: integer;
  point: TPoint;
begin
  result := Add(5, 3);        // Use exported function
  point.X := 10;               // Use exported type
  point.Y := 20;
end.
```

**Qualified names (explicit unit name):**

```pascal
program MyProgram;
uses MathUtils, GameTypes;

var
  result: integer;
  point: GameTypes.TPoint;     // Qualified name
begin
  result := MathUtils.Add(5, 3);  // Qualified name
  point.X := 10;
end.
```

**When to use qualified names:**
- **Name conflicts** — Same name in multiple units
- **Clarity** — Make it clear which unit a symbol comes from
- **Large projects** — Easier to understand code

---

## Name Conflicts and Resolution

### What is a Name Conflict?

A **name conflict** occurs when two units export symbols with the same name.

**Example:**
```pascal
// Unit A
unit UnitA;
interface
  type TPoint = record
    X, Y: integer;
  end;
end.

// Unit B
unit UnitB;
interface
  type TPoint = record
    R, G, B: byte;  // Different structure!
  end;
end.

// Program using both
program MyProgram;
uses UnitA, UnitB;

var
  p: TPoint;  // ERROR: Which TPoint? UnitA.TPoint or UnitB.TPoint?
begin
  // ...
end.
```

### Resolving Name Conflicts

**Solution 1: Use qualified names**

```pascal
program MyProgram;
uses UnitA, UnitB;

var
  point2D: UnitA.TPoint;      // Explicit: UnitA's TPoint
  point3D: UnitB.TPoint;       // Explicit: UnitB's TPoint
begin
  point2D.X := 10;
  point2D.Y := 20;
  
  point3D.R := 255;
  point3D.G := 128;
  point3D.B := 64;
end.
```

**Solution 2: Use namespace aliases (if using Rust-style modules)**

```pascal
program MyProgram;
uses graphics.geometry.Point2D, graphics.geometry.Point3D;

namespace Point2D = graphics.geometry.Point2D;  // Create alias
namespace Point3D = graphics.geometry.Point3D;  // Create alias

var
  p2: Point2D.TPoint;   // Use shorter alias
  p3: Point3D.TPoint;   // Use shorter alias
begin
  // ...
end.
```

**Solution 3: Use `using` statements (selective import)**

```pascal
program MyProgram;
uses graphics.geometry.Point2D, graphics.geometry.Point3D;

using graphics.geometry.Point2D;  // Bring Point2D into scope

var
  p2: TPoint;            // Unqualified (from Point2D)
  p3: Point3D.TPoint;   // Qualified (from Point3D)
begin
  // ...
end.
```

### Shadowing Rules

**Local declarations shadow imported symbols:**

```pascal
program MyProgram;
uses MathUtils;

var
  Add: integer;  // Local variable shadows MathUtils.Add

procedure Test;
begin
  Add := 5;              // Refers to local variable
  // MathUtils.Add(3, 4); // ERROR: Add is now a variable, not a function
end;
```

**Best practice: Avoid shadowing exported symbols**
- Use different names for local variables
- Use qualified names if you must shadow

---

## Qualified Names with Rust-Style Modules

### Flat Units (Traditional)

**Simple qualified names:**
```pascal
program MyGame;
uses MathUtils, GameTypes;

var
  result: integer;
  point: GameTypes.TPoint;
begin
  result := MathUtils.Add(5, 3);
  point := GameTypes.CreatePoint(10, 20);
end.
```

### Nested Modules (Rust-Style)

**Qualified names with dots:**
```pascal
program MyGame;
uses math.Vector2D, graphics.sprites.Sprite;

var
  vec: math.Vector2D.TVector2D;
  sprite: graphics.sprites.Sprite.TSprite;
begin
  vec := math.Vector2D.Create(10, 20);
  sprite := graphics.sprites.Sprite.Create;
end.
```

**Using namespace aliases:**
```pascal
program MyGame;
uses math.Vector2D, graphics.sprites.Sprite;

namespace Vec = math.Vector2D;      // Shorter alias
namespace Spr = graphics.sprites.Sprite;  // Shorter alias

var
  vec: Vec.TVector2D;        // Use alias
  sprite: Spr.TSprite;       // Use alias
begin
  vec := Vec.Create(10, 20);
  sprite := Spr.Create;
end.
```

**Using `using` statements:**
```pascal
program MyGame;
uses math.Vector2D, graphics.sprites.Sprite;

using math.Vector2D;  // Bring into scope

var
  vec: TVector2D;                    // Unqualified (from Vector2D)
  sprite: graphics.sprites.Sprite.TSprite;  // Still qualified
begin
  vec := Create(10, 20);             // Unqualified function call
  sprite := graphics.sprites.Sprite.Create;
end.
```

---

## Common Patterns

### Pattern 1: Utility Unit

**Export only what's needed:**
```pascal
unit StringUtils;
interface
  // Exported: Public API
  function UpperCase(s: string): string;
  function LowerCase(s: string): string;
  function Trim(s: string): string;
  
implementation
  // Private: Helper functions
  function IsWhitespace(c: char): boolean;
  begin
    IsWhitespace := (c = ' ') or (c = #9) or (c = #10) or (c = #13);
  end;
  
  function UpperCase(s: string): string;
  var
    i: integer;
  begin
    // Implementation uses private helper
    // ...
  end;
  
  // ... other implementations
end.
```

### Pattern 2: Type Library

**Export types and related functions:**
```pascal
unit Geometry;
interface
  // Exported types
  type
    TPoint = record
      X, Y: integer;
    end;
    
    TRect = record
      Left, Top, Right, Bottom: integer;
    end;
  
  // Exported functions for types
  function CreatePoint(x, y: integer): TPoint;
  function CreateRect(left, top, right, bottom: integer): TRect;
  function PointInRect(p: TPoint; r: TRect): boolean;
  
implementation
  function CreatePoint(x, y: integer): TPoint;
  begin
    CreatePoint.X := x;
    CreatePoint.Y := y;
  end;
  
  // ... other implementations
end.
```

### Pattern 3: Constants Library

**Export constants and related types:**
```pascal
unit GameConstants;
interface
  // Exported constants
  const
    SCREEN_WIDTH = 320;
    SCREEN_HEIGHT = 240;
    TILE_SIZE = 16;
    MAX_ENTITIES = 256;
  
  // Exported types for constants
  type
    TScreenSize = record
      Width, Height: integer;
    end;
  
  function GetScreenSize: TScreenSize;
  
implementation
  function GetScreenSize: TScreenSize;
  begin
    GetScreenSize.Width := SCREEN_WIDTH;
    GetScreenSize.Height := SCREEN_HEIGHT;
  end;
end.
```

---

## Best Practices

### 1. Export Only What's Needed

**❌ Bad: Exporting everything**
```pascal
unit BadUnit;
interface
  // Exports internal helpers that shouldn't be public
  function InternalHelper(x: integer): integer;
  type TInternal = record
    Data: integer;
  end;
end.
```

**✅ Good: Export only public API**
```pascal
unit GoodUnit;
interface
  // Only export what users need
  function PublicAPI(x: integer): integer;
  
implementation
  // Keep internal details private
  function InternalHelper(x: integer): integer;
  begin
    // ...
  end;
  
  function PublicAPI(x: integer): integer;
  begin
    PublicAPI := InternalHelper(x);
  end;
end.
```

### 2. Avoid Global Variables

**❌ Bad: Exporting global variables**
```pascal
unit BadState;
interface
  var
    GlobalCounter: integer;  // Anyone can modify
    GlobalFlag: boolean;
end.
```

**✅ Good: Use functions for state**
```pascal
unit GoodState;
interface
  function GetCounter: integer;
  procedure SetCounter(value: integer);
  function GetFlag: boolean;
  procedure SetFlag(value: boolean);
  
implementation
  var
    Counter: integer;  // Private
    Flag: boolean;     // Private
    
  function GetCounter: integer;
  begin
    GetCounter := Counter;
  end;
  
  procedure SetCounter(value: integer);
  begin
    Counter := value;
  end;
  
  // ... other functions
end.
```

### 3. Use Qualified Names for Clarity

**❌ Bad: Unclear which unit a symbol comes from**
```pascal
program Unclear;
uses UnitA, UnitB, UnitC;

var
  point: TPoint;  // Which unit? Unclear!
begin
  // ...
end.
```

**✅ Good: Explicit qualified names**
```pascal
program Clear;
uses UnitA, UnitB, UnitC;

var
  point: UnitA.TPoint;  // Clear: from UnitA
begin
  // ...
end.
```

### 4. Organize Related Symbols Together

**✅ Good: Group related exports**
```pascal
unit Geometry;
interface
  // Types first
  type
    TPoint = record
      X, Y: integer;
    end;
    
    TRect = record
      Left, Top, Right, Bottom: integer;
    end;
  
  // Then functions
  function CreatePoint(x, y: integer): TPoint;
  function CreateRect(left, top, right, bottom: integer): TRect;
  function PointInRect(p: TPoint; r: TRect): boolean;
end.
```

---

## Exercises

### Exercise 1: Create a Utility Unit

Create a unit called `StringUtils` that exports:
- `UpperCase(s: string): string` — Convert string to uppercase
- `LowerCase(s: string): string` — Convert string to lowercase
- `Trim(s: string): string` — Remove leading/trailing whitespace

Keep helper functions private in the implementation section.

### Exercise 2: Resolve Name Conflicts

Create two units:
- `UnitA` exports `TPoint` with `X, Y: integer`
- `UnitB` exports `TPoint` with `R, G, B: byte`

Write a program that uses both units and creates variables of both `TPoint` types using qualified names.

### Exercise 3: Build a Constants Library

Create a unit called `GameConfig` that exports:
- Constants: `SCREEN_WIDTH`, `SCREEN_HEIGHT`, `FPS`
- Type: `TScreenConfig` record with width, height, fps
- Function: `GetDefaultConfig: TScreenConfig`

Use the function to access constants instead of exporting variables.

---

**Previous Section:** [Interface vs Implementation](./01_InterfaceVsImplementation.md)  
**Next Section:** [Building Larger Projects](./03_BuildingLargerProjects.md)  
**Language Specification:** See [02_Grammar.md](../../languageSpecification/02_Grammar.md) and [04_Semantics.md](../../languageSpecification/04_Semantics.md)  
**Last Updated:** 2025-01-XX

