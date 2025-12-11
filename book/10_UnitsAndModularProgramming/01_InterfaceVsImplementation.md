# Interface vs Implementation

**Part of:** [Chapter 10: Units and Modular Programming](./README.md)

---

> **For GCSE students:**  
> Units are like toolboxes - you put related tools (functions and types) together in one box. The interface is like the label on the box telling you what's inside, and the implementation is the actual tools.
>
> **For A-Level students:**  
> Units provide modularity by separating public interface from private implementation. The interface declares what's available, while the implementation contains the actual code. This enables code reuse and maintainability.
>
> **For University students:**  
> Units implement the module abstraction, providing encapsulation, information hiding, and separate compilation. Understanding unit interfaces, implementation details, and dependency management is essential for large-scale software development and software architecture.

---

## Introduction

**Units** are SuperPascal's way of organizing code into reusable modules. They allow you to:
- **Separate interface from implementation** — Hide implementation details
- **Organize code** — Group related functionality together
- **Build libraries** — Create reusable code for multiple programs
- **Manage dependencies** — Control what code depends on what

**Why units matter:**
- **Modularity** — Break large programs into manageable pieces
- **Reusability** — Write code once, use it many times
- **Maintainability** — Change implementation without breaking users
- **Encapsulation** — Hide complexity behind a simple interface

---

## What is a Unit?

A **unit** is a separate file that contains:
1. **Interface section** — Public API (what other code can use)
2. **Implementation section** — Private code (how it works)

**Basic structure:**
```pascal
unit UnitName;
interface
  // Public declarations (exported)
implementation
  // Private code and implementations
end.
```

**Example: A simple math unit**
```pascal
unit MathUtils;
interface
  function Add(a, b: integer): integer;
  function Multiply(a, b: integer): integer;
implementation
  function Add(a, b: integer): integer;
  begin
    Add := a + b;
  end;
  
  function Multiply(a, b: integer): integer;
  begin
    Multiply := a * b;
  end;
end.
```

---

## Interface Section

The **interface section** defines what the unit **exports** (makes available to other code).

### What Goes in Interface?

**Public declarations:**
- **Types** — Type definitions that other code can use
- **Constants** — Named values
- **Procedures and Functions** — Routines that can be called
- **Variables** — Global variables (rare, usually avoid)

**Example:**
```pascal
unit GameUtils;
interface
  // Types
  type
    TPoint = record
      X, Y: integer;
    end;
  
  // Constants
  const
    MAX_PLAYERS = 4;
    SCREEN_WIDTH = 320;
  
  // Functions
  function Distance(p1, p2: TPoint): integer;
  procedure DrawText(x, y: integer; text: string);
  
  // Variables (rare, but possible)
  var
    GameScore: integer;
implementation
  // Implementation comes next...
end.
```

### Interface Rules

**Important rules:**
1. **No implementation** — Interface only declares, never implements
2. **Forward declarations** — Function signatures only, no body
3. **Public by default** — Everything in interface is exported
4. **Order matters** — Declare types before using them

**Example: Forward declaration**
```pascal
unit Graphics;
interface
  // Function signature (forward declaration)
  procedure DrawSprite(x, y: integer; spriteID: byte);
  
  // Type used in function
  type
    TColor = (Red, Green, Blue);
  
  // Function using the type
  procedure SetColor(color: TColor);
implementation
  // Implementation here
end.
```

---

## Implementation Section

The **implementation section** contains the **actual code** that makes the interface work.

### What Goes in Implementation?

**Private code:**
- **Function/procedure bodies** — Implementations of interface declarations
- **Helper routines** — Private functions not in interface
- **Private types** — Types only used internally
- **Private variables** — Internal state
- **Initialization code** — Code that runs when unit loads

**Example:**
```pascal
unit StringUtils;
interface
  function Reverse(const str: string): string;
  function ToUpper(const str: string): string;
implementation
  // Helper function (not exported, only used internally)
  function IsLower(ch: char): boolean;
  begin
    IsLower := (ch >= 'a') and (ch <= 'z');
  end;
  
  // Implementation of exported function
  function Reverse(const str: string): string;
  var
    i: integer;
    result: string;
  begin
    result := '';
    for i := Length(str) downto 1 do
      result := result + str[i];
    Reverse := result;
  end;
  
  // Implementation of exported function
  function ToUpper(const str: string): string;
  var
    i: integer;
    ch: char;
    result: string;
  begin
    result := '';
    for i := 1 to Length(str) do
    begin
      ch := str[i];
      if IsLower(ch) then
        ch := Chr(Ord(ch) - 32);  // Convert to uppercase
      result := result + ch;
    end;
    ToUpper := result;
  end;
end.
```

### Implementation Rules

**Important rules:**
1. **Must implement** — Every interface declaration must have an implementation
2. **Can add more** — Implementation can have extra private code
3. **Private by default** — Code in implementation is not exported
4. **Can use helpers** — Private functions can help implement public ones

---

## Public vs Private

Understanding **public** (interface) vs **private** (implementation) is crucial.

### Public (Interface)

**What is public:**
- Everything declared in `interface` section
- Can be used by code that `uses` the unit
- Part of the unit's API (Application Programming Interface)

**Example:**
```pascal
unit MathLib;
interface
  function Square(x: integer): integer;  // Public
implementation
  // ...
end.
```

**Using public functions:**
```pascal
program MyProgram;
uses MathLib;  // Import the unit
begin
  WriteLn(Square(5));  // Can use Square (it's public)
end.
```

### Private (Implementation)

**What is private:**
- Everything in `implementation` section that's not in interface
- Helper functions, private types, internal variables
- Cannot be used by code outside the unit

**Example:**
```pascal
unit MathLib;
interface
  function Square(x: integer): integer;  // Public
implementation
  // Private helper function
  function Multiply(a, b: integer): integer;  // Private
  begin
    Multiply := a * b;
  end;
  
  // Public function implementation
  function Square(x: integer): integer;
  begin
    Square := Multiply(x, x);  // Uses private helper
  end;
end.
```

**Cannot use private functions:**
```pascal
program MyProgram;
uses MathLib;
begin
  // WriteLn(Multiply(3, 4));  // ERROR: Multiply is private!
  WriteLn(Square(5));  // OK: Square is public
end.
```

---

## Complete Example

**A complete unit example:**
```pascal
unit Vector2D;
interface
  // Public type
  type
    TVector2D = record
      X, Y: integer;
    end;
  
  // Public functions
  function VectorAdd(v1, v2: TVector2D): TVector2D;
  function VectorScale(v: TVector2D; scalar: integer): TVector2D;
  function VectorLength(v: TVector2D): integer;
  
implementation
  // Private helper function
  function Sqr(x: integer): integer;
  begin
    Sqr := x * x;
  end;
  
  // Public function implementations
  function VectorAdd(v1, v2: TVector2D): TVector2D;
  begin
    VectorAdd.X := v1.X + v2.X;
    VectorAdd.Y := v1.Y + v2.Y;
  end;
  
  function VectorScale(v: TVector2D; scalar: integer): TVector2D;
  begin
    VectorScale.X := v.X * scalar;
    VectorScale.Y := v.Y * scalar;
  end;
  
  function VectorLength(v: TVector2D): integer;
  var
    lenSquared: integer;
  begin
    lenSquared := Sqr(v.X) + Sqr(v.Y);
    // Simple approximation (avoid square root for now)
    VectorLength := lenSquared;  // Return squared length
  end;
end.
```

**Using the unit:**
```pascal
program Game;
uses Vector2D;
var
  v1, v2, result: TVector2D;
begin
  v1.X := 10;
  v1.Y := 20;
  v2.X := 5;
  v2.Y := 15;
  
  result := VectorAdd(v1, v2);
  WriteLn('Result: (', result.X, ', ', result.Y, ')');
  
  // Can use TVector2D type and public functions
  // Cannot use Sqr (it's private)
end.
```

---

## Best Practices

### 1. Keep Interface Minimal

**Good:**
```pascal
interface
  function Calculate(x: integer): integer;  // Simple, clear API
implementation
  // Complex implementation hidden
end.
```

**Bad:**
```pascal
interface
  function Calculate(x: integer): integer;
  function Helper1(x: integer): integer;  // Should be private
  function Helper2(x: integer): integer;  // Should be private
implementation
  // ...
end.
```

### 2. Use Private Helpers

**Break complex functions into smaller private helpers:**
```pascal
implementation
  function ValidateInput(x: integer): boolean;  // Private helper
  begin
    ValidateInput := (x >= 0) and (x <= 100);
  end;
  
  function Process(x: integer): integer;  // Private helper
  begin
    Process := x * 2;
  end;
  
  function Calculate(x: integer): integer;  // Public function
  begin
    if ValidateInput(x) then
      Calculate := Process(x)
    else
      Calculate := 0;
  end;
end.
```

### 3. Document the Interface

**Add comments to explain the public API:**
```pascal
interface
  // Calculates the distance between two points
  // Returns: Distance squared (to avoid square root)
  function Distance(p1, p2: TPoint): integer;
  
  // Draws text at specified position
  // x, y: Screen coordinates (0-based)
  // text: String to display
  procedure DrawText(x, y: integer; text: string);
implementation
  // ...
end.
```

---

## Summary

**Key concepts:**
- **Units** organize code into modules
- **Interface** defines public API (what others can use)
- **Implementation** contains private code (how it works)
- **Public** = in interface = can be used by other code
- **Private** = only in implementation = hidden from other code

**Structure:**
```pascal
unit UnitName;
interface
  // Public declarations
implementation
  // Private code and implementations
end.
```

**Next:** Learn about exported symbols and how to use units in your programs.

---

## Unit Files and Organization

### File Naming Convention

**Unit name must match filename:**
- Unit `MathUtils` must be in file `MathUtils.pas`
- Unit `Graphics` must be in file `Graphics.pas`
- Case-sensitive on some platforms, case-insensitive on others

**Example:**
```pascal
// File: MathUtils.pas
unit MathUtils;  // Name matches filename
interface
  // ...
implementation
  // ...
end.
```

### Flat Structure (Traditional Pascal)

**Simple organization:**
```
MyProject/
├── Main.pas          # program Main
├── MathUtils.pas     # unit MathUtils
├── Graphics.pas      # unit Graphics
└── Audio.pas        # unit Audio
```

**All units in same directory:**
- Compiler looks in current directory
- Simple and straightforward
- Works well for small projects

### Subdirectories and Search Paths

**Organizing larger projects:**

SuperPascal supports organizing units in subdirectories using **unit search paths**:

```
MyGame/
├── Main.pas
├── utils/
│   ├── MathUtils.pas
│   └── StringUtils.pas
├── graphics/
│   ├── Graphics.pas
│   └── Sprites.pas
└── audio/
    └── Audio.pas
```

**Compiler search paths:**
- Current directory (always searched)
- Directories specified with `-I` or `--include` flag
- Standard library directory (if configured)

**Compiling with search paths:**
```bash
zc build Main.pas -I utils -I graphics -I audio
```

**Using units from subdirectories:**
```pascal
program MyGame;
uses
  MathUtils,      // Found in utils/ directory
  Graphics,       // Found in graphics/ directory
  Audio;          // Found in audio/ directory
begin
  // ...
end.
```

### Unit Name vs File Path

**Important rules:**
1. **Unit name is flat** — No dots or slashes in unit name
2. **File path can be nested** — File can be in subdirectory
3. **Compiler resolves path** — Searches directories to find unit file

**Example:**
```pascal
// File: utils/math/Vector2D.pas
unit Vector2D;  // Unit name is just "Vector2D", not "utils.math.Vector2D"
interface
  // ...
end.
```

**Using it:**
```pascal
program Game;
uses Vector2D;  // Just "Vector2D", compiler finds it in utils/math/
begin
  // ...
end.
```

### Project Organization Best Practices

**Small projects (flat):**
```
Project/
├── Main.pas
├── Utils.pas
└── Game.pas
```

**Medium projects (by feature):**
```
Project/
├── Main.pas
├── graphics/
│   ├── Graphics.pas
│   └── Sprites.pas
├── game/
│   ├── Game.pas
│   └── Player.pas
└── utils/
    └── MathUtils.pas
```

**Large projects (by layer):**
```
Project/
├── Main.pas
├── core/           # Core game systems
│   ├── ECS.pas
│   └── Physics.pas
├── graphics/       # Graphics layer
│   ├── Renderer.pas
│   └── Sprites.pas
├── audio/          # Audio layer
│   └── Audio.pas
└── utils/          # Shared utilities
    └── MathUtils.pas
```

### Rust-Style Module System (Planned)

SuperPascal will implement a **Rust-style module system** for better code organization:

#### Directory-Based Modules

**A directory can be a module:**
```
math/
├── mod.pas          # unit math; (module root)
└── Vector2D.pas     # unit math.Vector2D; (submodule)
```

**File structure:**
```pascal
// File: math/mod.pas
unit math;
interface
  // Public API for math module
  function Add(a, b: integer): integer;
implementation
  // ...
end.

// File: math/Vector2D.pas
unit math.Vector2D;
interface
  type
    TVector2D = record
      X, Y: integer;
    end;
  function VectorAdd(v1, v2: TVector2D): TVector2D;
implementation
  // ...
end.
```

#### Qualified Unit Names

**Use dot notation for nested modules:**
```pascal
program Game;
uses
  math,              // Import math module
  math.Vector2D;     // Import Vector2D submodule
var
  v: math.Vector2D.TVector2D;  // Qualified type name
begin
  v := math.Vector2D.VectorAdd(v1, v2);
end.
```

#### Module Resolution Rules

1. **File-based module**: `MathUtils.pas` → `unit MathUtils;`
2. **Directory module**: `math/mod.pas` → `unit math;`
3. **Nested module**: `math/vector/Vector2D.pas` → `unit math.vector.Vector2D;`
4. **Module search**: Compiler searches current directory and parent directories

#### Example Project Structure

```
MyGame/
├── Main.pas
├── math/
│   ├── mod.pas              # unit math;
│   └── Vector2D.pas         # unit math.Vector2D;
├── graphics/
│   ├── mod.pas              # unit graphics;
│   ├── Renderer.pas         # unit graphics.Renderer;
│   └── sprites/
│       ├── mod.pas          # unit graphics.sprites;
│       └── Sprite.pas       # unit graphics.sprites.Sprite;
└── utils/
    └── StringUtils.pas      # unit utils.StringUtils;
```

**Using nested modules:**
```pascal
program MyGame;
uses
  math,
  math.Vector2D,
  graphics,
  graphics.Renderer,
  graphics.sprites,
  graphics.sprites.Sprite;
begin
  // Use qualified names
  var v: math.Vector2D.TVector2D;
  var sprite: graphics.sprites.Sprite.TSprite;
end.
```

#### Benefits

- **Better organization** — Logical grouping of related code
- **Namespace safety** — Avoid name conflicts
- **Scalability** — Easy to grow large projects
- **Familiar** — Similar to Rust, C#, Java module systems

### Current Limitations

**Until Rust-style modules are implemented:**
- **No namespaces** — Unit names are flat (no `Utils.Math` syntax)
- **No relative imports** — Must use search paths or absolute paths
- **No circular dependencies** — Units cannot depend on each other circularly
- **File extension required** — Must use `.pas` extension

**Future enhancements (planned):**
- ✅ Rust-style module system (directory-based modules)
- ✅ Qualified unit names (`math.Vector2D`)
- ✅ Module resolution from directory structure
- ✅ **Namespace support** — Explicit `namespace` declarations and `using` statements
- ✅ **Namespace aliases** — Short names for long module paths
- ✅ **Global namespace access** — `::` operator for explicit global access
- ✅ Better circular dependency detection

### Namespace Support (Future Extension)

**Explicit namespace declarations:**
```pascal
// File: math/mod.pas
namespace math;
unit math;
interface
  function Add(a, b: integer): integer;
implementation
  // ...
end.
```

**Using statements (bring namespaces into scope):**
```pascal
program MyGame;
uses
  math,
  math.Vector2D;
  
using math;          // Bring math namespace into scope
using math.Vector2D;  // Bring Vector2D submodule into scope

var
  v: TVector2D;      // Can use unqualified name
begin
  v := VectorAdd(v1, v2);  // No need to qualify
  WriteLn(Add(5, 3));      // Uses math.Add
end.
```

**Namespace aliases:**
```pascal
program MyGame;
uses graphics.sprites.animation.SpriteAnimator;

namespace Anim = graphics.sprites.animation;  // Create alias

var
  anim: Anim.SpriteAnimator.TAnimation;  // Use shorter alias
begin
  anim := Anim.SpriteAnimator.CreateAnimation;
end.
```

**Global namespace access:**
```pascal
program MyGame;
uses math;
namespace MyMath = math;  // Local alias

var
  x: integer;
begin
  // Explicit global namespace access
  x := ::math.Add(5, 3);  // Use :: for global namespace
  // Or use qualified name
  x := math.Add(5, 3);
end.
```

**Benefits:**
- **Less verbosity** — Don't need to qualify every name
- **Familiar** — Similar to C++/C# namespaces
- **Flexible** — Can use qualified or unqualified names
- **Conflict resolution** — Explicit qualification when needed

---

## Exercises

### GCSE Level Exercises

**Exercise 1: Simple Unit**
Write a unit that:
1. Has an interface section with one procedure
2. Has an implementation section with the procedure code
3. Uses the unit in a main program
4. Calls the procedure from main

**Exercise 2: Math Unit**
Create a `MathUtils` unit that:
1. Exports functions: `Add`, `Subtract`, `Multiply`, `Divide`
2. Implements each function
3. Uses the unit in a program
4. Tests all functions

**Exercise 3: Two Units**
Create two units:
1. `Display` unit - exports `ShowMessage` procedure
2. `Input` unit - exports `GetNumber` function
3. Use both in a main program
4. Combine their functionality

### A-Level Exercises

**Exercise 1: Unit with Types**
Create a unit that:
1. Exports a record type (e.g., `TPoint`)
2. Exports procedures that work with the type
3. Hides implementation details
4. Uses the unit in multiple programs

**Exercise 2: Unit Dependencies**
Create units with dependencies:
1. `Geometry` unit - basic shapes
2. `Graphics` unit - uses `Geometry` unit
3. Main program - uses `Graphics` unit
4. Demonstrate dependency chain

**Exercise 3: Unit Organization**
Refactor a large program:
1. Split into logical units
2. Define clear interfaces
3. Organize by functionality
4. Test that everything still works

### University Level Exercises

**Exercise 1: Unit Design Patterns**
Design a unit system for:
1. Plugin architecture (units as plugins)
2. Dependency injection (units provide services)
3. Interface segregation (small, focused units)
4. Document design decisions

**Exercise 2: Unit Testing Framework**
Design a unit testing framework:
1. Test unit structure
2. Test discovery mechanism
3. Test execution system
4. Test reporting
5. Integration with build system

**Exercise 3: Circular Dependency Resolution**
Analyze and resolve circular dependencies:
1. Identify circular dependencies
2. Refactor to break cycles
3. Use dependency inversion
4. Measure impact on compilation
5. Document resolution strategy

---

**Previous Section:** [Chapter 09: Strings and Text Processing](../09_StringsAndTextProcessing/README.md)  
**Next Section:** [Exported Symbols](./02_ExportedSymbols.md)  
**Language Specification:** See [02_Grammar.md](../../languageSpecification/02_Grammar.md) and [04_Semantics.md](../../languageSpecification/04_Semantics.md)  
**Last Updated:** 2025-01-XX

