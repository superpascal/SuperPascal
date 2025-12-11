# Building Larger Projects

**Part of:** [Chapter 08: Units and Modular Programming](./README.md)

---

## Introduction

As your programs grow, you'll need to organize code across multiple units. This section teaches you how to:
- **Manage unit dependencies** — Control which units depend on which
- **Organize project structure** — Arrange files logically
- **Use unit initialization** — Set up units when the program starts
- **Build multi-file projects** — Compile and link multiple units

**Key concepts:**
- **Dependency order** — Units must be compiled in the right order
- **Circular dependencies** — What happens when units depend on each other
- **Initialization blocks** — Code that runs at program start
- **Project organization** — How to structure files and directories

---

## Unit Dependencies

### What is a Dependency?

A **dependency** occurs when one unit uses symbols from another unit.

**Example:**
```pascal
// Unit A: Geometry.pas
unit Geometry;
interface
  type
    TPoint = record
      X, Y: integer;
    end;
  function Distance(p1, p2: TPoint): integer;
end.

// Unit B: GameUtils.pas (depends on Geometry)
unit GameUtils;
interface
  uses Geometry;  // GameUtils depends on Geometry
  
  function PlayerDistance(p1, p2: Geometry.TPoint): integer;
implementation
  uses Geometry;
  
  function PlayerDistance(p1, p2: Geometry.TPoint): integer;
  begin
    PlayerDistance := Geometry.Distance(p1, p2);
  end;
end.
```

**Dependency chain:**
- `GameUtils` depends on `Geometry`
- `Geometry` has no dependencies

### Declaring Dependencies

**Dependencies are declared in `uses` clauses:**

```pascal
unit MyUnit;
interface
  uses
    UnitA,      // Dependencies for interface
    UnitB;
  
  // Can use types, constants, functions from UnitA and UnitB
  function DoSomething: UnitA.TSomeType;
  
implementation
  uses
    UnitC,      // Additional dependencies for implementation
    UnitD;
  
  // Can use UnitA, UnitB, UnitC, UnitD in implementation
  function DoSomething: UnitA.TSomeType;
  begin
    // Implementation
  end;
end.
```

**Rules:**
- **Interface `uses`**: Units needed for exported symbols
- **Implementation `uses`**: Units needed only for implementation
- **Dependency order**: Units are initialized in dependency order

### Dependency Order

**Units are initialized in dependency order:**

```pascal
// Unit A (no dependencies)
unit UnitA;
interface
  const VALUE = 42;
end.

// Unit B (depends on UnitA)
unit UnitB;
interface
  uses UnitA;
  const VALUE2 = UnitA.VALUE * 2;
end.

// Unit C (depends on UnitB, which depends on UnitA)
unit UnitC;
interface
  uses UnitB;
  const VALUE3 = UnitB.VALUE2 + 1;
end.

// Program (depends on UnitC)
program MyProgram;
uses UnitC;
begin
  WriteLn(UnitC.VALUE3);  // Output: 85
end.
```

**Initialization order:**
1. `UnitA` (no dependencies)
2. `UnitB` (depends on UnitA)
3. `UnitC` (depends on UnitB)
4. Program main block

### Circular Dependencies

**Circular dependencies are not allowed:**

```pascal
// Unit A depends on Unit B
unit UnitA;
interface
  uses UnitB;  // UnitA needs UnitB
  // ...
end.

// Unit B depends on Unit A
unit UnitB;
interface
  uses UnitA;  // UnitB needs UnitA
  // ERROR: Circular dependency!
end.
```

**Why circular dependencies are bad:**
- Compiler can't determine initialization order
- Creates tight coupling (units can't be separated)
- Makes code harder to understand and maintain

**Solutions:**
1. **Refactor** — Extract common code to a third unit
2. **Move dependency** — Move one dependency to implementation section
3. **Redesign** — Restructure to eliminate circular dependency

**Example refactoring:**
```pascal
// Extract common types to a shared unit
unit CommonTypes;
interface
  type
    TPoint = record
      X, Y: integer;
    end;
end.

// Unit A uses CommonTypes
unit UnitA;
interface
  uses CommonTypes;
  function ProcessPoint(p: CommonTypes.TPoint): integer;
end.

// Unit B uses CommonTypes
unit UnitB;
interface
  uses CommonTypes;
  function TransformPoint(p: CommonTypes.TPoint): CommonTypes.TPoint;
end.

// Now UnitA and UnitB can both use CommonTypes without circular dependency
```

---

## Unit Initialization

### Initialization Blocks

**Units can have initialization code that runs at program start:**

```pascal
unit GameState;
interface
  var
    Score: integer;
    Level: integer;
implementation
begin
  // Initialization code runs when program starts
  Score := 0;
  Level := 1;
  WriteLn('GameState initialized');
end.
```

**Initialization block syntax:**
```pascal
unit UnitName;
interface
  // Declarations
implementation
  // Implementation code
begin
  // Initialization code (optional)
  // Runs once at program start
end.
```

### Initialization Order

**Units are initialized in dependency order:**

```pascal
// Unit A
unit UnitA;
implementation
begin
  WriteLn('UnitA initialized');
end.

// Unit B (depends on UnitA)
unit UnitB;
interface
  uses UnitA;
implementation
begin
  WriteLn('UnitB initialized');
end.

// Program
program MyProgram;
uses UnitB;
begin
  WriteLn('Program main');
end.
```

**Output:**
```
UnitA initialized
UnitB initialized
Program main
```

**Why this order matters:**
- `UnitB` depends on `UnitA`, so `UnitA` must be initialized first
- If `UnitB` uses `UnitA` in its initialization, `UnitA` is already ready

### Common Initialization Patterns

**Pattern 1: Initialize global state**

```pascal
unit GameConfig;
interface
  var
    ScreenWidth: integer;
    ScreenHeight: integer;
    FPS: integer;
implementation
begin
  ScreenWidth := 320;
  ScreenHeight := 240;
  FPS := 60;
  WriteLn('GameConfig: ', ScreenWidth, 'x', ScreenHeight, ' @ ', FPS, ' FPS');
end.
```

**Pattern 2: Initialize hardware**

```pascal
unit Graphics;
interface
  procedure InitGraphics;
  procedure DrawPixel(x, y: integer; color: word);
implementation
  var
    Initialized: boolean;
  
  procedure InitGraphics;
  begin
    // Initialize graphics hardware
    Initialized := true;
  end;
  
  procedure DrawPixel(x, y: integer; color: word);
  begin
    if not Initialized then
      InitGraphics;
    // Draw pixel
  end;
  
begin
  // Auto-initialize on program start
  InitGraphics;
end.
```

**Pattern 3: Load configuration**

```pascal
unit Config;
interface
  function GetValue(key: string): string;
implementation
  var
    ConfigLoaded: boolean;
  
  procedure LoadConfig;
  begin
    // Load configuration from file or defaults
    ConfigLoaded := true;
  end;
  
  function GetValue(key: string): string;
  begin
    if not ConfigLoaded then
      LoadConfig;
    // Return value
  end;
  
begin
  LoadConfig;  // Load on startup
end.
```

### When to Use Initialization

**✅ Good uses:**
- Setting default values for global state
- Initializing hardware or resources
- Loading configuration data
- Setting up lookup tables
- Registering callbacks or handlers

**❌ Avoid:**
- Complex logic (keep initialization simple)
- User interaction (program hasn't started yet)
- Heavy computation (slows program startup)
- Error-prone operations (hard to debug)

---

## Project Organization

### Flat Structure (Small Projects)

**For small projects, flat structure is fine:**

```
MyGame/
├── Main.pas          # Program file
├── GameUtils.pas     # Utility functions
├── GameTypes.pas     # Type definitions
└── GameState.pas     # Game state management
```

**Example:**
```pascal
// Main.pas
program MyGame;
uses GameUtils, GameTypes, GameState;
begin
  // Main game loop
end.

// GameTypes.pas
unit GameTypes;
interface
  type
    TPlayer = record
      X, Y: integer;
    end;
end.

// GameUtils.pas
unit GameUtils;
interface
  uses GameTypes;
  function Distance(p1, p2: TPlayer): integer;
end.

// GameState.pas
unit GameState;
interface
  uses GameTypes;
  var Players: array[0..3] of TPlayer;
end.
```

### Hierarchical Structure (Larger Projects)

**For larger projects, use directories:**

```
MyGame/
├── Main.pas
├── core/
│   ├── mod.pas              # unit core;
│   ├── Types.pas             # unit core.Types;
│   └── Utils.pas             # unit core.Utils;
├── graphics/
│   ├── mod.pas               # unit graphics;
│   ├── Renderer.pas          # unit graphics.Renderer;
│   └── sprites/
│       ├── mod.pas           # unit graphics.sprites;
│       └── Sprite.pas        # unit graphics.sprites.Sprite;
├── audio/
│   ├── mod.pas               # unit audio;
│   └── Sound.pas             # unit audio.Sound;
└── game/
    ├── mod.pas               # unit game;
    ├── Player.pas             # unit game.Player;
    └── Scene.pas              # unit game.Scene;
```

**Example usage:**
```pascal
// Main.pas
program MyGame;
uses
  core,
  core.Types,
  graphics,
  graphics.Renderer,
  graphics.sprites.Sprite,
  audio,
  game,
  game.Player,
  game.Scene;

begin
  // Main game loop
end.
```

### Organizing by Feature

**Group related functionality together:**

```
MyGame/
├── Main.pas
├── math/
│   ├── mod.pas
│   ├── Vector2D.pas
│   └── Matrix.pas
├── physics/
│   ├── mod.pas
│   ├── Collision.pas
│   └── Gravity.pas
├── rendering/
│   ├── mod.pas
│   ├── Renderer.pas
│   └── Sprite.pas
└── gameplay/
    ├── mod.pas
    ├── Player.pas
    └── Enemy.pas
```

### Organizing by Layer

**Separate layers of your application:**

```
MyGame/
├── Main.pas
├── platform/          # Platform-specific code
│   ├── mod.pas
│   └── Hardware.pas
├── engine/            # Game engine
│   ├── mod.pas
│   ├── ECS.pas
│   └── Scene.pas
├── game/              # Game-specific code
│   ├── mod.pas
│   ├── Player.pas
│   └── Level.pas
└── utils/             # Utilities
    ├── mod.pas
    └── StringUtils.pas
```

---

## Building Multi-File Projects

### Compilation Process

**SuperPascal compiles units separately:**

1. **Compile units** — Each unit compiles to `.ZOU` (Zeal Object Unit) file
2. **Link program** — Linker combines units into final `.ZOF` (Zeal Object File)
3. **Resolve dependencies** — Linker ensures all dependencies are satisfied

**Example compilation:**
```bash
# Compile units (in dependency order)
spc Geometry.pas          # → Geometry.ZOU
spc GameUtils.pas          # → GameUtils.ZOU (uses Geometry)
spc GameState.pas          # → GameState.ZOU (uses GameTypes)

# Compile and link program
spc Main.pas               # Compiles Main, links with units
```

### Dependency Resolution

**Compiler automatically resolves dependencies:**

```pascal
// Main.pas
program MyGame;
uses GameUtils, GameState;  // Compiler finds GameUtils, GameState
begin
  // ...
end.
```

**Compiler search order:**
1. Current directory
2. Compiler search paths (`-I` flag)
3. Standard library directory

**Example with search paths:**
```bash
spc -I./lib -I./utils Main.pas
```

### Building Libraries

**Create reusable libraries:**

```
MyLibrary/
├── mod.pas              # unit MyLibrary;
├── Math.pas             # unit MyLibrary.Math;
├── StringUtils.pas      # unit MyLibrary.StringUtils;
└── Types.pas            # unit MyLibrary.Types;
```

**Using the library:**
```pascal
program MyProgram;
uses
  MyLibrary,
  MyLibrary.Math,
  MyLibrary.StringUtils;

begin
  // Use library functions
end.
```

**Compile library:**
```bash
# Compile library units
spc MyLibrary/mod.pas
spc MyLibrary/Math.pas
spc MyLibrary/StringUtils.pas
spc MyLibrary/Types.pas

# Use library in program
spc -I./MyLibrary MyProgram.pas
```

---

## Best Practices

### 1. Keep Dependencies Minimal

**❌ Bad: Too many dependencies**
```pascal
unit BadUnit;
interface
  uses UnitA, UnitB, UnitC, UnitD, UnitE, UnitF;  // Too many!
end.
```

**✅ Good: Only necessary dependencies**
```pascal
unit GoodUnit;
interface
  uses UnitA, UnitB;  // Only what's needed
end.
```

### 2. Avoid Circular Dependencies

**❌ Bad: Circular dependency**
```pascal
unit UnitA;
interface
  uses UnitB;  // UnitA depends on UnitB
end.

unit UnitB;
interface
  uses UnitA;  // UnitB depends on UnitA (circular!)
end.
```

**✅ Good: Extract common code**
```pascal
unit Common;
interface
  // Common types and functions
end.

unit UnitA;
interface
  uses Common;  // Both use Common
end.

unit UnitB;
interface
  uses Common;  // No circular dependency
end.
```

### 3. Organize by Responsibility

**✅ Good: Clear separation of concerns**
```
MyGame/
├── math/          # Math utilities
├── graphics/      # Graphics code
├── audio/         # Audio code
└── game/          # Game logic
```

**❌ Bad: Everything mixed together**
```
MyGame/
├── Stuff.pas      # Contains math, graphics, audio, game logic
└── MoreStuff.pas  # More mixed code
```

### 4. Use Meaningful Names

**✅ Good: Clear, descriptive names**
```pascal
unit graphics.Renderer;      // Clear purpose
unit math.Vector2D;          // Clear purpose
unit game.Player;            // Clear purpose
```

**❌ Bad: Vague or unclear names**
```pascal
unit Utils;        // Too vague
unit Stuff;        // Meaningless
unit Helper;       // Unclear what it helps with
```

### 5. Keep Initialization Simple

**✅ Good: Simple initialization**
```pascal
unit Config;
implementation
begin
  DefaultValue := 42;
  Initialized := true;
end.
```

**❌ Bad: Complex initialization**
```pascal
unit BadUnit;
implementation
begin
  // 100 lines of complex initialization code
  // Hard to debug, slow startup
end.
```

### 6. Document Dependencies

**✅ Good: Clear dependency documentation**
```pascal
unit GameUtils;
interface
  // Dependencies:
  //   - Geometry: For TPoint and distance calculations
  //   - GameTypes: For TPlayer type
  uses Geometry, GameTypes;
end.
```

---

## Complete Example: Multi-Unit Game

**Project structure:**
```
MyGame/
├── Main.pas
├── core/
│   ├── mod.pas
│   └── Types.pas
├── graphics/
│   ├── mod.pas
│   └── Renderer.pas
└── game/
    ├── mod.pas
    └── Player.pas
```

**core/Types.pas:**
```pascal
unit core.Types;
interface
  type
    TPoint = record
      X, Y: integer;
    end;
    
    TColor = word;
end.
```

**graphics/Renderer.pas:**
```pascal
unit graphics.Renderer;
interface
  uses core.Types;
  procedure DrawPixel(x, y: integer; color: core.Types.TColor);
  procedure DrawLine(x1, y1, x2, y2: integer; color: core.Types.TColor);
implementation
  procedure DrawPixel(x, y: integer; color: core.Types.TColor);
  begin
    // Draw pixel implementation
  end;
  
  procedure DrawLine(x1, y1, x2, y2: integer; color: core.Types.TColor);
  begin
    // Draw line implementation
  end;
end.
```

**game/Player.pas:**
```pascal
unit game.Player;
interface
  uses core.Types, graphics.Renderer;
  type
    TPlayer = record
      Position: core.Types.TPoint;
      Color: core.Types.TColor;
    end;
  procedure DrawPlayer(player: TPlayer);
implementation
  procedure DrawPlayer(player: TPlayer);
  begin
    graphics.Renderer.DrawPixel(
      player.Position.X,
      player.Position.Y,
      player.Color
    );
  end;
end.
```

**Main.pas:**
```pascal
program MyGame;
uses
  core.Types,
  graphics.Renderer,
  game.Player;

var
  player: game.Player.TPlayer;
begin
  player.Position.X := 160;
  player.Position.Y := 120;
  player.Color := $1F00;  // Red
  
  game.Player.DrawPlayer(player);
end.
```

**Compilation:**
```bash
# Compile units (in dependency order)
spc core/Types.pas
spc graphics/Renderer.pas
spc game/Player.pas

# Compile and link program
spc Main.pas
```

---

## Exercises

### Exercise 1: Create a Multi-Unit Project

Create a project with three units:
- `MathUtils` — Math functions (Add, Multiply, Distance)
- `Geometry` — Geometry types (TPoint, TRect) and functions
- `Game` — Game logic that uses both MathUtils and Geometry

Organize them in a logical directory structure.

### Exercise 2: Unit Initialization

Create a unit called `Config` that:
- Exports a function `GetValue(key: string): string`
- Initializes default values in an initialization block
- Loads configuration on program start

### Exercise 3: Resolve Circular Dependency

Given two units that depend on each other:
- `UnitA` needs `UnitB.TType`
- `UnitB` needs `UnitA.TType`

Refactor to eliminate the circular dependency by extracting common types to a third unit.

### Exercise 4: Build a Library

Create a small library with:
- `MyLib` — Main library unit
- `MyLib.Math` — Math functions
- `MyLib.StringUtils` — String utilities
- `MyLib.Types` — Common types

Build the library and use it in a separate program.

---

**Previous Section:** [Exported Symbols](./02_ExportedSymbols.md)  
**Next Chapter:** [Chapter 09: Graphics](../11_Graphics/README.md)  
**Language Specification:** See [02_Grammar.md](../../languageSpecification/02_Grammar.md) and [04_Semantics.md](../../languageSpecification/04_Semantics.md)  
**Last Updated:** 2025-01-XX

