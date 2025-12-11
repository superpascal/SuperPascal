# SuperPascal Language Specification — Rust-Style Module System

## Module System Design

**Version:** 1.0 (Planned)  
**Part of:** SuperPascal Language Specification  
**Status:** Design phase — Planned for future release

---

## 1. Overview

SuperPascal will implement a **Rust-style module system** that allows organizing code into hierarchical modules using directory structure. This provides:

- **Better code organization** — Logical grouping of related functionality
- **Namespace safety** — Avoid naming conflicts
- **Scalability** — Easy to grow large projects
- **Familiar syntax** — Similar to Rust, C#, Java module systems

---

## 2. Module Structure

### 2.1 Directory-Based Modules

A **directory can be a module** by containing a `mod.pas` file:

```
math/
├── mod.pas          # unit math; (module root)
└── Vector2D.pas     # unit math.Vector2D; (submodule)
```

**Module root file (`mod.pas`):**
```pascal
// File: math/mod.pas
unit math;
interface
  // Public API for math module
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

**Submodule file:**
```pascal
// File: math/Vector2D.pas
unit math.Vector2D;
interface
  type
    TVector2D = record
      X, Y: integer;
    end;
  function VectorAdd(v1, v2: TVector2D): TVector2D;
  function VectorScale(v: TVector2D; scalar: integer): TVector2D;
implementation
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
end.
```

### 2.2 Nested Modules

Modules can be nested to any depth:

```
graphics/
├── mod.pas                    # unit graphics;
├── Renderer.pas               # unit graphics.Renderer;
└── sprites/
    ├── mod.pas                # unit graphics.sprites;
    └── Sprite.pas             # unit graphics.sprites.Sprite;
```

**Nested module example:**
```pascal
// File: graphics/sprites/Sprite.pas
unit graphics.sprites.Sprite;
interface
  type
    TSprite = record
      X, Y: integer;
      TileID: byte;
    end;
  procedure DrawSprite(sprite: TSprite);
implementation
  procedure DrawSprite(sprite: TSprite);
  begin
    // Draw sprite at position
  end;
end.
```

---

## 3. Qualified Unit Names

### 3.1 Syntax

**Qualified unit names use dot notation:**
```pascal
unit math.Vector2D;        // Two-level module
unit graphics.sprites.Sprite;  // Three-level module
unit utils.math.helpers.Calculator;  // Four-level module
```

### 3.2 Using Qualified Units

**Import qualified units:**
```pascal
program MyGame;
uses
  math,                      // Import math module
  math.Vector2D,            // Import Vector2D submodule
  graphics,                 // Import graphics module
  graphics.sprites.Sprite;  // Import Sprite submodule
var
  v: math.Vector2D.TVector2D;
  sprite: graphics.sprites.Sprite.TSprite;
begin
  v := math.Vector2D.VectorAdd(v1, v2);
  graphics.sprites.Sprite.DrawSprite(sprite);
end.
```

### 3.3 Qualified Type Names

**Access types using qualified names:**
```pascal
var
  v: math.Vector2D.TVector2D;           // Qualified type
  sprite: graphics.sprites.Sprite.TSprite;  // Qualified type
```

---

## 4. Module Resolution

### 4.1 Resolution Rules

The compiler resolves modules using the following rules:

1. **File-based module**: `MathUtils.pas` → `unit MathUtils;`
2. **Directory module**: `math/mod.pas` → `unit math;`
3. **Nested module**: `math/vector/Vector2D.pas` → `unit math.vector.Vector2D;`
4. **Search order**:
   - Current directory
   - Parent directories (walking up)
   - Compiler search paths (`-I` flag)
   - Standard library directory

### 4.2 Module File Naming

**Conventions:**
- **Module root**: `mod.pas` (contains `unit math;`)
- **Submodule**: `SubmoduleName.pas` (contains `unit math.SubmoduleName;`)
- **Case sensitivity**: Platform-dependent (case-insensitive on Windows, case-sensitive on Unix)

### 4.3 Example Resolution

**Project structure:**
```
MyGame/
├── Main.pas
├── math/
│   ├── mod.pas
│   └── Vector2D.pas
└── graphics/
    ├── mod.pas
    └── sprites/
        ├── mod.pas
        └── Sprite.pas
```

**Resolution:**
- `uses math;` → Finds `math/mod.pas`
- `uses math.Vector2D;` → Finds `math/Vector2D.pas`
- `uses graphics.sprites.Sprite;` → Finds `graphics/sprites/Sprite.pas`

---

## 5. Module Visibility

### 5.1 Public vs Private

**Same rules as traditional units:**
- **Interface section** → Public (exported)
- **Implementation section** → Private (not exported)

**Example:**
```pascal
// File: math/mod.pas
unit math;
interface
  function Add(a, b: integer): integer;  // Public
implementation
  function Helper(x: integer): integer;  // Private
  begin
    Helper := x * 2;
  end;
  
  function Add(a, b: integer): integer;  // Public implementation
  begin
    Add := Helper(a) + Helper(b);  // Can use private helper
  end;
end.
```

### 5.2 Cross-Module Access

**Accessing other modules:**
```pascal
// File: math/Vector2D.pas
unit math.Vector2D;
interface
  type
    TVector2D = record
      X, Y: integer;
    end;
  function VectorLength(v: TVector2D): integer;
implementation
  uses math;  // Can use parent module
  
  function VectorLength(v: TVector2D): integer;
  var
    lenSquared: integer;
  begin
    lenSquared := math.Add(v.X * v.X, v.Y * v.Y);
    VectorLength := lenSquared;  // Simplified (no sqrt)
  end;
end.
```

---

## 6. Migration from Flat Units

### 6.1 Compatibility

**Flat units still work:**
```pascal
// File: MathUtils.pas (flat structure)
unit MathUtils;  // Still valid
interface
  // ...
end.
```

**Both styles can coexist:**
```pascal
program Game;
uses
  MathUtils,        // Flat unit (old style)
  math.Vector2D;    // Qualified unit (new style)
begin
  // ...
end.
```

### 6.2 Migration Path

**Step 1: Create module directory:**
```
utils/
└── mod.pas  # unit utils;
```

**Step 2: Move units into module:**
```
utils/
├── mod.pas
└── MathUtils.pas  # unit utils.MathUtils;
```

**Step 3: Update uses clauses:**
```pascal
// Old
uses MathUtils;

// New
uses utils.MathUtils;
```

---

## 7. Best Practices

### 7.1 Module Organization

**Organize by feature:**
```
game/
├── mod.pas
├── Player.pas
├── Enemy.pas
└── Item.pas
```

**Organize by layer:**
```
core/
├── mod.pas
├── ECS.pas
└── Physics.pas
graphics/
├── mod.pas
└── Renderer.pas
```

### 7.2 Module Naming

**Use lowercase for module names:**
- ✅ `math`, `graphics`, `utils`
- ❌ `Math`, `Graphics`, `Utils` (reserved for types)

**Use PascalCase for submodules:**
- ✅ `math.Vector2D`, `graphics.Sprite`
- ❌ `math.vector2d`, `graphics.sprite`

### 7.3 Module Size

**Keep modules focused:**
- One module = one responsibility
- Related functionality grouped together
- Avoid overly deep nesting (max 3-4 levels)

---

## 8. Implementation Notes

### 8.1 Compiler Changes Required

1. **Parser**: Support qualified identifiers in `unit` and `uses` clauses
2. **Module resolver**: Walk directory structure to find modules
3. **Symbol table**: Handle qualified names in symbol resolution
4. **Codegen**: Generate qualified symbol names in object files

### 8.2 File System Requirements

- Support for `mod.pas` files in directories
- Case-sensitive vs case-insensitive file systems
- Path resolution (relative vs absolute)

### 8.3 Backward Compatibility

- Flat units (`unit Math;`) must continue to work
- Existing projects should compile without changes
- Gradual migration path for existing code

---

## 9. Examples

### 9.1 Complete Example

**Project structure:**
```
MyGame/
├── Main.pas
├── math/
│   ├── mod.pas
│   └── Vector2D.pas
└── graphics/
    ├── mod.pas
    └── Renderer.pas
```

**Main program:**
```pascal
// File: Main.pas
program MyGame;
uses
  math,
  math.Vector2D,
  graphics,
  graphics.Renderer;
var
  v: math.Vector2D.TVector2D;
begin
  v.X := 10;
  v.Y := 20;
  graphics.Renderer.DrawVector(v);
end.
```

### 9.2 Nested Module Example

**Deep nesting:**
```
engine/
├── mod.pas
├── core/
│   ├── mod.pas
│   └── ECS.pas
└── graphics/
    ├── mod.pas
    └── renderer/
        ├── mod.pas
        └── Renderer.pas
```

**Using nested modules:**
```pascal
program Game;
uses
  engine,
  engine.core.ECS,
  engine.graphics.renderer.Renderer;
begin
  // Use deeply nested modules
end.
```

---

## 10. Namespace Support (Extension)

The Rust-style module system can be extended with **explicit namespace support** for even better code organization and name resolution.

### 10.1 Namespace Declarations

**Explicit namespace syntax (C++/C# style):**
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

**Or inline namespace:**
```pascal
// File: math/Vector2D.pas
namespace math;
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

### 10.2 Using Statements

**Bring namespaces into scope:**
```pascal
program MyGame;
uses
  math,
  math.Vector2D;
  
using math;        // Bring math namespace into scope
using math.Vector2D;  // Bring Vector2D submodule into scope

var
  v: TVector2D;     // Can use unqualified name (from math.Vector2D)
begin
  v := VectorAdd(v1, v2);  // Can use unqualified function name
  WriteLn(Add(5, 3));      // Can use math.Add without qualification
end.
```

### 10.3 Namespace Aliases

**Create shorter names for long namespaces:**
```pascal
program MyGame;
uses
  graphics.sprites.animation.SpriteAnimator;
  
namespace SpriteAnim = graphics.sprites.animation;

var
  anim: SpriteAnim.SpriteAnimator.TAnimation;
begin
  // Use alias instead of full path
  anim := SpriteAnim.SpriteAnimator.CreateAnimation;
end.
```

### 10.4 Global Namespace Access

**Access global namespace explicitly:**
```pascal
program MyGame;
uses math;
namespace MyMath = math;

var
  x: integer;
begin
  // If there's a conflict, use global namespace
  x := ::math.Add(5, 3);  // Explicitly use math namespace
  // Or use qualified name
  x := math.Add(5, 3);
end.
```

### 10.5 Namespace Scope Rules

**Namespace visibility:**
- `using` statements apply to current scope
- Can be used in interface or implementation sections
- Can be scoped to procedures/functions

**Example:**
```pascal
unit Game;
interface
  uses math;
  
  procedure DoMath;  // Uses math namespace from interface
implementation
  uses graphics;
  
  procedure DoMath;
  using math;  // Local using (only in this procedure)
  begin
    var result := Add(5, 3);  // Uses math.Add
  end;
  
  procedure DoGraphics;
  using graphics;  // Local using (only in this procedure)
  begin
    DrawSprite(10, 20);  // Uses graphics.DrawSprite
  end;
end.
```

### 10.6 Benefits of Namespace Support

**Advantages:**
- **Reduced verbosity** — Don't need to qualify every name
- **Familiar syntax** — Similar to C++/C# namespaces
- **Flexible** — Can use qualified or unqualified names
- **Conflict resolution** — Can explicitly qualify when needed
- **Aliases** — Shorten long namespace paths

**Example comparison:**
```pascal
// Without using statement
var v: math.Vector2D.TVector2D;
v := math.Vector2D.VectorAdd(v1, v2);

// With using statement
using math.Vector2D;
var v: TVector2D;
v := VectorAdd(v1, v2);
```

### 10.7 Grammar Extensions

**Extended grammar:**
```
namespace-decl ::= "namespace" qualified-ident ";"
using-stmt ::= "using" qualified-ident ";"
namespace-alias ::= "namespace" ident "=" qualified-ident ";"
qualified-ident ::= ident ("." ident)*
                  | "::" ident ("." ident)*  // Global namespace
```

**Examples:**
```pascal
namespace math;                    // Namespace declaration
using math.Vector2D;              // Using statement
namespace Vec = math.Vector2D;    // Namespace alias
var v: ::math.Vector2D.TVector2D;  // Global namespace access
```

---

## 11. Status

**Current status:** Design phase — Planned for future release

**Implementation priority:** High (improves code organization significantly)

**Dependencies:**
- Parser updates for qualified identifiers
- Module resolver implementation
- Symbol table updates
- Namespace resolution (for `using` statements)
- Testing infrastructure

**Future enhancements:**
- ✅ Rust-style module system (directory-based modules)
- ✅ Qualified unit names (`math.Vector2D`)
- ✅ Namespace support (`namespace`, `using`, aliases)
- ✅ Global namespace access (`::`)

---

**End of Rust-Style Module System Specification**

