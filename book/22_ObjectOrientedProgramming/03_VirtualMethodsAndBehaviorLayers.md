# Virtual Methods and Behavior Layers

**Part of:** [Chapter 22: Object-Oriented Programming](./README.md)

---

## Introduction

Virtual methods enable polymorphism—calling different implementations based on object type. This section teaches you how to use virtual methods, override them, and create behavior layers.

**Key concepts:**
- **Virtual methods** — Runtime method dispatch
- **Override** — Replacing parent implementation
- **Polymorphism** — Same interface, different behavior
- **Behavior layers** — Hierarchical behavior systems

---

## Understanding Virtual Methods

### What is a Virtual Method?

**A virtual method is resolved at runtime:**

- **Runtime dispatch** — Method called based on object type
- **VTable** — Virtual method table stores method addresses
- **Polymorphism** — Same call, different behavior
- **Override** — Child classes replace parent implementation

**Virtual vs non-virtual:**
- **Non-virtual** — Resolved at compile time (faster, no polymorphism)
- **Virtual** — Resolved at runtime (slower, enables polymorphism)

### Basic Virtual Method

**Define virtual method in parent:**

```pascal
type
  TEntity = class
  public
    procedure Update; virtual;
  end;
  
  TPlayer = class(TEntity)
  public
    procedure Update; override;
  end;
```

**Implementation:**
```pascal
implementation

procedure TEntity.Update;
begin
  WriteLn('Entity update');
end;

procedure TPlayer.Update;
begin
  WriteLn('Player update');
end;
```

**Usage:**
```pascal
var
  entity: TEntity;
  player: TPlayer;
begin
  entity := TEntity.Create;
  player := TPlayer.Create;
  
  entity.Update;  // Calls TEntity.Update
  player.Update;  // Calls TPlayer.Update
end;
```

---

## Polymorphism

### Runtime Dispatch

**Call method through parent reference:**

```pascal
var
  entity: TEntity;
  player: TPlayer;
begin
  player := TPlayer.Create;
  entity := player;  // Parent reference to child object
  
  entity.Update;  // Calls TPlayer.Update (polymorphism!)
end;
```

**How it works:**
1. `entity` is `TEntity` type
2. Points to `TPlayer` object
3. `Update` is virtual
4. Runtime looks up actual object type
5. Calls `TPlayer.Update`

### Polymorphism Example

**Different behaviors through same interface:**

```pascal
type
  TEntity = class
  public
    procedure Update; virtual;
    procedure Render; virtual;
  end;
  
  TPlayer = class(TEntity)
  public
    procedure Update; override;
    procedure Render; override;
  end;
  
  TEnemy = class(TEntity)
  public
    procedure Update; override;
    procedure Render; override;
  end;

var
  entities: array[0..9] of TEntity;
  i: byte;
begin
  entities[0] := TPlayer.Create;
  entities[1] := TEnemy.Create;
  entities[2] := TEnemy.Create;
  // ... more entities ...
  
  // Update all entities (polymorphism)
  for i := 0 to 9 do
  begin
    if entities[i] <> nil then
      entities[i].Update;  // Calls correct Update for each type
  end;
end;
```

---

## Override and Inherited

### Override Keyword

**Override replaces parent implementation:**

```pascal
type
  TEntity = class
  public
    procedure Update; virtual;
  end;
  
  TPlayer = class(TEntity)
  public
    procedure Update; override;  // Replaces TEntity.Update
  end;
```

**Rules:**
- Method must be `virtual` in parent
- Use `override` in child (not `virtual`)
- Method signature must match

### Inherited Keyword

**Call parent implementation:**

```pascal
procedure TPlayer.Update;
begin
  inherited Update;  // Call TEntity.Update first
  // Then do player-specific update
  HandleInput;
  CheckCollisions;
end;
```

**Use cases:**
- Extend parent behavior
- Add functionality before/after parent
- Chain initialization

---

## Behavior Layers

### What are Behavior Layers?

**Behavior layers organize related behaviors:**

- **Base layer** — Common behavior (TEntity)
- **Specific layers** — Specialized behavior (TPlayer, TEnemy)
- **Hierarchical** — Each layer builds on previous
- **Reusable** — Share common code

### Simple Behavior Layer

**Basic entity hierarchy:**

```pascal
type
  TEntity = class
  public
    X, Y: integer;
    procedure Update; virtual;
    procedure Render; virtual;
  end;
  
  TMovable = class(TEntity)
  public
    VX, VY: integer;
    procedure Update; override;
  end;
  
  TPlayer = class(TMovable)
  public
    Health: byte;
    procedure Update; override;
    procedure HandleInput;
  end;
```

**Implementation:**
```pascal
procedure TEntity.Update;
begin
  // Base update (empty or common logic)
end;

procedure TEntity.Render;
begin
  SpriteSet(0, X, Y, 1);
end;

procedure TMovable.Update;
begin
  inherited Update;  // Call TEntity.Update
  X := X + VX;       // Apply velocity
  Y := Y + VY;
end;

procedure TPlayer.Update;
begin
  inherited Update;  // Call TMovable.Update (which calls TEntity.Update)
  HandleInput;        // Player-specific
end;
```

### Complex Behavior Layers

**Multiple layers of behavior:**

```pascal
type
  TEntity = class
  public
    X, Y: integer;
    procedure Update; virtual;
  end;
  
  TCollidable = class(TEntity)
  public
    Width, Height: integer;
    function CheckCollision(other: TCollidable): boolean; virtual;
  end;
  
  TAnimated = class(TCollidable)
  public
    CurrentFrame: byte;
    procedure Update; override;
    procedure AdvanceFrame;
  end;
  
  TPlayer = class(TAnimated)
  public
    Health: byte;
    procedure Update; override;
    procedure HandleInput;
  end;
```

**Behavior chain:**
1. `TEntity` — Basic position
2. `TCollidable` — Collision detection
3. `TAnimated` — Animation
4. `TPlayer` — Player-specific behavior

---

## Practical Examples

### Game Entity System

**Complete entity hierarchy:**

```pascal
type
  TEntity = class
  public
    X, Y: integer;
    Active: boolean;
    
    constructor Create(x, y: integer);
    procedure Update; virtual;
    procedure Render; virtual;
  end;
  
  TEnemy = class(TEntity)
  public
    Health: byte;
    procedure Update; override;
    procedure TakeDamage(amount: byte);
  end;
  
  TPlayer = class(TEntity)
  public
    Health: byte;
    Score: word;
    procedure Update; override;
    procedure HandleInput;
  end;

implementation

constructor TEntity.Create(x, y: integer);
begin
  X := x;
  Y := y;
  Active := true;
end;

procedure TEntity.Update;
begin
  // Base update (empty or common)
end;

procedure TEntity.Render;
begin
  if Active then
    SpriteSet(0, X, Y, 1);
end;

procedure TEnemy.Update;
begin
  inherited Update;
  // Enemy AI
  if Health = 0 then
    Active := false;
end;

procedure TEnemy.TakeDamage(amount: byte);
begin
  if Health > amount then
    Health := Health - amount
  else
    Health := 0;
end;

procedure TPlayer.Update;
begin
  inherited Update;
  HandleInput;
  if Health = 0 then
    Active := false;
end;

procedure TPlayer.HandleInput;
var
  input: word;
begin
  input := ReadInput;
  if InputPressed(KEY_LEFT) then X := X - 2;
  if InputPressed(KEY_RIGHT) then X := X + 2;
  if InputPressed(KEY_UP) then Y := Y - 2;
  if InputPressed(KEY_DOWN) then Y := Y + 2;
end;
```

### UI Component System

**UI components with behavior layers:**

```pascal
type
  TUIComponent = class
  public
    X, Y: integer;
    Visible: boolean;
    procedure Update; virtual;
    procedure Render; virtual;
  end;
  
  TButton = class(TUIComponent)
  public
    Text: string;
    Pressed: boolean;
    procedure Update; override;
    procedure Render; override;
    function IsClicked(mx, my: integer): boolean;
  end;
  
  TMenu = class(TUIComponent)
  private
    Buttons: array[0..7] of TButton;
    Count: byte;
  public
    procedure Update; override;
    procedure Render; override;
    procedure AddButton(button: TButton);
  end;
```

---

## Complete Example

**Putting it all together:**

```pascal
program VirtualMethodsDemo;

type
  TEntity = class
  public
    X, Y: integer;
    
    constructor Create(x, y: integer);
    procedure Update; virtual;
    procedure Render; virtual;
  end;
  
  TPlayer = class(TEntity)
  public
    Health: byte;
    procedure Update; override;
  end;
  
  TEnemy = class(TEntity)
  public
    Health: byte;
    procedure Update; override;
  end;

var
  entities: array[0..9] of TEntity;
  entityCount: byte;

constructor TEntity.Create(x, y: integer);
begin
  X := x;
  Y := y;
end;

procedure TEntity.Update;
begin
  // Base update
end;

procedure TEntity.Render;
begin
  SpriteSet(0, X, Y, 1);
end;

procedure TPlayer.Update;
begin
  inherited Update;
  // Player input
  var input := ReadInput;
  if InputPressed(KEY_LEFT) then X := X - 2;
  if InputPressed(KEY_RIGHT) then X := X + 2;
end;

procedure TEnemy.Update;
begin
  inherited Update;
  // Enemy AI (move toward player)
  // Simplified: just move right
  X := X + 1;
end;

procedure UpdateAllEntities;
var
  i: byte;
begin
  for i := 0 to entityCount - 1 do
  begin
    if entities[i] <> nil then
      entities[i].Update;  // Polymorphism!
  end;
end;

procedure RenderAllEntities;
var
  i: byte;
begin
  for i := 0 to entityCount - 1 do
  begin
    if entities[i] <> nil then
      entities[i].Render;  // Polymorphism!
  end;
end;

begin
  InitGraphics;
  
  // Create entities
  entities[0] := TPlayer.Create(160, 120);
  entities[1] := TEnemy.Create(100, 100);
  entities[2] := TEnemy.Create(200, 100);
  entityCount := 3;
  
  while true do
  begin
    UpdateAllEntities;
    RenderAllEntities;
    WaitVBlank;
  end;
  
  // Cleanup
  var i: byte;
  for i := 0 to entityCount - 1 do
  begin
    if entities[i] <> nil then
      entities[i].Destroy;
  end;
end.
```

---

## Best Practices

### 1. Use Virtual for Polymorphism

**Make methods virtual when needed:**

```pascal
// ✅ GOOD: Virtual for polymorphism
type
  TEntity = class
    procedure Update; virtual;
  end;

// ❌ BAD: Non-virtual when polymorphism needed
type
  TEntity = class
    procedure Update;  // Can't override
  end;
```

### 2. Always Override, Never Re-Virtual

**Use override, not virtual in child:**

```pascal
// ✅ GOOD: Override
type
  TPlayer = class(TEntity)
    procedure Update; override;
  end;

// ❌ BAD: Re-virtual
type
  TPlayer = class(TEntity)
    procedure Update; virtual;  // Wrong!
  end;
```

### 3. Call Inherited When Appropriate

**Extend, don't replace:**

```pascal
// ✅ GOOD: Extend behavior
procedure TPlayer.Update;
begin
  inherited Update;  // Keep parent behavior
  HandleInput;        // Add new behavior
end;

// ❌ BAD: Replace without calling inherited
procedure TPlayer.Update;
begin
  HandleInput;  // Lost parent behavior
end;
```

### 4. Design Clear Hierarchies

**Logical inheritance structure:**

```pascal
// ✅ GOOD: Clear hierarchy
TEntity → TMovable → TPlayer
TEntity → TCollidable → TEnemy

// ❌ BAD: Confusing hierarchy
TPlayer → TEntity → TMovable  // Backwards
```

### 5. Keep Virtual Methods Focused

**One responsibility per virtual method:**

```pascal
// ✅ GOOD: Focused methods
procedure Update; virtual;
procedure Render; virtual;

// ❌ BAD: Too many responsibilities
procedure DoEverything; virtual;  // Hard to override
```

---

## Exercises

### Exercise 1: Virtual Methods

Write a program that:
1. Defines parent class with virtual method
2. Creates child class with override
3. Demonstrates polymorphism
4. Shows runtime dispatch

### Exercise 2: Behavior Layers

Write a program that:
1. Creates behavior layer hierarchy
2. Uses inherited calls
3. Builds up behavior incrementally
4. Demonstrates layered behavior

### Exercise 3: Polymorphism

Write a program that:
1. Creates multiple child classes
2. Stores in parent array
3. Calls methods polymorphically
4. Shows different behaviors

### Exercise 4: Complete System

Write a program that:
1. Implements complete entity system
2. Uses virtual methods throughout
3. Demonstrates behavior layers
4. Integrates with game loop

---

**Previous Section:** [Constructors](./02_Constructors.md)  
**Next Chapter:** [Chapter 23: Scenes, UI, and Game Architecture](../23_ScenesUIAndGameArchitecture/README.md)  
**Language Specification:** See [Type System](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

