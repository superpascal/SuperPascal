# Classes and Methods

**Part of:** [Chapter 24: Object-Oriented Programming](./README.md)

---

> **For GCSE students:**  
> Classes are like blueprints for objects. A "Player" class describes what every player has (like health and position) and what they can do (like move and jump). You create actual players from this blueprint.
>
> **For A-Level students:**  
> Classes implement object-oriented programming, encapsulating data (fields) and behavior (methods) into objects. Understanding classes, inheritance, and polymorphism is essential for modern software development.
>
> **For University students:**  
> Classes implement the object-oriented paradigm with encapsulation, inheritance, and polymorphism. SuperPascal's hybrid model combines classes (for behavior) with records (for data). Understanding OOP requires understanding virtual method dispatch, object layout, and when to use classes vs. records in the hybrid model.

---

## Introduction

Classes organize code into objects with data (fields) and behavior (methods). This section teaches you how to define classes, create objects, and use methods.

**Key concepts:**
- **Classes** — Object templates
- **Fields** — Object data
- **Methods** — Object behavior
- **Objects** — Class instances
- **Reference semantics** — How objects work

---

## Understanding Classes

### What is a Class?

**A class is a template for creating objects:**

- **Fields** — Data stored in objects
- **Methods** — Functions that operate on objects
- **Encapsulation** — Data and behavior together
- **Reusability** — Create multiple objects from one class

**Class vs Record:**
- **Records** — Value types, data only, copied on assignment
- **Classes** — Reference types, data + methods, shared references

### Basic Class Syntax

**Define a simple class:**

```pascal
type
  TPlayer = class
  public
    X, Y: integer;
    Health: byte;
    
    procedure Move(dx, dy: integer);
    function GetPosition: string;
  end;
```

**Components:**
- `type` — Type declaration keyword
- `TPlayer` — Class name (T prefix convention)
- `class` — Class keyword
- `public` — Visibility section
- Fields and methods — Class members

---

## Class Fields

### Field Declaration

**Fields store object data:**

```pascal
type
  TEntity = class
  public
    ID: word;
    X, Y: integer;
    Active: boolean;
  end;
```

**Field types:**
- Any valid type (integer, byte, boolean, record, etc.)
- Arrays and records allowed
- Other classes allowed (references)

### Field Access

**Access fields via object reference:**

```pascal
var
  entity: TEntity;
begin
  entity := TEntity.Create;
  entity.ID := 1;
  entity.X := 100;
  entity.Y := 50;
  entity.Active := true;
end;
```

**Syntax:**
- `object.field` — Access field
- Object must be created first (`Create`)

---

## Class Methods

### Method Declaration

**Methods define object behavior:**

```pascal
type
  TPlayer = class
  public
    X, Y: integer;
    
    procedure Move(dx, dy: integer);
    function GetDistance(x, y: integer): integer;
  end;
```

**Method types:**
- **Procedures** — No return value
- **Functions** — Return a value

### Method Implementation

**Implement methods in implementation section:**

```pascal
implementation

procedure TPlayer.Move(dx, dy: integer);
begin
  X := X + dx;
  Y := Y + dy;
end;

function TPlayer.GetDistance(x, y: integer): integer;
var
  dx, dy: integer;
begin
  dx := X - x;
  dy := Y - y;
  GetDistance := Round(Sqrt(dx * dx + dy * dy));
end;
```

**Syntax:**
- `ClassName.MethodName` — Full method name
- `Self` — Implicit reference to current object (optional)

### Using Methods

**Call methods on objects:**

```pascal
var
  player: TPlayer;
begin
  player := TPlayer.Create;
  player.X := 100;
  player.Y := 50;
  
  player.Move(10, 5);  // Move player
  // player.X is now 110, player.Y is now 55
  
  var dist := player.GetDistance(0, 0);
  WriteLn('Distance: ', dist);
end;
```

---

## Object Creation

### Creating Objects

**Objects are created with `Create`:**

```pascal
var
  player: TPlayer;
begin
  player := TPlayer.Create;  // Create object
  // ... use player ...
  player.Destroy;            // Free object
end;
```

**How it works:**
- `Create` allocates memory on heap
- Returns reference to object
- Object exists until `Destroy` called

### Object References

**Classes use reference semantics:**

```pascal
var
  player1, player2: TPlayer;
begin
  player1 := TPlayer.Create;
  player1.X := 100;
  
  player2 := player1;  // Copies reference, not object
  player2.X := 200;
  
  // player1.X is now 200 (same object!)
  WriteLn(player1.X);  // 200
  WriteLn(player2.X);  // 200
  
  player1.Destroy;     // Free object
  // player2 is now dangling pointer!
end;
```

**Important:**
- Assignment copies reference, not object
- Multiple variables can reference same object
- Only destroy once

---

## Visibility

### Public Members

**Public members accessible everywhere:**

```pascal
type
  TPlayer = class
  public
    X, Y: integer;  // Public field
    procedure Move; // Public method
  end;
```

**Usage:**
```pascal
var player: TPlayer;
player := TPlayer.Create;
player.X := 100;  // Can access
player.Move;      // Can call
```

### Private Members

**Private members only accessible in class:**

```pascal
type
  TPlayer = class
  private
    Health: byte;  // Private field
    procedure TakeDamage; // Private method
  public
    procedure Hit;  // Public method
  end;

implementation

procedure TPlayer.Hit;
begin
  TakeDamage;  // Can call private method
end;

procedure TPlayer.TakeDamage;
begin
  Health := Health - 1;
end;
```

**Usage:**
```pascal
var player: TPlayer;
player := TPlayer.Create;
player.Hit;        // OK: public method
// player.Health := 10;  // ERROR: private field
// player.TakeDamage;    // ERROR: private method
```

### Protected Members

**Protected members accessible in class and descendants:**

```pascal
type
  TEntity = class
  protected
    X, Y: integer;  // Protected field
  public
    procedure Move;
  end;
  
  TPlayer = class(TEntity)
  public
    procedure Update;
  end;

implementation

procedure TPlayer.Update;
begin
  X := X + 1;  // Can access protected field from parent
end;
```

---

## Practical Examples

### Simple Entity Class

**Basic entity with position:**

```pascal
type
  TEntity = class
  public
    X, Y: integer;
    
    constructor Create(x, y: integer);
    procedure Move(dx, dy: integer);
    function DistanceTo(other: TEntity): integer;
  end;

implementation

constructor TEntity.Create(x, y: integer);
begin
  X := x;
  Y := y;
end;

procedure TEntity.Move(dx, dy: integer);
begin
  X := X + dx;
  Y := Y + dy;
end;

function TEntity.DistanceTo(other: TEntity): integer;
var
  dx, dy: integer;
begin
  dx := X - other.X;
  dy := Y - other.Y;
  DistanceTo := Round(Sqrt(dx * dx + dy * dy));
end;
```

### Player Class

**Player with health and movement:**

```pascal
type
  TPlayer = class
  private
    Health: byte;
  public
    X, Y: integer;
    
    constructor Create;
    procedure Move(dx, dy: integer);
    procedure TakeDamage(amount: byte);
    function IsAlive: boolean;
  end;

implementation

constructor TPlayer.Create;
begin
  X := 160;
  Y := 120;
  Health := 100;
end;

procedure TPlayer.Move(dx, dy: integer);
begin
  X := X + dx;
  Y := Y + dy;
end;

procedure TPlayer.TakeDamage(amount: byte);
begin
  if Health > amount then
    Health := Health - amount
  else
    Health := 0;
end;

function TPlayer.IsAlive: boolean;
begin
  IsAlive := Health > 0;
end;
```

### Manager Class

**Game manager for multiple objects:**

```pascal
type
  TEntityManager = class
  private
    Entities: array[0..255] of TEntity;
    Count: byte;
  public
    constructor Create;
    procedure Add(entity: TEntity);
    procedure Update;
    procedure Render;
  end;

implementation

constructor TEntityManager.Create;
begin
  Count := 0;
end;

procedure TEntityManager.Add(entity: TEntity);
begin
  if Count < 256 then
  begin
    Entities[Count] := entity;
    Count := Count + 1;
  end;
end;

procedure TEntityManager.Update;
var
  i: byte;
begin
  for i := 0 to Count - 1 do
  begin
    if Entities[i] <> nil then
      Entities[i].Update;
  end;
end;

procedure TEntityManager.Render;
var
  i: byte;
begin
  for i := 0 to Count - 1 do
  begin
    if Entities[i] <> nil then
      Entities[i].Render;
  end;
end;
```

---

## Complete Example

**Putting it all together:**

```pascal
program ClassesDemo;

type
  TEntity = class
  public
    X, Y: integer;
    
    constructor Create(x, y: integer);
    procedure Update; virtual;
    procedure Render;
  end;
  
  TPlayer = class(TEntity)
  public
    procedure Update; override;
  end;

var
  player: TPlayer;
  entity: TEntity;

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
  // Render at X, Y
  SpriteSet(0, X, Y, 1);
end;

procedure TPlayer.Update;
begin
  // Player-specific update
  inherited Update;  // Call parent
end;

begin
  InitGraphics;
  
  player := TPlayer.Create(160, 120);
  entity := TEntity.Create(100, 100);
  
  while true do
  begin
    player.Update;
    entity.Update;
    player.Render;
    entity.Render;
    WaitVBlank;
  end;
  
  player.Destroy;
  entity.Destroy;
end.
```

---

## Best Practices

### 1. Use T Prefix for Classes

**Naming convention:**

```pascal
// ✅ GOOD: T prefix
type
  TPlayer = class
  TEntity = class

// ❌ BAD: No prefix
type
  Player = class
  Entity = class
```

### 2. Initialize Fields in Constructor

**Set initial values:**

```pascal
// ✅ GOOD: Initialize in constructor
constructor TPlayer.Create;
begin
  X := 0;
  Y := 0;
  Health := 100;
end;

// ❌ BAD: Uninitialized fields
constructor TPlayer.Create;
begin
  // Fields have undefined values
end;
```

### 3. Always Destroy Objects

**Free allocated memory:**

```pascal
// ✅ GOOD: Destroy objects
var player: TPlayer;
player := TPlayer.Create;
// ... use player ...
player.Destroy;

// ❌ BAD: Memory leak
var player: TPlayer;
player := TPlayer.Create;
// ... use player ...
// Never destroyed!
```

### 4. Use Private for Internal State

**Hide implementation details:**

```pascal
// ✅ GOOD: Private fields
type
  TPlayer = class
  private
    Health: byte;
  public
    procedure TakeDamage(amount: byte);
  end;

// ❌ BAD: Public everything
type
  TPlayer = class
  public
    Health: byte;  // Can be modified directly
  end;
```

### 5. Keep Methods Focused

**One responsibility per method:**

```pascal
// ✅ GOOD: Focused methods
procedure TPlayer.Move(dx, dy: integer);
procedure TPlayer.Attack(target: TEntity);

// ❌ BAD: Too many responsibilities
procedure TPlayer.DoEverything;
begin
  // Move, attack, update, render, etc.
end;
```

---

## Exercises

### Exercise 1: Basic Class

Write a program that:
1. Defines a simple class
2. Adds fields and methods
3. Creates objects
4. Calls methods

### Exercise 2: Class with Methods

Write a program that:
1. Creates a class with multiple methods
2. Implements method logic
3. Uses methods to modify object state
4. Demonstrates method calls

### Exercise 3: Visibility

Write a program that:
1. Uses public, private, protected
2. Demonstrates access rules
3. Shows encapsulation
4. Tests visibility boundaries

### Exercise 4: Manager Class

**GCSE Level:**
Write a program that:
1. Creates a manager class
2. Manages multiple objects
3. Implements update/render loops
4. Demonstrates object management

---

## Level-Specific Exercises

### GCSE Level Exercises

**Exercise 1: Simple Class**
Write a program that:
1. Defines a `Player` class with health and score
2. Creates a player object
3. Modifies health and score
4. Displays player information

**Exercise 2: Class with Methods**
Write a program that:
1. Creates a `BankAccount` class
2. Has methods: `Deposit`, `Withdraw`, `GetBalance`
3. Creates an account
4. Tests all methods

### A-Level Exercises

**Exercise 1: Class Hierarchy**
Create a class hierarchy:
1. Base `GameObject` class
2. Derived `Player` and `Enemy` classes
3. Shared and unique methods
4. Demonstrate inheritance

**Exercise 2: Encapsulation**
Design a class with:
1. Private fields
2. Public methods for access
3. Validation in methods
4. Demonstrate encapsulation benefits

**Exercise 3: Class vs Record**
Compare classes and records:
1. Implement same data structure as both
2. Compare memory usage
3. Compare performance
4. Determine when to use each

### University Level Exercises

**Exercise 1: Virtual Method Dispatch**
Implement virtual method system:
1. Base class with virtual methods
2. Derived classes with overrides
3. Measure dispatch overhead
4. Compare with function pointers
5. Analyze vtable implementation

**Exercise 2: Object Layout Analysis**
Analyze object memory layout:
1. Field ordering impact
2. Padding and alignment
3. Virtual method table location
4. Memory efficiency optimization
5. Compare with record layout

**Exercise 3: Hybrid OOP Model**
Design using hybrid model:
1. Classes for behavior (game logic)
2. Records for data (components)
3. ECS integration
4. Performance comparison
5. Design pattern documentation

---

**Previous Chapter:** [Chapter 21: Scripting and Events](../21_ScriptingAndEvents/README.md)  
**Next Section:** [Constructors](./02_Constructors.md)  
**Language Specification:** See [Type System](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

