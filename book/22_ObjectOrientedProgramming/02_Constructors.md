# Constructors

**Part of:** [Chapter 22: Object-Oriented Programming](./README.md)

---

## Introduction

Constructors initialize objects when they're created. This section teaches you how to define constructors, initialize objects, and manage object lifecycle.

**Key concepts:**
- **Constructors** — Object initialization
- **Constructor parameters** — Passing initial values
- **Destructors** — Object cleanup
- **Object lifecycle** — Creation to destruction

---

## Understanding Constructors

### What is a Constructor?

**A constructor initializes a new object:**

- **Called automatically** — When object is created
- **Named `Create`** — Standard constructor name
- **Allocates memory** — Creates object on heap
- **Initializes fields** — Sets initial values

**Constructor vs regular method:**
- Constructor: Called during `Create`, returns object reference
- Method: Called on existing object, no return value

### Basic Constructor

**Simple constructor:**

```pascal
type
  TEntity = class
  public
    X, Y: integer;
    
    constructor Create;
  end;

implementation

constructor TEntity.Create;
begin
  X := 0;
  Y := 0;
end;
```

**Usage:**
```pascal
var entity: TEntity;
entity := TEntity.Create;  // Constructor called
// entity.X is 0, entity.Y is 0
```

---

## Constructor Parameters

### Parameterized Constructors

**Pass initial values to constructor:**

```pascal
type
  TEntity = class
  public
    X, Y: integer;
    
    constructor Create(x, y: integer);
  end;

implementation

constructor TEntity.Create(x, y: integer);
begin
  X := x;
  Y := y;
end;
```

**Usage:**
```pascal
var entity: TEntity;
entity := TEntity.Create(100, 50);  // Initialize with values
// entity.X is 100, entity.Y is 50
```

### Multiple Constructors

**Overload constructors (same name, different parameters):**

```pascal
type
  TEntity = class
  public
    X, Y: integer;
    
    constructor Create;
    constructor Create(x, y: integer);
  end;

implementation

constructor TEntity.Create;
begin
  X := 0;
  Y := 0;
end;

constructor TEntity.Create(x, y: integer);
begin
  X := x;
  Y := y;
end;
```

**Usage:**
```pascal
var
  entity1, entity2: TEntity;
begin
  entity1 := TEntity.Create;        // Default constructor
  entity2 := TEntity.Create(100, 50); // Parameterized constructor
end;
```

---

## Constructor in Inheritance

### Calling Parent Constructor

**Call parent constructor with `inherited`:**

```pascal
type
  TEntity = class
  public
    X, Y: integer;
    constructor Create(x, y: integer);
  end;
  
  TPlayer = class(TEntity)
  public
    Health: byte;
    constructor Create(x, y: integer);
  end;

implementation

constructor TEntity.Create(x, y: integer);
begin
  X := x;
  Y := y;
end;

constructor TPlayer.Create(x, y: integer);
begin
  inherited Create(x, y);  // Call parent constructor
  Health := 100;            // Initialize child fields
end;
```

**Order:**
1. Parent constructor called first
2. Child initialization after

### Default Parent Constructor

**If parent has no constructor, fields are zero-initialized:**

```pascal
type
  TEntity = class
  public
    X, Y: integer;  // Zero-initialized
  end;
  
  TPlayer = class(TEntity)
  public
    Health: byte;
    constructor Create;
  end;

implementation

constructor TPlayer.Create;
begin
  // X and Y are already 0
  Health := 100;
end;
```

---

## Destructors

### What is a Destructor?

**A destructor cleans up when object is destroyed:**

- **Named `Destroy`** — Standard destructor name
- **Called manually** — Via `Destroy` method
- **Frees resources** — Memory, handles, etc.
- **Last chance** — To clean up before object destroyed

### Basic Destructor

**Simple destructor:**

```pascal
type
  TResource = class
  private
    Handle: word;
  public
    constructor Create;
    destructor Destroy;
  end;

implementation

constructor TResource.Create;
begin
  Handle := AcquireResource;
end;

destructor TResource.Destroy;
begin
  ReleaseResource(Handle);
end;
```

**Usage:**
```pascal
var resource: TResource;
resource := TResource.Create;
// ... use resource ...
resource.Destroy;  // Destructor called, resource freed
```

### Destructor in Inheritance

**Call parent destructor:**

```pascal
type
  TEntity = class
  public
    destructor Destroy;
  end;
  
  TPlayer = class(TEntity)
  private
    Inventory: ^TInventory;
  public
    constructor Create;
    destructor Destroy;
  end;

implementation

destructor TEntity.Destroy;
begin
  // Base cleanup
end;

destructor TPlayer.Destroy;
begin
  Dispose(Inventory);  // Clean up child resources
  inherited Destroy;   // Call parent destructor
end;
```

**Order:**
1. Child cleanup first
2. Parent destructor called last

---

## Object Lifecycle

### Creation to Destruction

**Complete object lifecycle:**

```pascal
var entity: TEntity;
begin
  // 1. Creation
  entity := TEntity.Create(100, 50);
  // Constructor called, object allocated
  
  // 2. Usage
  entity.Move(10, 5);
  entity.Update;
  
  // 3. Destruction
  entity.Destroy;
  // Destructor called, object freed
  // entity is now invalid (dangling pointer)
end;
```

### Lifecycle Rules

**Important rules:**

1. **Always destroy** — Every `Create` needs a `Destroy`
2. **Destroy once** — Don't destroy same object twice
3. **Check for nil** — Before using object
4. **Set to nil** — After destroying

**Example:**
```pascal
var entity: TEntity;
begin
  entity := TEntity.Create;
  
  // ... use entity ...
  
  entity.Destroy;
  entity := nil;  // Mark as destroyed
  
  // Check before use
  if entity <> nil then
    entity.Update;  // Won't execute
end;
```

---

## Practical Examples

### Entity with Initialization

**Entity with position and health:**

```pascal
type
  TEntity = class
  public
    X, Y: integer;
    Health: byte;
    
    constructor Create(x, y: integer; health: byte);
  end;

implementation

constructor TEntity.Create(x, y: integer; health: byte);
begin
  X := x;
  Y := y;
  Health := health;
end;

// Usage
var entity: TEntity;
entity := TEntity.Create(100, 50, 100);
```

### Resource Manager

**Class that manages resources:**

```pascal
type
  TResourceManager = class
  private
    Resources: array[0..15] of word;
    Count: byte;
  public
    constructor Create;
    destructor Destroy;
    procedure Add(resource: word);
  end;

implementation

constructor TResourceManager.Create;
begin
  Count := 0;
  FillChar(Resources, SizeOf(Resources), 0);
end;

destructor TResourceManager.Destroy;
var
  i: byte;
begin
  // Release all resources
  for i := 0 to Count - 1 do
    ReleaseResource(Resources[i]);
  Count := 0;
end;

procedure TResourceManager.Add(resource: word);
begin
  if Count < 16 then
  begin
    Resources[Count] := resource;
    Count := Count + 1;
  end;
end;
```

### Player with Default Values

**Player with optional initialization:**

```pascal
type
  TPlayer = class
  public
    X, Y: integer;
    Health: byte;
    Score: word;
    
    constructor Create;
    constructor Create(x, y: integer);
  end;

implementation

constructor TPlayer.Create;
begin
  X := 160;      // Center of screen
  Y := 120;
  Health := 100;
  Score := 0;
end;

constructor TPlayer.Create(x, y: integer);
begin
  X := x;
  Y := y;
  Health := 100;
  Score := 0;
end;

// Usage
var
  player1, player2: TPlayer;
begin
  player1 := TPlayer.Create;           // Default position
  player2 := TPlayer.Create(100, 50);  // Custom position
end;
```

---

## Complete Example

**Putting it all together:**

```pascal
program ConstructorsDemo;

type
  TEntity = class
  public
    X, Y: integer;
    Active: boolean;
    
    constructor Create(x, y: integer);
    destructor Destroy;
    procedure Update;
  end;
  
  TPlayer = class(TEntity)
  public
    Health: byte;
    
    constructor Create(x, y: integer);
    destructor Destroy;
    procedure Update; override;
  end;

var
  player: TPlayer;
  entity: TEntity;

constructor TEntity.Create(x, y: integer);
begin
  X := x;
  Y := y;
  Active := true;
  WriteLn('Entity created at (', X, ', ', Y, ')');
end;

destructor TEntity.Destroy;
begin
  WriteLn('Entity destroyed');
end;

procedure TEntity.Update;
begin
  // Base update
end;

constructor TPlayer.Create(x, y: integer);
begin
  inherited Create(x, y);  // Call parent
  Health := 100;
  WriteLn('Player created with health ', Health);
end;

destructor TPlayer.Destroy;
begin
  WriteLn('Player destroyed');
  inherited Destroy;  // Call parent
end;

procedure TPlayer.Update;
begin
  inherited Update;  // Call parent
  // Player-specific update
end;

begin
  InitGraphics;
  
  // Create objects
  player := TPlayer.Create(160, 120);
  entity := TEntity.Create(100, 100);
  
  // Use objects
  while true do
  begin
    player.Update;
    entity.Update;
    RenderGame;
    WaitVBlank;
  end;
  
  // Destroy objects
  player.Destroy;
  entity.Destroy;
end.
```

---

## Best Practices

### 1. Always Initialize Fields

**Set all fields in constructor:**

```pascal
// ✅ GOOD: Initialize all fields
constructor TPlayer.Create;
begin
  X := 0;
  Y := 0;
  Health := 100;
end;

// ❌ BAD: Uninitialized fields
constructor TPlayer.Create;
begin
  // Health has undefined value
end;
```

### 2. Call Parent Constructor

**Always call inherited constructor:**

```pascal
// ✅ GOOD: Call parent
constructor TPlayer.Create;
begin
  inherited Create;
  Health := 100;
end;

// ❌ BAD: Skip parent
constructor TPlayer.Create;
begin
  Health := 100;
  // Parent fields not initialized
end;
```

### 3. Always Destroy Objects

**Match every Create with Destroy:**

```pascal
// ✅ GOOD: Always destroy
var entity: TEntity;
entity := TEntity.Create;
// ... use ...
entity.Destroy;

// ❌ BAD: Memory leak
var entity: TEntity;
entity := TEntity.Create;
// ... use ...
// Never destroyed!
```

### 4. Clean Up in Destructor

**Free all resources:**

```pascal
// ✅ GOOD: Clean up resources
destructor TResource.Destroy;
begin
  ReleaseResource(Handle);
  Dispose(Data);
  inherited Destroy;
end;

// ❌ BAD: Leak resources
destructor TResource.Destroy;
begin
  // Resources not freed
end;
```

### 5. Use Named Constructors

**Clear constructor names:**

```pascal
// ✅ GOOD: Clear name
constructor Create(x, y: integer);

// ❌ BAD: Unclear name
constructor Init(x, y: integer);  // Non-standard
```

---

## Exercises

### Exercise 1: Basic Constructor

Write a program that:
1. Defines a class with constructor
2. Initializes fields in constructor
3. Creates objects
4. Demonstrates initialization

### Exercise 2: Parameterized Constructor

Write a program that:
1. Creates constructor with parameters
2. Passes initial values
3. Uses multiple constructors
4. Demonstrates overloading

### Exercise 3: Inheritance Constructors

Write a program that:
1. Creates parent and child classes
2. Calls parent constructor
3. Initializes child fields
4. Demonstrates inheritance

### Exercise 4: Destructors

Write a program that:
1. Implements destructors
2. Cleans up resources
3. Calls parent destructors
4. Demonstrates lifecycle

---

**Previous Section:** [Classes and Methods](./01_ClassesAndMethods.md)  
**Next Section:** [Virtual Methods and Behavior Layers](./03_VirtualMethodsAndBehaviorLayers.md)  
**Language Specification:** See [Type System](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

