# Advanced Patterns

**Part of:** [Chapter 30: Advanced Patterns](./README.md)

---

## Introduction

Advanced programming patterns provide reusable solutions to common problems. This section teaches you advanced design patterns, functional programming patterns, SuperPascal-specific idioms, performance-critical patterns, and how to write efficient, maintainable code.

**Key concepts:**
- **Design patterns** — Advanced pattern implementations
- **Functional patterns** — Higher-order functions, immutability
- **SuperPascal idioms** — Language-specific patterns
- **Performance patterns** — Patterns for speed
- **Maintainability** — Patterns for clarity

---

## Advanced Design Patterns

### Strategy Pattern

**Interchangeable algorithms:**

```pascal
type
  TSortStrategy = procedure(var arr: array of integer; left, right: integer);
  
  TSortContext = record
    Strategy: TSortStrategy;
  end;

procedure SetStrategy(var context: TSortContext; strategy: TSortStrategy);
begin
  context.Strategy := strategy;
end;

procedure Sort(var context: TSortContext; var arr: array of integer);
begin
  context.Strategy(arr, 0, Length(arr) - 1);
end;

// Usage
var
  context: TSortContext;
  arr: array[0..99] of integer;
begin
  SetStrategy(context, QuickSort);
  Sort(context, arr);
  
  SetStrategy(context, MergeSort);
  Sort(context, arr);
end;
```

### Observer Pattern

**Event notification system:**

```pascal
type
  PObserver = ^TObserver;
  TObserver = record
    OnNotify: procedure(sender: pointer; event: integer);
    Next: PObserver;
  end;
  
  TObservable = record
    Observers: PObserver;
  end;

procedure AttachObserver(var observable: TObservable; observer: PObserver);
begin
  observer^.Next := observable.Observers;
  observable.Observers := observer;
end;

procedure NotifyObservers(var observable: TObservable; sender: pointer; event: integer);
var
  observer: PObserver;
begin
  observer := observable.Observers;
  while observer <> nil do
  begin
    observer^.OnNotify(sender, event);
    observer := observer^.Next;
  end;
end;

// Usage
procedure OnScoreChanged(sender: pointer; event: integer);
begin
  WriteLn('Score changed!');
end;

var
  game: TObservable;
  scoreObserver: TObserver;
begin
  scoreObserver.OnNotify := OnScoreChanged;
  AttachObserver(game, @scoreObserver);
  NotifyObservers(game, @game, SCORE_CHANGED);
end;
```

### Factory Pattern

**Create objects without specifying exact class:**

```pascal
type
  TEnemyType = (etBasic, etFast, etStrong);
  
  TEnemy = class
  public
    procedure Update; virtual; abstract;
  end;
  
  TBasicEnemy = class(TEnemy)
  public
    procedure Update; override;
  end;
  
  TFastEnemy = class(TEnemy)
  public
    procedure Update; override;
  end;

function CreateEnemy(enemyType: TEnemyType): TEnemy;
begin
  case enemyType of
    etBasic: CreateEnemy := TBasicEnemy.Create;
    etFast: CreateEnemy := TFastEnemy.Create;
    etStrong: CreateEnemy := TStrongEnemy.Create;
  end;
end;
```

### Command Pattern

**Encapsulate requests as objects:**

```pascal
type
  TCommand = class
  public
    procedure Execute; virtual; abstract;
    procedure Undo; virtual; abstract;
  end;
  
  TMoveCommand = class(TCommand)
  private
    FEntity: ^TEntity;
    FDeltaX, FDeltaY: Q8.8;
    FOldX, FOldY: Q8.8;
  public
    constructor Create(entity: ^TEntity; dx, dy: Q8.8);
    procedure Execute; override;
    procedure Undo; override;
  end;

constructor TMoveCommand.Create(entity: ^TEntity; dx, dy: Q8.8);
begin
  inherited Create;
  FEntity := entity;
  FDeltaX := dx;
  FDeltaY := dy;
  FOldX := entity^.X;
  FOldY := entity^.Y;
end;

procedure TMoveCommand.Execute;
begin
  FEntity^.X := FEntity^.X + FDeltaX;
  FEntity^.Y := FEntity^.Y + FDeltaY;
end;

procedure TMoveCommand.Undo;
begin
  FEntity^.X := FOldX;
  FEntity^.Y := FOldY;
end;
```

---

## Functional Programming Patterns

### Higher-Order Functions

**Functions that take functions:**

```pascal
type
  TMapFunction = function(x: integer): integer;
  TFilterFunction = function(x: integer): boolean;

procedure Map(var arr: array of integer; func: TMapFunction);
var
  i: integer;
begin
  for i := 0 to Length(arr) - 1 do
    arr[i] := func(arr[i]);
end;

function Square(x: integer): integer;
begin
  Square := x * x;
end;

// Usage
var
  numbers: array[0..9] of integer;
begin
  // Initialize numbers...
  Map(numbers, Square);  // Square all numbers
end;
```

### Immutability Patterns

**Avoid modifying data:**

```pascal
// ❌ BAD: Mutates original
procedure AddOne(var arr: array of integer);
var
  i: integer;
begin
  for i := 0 to Length(arr) - 1 do
    arr[i] := arr[i] + 1;
end;

// ✅ GOOD: Returns new array
function AddOne(const arr: array of integer): array of integer;
var
  i: integer;
begin
  SetLength(result, Length(arr));
  for i := 0 to Length(arr) - 1 do
    result[i] := arr[i] + 1;
end;
```

### Function Composition

**Combine functions:**

```pascal
function Compose(f, g: TMapFunction): TMapFunction;
begin
  // Returns function that applies g then f
  // Implementation depends on language support
end;

// Usage
var
  double: TMapFunction;
  square: TMapFunction;
  doubleThenSquare: TMapFunction;
begin
  double := DoubleFunction;
  square := SquareFunction;
  doubleThenSquare := Compose(square, double);
end;
```

---

## SuperPascal-Specific Idioms

### Record vs Class Selection

**When to use each:**

```pascal
// ✅ GOOD: Record for data
type
  TPosition = record
    X, Y: Q8.8;
  end;
  
  TVelocity = record
    VX, VY: Q8.8;
  end;

// ✅ GOOD: Class for behavior
type
  TPlayer = class
  private
    FPosition: TPosition;
    FVelocity: TVelocity;
  public
    procedure Update;
    procedure Render;
  end;
```

### Fixed-Point Math Patterns

**Efficient fixed-point operations:**

```pascal
// ✅ GOOD: Pre-calculate constants
const
  GRAVITY = Q8_8(0.3);
  FRICTION = Q8_8(0.9);
  MAX_SPEED = Q8_8(10.0);

// ✅ GOOD: Use fixed-point directly
procedure ApplyPhysics(var velocity: Q8.8);
begin
  velocity := velocity + GRAVITY;
  velocity := velocity * FRICTION;
  
  if velocity > MAX_SPEED then
    velocity := MAX_SPEED
  else if velocity < -MAX_SPEED then
    velocity := -MAX_SPEED;
end;
```

### ECS Patterns

**Entity-Component-System idioms:**

```pascal
// ✅ GOOD: SoA for components
type
  TPositionComponent = record
    X, Y: array[0..MAX_ENTITIES - 1] of Q8.8;
  end;
  
  TVelocityComponent = record
    VX, VY: array[0..MAX_ENTITIES - 1] of Q8.8;
  end;

// ✅ GOOD: Systems process components
procedure MovementSystem(var positions: TPositionComponent;
                         var velocities: TVelocityComponent;
                         entities: array of TEntity);
var
  i: integer;
begin
  for i := 0 to Length(entities) - 1 do
  begin
    if EntityValid(entities[i]) then
    begin
      positions.X[i] := positions.X[i] + velocities.VX[i];
      positions.Y[i] := positions.Y[i] + velocities.VY[i];
    end;
  end;
end;
```

### Platform-Specific Idioms

**Hardware access patterns:**

```pascal
// ✅ GOOD: Direct memory access when needed
procedure FastCopy(src, dest: pointer; size: word);
begin
  DMA_Copy(src, dest, size);
end;

// ✅ GOOD: Port I/O for hardware
procedure SetLED(led: byte; state: boolean);
begin
  if state then
    PortOut(LED_PORT, PortIn(LED_PORT) or (1 shl led))
  else
    PortOut(LED_PORT, PortIn(LED_PORT) and not (1 shl led));
end;
```

---

## Performance-Critical Patterns

### Hot Path Optimization

**Optimize frequently executed code:**

```pascal
// ✅ GOOD: Inline critical functions
{$INLINE ON}
function FastDistance(x1, y1, x2, y2: Q8.8): Q8.8;
begin
  // Optimized distance calculation
  FastDistance := Sqrt(Sqr(x2 - x1) + Sqr(y2 - y1));
end;

// ✅ GOOD: Unroll small loops
{$UNROLL 4}
procedure ProcessFourEntities;
var
  i: integer;
begin
  for i := 0 to 3 do
    UpdateEntity(Entities[i]);
end;
```

### Data Locality

**Keep related data together:**

```pascal
// ✅ GOOD: Structure-of-Arrays (cache-friendly)
type
  TEntityData = record
    X, Y: array[0..99] of Q8.8;
    VelocityX, VelocityY: array[0..99] of Q8.8;
  end;

// Process all X, then all Y (better cache usage)
procedure UpdateAllX(var data: TEntityData);
var
  i: integer;
begin
  for i := 0 to EntityCount - 1 do
    data.X[i] := data.X[i] + data.VelocityX[i];
end;
```

### Branch Prediction

**Help CPU predict branches:**

```pascal
// ✅ GOOD: Sort by probability
// Most common case first
if MostCommonCondition then
  HandleCommonCase
else if LessCommonCondition then
  HandleLessCommonCase
else
  HandleRareCase;

// ✅ GOOD: Avoid branches when possible
// Use arithmetic instead of if
result := (condition and value1) or (not condition and value2);
```

### Memory Pool Pattern

**Reuse allocations:**

```pascal
type
  TMemoryPool = record
    Blocks: array[0..99] of pointer;
    FreeList: array[0..99] of integer;
    FreeCount: integer;
  end;

function PoolAllocate(var pool: TMemoryPool; size: word): pointer;
begin
  if pool.FreeCount > 0 then
  begin
    pool.FreeCount := pool.FreeCount - 1;
    PoolAllocate := pool.Blocks[pool.FreeList[pool.FreeCount]];
  end
  else
    PoolAllocate := GetMem(size);
end;
```

---

## Maintainability Patterns

### Clear Naming

**Self-documenting code:**

```pascal
// ✅ GOOD: Clear names
function CalculateDistanceBetweenPoints(x1, y1, x2, y2: Q8.8): Q8.8;

// ❌ BAD: Unclear names
function Calc(x1, y1, x2, y2: Q8.8): Q8.8;
```

### Single Responsibility

**One function, one purpose:**

```pascal
// ✅ GOOD: Single responsibility
procedure UpdatePlayerPosition(var player: TPlayer);
procedure RenderPlayer(const player: TPlayer);
procedure HandlePlayerInput(var player: TPlayer);

// ❌ BAD: Multiple responsibilities
procedure UpdateRenderHandlePlayer(var player: TPlayer);
// Does too much
```

### Error Handling Patterns

**Consistent error handling:**

```pascal
// ✅ GOOD: Consistent pattern
function TryLoadLevel(levelName: string; out level: TLevel): boolean;
begin
  try
    level := LoadLevel(levelName);
    TryLoadLevel := true;
  except
    on E: EFileNotFound do
      TryLoadLevel := false;
  end;
end;
```

### Documentation Patterns

**Document intent:**

```pascal
{*
  Calculates the distance between two points using Euclidean distance.
  
  Parameters:
    x1, y1 - First point coordinates
    x2, y2 - Second point coordinates
    
  Returns:
    Distance as fixed-point value
    
  Performance:
    O(1) time complexity
    Uses Sqrt which may be expensive
*}
function Distance(x1, y1, x2, y2: Q8.8): Q8.8;
```

---

## Best Practices

### 1. Choose Patterns Appropriately

**Match pattern to problem:**

```pascal
// ✅ GOOD: Use pattern when it helps
// Strategy pattern for interchangeable algorithms

// ❌ BAD: Over-engineer with patterns
// Simple function is better than complex pattern
```

### 2. Keep Patterns Simple

**Don't over-complicate:**

```pascal
// ✅ GOOD: Simple, clear pattern
procedure UpdateEntity(var entity: TEntity);
begin
  entity.X := entity.X + entity.VelocityX;
end;

// ❌ BAD: Overly complex pattern
// Multiple layers of abstraction for simple operation
```

### 3. Document Pattern Usage

**Explain why pattern is used:**

```pascal
{*
  Uses Strategy pattern to allow runtime selection
  of sorting algorithm based on data characteristics.
*}
procedure SortWithStrategy(var arr: array of integer);
```

### 4. Profile Pattern Performance

**Verify patterns don't hurt performance:**

```pascal
// ✅ GOOD: Profile pattern overhead
var before := Benchmark;
UsePattern;
var after := Benchmark;
// Verify acceptable overhead
```

### 5. Follow Language Idioms

**Use SuperPascal conventions:**

```pascal
// ✅ GOOD: SuperPascal idioms
// Records for data, classes for behavior
// Fixed-point for game math
// ECS for game entities

// ❌ BAD: Fighting the language
// Using patterns that don't fit SuperPascal
```

---

## Exercises

### Exercise 1: Design Pattern

Implement a design pattern:
1. Choose a pattern (Strategy, Observer, etc.)
2. Implement it in SuperPascal
3. Use it in a practical example
4. Explain when to use it

### Exercise 2: Functional Pattern

Write functional code:
1. Implement higher-order function
2. Use function composition
3. Apply immutability patterns
4. Compare with imperative approach

### Exercise 3: Performance Pattern

Optimize with patterns:
1. Identify performance bottleneck
2. Apply performance pattern
3. Measure improvement
4. Verify correctness

### Exercise 4: Idiom Usage

Use SuperPascal idioms:
1. Identify appropriate idiom
2. Refactor code to use idiom
3. Compare before/after
4. Document benefits

---

**Previous Chapter:** [Chapter 29: Optimization Techniques](../29_OptimizationTechniques/README.md)  
**Next Chapter:** [Chapter 31: Capstone Project Guide](../31_CapstoneProjectGuide/README.md)  
**Last Updated:** 2025-01-XX

