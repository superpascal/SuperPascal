# Designing Reusable Code

**Part of:** [Chapter 03: Procedures and Functions](./README.md)

---

## Introduction

Good code is **reusable** — it can be used in multiple places without duplication. This chapter teaches you how to design procedures and functions that are:
- **Reusable** — Can be used in different contexts
- **Modular** — Broken into logical pieces
- **Maintainable** — Easy to understand and modify
- **Testable** — Can be tested independently

---

## Why Reusability Matters

### The Problem: Code Duplication

**Bad: Duplicated code**
```pascal
begin
  // Calculate area of rectangle 1
  var area1: integer;
  area1 := 10 * 5;
  WriteLn('Area 1: ', area1);
  
  // Calculate area of rectangle 2
  var area2: integer;
  area2 := 7 * 3;
  WriteLn('Area 2: ', area2);
  
  // Calculate area of rectangle 3
  var area3: integer;
  area3 := 12 * 8;
  WriteLn('Area 3: ', area3);
end.
```

**Problems:**
- **Repetition** — Same code written multiple times
- **Errors** — Easy to make mistakes when copying
- **Maintenance** — Changes must be made in multiple places
- **Size** — Code is longer than necessary

### The Solution: Reusable Functions

**Good: Reusable function**
```pascal
function CalculateArea(width, height: integer): integer;
begin
  CalculateArea := width * height;
end;

procedure PrintArea(width, height: integer);
var area: integer;
begin
  area := CalculateArea(width, height);
  WriteLn('Area: ', area);
end;

begin
  PrintArea(10, 5);   // Area: 50
  PrintArea(7, 3);    // Area: 21
  PrintArea(12, 8);   // Area: 96
end.
```

**Benefits:**
- **No duplication** — Code written once
- **Consistency** — Same calculation everywhere
- **Maintainability** — Change once, affects all uses
- **Clarity** — Code is more readable

---

## Principles of Reusable Design

### 1. Single Responsibility

**Each procedure/function should do ONE thing:**

**Bad:**
```pascal
procedure DoEverything;
begin
  // Calculates, prints, saves, resets
  // Too many responsibilities!
end;
```

**Good:**
```pascal
function Calculate: integer;
begin
  // Only calculates
end;

procedure Print(value: integer);
begin
  // Only prints
end;

procedure Save(value: integer);
begin
  // Only saves
end;
```

### 2. Parameters Over Globals

**Use parameters instead of global variables:**

**Bad:**
```pascal
var globalX, globalY: integer;

procedure Calculate;
begin
  // Uses globalX and globalY
  // Not reusable - depends on globals
end;
```

**Good:**
```pascal
function Calculate(x, y: integer): integer;
begin
  // Uses parameters
  // Reusable - works with any x and y
end;
```

### 3. No Hidden Dependencies

**Functions should be self-contained:**

**Bad:**
```pascal
var magicNumber: integer;  // Hidden dependency

function Calculate(value: integer): integer;
begin
  Calculate := value + magicNumber;  // Depends on global
end;
```

**Good:**
```pascal
function Calculate(value, offset: integer): integer;
begin
  Calculate := value + offset;  // All dependencies explicit
end;
```

### 4. Clear Interface

**Function signature should be clear:**
```pascal
// Clear: Parameters and return type are obvious
function CalculateDistance(x1, y1, x2, y2: Q8.8): Q8.8;

// Unclear: What does this do?
function DoIt(a, b, c: integer): integer;
```

---

## Modular Design

### What is Modularity?

**Modularity** means breaking code into logical, independent pieces:
- **Each module** has a clear purpose
- **Modules interact** through well-defined interfaces
- **Modules can be** developed and tested independently

### Example: Game Module

**Break game into modules:**
```pascal
// Player module
procedure InitializePlayer(var player: TPlayer);
procedure UpdatePlayer(var player: TPlayer);
procedure DrawPlayer(player: TPlayer);

// Enemy module
procedure InitializeEnemy(var enemy: TEnemy);
procedure UpdateEnemy(var enemy: TEnemy);
procedure DrawEnemy(enemy: TEnemy);

// Collision module
function CheckCollision(obj1, obj2: TRectangle): boolean;

// Main game
begin
  InitializePlayer(player);
  InitializeEnemy(enemy);
  
  while not gameOver do
  begin
    UpdatePlayer(player);
    UpdateEnemy(enemy);
    
    if CheckCollision(player.rect, enemy.rect) then
      HandleCollision;
    
    DrawPlayer(player);
    DrawEnemy(enemy);
  end;
end.
```

---

## Scope and Visibility

### Understanding Scope

**Scope** determines where variables and procedures are visible:

**Global scope:**
```pascal
program Example;
var globalVar: integer;  // Visible everywhere

procedure Test;
begin
  globalVar := 10;  // Can access global
end;

begin
  globalVar := 5;   // Can access global
end.
```

**Local scope:**
```pascal
procedure Test;
var localVar: integer;  // Only visible in Test
begin
  localVar := 10;
end;

begin
  // localVar is not visible here
end.
```

### Visibility Rules

1. **Inner scopes** can see outer scopes
2. **Outer scopes** cannot see inner scopes
3. **Same name** in different scopes creates separate variables

**Example:**
```pascal
var x: integer;  // Global x

procedure Test;
var x: integer;  // Local x (different from global)
begin
  x := 10;       // Modifies local x
  // Global x is unchanged
end;

begin
  x := 5;        // Modifies global x
  Test;
  WriteLn(x);   // Still 5 (global x)
end.
```

### Best Practice: Minimize Globals

**Use local variables when possible:**
```pascal
// Bad: Too many globals
var playerX, playerY, playerScore, playerHealth: integer;

// Good: Use parameters and local variables
procedure UpdatePlayer(var x, y: integer; var score, health: integer);
begin
  // Use parameters, not globals
end;
```

---

## Parameter Design

### Choosing Parameter Modes

**Value parameters (default):**
- Use when function only **reads** the value
- Protects original variable
- Good for small types (integer, byte, boolean)

**`const` parameters:**
- Use when function only **reads** the value
- Documents intent (read-only)
- May allow compiler optimizations

**`var` parameters:**
- Use when function needs to **modify** the value
- Use when returning multiple values
- Use for large types (to avoid copying)

### Example: Well-Designed Function

```pascal
// Clear purpose: Calculates distance between two points
// Clear parameters: Two points (x1, y1) and (x2, y2)
// Clear return: Distance as Q8.8
// No side effects: Pure function
function CalculateDistance(x1, y1, x2, y2: Q8.8): Q8.8;
var dx, dy: Q8.8;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  // Simplified (actual would use sqrt)
  CalculateDistance := dx + dy;
end;
```

---

## Code Organization

### Group Related Functions

**Organize by functionality:**
```pascal
// Math functions
function Add(a, b: integer): integer;
function Multiply(a, b: integer): integer;
function Square(n: integer): integer;

// String functions
function FormatName(first, last: string): string;
function GetInitials(name: string): string;

// Game functions
procedure InitializeGame;
procedure UpdateGame;
procedure DrawGame;
```

### Order Matters

**Declare before use:**
```pascal
// Forward declaration (if needed)
procedure Helper; forward;

procedure Main;
begin
  Helper;  // Can call Helper
end;

procedure Helper;
begin
  // Implementation
end;
```

**Or declare in order:**
```pascal
procedure Helper;
begin
  // Implementation
end;

procedure Main;
begin
  Helper;  // Can call Helper
end;
```

---

## Testing Reusable Code

### Why Test?

**Testing ensures:**
- Code works correctly
- Code works in different situations
- Changes don't break existing functionality

### Simple Testing

**Test functions with known inputs:**
```pascal
function Add(a, b: integer): integer;
begin
  Add := a + b;
end;

begin
  // Test cases
  if Add(2, 3) <> 5 then
    WriteLn('ERROR: Add(2, 3) should be 5');
  
  if Add(0, 0) <> 0 then
    WriteLn('ERROR: Add(0, 0) should be 0');
  
  if Add(-5, 5) <> 0 then
    WriteLn('ERROR: Add(-5, 5) should be 0');
  
  WriteLn('All tests passed!');
end.
```

### Test Different Cases

**Test normal cases:**
```pascal
function Max(a, b: integer): integer;
begin
  if a > b then
    Max := a
  else
    Max := b;
end;

begin
  // Normal cases
  if Max(10, 5) <> 10 then WriteLn('ERROR');
  if Max(5, 10) <> 10 then WriteLn('ERROR');
  
  // Edge cases
  if Max(5, 5) <> 5 then WriteLn('ERROR');  // Equal values
  if Max(-10, -5) <> -5 then WriteLn('ERROR');  // Negative
end.
```

---

## Common Reusable Patterns

### Pattern 1: Utility Functions

**Create small, focused utility functions:**
```pascal
function Clamp(value, min, max: integer): integer;
begin
  if value < min then
    Clamp := min
  else if value > max then
    Clamp := max
  else
    Clamp := value;
end;

function IsInRange(value, min, max: integer): boolean;
begin
  IsInRange := (value >= min) and (value <= max);
end;
```

### Pattern 2: Initialization Procedures

**Separate initialization from main logic:**
```pascal
procedure InitializeGame;
begin
  score := 0;
  level := 1;
  isGameOver := false;
end;

begin
  InitializeGame;
  // Main game loop
end.
```

### Pattern 3: Calculation Functions

**Separate calculation from display:**
```pascal
function CalculateTotal(items: array[0..9] of integer): integer;
var i, sum: integer;
begin
  sum := 0;
  for i := 0 to 9 do
    sum := sum + items[i];
  CalculateTotal := sum;
end;

procedure DisplayTotal(items: array[0..9] of integer);
begin
  WriteLn('Total: ', CalculateTotal(items));
end;
```

---

## Refactoring: Making Code Reusable

### Step 1: Identify Duplication

**Find repeated code:**
```pascal
begin
  // Repeated calculation
  area1 := 10 * 5;
  area2 := 7 * 3;
  area3 := 12 * 8;
end.
```

### Step 2: Extract to Function

**Create reusable function:**
```pascal
function CalculateArea(width, height: integer): integer;
begin
  CalculateArea := width * height;
end;
```

### Step 3: Replace Duplication

**Use the function:**
```pascal
begin
  area1 := CalculateArea(10, 5);
  area2 := CalculateArea(7, 3);
  area3 := CalculateArea(12, 8);
end.
```

---

## Best Practices Summary

### Design Principles

1. **Single responsibility** — One thing per function
2. **Parameters over globals** — Pass data explicitly
3. **No hidden dependencies** — Make dependencies clear
4. **Clear interface** — Obvious what function does

### Organization

1. **Group related functions** — Organize by functionality
2. **Declare before use** — Or use forward declarations
3. **Minimize globals** — Use local variables and parameters

### Testing

1. **Test normal cases** — Typical usage
2. **Test edge cases** — Boundaries, special values
3. **Test error cases** — Invalid inputs (if applicable)

---

## Summary

**Key Concepts:**
- **Reusability** — Write once, use many times
- **Modularity** — Break code into logical pieces
- **Scope** — Where variables are visible
- **Parameters** — Pass data explicitly
- **Testing** — Verify code works correctly

**Principles:**
- Single responsibility
- Parameters over globals
- No hidden dependencies
- Clear interfaces

**Benefits:**
- Less code duplication
- Easier maintenance
- Better testing
- Clearer code

**Next:** Learn about control flow with conditionals and loops.

---

**Next Chapter:** [Chapter 04: Conditionals and Logic](../05_ConditionalsAndLogic/README.md)  
**Language Specification:** See [02_Grammar.md](../../languageSpecification/02_Grammar.md)  
**Last Updated:** 2025-01-XX

