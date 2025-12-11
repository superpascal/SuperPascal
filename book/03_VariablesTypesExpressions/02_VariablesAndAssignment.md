# Variables and Assignment

**Part of:** [Chapter 02: Variables, Types, and Expressions](./README.md)

---

## Introduction

**Variables** are named storage locations that hold values. They're the foundation of programming — without variables, programs couldn't store or manipulate data.

This chapter teaches you:
- How to declare variables
- How to assign values to variables
- How variables work in memory
- Variable scope and lifetime
- Best practices for naming and using variables

---

## What is a Variable?

A **variable** is:
- **A named location** in memory
- **A container** that holds a value
- **Mutable** — its value can change
- **Typed** — it can only hold values of its declared type

### Variables Are Named Boxes

Remember from [Chapter 1: What Is a Computer?](../01_Introduction/01_WhatIsAComputer.md)? We learned that memory is like "many numbered boxes." Variables are **named boxes** in that memory.

**The memory metaphor:**
- Memory has numbered boxes (addresses)
- Variables give names to those boxes
- When you use a variable, the computer looks in that box

**Visual example:**
```
Memory Box 100: [42]  ← This box has address 100
Variable x:     [42]  ← Variable x points to box 100
```

When you write:
```pascal
var x: integer;
x := 42;
```

**What happens:**
1. Computer finds an empty memory box (e.g., Box 100)
2. Computer stores 42 in that box
3. Computer remembers: "x" means "Box 100"
4. When you use `x`, computer looks in Box 100

**Analogy:** Think of a variable like a labeled box:
- **Name** — The label (e.g., "age")
- **Type** — What can go in the box (e.g., "integer")
- **Value** — What's currently in the box (e.g., 25)
- **Address** — The box number in memory (e.g., Box 100)

> **Note:** The technical details of how variables are stored in memory—including addresses, bytes, alignment, and memory layout—are covered in **Part IV: Computers in Depth** for advanced students.

---

## Declaring Variables

### Variable Declaration Syntax

**Basic syntax:**
```pascal
var variableName: type;
```

**Multiple variables of same type:**
```pascal
var variable1, variable2, variable3: type;
```

**Multiple variables of different types:**
```pascal
var
  variable1: type1;
  variable2: type2;
  variable3: type3;
```

### Declaration Location

**Variables are declared in a `var` block:**
```pascal
program Example;
var
  x: integer;
  name: string;
begin
  // Use variables here
end.
```

**Rules:**
- `var` block comes after `program`/`unit` declaration
- `var` block comes before `begin`
- Variables are available in the block where they're declared

### Example Declarations

```pascal
program Variables;
var
  age: integer;
  score: integer;
  playerName: string;
  isAlive: boolean;
  x, y: Q8.8;  // Multiple variables, same type
begin
  // Variables can be used here
end.
```

---

## Assigning Values

### Assignment Operator

**Syntax:** `variable := value;`

**The `:=` operator:**
- **Read as:** "becomes" or "is assigned"
- **Not `=`** — That's for comparison
- **Direction:** Right side assigned to left side

**Example:**
```pascal
var x: integer;
begin
  x := 10;      // x becomes 10
  x := 20;      // x becomes 20 (previous value replaced)
  x := x + 5;   // x becomes 25 (read x, add 5, assign back)
end.
```

### Initialization

**Variables start uninitialized** (contain garbage/undefined values):
```pascal
var x: integer;
begin
  // x has undefined value here!
  WriteLn(x);  // ERROR: Using uninitialized variable
end.
```

**Always initialize before use:**
```pascal
var x: integer;
begin
  x := 0;      // Initialize
  WriteLn(x); // OK: x is initialized
end.
```

### Multiple Assignments

**Assign to multiple variables:**
```pascal
var x, y, z: integer;
begin
  x := 10;
  y := 20;
  z := 30;
  
  // Or all at once (if same value)
  x := 0;
  y := 0;
  z := 0;
end.
```

---

## Variable Types and Values

### Type Compatibility

**Variables can only hold values of their declared type:**
```pascal
var x: integer;
var name: string;
begin
  x := 42;        // OK: integer to integer
  name := 'Test'; // OK: string to string
  x := 'hello';   // ERROR: Can't assign string to integer
  name := 42;     // ERROR: Can't assign integer to string
end.
```

### Type Conversions

**Explicit conversions:**
```pascal
var x: integer;
var b: byte;
begin
  x := 100;
  b := byte(x);  // Convert integer to byte (if in range)
  
  // Conversions must be explicit
  b := x;        // ERROR: Need explicit conversion
end.
```

**Safe conversions:**
- `integer` → `byte` (if value fits: 0-255)
- `byte` → `integer` (always safe)
- `char` ↔ `byte` (ASCII code)

**Unsafe conversions:**
- `integer` → `byte` (if value > 255, truncation)
- Large `integer` → small `integer` (if out of range)

---

## Variable Scope

### What is Scope?

**Scope** is where a variable is visible and can be used.

**Local scope:**
- Variables declared in a procedure/function
- Only visible within that procedure/function
- Not visible outside

**Global scope:**
- Variables declared in `program`/`unit`
- Visible throughout the program/unit
- Can be used anywhere

### Local Variables

```pascal
program Scope;
var
  globalX: integer;  // Global variable

procedure Test;
var
  localY: integer;  // Local variable
begin
  globalX := 10;     // OK: Can access global
  localY := 20;      // OK: Can access local
end;

begin
  globalX := 5;      // OK: Can access global
  localY := 15;      // ERROR: localY not visible here
end.
```

### Scope Rules

1. **Variables are visible** from their declaration point to the end of their block
2. **Inner scopes** can access outer scope variables
3. **Outer scopes** cannot access inner scope variables
4. **Same name** in different scopes creates separate variables

**Example:**
```pascal
program ScopeExample;
var x: integer;  // Global x

procedure Test;
var x: integer;  // Local x (different from global)
begin
  x := 10;       // Modifies local x
  // Global x is still 0 (or uninitialized)
end;

begin
  x := 5;        // Modifies global x
  Test;          // Local x in Test is separate
  WriteLn(x);    // Still 5 (global x unchanged)
end.
```

---

## Variable Lifetime

### What is Lifetime?

**Lifetime** is when a variable exists in memory.

**Global variables:**
- **Created:** When program starts
- **Destroyed:** When program ends
- **Lifetime:** Entire program execution

**Local variables:**
- **Created:** When procedure/function is called
- **Destroyed:** When procedure/function returns
- **Lifetime:** Single function call

### Stack vs. Heap

**Stack (local variables):**
- Fast allocation/deallocation
- Automatic cleanup
- Limited size
- Used for local variables

**Heap (dynamic allocation):**
- Slower allocation/deallocation
- Manual cleanup required
- Larger size available
- Used for pointers, dynamic arrays (future)

**For now:** Focus on stack variables (local and global).

---

## Memory Model

### How Variables are Stored

**Memory layout:**
```
Address    Variable    Value    Type
$1000      age         25       integer (2 bytes)
$1002      score      100      integer (2 bytes)
$1004      name        "John"   string (5 bytes: 1 length + 4 chars)
$1009      isAlive    true      boolean (1 byte)
```

**Understanding addresses:**
- Each variable has a **memory address**
- Addresses are platform-specific
- Compiler manages addresses automatically
- You don't need to know exact addresses (usually)

### Memory Visualization

**Think of memory as a grid:**
```
Memory Address    Contents
─────────────────────────
$1000            [age: 25]
$1002            [score: 100]
$1004            [name: "John"]
$1009            [isAlive: true]
```

**When you assign:**
```pascal
age := 30;
```

**Memory changes:**
```
$1000            [age: 30]  ← Changed from 25 to 30
```

---

## Variable Naming

### Naming Rules

**Valid names:**
- Start with letter or underscore
- Contain letters, digits, underscores
- Case-insensitive (but use consistent casing)
- No spaces or special characters

**Valid examples:**
```pascal
var
  age: integer;
  playerName: string;
  score_1: integer;
  _temp: integer;
```

**Invalid examples:**
```pascal
var
  1age: integer;      // ERROR: Can't start with digit
  player name: string; // ERROR: No spaces
  score-1: integer;    // ERROR: No hyphens
  my.name: string;     // ERROR: No dots
```

### Naming Conventions

**Recommended style:**
- **camelCase** for variables: `playerName`, `gameScore`
- **PascalCase** for types: `TPlayer`, `TGameState`
- **UPPER_CASE** for constants: `MAX_PLAYERS`, `SCREEN_WIDTH`
- **Descriptive names** — `age` not `a`, `playerName` not `pn`

**Good names:**
```pascal
var
  playerHealth: integer;
  enemyCount: integer;
  gameState: string;
  isPaused: boolean;
```

**Bad names:**
```pascal
var
  x: integer;        // Too vague
  temp: integer;     // Not descriptive
  data: integer;     // Too generic
  var1: integer;     // Not meaningful
```

---

## Best Practices

### 1. Always Initialize

**Bad:**
```pascal
var x: integer;
begin
  WriteLn(x);  // ERROR: Uninitialized
end.
```

**Good:**
```pascal
var x: integer;
begin
  x := 0;      // Initialize
  WriteLn(x);  // OK
end.
```

### 2. Use Descriptive Names

**Bad:**
```pascal
var a, b, c: integer;
```

**Good:**
```pascal
var playerX, playerY, playerScore: integer;
```

### 3. Declare Near Use

**Bad:**
```pascal
program Example;
var
  x: integer;  // Declared here
  // ... 100 lines of code ...
begin
  x := 10;     // Used here (far from declaration)
end.
```

**Good:**
```pascal
program Example;
begin
  var x: integer;  // Declared near use (if language supports)
  x := 10;
end.
```

**Note:** SuperPascal requires `var` blocks at the top, but keep related variables together.

### 4. One Purpose Per Variable

**Bad:**
```pascal
var temp: integer;  // Used for multiple purposes
begin
  temp := playerX;
  // ... later ...
  temp := score;   // Same variable, different purpose
end.
```

**Good:**
```pascal
var
  savedX: integer;
  currentScore: integer;
begin
  savedX := playerX;
  // ... later ...
  currentScore := score;
end.
```

### 5. Use Appropriate Types

**Bad:**
```pascal
var age: integer;  // Overkill for 0-120 range
```

**Good:**
```pascal
var age: byte;  // Fits 0-255, saves memory
```

---

## Common Patterns

### Counter Pattern

```pascal
var count: integer;
begin
  count := 0;        // Initialize
  count := count + 1; // Increment
  count := count + 1; // Increment again
  WriteLn('Count: ', count); // 2
end.
```

### Accumulator Pattern

```pascal
var sum: integer;
var value: integer;
begin
  sum := 0;          // Initialize accumulator
  value := 10;
  sum := sum + value; // Add to accumulator
  value := 20;
  sum := sum + value; // Add more
  WriteLn('Sum: ', sum); // 30
end.
```

### Swap Pattern

```pascal
var x, y, temp: integer;
begin
  x := 10;
  y := 20;
  
  // Swap values
  temp := x;
  x := y;
  y := temp;
  
  WriteLn('x: ', x, ', y: ', y); // x: 20, y: 10
end.
```

---

## Platform Considerations

### Memory Efficiency

**On ZealZ80 (512KB RAM):**
- Use `byte` when possible (saves 1 byte per variable)
- Use `Q8.8` for coordinates (efficient, fast)
- Avoid unnecessary variables

**Example:**
```pascal
// Efficient
var x, y: Q8.8;      // 2 bytes each
var color: byte;     // 1 byte

// Less efficient
var x, y: integer;   // 2 bytes each (but overkill if values are small)
var color: integer;  // 2 bytes (but only needs 0-255)
```

### Variable Scope

**Minimize global variables:**
- Use local variables when possible
- Reduces memory usage (locals are freed when function returns)
- Makes code clearer (variables used near declaration)

---

## Summary

**Key Concepts:**
- **Variables** are named storage locations
- **Declaration** creates a variable: `var name: type;`
- **Assignment** stores a value: `name := value;`
- **Scope** determines where a variable is visible
- **Lifetime** determines when a variable exists

**Best Practices:**
- Always initialize variables
- Use descriptive names
- Choose appropriate types
- Minimize global variables
- Declare near use (when possible)

**Next:** Learn about arithmetic operations and expressions.

---

**Next Section:** [Arithmetic and Operators](./03_ArithmeticAndOperators.md)  
**Language Specification:** See [03_TypeSystem.md](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

