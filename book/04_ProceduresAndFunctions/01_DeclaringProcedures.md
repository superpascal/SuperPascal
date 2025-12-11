# Declaring Procedures

**Part of:** [Chapter 04: Procedures and Functions](./README.md)

---

> **For GCSE students:**  
> Procedures are like recipes - you write the steps once, give it a name, and can use it whenever you need. Instead of writing the same code over and over, you write it once and call it by name.
>
> **For A-Level students:**  
> Procedures are subroutines that encapsulate code for reuse. They enable modular programming, code organization, and abstraction. Understanding procedures is fundamental to structured programming.
>
> **For University students:**  
> Procedures implement the subroutine abstraction, enabling code reuse, modularity, and separation of concerns. They have their own stack frame, parameter passing mechanisms (value, var, const), and scope. Understanding calling conventions and stack management is important for systems programming.

---

## Introduction

**Procedures** are named blocks of code that perform a task. They're one of the most important concepts in programming because they let you:
- **Reuse code** — Write once, use many times
- **Organize code** — Break programs into logical pieces
- **Simplify programs** — Hide complexity behind a name
- **Test code** — Test procedures independently

This chapter teaches you how to declare and use procedures in SuperPascal.

---

## What is a Procedure?

A **procedure** is:
- **A named block of code** — Has a name and contains statements
- **Reusable** — Can be called multiple times
- **Self-contained** — Has its own scope and variables
- **Task-oriented** — Performs a specific task

**Analogy:** Think of a procedure like a recipe:
- **Name** — "MakeCoffee" (what it does)
- **Ingredients** — Parameters (what it needs)
- **Steps** — Code (how it does it)
- **Result** — Task completed (no return value)

---

## Basic Procedure Syntax

### Declaration

**Basic syntax:**
```pascal
procedure ProcedureName;
begin
  // Statements here
end;
```

**Example:**
```pascal
procedure SayHello;
begin
  WriteLn('Hello, World!');
end;
```

### Calling a Procedure

**Syntax:** `ProcedureName;`

**Example:**
```pascal
program Example;
procedure SayHello;
begin
  WriteLn('Hello, World!');
end;

begin
  SayHello;  // Call the procedure
  SayHello;  // Call it again
end.
```

**Output:**
```
Hello, World!
Hello, World!
```

---

## Procedures with Parameters

### Value Parameters

**Parameters** are values passed to a procedure:

**Syntax:**
```pascal
procedure ProcedureName(param1: type1; param2: type2);
begin
  // Use param1 and param2 here
end;
```

**Example:**
```pascal
procedure Greet(name: string);
begin
  WriteLn('Hello, ', name, '!');
end;

begin
  Greet('Alice');
  Greet('Bob');
end.
```

**Output:**
```
Hello, Alice!
Hello, Bob!
```

**Value parameters:**
- **Copied** — Original variable is not changed
- **Local** — Parameter is a local variable in the procedure
- **Default** — This is the default parameter mode

**Example:**
```pascal
procedure Increment(x: integer);
begin
  x := x + 1;  // Only changes local copy
  WriteLn('Inside procedure: ', x);
end;

var value: integer;
begin
  value := 10;
  Increment(value);
  WriteLn('Outside procedure: ', value);  // Still 10!
end.
```

**Output:**
```
Inside procedure: 11
Outside procedure: 10
```

### Multiple Parameters

**Separate parameters with semicolons:**
```pascal
procedure PrintSum(a: integer; b: integer);
begin
  WriteLn('Sum: ', a + b);
end;

begin
  PrintSum(5, 3);      // Sum: 8
  PrintSum(10, 20);    // Sum: 30
end.
```

**Same type can be combined:**
```pascal
procedure PrintSum(a, b: integer);  // Both are integer
begin
  WriteLn('Sum: ', a + b);
end;
```

### Different Parameter Types

```pascal
procedure PrintInfo(name: string; age: integer; isStudent: boolean);
begin
  Write('Name: ', name);
  Write(', Age: ', age);
  if isStudent then
    WriteLn(', Student: Yes')
  else
    WriteLn(', Student: No');
end;

begin
  PrintInfo('Alice', 20, true);
  PrintInfo('Bob', 25, false);
end.
```

---

## Variable Parameters (`var`)

### What are Variable Parameters?

**Variable parameters** (`var`) allow a procedure to modify the original variable:

**Syntax:**
```pascal
procedure ProcedureName(var param: type);
begin
  // Changes to param affect the original variable
end;
```

**Example:**
```pascal
procedure Increment(var x: integer);
begin
  x := x + 1;  // Changes the original variable!
end;

var value: integer;
begin
  value := 10;
  Increment(value);
  WriteLn(value);  // 11 (changed!)
end.
```

**Key differences:**
- **`var` parameter** — Can modify original variable
- **Value parameter** — Cannot modify original variable
- **Must pass variable** — Can't pass literal to `var` parameter

**Example:**
```pascal
procedure Swap(var a, b: integer);
var temp: integer;
begin
  temp := a;
  a := b;
  b := temp;
end;

var x, y: integer;
begin
  x := 10;
  y := 20;
  Swap(x, y);
  WriteLn('x: ', x, ', y: ', y);  // x: 20, y: 10
end.
```

### When to Use `var`

**Use `var` when:**
- Procedure needs to **modify** the parameter
- Procedure needs to **return** a value through parameter
- Efficiency matters (avoids copying large data)

**Use value parameters when:**
- Procedure only **reads** the parameter
- You want to **protect** the original value
- Parameter is small (integer, byte, etc.)

---

## Constant Parameters (`const`)

### What are Constant Parameters?

**Constant parameters** (`const`) indicate the parameter won't be modified:

**Syntax:**
```pascal
procedure ProcedureName(const param: type);
begin
  // param cannot be modified (read-only)
end;
```

**Example:**
```pascal
procedure PrintValue(const x: integer);
begin
  WriteLn('Value: ', x);
  // x := 10;  // ERROR: Cannot modify const parameter
end;
```

**Benefits:**
- **Documentation** — Clearly shows parameter is read-only
- **Safety** — Compiler prevents accidental modification
- **Optimization** — Compiler may optimize better

**Example:**
```pascal
procedure DisplayInfo(const name: string; const age: integer);
begin
  WriteLn('Name: ', name);
  WriteLn('Age: ', age);
  // name and age cannot be modified here
end;
```

---

## Procedure Examples

### Example 1: Simple Output

```pascal
procedure PrintHeader;
begin
  WriteLn('================================');
  WriteLn('   SuperPascal Program');
  WriteLn('================================');
end;

begin
  PrintHeader;
  WriteLn('Main program code here');
  PrintHeader;
end.
```

### Example 2: Calculations

```pascal
procedure CalculateArea(width, height: integer);
var area: integer;
begin
  area := width * height;
  WriteLn('Area: ', area);
end;

begin
  CalculateArea(10, 5);   // Area: 50
  CalculateArea(7, 3);    // Area: 21
end.
```

### Example 3: Modifying Variables

```pascal
procedure ResetScore(var score: integer);
begin
  score := 0;
end;

procedure AddPoints(var score: integer; points: integer);
begin
  score := score + points;
end;

var playerScore: integer;
begin
  playerScore := 100;
  AddPoints(playerScore, 50);
  WriteLn('Score: ', playerScore);  // 150
  
  ResetScore(playerScore);
  WriteLn('Score: ', playerScore);  // 0
end.
```

### Example 4: Multiple Parameters

```pascal
procedure DrawRectangle(x, y, width, height: integer);
begin
  WriteLn('Drawing rectangle at (', x, ', ', y, ')');
  WriteLn('Size: ', width, ' x ', height);
end;

begin
  DrawRectangle(10, 20, 100, 50);
end.
```

---

## Procedure Scope

### Local Variables

**Variables declared in a procedure are local:**
```pascal
procedure Test;
var localVar: integer;  // Local to Test procedure
begin
  localVar := 10;
  WriteLn(localVar);
end;

begin
  Test;
  // localVar is not visible here
end.
```

### Parameter Scope

**Parameters are local variables:**
```pascal
procedure Example(x: integer);
begin
  // x is a local variable here
  WriteLn(x);
  x := 100;  // Only affects local x (if value parameter)
end;

var globalX: integer;
begin
  globalX := 10;
  Example(globalX);
  WriteLn(globalX);  // Still 10 (if x was value parameter)
end.
```

### Global vs. Local

```pascal
program ScopeExample;
var globalVar: integer;  // Global variable

procedure Test;
var localVar: integer;    // Local variable
begin
  globalVar := 100;  // Can access global
  localVar := 50;    // Local variable
  WriteLn('Global: ', globalVar);
  WriteLn('Local: ', localVar);
end;

begin
  globalVar := 10;
  Test;
  WriteLn('Global after Test: ', globalVar);  // 100
  // localVar is not visible here
end.
```

---

## Nested Procedures

### What are Nested Procedures?

**Procedures can be declared inside other procedures:**

```pascal
procedure Outer;
  procedure Inner;
  begin
    WriteLn('Inner procedure');
  end;
begin
  WriteLn('Outer procedure');
  Inner;  // Can call inner procedure
end;

begin
  Outer;
  // Inner;  // ERROR: Inner not visible here
end.
```

**Scope rules:**
- **Inner procedures** can access outer procedure's variables
- **Outer procedures** can call inner procedures
- **Outside** cannot call inner procedures directly

**Example:**
```pascal
procedure Calculate;
var result: integer;
  
  procedure Add(a, b: integer);
  begin
    result := a + b;  // Can access outer variable
  end;
  
begin
  Add(10, 20);
  WriteLn('Result: ', result);  // 30
end;
```

**Note:** Nested procedures are advanced. Start with top-level procedures first.

---

## Best Practices

### 1. Descriptive Names

**Bad:**
```pascal
procedure DoStuff;
procedure Proc1;
procedure X;
```

**Good:**
```pascal
procedure CalculateTotal;
procedure PrintPlayerInfo;
procedure ResetGame;
```

### 2. Single Responsibility

**Bad:**
```pascal
procedure DoEverything;
begin
  // Calculates, prints, saves, resets, etc.
end;
```

**Good:**
```pascal
procedure CalculateTotal;
procedure PrintResults;
procedure SaveData;
procedure ResetGame;
```

### 3. Use Appropriate Parameter Modes

**Use `const` for read-only:**
```pascal
procedure Display(const name: string; const age: integer);
```

**Use `var` for modification:**
```pascal
procedure Increment(var value: integer);
```

**Use value for simple data:**
```pascal
procedure Print(value: integer);
```

### 4. Keep Procedures Focused

**Each procedure should do one thing:**
```pascal
// Good: Focused procedure
procedure PrintHeader;
begin
  WriteLn('=== Header ===');
end;

// Bad: Does multiple things
procedure PrintHeaderAndFooterAndReset;
begin
  // Too many responsibilities
end;
```

### 5. Document with Comments

```pascal
// Calculates the area of a rectangle
// Parameters: width and height in pixels
// Returns: nothing (prints result)
procedure CalculateArea(width, height: integer);
begin
  // Implementation
end;
```

---

## Common Patterns

### Pattern 1: Initialization

```pascal
procedure Initialize;
begin
  // Set up initial state
  score := 0;
  level := 1;
  isGameOver := false;
end;
```

### Pattern 2: Cleanup

```pascal
procedure Cleanup;
begin
  // Free resources, reset state
  score := 0;
  // Close files, etc.
end;
```

### Pattern 3: Validation

```pascal
procedure ValidateInput(var value: integer);
begin
  if value < 0 then
    value := 0
  else if value > 100 then
    value := 100;
end;
```

### Pattern 4: Formatting

```pascal
procedure PrintFormatted(name: string; score: integer);
begin
  WriteLn('Player: ', name);
  WriteLn('Score: ', score);
  WriteLn('---');
end;
```

---

## Platform Considerations

### Memory Efficiency

**On ZealZ80 (limited memory):**
- Keep procedures small
- Minimize local variables
- Reuse procedures when possible
- Avoid deep nesting

### Performance

**Procedure calls have overhead:**
- **Small procedures** — Overhead may be significant
- **Large procedures** — Overhead is negligible
- **Frequent calls** — Consider inlining (future topic)

---

## Summary

**Key Concepts:**
- **Procedures** are named, reusable blocks of code
- **Parameters** pass data to procedures
- **Parameter modes:** value (default), `var` (modifiable), `const` (read-only)
- **Scope** determines variable visibility
- **Nested procedures** are advanced (use sparingly)

**Syntax:**
```pascal
procedure Name(param1: type1; param2: type2);
begin
  // Code
end;
```

**Best Practices:**
- Descriptive names
- Single responsibility
- Appropriate parameter modes
- Focused procedures
- Good documentation

**Next:** Learn about functions and return values.

---

---

## Exercises

### GCSE Level Exercises

**Exercise 1: Simple Procedure**
Write a procedure called `Greet` that:
1. Takes a name as a parameter
2. Displays "Hello, [name]!"
3. Call it with your name

**Exercise 2: Procedure with Multiple Parameters**
Write a procedure called `DisplayInfo` that:
1. Takes name (string) and age (integer) as parameters
2. Displays: "Name: [name], Age: [age]"
3. Call it with different values

**Exercise 3: Procedure to Draw Box**
Write a procedure called `DrawBox` that:
1. Takes width and height as parameters
2. Draws a box using asterisks (*)
3. Call it to draw boxes of different sizes

### A-Level Exercises

**Exercise 1: Parameter Modes**
Write procedures demonstrating:
1. Value parameter (modify inside procedure, original unchanged)
2. `var` parameter (modify inside procedure, original changed)
3. `const` parameter (read-only, cannot modify)
4. Test each with the same variable

**Exercise 2: Procedure Organization**
Refactor a program into procedures:
1. Take a program with 50+ lines
2. Identify logical sections
3. Extract each section into a procedure
4. Call procedures from main program
5. Compare organization before/after

**Exercise 3: Nested Procedures**
Write a program with:
1. Outer procedure that processes data
2. Inner procedure that performs calculation
3. Demonstrate scope rules
4. Explain when nested procedures are useful

### University Level Exercises

**Exercise 1: Procedure Call Stack Analysis**
Implement a procedure call tracer:
1. Track procedure entry/exit
2. Display call stack depth
3. Measure stack usage
4. Analyze recursion depth limits
5. Compare stack usage for different call patterns

**Exercise 2: Parameter Passing Performance**
Compare parameter passing mechanisms:
1. Value parameters (copy)
2. `var` parameters (reference)
3. `const` parameters (read-only reference)
4. Measure performance differences
5. Analyze memory usage
6. Determine when to use each

**Exercise 3: Procedure Abstraction Design**
Design a procedure library for:
1. Mathematical operations (add, subtract, multiply, divide)
2. Each operation as separate procedure
3. Error handling for invalid operations
4. Consistent interface design
5. Documentation of preconditions/postconditions

---

**Next Section:** [Functions and Return Values](./02_FunctionsAndReturnValues.md)  
**Language Specification:** See [02_Grammar.md](../../languageSpecification/02_Grammar.md)  
**Last Updated:** 2025-01-XX

