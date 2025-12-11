# Functions and Return Values

**Part of:** [Chapter 03: Procedures and Functions](./README.md)

---

## Introduction

**Functions** are like procedures, but they **return a value**. While procedures perform tasks, functions **calculate and return results**.

This chapter teaches you:
- How to declare functions
- How to return values
- How to use function results
- When to use functions vs. procedures

---

## What is a Function?

A **function** is:
- **Like a procedure** — Named block of code
- **Returns a value** — Produces a result
- **Can be used in expressions** — Function calls are expressions
- **Has a return type** — Specifies what type of value it returns

**Analogy:** Think of a function like a calculator:
- **Input** — Parameters (numbers to calculate)
- **Process** — Code (calculation steps)
- **Output** — Return value (the result)

**Difference from procedure:**
- **Procedure:** Does something (prints, modifies, etc.)
- **Function:** Calculates something and returns it

---

## Basic Function Syntax

### Declaration

**Basic syntax:**
```pascal
function FunctionName: returnType;
begin
  // Code here
  FunctionName := value;  // Return value
end;
```

**Example:**
```pascal
function GetTen: integer;
begin
  GetTen := 10;  // Return 10
end;
```

### Using a Function

**Functions are used in expressions:**
```pascal
var result: integer;
begin
  result := GetTen;        // result is 10
  result := GetTen + 5;    // result is 15
  WriteLn(GetTen);         // Prints 10
end.
```

---

## Functions with Parameters

### Parameters Work the Same

**Functions can have parameters just like procedures:**
```pascal
function Add(a, b: integer): integer;
begin
  Add := a + b;  // Return the sum
end;

begin
  WriteLn(Add(5, 3));      // 8
  WriteLn(Add(10, 20));    // 30
end.
```

### Return Statement

**Assign to function name to return:**
```pascal
function Multiply(x, y: integer): integer;
begin
  Multiply := x * y;  // Return x * y
end;
```

**Note:** In SuperPascal, you assign to the function name to return a value. This is Pascal's traditional syntax.

---

## Return Value Examples

### Example 1: Simple Calculation

```pascal
function Square(n: integer): integer;
begin
  Square := n * n;
end;

begin
  WriteLn(Square(5));   // 25
  WriteLn(Square(10));  // 100
end.
```

### Example 2: Comparison

```pascal
function Max(a, b: integer): integer;
begin
  if a > b then
    Max := a
  else
    Max := b;
end;

begin
  WriteLn(Max(10, 5));   // 10
  WriteLn(Max(3, 8));    // 8
end.
```

### Example 3: Boolean Function

```pascal
function IsEven(n: integer): boolean;
begin
  IsEven := (n mod 2) = 0;
end;

begin
  if IsEven(8) then
    WriteLn('8 is even');
  if not IsEven(7) then
    WriteLn('7 is odd');
end.
```

### Example 4: String Function

```pascal
function Greet(name: string): string;
begin
  Greet := 'Hello, ' + name + '!';
end;

begin
  WriteLn(Greet('Alice'));  // Hello, Alice!
end.
```

---

## Return Types

### Integer Functions

```pascal
function CalculateSum(a, b, c: integer): integer;
begin
  CalculateSum := a + b + c;
end;
```

### Boolean Functions

```pascal
function IsPositive(n: integer): boolean;
begin
  IsPositive := n > 0;
end;

function IsInRange(value, min, max: integer): boolean;
begin
  IsInRange := (value >= min) and (value <= max);
end;
```

### String Functions

```pascal
function FormatName(first, last: string): string;
begin
  FormatName := first + ' ' + last;
end;
```

### Fixed-Point Functions

```pascal
function CalculateDistance(x1, y1, x2, y2: Q8.8): Q8.8;
var dx, dy: Q8.8;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  // Simplified distance (actual would use sqrt)
  CalculateDistance := dx + dy;  // Placeholder
end;
```

---

## Functions vs. Procedures

### When to Use Functions

**Use functions when:**
- You need to **calculate a value**
- You need to **return a result**
- You want to use the result in an **expression**
- The operation is a **calculation**

**Examples:**
```pascal
function CalculateArea(width, height: integer): integer;
function IsValid(value: integer): boolean;
function GetPlayerName(id: integer): string;
```

### When to Use Procedures

**Use procedures when:**
- You need to **perform an action** (print, modify, etc.)
- You don't need to **return a value**
- You need to **modify parameters** (use `var`)
- The operation is a **task**, not a calculation

**Examples:**
```pascal
procedure PrintHeader;
procedure ResetGame(var score: integer);
procedure DrawSprite(x, y: integer);
```

### Comparison Example

**Function (returns value):**
```pascal
function GetArea(width, height: integer): integer;
begin
  GetArea := width * height;
end;

var area: integer;
begin
  area := GetArea(10, 5);  // area is 50
end.
```

**Procedure (performs action):**
```pascal
procedure PrintArea(width, height: integer);
var area: integer;
begin
  area := width * height;
  WriteLn('Area: ', area);
end;

begin
  PrintArea(10, 5);  // Prints "Area: 50"
end.
```

---

## Using Functions in Expressions

### Function Calls are Expressions

**Functions can be used anywhere an expression is expected:**
```pascal
var result: integer;
begin
  result := Add(5, 3);              // Assignment
  result := Multiply(Add(2, 3), 4);  // Nested calls
  WriteLn(Square(5));               // Procedure parameter
  if IsEven(8) then                 // Condition
    WriteLn('Even');
end.
```

### Nested Function Calls

**Functions can call other functions:**
```pascal
function Add(a, b: integer): integer;
begin
  Add := a + b;
end;

function Multiply(x, y: integer): integer;
begin
  Multiply := x * y;
end;

function Calculate(a, b, c: integer): integer;
begin
  Calculate := Multiply(Add(a, b), c);
  // Calculates (a + b) * c
end;

begin
  WriteLn(Calculate(2, 3, 4));  // (2 + 3) * 4 = 20
end.
```

### Function Composition

**Build complex calculations from simple functions:**
```pascal
function Square(n: integer): integer;
begin
  Square := n * n;
end;

function Add(a, b: integer): integer;
begin
  Add := a + b;
end;

begin
  // Calculate (5 + 3)^2
  WriteLn(Square(Add(5, 3)));  // 64
end.
```

---

## Common Function Patterns

### Pattern 1: Validation

```pascal
function IsValidScore(score: integer): boolean;
begin
  IsValidScore := (score >= 0) and (score <= 100);
end;

begin
  if IsValidScore(85) then
    WriteLn('Valid score');
end.
```

### Pattern 2: Calculation

```pascal
function CalculateAverage(sum, count: integer): integer;
begin
  if count > 0 then
    CalculateAverage := sum div count
  else
    CalculateAverage := 0;
end;
```

### Pattern 3: Conversion

```pascal
function ToString(n: integer): string;
begin
  // Convert integer to string (simplified)
  ToString := 'Value: ' + IntToStr(n);  // Assuming IntToStr exists
end;
```

### Pattern 4: Lookup

```pascal
function GetStatus(code: integer): string;
begin
  case code of
    0: GetStatus := 'OK';
    1: GetStatus := 'Error';
    2: GetStatus := 'Warning';
    else GetStatus := 'Unknown';
  end;
end;
```

---

## Return Value Best Practices

### 1. Always Return a Value

**Bad:**
```pascal
function GetValue: integer;
begin
  if condition then
    GetValue := 10;
  // ERROR: What if condition is false? No return value!
end;
```

**Good:**
```pascal
function GetValue: integer;
begin
  if condition then
    GetValue := 10
  else
    GetValue := 0;  // Always return a value
end;
```

### 2. Return Appropriate Types

**Use correct return type:**
```pascal
function IsPositive(n: integer): boolean;  // Not integer!
begin
  IsPositive := n > 0;
end;
```

### 3. Document Return Values

```pascal
// Calculates the area of a rectangle
// Parameters: width and height
// Returns: area (width * height)
function CalculateArea(width, height: integer): integer;
begin
  CalculateArea := width * height;
end;
```

### 4. Keep Functions Pure (When Possible)

**Pure function** — Same input always produces same output, no side effects:
```pascal
// Pure function
function Add(a, b: integer): integer;
begin
  Add := a + b;  // No side effects
end;

// Not pure (has side effect)
function AddAndPrint(a, b: integer): integer;
begin
  WriteLn('Adding');  // Side effect!
  AddAndPrint := a + b;
end;
```

**Note:** Pure functions are easier to test and reason about.

---

## Advanced: Early Return

### Using Exit

**In some Pascal dialects, you can use `Exit` to return early:**
```pascal
function FindValue(arr: array[0..9] of integer; target: integer): integer;
var i: integer;
begin
  for i := 0 to 9 do
    if arr[i] = target then
    begin
      FindValue := i;  // Return index
      Exit;            // Exit function early
    end;
  FindValue := -1;     // Not found
end;
```

**Note:** Check SuperPascal specification for `Exit` support.

---

## Functions with `var` Parameters

### Modifying Parameters and Returning

**Functions can modify parameters AND return values:**
```pascal
function IncrementAndReturn(var x: integer): integer;
begin
  x := x + 1;           // Modify parameter
  IncrementAndReturn := x;  // Return new value
end;

var value: integer;
begin
  value := 10;
  WriteLn(IncrementAndReturn(value));  // 11
  WriteLn(value);                      // 11 (modified)
end.
```

**Use sparingly** — Can be confusing. Prefer either:
- Function that returns value (no modification)
- Procedure that modifies parameter (no return)

---

## Platform Considerations

### Return Value Handling

**Return values are handled by ABI:**
- **Small values** (integer, byte) — Returned in registers
- **Large values** (records, arrays) — Returned via pointer or stack
- **Platform-specific** — Different platforms may handle differently

**Efficiency:**
- **Small returns** — Very efficient (register)
- **Large returns** — May involve copying (consider `var` parameter instead)

---

## Summary

**Key Concepts:**
- **Functions** return values, procedures don't
- **Return syntax:** Assign to function name: `FunctionName := value;`
- **Functions are expressions** — Can be used in expressions
- **Return types** specify what the function returns
- **Functions vs. procedures** — Use functions for calculations, procedures for actions

**Syntax:**
```pascal
function Name(param: type): returnType;
begin
  Name := value;  // Return value
end;
```

**Best Practices:**
- Always return a value
- Use appropriate return types
- Document return values
- Keep functions pure when possible
- Use functions for calculations, procedures for actions

**Next:** Learn about designing reusable code with procedures and functions.

---

**Next Section:** [Designing Reusable Code](./03_DesigningReusableCode.md)  
**Language Specification:** See [02_Grammar.md](../../languageSpecification/02_Grammar.md)  
**Last Updated:** 2025-01-XX

