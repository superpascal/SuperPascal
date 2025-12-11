# For Loops

**Part of:** [Chapter 05: Loops and Iteration](./README.md)

---

## Introduction

**`for` loops** are perfect when you know **exactly how many times** you want to repeat code. They're the most common loop type for:
- **Counting** — Iterate a specific number of times
- **Array processing** — Visit each element
- **Ranges** — Process values in a range

This chapter teaches you how to use `for` loops effectively.

---

## What is a For Loop?

A **`for` loop** repeats code a **specific number of times**:
- **Counter variable** — Automatically managed
- **Start value** — Where to begin
- **End value** — Where to end
- **Automatic increment** — Counter increases/decreases automatically
- **Exact count** — Know exactly how many iterations

**Analogy:** Think of a `for` loop like counting:
- **Count from 1 to 10** — Do something each time
- **Visit each item** — Process each element in a list
- **Iterate exactly N times** — Repeat a specific number of times

---

## Basic For Loop Syntax

### Syntax: `to` (Counting Up)

**Basic `for` loop counting up:**
```pascal
for variable := startValue to endValue do
  statement;
```

**With block:**
```pascal
for variable := startValue to endValue do
begin
  statement1;
  statement2;
end;
```

### Simple Example

```pascal
var i: integer;
begin
  for i := 1 to 5 do
    WriteLn('Count: ', i);
end.
```

**Output:**
```
Count: 1
Count: 2
Count: 3
Count: 4
Count: 5
```

### Syntax: `downto` (Counting Down)

**Basic `for` loop counting down:**
```pascal
for variable := startValue downto endValue do
  statement;
```

**Example:**
```pascal
var i: integer;
begin
  for i := 5 downto 1 do
    WriteLn('Countdown: ', i);
end.
```

**Output:**
```
Countdown: 5
Countdown: 4
Countdown: 3
Countdown: 2
Countdown: 1
```

---

## How For Loops Work

### Execution Flow (to)

1. **Initialize** — Set variable to start value
2. **Check** — Is variable <= end value?
3. **If yes** — Execute body, increment variable, go to step 2
4. **If no** — Exit loop

### Step-by-Step Example

```pascal
var i: integer;
begin
  for i := 1 to 3 do
    WriteLn('i = ', i);
end.
```

**Execution:**
- **Initialize:** i = 1
- **Iteration 1:** Check `i <= 3` (1 <= 3 = true) → Print "i = 1" → i becomes 2
- **Iteration 2:** Check `i <= 3` (2 <= 3 = true) → Print "i = 2" → i becomes 3
- **Iteration 3:** Check `i <= 3` (3 <= 3 = true) → Print "i = 3" → i becomes 4
- **Check:** `i <= 3` (4 <= 3 = false) → Exit loop

**Output:**
```
i = 1
i = 2
i = 3
```

### Execution Flow (downto)

1. **Initialize** — Set variable to start value
2. **Check** — Is variable >= end value?
3. **If yes** — Execute body, decrement variable, go to step 2
4. **If no** — Exit loop

---

## Common For Loop Patterns

### Pattern 1: Counting

```pascal
var i: integer;
begin
  for i := 1 to 10 do
    WriteLn('Number: ', i);
end.
```

### Pattern 2: Summing

```pascal
var i: integer;
var sum: integer;
begin
  sum := 0;
  for i := 1 to 10 do
    sum := sum + i;
  WriteLn('Sum: ', sum);
end.
```

### Pattern 3: Array Processing

```pascal
var numbers: array[0..4] of integer;
var i: integer;
begin
  numbers[0] := 10;
  numbers[1] := 20;
  numbers[2] := 30;
  numbers[3] := 40;
  numbers[4] := 50;
  
  for i := 0 to 4 do
    WriteLn('Element ', i, ': ', numbers[i]);
end.
```

### Pattern 4: Countdown

```pascal
var i: integer;
begin
  for i := 10 downto 1 do
  begin
    WriteLn('Time: ', i);
    Wait(1000);  // Wait 1 second
  end;
  WriteLn('Time up!');
end.
```

---

## For Loop Examples

### Example 1: Multiplication Table

```pascal
var i, j: integer;
begin
  for i := 1 to 5 do
  begin
    for j := 1 to 5 do
      Write(i * j, ' ');
    WriteLn;  // New line
  end;
end.
```

**Output:**
```
1 2 3 4 5
2 4 6 8 10
3 6 9 12 15
4 8 12 16 20
5 10 15 20 25
```

### Example 2: Factorial

```pascal
var i: integer;
var factorial: integer;
begin
  factorial := 1;
  for i := 1 to 5 do
    factorial := factorial * i;
  WriteLn('5! = ', factorial);
end.
```

**Output:**
```
5! = 120
```

### Example 3: Array Initialization

```pascal
var arr: array[0..9] of integer;
var i: integer;
begin
  for i := 0 to 9 do
    arr[i] := i * 2;  // Initialize each element
  
  for i := 0 to 9 do
    WriteLn('arr[', i, '] = ', arr[i]);
end.
```

### Example 4: Reverse Array

```pascal
var arr: array[0..4] of integer;
var i: integer;
begin
  arr[0] := 1;
  arr[1] := 2;
  arr[2] := 3;
  arr[3] := 4;
  arr[4] := 5;
  
  // Print in reverse
  for i := 4 downto 0 do
    WriteLn(arr[i]);
end.
```

**Output:**
```
5
4
3
2
1
```

---

## Nested For Loops

### What is Nesting?

**Nested `for` loops** are `for` loops inside other `for` loops:
```pascal
for i := 1 to 3 do
  for j := 1 to 3 do
    statement;
```

### Example: Grid Processing

```pascal
var i, j: integer;
begin
  for i := 0 to 2 do
  begin
    for j := 0 to 2 do
      Write('(', i, ',', j, ') ');
    WriteLn;  // New line after each row
  end;
end.
```

**Output:**
```
(0,0) (0,1) (0,2)
(1,0) (1,1) (1,2)
(2,0) (2,1) (2,2)
```

### Example: 2D Array Processing

```pascal
var matrix: array[0..2, 0..2] of integer;
var i, j: integer;
begin
  // Initialize
  for i := 0 to 2 do
    for j := 0 to 2 do
      matrix[i, j] := i * 3 + j;
  
  // Print
  for i := 0 to 2 do
  begin
    for j := 0 to 2 do
      Write(matrix[i, j], ' ');
    WriteLn;
  end;
end.
```

---

## Loop Variable Scope

### Loop Variable is Local

**The loop variable exists only in the loop:**
```pascal
var i: integer;
begin
  i := 100;  // Outer i
  for i := 1 to 5 do
    WriteLn('Loop i: ', i);
  WriteLn('After loop i: ', i);  // What is i here?
end.
```

**Note:** Check SuperPascal specification for loop variable scope behavior. Some Pascal dialects preserve the final value, others restore the original.

### Don't Modify Loop Variable

**Bad:**
```pascal
var i: integer;
begin
  for i := 1 to 10 do
  begin
    WriteLn(i);
    i := i + 5;  // ERROR: Don't modify loop variable!
  end;
end.
```

**Good:**
```pascal
var i: integer;
begin
  for i := 1 to 10 do
    WriteLn(i);  // Let loop handle i
end.
```

---

## For vs. While vs. Repeat-Until

### When to Use Each

**Use `for` when:**
- You know **exact number** of iterations
- You're **counting** or iterating a range
- You're **processing arrays** with known size
- You want **automatic counter management**

**Use `while` when:**
- Condition is **complex** (not just counting)
- Number of iterations is **unknown**
- You need to **check condition first**
- Loop might **not execute** (condition false initially)

**Use `repeat-until` when:**
- You need to **execute at least once**
- You're getting **user input**
- You're showing a **menu**
- Condition is checked **after** body

### Comparison Example

**Same task, different loops:**

**For loop:**
```pascal
var i: integer;
begin
  for i := 1 to 10 do
    WriteLn(i);
end.
```

**While loop:**
```pascal
var i: integer;
begin
  i := 1;
  while i <= 10 do
  begin
    WriteLn(i);
    i := i + 1;
  end;
end.
```

**Repeat-until loop:**
```pascal
var i: integer;
begin
  i := 1;
  repeat
    WriteLn(i);
    i := i + 1;
  until i > 10;
end.
```

**For loop is clearest** for this simple counting task!

---

## Best Practices

### 1. Use For for Counting

**Bad:**
```pascal
var i: integer;
i := 0;
while i < 10 do
begin
  WriteLn(i);
  i := i + 1;
end;
```

**Good:**
```pascal
var i: integer;
for i := 0 to 9 do
  WriteLn(i);
```

### 2. Use Clear Variable Names

**Bad:**
```pascal
for x := 1 to 10 do  // What does x represent?
```

**Good:**
```pascal
for i := 1 to 10 do        // Index/counter
for row := 0 to 9 do        // Row index
for playerIndex := 0 to 3 do  // Player index
```

### 3. Don't Modify Loop Variable

**Bad:**
```pascal
for i := 1 to 10 do
begin
  i := i * 2;  // Don't modify!
end;
```

**Good:**
```pascal
for i := 1 to 10 do
begin
  var doubled: integer;
  doubled := i * 2;  // Use different variable
  WriteLn(doubled);
end;
```

### 4. Use Appropriate Range

**Bad:**
```pascal
for i := 0 to 10 do
  if i > 0 then  // Skip first iteration
    WriteLn(i);
```

**Good:**
```pascal
for i := 1 to 10 do  // Start at 1
  WriteLn(i);
```

---

## Common Mistakes

### Mistake 1: Modifying Loop Variable

**Wrong:**
```pascal
for i := 1 to 10 do
  i := i + 1;  // ERROR: Don't modify loop variable!
```

**Correct:**
```pascal
for i := 1 to 10 do
  WriteLn(i);  // Let loop handle i
```

### Mistake 2: Wrong Range

**Wrong:**
```pascal
var arr: array[0..9] of integer;
for i := 1 to 10 do  // Wrong: array starts at 0!
  arr[i] := i;
```

**Correct:**
```pascal
var arr: array[0..9] of integer;
for i := 0 to 9 do  // Correct: matches array bounds
  arr[i] := i;
```

### Mistake 3: Confusing to and downto

**Wrong:**
```pascal
for i := 10 to 1 do  // ERROR: 10 > 1, so loop never runs!
  WriteLn(i);
```

**Correct:**
```pascal
for i := 10 downto 1 do  // Use downto for counting down
  WriteLn(i);
```

---

## Platform Considerations

### Performance

**For loops:**
- **Very efficient** — Compiler can optimize well
- **Predictable** — Known number of iterations
- **Fast** — Counter management is efficient
- **On ZealZ80** — Excellent performance for array processing

### Code Size

**For loops:**
- **Compact** — Less code than equivalent `while` loop
- **Optimizable** — Compiler can unroll small loops
- **On ZealZ80** — Efficient code generation

---

## Frame-Based Thinking (Game Loops)

### What is Frame-Based?

**Frame-based** means thinking in terms of **frames** (screen updates):
- **Each frame** — One iteration of the game loop
- **60 FPS** — 60 frames per second
- **Frame time** — Time between frames

### For Loop in Game Context

**While `for` loops aren't typically used for main game loops, they're perfect for:**
- **Processing entities** — Update each entity
- **Rendering sprites** — Draw each sprite
- **Processing arrays** — Handle each element

**Example:**
```pascal
var entities: array[0..99] of TEntity;
var i: integer;
begin
  // Update all entities
  for i := 0 to 99 do
    UpdateEntity(entities[i]);
  
  // Draw all entities
  for i := 0 to 99 do
    DrawEntity(entities[i]);
end.
```

**Main game loop uses `while` or `repeat-until`:**
```pascal
var isGameOver: boolean;
begin
  isGameOver := false;
  while not isGameOver do
  begin
    // Process all entities (for loop)
    for i := 0 to 99 do
      UpdateEntity(entities[i]);
    
    DrawGame;
    isGameOver := CheckGameOver;
  end;
end.
```

---

## Summary

**Key Concepts:**
- **`for` loops** repeat a specific number of times
- **`to`** counts up, **`downto`** counts down
- **Automatic counter** — Loop manages the variable
- **Perfect for arrays** — Process each element
- **Nested loops** — Loops inside loops

**Syntax:**
```pascal
for variable := start to end do
  statement;

for variable := start downto end do
  statement;
```

**When to Use:**
- **`for`** — Known number of iterations, counting, arrays
- **`while`** — Complex conditions, unknown iterations
- **`repeat-until`** — Always execute once, user input

**Best Practices:**
- Use `for` for counting and arrays
- Use clear variable names
- Don't modify loop variable
- Use appropriate range (match array bounds)

**Next:** Learn about arrays and records for structured data.

---

**Next Chapter:** [Chapter 06: Arrays and Records](../08_ArraysAndRecords/README.md)  
**Language Specification:** See [02_Grammar.md](../../languageSpecification/02_Grammar.md)  
**Last Updated:** 2025-01-XX

