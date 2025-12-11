# While Loops

**Part of:** [Chapter 06: Loops and Iteration](./README.md)

---

> **For GCSE students:**  
> Loops let you repeat code. A `while` loop keeps running as long as a condition is true. Think of it like "keep doing this while something is true."
>
> **For A-Level students:**  
> Loops enable iteration over data and conditional repetition. `while` loops check a condition before each iteration. Understanding loop invariants and termination conditions is important for correctness.
>
> **For University students:**  
> Loops implement iteration, a fundamental control structure. `while` loops are pre-test loops with O(1) condition check per iteration. Understanding loop complexity, termination proofs, and loop optimization is essential for algorithm analysis.

---

## Introduction

**Loops** let you repeat code multiple times. They're essential for:
- **Processing data** — Handle each item in a collection
- **Game loops** — Update and draw game every frame
- **User input** — Keep asking until valid input
- **Iteration** — Repeat until a condition is met

This chapter starts with **`while` loops** — the most fundamental loop type.

---

## What is a While Loop?

A **`while` loop** repeats code **while a condition is true**:
- **Checks condition first** — Before each iteration
- **Repeats if true** — Continues while condition is true
- **Stops if false** — Exits when condition becomes false
- **May never execute** — If condition is false initially

**Analogy:** Think of a `while` loop like "keep doing this while something is true":
- **While** it's raining → stay inside
- **While** score < 100 → keep playing
- **While** user hasn't quit → keep running

---

## Basic While Loop Syntax

### Syntax

**Basic `while` loop:**
```pascal
while condition do
  statement;
```

**With block:**
```pascal
while condition do
begin
  statement1;
  statement2;
end;
```

### Simple Example

```pascal
var count: integer;
begin
  count := 0;
  while count < 5 do
  begin
    WriteLn('Count: ', count);
    count := count + 1;
  end;
end.
```

**Output:**
```
Count: 0
Count: 1
Count: 2
Count: 3
Count: 4
```

---

## How While Loops Work

### Execution Flow

1. **Check condition** — Is it true?
2. **If true** — Execute loop body
3. **Repeat** — Go back to step 1
4. **If false** — Exit loop, continue after loop

### Step-by-Step Example

```pascal
var i: integer;
begin
  i := 0;
  while i < 3 do
  begin
    WriteLn('i = ', i);
    i := i + 1;
  end;
  WriteLn('Loop finished');
end.
```

**Execution:**
- **Iteration 1:** Check `i < 3` (0 < 3 = true) → Print "i = 0" → i becomes 1
- **Iteration 2:** Check `i < 3` (1 < 3 = true) → Print "i = 1" → i becomes 2
- **Iteration 3:** Check `i < 3` (2 < 3 = true) → Print "i = 2" → i becomes 3
- **Check:** `i < 3` (3 < 3 = false) → Exit loop
- **After loop:** Print "Loop finished"

**Output:**
```
i = 0
i = 1
i = 2
Loop finished
```

---

## Common While Loop Patterns

### Pattern 1: Counting

```pascal
var counter: integer;
begin
  counter := 0;
  while counter < 10 do
  begin
    WriteLn('Counter: ', counter);
    counter := counter + 1;
  end;
end.
```

### Pattern 2: User Input Validation

```pascal
var value: integer;
begin
  value := -1;  // Invalid initial value
  while (value < 0) or (value > 100) do
  begin
    Write('Enter a value (0-100): ');
    ReadLn(value);  // Get user input
  end;
  WriteLn('Valid value: ', value);
end.
```

### Pattern 3: Processing Until Done

```pascal
var isDone: boolean;
var count: integer;
begin
  isDone := false;
  count := 0;
  while not isDone do
  begin
    count := count + 1;
    WriteLn('Processing... ', count);
    
    if count >= 5 then
      isDone := true;  // Exit condition
  end;
  WriteLn('Done!');
end.
```

### Pattern 4: Game Loop

```pascal
var isGameOver: boolean;
begin
  isGameOver := false;
  while not isGameOver do
  begin
    UpdateGame;      // Update game state
    DrawGame;        // Draw game graphics
    HandleInput;     // Handle user input
    
    if CheckGameOver then
      isGameOver := true;
  end;
  WriteLn('Game Over!');
end.
```

---

## Infinite Loops

### What is an Infinite Loop?

**An infinite loop** runs forever because the condition never becomes false.

**Example:**
```pascal
var x: integer;
begin
  x := 0;
  while x < 5 do
  begin
    WriteLn('x = ', x);
    // Forgot to increment x!
    // x never changes, so loop never ends!
  end;
end.
```

### How to Avoid Infinite Loops

**Always ensure the condition can become false:**
```pascal
var x: integer;
begin
  x := 0;
  while x < 5 do
  begin
    WriteLn('x = ', x);
    x := x + 1;  // Increment x so condition eventually becomes false
  end;
end.
```

### Breaking Infinite Loops

**If you create an infinite loop:**
- **Stop the program** — Use IDE's stop button
- **Fix the condition** — Make sure it can become false
- **Add a break condition** — Use a counter or flag

---

## While Loop Examples

### Example 1: Sum Numbers

```pascal
var sum: integer;
var count: integer;
begin
  sum := 0;
  count := 1;
  while count <= 10 do
  begin
    sum := sum + count;
    count := count + 1;
  end;
  WriteLn('Sum of 1 to 10: ', sum);
end.
```

### Example 2: Find Maximum

```pascal
var numbers: array[0..4] of integer;
var i: integer;
var max: integer;
begin
  numbers[0] := 10;
  numbers[1] := 5;
  numbers[2] := 20;
  numbers[3] := 15;
  numbers[4] := 8;
  
  max := numbers[0];
  i := 1;
  while i < 5 do
  begin
    if numbers[i] > max then
      max := numbers[i];
    i := i + 1;
  end;
  WriteLn('Maximum: ', max);
end.
```

### Example 3: Countdown

```pascal
var timer: integer;
begin
  timer := 10;
  while timer > 0 do
  begin
    WriteLn('Time: ', timer);
    timer := timer - 1;
    Wait(1000);  // Wait 1 second (if Wait function exists)
  end;
  WriteLn('Time up!');
end.
```

---

## Nested While Loops

### What is Nesting?

**Nested loops** are loops inside other loops:
```pascal
while condition1 do
  while condition2 do
    statement;
```

### Example: Multiplication Table

```pascal
var i, j: integer;
begin
  i := 1;
  while i <= 3 do
  begin
    j := 1;
    while j <= 3 do
    begin
      Write(i * j, ' ');
      j := j + 1;
    end;
    WriteLn;  // New line
    i := i + 1;
  end;
end.
```

**Output:**
```
1 2 3
2 4 6
3 6 9
```

### When to Use Nested Loops

**Use nested loops for:**
- **2D arrays** — Process rows and columns
- **Grids** — Check each cell
- **Nested structures** — Process items within items

**Be careful:**
- **Performance** — Nested loops can be slow (O(n²))
- **Complexity** — Harder to understand and debug
- **On ZealZ80** — Keep nested loops simple for performance

---

## Loop Control

### Early Exit (if supported)

**Some Pascal dialects support `break` to exit early:**
```pascal
var i: integer;
begin
  i := 0;
  while i < 10 do
  begin
    if i = 5 then
      break;  // Exit loop early
    WriteLn(i);
    i := i + 1;
  end;
end.
```

**Note:** Check SuperPascal specification for `break` support.

### Continue (if supported)

**Some Pascal dialects support `continue` to skip to next iteration:**
```pascal
var i: integer;
begin
  i := 0;
  while i < 10 do
  begin
    i := i + 1;
    if (i mod 2) = 0 then
      continue;  // Skip even numbers
    WriteLn(i);  // Only prints odd numbers
  end;
end.
```

**Note:** Check SuperPascal specification for `continue` support.

---

## Best Practices

### 1. Initialize Loop Variables

**Bad:**
```pascal
var count: integer;
while count < 10 do  // count is uninitialized!
```

**Good:**
```pascal
var count: integer;
count := 0;
while count < 10 do
```

### 2. Update Loop Variables

**Bad:**
```pascal
var i: integer;
i := 0;
while i < 10 do
  WriteLn(i);
  // Forgot to increment i - infinite loop!
```

**Good:**
```pascal
var i: integer;
i := 0;
while i < 10 do
begin
  WriteLn(i);
  i := i + 1;  // Always update loop variable
end;
```

### 3. Use Clear Conditions

**Bad:**
```pascal
while x do  // What does x mean?
```

**Good:**
```pascal
while not isDone do  // Clear what condition checks
```

### 4. Avoid Infinite Loops

**Always ensure condition can become false:**
```pascal
var count: integer;
count := 0;
while count < 10 do
begin
  // Do something
  count := count + 1;  // Ensure count increases
end;
```

---

## Platform Considerations

### Performance

**While loops:**
- **Efficient** — CPU can branch efficiently
- **Conditional** — Only runs while condition is true
- **On ZealZ80** — Keep loops simple for performance

### Memory

**Loop variables:**
- **Stack allocated** — Local variables use stack
- **Minimal overhead** — Loop variables are small
- **On ZealZ80** — Be mindful of stack usage in nested loops

---

## Common Mistakes

### Mistake 1: Infinite Loop

**Wrong:**
```pascal
var i: integer;
i := 0;
while i < 10 do
  WriteLn(i);
  // i never changes - infinite loop!
```

**Correct:**
```pascal
var i: integer;
i := 0;
while i < 10 do
begin
  WriteLn(i);
  i := i + 1;  // Update i
end;
```

### Mistake 2: Wrong Condition

**Wrong:**
```pascal
var i: integer;
i := 0;
while i > 10 do  // Condition is false initially - loop never runs!
  // ...
```

**Correct:**
```pascal
var i: integer;
i := 0;
while i < 10 do  // Correct condition
  // ...
```

### Mistake 3: Missing Begin/End

**Wrong:**
```pascal
var i: integer;
i := 0;
while i < 10 do
  WriteLn(i);
  i := i + 1;  // Always executes! Not part of loop!
```

**Correct:**
```pascal
var i: integer;
i := 0;
while i < 10 do
begin
  WriteLn(i);
  i := i + 1;  // Part of loop
end;
```

---

## Summary

**Key Concepts:**
- **`while` loops** repeat while condition is true
- **Condition checked first** — Before each iteration
- **May never execute** — If condition is false initially
- **Must update condition** — To avoid infinite loops
- **Nested loops** — Loops inside loops

**Syntax:**
```pascal
while condition do
begin
  statements;
end;
```

**Best Practices:**
- Initialize loop variables
- Update loop variables
- Use clear conditions
- Avoid infinite loops
- Use begin/end for multiple statements

**Next:** Learn about repeat-until loops.

---

**Next Section:** [Repeat-Until Loops](./02_RepeatUntilLoops.md)  
**Language Specification:** See [02_Grammar.md](../../languageSpecification/02_Grammar.md)  
**Last Updated:** 2025-01-XX

