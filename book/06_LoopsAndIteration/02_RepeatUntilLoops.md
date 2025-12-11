# Repeat-Until Loops

**Part of:** [Chapter 05: Loops and Iteration](./README.md)

---

## Introduction

**`repeat-until` loops** are similar to `while` loops, but with a key difference: they **always execute at least once** because the condition is checked **after** the loop body.

This chapter teaches you:
- How `repeat-until` loops work
- When to use `repeat-until` vs. `while`
- The differences between the two loop types

---

## What is a Repeat-Until Loop?

A **`repeat-until` loop**:
- **Executes body first** — Before checking condition
- **Checks condition after** — At the end of each iteration
- **Repeats if false** — Continues while condition is false
- **Stops if true** — Exits when condition becomes true
- **Always executes once** — Body runs at least once

**Key difference from `while`:**
- **`while`** — Checks condition first (may never execute)
- **`repeat-until`** — Checks condition last (always executes once)

---

## Basic Repeat-Until Syntax

### Syntax

**Basic `repeat-until` loop:**
```pascal
repeat
  statement1;
  statement2;
until condition;
```

**Note:** No `begin/end` needed — `repeat` and `until` act as block delimiters.

### Simple Example

```pascal
var count: integer;
begin
  count := 0;
  repeat
    WriteLn('Count: ', count);
    count := count + 1;
  until count >= 5;
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

## How Repeat-Until Loops Work

### Execution Flow

1. **Execute body** — Run loop statements
2. **Check condition** — Is it true?
3. **If false** — Go back to step 1
4. **If true** — Exit loop, continue after loop

### Step-by-Step Example

```pascal
var i: integer;
begin
  i := 0;
  repeat
    WriteLn('i = ', i);
    i := i + 1;
  until i >= 3;
  WriteLn('Loop finished');
end.
```

**Execution:**
- **Iteration 1:** Execute body → Print "i = 0" → i becomes 1 → Check `i >= 3` (1 >= 3 = false) → Continue
- **Iteration 2:** Execute body → Print "i = 1" → i becomes 2 → Check `i >= 3` (2 >= 3 = false) → Continue
- **Iteration 3:** Execute body → Print "i = 2" → i becomes 3 → Check `i >= 3` (3 >= 3 = true) → Exit loop
- **After loop:** Print "Loop finished"

**Output:**
```
i = 0
i = 1
i = 2
Loop finished
```

---

## While vs. Repeat-Until

### Key Differences

| Feature | `while` | `repeat-until` |
|---------|---------|----------------|
| **Condition check** | Before body | After body |
| **Minimum executions** | 0 (may never run) | 1 (always runs once) |
| **Condition logic** | Continues while true | Continues until true |
| **Syntax** | Needs `begin/end` | No `begin/end` needed |

### When Condition is False Initially

**`while` loop:**
```pascal
var x: integer;
begin
  x := 10;
  while x < 5 do  // Condition is false initially
  begin
    WriteLn(x);   // Never executes!
    x := x + 1;
  end;
end.
```

**Output:** (nothing - loop never runs)

**`repeat-until` loop:**
```pascal
var x: integer;
begin
  x := 10;
  repeat
    WriteLn(x);   // Executes once!
    x := x + 1;
  until x < 5;    // Condition checked after
end.
```

**Output:**
```
10
```

### Condition Logic

**`while` continues while condition is true:**
```pascal
while count < 10 do  // Continue while count is less than 10
```

**`repeat-until` continues until condition is true:**
```pascal
repeat
  // ...
until count >= 10;  // Continue until count is 10 or more
```

**Note:** The condition logic is **opposite** — `while` uses "less than", `repeat-until` uses "greater than or equal".

---

## Common Repeat-Until Patterns

### Pattern 1: User Input Validation

**Perfect for `repeat-until` — always ask at least once:**
```pascal
var value: integer;
begin
  repeat
    Write('Enter a value (0-100): ');
    ReadLn(value);
    if (value < 0) or (value > 100) then
      WriteLn('Invalid! Try again.');
  until (value >= 0) and (value <= 100);
  WriteLn('Valid value: ', value);
end.
```

### Pattern 2: Menu System

```pascal
var choice: integer;
begin
  repeat
    ShowMenu;
    Write('Enter choice: ');
    ReadLn(choice);
    ProcessChoice(choice);
  until choice = 0;  // 0 means quit
  WriteLn('Goodbye!');
end.
```

### Pattern 3: Game Loop

```pascal
var isGameOver: boolean;
begin
  repeat
    UpdateGame;
    DrawGame;
    HandleInput;
    isGameOver := CheckGameOver;
  until isGameOver;
  ShowGameOverScreen;
end.
```

### Pattern 4: Processing Until Done

```pascal
var isComplete: boolean;
var attempts: integer;
begin
  attempts := 0;
  repeat
    attempts := attempts + 1;
    WriteLn('Attempt ', attempts);
    isComplete := TryOperation;
  until isComplete or (attempts >= 10);
end.
```

---

## Repeat-Until Examples

### Example 1: Countdown

```pascal
var timer: integer;
begin
  timer := 5;
  repeat
    WriteLn('Time: ', timer);
    timer := timer - 1;
    Wait(1000);  // Wait 1 second
  until timer < 0;
  WriteLn('Time up!');
end.
```

### Example 2: Sum Until Threshold

```pascal
var sum: integer;
var value: integer;
begin
  sum := 0;
  repeat
    Write('Enter a number (0 to stop): ');
    ReadLn(value);
    sum := sum + value;
  until value = 0;
  WriteLn('Total sum: ', sum);
end.
```

### Example 3: Retry Logic

```pascal
var success: boolean;
var attempts: integer;
begin
  attempts := 0;
  repeat
    attempts := attempts + 1;
    WriteLn('Attempt ', attempts);
    success := TryOperation;
    if not success then
      WriteLn('Failed, retrying...');
  until success or (attempts >= 3);
  
  if success then
    WriteLn('Success!')
  else
    WriteLn('Failed after 3 attempts');
end.
```

---

## When to Use Repeat-Until

### Use `repeat-until` when:

1. **Always execute once** — You need the body to run at least once
2. **User input** — Asking for input (always ask at least once)
3. **Menu systems** — Show menu, then check if quit
4. **Retry logic** — Try operation, then check if successful
5. **Natural "until" logic** — Condition naturally reads as "until"

### Use `while` when:

1. **May not execute** — Body might not need to run
2. **Pre-check needed** — Need to check condition before executing
3. **Natural "while" logic** — Condition naturally reads as "while"
4. **Counting loops** — Iterating a known number of times (or use `for`)

---

## Nested Repeat-Until Loops

### Nesting Syntax

**Nested `repeat-until` loops:**
```pascal
repeat
  // Outer loop body
  repeat
    // Inner loop body
  until innerCondition;
until outerCondition;
```

### Example: Multiplication Table

```pascal
var i, j: integer;
begin
  i := 1;
  repeat
    j := 1;
    repeat
      Write(i * j, ' ');
      j := j + 1;
    until j > 3;
    WriteLn;  // New line
    i := i + 1;
  until i > 3;
end.
```

**Output:**
```
1 2 3
2 4 6
3 6 9
```

---

## Best Practices

### 1. Use Clear Conditions

**Bad:**
```pascal
repeat
  // ...
until x;  // What does x mean?
```

**Good:**
```pascal
repeat
  // ...
until isDone;  // Clear what condition checks
```

### 2. Ensure Condition Can Become True

**Bad:**
```pascal
var x: integer;
x := 0;
repeat
  WriteLn(x);
  x := x + 1;
until x < 0;  // x will never be < 0 - infinite loop!
```

**Good:**
```pascal
var x: integer;
x := 0;
repeat
  WriteLn(x);
  x := x + 1;
until x >= 5;  // x will eventually be >= 5
```

### 3. Use Appropriate Loop Type

**Use `repeat-until` for:**
- User input (always ask once)
- Menu systems (always show menu)
- Retry logic (always try once)

**Use `while` for:**
- Conditional execution (may not need to run)
- Pre-checked conditions
- Counting (or use `for`)

---

## Common Mistakes

### Mistake 1: Infinite Loop

**Wrong:**
```pascal
var x: integer;
x := 0;
repeat
  WriteLn(x);
  // Forgot to increment x!
until x >= 5;  // x never changes - infinite loop!
```

**Correct:**
```pascal
var x: integer;
x := 0;
repeat
  WriteLn(x);
  x := x + 1;  // Update x
until x >= 5;
```

### Mistake 2: Wrong Condition Logic

**Wrong:**
```pascal
var count: integer;
count := 0;
repeat
  WriteLn(count);
  count := count + 1;
until count < 10;  // Logic error: continues while count < 10
                   // But count starts at 0, so runs once then stops!
```

**Correct:**
```pascal
var count: integer;
count := 0;
repeat
  WriteLn(count);
  count := count + 1;
until count >= 10;  // Correct: continues until count >= 10
```

### Mistake 3: Confusing While and Until

**Remember:**
- **`while`** — Continues **while** condition is **true**
- **`until`** — Continues **until** condition is **true** (opposite logic!)

---

## Platform Considerations

### Performance

**Repeat-until loops:**
- **Same performance** as `while` loops
- **One extra check** — Condition checked after body (minimal overhead)
- **On ZealZ80** — Keep loops simple for performance

### Code Size

**Repeat-until:**
- **Slightly different codegen** — Condition at end
- **Similar size** — About same as `while`
- **On ZealZ80** — No significant difference

---

## Summary

**Key Concepts:**
- **`repeat-until` loops** always execute at least once
- **Condition checked after** — Body runs before condition check
- **Continues until true** — Opposite logic from `while`
- **No begin/end needed** — `repeat` and `until` are block delimiters
- **Use for user input** — Perfect when you always need to ask once

**Syntax:**
```pascal
repeat
  statements;
until condition;
```

**Differences from `while`:**
- **`while`** — Checks before, may not execute
- **`repeat-until`** — Checks after, always executes once
- **`while`** — Continues while true
- **`repeat-until`** — Continues until true

**Best Practices:**
- Use clear conditions
- Ensure condition can become true
- Use `repeat-until` for user input/menus
- Use `while` for conditional execution

**Next:** Learn about `for` loops for counting and iteration.

---

**Next Section:** [For Loops](./03_ForLoops.md)  
**Language Specification:** See [02_Grammar.md](../../languageSpecification/02_Grammar.md)  
**Last Updated:** 2025-01-XX

