# If, Else, and Nested Conditions

**Part of:** [Chapter 05: Conditionals and Logic](./README.md)

---

> **For GCSE students:**  
> Conditionals let your program make choices. Think of `if` like asking a question: "If this is true, do that. Otherwise, do something else." It's like choosing what to wear based on the weather.
>
> **For A-Level students:**  
> Conditionals implement decision-making in programs. `if` statements evaluate boolean expressions and execute code conditionally. Understanding conditionals is fundamental to control flow and program logic.
>
> **For University students:**  
> Conditionals implement branching control flow based on predicate evaluation. They enable non-linear program execution. Understanding conditionals requires understanding boolean logic, short-circuit evaluation, and how conditionals affect program complexity and testability.

---

## Introduction

**Conditional statements** let your program make decisions. They're the foundation of interactive programs — programs that respond differently based on different situations.

This chapter teaches you:
- How to use `if` statements
- How to use `else` clauses
- How to nest conditions
- How to chain conditions with `else if`

---

## What is Conditional Execution?

**Conditional execution** means:
- **Code runs only if a condition is true**
- **Different code runs for different conditions**
- **Programs can make decisions**

**Analogy:** Think of a traffic light:
- **If** light is green → go
- **Else if** light is yellow → slow down
- **Else** (light is red) → stop

---

## Basic If Statement

### Syntax

**Basic `if` statement:**
```pascal
if condition then
  statement;
```

**If with block:**
```pascal
if condition then
begin
  statement1;
  statement2;
end;
```

### Simple Example

```pascal
var age: integer;
begin
  age := 18;
  if age >= 18 then
    WriteLn('You are an adult');
end.
```

**Output:**
```
You are an adult
```

### If with Multiple Statements

```pascal
var score: integer;
begin
  score := 100;
  if score > 50 then
  begin
    WriteLn('You passed!');
    WriteLn('Congratulations!');
  end;
end.
```

---

## If-Else Statement

### Syntax

**`if-else` statement:**
```pascal
if condition then
  statement1
else
  statement2;
```

**With blocks:**
```pascal
if condition then
begin
  statement1;
  statement2;
end
else
begin
  statement3;
  statement4;
end;
```

### Example

```pascal
var age: integer;
begin
  age := 15;
  if age >= 18 then
    WriteLn('You are an adult')
  else
    WriteLn('You are a minor');
end.
```

**Output:**
```
You are a minor
```

### Real-World Example

```pascal
var temperature: integer;
begin
  temperature := 25;
  if temperature > 20 then
    WriteLn('It is warm')
  else
    WriteLn('It is cold');
end.
```

---

## Else-If Chains

### Multiple Conditions

**Chain multiple conditions with `else if`:**
```pascal
if condition1 then
  statement1
else if condition2 then
  statement2
else if condition3 then
  statement3
else
  statement4;
```

### Example: Grade System

```pascal
var score: integer;
begin
  score := 85;
  if score >= 90 then
    WriteLn('Grade: A')
  else if score >= 80 then
    WriteLn('Grade: B')
  else if score >= 70 then
    WriteLn('Grade: C')
  else if score >= 60 then
    WriteLn('Grade: D')
  else
    WriteLn('Grade: F');
end.
```

**Output:**
```
Grade: B
```

### Example: Age Categories

```pascal
var age: integer;
begin
  age := 25;
  if age < 13 then
    WriteLn('Child')
  else if age < 20 then
    WriteLn('Teenager')
  else if age < 65 then
    WriteLn('Adult')
  else
    WriteLn('Senior');
end.
```

---

## Nested Conditions

### What is Nesting?

**Nested conditions** are `if` statements inside other `if` statements:
```pascal
if condition1 then
  if condition2 then
    statement;
```

### Example: Nested If

```pascal
var age: integer;
var hasLicense: boolean;
begin
  age := 18;
  hasLicense := true;
  
  if age >= 16 then
    if hasLicense then
      WriteLn('You can drive')
    else
      WriteLn('You need a license')
  else
    WriteLn('You are too young to drive');
end.
```

### Example: Complex Logic

```pascal
var score: integer;
var timeRemaining: integer;
begin
  score := 100;
  timeRemaining := 30;
  
  if score > 50 then
  begin
    if timeRemaining > 0 then
      WriteLn('You are winning!')
    else
      WriteLn('Time is up, but you scored well');
  end
  else
    WriteLn('Keep trying!');
end.
```

### When to Nest

**Use nesting when:**
- Conditions are **dependent** (one condition depends on another)
- You need to check **multiple related conditions**
- Logic is **hierarchical** (parent-child relationship)

**Example:**
```pascal
// Nested: Check if user exists, then check permissions
if userExists then
  if hasPermission then
    AllowAccess
  else
    DenyAccess('No permission')
else
  DenyAccess('User not found');
```

---

## Common Patterns

### Pattern 1: Range Checking

```pascal
var value: integer;
begin
  value := 75;
  if (value >= 0) and (value <= 100) then
    WriteLn('Valid value')
  else
    WriteLn('Invalid value');
end.
```

### Pattern 2: Minimum/Maximum

```pascal
var x, y: integer;
var max: integer;
begin
  x := 10;
  y := 20;
  if x > y then
    max := x
  else
    max := y;
  WriteLn('Maximum: ', max);
end.
```

### Pattern 3: Validation

```pascal
var age: integer;
begin
  age := 25;
  if age < 0 then
    WriteLn('Error: Age cannot be negative')
  else if age > 150 then
    WriteLn('Error: Age seems invalid')
  else
    WriteLn('Valid age: ', age);
end.
```

### Pattern 4: State Checking

```pascal
var isGameOver: boolean;
var score: integer;
begin
  isGameOver := false;
  score := 100;
  
  if not isGameOver then
  begin
    if score > 1000 then
      WriteLn('High score!')
    else
      WriteLn('Keep playing!');
  end
  else
    WriteLn('Game over');
end.
```

---

## Best Practices

### 1. Use Clear Conditions

**Bad:**
```pascal
if x then  // What does x mean?
```

**Good:**
```pascal
if isReady then  // Clear what condition checks
```

### 2. Avoid Deep Nesting

**Bad: Too nested**
```pascal
if condition1 then
  if condition2 then
    if condition3 then
      if condition4 then
        DoSomething;  // Too deep!
```

**Good: Flatten with else-if**
```pascal
if condition1 and condition2 and condition3 and condition4 then
  DoSomething;
```

### 3. Use Blocks for Clarity

**Bad:**
```pascal
if condition then
  DoThis;
  DoThat;  // Always executes! (not part of if)
```

**Good:**
```pascal
if condition then
begin
  DoThis;
  DoThat;  // Only executes if condition is true
end;
```

### 4. Handle All Cases

**Bad:**
```pascal
if score > 50 then
  WriteLn('Pass');
// What if score <= 50? Nothing happens!
```

**Good:**
```pascal
if score > 50 then
  WriteLn('Pass')
else
  WriteLn('Fail');
```

---

## Platform Considerations

### Performance

**Conditional statements:**
- **Fast** — CPU can branch efficiently
- **Predictable** — Same behavior on all platforms
- **Optimizable** — Compiler can optimize branches

### Memory

**Conditional code:**
- **No extra memory** — Conditions don't use extra variables (usually)
- **Code size** — More conditions = more code
- **On ZealZ80** — Keep conditions simple to save code space

---

## Common Mistakes

### Mistake 1: Missing Begin/End

**Wrong:**
```pascal
if condition then
  DoThis;
  DoThat;  // Always executes!
```

**Correct:**
```pascal
if condition then
begin
  DoThis;
  DoThat;  // Only if condition is true
end;
```

### Mistake 2: Using = Instead of :=

**Wrong:**
```pascal
if x = 10 then  // Comparison (correct)
  x = 20;       // ERROR: = is comparison, not assignment
```

**Correct:**
```pascal
if x = 10 then  // Comparison
  x := 20;      // Assignment
```

### Mistake 3: Forgetting Else

**Wrong:**
```pascal
if age >= 18 then
  WriteLn('Adult');
WriteLn('Processing...');  // Always executes
```

**Correct:**
```pascal
if age >= 18 then
  WriteLn('Adult')
else
  WriteLn('Minor');
WriteLn('Processing...');  // Always executes
```

---

## Summary

**Key Concepts:**
- **`if` statements** execute code conditionally
- **`else` clauses** handle the opposite case
- **`else if` chains** handle multiple conditions
- **Nested conditions** check dependent conditions
- **Blocks** (`begin...end`) group multiple statements

**Syntax:**
```pascal
if condition then
  statement
else if condition2 then
  statement2
else
  statement3;
```

**Best Practices:**
- Use clear condition names
- Avoid deep nesting
- Use blocks for clarity
- Handle all cases
- Use else-if for multiple conditions

**Next:** Learn about boolean operators for complex conditions.

---

## Exercises

### GCSE Level Exercises

**Exercise 1: Simple If Statement**
Write a program that:
1. Asks for the user's age
2. If age >= 18, displays "You are an adult"
3. Otherwise, displays "You are a minor"

**Exercise 2: If-Else Practice**
Write a program that:
1. Asks for a number
2. If the number is positive, displays "Positive"
3. If the number is negative, displays "Negative"
4. If the number is zero, displays "Zero"

**Exercise 3: Nested Conditions**
Write a program that:
1. Asks for a score (0-100)
2. If score >= 90, displays "Grade: A"
3. Else if score >= 80, displays "Grade: B"
4. Else if score >= 70, displays "Grade: C"
5. Else displays "Grade: F"

### A-Level Exercises

**Exercise 1: Complex Conditionals**
Write a program that determines ticket price:
1. Age < 12: Child ticket (£5)
2. Age 12-17: Student ticket (£8)
3. Age 18-64: Adult ticket (£12)
4. Age >= 65: Senior ticket (£6)
5. Use nested if-else statements

**Exercise 2: Boolean Logic**
Write a program that:
1. Checks if a year is a leap year
2. Conditions: divisible by 4, but not by 100, unless also by 400
3. Display result with explanation

**Exercise 3: Menu System**
Create a simple menu system:
1. Display menu options (1-4)
2. Use if-else-if chain to handle each option
3. Each option performs a different action
4. Handle invalid input

### University Level Exercises

**Exercise 1: Cyclomatic Complexity Analysis**
Analyze the cyclomatic complexity of:
1. A function with nested conditionals
2. A function with multiple if-else chains
3. Refactor to reduce complexity
4. Measure and compare complexity metrics

**Exercise 2: Branch Prediction Analysis**
Implement two versions of a function:
1. Version with predictable branches (sorted data)
2. Version with unpredictable branches (random data)
3. Measure performance difference
4. Analyze branch prediction impact

**Exercise 3: Conditional Optimization**
Optimize a function with many conditionals:
1. Use lookup tables where appropriate
2. Reorder conditions by frequency
3. Use short-circuit evaluation effectively
4. Measure performance improvements

---

**Next Section:** [Boolean Operators](./02_BooleanOperators.md)  
**Language Specification:** See [02_Grammar.md](../../languageSpecification/02_Grammar.md)  
**Last Updated:** 2025-01-XX

