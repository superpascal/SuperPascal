# Boolean Operators

**Part of:** [Chapter 04: Conditionals and Logic](./README.md)

---

## Introduction

**Boolean operators** let you combine and manipulate true/false values. They're essential for writing complex conditions that check multiple things at once.

This chapter covers:
- Logical AND (`and`)
- Logical OR (`or`)
- Logical NOT (`not`)
- Exclusive OR (`xor`)
- Combining operators
- Operator precedence

---

## Logical AND (`and`)

### What is AND?

**`and` returns `true` only if BOTH operands are `true`:**

| A     | B     | A and B |
|-------|-------|---------|
| true  | true  | true    |
| true  | false | false   |
| false | true  | false   |
| false | false | false   |

### Basic Usage

```pascal
var a, b: boolean;
begin
  a := true;
  b := true;
  if a and b then
    WriteLn('Both are true');
end.
```

### Real-World Example

```pascal
var age: integer;
var hasLicense: boolean;
begin
  age := 18;
  hasLicense := true;
  
  if (age >= 16) and hasLicense then
    WriteLn('You can drive')
  else
    WriteLn('You cannot drive');
end.
```

### Multiple AND Conditions

```pascal
var score: integer;
var timeRemaining: integer;
var isAlive: boolean;
begin
  score := 100;
  timeRemaining := 30;
  isAlive := true;
  
  if (score > 50) and (timeRemaining > 0) and isAlive then
    WriteLn('Game continues');
end.
```

---

## Logical OR (`or`)

### What is OR?

**`or` returns `true` if EITHER operand is `true`:**

| A     | B     | A or B |
|-------|-------|--------|
| true  | true  | true   |
| true  | false | true   |
| false | true  | true   |
| false | false | false  |

### Basic Usage

```pascal
var isWeekend: boolean;
var isHoliday: boolean;
begin
  isWeekend := true;
  isHoliday := false;
  
  if isWeekend or isHoliday then
    WriteLn('You can sleep in');
end.
```

### Real-World Example

```pascal
var temperature: integer;
begin
  temperature := 25;
  
  if (temperature < 0) or (temperature > 40) then
    WriteLn('Extreme weather')
  else
    WriteLn('Normal weather');
end.
```

### Multiple OR Conditions

```pascal
var key: char;
begin
  key := 'q';
  
  if (key = 'q') or (key = 'Q') or (key = #27) then  // q, Q, or Escape
    WriteLn('Quit');
end.
```

---

## Logical NOT (`not`)

### What is NOT?

**`not` returns the opposite:**

| A     | not A |
|-------|-------|
| true  | false |
| false | true  |

### Basic Usage

```pascal
var isFinished: boolean;
begin
  isFinished := false;
  
  if not isFinished then
    WriteLn('Keep working');
end.
```

### Real-World Example

```pascal
var isGameOver: boolean;
begin
  isGameOver := false;
  
  if not isGameOver then
  begin
    WriteLn('Game continues');
    // Update game state
  end;
end.
```

### Negating Comparisons

```pascal
var score: integer;
begin
  score := 50;
  
  if not (score > 100) then
    WriteLn('Score is not greater than 100');
  // Equivalent to: if score <= 100 then
end.
```

---

## Exclusive OR (`xor`)

### What is XOR?

**`xor` returns `true` if operands DIFFER:**

| A     | B     | A xor B |
|-------|-------|---------|
| true  | true  | false   |
| true  | false | true    |
| false | true  | true    |
| false | false | false   |

### Basic Usage

```pascal
var option1, option2: boolean;
begin
  option1 := true;
  option2 := false;
  
  if option1 xor option2 then
    WriteLn('Exactly one option is true');
end.
```

### Real-World Example

```pascal
var hasKey1, hasKey2: boolean;
begin
  hasKey1 := true;
  hasKey2 := false;
  
  // Door opens if exactly one key is present
  if hasKey1 xor hasKey2 then
    WriteLn('Door opens');
end.
```

---

## Combining Operators

### AND and OR Together

```pascal
var age: integer;
var hasLicense: boolean;
var hasInsurance: boolean;
begin
  age := 18;
  hasLicense := true;
  hasInsurance := true;
  
  // Can drive if: (age >= 16) AND (has license OR has insurance)
  if (age >= 16) and (hasLicense or hasInsurance) then
    WriteLn('You can drive');
end.
```

### Using Parentheses

**Parentheses clarify precedence:**
```pascal
var a, b, c: boolean;
begin
  a := true;
  b := false;
  c := true;
  
  // Clear: (a and b) or c
  if (a and b) or c then
    WriteLn('True');
  
  // Different: a and (b or c)
  if a and (b or c) then
    WriteLn('Also true');
end.
```

### Complex Conditions

```pascal
var score: integer;
var timeRemaining: integer;
var isAlive: boolean;
var hasPowerUp: boolean;
begin
  score := 100;
  timeRemaining := 30;
  isAlive := true;
  hasPowerUp := false;
  
  // Complex condition
  if ((score > 50) and isAlive) or (hasPowerUp and (timeRemaining > 0)) then
    WriteLn('Game continues');
end.
```

---

## Operator Precedence

### Precedence Order

**From highest to lowest:**
1. **`not`** — Highest precedence
2. **`and`** — Middle precedence
3. **`or`, `xor`** — Lowest precedence

### Examples

**Without parentheses:**
```pascal
// not has highest precedence
if not a and b then
  // Evaluated as: (not a) and b

// and has higher precedence than or
if a and b or c then
  // Evaluated as: (a and b) or c
```

**With parentheses (recommended):**
```pascal
// Clear intent
if (not a) and b then
  // Explicit: not a, then and with b

if (a and b) or c then
  // Explicit: a and b, then or with c
```

### Best Practice: Use Parentheses

**Even when not needed, parentheses clarify intent:**
```pascal
// Clear
if (age >= 18) and (hasLicense or hasInsurance) then

// Also clear (but parentheses help)
if (score > 100) or ((timeRemaining > 0) and isAlive) then
```

---

## Common Patterns

### Pattern 1: Range Checking

```pascal
var value: integer;
begin
  value := 75;
  if (value >= 0) and (value <= 100) then
    WriteLn('Value is in range');
end.
```

### Pattern 2: Multiple Conditions

```pascal
var x, y: integer;
begin
  x := 10;
  y := 20;
  if (x > 0) and (y > 0) and (x < 100) and (y < 100) then
    WriteLn('Both coordinates are valid');
end.
```

### Pattern 3: Exclusive Choice

```pascal
var option1, option2: boolean;
begin
  option1 := true;
  option2 := false;
  if option1 xor option2 then
    WriteLn('Exactly one option selected');
end.
```

### Pattern 4: Negation

```pascal
var isGameOver: boolean;
begin
  isGameOver := false;
  if not isGameOver then
    WriteLn('Game continues');
end.
```

### Pattern 5: Complex Validation

```pascal
var age: integer;
var hasLicense: boolean;
var hasInsurance: boolean;
var isSober: boolean;
begin
  age := 20;
  hasLicense := true;
  hasInsurance := true;
  isSober := true;
  
  if (age >= 18) and hasLicense and hasInsurance and isSober then
    WriteLn('All conditions met for driving');
end.
```

---

## De Morgan's Laws

### What are De Morgan's Laws?

**De Morgan's Laws** help simplify boolean expressions:

1. **`not (A and B)` = `(not A) or (not B)`**
2. **`not (A or B)` = `(not A) and (not B)`**

### Examples

**Law 1:**
```pascal
// These are equivalent:
if not (a and b) then
  // ...

if (not a) or (not b) then
  // ...
```

**Law 2:**
```pascal
// These are equivalent:
if not (a or b) then
  // ...

if (not a) and (not b) then
  // ...
```

### When to Use

**Use De Morgan's Laws to:**
- **Simplify expressions** — Make code clearer
- **Avoid double negatives** — `not not` is confusing
- **Optimize conditions** — Sometimes one form is faster

**Example:**
```pascal
// Complex
if not ((score > 100) and (timeRemaining > 0)) then

// Simplified (De Morgan)
if (score <= 100) or (timeRemaining <= 0) then
```

---

## Short-Circuit Evaluation

### What is Short-Circuit?

**Short-circuit evaluation** means:
- **`and`** — If left side is `false`, don't evaluate right side
- **`or`** — If left side is `true`, don't evaluate right side

### Benefits

**Performance:**
```pascal
// If x is 0, (x > 0) is false, so (y div x) is never evaluated
if (x > 0) and ((y div x) > 5) then
  // Safe: won't divide by zero if x <= 0
```

**Safety:**
```pascal
// If arr is nil, arr.Length is never accessed
if (arr <> nil) and (arr.Length > 0) then
  // Safe: won't crash if arr is nil
```

**Note:** SuperPascal may or may not use short-circuit evaluation. Check specification or test behavior.

---

## Common Mistakes

### Mistake 1: Confusing AND and OR

**Wrong:**
```pascal
// This is almost always true!
if (score > 0) or (score < 100) then  // Almost any number!
```

**Correct:**
```pascal
// This checks if score is in range
if (score > 0) and (score < 100) then
```

### Mistake 2: Missing Parentheses

**Wrong:**
```pascal
// Unclear precedence
if a and b or c then  // What does this mean?
```

**Correct:**
```pascal
// Clear intent
if (a and b) or c then
```

### Mistake 3: Double Negation

**Wrong:**
```pascal
// Confusing
if not (not isReady) then  // Just use isReady!
```

**Correct:**
```pascal
// Clear
if isReady then
```

---

## Summary

**Key Concepts:**
- **`and`** — Both must be true
- **`or`** — Either can be true
- **`not`** — Negates the value
- **`xor`** — Exactly one must be true
- **Precedence** — `not` > `and` > `or`/`xor`
- **Parentheses** — Use to clarify intent

**Truth Tables:**

| A     | B     | A and B | A or B | A xor B | not A |
|-------|-------|---------|--------|---------|-------|
| true  | true  | true    | true   | false   | false |
| true  | false | false   | true   | true    | false |
| false | true  | false   | true   | true    | true  |
| false | false | false   | false  | false   | true  |

**Best Practices:**
- Use parentheses to clarify intent
- Understand operator precedence
- Use De Morgan's Laws to simplify
- Avoid double negation
- Test complex conditions carefully

**Next:** Learn about case statements for multiway decisions.

---

**Next Section:** [Multiway Decisions](./03_MultiwayDecisions.md)  
**Language Specification:** See [02_Grammar.md](../../languageSpecification/02_Grammar.md)  
**Last Updated:** 2025-01-XX

