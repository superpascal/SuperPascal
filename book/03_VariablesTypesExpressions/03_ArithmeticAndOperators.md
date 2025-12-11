# Arithmetic and Operators

**Part of:** [Chapter 02: Variables, Types, and Expressions](./README.md)

---

## Introduction

**Operators** perform operations on values. They're the tools you use to manipulate data: adding numbers, comparing values, combining conditions.

This chapter covers:
- Arithmetic operators (+, -, *, /, div, mod)
- Comparison operators (=, <>, <, >, <=, >=)
- Logical operators (and, or, not, xor)
- Operator precedence
- Expressions and evaluation

---

## Arithmetic Operators

### Addition and Subtraction

**`+` (Addition):**
```pascal
var sum: integer;
begin
  sum := 10 + 5;      // 15
  sum := sum + 3;     // 18
  WriteLn('Sum: ', sum);
end.
```

**`-` (Subtraction):**
```pascal
var diff: integer;
begin
  diff := 10 - 5;     // 5
  diff := diff - 2;   // 3
  WriteLn('Difference: ', diff);
end.
```

**Unary minus (negation):**
```pascal
var x: integer;
begin
  x := -10;           // x is -10
  x := -x;            // x is 10 (negate)
end.
```

### Multiplication and Division

**`*` (Multiplication):**
```pascal
var product: integer;
begin
  product := 5 * 3;   // 15
  product := 2 * product; // 30
  WriteLn('Product: ', product);
end.
```

**`div` (Integer Division):**
```pascal
var quotient: integer;
begin
  quotient := 10 div 3;  // 3 (integer division, truncates)
  quotient := 7 div 2;   // 3
  WriteLn('Quotient: ', quotient);
end.
```

**`/` (Real Division) - if supported:**
```pascal
// Note: / may require floating-point types
// For integer division, use 'div'
```

**`mod` (Modulo/Remainder):**
```pascal
var remainder: integer;
begin
  remainder := 10 mod 3;  // 1 (10 divided by 3 = 3 remainder 1)
  remainder := 7 mod 2;   // 1
  remainder := 8 mod 2;   // 0 (even number)
  WriteLn('Remainder: ', remainder);
end.
```

### Arithmetic Examples

**Calculating area:**
```pascal
var width, height, area: integer;
begin
  width := 10;
  height := 5;
  area := width * height;  // 50
  WriteLn('Area: ', area);
end.
```

**Calculating average:**
```pascal
var sum, count, average: integer;
begin
  sum := 100;
  count := 4;
  average := sum div count;  // 25
  WriteLn('Average: ', average);
end.
```

**Checking even/odd:**
```pascal
var number: integer;
var isEven: boolean;
begin
  number := 8;
  isEven := (number mod 2) = 0;  // true if remainder is 0
  if isEven then
    WriteLn('Even')
  else
    WriteLn('Odd');
end.
```

---

## Comparison Operators

### Equality and Inequality

**`=` (Equal):**
```pascal
var result: boolean;
begin
  result := 5 = 5;    // true
  result := 5 = 10;   // false
  WriteLn(result);
end.
```

**`<>` (Not Equal):**
```pascal
var result: boolean;
begin
  result := 5 <> 10;  // true
  result := 5 <> 5;   // false
  WriteLn(result);
end.
```

### Ordering Comparisons

**`<` (Less Than):**
```pascal
var result: boolean;
begin
  result := 3 < 5;    // true
  result := 10 < 5;   // false
end.
```

**`>` (Greater Than):**
```pascal
var result: boolean;
begin
  result := 10 > 5;   // true
  result := 3 > 5;    // false
end.
```

**`<=` (Less Than or Equal):**
```pascal
var result: boolean;
begin
  result := 5 <= 5;   // true
  result := 3 <= 5;   // true
  result := 10 <= 5;  // false
end.
```

**`>=` (Greater Than or Equal):**
```pascal
var result: boolean;
begin
  result := 5 >= 5;   // true
  result := 10 >= 5;   // true
  result := 3 >= 5;    // false
end.
```

### Comparison Examples

**Range checking:**
```pascal
var score: integer;
var isValid: boolean;
begin
  score := 85;
  isValid := (score >= 0) and (score <= 100);
  if isValid then
    WriteLn('Valid score')
  else
    WriteLn('Invalid score');
end.
```

**String comparison:**
```pascal
var name: string;
begin
  name := 'Alice';
  if name = 'Alice' then
    WriteLn('Hello, Alice!')
  else if name < 'M' then
    WriteLn('Name comes before M')
  else
    WriteLn('Name comes after M');
end.
```

---

## Logical Operators

### `and` (Logical AND)

**Returns `true` only if both operands are `true`:**
```pascal
var result: boolean;
begin
  result := true and true;   // true
  result := true and false;  // false
  result := false and true;  // false
  result := false and false; // false
end.
```

**Example:**
```pascal
var age: integer;
var hasLicense: boolean;
var canDrive: boolean;
begin
  age := 18;
  hasLicense := true;
  canDrive := (age >= 16) and hasLicense;  // true
end.
```

### `or` (Logical OR)

**Returns `true` if either operand is `true`:**
```pascal
var result: boolean;
begin
  result := true or true;    // true
  result := true or false;   // true
  result := false or true;   // true
  result := false or false;  // false
end.
```

**Example:**
```pascal
var isWeekend: boolean;
var isHoliday: boolean;
var canSleepIn: boolean;
begin
  isWeekend := true;
  isHoliday := false;
  canSleepIn := isWeekend or isHoliday;  // true
end.
```

### `not` (Logical NOT)

**Returns the opposite:**
```pascal
var result: boolean;
begin
  result := not true;   // false
  result := not false;  // true
end.
```

**Example:**
```pascal
var isFinished: boolean;
var canContinue: boolean;
begin
  isFinished := false;
  canContinue := not isFinished;  // true
end.
```

### `xor` (Exclusive OR)

**Returns `true` if operands differ:**
```pascal
var result: boolean;
begin
  result := true xor true;    // false (both same)
  result := true xor false;   // true (different)
  result := false xor true;   // true (different)
  result := false xor false;  // false (both same)
end.
```

**Example:**
```pascal
var option1, option2: boolean;
var exactlyOne: boolean;
begin
  option1 := true;
  option2 := false;
  exactlyOne := option1 xor option2;  // true (exactly one is true)
end.
```

### Complex Logical Expressions

**Combining operators:**
```pascal
var age: integer;
var hasLicense: boolean;
var hasInsurance: boolean;
var canDrive: boolean;
begin
  age := 18;
  hasLicense := true;
  hasInsurance := true;
  
  canDrive := (age >= 16) and hasLicense and hasInsurance;
  // true only if all conditions are true
end.
```

**Using parentheses:**
```pascal
var x, y: integer;
var result: boolean;
begin
  x := 10;
  y := 5;
  result := (x > 5) and (y < 10) or (x = 10);
  // Use parentheses to clarify intent
  result := ((x > 5) and (y < 10)) or (x = 10);
end.
```

---

## Operator Precedence

### What is Precedence?

**Precedence** determines which operations are performed first when multiple operators appear in an expression.

**Order (highest to lowest):**
1. **Parentheses** `()` — Highest precedence
2. **Unary operators** `-`, `not`
3. **Multiplicative** `*`, `div`, `mod`
4. **Additive** `+`, `-`
5. **Comparison** `=`, `<>`, `<`, `>`, `<=`, `>=`
6. **Logical** `and`, `or`, `xor` — Lowest precedence

### Precedence Examples

**Without parentheses:**
```pascal
var result: integer;
begin
  result := 2 + 3 * 4;  // 14 (not 20!)
  // Evaluated as: 2 + (3 * 4) = 2 + 12 = 14
end.
```

**With parentheses:**
```pascal
var result: integer;
begin
  result := (2 + 3) * 4;  // 20
  // Evaluated as: (2 + 3) * 4 = 5 * 4 = 20
end.
```

**Logical precedence:**
```pascal
var result: boolean;
begin
  result := true or false and false;  // true (not false!)
  // Evaluated as: true or (false and false) = true or false = true
  
  result := (true or false) and false;  // false
  // Evaluated as: (true or false) and false = true and false = false
end.
```

### Best Practice: Use Parentheses

**Even when not needed, parentheses clarify intent:**
```pascal
// Clear
var result: integer;
result := (2 + 3) * 4;

// Also clear (but parentheses help readability)
var canDrive: boolean;
canDrive := (age >= 16) and (hasLicense);
```

---

## Expressions

### What is an Expression?

An **expression** is a combination of:
- **Values** (literals, variables)
- **Operators** (+, -, *, etc.)
- **Function calls**
- **Parentheses**

**Expressions evaluate to a value:**
```pascal
var result: integer;
begin
  result := 10 + 5;        // Expression: 10 + 5, evaluates to 15
  result := result * 2;    // Expression: result * 2, evaluates to 30
  WriteLn(result);        // 30
end.
```

### Expression Types

**Arithmetic expressions:**
```pascal
var x: integer;
x := 10 + 5 * 2;  // Arithmetic expression
```

**Boolean expressions:**
```pascal
var isValid: boolean;
isValid := (x > 0) and (x < 100);  // Boolean expression
```

**String expressions:**
```pascal
var message: string;
message := 'Hello' + ' ' + 'World';  // String expression
```

### Expression Evaluation

**Evaluation order:**
1. **Parentheses** — Innermost first
2. **Operators** — By precedence
3. **Left to right** — For same precedence

**Example:**
```pascal
var result: integer;
begin
  result := 2 + 3 * 4 - 1;
  // Step 1: 3 * 4 = 12 (multiplication first)
  // Step 2: 2 + 12 = 14 (addition)
  // Step 3: 14 - 1 = 13 (subtraction)
  // Result: 13
end.
```

---

## Fixed-Point Arithmetic

### Q8.8 Operations

**Q8.8 supports standard arithmetic:**
```pascal
var x, y, sum: Q8.8;
begin
  x := 10.5;
  y := 5.25;
  sum := x + y;      // 15.75
  sum := x - y;      // 5.25
  sum := x * 2.0;    // 21.0
  sum := x / 2.0;    // 5.25
end.
```

**Comparisons work:**
```pascal
var x, y: Q8.8;
var isGreater: boolean;
begin
  x := 10.5;
  y := 5.25;
  isGreater := x > y;  // true
end.
```

### Q12.12 Operations

**Same operations, higher precision:**
```pascal
var x, y: Q12.12;
begin
  x := 100.5;
  y := 50.25;
  // All arithmetic operations work
  x := x + y;
  x := x * 1.5;
end.
```

---

## Common Patterns

### Increment/Decrement

```pascal
var counter: integer;
begin
  counter := 0;
  counter := counter + 1;  // Increment
  counter := counter + 1;  // Increment again
  counter := counter - 1;   // Decrement
  WriteLn('Counter: ', counter); // 1
end.
```

### Accumulation

```pascal
var sum: integer;
var value: integer;
begin
  sum := 0;
  value := 10;
  sum := sum + value;  // Add to sum
  value := 20;
  sum := sum + value;  // Add more
  WriteLn('Sum: ', sum); // 30
end.
```

### Range Checking

```pascal
var score: integer;
var isValid: boolean;
begin
  score := 85;
  isValid := (score >= 0) and (score <= 100);
  if isValid then
    WriteLn('Valid');
end.
```

### Even/Odd Check

```pascal
var number: integer;
var isEven: boolean;
begin
  number := 8;
  isEven := (number mod 2) = 0;
  if isEven then
    WriteLn('Even');
end.
```

---

## Operator Summary Table

| Operator | Type | Description | Example |
|----------|------|-------------|---------|
| `+` | Arithmetic | Addition | `5 + 3 = 8` |
| `-` | Arithmetic | Subtraction | `5 - 3 = 2` |
| `*` | Arithmetic | Multiplication | `5 * 3 = 15` |
| `div` | Arithmetic | Integer division | `10 div 3 = 3` |
| `mod` | Arithmetic | Modulo/remainder | `10 mod 3 = 1` |
| `=` | Comparison | Equal | `5 = 5` → `true` |
| `<>` | Comparison | Not equal | `5 <> 3` → `true` |
| `<` | Comparison | Less than | `3 < 5` → `true` |
| `>` | Comparison | Greater than | `10 > 5` → `true` |
| `<=` | Comparison | Less or equal | `5 <= 5` → `true` |
| `>=` | Comparison | Greater or equal | `10 >= 5` → `true` |
| `and` | Logical | Logical AND | `true and false` → `false` |
| `or` | Logical | Logical OR | `true or false` → `true` |
| `not` | Logical | Logical NOT | `not true` → `false` |
| `xor` | Logical | Exclusive OR | `true xor false` → `true` |

---

## Summary

**Key Concepts:**
- **Arithmetic operators** perform math: `+`, `-`, `*`, `div`, `mod`
- **Comparison operators** compare values: `=`, `<>`, `<`, `>`, `<=`, `>=`
- **Logical operators** combine conditions: `and`, `or`, `not`, `xor`
- **Precedence** determines evaluation order
- **Expressions** combine values and operators

**Best Practices:**
- Use parentheses to clarify intent
- Understand operator precedence
- Use appropriate operators for types
- Test expressions with different values

**Next:** Learn about procedures and functions.

---

**Next Chapter:** [Chapter 03: Procedures and Functions](../04_ProceduresAndFunctions/README.md)  
**Language Specification:** See [02_Grammar.md](../../languageSpecification/02_Grammar.md)  
**Last Updated:** 2025-01-XX

