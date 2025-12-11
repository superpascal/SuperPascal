# Expressions and the ALU

**Part of:** [Chapter 03: Variables, Types, and Expressions](./README.md)

---

> **For GCSE students:**  
> Expressions are like math problems. The ALU (the calculator inside the computer) solves them. When you write `5 + 3`, the ALU adds them and gives you 8.
>
> **For A-Level students:**  
> Expressions combine values and operators to produce results. The ALU evaluates expressions by performing arithmetic and logic operations. Understanding expression evaluation helps you write efficient code.
>
> **For University students:**  
> Expressions are evaluated by the ALU using arithmetic and logic circuits. The order of operations, operator precedence, and type coercion all affect evaluation. *Note: This chapter provides a beginner-friendly introduction. Technical details about ALU circuits, binary arithmetic, and hardware implementation are covered in Part IV: Computers in Depth.*

---

## Introduction

In the previous chapters, you learned about variables and types. Now let's learn about **expressions** — combinations of values and operators that produce results.

Remember from [Chapter 1: What Is a Computer?](../01_Introduction/01_WhatIsAComputer.md)? The **ALU (Arithmetic & Logic Unit)** is "the calculator inside the computer." This chapter shows you how the ALU evaluates expressions.

**What you'll learn:**
- What expressions are
- How the ALU evaluates expressions
- Order of operations
- Expression examples
- How expressions connect to variables

---

## What is an Expression?

An **expression** is a combination of:
- **Values** — Numbers, variables, constants
- **Operators** — +, -, *, /, and more
- **Functions** — Built-in or custom functions

**Expressions produce a result:**
```pascal
5 + 3           // Expression that produces 8
x * 2           // Expression that produces x times 2
10 > 5          // Expression that produces true
```

**Expressions can be used:**
- In assignments: `x := 5 + 3;`
- In conditions: `if x > 10 then ...`
- In function calls: `WriteLn(x + 1);`

---

## How the ALU Evaluates Expressions

Remember from Chapter 1? The **ALU** is the calculator inside the CPU. When you write an expression, the ALU evaluates it.

### Simple Expression Evaluation

**Example:**
```pascal
result := 5 + 3;
```

**What happens:**
1. CPU sees the expression `5 + 3`
2. CPU sends 5 and 3 to the ALU
3. ALU adds them: 5 + 3 = 8
4. ALU sends result (8) back to CPU
5. CPU stores 8 in `result`'s memory box

**Visual:**
```
Expression: 5 + 3
    ↓
ALU receives: 5 and 3
    ↓
ALU calculates: 8
    ↓
Result: 8
```

### Expressions with Variables

**Example:**
```pascal
var x: integer;
x := 10;
result := x + 5;
```

**What happens:**
1. CPU reads value from `x`'s memory box (10)
2. CPU sends 10 and 5 to the ALU
3. ALU adds them: 10 + 5 = 15
4. ALU sends result (15) back to CPU
5. CPU stores 15 in `result`'s memory box

**Visual:**
```
x's box: [10]
    ↓
Expression: x + 5
    ↓
ALU receives: 10 and 5
    ↓
ALU calculates: 15
    ↓
result's box: [15]
```

### Multiple Operations

**Example:**
```pascal
result := 5 + 3 * 2;
```

The ALU evaluates this **step by step**, following the order of operations (we'll learn about this next).

---

## Order of Operations

Just like in math, expressions follow an **order of operations**. The ALU evaluates expressions in a specific order.

### Basic Order

1. **Parentheses** — `()` are evaluated first
2. **Multiplication/Division** — `*` and `/` next
3. **Addition/Subtraction** — `+` and `-` last

**Example:**
```pascal
result := 5 + 3 * 2;
```

**Step by step:**
1. ALU sees `3 * 2` first (multiplication)
2. ALU calculates: 3 * 2 = 6
3. ALU then sees `5 + 6` (addition)
4. ALU calculates: 5 + 6 = 11
5. Result: 11

**With parentheses:**
```pascal
result := (5 + 3) * 2;
```

**Step by step:**
1. ALU sees `(5 + 3)` first (parentheses)
2. ALU calculates: 5 + 3 = 8
3. ALU then sees `8 * 2` (multiplication)
4. ALU calculates: 8 * 2 = 16
5. Result: 16

### Why Order Matters

**Different order = different result:**
```pascal
5 + 3 * 2    // = 11 (multiplication first)
(5 + 3) * 2  // = 16 (addition first)
```

Always use parentheses when the order isn't clear!

> **Note:** This is a simplified explanation. The technical details of operator precedence, associativity, and how the compiler generates code for expression evaluation are covered in **Part IV: Computers in Depth** for advanced students.

---

## Expression Examples

### Arithmetic Expressions

**Addition:**
```pascal
sum := 10 + 20;        // sum = 30
```

**Subtraction:**
```pascal
difference := 50 - 30; // difference = 20
```

**Multiplication:**
```pascal
product := 5 * 6;      // product = 30
```

**Division:**
```pascal
quotient := 20 / 4;    // quotient = 5
```

### Expressions with Variables

```pascal
var x, y: integer;
x := 10;
y := 5;
result := x + y;       // result = 15
result := x * y;       // result = 50
result := x - y;       // result = 5
```

### Complex Expressions

```pascal
var a, b, c: integer;
a := 10;
b := 5;
c := 2;
result := a + b * c;        // result = 20 (b*c first)
result := (a + b) * c;      // result = 30 (a+b first)
result := a * b + c;        // result = 52 (a*b first)
```

### Comparison Expressions

The ALU also does comparisons:
```pascal
var x: integer;
x := 10;
if x > 5 then ...      // ALU compares: 10 > 5? → true
if x < 5 then ...      // ALU compares: 10 < 5? → false
```

---

## How Expressions Connect to Variables

Expressions are often used with variables:

**Pattern 1: Calculate and store**
```pascal
var x, y, result: integer;
x := 10;
y := 5;
result := x + y;  // Calculate expression, store in variable
```

**Pattern 2: Update a variable**
```pascal
var x: integer;
x := 10;
x := x + 5;  // Read x (10), add 5, store back in x (15)
```

**Pattern 3: Use in conditions**
```pascal
var x: integer;
x := 10;
if x > 5 then ...  // Expression x > 5 produces true/false
```

---

## Summary

**Key concepts:**
- **Expressions** combine values and operators
- **ALU** evaluates expressions (the calculator inside)
- **Order of operations** matters (parentheses, multiplication/division, addition/subtraction)
- **Expressions** can use variables and produce results

**What you learned:**
- How the ALU evaluates simple expressions
- How expressions work with variables
- Why order of operations matters
- How to write and use expressions

**What's next:**
- We'll learn more about operators
- We'll explore type conversions
- We'll write programs that use expressions
- Later, we'll learn how the ALU really works (Part IV)

**Remember:**
- This chapter gave you a **beginner-friendly** introduction
- Technical details about ALU circuits come later in **Part IV**
- Build intuition first, rigor later

---

**Previous Section:** [Variables and Assignment](./02_VariablesAndAssignment.md)  
**Next Section:** [Type Casting](./04_TypeCasting.md)  
**Language Specification:** See [Type System](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

