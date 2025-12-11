# Understanding Recursion

**Part of:** [Chapter 07: Recursion](./README.md)

---

> **For GCSE students:**  
> Recursion is when a function calls itself. It's like a loop, but the function keeps calling itself with smaller problems until it reaches a simple case it can solve directly.
>
> **For A-Level students:**  
> Recursion is a powerful problem-solving technique where a function solves a problem by solving smaller instances of the same problem. It requires a base case (stopping condition) and a recursive case (calling itself with modified parameters).
>
> **For University students:**  
> Recursion is a fundamental programming paradigm based on mathematical induction. It involves defining a problem in terms of itself, with base cases and recursive cases. Understanding recursion requires understanding the call stack, tail recursion, and when recursion is appropriate vs. iteration.

---

## Introduction

Recursion is a programming technique where a function calls itself. It's a powerful way to solve problems that have a natural recursive structure, like trees, graphs, or problems that can be broken into smaller versions of themselves.

**Key concepts:**
- **Recursive function** - A function that calls itself
- **Base case** - The condition that stops the recursion
- **Recursive case** - The part that calls itself with a smaller problem
- **Call stack** - The stack of function calls

---

## What is Recursion?

### Simple Definition

**Recursion** is when a function calls itself to solve a smaller version of the same problem.

**Think of it like:**
- Russian nesting dolls (each doll contains a smaller doll)
- Factorial: n! = n × (n-1)! (factorial defined in terms of itself)
- A function that calls itself

### Example: Factorial

**Factorial definition:**
- 0! = 1 (base case)
- n! = n × (n-1)! (recursive case)

**In SuperPascal:**
```pascal
function Factorial(n: integer): integer;
begin
  if n <= 1 then
    Factorial := 1  // Base case
  else
    Factorial := n * Factorial(n - 1);  // Recursive case
end;
```

**How it works:**
- Factorial(5) calls Factorial(4)
- Factorial(4) calls Factorial(3)
- Factorial(3) calls Factorial(2)
- Factorial(2) calls Factorial(1)
- Factorial(1) returns 1 (base case)
- Each function returns, multiplying as it goes back

---

## How Recursion Works

### The Call Stack

**When a function calls itself:**
1. Current function pauses
2. New function call starts
3. New call is added to the call stack
4. When base case is reached, functions return in reverse order

**Example: Factorial(3)**

```
Call stack:
Factorial(3)  ← Top (currently executing)
  calls Factorial(2)
    calls Factorial(1)
      returns 1
    returns 2 × 1 = 2
  returns 3 × 2 = 6
```

### Visualizing Recursion

**Think of recursion like:**
- Going deeper into a maze
- Each recursive call goes one level deeper
- Base case is finding the exit
- Returning is going back out

---

## Base Cases and Recursive Cases

### Base Case

**Base case** is the condition that stops recursion.

**Characteristics:**
- Must be reachable
- Must not call the function again
- Must return a value directly

**Example:**
```pascal
if n <= 1 then
  Factorial := 1;  // Base case - stops recursion
```

### Recursive Case

**Recursive case** calls the function with a smaller problem.

**Characteristics:**
- Must make progress toward base case
- Must call function with smaller input
- Must eventually reach base case

**Example:**
```pascal
else
  Factorial := n * Factorial(n - 1);  // Recursive case - n-1 is smaller
```

### Why Both Are Needed

**Without base case:**
- Function calls itself forever
- Stack overflow error
- Program crashes

**Without recursive case:**
- Function never calls itself
- Not actually recursive
- Just a regular function

---

## Common Mistakes

### Mistake 1: Missing Base Case

**❌ Bad:**
```pascal
function Factorial(n: integer): integer;
begin
  Factorial := n * Factorial(n - 1);  // No base case!
end;
```

**Problem:** Infinite recursion, stack overflow

**✅ Good:**
```pascal
function Factorial(n: integer): integer;
begin
  if n <= 1 then
    Factorial := 1  // Base case
  else
    Factorial := n * Factorial(n - 1);
end;
```

### Mistake 2: Not Making Progress

**❌ Bad:**
```pascal
function BadRecursion(n: integer): integer;
begin
  if n <= 0 then
    BadRecursion := 0
  else
    BadRecursion := BadRecursion(n);  // Same value - no progress!
end;
```

**Problem:** Infinite recursion (never reaches base case)

**✅ Good:**
```pascal
function GoodRecursion(n: integer): integer;
begin
  if n <= 0 then
    GoodRecursion := 0
  else
    GoodRecursion := GoodRecursion(n - 1);  // n-1 is smaller - makes progress
end;
```

### Mistake 3: Wrong Base Case

**❌ Bad:**
```pascal
function Factorial(n: integer): integer;
begin
  if n = 0 then  // Only handles n=0, not n=1
    Factorial := 1
  else
    Factorial := n * Factorial(n - 1);
end;
```

**Problem:** Factorial(1) calls Factorial(0), which is fine, but less clear

**✅ Good:**
```pascal
function Factorial(n: integer): integer;
begin
  if n <= 1 then  // Handles both n=0 and n=1
    Factorial := 1
  else
    Factorial := n * Factorial(n - 1);
end;
```

---

## Best Practices

### 1. Always Have a Base Case

**Before writing recursive function:**
- Identify the base case
- Write it first
- Make sure it's reachable

### 2. Make Progress

**In recursive case:**
- Ensure input gets smaller
- Ensure you're moving toward base case
- Check that recursion will eventually stop

### 3. Test with Small Values

**Test your recursive function:**
- Start with base case (n=0, n=1)
- Test with small values (n=2, n=3)
- Verify it works before trying large values

### 4. Understand the Call Stack

**Think about:**
- How deep will recursion go?
- Will stack overflow?
- Is there a better way?

---

## Exercises

### Exercise 1: Identify Base Cases

For each problem, identify the base case:
1. Factorial
2. Sum of array
3. Count items in list
4. Find maximum in array

### Exercise 2: Identify Recursive Cases

For each problem, identify the recursive case:
1. Factorial
2. Sum of array
3. Count items in list
4. Find maximum in array

### Exercise 3: Find the Mistake

What's wrong with these recursive functions?
1. Function that calls itself with same value
2. Function without base case

---

## Level-Specific Exercises

### GCSE Level Exercises

**Exercise 1: Simple Recursive Count**
Write a recursive function that counts from 1 to 10. The function should print each number and call itself with the next number.

**Exercise 2: Recursive Sum**
Write a recursive function that sums numbers from 1 to n. Test with n = 5 (should return 15).

**Exercise 3: Understanding Base Cases**
For the factorial function, explain why the base case is `n = 0` or `n = 1`. What would happen if we used `n = 2` as the base case?

### A-Level Exercises

**Exercise 1: Recursive Array Sum**
Write a recursive function that sums all elements in an array. The function should:
- Take an array and current index as parameters
- Base case: index equals array length
- Recursive case: add current element and recurse with next index

**Exercise 2: Tail Recursion Analysis**
Implement factorial both recursively and iteratively. Compare:
- Code clarity
- Stack usage
- Performance implications

**Exercise 3: Recursive Binary Search**
Implement binary search recursively. Analyze:
- Base cases (found, not found, invalid range)
- Recursive cases (search left/right half)
- Stack depth for array of size n

### University Level Exercises

**Exercise 1: Recursive Tree Traversal**
Implement a binary tree structure and write recursive functions for:
- In-order traversal
- Pre-order traversal
- Post-order traversal
- Calculate tree height
- Count nodes

Analyze time and space complexity of each.

**Exercise 2: Memoization and Dynamic Programming**
Implement recursive Fibonacci with and without memoization:
- Naive recursive (exponential time)
- Memoized recursive (linear time)
- Compare performance for n = 30, 40, 50

**Exercise 3: Recursive Backtracking**
Implement a recursive backtracking solution for the N-Queens problem:
- Place N queens on an N×N board
- No two queens attack each other
- Use recursion to explore all possibilities
- Backtrack when placement is invalid

Analyze the search space and optimization opportunities.
3. Function with unreachable base case

---

**Next Section:** [Simple Recursive Examples](./02_SimpleRecursiveExamples.md)  
**Last Updated:** 2025-01-XX

