# Simple Recursive Examples

**Part of:** [Chapter 07: Recursion](./README.md)

---

## Introduction

This section teaches recursion through simple, practical examples. You'll see how recursion works with familiar problems and learn to identify the base case and recursive case in each.

---

## Example 1: Factorial

### Problem

Calculate n! (n factorial) where:
- 0! = 1
- n! = n × (n-1)!

### Recursive Solution

```pascal
function Factorial(n: integer): integer;
begin
  // Base case
  if n <= 1 then
    Factorial := 1
  // Recursive case
  else
    Factorial := n * Factorial(n - 1);
end;
```

### How It Works

**Factorial(4):**
- Factorial(4) = 4 × Factorial(3)
- Factorial(3) = 3 × Factorial(2)
- Factorial(2) = 2 × Factorial(1)
- Factorial(1) = 1 (base case)
- Returns: 1 → 2 → 6 → 24

### Iterative Version (Comparison)

```pascal
function FactorialIterative(n: integer): integer;
var
  i, result: integer;
begin
  result := 1;
  for i := 2 to n do
    result := result * i;
  FactorialIterative := result;
end;
```

---

## Example 2: Fibonacci

### Problem

Calculate Fibonacci number F(n) where:
- F(0) = 0
- F(1) = 1
- F(n) = F(n-1) + F(n-2)

### Recursive Solution

```pascal
function Fibonacci(n: integer): integer;
begin
  // Base cases
  if n = 0 then
    Fibonacci := 0
  else if n = 1 then
    Fibonacci := 1
  // Recursive case
  else
    Fibonacci := Fibonacci(n - 1) + Fibonacci(n - 2);
end;
```

### Problem: Inefficient

**This version recalculates values many times!**

**Fibonacci(5) calls:**
- Fibonacci(4) and Fibonacci(3)
- Fibonacci(4) calls Fibonacci(3) and Fibonacci(2)
- Fibonacci(3) is calculated twice!

### Improved: Memoization

```pascal
var
  FibMemo: array[0..99] of integer;
  FibMemoized: array[0..99] of boolean;

function FibonacciMemo(n: integer): integer;
begin
  if n <= 1 then
  begin
    FibonacciMemo := n;
    Exit;
  end;
  
  if FibMemoized[n] then
    FibonacciMemo := FibMemo[n]
  else
  begin
    FibMemo[n] := FibonacciMemo(n - 1) + FibonacciMemo(n - 2);
    FibMemoized[n] := true;
    FibonacciMemo := FibMemo[n];
  end;
end;
```

---

## Example 3: Sum of Array

### Problem

Calculate the sum of all numbers in an array.

### Recursive Solution

```pascal
function SumArray(arr: array of integer; index: integer): integer;
begin
  // Base case: reached end of array
  if index >= Length(arr) then
    SumArray := 0
  // Recursive case: current element + sum of rest
  else
    SumArray := arr[index] + SumArray(arr, index + 1);
end;

// Wrapper function
function Sum(arr: array of integer): integer;
begin
  Sum := SumArray(arr, 0);
end;
```

### How It Works

**Sum([10, 20, 30]):**
- SumArray([10,20,30], 0) = 10 + SumArray([10,20,30], 1)
- SumArray([10,20,30], 1) = 20 + SumArray([10,20,30], 2)
- SumArray([10,20,30], 2) = 30 + SumArray([10,20,30], 3)
- SumArray([10,20,30], 3) = 0 (base case)
- Returns: 0 → 30 → 50 → 60

---

## Example 4: Count Items

### Problem

Count how many items are in an array (or count items matching a condition).

### Recursive Solution

```pascal
function CountItems(arr: array of integer; index: integer): integer;
begin
  // Base case: reached end
  if index >= Length(arr) then
    CountItems := 0
  // Recursive case: 1 (current item) + count of rest
  else
    CountItems := 1 + CountItems(arr, index + 1);
end;

// Count items matching condition
function CountIf(arr: array of integer; index: integer; 
                 condition: function(value: integer): boolean): integer;
begin
  if index >= Length(arr) then
    CountIf := 0
  else if condition(arr[index]) then
    CountIf := 1 + CountIf(arr, index + 1, condition)
  else
    CountIf := CountIf(arr, index + 1, condition);
end;
```

---

## Example 5: Find Maximum

### Problem

Find the maximum value in an array.

### Recursive Solution

```pascal
function FindMax(arr: array of integer; index: integer): integer;
var
  restMax: integer;
begin
  // Base case: last element
  if index = Length(arr) - 1 then
    FindMax := arr[index]
  // Recursive case: max of current and rest
  else
  begin
    restMax := FindMax(arr, index + 1);
    if arr[index] > restMax then
      FindMax := arr[index]
    else
      FindMax := restMax;
  end;
end;
```

---

## Pattern Recognition

### Common Recursive Patterns

**1. Process each element:**
- Base case: empty/end of collection
- Recursive case: process current + recurse on rest

**2. Divide and conquer:**
- Base case: small enough to solve directly
- Recursive case: split in half, solve each, combine

**3. Build up result:**
- Base case: return initial value
- Recursive case: combine current with recursive result

---

## Exercises

### Exercise 1: Implement Recursively

Write recursive functions for:
1. Product of array (multiply all numbers)
2. Find minimum in array
3. Check if array contains value
4. Reverse array

### Exercise 2: Compare with Iteration

For each recursive function above:
1. Write iterative version
2. Compare complexity
3. Decide which is clearer

### Exercise 3: Identify Base Cases

For each problem, identify:
- Base case(s)
- Recursive case
- How progress is made

---

**Previous Section:** [Understanding Recursion](./01_UnderstandingRecursion.md)  
**Next Section:** [Recursion vs Iteration](./03_RecursionVsIteration.md)  
**Last Updated:** 2025-01-XX

