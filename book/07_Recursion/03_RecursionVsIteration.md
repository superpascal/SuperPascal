# Recursion vs Iteration

**Part of:** [Chapter 07: Recursion](./README.md)

---

## Introduction

Both recursion and iteration can solve the same problems. This section teaches you when to use each, how to convert between them, and the trade-offs involved.

---

## When to Use Recursion

### Natural Recursive Structure

**Use recursion when:**
- Problem has natural recursive structure (trees, graphs)
- Recursive solution is clearer and simpler
- Divide and conquer algorithms
- Backtracking problems

### Examples

**Tree traversal:**
```pascal
procedure TraverseTree(node: PTreeNode);
begin
  if node = nil then
    Exit;
  
  Process(node);
  TraverseTree(node^.Left);
  TraverseTree(node^.Right);
end;
```

**Recursive structure makes this natural!**

---

## When to Use Iteration

### Simple Loops

**Use iteration when:**
- Simple loops are clearer
- Performance matters (iteration is usually faster)
- Stack depth is a concern
- Problem is naturally iterative

### Examples

**Sum of array (iterative):**
```pascal
function SumIterative(arr: array of integer): integer;
var
  i, sum: integer;
begin
  sum := 0;
  for i := 0 to Length(arr) - 1 do
    sum := sum + arr[i];
  SumIterative := sum;
end;
```

**Simple and clear!**

---

## Converting Between Recursion and Iteration

### Recursive to Iterative

**General approach:**
1. Identify what the recursion does
2. Use a loop to do the same thing
3. Use a stack if needed (for complex recursion)

**Example: Factorial**

**Recursive:**
```pascal
function Factorial(n: integer): integer;
begin
  if n <= 1 then
    Factorial := 1
  else
    Factorial := n * Factorial(n - 1);
end;
```

**Iterative:**
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

### Iterative to Recursive

**General approach:**
1. Identify the loop variable
2. Make it a parameter
3. Base case: loop termination condition
4. Recursive case: loop body + recursive call

**Example: Sum Array**

**Iterative:**
```pascal
function SumIterative(arr: array of integer): integer;
var
  i, sum: integer;
begin
  sum := 0;
  for i := 0 to Length(arr) - 1 do
    sum := sum + arr[i];
  SumIterative := sum;
end;
```

**Recursive:**
```pascal
function SumRecursive(arr: array of integer; index: integer): integer;
begin
  if index >= Length(arr) then
    SumRecursive := 0
  else
    SumRecursive := arr[index] + SumRecursive(arr, index + 1);
end;
```

---

## Trade-offs

### Recursion Advantages

**✅ Pros:**
- Often clearer for recursive problems
- Natural for trees/graphs
- Elegant for divide and conquer
- Less code (sometimes)

### Recursion Disadvantages

**❌ Cons:**
- Stack overhead (each call uses stack space)
- Can cause stack overflow (deep recursion)
- Usually slower (function call overhead)
- Harder to debug (deep call stacks)

### Iteration Advantages

**✅ Pros:**
- Usually faster
- No stack overflow risk
- Easier to debug
- More memory efficient

### Iteration Disadvantages

**❌ Cons:**
- Can be more complex for recursive problems
- More code (sometimes)
- Less natural for trees/graphs

---

## Decision Guide

### Use Recursion When:
- ✅ Problem has recursive structure
- ✅ Recursive solution is clearer
- ✅ Stack depth is reasonable
- ✅ Performance is not critical

### Use Iteration When:
- ✅ Simple loop is clearer
- ✅ Performance matters
- ✅ Deep recursion would overflow stack
- ✅ Problem is naturally iterative

---

## Best Practices

### 1. Start with Clarity

**Choose the approach that's clearer:**
- If recursive structure is natural → use recursion
- If loop is natural → use iteration

### 2. Consider Performance

**If performance matters:**
- Prefer iteration (usually faster)
- Or optimize recursion (memoization, tail recursion)

### 3. Watch Stack Depth

**If recursion might be deep:**
- Use iteration instead
- Or use iterative approach with explicit stack

### 4. Know Both

**Be able to:**
- Write recursive solutions
- Write iterative solutions
- Convert between them
- Choose the right one

---

## Exercises

### Exercise 1: Convert to Iteration

Convert these recursive functions to iterative:
1. Factorial
2. Sum of array
3. Count items

### Exercise 2: Convert to Recursion

Convert these iterative functions to recursive:
1. Find maximum
2. Check if sorted
3. Reverse array

### Exercise 3: Choose the Right Approach

For each problem, decide: recursion or iteration?
1. Tree traversal
2. Sum of array
3. Factorial
4. Binary search
5. Quicksort

---

**Previous Section:** [Simple Recursive Examples](./02_SimpleRecursiveExamples.md)  
**Next Section:** [Advanced Recursive Patterns](./04_AdvancedRecursivePatterns.md)  
**Last Updated:** 2025-01-XX

