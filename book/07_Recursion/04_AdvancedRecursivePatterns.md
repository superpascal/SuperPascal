# Advanced Recursive Patterns

**Part of:** [Chapter 07: Recursion](./README.md)

---

## Introduction

This section covers advanced recursive patterns used in real-world programming: tree traversal, divide and conquer, backtracking, and tail recursion.

**Target Levels:**
- **GCSE:** Optional - Basic understanding
- **A-Level:** Important - Practical applications
- **University:** Essential - Deep understanding required

---

## Tree Traversal

### What is Tree Traversal?

**Tree traversal** means visiting every node in a tree. Recursion is natural for this because trees are recursive structures.

### Types of Traversal

**1. Pre-order:** Process node, then children
**2. In-order:** Process left, node, right
**3. Post-order:** Process children, then node

### Example: Binary Tree Traversal

```pascal
type
  PTreeNode = ^TTreeNode;
  TTreeNode = record
    Value: integer;
    Left, Right: PTreeNode;
  end;

// Pre-order traversal
procedure PreOrder(node: PTreeNode);
begin
  if node = nil then
    Exit;
  
  Process(node^.Value);  // Process node first
  PreOrder(node^.Left);
  PreOrder(node^.Right);
end;

// In-order traversal
procedure InOrder(node: PTreeNode);
begin
  if node = nil then
    Exit;
  
  InOrder(node^.Left);
  Process(node^.Value);  // Process node in middle
  InOrder(node^.Right);
end;

// Post-order traversal
procedure PostOrder(node: PTreeNode);
begin
  if node = nil then
    Exit;
  
  PostOrder(node^.Left);
  PostOrder(node^.Right);
  Process(node^.Value);  // Process node last
end;
```

---

## Divide and Conquer

### What is Divide and Conquer?

**Divide and conquer** means:
1. Divide problem into smaller subproblems
2. Solve subproblems recursively
3. Combine solutions

### Example: Binary Search

```pascal
function BinarySearch(arr: array of integer; 
                     left, right, target: integer): integer;
var
  mid: integer;
begin
  // Base case: not found
  if left > right then
  begin
    BinarySearch := -1;
    Exit;
  end;
  
  mid := (left + right) div 2;
  
  // Base case: found
  if arr[mid] = target then
    BinarySearch := mid
  // Recursive case: search left or right half
  else if arr[mid] > target then
    BinarySearch := BinarySearch(arr, left, mid - 1, target)
  else
    BinarySearch := BinarySearch(arr, mid + 1, right, target);
end;
```

### Example: Quicksort

```pascal
procedure QuickSort(var arr: array of integer; left, right: integer);
var
  pivotIndex: integer;
begin
  if left < right then
  begin
    // Divide: partition array
    pivotIndex := Partition(arr, left, right);
    
    // Conquer: sort each part
    QuickSort(arr, left, pivotIndex - 1);
    QuickSort(arr, pivotIndex + 1, right);
  end;
end;
```

---

## Backtracking

### What is Backtracking?

**Backtracking** means:
1. Try a solution
2. If it doesn't work, undo (backtrack)
3. Try next possibility

### Example: N-Queens Problem

```pascal
function IsSafe(board: array of array of boolean; 
                row, col, n: integer): boolean;
begin
  // Check if queen can be placed at (row, col)
  // (Implementation details omitted)
  IsSafe := true;  // Simplified
end;

function SolveNQueens(var board: array of array of boolean; 
                     row, n: integer): boolean;
var
  col: integer;
begin
  // Base case: all queens placed
  if row >= n then
  begin
    SolveNQueens := true;
    Exit;
  end;
  
  // Try each column
  for col := 0 to n - 1 do
  begin
    if IsSafe(board, row, col, n) then
    begin
      // Try placing queen
      board[row, col] := true;
      
      // Recursively solve rest
      if SolveNQueens(board, row + 1, n) then
      begin
        SolveNQueens := true;
        Exit;
      end;
      
      // Backtrack: remove queen
      board[row, col] := false;
    end;
  end;
  
  SolveNQueens := false;
end;
```

---

## Tail Recursion

### What is Tail Recursion?

**Tail recursion** is when the recursive call is the last operation. It can be optimized by the compiler.

### Example: Tail Recursive Factorial

```pascal
function FactorialTail(n: integer; acc: integer): integer;
begin
  if n <= 1 then
    FactorialTail := acc
  else
    FactorialTail := FactorialTail(n - 1, n * acc);
end;

function Factorial(n: integer): integer;
begin
  Factorial := FactorialTail(n, 1);
end;
```

**The recursive call is the last operation - this is tail recursion!**

### Why Tail Recursion Matters

**Benefits:**
- Can be optimized to iteration
- No stack growth
- Same performance as iteration

---

## Best Practices

### 1. Recognize Patterns

**Learn to recognize:**
- Tree/graph structure → recursion
- Divide and conquer → recursion
- Backtracking → recursion
- Simple loops → iteration

### 2. Optimize When Needed

**If recursion is slow:**
- Use memoization
- Convert to tail recursion
- Use iteration instead

### 3. Understand Stack Depth

**Be aware of:**
- How deep recursion will go
- Stack overflow risk
- When to use iteration instead

---

## Exercises

### Exercise 1: Tree Traversal

Implement all three traversal types for a binary tree.

### Exercise 2: Divide and Conquer

Implement mergesort using divide and conquer.

### Exercise 3: Backtracking

Solve the N-Queens problem for n=4.

### Exercise 4: Tail Recursion

Convert factorial to tail recursive form.

---

**Previous Section:** [Recursion vs Iteration](./03_RecursionVsIteration.md)  
**Next Chapter:** [Chapter 08: Arrays and Records](../08_ArraysAndRecords/README.md)  
**Last Updated:** 2025-01-XX

