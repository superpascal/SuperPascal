# Other Important Algorithms

**Part of:** [Chapter 27: Advanced Algorithms](./README.md)

---

## Introduction

Beyond sorting, searching, graphs, and dynamic programming, there are many other important algorithms that every programmer should know. This section covers pathfinding, string algorithms, mathematical algorithms, and more.

**Key concepts:**
- **Pathfinding** — A* algorithm
- **String algorithms** — Pattern matching, string manipulation
- **Mathematical algorithms** — GCD, prime numbers, factorials
- **Greedy algorithms** — Making locally optimal choices
- **Backtracking** — Exploring all possibilities

---

## Pathfinding Algorithms

### A* Algorithm

**Optimal pathfinding:**

```pascal
type
  TNode = record
    X, Y: integer;
    G, H, F: integer;  // G = cost from start, H = heuristic, F = G + H
    Parent: ^TNode;
  end;

function Heuristic(x1, y1, x2, y2: integer): integer;
begin
  // Manhattan distance
  Heuristic := Abs(x2 - x1) + Abs(y2 - y1);
end;

function AStar(startX, startY, goalX, goalY: integer): boolean;
var
  openList, closedList: array of TNode;
  current: TNode;
  neighbors: array[0..3] of TNode;
  i: integer;
begin
  // Initialize
  current.X := startX;
  current.Y := startY;
  current.G := 0;
  current.H := Heuristic(startX, startY, goalX, goalY);
  current.F := current.G + current.H;
  
  // A* algorithm implementation
  // (Simplified - full implementation would use priority queue)
  
  AStar := false;  // Placeholder
end;
```

**Pros:**
- ✅ Finds optimal path
- ✅ Efficient with good heuristic
- ✅ Widely used in games

**Cons:**
- ❌ Requires good heuristic
- ❌ More complex than BFS/DFS

**When to use:**
- Pathfinding in games
- Navigation systems
- When optimal path is needed

---

## String Algorithms

### Pattern Matching (KMP-like)

**Efficient string search:**

```pascal
function FindPattern(text, pattern: string): integer;
var
  i, j: integer;
begin
  i := 1;
  j := 1;
  
  while (i <= Length(text)) and (j <= Length(pattern)) do
  begin
    if text[i] = pattern[j] then
    begin
      i := i + 1;
      j := j + 1;
    end
    else
    begin
      i := i - j + 2;
      j := 1;
    end;
  end;
  
  if j > Length(pattern) then
    FindPattern := i - Length(pattern)
  else
    FindPattern := 0;
end;
```

### String Reversal

**Reverse a string:**

```pascal
function ReverseString(s: string): string;
var
  i: integer;
  result: string;
begin
  result := '';
  for i := Length(s) downto 1 do
    result := result + s[i];
  ReverseString := result;
end;
```

### Palindrome Check

**Check if string is palindrome:**

```pascal
function IsPalindrome(s: string): boolean;
var
  i, len: integer;
begin
  len := Length(s);
  for i := 1 to len div 2 do
    if s[i] <> s[len - i + 1] then
    begin
      IsPalindrome := false;
      Exit;
    end;
  IsPalindrome := true;
end;
```

---

## Mathematical Algorithms

### Greatest Common Divisor (GCD)

**Euclidean algorithm:**

```pascal
function GCD(a, b: integer): integer;
var
  temp: integer;
begin
  while b <> 0 do
  begin
    temp := b;
    b := a mod b;
    a := temp;
  end;
  GCD := a;
end;
```

### Least Common Multiple (LCM)

**Using GCD:**

```pascal
function LCM(a, b: integer): integer;
begin
  LCM := (a * b) div GCD(a, b);
end;
```

### Prime Number Check

**Check if number is prime:**

```pascal
function IsPrime(n: integer): boolean;
var
  i: integer;
begin
  if n < 2 then
  begin
    IsPrime := false;
    Exit;
  end;
  
  for i := 2 to Trunc(Sqrt(n)) do
    if n mod i = 0 then
    begin
      IsPrime := false;
      Exit;
    end;
  
  IsPrime := true;
end;
```

### Factorial

**Calculate factorial:**

```pascal
function Factorial(n: integer): integer;
begin
  if n <= 1 then
    Factorial := 1
  else
    Factorial := n * Factorial(n - 1);
end;
```

---

## Greedy Algorithms

### Activity Selection

**Select maximum activities:**

```pascal
type
  TActivity = record
    Start, Finish: integer;
  end;

procedure SelectActivities(activities: array of TActivity);
var
  selected: array[0..99] of integer;
  count, lastFinish, i: integer;
begin
  // Sort by finish time (assume sorted)
  count := 0;
  lastFinish := 0;
  
  for i := 0 to Length(activities) - 1 do
  begin
    if activities[i].Start >= lastFinish then
    begin
      selected[count] := i;
      count := count + 1;
      lastFinish := activities[i].Finish;
    end;
  end;
end;
```

**Greedy strategy:**
- Always choose activity that finishes earliest
- Leaves maximum time for remaining activities

---

## Backtracking

### N-Queens Problem

**Place N queens on N×N board:**

```pascal
function IsSafe(board: array of array of boolean; 
                row, col, n: integer): boolean;
var
  i, j: integer;
begin
  // Check column
  for i := 0 to row - 1 do
    if board[i, col] then
    begin
      IsSafe := false;
      Exit;
    end;
  
  // Check diagonal
  i := row - 1;
  j := col - 1;
  while (i >= 0) and (j >= 0) do
  begin
    if board[i, j] then
    begin
      IsSafe := false;
      Exit;
    end;
    i := i - 1;
    j := j - 1;
  end;
  
  // Check other diagonal
  i := row - 1;
  j := col + 1;
  while (i >= 0) and (j < n) do
  begin
    if board[i, j] then
    begin
      IsSafe := false;
      Exit;
    end;
    i := i - 1;
    j := j + 1;
  end;
  
  IsSafe := true;
end;

function SolveNQueens(var board: array of array of boolean; 
                     row, n: integer): boolean;
var
  col: integer;
begin
  if row >= n then
  begin
    SolveNQueens := true;
    Exit;
  end;
  
  for col := 0 to n - 1 do
  begin
    if IsSafe(board, row, col, n) then
    begin
      board[row, col] := true;
      if SolveNQueens(board, row + 1, n) then
      begin
        SolveNQueens := true;
        Exit;
      end;
      board[row, col] := false;  // Backtrack
    end;
  end;
  
  SolveNQueens := false;
end;
```

---

## Algorithm Complexity Review

### Common Complexities

**Time complexities:**
- O(1) — Constant (hash lookup)
- O(log n) — Logarithmic (binary search)
- O(n) — Linear (array iteration)
- O(n log n) — Linearithmic (efficient sorting)
- O(n²) — Quadratic (nested loops)
- O(2^n) — Exponential (brute force)

**Space complexities:**
- O(1) — Constant (in-place)
- O(n) — Linear (array storage)
- O(n²) — Quadratic (2D array)

---

## Best Practices

### 1. Understand the Problem

**Analyze before coding:**

```pascal
// ✅ GOOD: Understand problem first
// What are the constraints?
// What's the input/output?
// What's the goal?

// ❌ BAD: Start coding immediately
```

### 2. Choose Right Algorithm

**Match algorithm to problem:**

```pascal
// ✅ GOOD: Choose appropriate algorithm
if SmallRange then
  CountingSort(arr)
else
  QuickSort(arr, 0, Length(arr) - 1);

// ❌ BAD: Always use one algorithm
```

### 3. Test Thoroughly

**Test edge cases:**

```pascal
// ✅ GOOD: Test various cases
TestAlgorithm(EmptyArray);
TestAlgorithm(SingleElement);
TestAlgorithm(SortedArray);
TestAlgorithm(ReverseSorted);
TestAlgorithm(RandomArray);

// ❌ BAD: Only test one case
```

---

## Exercises

### Exercise 1: Implement A*

Write a program that:
1. Implements A* pathfinding
2. Finds path on grid
3. Visualizes the path
4. Compares with BFS/DFS

### Exercise 2: String Algorithms

Write a program that:
1. Implements pattern matching
2. Finds all occurrences
3. Handles edge cases
4. Measures performance

### Exercise 3: Mathematical Algorithms

Write a program that:
1. Implements GCD/LCM
2. Finds prime numbers
3. Calculates factorials
4. Tests with various inputs

### Exercise 4: Backtracking

Write a program that:
1. Solves N-Queens problem
2. Finds all solutions
3. Counts solutions
4. Visualizes solutions

---

**Previous Section:** [Dynamic Programming](./04_DynamicProgramming.md)  
**Next Chapter:** [Chapter 28: Advanced Data Structures](../28_AdvancedDataStructures/README.md)  
**Last Updated:** 2025-01-XX

