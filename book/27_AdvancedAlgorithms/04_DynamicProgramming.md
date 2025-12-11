# Dynamic Programming

**Part of:** [Chapter 27: Advanced Algorithms](./README.md)

---

## Introduction

Dynamic programming solves complex problems by breaking them into simpler subproblems and storing results to avoid redundant calculations. This section teaches you memoization, optimal substructure, and how to apply dynamic programming to solve real problems.

**Key concepts:**
- **Memoization** — Caching results
- **Optimal substructure** — Building solutions from subproblems
- **Overlapping subproblems** — Avoiding redundant calculations
- **Bottom-up vs top-down** — Two approaches to DP

---

## What is Dynamic Programming?

### Core Concepts

**Dynamic programming has two key properties:**

1. **Optimal Substructure**
   - Optimal solution contains optimal solutions to subproblems
   - Can build solution from subproblem solutions

2. **Overlapping Subproblems**
   - Same subproblems solved multiple times
   - Cache results to avoid recomputation

### When to Use DP

**Use dynamic programming when:**
- Problem can be broken into subproblems
- Subproblems overlap (same subproblem appears multiple times)
- Optimal substructure exists
- Brute force is too slow

**Don't use DP when:**
- No overlapping subproblems
- Greedy algorithm works
- Simple recursive solution is fast enough

---

## Memoization (Top-Down)

### Fibonacci with Memoization

**Cache results to avoid recomputation:**

```pascal
var
  FibMemo: array[0..99] of integer;
  FibMemoized: array[0..99] of boolean;

function Fibonacci(n: integer): integer;
begin
  // Base cases
  if n <= 1 then
  begin
    Fibonacci := n;
    Exit;
  end;
  
  // Check memo
  if FibMemoized[n] then
  begin
    Fibonacci := FibMemo[n];
    Exit;
  end;
  
  // Compute and memoize
  FibMemo[n] := Fibonacci(n - 1) + Fibonacci(n - 2);
  FibMemoized[n] := true;
  Fibonacci := FibMemo[n];
end;
```

**Benefits:**
- Reduces O(2^n) to O(n) time
- Trades space for time
- Easy to implement

**Pitfalls:**
- ⚠️ Forgetting to initialize memo array
- ⚠️ Stack overflow for very large n
- ⚠️ Memory usage for large problems

---

## Tabulation (Bottom-Up)

### Fibonacci with Tabulation

**Build table from bottom up:**

```pascal
function FibonacciDP(n: integer): integer;
var
  dp: array[0..99] of integer;
  i: integer;
begin
  // Base cases
  dp[0] := 0;
  dp[1] := 1;
  
  // Build up
  for i := 2 to n do
    dp[i] := dp[i - 1] + dp[i - 2];
  
  FibonacciDP := dp[n];
end;
```

**Benefits:**
- No recursion (no stack overflow)
- Often faster than memoization
- Clear iteration pattern

**Pitfalls:**
- ⚠️ Must compute all subproblems
- ⚠️ May compute unnecessary subproblems

---

## Classic DP Problems

### Longest Common Subsequence (LCS)

**Find longest common subsequence:**

```pascal
function LCS(s1, s2: string; len1, len2: integer): integer;
var
  dp: array[0..99, 0..99] of integer;
  i, j: integer;
begin
  // Initialize base cases
  for i := 0 to len1 do
    dp[i, 0] := 0;
  for j := 0 to len2 do
    dp[0, j] := 0;
  
  // Fill DP table
  for i := 1 to len1 do
  begin
    for j := 1 to len2 do
    begin
      if s1[i] = s2[j] then
        dp[i, j] := dp[i - 1, j - 1] + 1
      else
      begin
        if dp[i - 1, j] > dp[i, j - 1] then
          dp[i, j] := dp[i - 1, j]
        else
          dp[i, j] := dp[i, j - 1];
      end;
    end;
  end;
  
  LCS := dp[len1, len2];
end;
```

### Coin Change Problem

**Minimum coins to make amount:**

```pascal
function CoinChange(coins: array of integer; amount: integer): integer;
var
  dp: array[0..999] of integer;
  i, j: integer;
begin
  // Initialize: dp[i] = minimum coins for amount i
  dp[0] := 0;
  for i := 1 to amount do
    dp[i] := MAX_INT;
  
  // For each coin
  for i := 0 to Length(coins) - 1 do
  begin
    // For each amount
    for j := coins[i] to amount do
    begin
      if dp[j - coins[i]] <> MAX_INT then
      begin
        if dp[j - coins[i]] + 1 < dp[j] then
          dp[j] := dp[j - coins[i]] + 1;
      end;
    end;
  end;
  
  if dp[amount] = MAX_INT then
    CoinChange := -1  // Not possible
  else
    CoinChange := dp[amount];
end;
```

### Knapsack Problem

**0/1 Knapsack:**

```pascal
function Knapsack(weights, values: array of integer; 
                  capacity: integer; itemCount: integer): integer;
var
  dp: array[0..99, 0..999] of integer;
  i, w: integer;
begin
  // Initialize
  for i := 0 to itemCount do
    for w := 0 to capacity do
      dp[i, w] := 0;
  
  // Fill DP table
  for i := 1 to itemCount do
  begin
    for w := 1 to capacity do
    begin
      if weights[i - 1] <= w then
      begin
        // Can include item
        if values[i - 1] + dp[i - 1, w - weights[i - 1]] > dp[i - 1, w] then
          dp[i, w] := values[i - 1] + dp[i - 1, w - weights[i - 1]]
        else
          dp[i, w] := dp[i - 1, w];
      end
      else
        // Cannot include item
        dp[i, w] := dp[i - 1, w];
    end;
  end;
  
  Knapsack := dp[itemCount, capacity];
end;
```

---

## DP Patterns

### Pattern 1: 1D DP

**Single dimension:**

```pascal
// Example: Fibonacci, climbing stairs
dp[i] = f(dp[i-1], dp[i-2], ...)
```

### Pattern 2: 2D DP

**Two dimensions:**

```pascal
// Example: LCS, edit distance
dp[i, j] = f(dp[i-1, j], dp[i, j-1], dp[i-1, j-1])
```

### Pattern 3: State Machine

**Multiple states:**

```pascal
// Example: Stock trading
dp[i][state] = f(dp[i-1][previous_states])
```

---

## Best Practices

### 1. Identify Subproblems

**Break problem down:**

```pascal
// ✅ GOOD: Identify subproblems
// LCS: What's LCS of s1[0..i] and s2[0..j]?

// ❌ BAD: Try to solve whole problem at once
```

### 2. Define State

**Clear state definition:**

```pascal
// ✅ GOOD: Clear state
// dp[i] = minimum coins for amount i

// ❌ BAD: Unclear state
// dp[i] = something related to i
```

### 3. Find Recurrence

**How to compute state from previous:**

```pascal
// ✅ GOOD: Clear recurrence
dp[i] := Min(dp[i - coin1], dp[i - coin2], ...) + 1;

// ❌ BAD: Unclear how to compute
```

---

**Previous Section:** [Graph Algorithms](./03_GraphAlgorithms.md)  
**Next Section:** [Other Important Algorithms](./05_OtherAlgorithms.md)  
**Last Updated:** 2025-01-XX

