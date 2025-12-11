# Programs and Algorithms

**Part of:** [Chapter 01: Introduction to Programming on Zeal](./README.md)

---

## Introduction

Now that you understand what a computer is (from [Chapter 1: What Is a Computer?](./01_WhatIsAComputer.md)), let's learn what **programs** are and how they work. 

Remember from Chapter 1: computers follow instructions step-by-step. A **program** is a sequence of those instructions that tell the computer what to do. But before we write programs, we need to understand **algorithms** — the step-by-step procedures that solve problems.

> **Connection to Chapter 1:** Recall that the CPU follows a fetch → decode → execute cycle. Programs are the instructions that the CPU fetches, decodes, and executes. The ALU performs the calculations, and memory stores the data. *Technical details about how programs are stored in memory and executed are covered in Part IV: Computers in Depth.*

---

## What is a Program?

A **program** is a sequence of instructions that a computer can execute. Programs tell the computer:
- **What data to use** — Variables, arrays, structures
- **What operations to perform** — Calculations, comparisons, decisions
- **What order to do things** — Sequence, conditions, loops
- **How to interact** — Input, output, hardware access

### Programs vs. Data

**Programs** are instructions:
- **Code** — The instructions themselves
- **Executable** — The computer runs them
- **Stored in memory** — Loaded from storage into RAM
- **Sequential** — Executed one instruction at a time

**Data** is information:
- **Variables** — Named storage locations
- **Values** — Numbers, text, structures
- **Processed** — Programs operate on data
- **Stored** — Saved in memory or files

### Example: Simple Program

```pascal
program Hello;
begin
  WriteLn('Hello, World!');
end.
```

This program:
1. **Declares** a program named "Hello"
2. **Executes** the `begin...end` block
3. **Calls** `WriteLn` to output text
4. **Ends** the program

---

## What is an Algorithm?

An **algorithm** is a step-by-step procedure for solving a problem. Algorithms are:
- **Precise** — Each step is clearly defined
- **Deterministic** — Same input always produces same output
- **Finite** — Algorithm eventually completes
- **Effective** — Solves the problem correctly

### Algorithm Characteristics

**Input:**
- Algorithms take **input** (data to process)
- Input can be from user, files, or other sources
- Input defines what the algorithm works with

**Output:**
- Algorithms produce **output** (results)
- Output can be displayed, saved, or used by other programs
- Output is the solution to the problem

**Steps:**
- Algorithms have **clear steps**
- Each step is unambiguous
- Steps are executed in order (or as specified)

**Termination:**
- Algorithms **must terminate**
- They eventually finish (no infinite loops without purpose)
- They produce a result or indicate failure

### Example: Finding the Largest Number

**Problem:** Find the largest number in a list.

**Algorithm:**
1. Start with the first number as the largest
2. For each remaining number:
   - If this number is larger than the current largest
   - Make this number the new largest
3. Return the largest number

**In SuperPascal:**
```pascal
function FindLargest(numbers: array[0..9] of integer): integer;
var
  i: integer;
  largest: integer;
begin
  largest := numbers[0];  // Start with first number
  for i := 1 to 9 do
    if numbers[i] > largest then
      largest := numbers[i];
  FindLargest := largest;
end;
```

---

## Flow of Control

**Flow of control** is the order in which instructions execute. Understanding flow of control is essential for programming.

### Sequential Execution

**Default:** Programs execute **sequentially** (top to bottom):

```pascal
program Sequential;
begin
  WriteLn('First');
  WriteLn('Second');
  WriteLn('Third');
end.
```

**Output:**
```
First
Second
Third
```

Instructions execute in the order written.

### Conditional Execution

**Conditionals** change flow based on conditions:

```pascal
program Conditional;
var age: integer;
begin
  age := 18;
  if age >= 18 then
    WriteLn('Adult')
  else
    WriteLn('Minor');
end.
```

**Flow:**
- **If** condition is true → execute "then" block
- **Else** → execute "else" block (if present)
- **Skip** → continue after the if statement

### Repetition (Loops)

**Loops** repeat instructions:

```pascal
program Loop;
var i: integer;
begin
  for i := 1 to 5 do
    WriteLn('Count: ', i);
end.
```

**Output:**
```
Count: 1
Count: 2
Count: 3
Count: 4
Count: 5
```

**Flow:**
- **Repeat** the loop body
- **Increment** the counter
- **Check** if done
- **Continue** or exit

### Function Calls

**Functions** jump to other code and return:

```pascal
program Functions;
function Add(a, b: integer): integer;
begin
  Add := a + b;
end;

begin
  WriteLn('Sum: ', Add(3, 4));
end.
```

**Flow:**
1. **Call** function (jump to function code)
2. **Execute** function body
3. **Return** to caller with result
4. **Continue** after function call

---

## Algorithm Design

### Step 1: Understand the Problem

**Ask:**
- What is the input?
- What is the output?
- What are the constraints?
- What are the edge cases?

**Example:** Calculate average of numbers
- **Input:** List of numbers
- **Output:** Average (sum / count)
- **Constraints:** Numbers are integers
- **Edge cases:** Empty list? Division by zero?

### Step 2: Design the Algorithm

**Break down into steps:**
1. Initialize variables
2. Process input
3. Perform calculations
4. Produce output

**Example:** Calculate average
1. Initialize sum = 0, count = 0
2. For each number: add to sum, increment count
3. Calculate average = sum / count
4. Return average

### Step 3: Write the Code

**Translate algorithm to SuperPascal:**

```pascal
function CalculateAverage(numbers: array[0..9] of integer): integer;
var
  i, sum, count: integer;
  average: integer;
begin
  sum := 0;
  count := 10;  // Array has 10 elements
  
  for i := 0 to 9 do
    sum := sum + numbers[i];
  
  average := sum div count;  // Integer division
  CalculateAverage := average;
end;
```

### Step 4: Test the Algorithm

**Test with different inputs:**
- Normal case: `[10, 20, 30]` → average = 20
- Edge case: `[5, 5, 5]` → average = 5
- Boundary: `[0, 0, 0]` → average = 0

---

## Common Algorithm Patterns

### Pattern 1: Accumulation

**Accumulate** values in a variable:

```pascal
var sum: integer;
sum := 0;
for i := 1 to 10 do
  sum := sum + i;  // Accumulate
```

**Use for:** Sums, products, counting

### Pattern 2: Search

**Search** for a value:

```pascal
function FindValue(arr: array[0..9] of integer; target: integer): boolean;
var i: integer;
begin
  for i := 0 to 9 do
    if arr[i] = target then
    begin
      FindValue := true;
      Exit;  // Found, stop searching
    end;
  FindValue := false;  // Not found
end;
```

**Use for:** Finding items, checking existence

### Pattern 3: Transformation

**Transform** each element:

```pascal
procedure DoubleArray(var arr: array[0..9] of integer);
var i: integer;
begin
  for i := 0 to 9 do
    arr[i] := arr[i] * 2;  // Transform each element
end;
```

**Use for:** Modifying data, applying operations

### Pattern 4: Filtering

**Filter** elements that meet criteria:

```pascal
function CountEven(arr: array[0..9] of integer): integer;
var i, count: integer;
begin
  count := 0;
  for i := 0 to 9 do
    if (arr[i] mod 2) = 0 then  // Filter: even numbers
      count := count + 1;
  CountEven := count;
end;
```

**Use for:** Selecting items, counting matches

---

## Algorithm Complexity

### Time Complexity

**Time complexity** measures how long an algorithm takes:
- **Fast:** Constant time O(1) — instant
- **Medium:** Linear time O(n) — proportional to input size
- **Slow:** Quadratic time O(n²) — grows quickly

**Example:** Finding largest number
- **Time:** O(n) — must check each number once
- **Space:** O(1) — only needs a few variables

### Space Complexity

**Space complexity** measures memory usage:
- **Efficient:** O(1) — constant memory
- **Moderate:** O(n) — proportional to input
- **Inefficient:** O(n²) — grows quickly

**On retro platforms:** Space is often more limited than time!

---

## From Algorithm to Program

### The Translation Process

1. **Algorithm** (human language) → Step-by-step procedure
2. **Pseudocode** (structured language) → Algorithm-like code
3. **SuperPascal** (programming language) → Executable code
4. **Machine Code** (CPU instructions) → What CPU executes

### Example Translation

**Algorithm:**
"Find the largest number in a list"

**Pseudocode:**
```
largest = first number
for each number in list:
  if number > largest:
    largest = number
return largest
```

**SuperPascal:**
```pascal
function FindLargest(numbers: array[0..9] of integer): integer;
var
  i, largest: integer;
begin
  largest := numbers[0];
  for i := 1 to 9 do
    if numbers[i] > largest then
      largest := numbers[i];
  FindLargest := largest;
end;
```

**Machine Code (Z80 example):**
```assembly
LD HL, numbers    ; Load array address
LD A, (HL)        ; Load first number
LD B, 9           ; Loop counter
loop:
  INC HL          ; Next number
  CP (HL)         ; Compare
  JP NC, skip     ; If not larger, skip
  LD A, (HL)      ; Update largest
skip:
  DJNZ loop       ; Decrement and loop
RET               ; Return result in A
```

---

## Thinking Like a Programmer

### Problem-Solving Steps

1. **Understand** — What is the problem?
2. **Break down** — Divide into smaller parts
3. **Design** — Create an algorithm
4. **Code** — Write the program
5. **Test** — Verify it works
6. **Refine** — Improve and optimize

### Mental Models

**Variables as boxes:**
- Variables are like labeled boxes
- You put values in boxes
- You read values from boxes
- Boxes have types (what can go in them)

**Functions as machines:**
- Functions take input
- Functions process input
- Functions produce output
- Functions can be reused

**Arrays as shelves:**
- Arrays are like numbered shelves
- Each shelf holds one item
- You access items by number (index)
- Items are the same type

---

## Summary

**Key Concepts:**
- **Programs** are sequences of instructions
- **Algorithms** are step-by-step problem-solving procedures
- **Flow of control** determines execution order
- **Algorithm design** is breaking problems into steps
- **Translation** converts algorithms to code

**Flow of Control:**
- **Sequential** — Top to bottom
- **Conditional** — If/else decisions
- **Repetition** — Loops
- **Functions** — Jump and return

**Next:** Run your first SuperPascal program and learn to use ZealIDE.

---

**Next Section:** [Running Your First Program](./03_RunningYourFirstProgram.md)  
**Language Specification:** See [02_Grammar.md](../../languageSpecification/02_Grammar.md)  
**Last Updated:** 2025-01-XX

