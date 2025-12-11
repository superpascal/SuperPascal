# One-Dimensional Arrays

**Part of:** [Chapter 08: Arrays and Records](./README.md)

---

> **For GCSE students:**  
> Arrays are like numbered boxes where you can store multiple items. Instead of having variables like `score1`, `score2`, `score3`, you can have one array `scores` and access each item by its number (index).
>
> **For A-Level students:**  
> Arrays are contiguous memory regions storing homogeneous elements accessed by index. They provide O(1) random access but fixed size. Understanding arrays is fundamental to data structures and algorithms.
>
> **For University students:**  
> Arrays are contiguous memory allocations with compile-time or runtime-determined size. They provide constant-time random access via pointer arithmetic. Understanding memory layout, cache behavior, and bounds checking is essential for performance-critical code.

---

## Introduction

**Arrays** let you store multiple values of the same type in a single variable. They're essential for:
- **Collections of data** — Lists of numbers, names, scores
- **Processing sequences** — Handle each item in a list
- **Efficient storage** — Access elements by index
- **Game data** — Sprite positions, scores, inventory items

This chapter starts with **one-dimensional arrays** — the foundation for all array concepts.

---

## What is an Array?

An **array** is:
- **A collection** — Multiple values of the same type
- **Indexed** — Each element has a position (index)
- **Fixed size** — Size determined at declaration
- **Contiguous** — Elements stored in order in memory

**Analogy:** Think of an array like a row of boxes:
- **Each box** — One element
- **Box number** — The index (0, 1, 2, ...)
- **Same type** — All boxes hold the same kind of thing
- **Fixed number** — Number of boxes is fixed

---

## Array Declaration

### Basic Syntax

**Array declaration:**
```pascal
var arrayName: array[indexRange] of elementType;
```

**Example:**
```pascal
var numbers: array[0..9] of integer;
```

**This creates:**
- An array named `numbers`
- 10 elements (indices 0 through 9)
- Each element is an `integer`

### Index Ranges

**Arrays can start at any index:**
```pascal
var arr1: array[0..9] of integer;    // Indices 0-9
var arr2: array[1..10] of integer;   // Indices 1-10
var arr3: array[5..14] of integer;   // Indices 5-14
```

**Common patterns:**
- **0-based** — `array[0..N-1]` (common in programming)
- **1-based** — `array[1..N]` (Pascal tradition)
- **Custom** — `array[Start..End]` (any range)

### Type Declaration

**You can declare array types:**
```pascal
type
  IntArray = array[0..9] of integer;
  StringArray = array[0..4] of string;

var numbers: IntArray;
var names: StringArray;
```

---

## Accessing Array Elements

### Indexing

**Access elements using square brackets:**
```pascal
var numbers: array[0..4] of integer;
begin
  numbers[0] := 10;  // Set first element
  numbers[1] := 20;  // Set second element
  numbers[2] := 30;  // Set third element
  
  WriteLn(numbers[0]);  // Read first element: 10
  WriteLn(numbers[1]);  // Read second element: 20
end.
```

### Index Bounds

**Indices must be within the declared range:**
```pascal
var arr: array[0..4] of integer;
begin
  arr[0] := 10;  // OK: 0 is in range [0..4]
  arr[4] := 50;  // OK: 4 is in range [0..4]
  arr[5] := 60;  // ERROR: 5 is out of range [0..4]
  arr[-1] := 0;  // ERROR: -1 is out of range
end.
```

**Bounds checking:**
- **Compile-time** — Compiler checks constant indices
- **Runtime** — Runtime checks variable indices (in debug mode)
- **Safety** — Prevents accessing invalid memory

---

## Array Initialization

### Manual Initialization

**Set each element individually:**
```pascal
var numbers: array[0..4] of integer;
begin
  numbers[0] := 10;
  numbers[1] := 20;
  numbers[2] := 30;
  numbers[3] := 40;
  numbers[4] := 50;
end.
```

### Loop Initialization

**Use loops to initialize:**
```pascal
var numbers: array[0..9] of integer;
var i: integer;
begin
  for i := 0 to 9 do
    numbers[i] := i * 10;  // 0, 10, 20, ..., 90
end.
```

### Initialization with Values (if supported)

**Some Pascal dialects support:**
```pascal
var numbers: array[0..4] of integer = (10, 20, 30, 40, 50);
```

**Note:** Check SuperPascal specification for initialization syntax support.

---

## Processing Arrays

### Pattern 1: Iterate All Elements

```pascal
var numbers: array[0..4] of integer;
var i: integer;
begin
  numbers[0] := 10;
  numbers[1] := 20;
  numbers[2] := 30;
  numbers[3] := 40;
  numbers[4] := 50;
  
  for i := 0 to 4 do
    WriteLn('Element ', i, ': ', numbers[i]);
end.
```

### Pattern 2: Find Maximum

```pascal
var numbers: array[0..4] of integer;
var i: integer;
var max: integer;
begin
  numbers[0] := 10;
  numbers[1] := 50;
  numbers[2] := 30;
  numbers[3] := 20;
  numbers[4] := 40;
  
  max := numbers[0];
  for i := 1 to 4 do
    if numbers[i] > max then
      max := numbers[i];
  
  WriteLn('Maximum: ', max);
end.
```

### Pattern 3: Sum Elements

```pascal
var numbers: array[0..4] of integer;
var i: integer;
var sum: integer;
begin
  numbers[0] := 10;
  numbers[1] := 20;
  numbers[2] := 30;
  numbers[3] := 40;
  numbers[4] := 50;
  
  sum := 0;
  for i := 0 to 4 do
    sum := sum + numbers[i];
  
  WriteLn('Sum: ', sum);
end.
```

### Pattern 4: Search

```pascal
var numbers: array[0..4] of integer;
var i: integer;
var target: integer;
var found: boolean;
begin
  numbers[0] := 10;
  numbers[1] := 20;
  numbers[2] := 30;
  numbers[3] := 40;
  numbers[4] := 50;
  
  target := 30;
  found := false;
  for i := 0 to 4 do
    if numbers[i] = target then
    begin
      found := true;
      WriteLn('Found at index ', i);
      break;  // Exit loop if break supported
    end;
  
  if not found then
    WriteLn('Not found');
end.
```

---

## Array Examples

### Example 1: Score Tracking

```pascal
var scores: array[0..9] of integer;
var i: integer;
var total: integer;
var average: integer;
begin
  // Initialize scores
  for i := 0 to 9 do
    scores[i] := (i + 1) * 10;  // 10, 20, 30, ..., 100
  
  // Calculate total
  total := 0;
  for i := 0 to 9 do
    total := total + scores[i];
  
  // Calculate average
  average := total div 10;
  
  WriteLn('Total: ', total);
  WriteLn('Average: ', average);
end.
```

### Example 2: Temperature Data

```pascal
var temperatures: array[0..6] of integer;  // One week
var i: integer;
var day: integer;
begin
  // Set temperatures for each day
  temperatures[0] := 20;  // Monday
  temperatures[1] := 22;  // Tuesday
  temperatures[2] := 18;  // Wednesday
  temperatures[3] := 25;  // Thursday
  temperatures[4] := 23;  // Friday
  temperatures[5] := 21;  // Saturday
  temperatures[6] := 19;  // Sunday
  
  // Find hottest day
  day := 0;
  for i := 1 to 6 do
    if temperatures[i] > temperatures[day] then
      day := i;
  
  WriteLn('Hottest day: Day ', day, ' (', temperatures[day], ' degrees)');
end.
```

### Example 3: Reversing an Array

```pascal
var numbers: array[0..4] of integer;
var temp: integer;
var i: integer;
begin
  // Initialize
  for i := 0 to 4 do
    numbers[i] := (i + 1) * 10;  // 10, 20, 30, 40, 50
  
  // Reverse
  for i := 0 to 2 do  // Only need to go halfway
  begin
    temp := numbers[i];
    numbers[i] := numbers[4 - i];
    numbers[4 - i] := temp;
  end;
  
  // Print reversed
  for i := 0 to 4 do
    WriteLn(numbers[i]);  // 50, 40, 30, 20, 10
end.
```

---

## Array Parameters

### Passing Arrays to Procedures

**Arrays can be passed as parameters:**
```pascal
procedure PrintArray(arr: array[0..4] of integer);
var i: integer;
begin
  for i := 0 to 4 do
    WriteLn('Element ', i, ': ', arr[i]);
end;

var numbers: array[0..4] of integer;
begin
  numbers[0] := 10;
  numbers[1] := 20;
  numbers[2] := 30;
  numbers[3] := 40;
  numbers[4] := 50;
  
  PrintArray(numbers);
end.
```

### Modifying Arrays

**Use `var` to modify array elements:**
```pascal
procedure DoubleArray(var arr: array[0..4] of integer);
var i: integer;
begin
  for i := 0 to 4 do
    arr[i] := arr[i] * 2;
end;

var numbers: array[0..4] of integer;
begin
  numbers[0] := 10;
  numbers[1] := 20;
  numbers[2] := 30;
  numbers[3] := 40;
  numbers[4] := 50;
  
  DoubleArray(numbers);
  // numbers now contains: 20, 40, 60, 80, 100
end.
```

---

## Array Size and Bounds

### Getting Array Size

**Array size is fixed at declaration:**
```pascal
var arr: array[0..9] of integer;  // Size is 10
```

**To get size programmatically:**
```pascal
var size: integer;
size := Length(arr);  // Get number of elements (10 for array[0..9])
// Alternative: size := High(arr) - Low(arr) + 1;
```

**Manual calculation:**
```pascal
// For array[0..N-1], size is N
// For array[Start..End], size is (End - Start + 1)

var arr: array[0..9] of integer;
var size: integer;
size := 9 - 0 + 1;  // 10
```

### Bounds Constants

**Use constants for array bounds:**
```pascal
const
  ARRAY_SIZE = 10;
  ARRAY_START = 0;
  ARRAY_END = ARRAY_SIZE - 1;

var numbers: array[ARRAY_START..ARRAY_END] of integer;
var i: integer;
begin
  for i := ARRAY_START to ARRAY_END do
    numbers[i] := i;
end.
```

---

## Common Patterns

### Pattern 1: Initialize with Sequence

```pascal
var arr: array[0..9] of integer;
var i: integer;
begin
  for i := 0 to 9 do
    arr[i] := i;  // 0, 1, 2, ..., 9
end.
```

### Pattern 2: Initialize with Value

```pascal
var arr: array[0..9] of integer;
var i: integer;
begin
  for i := 0 to 9 do
    arr[i] := 0;  // All zeros
end.
```

### Pattern 3: Copy Array

```pascal
var source: array[0..4] of integer;
var dest: array[0..4] of integer;
var i: integer;
begin
  // Initialize source
  for i := 0 to 4 do
    source[i] := (i + 1) * 10;
  
  // Copy to dest
  for i := 0 to 4 do
    dest[i] := source[i];
end.
```

### Pattern 4: Count Occurrences

```pascal
var numbers: array[0..9] of integer;
var target: integer;
var count: integer;
var i: integer;
begin
  // Initialize array
  for i := 0 to 9 do
    numbers[i] := (i mod 3) + 1;  // 1, 2, 3, 1, 2, 3, ...
  
  target := 2;
  count := 0;
  for i := 0 to 9 do
    if numbers[i] = target then
      count := count + 1;
  
  WriteLn('Count of ', target, ': ', count);
end.
```

---

## Best Practices

### 1. Use Constants for Bounds

**Bad:**
```pascal
var arr: array[0..9] of integer;
for i := 0 to 9 do  // Magic numbers!
```

**Good:**
```pascal
const ARRAY_SIZE = 10;
var arr: array[0..ARRAY_SIZE-1] of integer;
for i := 0 to ARRAY_SIZE-1 do
```

### 2. Check Bounds

**Always ensure indices are in range:**
```pascal
var index: integer;
if (index >= 0) and (index < ARRAY_SIZE) then
  arr[index] := value
else
  WriteLn('Index out of bounds');
```

### 3. Use Meaningful Names

**Bad:**
```pascal
var a: array[0..9] of integer;
```

**Good:**
```pascal
var scores: array[0..9] of integer;
var temperatures: array[0..6] of integer;
```

### 4. Initialize Before Use

**Always initialize arrays:**
```pascal
var arr: array[0..9] of integer;
var i: integer;
begin
  // Initialize all elements
  for i := 0 to 9 do
    arr[i] := 0;
  
  // Now safe to use
end.
```

---

## Platform Considerations

### Memory Usage

**Arrays use memory:**
- **Size** = (number of elements) × (size of element type)
- **Example:** `array[0..99] of integer` = 100 × 2 bytes = 200 bytes

**On ZealZ80 (512KB RAM):**
- **Be mindful** of array sizes
- **Use appropriate types** — `byte` instead of `integer` when possible
- **Consider memory limits** — Large arrays may not fit

### Performance

**Array access:**
- **Very fast** — Direct memory access by index
- **O(1) access** — Constant time to access any element
- **Cache-friendly** — Elements stored contiguously

---

## Common Mistakes

### Mistake 1: Index Out of Bounds

**Wrong:**
```pascal
var arr: array[0..4] of integer;
arr[5] := 10;  // ERROR: Index 5 is out of range [0..4]
```

**Correct:**
```pascal
var arr: array[0..4] of integer;
arr[4] := 10;  // OK: Index 4 is in range
```

### Mistake 2: Wrong Index Range

**Wrong:**
```pascal
var arr: array[0..9] of integer;
for i := 1 to 10 do  // Wrong: should be 0 to 9
  arr[i] := i;
```

**Correct:**
```pascal
var arr: array[0..9] of integer;
for i := 0 to 9 do  // Correct: matches array bounds
  arr[i] := i;
```

### Mistake 3: Uninitialized Array

**Wrong:**
```pascal
var arr: array[0..9] of integer;
WriteLn(arr[0]);  // ERROR: arr[0] is uninitialized!
```

**Correct:**
```pascal
var arr: array[0..9] of integer;
var i: integer;
for i := 0 to 9 do
  arr[i] := 0;  // Initialize
WriteLn(arr[0]);  // OK: now initialized
```

---

## Summary

**Key Concepts:**
- **Arrays** store multiple values of the same type
- **Indexed access** — Use `[index]` to access elements
- **Fixed size** — Size determined at declaration
- **Bounds checking** — Indices must be in range
- **Loop processing** — Use loops to process arrays

**Syntax:**
```pascal
var arr: array[start..end] of type;
arr[index] := value;  // Set element
value := arr[index];  // Get element
```

**Common Patterns:**
- Initialize with loops
- Process all elements
- Find maximum/minimum
- Sum elements
- Search for values

**Best Practices:**
- Use constants for bounds
- Check bounds before access
- Initialize before use
- Use meaningful names

**Next:** Learn about multi-dimensional arrays and matrices.

---

## Exercises

### GCSE Level Exercises

**Exercise 1: Create and Display Array**
Write a program that:
1. Creates an array of 5 integers
2. Assigns values: 10, 20, 30, 40, 50
3. Displays all values using a loop

**Exercise 2: Sum Array Elements**
Write a program that:
1. Creates an array of 10 numbers
2. Calculates and displays the sum of all elements
3. Displays the average

**Exercise 3: Find Maximum**
Write a program that:
1. Creates an array of 8 integers
2. Finds and displays the largest number
3. Finds and displays the smallest number

### A-Level Exercises

**Exercise 1: Array Statistics**
Write a program that:
1. Reads 20 integers into an array
2. Calculates and displays:
   - Sum
   - Average
   - Maximum
   - Minimum
   - Count of positive numbers
   - Count of negative numbers

**Exercise 2: Array Search and Count**
Write a program that:
1. Creates an array of 15 integers
2. Searches for a specific value
3. Counts how many times the value appears
4. Displays all positions where the value is found

**Exercise 3: Array Reversal**
Write a program that:
1. Creates an array of 10 integers
2. Reverses the array (first becomes last, etc.)
3. Displays the original and reversed arrays

### University Level Exercises

**Exercise 1: Efficient Array Operations**
Implement a set of array utility functions:
1. `FindAll(arr, value)` - Returns array of all indices where value appears
2. `RemoveDuplicates(arr)` - Returns new array with duplicates removed
3. `Partition(arr, pivot)` - Partitions array around pivot value
4. Analyze time and space complexity of each

**Exercise 2: Array-Based Data Structure**
Implement a dynamic array (vector) that:
1. Starts with capacity 4
2. Automatically grows when full (doubles capacity)
3. Supports append, insert, delete operations
4. Maintains O(1) amortized append time
5. Tracks size vs capacity

**Exercise 3: Cache-Friendly Array Processing**
Compare two approaches to processing large arrays:
1. Sequential access (cache-friendly)
2. Random access (cache-unfriendly)
3. Measure and compare performance
4. Analyze cache behavior and memory access patterns

---

**Next Section:** [Array Manipulation](./02_ArrayManipulation.md)  
**Language Specification:** See [03_TypeSystem.md](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

