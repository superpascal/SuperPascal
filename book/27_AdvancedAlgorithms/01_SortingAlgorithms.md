# Sorting Algorithms

**Part of:** [Chapter 27: Advanced Algorithms](./README.md)

---

## Introduction

Sorting is one of the most fundamental operations in programming. This section teaches you eight important sorting algorithms, their pros and cons, common pitfalls, and when to use each one.

**Key concepts:**
- **Basic sorts** — Bubble, selection, insertion
- **Intermediate sorts** — Shell sort
- **Advanced sorts** — Quicksort, mergesort, heapsort
- **Specialized sorts** — Counting sort
- **Algorithm comparison** — When to use which

---

## Understanding Sorting

### What is Sorting?

**Sorting arranges data in order:**
- **Ascending** — Smallest to largest
- **Descending** — Largest to smallest
- **Stable** — Preserves order of equal elements
- **In-place** — Uses only constant extra memory

### Sorting Criteria

**Compare algorithms by:**
- **Time complexity** — How fast?
- **Space complexity** — How much memory?
- **Stability** — Preserves order?
- **Adaptive** — Faster on nearly sorted data?
- **Implementation complexity** — How hard to code?

---

## Basic Sorting Algorithms

### 1. Bubble Sort

**Simple but slow:**

```pascal
procedure BubbleSort(var arr: array of integer);
var
  i, j, temp: integer;
  swapped: boolean;
begin
  for i := 0 to Length(arr) - 2 do
  begin
    swapped := false;
    for j := 0 to Length(arr) - 2 - i do
    begin
      if arr[j] > arr[j + 1] then
      begin
        // Swap
        temp := arr[j];
        arr[j] := arr[j + 1];
        arr[j + 1] := temp;
        swapped := true;
      end;
    end;
    
    // Optimization: stop if no swaps
    if not swapped then
      Break;
  end;
end;
```

**Pros:**
- ✅ Simple to understand
- ✅ Easy to implement
- ✅ Stable (preserves order)
- ✅ In-place (O(1) extra space)
- ✅ Adaptive (stops early if sorted)

**Cons:**
- ❌ Very slow: O(n²) time
- ❌ Many unnecessary comparisons
- ❌ Many swaps even for small arrays

**Pitfalls:**
- ⚠️ Off-by-one errors in loop bounds
- ⚠️ Forgetting to swap correctly
- ⚠️ Not optimizing with early exit

**When to use:**
- Educational purposes
- Very small arrays (< 10 elements)
- Nearly sorted data (with optimization)

---

### 2. Selection Sort

**Find minimum and swap:**

```pascal
procedure SelectionSort(var arr: array of integer);
var
  i, j, minIndex, temp: integer;
begin
  for i := 0 to Length(arr) - 2 do
  begin
    // Find minimum in unsorted portion
    minIndex := i;
    for j := i + 1 to Length(arr) - 1 do
      if arr[j] < arr[minIndex] then
        minIndex := j;
    
    // Swap with first unsorted element
    if minIndex <> i then
    begin
      temp := arr[i];
      arr[i] := arr[minIndex];
      arr[minIndex] := temp;
    end;
  end;
end;
```

**Pros:**
- ✅ Simple to understand
- ✅ Easy to implement
- ✅ In-place (O(1) extra space)
- ✅ Fewer swaps than bubble sort
- ✅ Predictable: always O(n²)

**Cons:**
- ❌ Slow: O(n²) time
- ❌ Not stable (may change order of equal elements)
- ❌ Not adaptive (always same time)
- ❌ Many comparisons

**Pitfalls:**
- ⚠️ Forgetting to check if minIndex changed
- ⚠️ Off-by-one in loop bounds
- ⚠️ Swapping when not needed

**When to use:**
- Educational purposes
- Small arrays
- When minimizing swaps is important
- When stability doesn't matter

---

### 3. Insertion Sort

**Insert elements in correct position:**

```pascal
procedure InsertionSort(var arr: array of integer);
var
  i, j, key: integer;
begin
  for i := 1 to Length(arr) - 1 do
  begin
    key := arr[i];
    j := i - 1;
    
    // Shift elements greater than key
    while (j >= 0) and (arr[j] > key) do
    begin
      arr[j + 1] := arr[j];
      j := j - 1;
    end;
    
    // Insert key in correct position
    arr[j + 1] := key;
  end;
end;
```

**Pros:**
- ✅ Simple to understand
- ✅ Efficient for small arrays
- ✅ Stable (preserves order)
- ✅ In-place (O(1) extra space)
- ✅ Adaptive (fast on nearly sorted data)
- ✅ Online (can sort as data arrives)

**Cons:**
- ❌ Slow for large arrays: O(n²) worst case
- ❌ Many shifts for unsorted data

**Pitfalls:**
- ⚠️ Off-by-one in insertion position
- ⚠️ Forgetting to shift elements
- ⚠️ Not handling empty/single-element arrays

**When to use:**
- Small arrays (< 50 elements)
- Nearly sorted data
- Part of hybrid algorithms (e.g., timsort)
- Online sorting (data arriving one at a time)

---

## Intermediate Sorting Algorithms

### 4. Shell Sort

**Improved insertion sort:**

```pascal
procedure ShellSort(var arr: array of integer);
var
  gap, i, j, temp: integer;
begin
  // Start with large gap, reduce it
  gap := Length(arr) div 2;
  
  while gap > 0 do
  begin
    // Do insertion sort for this gap
    for i := gap to Length(arr) - 1 do
    begin
      temp := arr[i];
      j := i;
      
      // Shift elements by gap
      while (j >= gap) and (arr[j - gap] > temp) do
      begin
        arr[j] := arr[j - gap];
        j := j - gap;
      end;
      
      arr[j] := temp;
    end;
    
    gap := gap div 2;
  end;
end;
```

**Pros:**
- ✅ Better than O(n²) in practice
- ✅ In-place (O(1) extra space)
- ✅ Adaptive (faster on partially sorted data)
- ✅ Simple to implement

**Cons:**
- ❌ Time complexity depends on gap sequence
- ❌ Not stable
- ❌ Worst case can be O(n²)

**Pitfalls:**
- ⚠️ Choosing poor gap sequence
- ⚠️ Off-by-one in gap calculations
- ⚠️ Not handling edge cases

**When to use:**
- Medium-sized arrays
- When in-place sorting is required
- When stability isn't needed
- Embedded systems with memory constraints

---

## Advanced Sorting Algorithms

### 5. Quicksort

**Divide and conquer:**

```pascal
function Partition(var arr: array of integer; left, right: integer): integer;
var
  pivot, i, j, temp: integer;
begin
  // Choose pivot (middle element)
  pivot := arr[(left + right) div 2];
  i := left;
  j := right;
  
  while i <= j do
  begin
    while arr[i] < pivot do
      i := i + 1;
    while arr[j] > pivot do
      j := j - 1;
    
    if i <= j then
    begin
      // Swap
      temp := arr[i];
      arr[i] := arr[j];
      arr[j] := temp;
      i := i + 1;
      j := j - 1;
    end;
  end;
  
  Partition := i;
end;

procedure QuickSort(var arr: array of integer; left, right: integer);
var
  pivotIndex: integer;
begin
  if left < right then
  begin
    pivotIndex := Partition(arr, left, right);
    QuickSort(arr, left, pivotIndex - 1);
    QuickSort(arr, pivotIndex, right);
  end;
end;
```

**Pros:**
- ✅ Fast: O(n log n) average
- ✅ In-place (O(1) extra space)
- ✅ Cache-friendly (good locality)
- ✅ Fast in practice

**Cons:**
- ❌ Worst case: O(n²) if pivot is bad
- ❌ Not stable
- ❌ Recursive (stack space)

**Pitfalls:**
- ⚠️ Poor pivot selection (worst case)
- ⚠️ Stack overflow on large arrays
- ⚠️ Off-by-one in partition
- ⚠️ Infinite recursion if not careful

**When to use:**
- General-purpose sorting
- Large arrays
- When average performance matters
- When stability isn't needed

**Improvements:**
- Use median-of-three for pivot
- Switch to insertion sort for small subarrays
- Use iterative version to avoid stack overflow

---

### 6. Mergesort

**Divide and merge:**

```pascal
procedure Merge(var arr: array of integer; 
                left, mid, right: integer);
var
  temp: array of integer;
  i, j, k: integer;
begin
  SetLength(temp, right - left + 1);
  
  i := left;
  j := mid + 1;
  k := 0;
  
  // Merge sorted halves
  while (i <= mid) and (j <= right) do
  begin
    if arr[i] <= arr[j] then
    begin
      temp[k] := arr[i];
      i := i + 1;
    end
    else
    begin
      temp[k] := arr[j];
      j := j + 1;
    end;
    k := k + 1;
  end;
  
  // Copy remaining elements
  while i <= mid do
  begin
    temp[k] := arr[i];
    i := i + 1;
    k := k + 1;
  end;
  
  while j <= right do
  begin
    temp[k] := arr[j];
    j := j + 1;
    k := k + 1;
  end;
  
  // Copy back
  for i := 0 to k - 1 do
    arr[left + i] := temp[i];
end;

procedure MergeSort(var arr: array of integer; left, right: integer);
var
  mid: integer;
begin
  if left < right then
  begin
    mid := (left + right) div 2;
    MergeSort(arr, left, mid);
    MergeSort(arr, mid + 1, right);
    Merge(arr, left, mid, right);
  end;
end;
```

**Pros:**
- ✅ Always O(n log n) (predictable)
- ✅ Stable (preserves order)
- ✅ Good for linked lists
- ✅ Parallelizable

**Cons:**
- ❌ Requires O(n) extra memory
- ❌ Not in-place
- ❌ Slower than quicksort in practice
- ❌ More complex to implement

**Pitfalls:**
- ⚠️ Off-by-one in merge indices
- ⚠️ Memory allocation for temp array
- ⚠️ Forgetting to copy remaining elements
- ⚠️ Stack overflow on very large arrays

**When to use:**
- When stability is required
- When worst-case performance matters
- Sorting linked lists
- External sorting (large files)
- When memory is available

---

### 7. Heapsort

**Uses heap data structure:**

```pascal
procedure Heapify(var arr: array of integer; n, i: integer);
var
  largest, left, right, temp: integer;
begin
  largest := i;
  left := 2 * i + 1;
  right := 2 * i + 2;
  
  // Find largest among root and children
  if (left < n) and (arr[left] > arr[largest]) then
    largest := left;
  
  if (right < n) and (arr[right] > arr[largest]) then
    largest := right;
  
  // If root is not largest, swap and heapify
  if largest <> i then
  begin
    temp := arr[i];
    arr[i] := arr[largest];
    arr[largest] := temp;
    Heapify(arr, n, largest);
  end;
end;

procedure HeapSort(var arr: array of integer);
var
  i, temp: integer;
begin
  // Build max heap
  for i := (Length(arr) div 2) - 1 downto 0 do
    Heapify(arr, Length(arr), i);
  
  // Extract elements from heap
  for i := Length(arr) - 1 downto 1 do
  begin
    // Move root to end
    temp := arr[0];
    arr[0] := arr[i];
    arr[i] := temp;
    
    // Heapify reduced heap
    Heapify(arr, i, 0);
  end;
end;
```

**Pros:**
- ✅ Always O(n log n) (predictable)
- ✅ In-place (O(1) extra space)
- ✅ No worst-case degradation
- ✅ Good worst-case performance

**Cons:**
- ❌ Not stable
- ❌ Slower than quicksort in practice
- ❌ More complex to implement
- ❌ Poor cache performance

**Pitfalls:**
- ⚠️ Off-by-one in heap indices
- ⚠️ Forgetting to build heap first
- ⚠️ Incorrect heapify logic
- ⚠️ Stack overflow on large arrays

**When to use:**
- When worst-case O(n log n) is required
- When in-place sorting is needed
- When stability isn't required
- Priority queue applications

---

## Specialized Sorting Algorithms

### 8. Counting Sort

**For small integer ranges:**

```pascal
procedure CountingSort(var arr: array of integer; maxValue: integer);
var
  count: array of integer;
  output: array of integer;
  i: integer;
begin
  // Initialize count array
  SetLength(count, maxValue + 1);
  FillChar(count[0], Length(count) * SizeOf(integer), 0);
  
  // Count occurrences
  for i := 0 to Length(arr) - 1 do
    count[arr[i]] := count[arr[i]] + 1;
  
  // Cumulative count
  for i := 1 to maxValue do
    count[i] := count[i] + count[i - 1];
  
  // Build output array
  SetLength(output, Length(arr));
  for i := Length(arr) - 1 downto 0 do
  begin
    output[count[arr[i]] - 1] := arr[i];
    count[arr[i]] := count[arr[i]] - 1;
  end;
  
  // Copy back
  for i := 0 to Length(arr) - 1 do
    arr[i] := output[i];
end;
```

**Pros:**
- ✅ Very fast: O(n + k) where k is range
- ✅ Stable (preserves order)
- ✅ Linear time for small ranges

**Cons:**
- ❌ Only works for integers
- ❌ Requires known range
- ❌ Uses O(n + k) extra memory
- ❌ Not efficient for large ranges

**Pitfalls:**
- ⚠️ Array out of bounds if value > maxValue
- ⚠️ Forgetting to initialize count array
- ⚠️ Off-by-one in cumulative count
- ⚠️ Not handling negative numbers

**When to use:**
- Small integer ranges (0-255, 0-1000)
- When range is known
- When stability is needed
- As part of radix sort

---

## Algorithm Comparison

### Time Complexity

| Algorithm | Best | Average | Worst | Space |
|-----------|------|---------|-------|-------|
| Bubble Sort | O(n) | O(n²) | O(n²) | O(1) |
| Selection Sort | O(n²) | O(n²) | O(n²) | O(1) |
| Insertion Sort | O(n) | O(n²) | O(n²) | O(1) |
| Shell Sort | O(n log n) | O(n^1.5) | O(n²) | O(1) |
| Quicksort | O(n log n) | O(n log n) | O(n²) | O(log n) |
| Mergesort | O(n log n) | O(n log n) | O(n log n) | O(n) |
| Heapsort | O(n log n) | O(n log n) | O(n log n) | O(1) |
| Counting Sort | O(n + k) | O(n + k) | O(n + k) | O(n + k) |

### Stability

**Stable algorithms (preserve order of equal elements):**
- ✅ Bubble Sort
- ✅ Insertion Sort
- ✅ Mergesort
- ✅ Counting Sort

**Unstable algorithms:**
- ❌ Selection Sort
- ❌ Shell Sort
- ❌ Quicksort
- ❌ Heapsort

### When to Use Each

**Bubble Sort:**
- Educational, very small arrays, nearly sorted data

**Selection Sort:**
- Educational, small arrays, minimizing swaps

**Insertion Sort:**
- Small arrays, nearly sorted data, hybrid algorithms

**Shell Sort:**
- Medium arrays, in-place requirement, embedded systems

**Quicksort:**
- General-purpose, large arrays, average performance

**Mergesort:**
- Stability required, worst-case guarantee, external sorting

**Heapsort:**
- Worst-case guarantee, in-place requirement, priority queues

**Counting Sort:**
- Small integer ranges, known range, stability needed

---

## Common Pitfalls

### Pitfall 1: Off-by-One Errors

**Common mistake:**

```pascal
// ❌ BAD: Off-by-one
for i := 0 to Length(arr) do  // Should be Length(arr) - 1
  Process(arr[i]);

// ✅ GOOD: Correct bounds
for i := 0 to Length(arr) - 1 do
  Process(arr[i]);
```

### Pitfall 2: Forgetting Edge Cases

**Handle empty/single-element arrays:**

```pascal
// ✅ GOOD: Check edge cases
procedure SafeSort(var arr: array of integer);
begin
  if Length(arr) <= 1 then
    Exit;  // Already sorted
  
  QuickSort(arr, 0, Length(arr) - 1);
end;
```

### Pitfall 3: Stack Overflow

**Recursive algorithms can overflow:**

```pascal
// ❌ BAD: May overflow on large arrays
procedure QuickSort(var arr: array of integer; left, right: integer);
// Recursive version

// ✅ GOOD: Use iterative or limit recursion
procedure QuickSortIterative(var arr: array of integer);
// Iterative version with explicit stack
```

### Pitfall 4: Memory Leaks

**Free temporary arrays:**

```pascal
// ✅ GOOD: Clean up
procedure MergeSort(var arr: array of integer; left, right: integer);
var
  temp: array of integer;
begin
  // ... merge logic ...
  SetLength(temp, 0);  // Free memory
end;
```

---

## Best Practices

### 1. Choose the Right Algorithm

**Match algorithm to data:**

```pascal
// ✅ GOOD: Counting sort for small range
if (maxValue - minValue) < 1000 then
  CountingSort(arr, maxValue)
else
  QuickSort(arr, 0, Length(arr) - 1);

// ❌ BAD: Always use one algorithm
QuickSort(arr, 0, Length(arr) - 1);  // May be inefficient
```

### 2. Optimize for Your Use Case

**Consider your requirements:**

```pascal
// ✅ GOOD: Stable sort if order matters
if NeedStableSort then
  MergeSort(arr, 0, Length(arr) - 1)
else
  QuickSort(arr, 0, Length(arr) - 1);

// ❌ BAD: Ignore requirements
QuickSort(arr, 0, Length(arr) - 1);  // May break order
```

### 3. Test with Different Data

**Test various scenarios:**

```pascal
// ✅ GOOD: Test multiple cases
TestSort(RandomArray);
TestSort(SortedArray);
TestSort(ReverseSortedArray);
TestSort(NearlySortedArray);

// ❌ BAD: Only test one case
TestSort(RandomArray);  // May miss edge cases
```

### 4. Profile Performance

**Measure actual performance:**

```pascal
// ✅ GOOD: Benchmark
StartTimer;
SortAlgorithm(arr);
elapsed := GetElapsedTime;
WriteLn('Sort took: ', elapsed, 'ms');

// ❌ BAD: Assume performance
SortAlgorithm(arr);  // Don't know if it's fast
```

---

## Exercises

### Exercise 1: Implement All Sorts

Write a program that:
1. Implements all 8 sorting algorithms
2. Tests each with the same data
3. Compares performance
4. Analyzes when each is best

### Exercise 2: Sort Comparison

Write a program that:
1. Creates test arrays (random, sorted, reverse sorted)
2. Sorts with different algorithms
3. Measures time for each
4. Creates comparison table

### Exercise 3: Fix Pitfalls

Write a program that:
1. Demonstrates common pitfalls
2. Shows incorrect implementations
3. Fixes the issues
4. Explains the fixes

### Exercise 4: Hybrid Sort

Write a program that:
1. Combines multiple sorting algorithms
2. Uses insertion sort for small arrays
3. Uses quicksort for large arrays
4. Optimizes based on data characteristics

---

## Advanced Examples (University Level)

### Example 1: Adaptive Sorting with Performance Profiling

**Real-world scenario:** Implementing a production-grade sorting system that adapts to data characteristics and profiles its own performance.

```pascal
type
  TSortStrategy = (ssInsertion, ssQuick, ssMerge, ssHeap);
  TPerformanceMetrics = record
    Comparisons: longword;
    Swaps: longword;
    TimeMs: integer;
    Strategy: TSortStrategy;
  end;

function AdaptiveSort(var arr: array of integer): TPerformanceMetrics;
var
  metrics: TPerformanceMetrics;
  isNearlySorted: boolean;
  smallArray: boolean;
begin
  // Analyze data characteristics
  isNearlySorted := CalculateInversions(arr) < Length(arr) div 10;
  smallArray := Length(arr) < 50;
  
  // Choose strategy based on characteristics
  if smallArray then
    metrics.Strategy := ssInsertion
  else if isNearlySorted then
    metrics.Strategy := ssInsertion
  else
    metrics.Strategy := ssQuick;
  
  // Profile the sort
  metrics.Comparisons := 0;
  metrics.Swaps := 0;
  StartTimer;
  
  case metrics.Strategy of
    ssInsertion: InsertionSortProfiled(arr, metrics);
    ssQuick: QuickSortProfiled(arr, 0, Length(arr) - 1, metrics);
    ssMerge: MergeSortProfiled(arr, 0, Length(arr) - 1, metrics);
    ssHeap: HeapSortProfiled(arr, metrics);
  end;
  
  metrics.TimeMs := GetElapsedTime;
  AdaptiveSort := metrics;
end;
```

**Key concepts:**
- Performance profiling and measurement
- Adaptive algorithm selection
- Real-world optimization strategies
- Metrics collection and analysis

### Example 2: Parallel Sorting (Conceptual)

**Advanced concept:** Understanding how sorting could be parallelized (for future multi-threading support).

```pascal
// Conceptual: Parallel merge sort
// Note: SuperPascal is currently single-threaded, but this shows
// how sorting algorithms can be parallelized

procedure ParallelMergeSort(var arr: array of integer; 
                            left, right: integer; 
                            depth: integer);
var
  mid: integer;
begin
  if left < right then
  begin
    mid := (left + right) div 2;
    
    // Parallelize if depth allows (conceptual)
    if depth > 0 then
    begin
      // In parallel (conceptual):
      // ParallelMergeSort(arr, left, mid, depth - 1);
      // ParallelMergeSort(arr, mid + 1, right, depth - 1);
      // Wait for both to complete
      // Merge(arr, left, mid, right);
    end
    else
    begin
      // Sequential fallback
      MergeSort(arr, left, mid);
      MergeSort(arr, mid + 1, right);
      Merge(arr, left, mid, right);
    end;
  end;
end;
```

**Key concepts:**
- Divide-and-conquer parallelization
- Work distribution strategies
- Synchronization points
- Scalability considerations

### Example 3: Stable Sort Implementation with Custom Comparators

**Professional pattern:** Implementing stable sorting with flexible comparison functions.

```pascal
type
  TCompareFunc = function(a, b: integer): integer;

function StableMergeSort(var arr: array of integer; 
                         left, right: integer;
                         compare: TCompareFunc): integer;
var
  mid, i, j, k: integer;
  temp: array of integer;
begin
  if left >= right then
  begin
    StableMergeSort := 0;
    Exit;
  end;
  
  mid := (left + right) div 2;
  StableMergeSort := StableMergeSort(arr, left, mid, compare) +
                     StableMergeSort(arr, mid + 1, right, compare);
  
  // Merge with stability guarantee
  SetLength(temp, right - left + 1);
  i := left;
  j := mid + 1;
  k := 0;
  
  while (i <= mid) and (j <= right) do
  begin
    if compare(arr[i], arr[j]) <= 0 then
    begin
      temp[k] := arr[i];
      Inc(i);
    end
    else
    begin
      temp[k] := arr[j];
      Inc(j);
    end;
    Inc(k);
  end;
  
  // Copy remaining elements
  while i <= mid do
  begin
    temp[k] := arr[i];
    Inc(i);
    Inc(k);
  end;
  
  while j <= right do
  begin
    temp[k] := arr[j];
    Inc(j);
    Inc(k);
  end;
  
  // Copy back
  for i := 0 to k - 1 do
    arr[left + i] := temp[i];
end;
```

**Key concepts:**
- Stable sorting guarantees
- Function pointers / callbacks
- Generic algorithm design
- Professional code patterns

---

## Level-Specific Exercises

### GCSE Level Exercises

**Exercise 1: Simple Sort**
Write a program that sorts 10 numbers using bubble sort. Display the array before and after sorting.

**Exercise 2: Compare Two Sorts**
Implement bubble sort and insertion sort. Sort the same array with both and count how many comparisons each makes.

### A-Level Exercises

**Exercise 1: Sort Analysis**
Implement quicksort and mergesort. Test with different array sizes (10, 100, 1000 elements) and measure execution time. Create a comparison table.

**Exercise 2: Adaptive Sorting**
Write a function that chooses between insertion sort and quicksort based on array size. Test with various sizes and verify the choice is optimal.

### University Level Exercises

**Exercise 1: Performance Profiling System**
Implement a sorting framework that:
- Profiles multiple sorting algorithms
- Collects metrics (comparisons, swaps, time)
- Generates performance reports
- Identifies optimal algorithm for given data characteristics

**Exercise 2: Stable Sort with Custom Comparators**
Implement a stable merge sort that accepts a comparison function. Test with:
- Integer comparison (ascending/descending)
- Custom record comparison (by multiple fields)
- Verify stability is maintained

**Exercise 3: Hybrid Sort Implementation**
Design and implement a hybrid sorting algorithm (like Timsort) that:
- Uses insertion sort for small subarrays
- Uses merge sort for larger arrays
- Detects and optimizes for nearly-sorted data
- Benchmarks against standard algorithms

---

**Next Section:** [Searching Algorithms](./02_SearchingAlgorithms.md)  
**Last Updated:** 2025-01-XX

