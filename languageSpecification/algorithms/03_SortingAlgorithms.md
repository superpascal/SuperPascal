# Sorting Algorithms

**Part of:** [Algorithms Appendix](../99_Algorithms_Appendix.md)

---

## Overview

Sorting algorithms for organizing data efficiently. These algorithms are **generic** and work on **all platforms**.

**Source Material:** `docs/mikro_docs_archive/Coding/1/SORT_ALG.TXT`

**See Also:**
- Book Chapter: [Chapter 27: Advanced Algorithms](../../book/27_AdvancedAlgorithms/README.md)
- Standard Library: [Array Functions](../13_StandardLibrary.md#array-functions)

---

## Algorithm Comparison

| Algorithm | Average Time | Worst Time | Space | In-Place | Stable |
|-----------|--------------|------------|-------|----------|--------|
| **Quicksort** | O(n log n) | O(n²) | O(log n) | Yes | No |
| **Shellsort** | O(n^1.5) | O(n^1.5) | O(1) | Yes | No |
| **Mergesort** | O(n log n) | O(n log n) | O(n) | No | Yes |
| **Heapsort** | O(n log n) | O(n log n) | O(1) | Yes | No |

**When to Use:**
- **Quicksort:** General-purpose, fast average case
- **Shellsort:** Simple, good for small arrays
- **Mergesort:** Guaranteed O(n log n), stable
- **Heapsort:** Guaranteed O(n log n), in-place

---

## Quicksort

**Algorithm:** Divide and conquer using a pivot element

**Complexity:**
- **Average:** O(n log n)
- **Worst:** O(n²) when array is already sorted
- **Space:** O(log n) for recursion stack
- **In-Place:** Yes

**Algorithm:**
```pascal
procedure Quicksort(left, right: integer; var arr: array of integer);
var
  m, i, j: integer;
begin
  i := left;
  j := right;
  m := arr[(left + right) div 2];  // Pivot: middle element
  
  repeat
    while arr[i] < m do
      i := i + 1;
    while arr[j] > m do
      j := j - 1;
    
    if i <= j then
    begin
      Swap(arr[i], arr[j]);
      i := i + 1;
      j := j - 1;
    end;
  until i > j;
  
  if left < j then
    Quicksort(left, j, arr);
  if i < right then
    Quicksort(i, right, arr);
end;
```

**Helper Function:**
```pascal
procedure Swap(var a, b: integer);
var
  temp: integer;
begin
  temp := a;
  a := b;
  b := temp;
end;
```

**Usage:**
```pascal
var
  data: array[0..99] of integer;
begin
  // ... fill data array ...
  Quicksort(0, 99, data);
end;
```

**Performance Notes:**
- Very fast on average
- Worst case occurs when pivot is always the smallest or largest element
- Can be optimized by choosing median-of-three pivot
- For small arrays (n < 10), insertion sort may be faster

**Platform Considerations:**
- **Recursion depth:** May cause stack overflow on 8-bit systems for large arrays
- **Optimization:** Consider iterative version for embedded systems

---

## Shellsort

**Algorithm:** Improved insertion sort with decreasing gap sizes

**Complexity:**
- **Average:** O(n^1.5)
- **Worst:** O(n^1.5)
- **Space:** O(1)
- **In-Place:** Yes

**Algorithm:**
```pascal
procedure Shellsort(n: integer; var arr: array of integer);
var
  d, i, j: integer;
  flag: boolean;
begin
  d := n div 2;
  
  while d >= 1 do
  begin
    for i := 0 to n - d - 1 do
    begin
      j := i;
      flag := true;
      
      while flag do
      begin
        flag := false;
        if j >= 0 then
        begin
          if arr[j] > arr[j + d] then
          begin
            Swap(arr[j], arr[j + d]);
            flag := true;
            j := j - d;
          end;
        end;
      end;
    end;
    
    d := d div 2;
  end;
end;
```

**Usage:**
```pascal
var
  data: array[0..99] of integer;
begin
  // ... fill data array ...
  Shellsort(100, data);
end;
```

**Performance Notes:**
- Simple to implement
- Good for small to medium arrays
- Gap sequence affects performance (this implementation uses n/2, n/4, ...)
- Better gap sequences exist (e.g., Knuth's: 1, 4, 13, 40, 121, ...)

**Platform Considerations:**
- **No recursion:** Good for systems with limited stack
- **Simple:** Easy to optimize for specific platforms

---

## Mergesort

**Algorithm:** Divide array in half, sort each half, merge results

**Complexity:**
- **Average:** O(n log n)
- **Worst:** O(n log n)
- **Space:** O(n) for temporary array
- **In-Place:** No (requires temporary storage)
- **Stable:** Yes (preserves order of equal elements)

**Algorithm:**
```pascal
procedure Mergesort(left, right: integer; var arr: array of integer);
var
  middle: integer;
begin
  if left < right then
  begin
    middle := (left + right) div 2;
    Mergesort(left, middle, arr);
    Mergesort(middle + 1, right, arr);
    Merge(left, middle, middle + 1, right, arr);
  end;
end;

procedure Merge(left1, right1, left2, right2: integer; var arr: array of integer);
var
  temp: array of integer;
  i, j, k: integer;
begin
  // Allocate temporary array
  SetLength(temp, right2 - left1 + 1);
  
  i := left1;
  j := left2;
  k := 0;
  
  // Merge both halves
  while (i <= right1) and (j <= right2) do
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
  
  // Copy remaining elements from first half
  while i <= right1 do
  begin
    temp[k] := arr[i];
    i := i + 1;
    k := k + 1;
  end;
  
  // Copy remaining elements from second half
  while j <= right2 do
  begin
    temp[k] := arr[j];
    j := j + 1;
    k := k + 1;
  end;
  
  // Copy back to original array
  for i := 0 to k - 1 do
    arr[left1 + i] := temp[i];
end;
```

**Usage:**
```pascal
var
  data: array[0..99] of integer;
begin
  // ... fill data array ...
  Mergesort(0, 99, data);
end;
```

**Performance Notes:**
- Guaranteed O(n log n) performance
- Stable (preserves order of equal elements)
- Requires O(n) extra memory
- Good for linked lists (can be done in-place)

**Platform Considerations:**
- **Memory:** Requires temporary array (may be issue on memory-constrained systems)
- **Recursion:** Uses recursion (consider iterative version for embedded systems)

---

## Heapsort

**Algorithm:** Build heap, repeatedly extract maximum

**Complexity:**
- **Average:** O(n log n)
- **Worst:** O(n log n)
- **Space:** O(1)
- **In-Place:** Yes

**Algorithm:**
```pascal
procedure Heapsort(n: integer; var arr: array of integer);
var
  i, heapsize, max: integer;

  procedure FixHeap(lower, element, upper: integer);
  var
    child, maxChild: integer;
  begin
    // Reestablish the heap property in the tree
    // The element to insert at position 'lower' is 'element'
    // Last index in heap is 'upper'
    // The heap property means that the element at index 'lower' should
    // be the largest in the subtree it represents
    
    while lower * 2 + 1 <= upper do
    begin
      child := lower * 2 + 1;  // Left child
      
      // Find larger child
      if (child < upper) and (arr[child] < arr[child + 1]) then
        child := child + 1;
      
      // If element is larger than largest child, heap property satisfied
      if element >= arr[child] then
        break;
      
      // Move larger child up
      arr[lower] := arr[child];
      lower := child;
    end;
    
    arr[lower] := element;
  end;

begin
  // Heap construction: build max-heap from bottom up
  for i := (n div 2) - 1 downto 0 do
    FixHeap(i, arr[i], n - 1);
  
  // Rearrange: repeatedly extract maximum
  for heapsize := n - 1 downto 1 do
  begin
    max := arr[0];
    FixHeap(0, arr[heapsize], heapsize - 1);
    arr[heapsize] := max;
  end;
end;
```

**Usage:**
```pascal
var
  data: array[0..99] of integer;
begin
  // ... fill data array ...
  Heapsort(100, data);
end;
```

**Performance Notes:**
- Guaranteed O(n log n) performance
- In-place (no extra memory needed)
- Not stable (may change order of equal elements)
- Slower than quicksort in practice due to poor cache locality

**Platform Considerations:**
- **No recursion:** Good for systems with limited stack
- **In-place:** Good for memory-constrained systems
- **Cache:** Poor cache performance on modern CPUs

---

## Utility: Array Shuffling

**Algorithm:** Fisher-Yates shuffle for randomizing array order

**Complexity:**
- **Time:** O(n)
- **Space:** O(1)
- **In-Place:** Yes

**Algorithm:**
```pascal
procedure Shuffle(n: integer; var arr: array of integer);
var
  i, j: integer;
begin
  // Initialize array with sequential values
  for i := 0 to n - 1 do
    arr[i] := i;
  
  // Shuffle: swap each element with random element before it
  for i := n - 1 downto 1 do
  begin
    j := Random(i + 1);  // Random index from 0 to i
    Swap(arr[j], arr[i]);
  end;
end;
```

**Usage:**
```pascal
var
  data: array[0..99] of integer;
begin
  Shuffle(100, data);
  // data now contains values 0..99 in random order
end;
```

**Performance Notes:**
- O(n) time complexity
- Produces uniform random permutation
- Requires good random number generator

---

## Choosing the Right Algorithm

**For Small Arrays (n < 10):**
- Use **Insertion Sort** (simple, fast for small n)

**For Medium Arrays (10 < n < 1000):**
- Use **Quicksort** (fast average case)
- Or **Shellsort** (simple, no recursion)

**For Large Arrays (n > 1000):**
- Use **Quicksort** with median-of-three pivot
- Or **Mergesort** if stability is required
- Or **Heapsort** if memory is constrained

**For Embedded Systems:**
- Prefer **Shellsort** or **Heapsort** (no recursion)
- Avoid **Mergesort** (requires extra memory)

**For Stable Sorting:**
- Use **Mergesort** (only stable O(n log n) algorithm)

---

## Implementation Notes

**For Compiler Implementation:**
- Consider providing built-in sort functions in standard library
- Use platform-specific optimizations (e.g., inline Swap)
- Consider template/generic versions for different types

**For Runtime:**
- Sort functions should be in standard library
- Provide both generic and type-specific versions
- Document performance characteristics per platform

---

**Previous:** [Mathematical Algorithms](./02_MathematicalAlgorithms.md)  
**Next:** [Graphics Algorithms](./04_GraphicsAlgorithms.md)  
**Last Updated:** 2025-01-XX
