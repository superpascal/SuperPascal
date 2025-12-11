# Array Manipulation

**Part of:** [Chapter 06: Arrays and Records](./README.md)

---

## Introduction

**Array manipulation functions** make working with arrays easier and more efficient. They provide common operations like adding, removing, searching, and sorting elements.

**Why array manipulation matters:**
- **Efficiency** — Optimized implementations
- **Readability** — Clear, expressive code
- **Safety** — Bounds checking and error handling
- **Productivity** — Don't reinvent the wheel

**This section covers:**
- Stack operations (Push, Pop, LPOP, RPOP)
- Insert and delete operations
- Search functions (IndexOf, Contains)
- Sorting and reversing
- Slicing and copying
- Utility functions (Fill, Clear, Count)

---

## Stack Operations

### Push (Add to End)

**Add element to end of array:**
```pascal
procedure Push(var arr: array of integer; var count: integer; value: integer);
begin
  if count < High(arr) - Low(arr) + 1 then
  begin
    arr[Low(arr) + count] := value;
    count := count + 1;
  end
  else
    WriteLn('Error: Array full');
end;
```

**Example:**
```pascal
var stack: array[0..9] of integer;
var count: integer;
begin
  count := 0;
  Push(stack, count, 10);
  Push(stack, count, 20);
  Push(stack, count, 30);
  // stack now contains [10, 20, 30], count = 3
end.
```

### Pop (Remove from End)

**Remove and return element from end:**
```pascal
function Pop(var arr: array of integer; var count: integer): integer;
begin
  if count > 0 then
  begin
    count := count - 1;
    Pop := arr[Low(arr) + count];
  end
  else
  begin
    WriteLn('Error: Array empty');
    Pop := 0;
  end;
end;
```

**Example:**
```pascal
var stack: array[0..9] of integer;
var count, value: integer;
begin
  count := 3;
  stack[0] := 10;
  stack[1] := 20;
  stack[2] := 30;
  
  value := Pop(stack, count);  // value = 30, count = 2
end.
```

### RPOP (Right Pop - Remove from End)

**RPOP is the same as Pop** — Remove from right/end:
```pascal
function RPOP(var arr: array of integer; var count: integer): integer;
begin
  RPOP := Pop(arr, count);  // Same as Pop
end;
```

**Naming:**
- **Pop** — Traditional name
- **RPOP** — Explicit "right pop" (removes from end)
- **Both are identical** — Use whichever you prefer

### LPOP (Left Pop - Remove from Beginning)

**Remove and return element from beginning:**
```pascal
function LPOP(var arr: array of integer; var count: integer): integer;
var i: integer;
begin
  if count > 0 then
  begin
    LPOP := arr[Low(arr)];  // Get first element
    
    // Shift all elements left
    for i := Low(arr) to Low(arr) + count - 2 do
      arr[i] := arr[i + 1];
    
    count := count - 1;
  end
  else
  begin
    WriteLn('Error: Array empty');
    LPOP := 0;
  end;
end;
```

**Example:**
```pascal
var queue: array[0..9] of integer;
var count, value: integer;
begin
  count := 3;
  queue[0] := 10;
  queue[1] := 20;
  queue[2] := 30;
  
  value := LPOP(queue, count);  // value = 10, count = 2
  // queue now contains [20, 30]
end.
```

**Performance note:** LPOP is O(n) because it shifts all elements. Use RPOP when possible for better performance.

---

## Insert and Delete Operations

### Insert at Index

**Insert element at specific index:**
```pascal
procedure InsertAt(var arr: array of integer; var count: integer;
                  index: integer; value: integer);
var i: integer;
begin
  if (index >= 0) and (index <= count) and 
     (count < High(arr) - Low(arr) + 1) then
  begin
    // Shift elements right
    for i := count downto index + 1 do
      arr[Low(arr) + i] := arr[Low(arr) + i - 1];
    
    // Insert new element
    arr[Low(arr) + index] := value;
    count := count + 1;
  end
  else
    WriteLn('Error: Invalid index or array full');
end;
```

**Example:**
```pascal
var arr: array[0..9] of integer;
var count: integer;
begin
  count := 3;
  arr[0] := 10;
  arr[1] := 20;
  arr[2] := 30;
  
  InsertAt(arr, count, 1, 15);  // Insert 15 at index 1
  // arr now contains [10, 15, 20, 30], count = 4
end.
```

### Delete at Index

**Remove element at specific index:**
```pascal
procedure DeleteAt(var arr: array of integer; var count: integer;
                  index: integer);
var i: integer;
begin
  if (index >= 0) and (index < count) then
  begin
    // Shift elements left
    for i := index to count - 2 do
      arr[Low(arr) + i] := arr[Low(arr) + i + 1];
    
    count := count - 1;
  end
  else
    WriteLn('Error: Invalid index');
end;
```

**Example:**
```pascal
var arr: array[0..9] of integer;
var count: integer;
begin
  count := 4;
  arr[0] := 10;
  arr[1] := 15;
  arr[2] := 20;
  arr[3] := 30;
  
  DeleteAt(arr, count, 1);  // Delete element at index 1
  // arr now contains [10, 20, 30], count = 3
end.
```

### Remove Value

**Find and remove first occurrence of value:**
```pascal
function RemoveValue(var arr: array of integer; var count: integer;
                    value: integer): boolean;
var index: integer;
begin
  index := IndexOf(arr, count, value);
  if index >= 0 then
  begin
    DeleteAt(arr, count, index);
    RemoveValue := true;
  end
  else
    RemoveValue := false;
end;
```

---

## Search Functions

### IndexOf

**Find index of first occurrence:**
```pascal
function IndexOf(const arr: array of integer; count: integer;
                value: integer): integer;
var i: integer;
begin
  for i := 0 to count - 1 do
    if arr[Low(arr) + i] = value then
    begin
      IndexOf := i;
      Exit;
    end;
  IndexOf := -1;  // Not found
end;
```

**Example:**
```pascal
var arr: array[0..9] of integer;
var count, index: integer;
begin
  count := 4;
  arr[0] := 10;
  arr[1] := 20;
  arr[2] := 30;
  arr[3] := 20;
  
  index := IndexOf(arr, count, 20);  // index = 1 (first occurrence)
  index := IndexOf(arr, count, 99);  // index = -1 (not found)
  
  // Find index (returns -1 if not found)
  index := Find(arr, count, 20);  // index = 1
  index := Find(arr, count, 99);  // index = -1
  
  // Find is an alias for IndexOf()
  index := IndexOf(arr, count, 20);  // Same as Find()
  
  // Python-like membership test (SuperPascal extension)
  if 20 in arr[:count] then
    WriteLn('20 is in array');
  
  // Equivalent to IndexOf() >= 0
  if IndexOf(arr, count, 20) >= 0 then
    WriteLn('20 is in array');
end.
```

### LastIndexOf

**Find index of last occurrence:**
```pascal
function LastIndexOf(const arr: array of integer; count: integer;
                    value: integer): integer;
var i: integer;
begin
  for i := count - 1 downto 0 do
    if arr[Low(arr) + i] = value then
    begin
      LastIndexOf := i;
      Exit;
    end;
  LastIndexOf := -1;  // Not found
end;
```

### Contains

**Check if array contains value:**
```pascal
function Contains(const arr: array of integer; count: integer;
                 value: integer): boolean;
begin
  Contains := IndexOf(arr, count, value) >= 0;
end;
```

**Example:**
```pascal
var arr: array[0..9] of integer;
var count: integer;
var found: boolean;
begin
  count := 3;
  arr[0] := 10;
  arr[1] := 20;
  arr[2] := 30;
  
  found := Contains(arr, count, 20);  // true
  found := Contains(arr, count, 99);  // false
  
  // Python-like membership test (SuperPascal extension)
  if 20 in arr[:count] then
    WriteLn('20 is in array');
  
  // Equivalent to Contains()
  if Contains(arr, count, 20) then
    WriteLn('20 is in array');
end.
```

### Count

**Count occurrences of value:**
```pascal
function Count(const arr: array of integer; count: integer;
              value: integer): integer;
var i, result: integer;
begin
  result := 0;
  for i := 0 to count - 1 do
    if arr[Low(arr) + i] = value then
      result := result + 1;
  Count := result;
end;
```

---

## Sorting and Reversing

### Sort (Bubble Sort - Simple)

**Sort array in ascending order:**
```pascal
procedure Sort(var arr: array of integer; count: integer);
var i, j: integer;
var temp: integer;
begin
  // Bubble sort (simple but not most efficient)
  for i := 0 to count - 2 do
    for j := 0 to count - 2 - i do
      if arr[Low(arr) + j] > arr[Low(arr) + j + 1] then
      begin
        // Swap
        temp := arr[Low(arr) + j];
        arr[Low(arr) + j] := arr[Low(arr) + j + 1];
        arr[Low(arr) + j + 1] := temp;
      end;
end;
```

**Example:**
```pascal
var arr: array[0..9] of integer;
var count: integer;
begin
  count := 5;
  arr[0] := 30;
  arr[1] := 10;
  arr[2] := 50;
  arr[3] := 20;
  arr[4] := 40;
  
  Sort(arr, count);
  // arr now contains [10, 20, 30, 40, 50]
end.
```

### SortDescending

**Sort array in descending order:**
```pascal
procedure SortDescending(var arr: array of integer; count: integer);
var i, j: integer;
var temp: integer;
begin
  for i := 0 to count - 2 do
    for j := 0 to count - 2 - i do
      if arr[Low(arr) + j] < arr[Low(arr) + j + 1] then
      begin
        temp := arr[Low(arr) + j];
        arr[Low(arr) + j] := arr[Low(arr) + j + 1];
        arr[Low(arr) + j + 1] := temp;
      end;
end;
```

### Reverse

**Reverse array order:**
```pascal
procedure Reverse(var arr: array of integer; count: integer);
var i, temp: integer;
begin
  for i := 0 to (count div 2) - 1 do
  begin
    temp := arr[Low(arr) + i];
    arr[Low(arr) + i] := arr[Low(arr) + count - 1 - i];
    arr[Low(arr) + count - 1 - i] := temp;
  end;
end;
```

**Example:**
```pascal
var arr: array[0..9] of integer;
var count: integer;
begin
  count := 4;
  arr[0] := 10;
  arr[1] := 20;
  arr[2] := 30;
  arr[3] := 40;
  
  Reverse(arr, count);
  // arr now contains [40, 30, 20, 10]
end.
```

---

## Array Slicing

### Slice Syntax (Python-style)

**SuperPascal supports Python-style array slicing with Pascal syntax:**

```pascal
arr[start:end]      // Slice from start to end (end exclusive)
arr[start:end:step] // Slice with step
arr[:end]           // From beginning to end
arr[start:]         // From start to end
arr[:]              // Copy entire array
```

**Syntax:**
- **`arr[start:end]`** — Elements from `start` to `end-1` (end is exclusive, like Python)
- **`arr[start:end:step]`** — Elements from `start` to `end-1` with step
- **`arr[:end]`** — Elements from beginning (index 0) to `end-1`
- **`arr[start:]`** — Elements from `start` to end of array
- **`arr[:]`** — All elements (copy entire array)

**Returns:** New array with sliced elements

**Example:**
```pascal
var arr: array[0..9] of integer;
var slice: array[0..9] of integer;
var i: integer;
begin
  // Initialize array
  for i := 0 to 9 do
    arr[i] := i * 10;  // [0, 10, 20, 30, 40, 50, 60, 70, 80, 90]
  
  // Slice from index 1 to 5 (exclusive)
  slice := arr[1:5];  // [10, 20, 30, 40]
  
  // Slice from beginning to 5
  slice := arr[:5];  // [0, 10, 20, 30, 40]
  
  // Slice from 3 to end
  slice := arr[3:];  // [30, 40, 50, 60, 70, 80, 90]
  
  // Slice with step 2
  slice := arr[0:10:2];  // [0, 20, 40, 60, 80]
  
  // Copy entire array
  slice := arr[:];  // [0, 10, 20, 30, 40, 50, 60, 70, 80, 90]
end.
```

### Slice Behavior

**Bounds handling:**
- **Negative indices** — Count from end (like Python)
  - `arr[-1]` — Last element
  - `arr[-2:]` — Last two elements
- **Out of bounds** — Clamped to valid range
  - `arr[5:20]` on array[0..9] → `arr[5:10]` (clamped)
- **Empty slice** — Returns empty array if start >= end

**Step behavior:**
- **Positive step** — Forward iteration
- **Negative step** — Reverse iteration (reverses array)
- **Step = 0** — Error (invalid)

**Examples with negative indices:**
```pascal
var arr: array[0..9] of integer;
var slice: array[0..9] of integer;
begin
  // Initialize
  for i := 0 to 9 do
    arr[i] := i;
  
  // Last 3 elements
  slice := arr[-3:];  // [7, 8, 9]
  
  // All but last 2
  slice := arr[:-2];  // [0, 1, 2, 3, 4, 5, 6, 7]
  
  // Reverse array
  slice := arr[::-1];  // [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
end.
```

### Slice Assignment

**Assign to slice (modifies original array):**
```pascal
var arr: array[0..9] of integer;
begin
  // Initialize
  for i := 0 to 9 do
    arr[i] := i;
  
  // Replace slice with new values
  arr[2:5] := [20, 30, 40];  // arr[2..4] = [20, 30, 40]
  
  // Result: [0, 1, 20, 30, 40, 5, 6, 7, 8, 9]
end.
```

**Note:** Slice assignment requires source array to have same length as slice.

### Slice in Expressions

**Use slices in expressions:**
```pascal
var arr1, arr2: array[0..9] of integer;
var result: array[0..9] of integer;
begin
  // Concatenate slices
  result := arr1[0:5] + arr2[5:10];  // First half of arr1, second half of arr2
  
  // Compare slices
  if arr1[0:5] = arr2[0:5] then
    WriteLn('First halves match');
end.
```

### Performance

**Slice operations:**
- **O(n)** — Linear time (where n is slice length)
- **Creates new array** — Slicing allocates new array
- **Memory efficient** — Compiler optimizes when possible

---

## Slicing and Copying (Function-based)

### Slice Function

**Extract subarray using function (alternative to slice syntax):**
```pascal
procedure Slice(const src: array of integer; srcCount: integer;
               start, length: integer;
               var dest: array of integer; var destCount: integer);
var i: integer;
begin
  destCount := 0;
  if (start >= 0) and (start < srcCount) and (length > 0) then
  begin
    for i := 0 to length - 1 do
    begin
      if (start + i < srcCount) and 
         (destCount < High(dest) - Low(dest) + 1) then
      begin
        dest[Low(dest) + destCount] := src[Low(src) + start + i];
        destCount := destCount + 1;
      end;
    end;
  end;
end;
```

**Example:**
```pascal
var src: array[0..9] of integer;
var dest: array[0..9] of integer;
var srcCount, destCount: integer;
begin
  srcCount := 5;
  src[0] := 10;
  src[1] := 20;
  src[2] := 30;
  src[3] := 40;
  src[4] := 50;
  
  Slice(src, srcCount, 1, 3, dest, destCount);
  // dest now contains [20, 30, 40], destCount = 3
  
  // Equivalent to: dest := src[1:4];
end.
```

### Copy Array

**Copy entire array:**
```pascal
procedure CopyArray(const src: array of integer; srcCount: integer;
                   var dest: array of integer; var destCount: integer);
begin
  Slice(src, srcCount, 0, srcCount, dest, destCount);
end;
```

**Or use slice syntax:**
```pascal
dest := src[:];  // Copy entire array
```

---

## Array Concatenation

### Concat (Combine Arrays)

**Concatenate two arrays into one:**
```pascal
procedure Concat(const arr1: array of integer; count1: integer;
                const arr2: array of integer; count2: integer;
                var result: array of integer; var resultCount: integer);
var i: integer;
var maxSize: integer;
begin
  resultCount := 0;
  maxSize := High(result) - Low(result) + 1;
  
  // Copy first array
  for i := 0 to count1 - 1 do
  begin
    if resultCount < maxSize then
    begin
      result[Low(result) + resultCount] := arr1[Low(arr1) + i];
      resultCount := resultCount + 1;
    end;
  end;
  
  // Copy second array
  for i := 0 to count2 - 1 do
  begin
    if resultCount < maxSize then
    begin
      result[Low(result) + resultCount] := arr2[Low(arr2) + i];
      resultCount := resultCount + 1;
    end;
  end;
end;
```

**Example:**
```pascal
var arr1: array[0..4] of integer;
var arr2: array[0..4] of integer;
var result: array[0..9] of integer;
var count1, count2, resultCount: integer;
begin
  count1 := 3;
  arr1[0] := 10;
  arr1[1] := 20;
  arr1[2] := 30;
  
  count2 := 2;
  arr2[0] := 40;
  arr2[1] := 50;
  
  Concat(arr1, count1, arr2, count2, result, resultCount);
  // result now contains [10, 20, 30, 40, 50], resultCount = 5
end.
```

### Concat Operator (Syntax Extension)

**Use `+` operator for concatenation (SuperPascal extension):**
```pascal
var arr1, arr2, result: array[0..9] of integer;
var count1, count2, resultCount: integer;
begin
  count1 := 3;
  count2 := 2;
  
  // Initialize arrays...
  
  // Concatenate using + operator
  result := arr1[:count1] + arr2[:count2];
  // Equivalent to: Concat(arr1, count1, arr2, count2, result, resultCount);
end.
```

**Multiple concatenation:**
```pascal
var arr1, arr2, arr3, result: array[0..9] of integer;
begin
  result := arr1[:count1] + arr2[:count2] + arr3[:count3];
end.
```

---

## Array Growth and Resizing

### Grow (Copy to Larger Array)

**Copy array to a larger array (growth):**
```pascal
procedure Grow(const src: array of integer; srcCount: integer;
              var dest: array of integer; var destCount: integer);
begin
  // Copy all elements from source to destination
  CopyArray(src, srcCount, dest, destCount);
  
  // Note: dest must be larger than src for this to be "growth"
  // If dest is same size or smaller, it's just a copy
end;
```

**Example: Growing an array:**
```pascal
var small: array[0..4] of integer;
var large: array[0..9] of integer;
var smallCount, largeCount: integer;
begin
  smallCount := 5;
  // Fill small array...
  
  // Grow to larger array
  Grow(small, smallCount, large, largeCount);
  // large now contains all elements from small
  // largeCount = smallCount (5)
  
  // Now can add more elements to large
  Push(large, largeCount, 100);
  Push(large, largeCount, 200);
  // largeCount is now 7
end.
```

### Resize (Copy with New Size)

**Resize array by copying to new array:**
```pascal
procedure Resize(const src: array of integer; srcCount: integer;
                var dest: array of integer; var destCount: integer;
                newSize: integer);
var copyCount: integer;
begin
  destCount := 0;
  
  // Determine how many elements to copy
  if newSize > srcCount then
    copyCount := srcCount  // Growing: copy all
  else
    copyCount := newSize;  // Shrinking: copy only what fits
  
  // Copy elements
  CopyArray(src, copyCount, dest, destCount);
  
  // If growing, new elements are uninitialized (or zero)
  // If shrinking, excess elements are lost
end;
```

### Memory Considerations

**Array growth requires memory copying:**
```pascal
// Original array (small)
var arr: array[0..4] of integer;
var count: integer;

// Need to grow? Copy to larger array
var larger: array[0..9] of integer;
var largerCount: integer;

// Copy all elements (memory operation)
Grow(arr, count, larger, largerCount);

// Old array can be discarded (if on stack, automatically freed)
// New array is now the active one
```

**Performance:**
- **O(n)** — Linear time (where n is array size)
- **Memory copy** — All elements are copied
- **Stack allocation** — Both arrays on stack (if local variables)
- **No heap** — Fixed-size arrays don't use heap

### Dynamic Array Pattern

**Pattern for "growing" arrays:**
```pascal
procedure GrowArray(var current: array of integer; var currentCount: integer;
                   var next: array of integer; var nextCount: integer);
begin
  // Copy current to next (larger array)
  Grow(current, currentCount, next, nextCount);
  
  // Swap: next becomes current
  // (In practice, you'd swap the arrays)
end;
```

**Example with multiple growth stages:**
```pascal
var arr1: array[0..4] of integer;   // Stage 1: 5 elements
var arr2: array[0..9] of integer;   // Stage 2: 10 elements
var arr3: array[0..19] of integer;  // Stage 3: 20 elements
var count1, count2, count3: integer;

begin
  count1 := 5;
  // Fill arr1...
  
  // Grow to stage 2
  Grow(arr1, count1, arr2, count2);
  
  // Add more elements to arr2
  Push(arr2, count2, 100);
  Push(arr2, count2, 200);
  // count2 is now 7
  
  // Grow to stage 3
  Grow(arr2, count2, arr3, count3);
  
  // Continue with arr3...
end.
```

### Efficient Growth Strategy

**Double the size when growing (common pattern):**
```pascal
procedure GrowDouble(const src: array of integer; srcCount: integer;
                    var dest: array of integer; var destCount: integer);
begin
  // Dest should be at least 2x the size of src
  Grow(src, srcCount, dest, destCount);
end;
```

**Example:**
```pascal
var arr1: array[0..4] of integer;   // 5 elements
var arr2: array[0..9] of integer;   // 10 elements (2x)
var arr3: array[0..19] of integer;  // 20 elements (2x again)
```

**Why double?**
- **Amortized O(1)** — Average cost per insertion is constant
- **Fewer copies** — Less frequent growth operations
- **Balance** — Not too much wasted space, not too many copies

---

## Complete Function Reference

### Concatenation

| Function/Syntax | Description |
|-----------------|-------------|
| `Concat(arr1, count1, arr2, count2, result, resultCount)` | Concatenate two arrays |
| `arr1 + arr2` | Concatenate using `+` operator (SuperPascal extension) |
| `arr1[:count1] + arr2[:count2]` | Concatenate slices |

### Growth and Resizing

| Function | Signature | Description |
|----------|-----------|-------------|
| `Grow` | `Grow(const src; srcCount; var dest; var destCount)` | Copy to larger array |
| `Resize` | `Resize(const src; srcCount; var dest; var destCount; newSize)` | Resize array (grow or shrink) |

**Performance:**
- **Concat** — O(n + m) where n, m are array sizes
- **Grow/Resize** — O(n) where n is source array size
- **Memory** — Creates new array, copies all elements

---

## Array Length Functions

### Length (Array Size)

**Get the number of elements in an array:**
```pascal
function Length(const arr: array of T): integer;
```

**Returns:** Number of elements in the array (High - Low + 1)

**Example:**
```pascal
var arr: array[0..9] of integer;
var size: integer;
begin
  size := Length(arr);  // size = 10
  WriteLn('Array size: ', size);
end.
```

**For multi-dimensional arrays:**
```pascal
var matrix: array[0..3, 0..4] of integer;
var rows, cols: integer;
begin
  rows := Length(matrix);        // First dimension: 4
  cols := Length(matrix[0]);     // Second dimension: 5
  WriteLn('Matrix: ', rows, 'x', cols);
end.
```

### High (Upper Bound)

**Get the upper bound of an array:**
```pascal
function High(const arr: array of T): integer;
```

**Returns:** Highest valid index

**Example:**
```pascal
var arr: array[0..9] of integer;
var upper: integer;
begin
  upper := High(arr);  // upper = 9
  WriteLn('Upper bound: ', upper);
end.
```

**For custom ranges:**
```pascal
var arr: array[5..14] of integer;
var upper: integer;
begin
  upper := High(arr);  // upper = 14
end.
```

### Low (Lower Bound)

**Get the lower bound of an array:**
```pascal
function Low(const arr: array of T): integer;
```

**Returns:** Lowest valid index

**Example:**
```pascal
var arr: array[0..9] of integer;
var lower: integer;
begin
  lower := Low(arr);  // lower = 0
  WriteLn('Lower bound: ', lower);
end.
```

**For custom ranges:**
```pascal
var arr: array[5..14] of integer;
var lower: integer;
begin
  lower := Low(arr);  // lower = 5
end.
```

### Relationship

**Length = High - Low + 1:**
```pascal
var arr: array[0..9] of integer;
var len, lowBound, highBound: integer;
begin
  lowBound := Low(arr);   // 0
  highBound := High(arr); // 9
  len := Length(arr);     // 10
  
  // Always true: Length = High - Low + 1
  WriteLn('Length: ', len);
  WriteLn('High - Low + 1: ', highBound - lowBound + 1);
end.
```

### Common Usage Patterns

**Iterate over array:**
```pascal
var arr: array[0..9] of integer;
var i: integer;
begin
  // Method 1: Using Length
  for i := 0 to Length(arr) - 1 do
    arr[i] := i;
  
  // Method 2: Using Low and High (more Pascal-like)
  for i := Low(arr) to High(arr) do
    arr[i] := i;
end.
```

**Check if index is valid:**
```pascal
function IsValidIndex(const arr: array of integer; index: integer): boolean;
begin
  IsValidIndex := (index >= Low(arr)) and (index <= High(arr));
end;
```

**Get array size in bytes:**
```pascal
function ArraySize(const arr: array of integer): integer;
begin
  ArraySize := Length(arr) * SizeOf(integer);
end;
```

---

## Utility Functions

### Fill

**Set all elements to value:**
```pascal
procedure Fill(var arr: array of integer; count: integer; value: integer);
var i: integer;
begin
  for i := 0 to count - 1 do
    arr[Low(arr) + i] := value;
end;
```

**Example:**
```pascal
var arr: array[0..9] of integer;
var count: integer;
begin
  count := 5;
  Fill(arr, count, 0);  // Set all to 0
end.
```

### Clear

**Reset array (set count to 0):**
```pascal
procedure Clear(var count: integer);
begin
  count := 0;
end;
```

**Note:** Clear doesn't modify array elements, just resets the count. Use Fill if you need to zero elements.

### IsEmpty

**Check if array is empty:**
```pascal
function IsEmpty(count: integer): boolean;
begin
  IsEmpty := count = 0;
end;
```

### IsFull

**Check if array is full:**
```pascal
function IsFull(count: integer; maxSize: integer): boolean;
begin
  IsFull := count >= maxSize;
end;
```

---

## Complete Function Reference

### Stack Operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `Push` | `Push(var arr; var count; value)` | Add to end |
| `Pop` | `Pop(var arr; var count): T` | Remove from end |
| `RPOP` | `RPOP(var arr; var count): T` | Remove from end (alias) |
| `LPOP` | `LPOP(var arr; var count): T` | Remove from beginning |

### Insert/Delete

| Function | Signature | Description |
|----------|-----------|-------------|
| `InsertAt` | `InsertAt(var arr; var count; index; value)` | Insert at index |
| `DeleteAt` | `DeleteAt(var arr; var count; index)` | Delete at index |
| `RemoveValue` | `RemoveValue(var arr; var count; value): boolean` | Remove first occurrence |

### Search

| Function | Signature | Description |
|----------|-----------|-------------|
| `IndexOf` | `IndexOf(const arr; count; value): integer` | Find index (-1 if not found) |
| `LastIndexOf` | `LastIndexOf(const arr; count; value): integer` | Find last index |
| `Contains` | `Contains(const arr; count; value): boolean` | Check if contains |
| `Count` | `Count(const arr; count; value): integer` | Count occurrences |

### Sorting

| Function | Signature | Description |
|----------|-----------|-------------|
| `Sort` | `Sort(var arr; count)` | Sort ascending |
| `SortDescending` | `SortDescending(var arr; count)` | Sort descending |
| `Reverse` | `Reverse(var arr; count)` | Reverse order |

### Slicing

| Syntax/Function | Description |
|-----------------|-------------|
| `arr[start:end]` | Slice from start to end (end exclusive) |
| `arr[start:end:step]` | Slice with step |
| `arr[:end]` | From beginning to end |
| `arr[start:]` | From start to end |
| `arr[:]` | Copy entire array |
| `arr[-n:]` | Last n elements (negative index) |
| `arr[:-n]` | All but last n elements |
| `arr[::-1]` | Reverse array |
| `Slice()` | Function-based slicing (alternative) |
| `CopyArray()` | Function-based copying (alternative) |

### Concatenation

| Function/Syntax | Description |
|-----------------|-------------|
| `Concat(arr1, count1, arr2, count2, result, resultCount)` | Concatenate two arrays |
| `arr1 + arr2` | Concatenate using `+` operator (SuperPascal extension) |
| `arr1[:count1] + arr2[:count2]` | Concatenate slices |

### Growth and Resizing

| Function | Signature | Description |
|----------|-----------|-------------|
| `Grow` | `Grow(const src; srcCount; var dest; var destCount)` | Copy to larger array |
| `Resize` | `Resize(const src; srcCount; var dest; var destCount; newSize)` | Resize array (grow or shrink) |

### Array Length

| Function | Signature | Description |
|----------|-----------|-------------|
| `Length` | `Length(const arr: array of T): integer` | Get number of elements |
| `High` | `High(const arr: array of T): integer` | Get upper bound (highest index) |
| `Low` | `Low(const arr: array of T): integer` | Get lower bound (lowest index) |

**Note:** `Length(arr) = High(arr) - Low(arr) + 1`

### Utilities

| Function | Signature | Description |
|----------|-----------|-------------|
| `Fill` | `Fill(var arr; count; value)` | Set all to value |
| `Clear` | `Clear(var count)` | Reset count to 0 |
| `IsEmpty` | `IsEmpty(count): boolean` | Check if empty |
| `IsFull` | `IsFull(count; maxSize): boolean` | Check if full |

---

## Common Patterns

### Pattern 1: Stack

**Use Push/Pop for stack:**
```pascal
var stack: array[0..9] of integer;
var count: integer;
begin
  count := 0;
  
  // Push elements
  Push(stack, count, 10);
  Push(stack, count, 20);
  Push(stack, count, 30);
  
  // Pop elements (LIFO - Last In First Out)
  while count > 0 do
    WriteLn(Pop(stack, count));
  // Output: 30, 20, 10
end.
```

### Pattern 2: Queue

**Use Push/LPOP for queue:**
```pascal
var queue: array[0..9] of integer;
var count: integer;
begin
  count := 0;
  
  // Enqueue (add to end)
  Push(queue, count, 10);
  Push(queue, count, 20);
  Push(queue, count, 30);
  
  // Dequeue (remove from beginning)
  while count > 0 do
    WriteLn(LPOP(queue, count));
  // Output: 10, 20, 30 (FIFO - First In First Out)
end.
```

### Pattern 3: Dynamic Array

**Use count to track array size:**
```pascal
var arr: array[0..99] of integer;
var count: integer;
begin
  count := 0;
  
  // Add elements
  Push(arr, count, 10);
  Push(arr, count, 20);
  Push(arr, count, 30);
  
  // Process array
  var i: integer;
  for i := 0 to count - 1 do
    WriteLn(arr[i]);
end.
```

---

## Best Practices

### 1. Always Track Count

**Use count variable to track array size:**
```pascal
// Good: Track count
var arr: array[0..9] of integer;
var count: integer;
count := 0;
Push(arr, count, 10);

// Bad: Don't assume array size
// var arr: array[0..9] of integer;
// arr[0] := 10;  // What if array isn't empty?
```

### 2. Check Bounds

**Always check bounds before operations:**
```pascal
if not IsFull(count, 10) then
  Push(arr, count, value)
else
  WriteLn('Array full');
```

### 3. Use Appropriate Operations

**Choose the right function:**
```pascal
// Stack: Use Push/Pop
Push(stack, count, value);
value := Pop(stack, count);

// Queue: Use Push/LPOP
Push(queue, count, value);
value := LPOP(queue, count);

// Insert: Use InsertAt
InsertAt(arr, count, index, value);
```

### 4. Consider Performance

**Performance characteristics:**
- **Push/Pop/RPOP** — O(1) fast
- **LPOP** — O(n) slow (shifts elements)
- **InsertAt/DeleteAt** — O(n) slow (shifts elements)
- **IndexOf** — O(n) linear search
- **Sort** — O(n²) bubble sort (simple but slow)

**For large arrays, consider:**
- Using RPOP instead of LPOP when possible
- Using sorted arrays with binary search
- Using more efficient sort algorithms

---

## Platform Considerations

### Fixed-Size Arrays

**SuperPascal arrays are fixed-size:**
- **Size determined at compile time**
- **Cannot resize** at runtime
- **Use count** to track used portion
- **Maximum size** limited by array declaration

### Memory

**Array operations:**
- **No heap allocation** — All operations use stack
- **Efficient** — Direct memory access
- **Cache-friendly** — Contiguous memory layout

### Performance

**Operation costs:**
- **O(1)** — Push, Pop, RPOP, array access
- **O(n)** — LPOP, InsertAt, DeleteAt, IndexOf, Reverse
- **O(n²)** — Sort (bubble sort)

**Optimization tips:**
- Cache array bounds
- Use RPOP instead of LPOP when possible
- Consider lookup tables for frequent searches

---

## Summary

**Key Concepts:**
- **Stack operations** — Push, Pop, RPOP (end of array)
- **Queue operations** — Push, LPOP (beginning of array)
- **Insert/Delete** — InsertAt, DeleteAt, RemoveValue
- **Search** — IndexOf, LastIndexOf, Contains, Count
- **Sorting** — Sort, SortDescending, Reverse
- **Slicing** — Slice, CopyArray, slice syntax (`arr[start:end]`)
- **Concatenation** — Concat, `+` operator (`arr1 + arr2`)
- **Growth** — Grow, Resize (memory copying to larger arrays)
- **Utilities** — Fill, Clear, IsEmpty, IsFull

**Stack vs Queue:**
- **Stack (LIFO)** — Push/Pop — Last In First Out
- **Queue (FIFO)** — Push/LPOP — First In First Out

**Performance:**
- **Fast (O(1))** — Push, Pop, RPOP
- **Slow (O(n))** — LPOP, InsertAt, DeleteAt, Concat, Grow, Resize
- **Very Slow (O(n²))** — Sort

**Best Practices:**
- Always track count
- Check bounds before operations
- Use appropriate operations for your needs
- Consider performance for large arrays

**Next:** Learn about multi-dimensional arrays and matrices.

---

**Next Section:** [Multi-Dimensional Arrays](./03_MultiDimensionalArrays.md)  
**Also See:** [Lists and Linked Lists](./05_ListsAndLinkedLists.md) for dynamic data structures  
**Language Specification:** See [13_StandardLibrary.md](../../languageSpecification/13_StandardLibrary.md)  
**Last Updated:** 2025-01-XX

