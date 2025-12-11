# Searching Algorithms

**Part of:** [Chapter 27: Advanced Algorithms](./README.md)

---

## Introduction

Searching is finding data in a collection. This section teaches you various searching algorithms, from simple linear search to advanced tree and hash-based searching.

**Key concepts:**
- **Linear search** — Simple sequential search
- **Binary search** — Fast search on sorted data
- **Tree search** — Binary search trees
- **Hash search** — Constant-time lookup
- **String search** — Pattern matching

---

## Linear Search

### Basic Linear Search

**Search sequentially:**

```pascal
function LinearSearch(arr: array of integer; value: integer): integer;
var
  i: integer;
begin
  for i := 0 to Length(arr) - 1 do
    if arr[i] = value then
    begin
      LinearSearch := i;
      Exit;
    end;
  LinearSearch := -1;  // Not found
end;
```

**Pros:**
- ✅ Simple to understand
- ✅ Works on unsorted data
- ✅ Easy to implement

**Cons:**
- ❌ Slow: O(n) time
- ❌ Must check every element (worst case)

**When to use:**
- Unsorted data
- Small arrays
- One-time searches

---

## Binary Search

### Binary Search on Sorted Array

**Divide and conquer:**

```pascal
function BinarySearch(arr: array of integer; value: integer): integer;
var
  left, right, mid: integer;
begin
  left := 0;
  right := Length(arr) - 1;
  
  while left <= right do
  begin
    mid := (left + right) div 2;
    
    if arr[mid] = value then
    begin
      BinarySearch := mid;
      Exit;
    end
    else if arr[mid] < value then
      left := mid + 1
    else
      right := mid - 1;
  end;
  
  BinarySearch := -1;  // Not found
end;
```

**Pros:**
- ✅ Fast: O(log n) time
- ✅ Efficient for large sorted arrays
- ✅ Simple to implement

**Cons:**
- ❌ Requires sorted data
- ❌ O(n log n) to sort first (if unsorted)

**Pitfalls:**
- ⚠️ Off-by-one in bounds
- ⚠️ Integer overflow in mid calculation
- ⚠️ Forgetting array must be sorted

**When to use:**
- Sorted arrays
- Large datasets
- Frequent searches

---

## Tree-Based Search

### Binary Search Tree

**Tree structure for searching:**

```pascal
type
  PTreeNode = ^TTreeNode;
  TTreeNode = record
    Value: integer;
    Left, Right: PTreeNode;
  end;

function BSTSearch(root: PTreeNode; value: integer): PTreeNode;
begin
  if (root = nil) or (root^.Value = value) then
    BSTSearch := root
  else if value < root^.Value then
    BSTSearch := BSTSearch(root^.Left, value)
  else
    BSTSearch := BSTSearch(root^.Right, value);
end;
```

**Pros:**
- ✅ Fast: O(log n) average
- ✅ Dynamic (can add/remove)
- ✅ Maintains sorted order

**Cons:**
- ❌ Worst case: O(n) if unbalanced
- ❌ Requires tree balancing
- ❌ More complex than array search

**When to use:**
- Dynamic data (frequent insertions/deletions)
- Need sorted order maintained
- Range queries

---

## Hash-Based Search

### Hash Table Lookup

**Constant-time search:**

```pascal
const
  HASH_TABLE_SIZE = 101;

function Hash(key: integer): integer;
begin
  Hash := key mod HASH_TABLE_SIZE;
end;

function HashSearch(var table: THashTable; key: integer; 
                   out value: integer): boolean;
var
  index, startIndex: integer;
begin
  index := Hash(key);
  startIndex := index;
  
  repeat
    if not table[index].Used then
    begin
      HashSearch := false;
      Exit;
    end
    else if table[index].Key = key then
    begin
      value := table[index].Value;
      HashSearch := true;
      Exit;
    end;
    
    index := (index + 1) mod HASH_TABLE_SIZE;
  until index = startIndex;
  
  HashSearch := false;
end;
```

**Pros:**
- ✅ Very fast: O(1) average
- ✅ Efficient for large datasets
- ✅ Good for key-value lookups

**Cons:**
- ❌ Requires good hash function
- ❌ Collision handling needed
- ❌ Worst case: O(n) with many collisions

**When to use:**
- Fast lookups needed
- Key-value pairs
- No ordering required

---

## String Search Algorithms

### Simple String Search

**Find substring in string:**

```pascal
function SimpleStringSearch(text, pattern: string): integer;
var
  i, j: integer;
begin
  for i := 1 to Length(text) - Length(pattern) + 1 do
  begin
    j := 1;
    while (j <= Length(pattern)) and (text[i + j - 1] = pattern[j]) do
      j := j + 1;
    
    if j > Length(pattern) then
    begin
      SimpleStringSearch := i;
      Exit;
    end;
  end;
  
  SimpleStringSearch := 0;  // Not found
end;
```

**Pros:**
- ✅ Simple to understand
- ✅ Easy to implement

**Cons:**
- ❌ Slow: O(n*m) where n=text, m=pattern
- ❌ Many unnecessary comparisons

**When to use:**
- Short patterns
- One-time searches
- Educational purposes

---

## Best Practices

### 1. Choose Right Algorithm

**Match search to data:**

```pascal
// ✅ GOOD: Binary search for sorted
if IsSorted(arr) then
  index := BinarySearch(arr, value)
else
  index := LinearSearch(arr, value);

// ❌ BAD: Always linear search
index := LinearSearch(arr, value);  // May be slow
```

### 2. Pre-sort if Needed

**Sort once, search many:**

```pascal
// ✅ GOOD: Sort once, search many times
QuickSort(arr, 0, Length(arr) - 1);
for i := 0 to SearchCount - 1 do
  index := BinarySearch(arr, searchValues[i]);

// ❌ BAD: Linear search many times
for i := 0 to SearchCount - 1 do
  index := LinearSearch(arr, searchValues[i]);  // Slow
```

---

**Previous Section:** [Sorting Algorithms](./01_SortingAlgorithms.md)  
**Next Section:** [Graph Algorithms](./03_GraphAlgorithms.md)  
**Last Updated:** 2025-01-XX

