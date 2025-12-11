# Algorithm Selection Guide

**Part of:** [Chapter 29: Advanced Algorithms](./README.md)

---

## Introduction

Knowing how to implement algorithms is important, but knowing **when to use which algorithm** is equally critical. This section provides decision-making frameworks to help you choose the right algorithm for your problem.

**Target Levels:**
- **GCSE:** Simple examples and basic decision rules
- **A-Level:** Decision trees and trade-off analysis
- **University:** Comprehensive analysis with performance implications

---

## Decision Framework

### Questions to Ask

**Before choosing an algorithm, ask:**
1. What is the problem I'm solving?
2. What are my constraints? (time, space, data size)
3. What operations do I need? (sort, search, traverse)
4. What are my data characteristics? (sorted, random, nearly sorted)
5. What are my requirements? (stability, in-place, worst-case guarantee)

---

## Sorting Algorithm Selection

### Quick Decision Tree

**Is your data already sorted or nearly sorted?**
- ✅ **Yes** → Insertion Sort (O(n) best case)
- ❌ **No** → Continue...

**Is your array very small (< 10 elements)?**
- ✅ **Yes** → Insertion Sort or Bubble Sort (simple, fast enough)
- ❌ **No** → Continue...

**Do you need stability?**
- ✅ **Yes** → Mergesort or Insertion Sort
- ❌ **No** → Continue...

**Do you need worst-case guarantee?**
- ✅ **Yes** → Mergesort or Heapsort
- ❌ **No** → Quicksort (faster average)

**Is your data small integer range?**
- ✅ **Yes** → Counting Sort (O(n + k))
- ❌ **No** → Use general-purpose sort

### Detailed Selection Guide

#### When to Use Bubble Sort

**Use when:**
- Educational purposes
- Very small arrays (< 10 elements)
- Nearly sorted data (with optimization)
- Simple implementation needed

**Don't use when:**
- Large arrays (> 50 elements)
- Performance matters
- General-purpose sorting

#### When to Use Selection Sort

**Use when:**
- Educational purposes
- Small arrays
- Minimizing swaps is important
- Stability doesn't matter

**Don't use when:**
- Large arrays
- Stability required
- Performance matters

#### When to Use Insertion Sort

**Use when:**
- Small arrays (< 50 elements)
- Nearly sorted data
- Stability required
- Part of hybrid algorithms (e.g., timsort)
- Online sorting (data arriving one at a time)

**Don't use when:**
- Large unsorted arrays
- Performance is critical

#### When to Use Shell Sort

**Use when:**
- Medium-sized arrays
- In-place sorting required
- Stability not needed
- Embedded systems with memory constraints

**Don't use when:**
- Stability required
- Worst-case guarantee needed

#### When to Use Quicksort

**Use when:**
- General-purpose sorting
- Large arrays
- Average performance matters
- In-place sorting preferred
- Stability not required

**Don't use when:**
- Worst-case guarantee needed
- Stability required
- Data is already sorted (worst case)

#### When to Use Mergesort

**Use when:**
- Stability required
- Worst-case guarantee needed
- External sorting (large files)
- Linked lists
- Predictable performance

**Don't use when:**
- In-place sorting required
- Memory is limited
- Average performance is more important than worst-case

#### When to Use Heapsort

**Use when:**
- Worst-case O(n log n) required
- In-place sorting needed
- Stability not required
- Priority queue applications

**Don't use when:**
- Stability required
- Average performance is priority
- Cache performance matters

#### When to Use Counting Sort

**Use when:**
- Small integer range (0-255, 0-1000)
- Range is known
- Stability needed
- Linear time required

**Don't use when:**
- Large range
- Range unknown
- Non-integer data

---

## Searching Algorithm Selection

### Quick Decision Tree

**Is your data sorted?**
- ✅ **Yes** → Binary Search (O(log n))
- ❌ **No** → Continue...

**Is your data small (< 50 elements)?**
- ✅ **Yes** → Linear Search (simple, fast enough)
- ❌ **No** → Sort first, then binary search

**Do you need dynamic insertions/deletions?**
- ✅ **Yes** → Binary Search Tree
- ❌ **No** → Binary Search on sorted array

**Do you need O(1) average lookup?**
- ✅ **Yes** → Hash Table
- ❌ **No** → Binary Search or BST

### Detailed Selection Guide

#### When to Use Linear Search

**Use when:**
- Unsorted data
- Small datasets (< 50 elements)
- One-time searches
- Simple implementation needed

**Don't use when:**
- Large datasets
- Frequent searches
- Performance matters

#### When to Use Binary Search

**Use when:**
- Sorted data
- Large datasets
- Frequent searches
- O(log n) performance needed

**Don't use when:**
- Unsorted data (sort first)
- Dynamic insertions/deletions needed
- O(1) lookup required

#### When to Use Binary Search Tree

**Use when:**
- Dynamic data (frequent insertions/deletions)
- Need sorted order maintained
- Range queries needed
- O(log n) average operations

**Don't use when:**
- Static data (use sorted array)
- O(1) lookup required
- Worst-case O(n) is unacceptable

#### When to Use Hash Table

**Use when:**
- O(1) average lookup needed
- Key-value pairs
- No ordering required
- Fast insertions/deletions

**Don't use when:**
- Sorted order needed
- Range queries needed
- Worst-case O(n) is unacceptable

---

## Graph Algorithm Selection

### When to Use BFS

**Use when:**
- Shortest path (unweighted graphs)
- Level-order traversal
- Finding connected components
- Minimum steps problems

**Don't use when:**
- Weighted graphs (use Dijkstra's)
- Deep exploration needed (use DFS)

### When to Use DFS

**Use when:**
- Path finding
- Cycle detection
- Topological sorting
- Deep exploration
- Memory is limited

**Don't use when:**
- Shortest path needed (use BFS)
- Level-order traversal needed

### When to Use Dijkstra's Algorithm

**Use when:**
- Shortest path in weighted graphs
- Positive edge weights only
- Single-source shortest path
- Optimal solution needed

**Don't use when:**
- Negative weights (use Bellman-Ford)
- All-pairs shortest path (use Floyd-Warshall)

---

## Dynamic Programming Selection

### When to Use Memoization (Top-Down)

**Use when:**
- Natural recursive structure
- Not all subproblems needed
- Easy to implement recursively
- Stack depth is reasonable

**Don't use when:**
- All subproblems needed anyway
- Stack overflow risk
- Iterative approach is clearer

### When to Use Tabulation (Bottom-Up)

**Use when:**
- All subproblems needed
- No stack overflow risk
- Often faster than memoization
- Clear iteration pattern

**Don't use when:**
- Not all subproblems needed
- Recursive structure is clearer

---

## Algorithm Selection Checklist

### Before Choosing an Algorithm

- [ ] What problem am I solving?
- [ ] What are my data characteristics?
- [ ] What are my constraints? (time, space)
- [ ] What operations do I need?
- [ ] What are my requirements? (stability, worst-case, etc.)
- [ ] How large is my data?
- [ ] How often will I use this?
- [ ] What's the simplest solution that works?

### After Choosing an Algorithm

- [ ] Does it solve my problem correctly?
- [ ] Does it meet my performance requirements?
- [ ] Does it meet my space requirements?
- [ ] Is it the simplest solution?
- [ ] Can I implement it correctly?
- [ ] Have I tested it?

---

## Common Mistakes

### Mistake 1: Over-Engineering

**❌ Bad:**
```pascal
// Using quicksort for 5 elements
QuickSort(smallArray, 0, 4);
```

**✅ Good:**
```pascal
// Simple insertion sort for small array
InsertionSort(smallArray);
```

### Mistake 2: Under-Engineering

**❌ Bad:**
```pascal
// Using bubble sort for 10,000 elements
BubbleSort(largeArray);
```

**✅ Good:**
```pascal
// Efficient quicksort for large array
QuickSort(largeArray, 0, Length(largeArray) - 1);
```

### Mistake 3: Ignoring Requirements

**❌ Bad:**
```pascal
// Using quicksort when stability is required
QuickSort(data, 0, n - 1);  // May break order of equal elements
```

**✅ Good:**
```pascal
// Using mergesort for stability
MergeSort(data, 0, n - 1);  // Preserves order
```

---

## Best Practices

### 1. Start Simple

**Begin with the simplest algorithm that works:**
- Small data → Simple algorithm
- If it's fast enough, you're done
- Only optimize if needed

### 2. Measure First

**Don't optimize prematurely:**
- Implement simple solution
- Measure performance
- Optimize only if needed

### 3. Consider Trade-offs

**Every algorithm has trade-offs:**
- Time vs space
- Simplicity vs performance
- Average vs worst-case
- Stability vs speed

### 4. Know Your Data

**Understand your data:**
- Size
- Characteristics (sorted, random, etc.)
- Access patterns
- Update frequency

---

## Exercises

### Exercise 1: Algorithm Selection

For each scenario, choose the best algorithm:
1. Sort 1000 random integers
2. Sort 10 nearly-sorted integers
3. Find item in sorted array of 1 million elements
4. Find item in unsorted array of 20 elements
5. Maintain sorted order with frequent insertions

### Exercise 2: Trade-off Analysis

For each algorithm pair, analyze trade-offs:
1. Quicksort vs Mergesort
2. Binary Search vs Hash Table
3. BFS vs DFS
4. Memoization vs Tabulation

### Exercise 3: Real-World Scenarios

Choose algorithms for:
1. Game leaderboard (top 10 scores)
2. Dictionary lookup
3. Pathfinding in game
4. Sorting player inventory

---

**Previous Section:** [Other Important Algorithms](./05_OtherAlgorithms.md)  
**Next Chapter:** [Chapter 30: Advanced Data Structures](../28_AdvancedDataStructures/README.md)  
**Last Updated:** 2025-01-XX

