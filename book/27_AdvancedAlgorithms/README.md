# Chapter 27: Advanced Algorithms

**Part of:** [SuperPascal — How to Think Like a Programmer](../ThinkLikeAProgrammerBook.md)

---

## Overview

This chapter teaches you advanced algorithms that are essential for efficient programming. You'll learn comprehensive sorting algorithms (8 different sorts with pros, cons, and pitfalls), searching techniques, graph algorithms, dynamic programming, and other important algorithms that fully exercise your SuperPascal skills.

**Learning Objectives:**
- Master 8 sorting algorithms (bubble, selection, insertion, shell, quicksort, mergesort, heapsort, counting sort)
- Implement advanced searching techniques (binary search, tree search, hash-based search)
- Work with graph algorithms (BFS, DFS, shortest path)
- Apply dynamic programming with memoization and tabulation
- Learn pathfinding, string algorithms, mathematical algorithms, greedy algorithms, and backtracking
- Analyze algorithm complexity using Big O notation

---

## Chapter Structure

### [01: Sorting Algorithms](./01_SortingAlgorithms.md)
Learn how to:
- Implement 8 sorting algorithms (bubble, selection, insertion, shell, quicksort, mergesort, heapsort, counting sort)
- Understand pros, cons, and pitfalls of each algorithm
- Choose the right sorting algorithm for your data
- Compare algorithm performance
- Avoid common sorting pitfalls

### [02: Searching Algorithms](./02_SearchingAlgorithms.md)
Learn how to:
- Implement linear search for unsorted data
- Use binary search on sorted arrays
- Build and search binary search trees
- Implement hash-based search for constant-time lookup
- Apply string search algorithms

### [03: Graph Algorithms](./03_GraphAlgorithms.md)
Learn how to:
- Represent graphs (adjacency lists and matrices)
- Traverse graphs with BFS and DFS
- Find shortest paths with Dijkstra's algorithm
- Detect cycles in graphs
- Find connected components

### [04: Dynamic Programming](./04_DynamicProgramming.md)
Learn how to:
- Apply memoization (top-down approach)
- Use tabulation (bottom-up approach)
- Solve classic DP problems (Fibonacci, LCS, coin change, knapsack)
- Identify optimal substructure
- Recognize overlapping subproblems

### [05: Other Important Algorithms](./05_OtherAlgorithms.md)
Learn how to:
- Implement A* pathfinding algorithm
- Work with string algorithms (pattern matching, palindromes)
- Use mathematical algorithms (GCD, LCM, prime numbers, factorials)
- Apply greedy algorithms
- Use backtracking (N-Queens problem)

### [06: Algorithm Selection Guide](./06_AlgorithmSelectionGuide.md)
Learn how to:
- Choose the right sorting algorithm for your data
- Select appropriate searching algorithms
- Decide between graph algorithms (BFS, DFS, Dijkstra's)
- Choose between memoization and tabulation
- Use decision trees and trade-off analysis
- Avoid common algorithm selection mistakes

---

## Prerequisites

Before starting this chapter, you should have completed:
- **Chapters 1-8:** Core programming concepts
- **Chapters 9-16:** Graphics, input, game loop, ECS, tilemaps, physics
- **Chapters 17-24:** Mathematics, audio, scripting, OOP, scenes, debugging, file I/O, hardware

---

## Key Concepts

### Sorting Algorithms

**Basic sorts:**
- **Bubble Sort** — Simple, O(n²), stable, adaptive
- **Selection Sort** — Simple, O(n²), in-place, predictable
- **Insertion Sort** — Efficient for small arrays, O(n²), stable, adaptive

**Intermediate sorts:**
- **Shell Sort** — Improved insertion sort, better than O(n²) in practice

**Advanced sorts:**
- **Quicksort** — Fast average O(n log n), in-place, not stable
- **Mergesort** — Always O(n log n), stable, requires extra memory
- **Heapsort** — Always O(n log n), in-place, not stable

**Specialized sorts:**
- **Counting Sort** — O(n + k) for small integer ranges, stable

### Searching Algorithms

- **Linear Search** — O(n), works on unsorted data
- **Binary Search** — O(log n), requires sorted data
- **Tree Search** — O(log n) average, dynamic data
- **Hash Search** — O(1) average, key-value lookups

### Graph Algorithms

- **BFS** — Breadth-first search, shortest path (unweighted)
- **DFS** — Depth-first search, path finding, cycle detection
- **Dijkstra's** — Shortest path in weighted graphs
- **Graph Representation** — Adjacency lists and matrices

### Dynamic Programming

- **Memoization** — Top-down, cache results
- **Tabulation** — Bottom-up, build table
- **Optimal Substructure** — Building solutions from subproblems
- **Overlapping Subproblems** — Avoiding redundant calculations

### Other Algorithms

- **A* Pathfinding** — Optimal pathfinding with heuristics
- **String Algorithms** — Pattern matching, manipulation
- **Mathematical Algorithms** — GCD, LCM, primes, factorials
- **Greedy Algorithms** — Locally optimal choices
- **Backtracking** — Exploring all possibilities

### Complexity Analysis

- **Big O Notation** — Describing algorithm efficiency
- **Time Complexity** — How runtime grows with input
- **Space Complexity** — Memory usage analysis

---

## Algorithm Comparison

### Sorting Algorithm Comparison

| Algorithm | Best | Average | Worst | Space | Stable |
|-----------|------|---------|-------|-------|--------|
| Bubble Sort | O(n) | O(n²) | O(n²) | O(1) | ✅ |
| Selection Sort | O(n²) | O(n²) | O(n²) | O(1) | ❌ |
| Insertion Sort | O(n) | O(n²) | O(n²) | O(1) | ✅ |
| Shell Sort | O(n log n) | O(n^1.5) | O(n²) | O(1) | ❌ |
| Quicksort | O(n log n) | O(n log n) | O(n²) | O(log n) | ❌ |
| Mergesort | O(n log n) | O(n log n) | O(n log n) | O(n) | ✅ |
| Heapsort | O(n log n) | O(n log n) | O(n log n) | O(1) | ❌ |
| Counting Sort | O(n + k) | O(n + k) | O(n + k) | O(n + k) | ✅ |

### When to Use Each Algorithm

**Bubble Sort:** Educational, very small arrays, nearly sorted data  
**Selection Sort:** Educational, small arrays, minimizing swaps  
**Insertion Sort:** Small arrays, nearly sorted data, hybrid algorithms  
**Shell Sort:** Medium arrays, in-place requirement, embedded systems  
**Quicksort:** General-purpose, large arrays, average performance  
**Mergesort:** Stability required, worst-case guarantee, external sorting  
**Heapsort:** Worst-case guarantee, in-place requirement, priority queues  
**Counting Sort:** Small integer ranges, known range, stability needed

---

**Previous Chapter:** [Chapter 26: Hardware Interfacing](../26_HardwareInterfacing/README.md)  
**Next Chapter:** [Chapter 28: Advanced Data Structures](../28_AdvancedDataStructures/README.md)  
**Last Updated:** 2025-01-XX
