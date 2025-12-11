# Data Structure Selection Guide

**Part of:** [Chapter 30: Advanced Data Structures](./README.md)

---

## Introduction

Choosing the right data structure is crucial for writing efficient programs. This section provides decision-making frameworks to help you select the best data structure for your problem.

**Target Levels:**
- **GCSE:** Simple comparisons (arrays vs lists)
- **A-Level:** When to use which structure, basic trade-offs
- **University:** Comprehensive comparison, performance analysis

---

## Decision Framework

### Questions to Ask

**Before choosing a data structure, ask:**
1. What operations do I need? (insert, delete, search, access)
2. How is data accessed? (random, sequential, by key)
3. What are my performance requirements?
4. What are my space constraints?
5. Do I need ordering?
6. How often will data change?

---

## Array vs List Selection

### When to Use Arrays

**Use arrays when:**
- Fixed size known at compile time
- Random access needed (by index)
- Memory efficiency matters
- Sequential access pattern
- Simple implementation needed

**Example:**
```pascal
// Game entities with fixed maximum
var entities: array[0..99] of TEntity;
```

### When to Use Lists

**Use lists when:**
- Dynamic size needed
- Frequent insertions/deletions in middle
- Size unknown at compile time
- Heterogeneous data types needed
- Order matters

**Example:**
```pascal
// Dynamic list of game items
var inventory: TList;
```

### Comparison

| Feature | Array | List |
|---------|-------|------|
| Size | Fixed | Dynamic |
| Random Access | O(1) | O(n) |
| Insert at End | O(1) | O(1) |
| Insert in Middle | O(n) | O(1) |
| Memory | Contiguous | Scattered |
| Cache Performance | Good | Poor |

---

## Tree vs Hash Table Selection

### When to Use Binary Search Tree

**Use BST when:**
- Sorted order needed
- Range queries needed
- Dynamic insertions/deletions
- O(log n) operations acceptable
- Predictable performance

**Example:**
```pascal
// Sorted player scores
var scoreTree: PTreeNode;
```

### When to Use Hash Table

**Use hash table when:**
- O(1) average lookup needed
- No ordering required
- Key-value pairs
- Fast insertions/deletions
- Large dataset

**Example:**
```pascal
// Player name to ID mapping
var playerTable: THashTable;
```

### Comparison

| Feature | BST | Hash Table |
|---------|-----|------------|
| Average Search | O(log n) | O(1) |
| Worst Search | O(n) | O(n) |
| Sorted Order | Yes | No |
| Range Queries | Yes | No |
| Memory | O(n) | O(n) |

---

## Graph Representation Selection

### When to Use Adjacency List

**Use adjacency list when:**
- Sparse graph (few edges)
- Memory efficiency matters
- Dynamic graph (add/remove edges)
- Iterate over neighbors

**Example:**
```pascal
// Game map with few connections
var graph: TGraph;  // Adjacency list
```

### When to Use Adjacency Matrix

**Use adjacency matrix when:**
- Dense graph (many edges)
- Fast edge queries needed
- Fixed graph size
- O(1) edge lookup needed

**Example:**
```pascal
// Fully connected network
var matrix: TAdjMatrix;
```

### Comparison

| Feature | Adjacency List | Adjacency Matrix |
|---------|----------------|------------------|
| Memory | O(V + E) | O(V²) |
| Edge Query | O(degree) | O(1) |
| Iterate Neighbors | O(degree) | O(V) |
| Add Edge | O(1) | O(1) |
| Space Efficiency | Sparse graphs | Dense graphs |

---

## Priority Queue Selection

### When to Use Heap

**Use heap when:**
- Priority queue needed
- Extract max/min frequently
- O(log n) insert/extract needed
- In-place sorting (heapsort)

**Example:**
```pascal
// Event queue (process by priority)
var eventQueue: THeap;
```

### When to Use Sorted Array

**Use sorted array when:**
- Small dataset
- Infrequent updates
- Simple implementation
- Memory efficiency

**Example:**
```pascal
// Top 10 scores (small, infrequent updates)
var topScores: array[0..9] of integer;  // Sorted
```

---

## Data Structure Selection Checklist

### Before Choosing

- [ ] What operations do I need?
- [ ] How is data accessed?
- [ ] What are performance requirements?
- [ ] What are space constraints?
- [ ] Do I need ordering?
- [ ] How often does data change?
- [ ] What's the data size?

### After Choosing

- [ ] Does it support needed operations?
- [ ] Does it meet performance requirements?
- [ ] Does it meet space requirements?
- [ ] Is it the simplest solution?
- [ ] Can I implement it correctly?

---

## Common Selection Patterns

### Pattern 1: Key-Value Lookup

**Problem:** Fast lookup by key

**Solutions:**
- Hash table (O(1) average)
- BST (O(log n), sorted)
- Sorted array (O(log n), binary search)

**Choose:**
- Hash table if no ordering needed
- BST if ordering needed
- Sorted array if static data

### Pattern 2: Dynamic Collection

**Problem:** Collection that grows/shrinks

**Solutions:**
- List (dynamic size)
- Array with resizing (complex)
- Tree (sorted, dynamic)

**Choose:**
- List for simple dynamic collection
- Tree if sorted order needed
- Array if size known/limited

### Pattern 3: Priority Processing

**Problem:** Process items by priority

**Solutions:**
- Heap (O(log n) operations)
- Sorted array (O(n) insert)
- Unsorted array (O(n) find max)

**Choose:**
- Heap for frequent operations
- Sorted array for small, infrequent updates

---

## Best Practices

### 1. Start Simple

**Use the simplest structure that works:**
- Array for fixed-size, indexed access
- List for dynamic size
- Only use complex structures if needed

### 2. Consider Access Patterns

**Match structure to access:**
- Random access → Array
- Sequential access → List or Array
- Key lookup → Hash table or BST
- Priority access → Heap

### 3. Measure Performance

**Don't assume:**
- Implement simple solution
- Measure performance
- Optimize if needed

### 4. Know Trade-offs

**Every structure has trade-offs:**
- Time vs space
- Simplicity vs performance
- Flexibility vs efficiency

---

## Exercises

### Exercise 1: Structure Selection

Choose data structure for:
1. Game inventory (add/remove items)
2. Player name to score mapping
3. Event queue (process by time)
4. Game map connections
5. Top 10 leaderboard

### Exercise 2: Trade-off Analysis

Compare:
1. Array vs List for your use case
2. BST vs Hash Table for lookup
3. Adjacency List vs Matrix for graph

### Exercise 3: Real-World Scenarios

Design data structures for:
1. Game entity system
2. Save game data
3. Menu system
4. Collision detection

---

**Previous Section:** [Advanced Data Structures](./01_AdvancedDataStructures.md)  
**Next Chapter:** [Chapter 31: Optimization Techniques](../29_OptimizationTechniques/README.md)  
**Last Updated:** 2025-01-XX

