# Advanced Data Structures

**Part of:** [Chapter 28: Advanced Data Structures](./README.md)

---

## Introduction

Advanced data structures enable efficient solutions to complex problems. This section teaches you binary trees and tree traversal, hash tables, graphs, priority queues and heaps, and when to use each data structure.

**Key concepts:**
- **Binary trees** — Tree structures and traversal
- **Hash tables** — Fast lookup structures
- **Graphs** — Network representations
- **Heaps** — Priority queue implementation
- **Choosing structures** — When to use each

---

## Binary Trees and Tree Traversal

### What is a Binary Tree?

**Binary tree structure:**
- Each node has at most 2 children
- Left child < parent < right child (BST property)
- Hierarchical organization
- Efficient searching and insertion

### Tree Node Structure

**Basic tree node:**

```pascal
type
  PTreeNode = ^TTreeNode;
  TTreeNode = record
    Data: integer;
    Left, Right: PTreeNode;
  end;
  
  TBinaryTree = record
    Root: PTreeNode;
  end;
```

### Tree Traversal

**Three main traversal methods:**

```pascal
// In-order: Left, Root, Right (sorted order for BST)
procedure InOrderTraversal(node: PTreeNode);
begin
  if node <> nil then
  begin
    InOrderTraversal(node^.Left);
    WriteLn(node^.Data);
    InOrderTraversal(node^.Right);
  end;
end;

// Pre-order: Root, Left, Right
procedure PreOrderTraversal(node: PTreeNode);
begin
  if node <> nil then
  begin
    WriteLn(node^.Data);
    PreOrderTraversal(node^.Left);
    PreOrderTraversal(node^.Right);
  end;
end;

// Post-order: Left, Right, Root
procedure PostOrderTraversal(node: PTreeNode);
begin
  if node <> nil then
  begin
    PostOrderTraversal(node^.Left);
    PostOrderTraversal(node^.Right);
    WriteLn(node^.Data);
  end;
end;
```

### Tree Operations

**Insert, search, delete:**

```pascal
function InsertNode(var root: PTreeNode; value: integer): PTreeNode;
begin
  if root = nil then
  begin
    New(root);
    root^.Data := value;
    root^.Left := nil;
    root^.Right := nil;
    InsertNode := root;
  end
  else if value < root^.Data then
    InsertNode := InsertNode(root^.Left, value)
  else if value > root^.Data then
    InsertNode := InsertNode(root^.Right, value)
  else
    InsertNode := root;  // Value already exists
end;

function SearchTree(root: PTreeNode; value: integer): PTreeNode;
begin
  if (root = nil) or (root^.Data = value) then
    SearchTree := root
  else if value < root^.Data then
    SearchTree := SearchTree(root^.Left, value)
  else
    SearchTree := SearchTree(root^.Right, value);
end;

function FindMin(node: PTreeNode): PTreeNode;
begin
  if node = nil then
    FindMin := nil
  else if node^.Left = nil then
    FindMin := node
  else
    FindMin := FindMin(node^.Left);
end;

function DeleteNode(var root: PTreeNode; value: integer): PTreeNode;
var
  temp: PTreeNode;
begin
  if root = nil then
    DeleteNode := nil
  else if value < root^.Data then
    root^.Left := DeleteNode(root^.Left, value)
  else if value > root^.Data then
    root^.Right := DeleteNode(root^.Right, value)
  else
  begin
    // Node to delete found
    if root^.Left = nil then
    begin
      temp := root^.Right;
      Dispose(root);
      DeleteNode := temp;
    end
    else if root^.Right = nil then
    begin
      temp := root^.Left;
      Dispose(root);
      DeleteNode := temp;
    end
    else
    begin
      // Node has two children
      temp := FindMin(root^.Right);
      root^.Data := temp^.Data;
      root^.Right := DeleteNode(root^.Right, temp^.Data);
    end;
  end;
  DeleteNode := root;
end;
```

---

## Hash Tables and Hash Functions

### What is a Hash Table?

**Hash table characteristics:**
- Key-value storage
- O(1) average lookup time
- Uses hash function to map keys to indices
- Handles collisions (multiple keys map to same index)

### Hash Function

**Simple hash function:**

```pascal
const
  HASH_TABLE_SIZE = 101;

function Hash(key: integer): integer;
begin
  Hash := key mod HASH_TABLE_SIZE;
end;

function HashString(s: string): integer;
var
  i: integer;
  hash: integer;
begin
  hash := 0;
  for i := 1 to Length(s) do
    hash := (hash * 31 + Ord(s[i])) mod HASH_TABLE_SIZE;
  HashString := hash;
end;
```

### Hash Table Implementation

**With collision handling:**

```pascal
type
  THashEntry = record
    Key: integer;
    Value: integer;
    Used: boolean;
  end;
  
  THashTable = record
    Table: array[0..HASH_TABLE_SIZE - 1] of THashEntry;
  end;

procedure InitHashTable(var ht: THashTable);
var
  i: integer;
begin
  for i := 0 to HASH_TABLE_SIZE - 1 do
    ht.Table[i].Used := false;
end;

function HashInsert(var ht: THashTable; key, value: integer): boolean;
var
  index, startIndex: integer;
begin
  index := Hash(key);
  startIndex := index;
  
  // Linear probing
  repeat
    if not ht.Table[index].Used then
    begin
      ht.Table[index].Key := key;
      ht.Table[index].Value := value;
      ht.Table[index].Used := true;
      HashInsert := true;
      Exit;
    end
    else if ht.Table[index].Key = key then
    begin
      // Update existing
      ht.Table[index].Value := value;
      HashInsert := true;
      Exit;
    end;
    
    index := (index + 1) mod HASH_TABLE_SIZE;
  until index = startIndex;
  
  HashInsert := false;  // Table full
end;

function HashLookup(var ht: THashTable; key: integer; 
                   out value: integer): boolean;
var
  index, startIndex: integer;
begin
  index := Hash(key);
  startIndex := index;
  
  // Linear probing
  repeat
    if not ht.Table[index].Used then
    begin
      HashLookup := false;
      Exit;
    end
    else if ht.Table[index].Key = key then
    begin
      value := ht.Table[index].Value;
      HashLookup := true;
      Exit;
    end;
    
    index := (index + 1) mod HASH_TABLE_SIZE;
  until index = startIndex;
  
  HashLookup := false;
end;
```

### Collision Resolution Strategies

**Different approaches:**

```pascal
// 1. Linear Probing (shown above)
// 2. Chaining (linked list at each bucket)

type
  PChainNode = ^TChainNode;
  TChainNode = record
    Key: integer;
    Value: integer;
    Next: PChainNode;
  end;
  
  TChainedHashTable = record
    Table: array[0..HASH_TABLE_SIZE - 1] of PChainNode;
  end;

procedure ChainedInsert(var ht: TChainedHashTable; key, value: integer);
var
  index: integer;
  newNode, current: PChainNode;
begin
  index := Hash(key);
  
  // Check if key exists
  current := ht.Table[index];
  while current <> nil do
  begin
    if current^.Key = key then
    begin
      current^.Value := value;  // Update
      Exit;
    end;
    current := current^.Next;
  end;
  
  // Insert new node
  New(newNode);
  newNode^.Key := key;
  newNode^.Value := value;
  newNode^.Next := ht.Table[index];
  ht.Table[index] := newNode;
end;
```

---

## Graphs and Graph Representation

### What is a Graph?

**Graph structure:**
- Vertices (nodes) and edges (connections)
- Directed or undirected
- Weighted or unweighted
- Represents relationships/networks

### Adjacency List Representation

**Linked list of neighbors:**

```pascal
type
  PGraphNode = ^TGraphNode;
  TGraphNode = record
    Vertex: integer;
    Weight: integer;  // For weighted graphs
    Next: PGraphNode;
  end;
  
  TGraph = record
    AdjList: array[0..99] of PGraphNode;
    VertexCount: integer;
  end;

procedure InitGraph(var graph: TGraph; vertexCount: integer);
var
  i: integer;
begin
  graph.VertexCount := vertexCount;
  for i := 0 to vertexCount - 1 do
    graph.AdjList[i] := nil;
end;

procedure AddEdge(var graph: TGraph; from, to, weight: integer);
var
  newNode: PGraphNode;
begin
  New(newNode);
  newNode^.Vertex := to;
  newNode^.Weight := weight;
  newNode^.Next := graph.AdjList[from];
  graph.AdjList[from] := newNode;
  
  // For undirected graph, add reverse edge
  // New(newNode);
  // newNode^.Vertex := from;
  // newNode^.Weight := weight;
  // newNode^.Next := graph.AdjList[to];
  // graph.AdjList[to] := newNode;
end;
```

### Adjacency Matrix Representation

**2D array representation:**

```pascal
type
  TAdjMatrix = array[0..99, 0..99] of integer;
  
  TMatrixGraph = record
    Matrix: TAdjMatrix;
    VertexCount: integer;
  end;

procedure InitMatrixGraph(var graph: TMatrixGraph; vertexCount: integer);
var
  i, j: integer;
begin
  graph.VertexCount := vertexCount;
  for i := 0 to vertexCount - 1 do
    for j := 0 to vertexCount - 1 do
      graph.Matrix[i, j] := 0;  // 0 = no edge, or use -1 for no edge
end;

procedure AddMatrixEdge(var graph: TMatrixGraph; from, to, weight: integer);
begin
  graph.Matrix[from, to] := weight;
  // For undirected: graph.Matrix[to, from] := weight;
end;

function HasEdge(graph: TMatrixGraph; from, to: integer): boolean;
begin
  HasEdge := graph.Matrix[from, to] <> 0;
end;
```

### Graph Traversal

**BFS and DFS (covered in Section 01):**

```pascal
// See Section 01: Advanced Algorithms for BFS/DFS implementations
// Graphs use the same traversal algorithms
```

---

## Priority Queues and Heaps

### What is a Heap?

**Heap properties:**
- Complete binary tree
- Heap property: parent >= children (max heap) or parent <= children (min heap)
- Efficient priority queue implementation
- O(log n) insert/extract

### Max Heap Implementation

**Parent always >= children:**

```pascal
type
  TMaxHeap = record
    Data: array[0..99] of integer;
    Size: integer;
  end;

procedure HeapifyUp(var heap: TMaxHeap; index: integer);
var
  parent, temp: integer;
begin
  while index > 0 do
  begin
    parent := (index - 1) div 2;
    if heap.Data[parent] >= heap.Data[index] then
      Break;
    
    // Swap with parent
    temp := heap.Data[parent];
    heap.Data[parent] := heap.Data[index];
    heap.Data[index] := temp;
    
    index := parent;
  end;
end;

procedure HeapifyDown(var heap: TMaxHeap; index: integer);
var
  largest, left, right, temp: integer;
begin
  while true do
  begin
    largest := index;
    left := 2 * index + 1;
    right := 2 * index + 2;
    
    if (left < heap.Size) and (heap.Data[left] > heap.Data[largest]) then
      largest := left;
    
    if (right < heap.Size) and (heap.Data[right] > heap.Data[largest]) then
      largest := right;
    
    if largest = index then
      Break;
    
    // Swap with largest child
    temp := heap.Data[index];
    heap.Data[index] := heap.Data[largest];
    heap.Data[largest] := temp;
    
    index := largest;
  end;
end;

procedure HeapInsert(var heap: TMaxHeap; value: integer);
begin
  if heap.Size >= 100 then
    Exit;  // Heap full
    
  heap.Data[heap.Size] := value;
  HeapifyUp(heap, heap.Size);
  heap.Size := heap.Size + 1;
end;

function HeapExtractMax(var heap: TMaxHeap): integer;
begin
  if heap.Size = 0 then
  begin
    HeapExtractMax := 0;  // Error
    Exit;
  end;
  
  HeapExtractMax := heap.Data[0];
  heap.Data[0] := heap.Data[heap.Size - 1];
  heap.Size := heap.Size - 1;
  
  if heap.Size > 0 then
    HeapifyDown(heap, 0);
end;
```

### Priority Queue Operations

**Using heap:**

```pascal
type
  TPriorityQueue = TMaxHeap;  // Alias

function PriorityEnqueue(var pq: TPriorityQueue; value: integer): boolean;
begin
  HeapInsert(pq, value);
  PriorityEnqueue := true;
end;

function PriorityDequeue(var pq: TPriorityQueue; out value: integer): boolean;
begin
  if pq.Size = 0 then
  begin
    PriorityDequeue := false;
    Exit;
  end;
  
  value := HeapExtractMax(pq);
  PriorityDequeue := true;
end;

function PriorityPeek(pq: TPriorityQueue; out value: integer): boolean;
begin
  if pq.Size = 0 then
  begin
    PriorityPeek := false;
    Exit;
  end;
  
  value := pq.Data[0];
  PriorityPeek := true;
end;
```

---

## When to Use Each Data Structure

### Decision Guide

**Choose based on operations needed:**

| Operation | Array | Linked List | BST | Hash Table | Heap |
|-----------|-------|-------------|-----|------------|------|
| Search | O(n) | O(n) | O(log n) | O(1) | O(n) |
| Insert | O(1) | O(1) | O(log n) | O(1) | O(log n) |
| Delete | O(n) | O(1) | O(log n) | O(1) | O(log n) |
| Min/Max | O(n) | O(n) | O(log n) | O(n) | O(1) |
| Traverse | O(n) | O(n) | O(n) | O(n) | O(n log n) |

### Use Cases

**Array:**
- Fixed size collections
- Random access needed
- Simple iteration

**Linked List:**
- Dynamic size
- Frequent insertions/deletions
- No random access needed

**Binary Search Tree:**
- Sorted data
- Range queries
- Dynamic sorted collection

**Hash Table:**
- Fast lookup
- Key-value pairs
- No ordering needed

**Heap:**
- Priority queue
- Min/max operations
- Scheduling algorithms

**Graph:**
- Network representation
- Relationships
- Path finding

### Example: Choosing a Structure

**Scenario: Game high scores**

```pascal
// Option 1: Array (if fixed size, simple)
type
  THighScores = array[0..9] of integer;

// Option 2: Heap (if need top N efficiently)
type
  THighScoreHeap = TMaxHeap;

// Option 3: BST (if need sorted order)
type
  THighScoreTree = TBinaryTree;

// Best choice: Heap for top N, or sorted array for display
```

---

## Best Practices

### 1. Choose Appropriate Structure

**Match structure to operations:**

```pascal
// ✅ GOOD: Hash table for fast lookup
var playerScores: THashTable;
HashLookup(playerScores, playerID, score);

// ❌ BAD: Array for frequent lookups
var scores: array[0..999] of integer;
// Need to search entire array
```

### 2. Consider Memory Usage

**Balance speed and memory:**

```pascal
// ✅ GOOD: Adjacency list for sparse graph
// Uses less memory than matrix

// ✅ GOOD: Adjacency matrix for dense graph
// Faster edge queries
```

### 3. Handle Edge Cases

**Empty structures, full structures:**

```pascal
// ✅ GOOD: Check before operations
if heap.Size = 0 then
  WriteLn('Heap is empty')
else
  value := HeapExtractMax(heap);

// ❌ BAD: Assume structure has data
value := HeapExtractMax(heap);  // May crash
```

### 4. Maintain Invariants

**Keep structure properties:**

```pascal
// ✅ GOOD: Maintain heap property
procedure HeapInsert(var heap: TMaxHeap; value: integer);
begin
  // Insert and heapify to maintain property
  heap.Data[heap.Size] := value;
  HeapifyUp(heap, heap.Size);
  heap.Size := heap.Size + 1;
end;
```

---

## Exercises

### Exercise 1: Binary Tree

Write a program that:
1. Implements binary search tree
2. Inserts values
3. Traverses in-order, pre-order, post-order
4. Searches for values

### Exercise 2: Hash Table

Write a program that:
1. Implements hash table with linear probing
2. Inserts key-value pairs
3. Looks up values by key
4. Handles collisions

### Exercise 3: Graph

Write a program that:
1. Creates a graph (adjacency list)
2. Adds edges
3. Traverses with BFS and DFS
4. Finds shortest path

### Exercise 4: Priority Queue

Write a program that:
1. Implements max heap
2. Inserts values
3. Extracts maximum
4. Maintains heap property

---

**Previous Chapter:** [Chapter 27: Advanced Algorithms](../27_AdvancedAlgorithms/README.md)  
**Next Chapter:** [Chapter 29: Optimization Techniques](../29_OptimizationTechniques/README.md)  
**Last Updated:** 2025-01-XX

