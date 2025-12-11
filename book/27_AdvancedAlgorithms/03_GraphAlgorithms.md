# Graph Algorithms

**Part of:** [Chapter 27: Advanced Algorithms](./README.md)

---

## Introduction

Graphs represent relationships and connections. This section teaches you how to represent graphs and implement fundamental graph algorithms including traversal, path finding, and shortest path algorithms.

**Key concepts:**
- **Graph representation** — Adjacency lists and matrices
- **Graph traversal** — BFS and DFS
- **Path finding** — Finding paths between nodes
- **Shortest path** — Dijkstra's algorithm
- **Graph applications** — Real-world uses

---

## Graph Representation

### Adjacency List

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
end;
```

**Pros:**
- ✅ Efficient for sparse graphs
- ✅ Uses less memory
- ✅ Easy to add/remove edges

**Cons:**
- ❌ Slower edge queries
- ❌ More complex to implement

### Adjacency Matrix

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
      graph.Matrix[i, j] := 0;  // 0 = no edge
end;

procedure AddMatrixEdge(var graph: TMatrixGraph; from, to, weight: integer);
begin
  graph.Matrix[from, to] := weight;
end;
```

**Pros:**
- ✅ Fast edge queries: O(1)
- ✅ Simple to implement
- ✅ Good for dense graphs

**Cons:**
- ❌ Uses O(V²) memory
- ❌ Inefficient for sparse graphs

---

## Breadth-First Search (BFS)

### BFS Implementation

**Level-order traversal:**

```pascal
procedure BFS(graph: TGraph; start: integer);
var
  queue: array[0..99] of integer;
  front, rear: integer;
  visited: array[0..99] of boolean;
  current, neighbor: integer;
  node: PGraphNode;
begin
  // Initialize
  front := 0;
  rear := 0;
  FillChar(visited, SizeOf(visited), false);
  
  // Enqueue start
  queue[rear] := start;
  rear := rear + 1;
  visited[start] := true;
  
  while front < rear do
  begin
    // Dequeue
    current := queue[front];
    front := front + 1;
    
    WriteLn('Visited: ', current);
    
    // Visit neighbors
    node := graph.AdjList[current];
    while node <> nil do
    begin
      neighbor := node^.Vertex;
      if not visited[neighbor] then
      begin
        visited[neighbor] := true;
        queue[rear] := neighbor;
        rear := rear + 1;
      end;
      node := node^.Next;
    end;
  end;
end;
```

**Pros:**
- ✅ Finds shortest path (unweighted)
- ✅ Explores level by level
- ✅ Guaranteed to find path if exists

**Cons:**
- ❌ Uses more memory (queue)
- ❌ May visit many nodes

**When to use:**
- Shortest path (unweighted graphs)
- Level-order traversal
- Finding connected components

---

## Depth-First Search (DFS)

### DFS Implementation

**Recursive traversal:**

```pascal
procedure DFSRecursive(graph: TGraph; vertex: integer; 
                      var visited: array of boolean);
var
  neighbor: integer;
  node: PGraphNode;
begin
  visited[vertex] := true;
  WriteLn('Visited: ', vertex);
  
  node := graph.AdjList[vertex];
  while node <> nil do
  begin
    neighbor := node^.Vertex;
    if not visited[neighbor] then
      DFSRecursive(graph, neighbor, visited);
    node := node^.Next;
  end;
end;

procedure DFS(graph: TGraph; start: integer);
var
  visited: array[0..99] of boolean;
begin
  FillChar(visited, SizeOf(visited), false);
  DFSRecursive(graph, start, visited);
end;
```

**Pros:**
- ✅ Uses less memory (stack)
- ✅ Explores deeply
- ✅ Good for path finding

**Cons:**
- ❌ May not find shortest path
- ❌ Stack overflow on deep graphs

**When to use:**
- Path finding
- Cycle detection
- Topological sorting
- Maze solving

---

## Shortest Path Algorithms

### Dijkstra's Algorithm

**Find shortest path in weighted graph:**

```pascal
procedure Dijkstra(graph: TGraph; start: integer; 
                   var dist: array of integer);
var
  visited: array[0..99] of boolean;
  i, u, v, minDist: integer;
  node: PGraphNode;
begin
  // Initialize
  for i := 0 to graph.VertexCount - 1 do
  begin
    dist[i] := MAX_INT;
    visited[i] := false;
  end;
  dist[start] := 0;
  
  // Find shortest path for all vertices
  for i := 0 to graph.VertexCount - 1 do
  begin
    // Find unvisited vertex with minimum distance
    u := -1;
    minDist := MAX_INT;
    for v := 0 to graph.VertexCount - 1 do
    begin
      if not visited[v] and (dist[v] < minDist) then
      begin
        minDist := dist[v];
        u := v;
      end;
    end;
    
    if u = -1 then
      Break;  // No more reachable vertices
    
    visited[u] := true;
    
    // Update distances to neighbors
    node := graph.AdjList[u];
    while node <> nil do
    begin
      v := node^.Vertex;
      if not visited[v] and (dist[u] + node^.Weight < dist[v]) then
        dist[v] := dist[u] + node^.Weight;
      node := node^.Next;
    end;
  end;
end;
```

**Pros:**
- ✅ Finds shortest path in weighted graphs
- ✅ Guaranteed optimal solution
- ✅ Works with positive weights

**Cons:**
- ❌ Doesn't work with negative weights
- ❌ O(V²) time (can be optimized with heap)

**When to use:**
- Shortest path in weighted graphs
- Positive edge weights only
- Single-source shortest path

---

## Graph Applications

### Finding Connected Components

**Find all connected groups:**

```pascal
procedure FindConnectedComponents(graph: TGraph);
var
  visited: array[0..99] of boolean;
  component: integer;
  i: integer;
begin
  FillChar(visited, SizeOf(visited), false);
  component := 0;
  
  for i := 0 to graph.VertexCount - 1 do
  begin
    if not visited[i] then
    begin
      component := component + 1;
      WriteLn('Component ', component, ':');
      DFSRecursive(graph, i, visited);
    end;
  end;
end;
```

### Cycle Detection

**Detect cycles in graph:**

```pascal
function HasCycle(graph: TGraph): boolean;
var
  visited: array[0..99] of boolean;
  recStack: array[0..99] of boolean;
  
  function DFSWithCycle(vertex: integer): boolean;
  var
    neighbor: integer;
    node: PGraphNode;
  begin
    visited[vertex] := true;
    recStack[vertex] := true;
    
    node := graph.AdjList[vertex];
    while node <> nil do
    begin
      neighbor := node^.Vertex;
      if not visited[neighbor] then
      begin
        if DFSWithCycle(neighbor) then
        begin
          DFSWithCycle := true;
          Exit;
        end;
      end
      else if recStack[neighbor] then
      begin
        DFSWithCycle := true;
        Exit;
      end;
      node := node^.Next;
    end;
    
    recStack[vertex] := false;
    DFSWithCycle := false;
  end;
  
var
  i: integer;
begin
  FillChar(visited, SizeOf(visited), false);
  FillChar(recStack, SizeOf(recStack), false);
  
  for i := 0 to graph.VertexCount - 1 do
    if not visited[i] then
      if DFSWithCycle(i) then
      begin
        HasCycle := true;
        Exit;
      end;
  
  HasCycle := false;
end;
```

---

## Best Practices

### 1. Choose Right Representation

**Match representation to graph:**

```pascal
// ✅ GOOD: Adjacency list for sparse graph
// ✅ GOOD: Adjacency matrix for dense graph

// ❌ BAD: Matrix for very sparse graph (wastes memory)
```

### 2. Handle Edge Cases

**Check for empty graphs:**

```pascal
// ✅ GOOD: Check edge cases
if graph.VertexCount = 0 then
  Exit;

// ❌ BAD: Assume graph has vertices
```

---

**Previous Section:** [Searching Algorithms](./02_SearchingAlgorithms.md)  
**Next Section:** [Dynamic Programming](./04_DynamicProgramming.md)  
**Last Updated:** 2025-01-XX

