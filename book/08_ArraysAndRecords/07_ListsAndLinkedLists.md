# Lists and Linked Lists

**Part of:** [Chapter 06: Arrays and Records](./README.md)

---

## Introduction

**Lists and linked lists** are dynamic data structures that can grow and shrink at runtime. Unlike arrays (which have fixed size and single type), lists can change size and **contain elements of different types** (heterogeneous).

**Key Difference: Arrays vs Lists**
- **Arrays**: Homogeneous (single type), fixed size, compile-time bounds
- **Lists**: Heterogeneous (any types), dynamic size, runtime bounds

**Why lists matter:**
- **Dynamic size** — Can grow and shrink
- **Heterogeneous** — Can contain any type or combination of types (like Python lists)
- **Efficient insertion** — Add elements anywhere
- **Efficient deletion** — Remove elements anywhere
- **Memory efficient** — Only use what you need
- **Flexible** — No need to know size or types in advance

**Types of lists:**
- **SuperPascal lists** — Built-in heterogeneous list type (like Python)
- **Array-based lists** — Use arrays with count variable (homogeneous)
- **Singly linked lists** — Each node points to next
- **Doubly linked lists** — Each node points to next and previous

---

## Array-Based Lists

### What is an Array-Based List?

**Array-based list** uses a fixed-size array with a `count` variable to track used elements. Unlike SuperPascal lists, array-based lists are **homogeneous** (single type):

```pascal
type
  TList = record
    Data: array[0..99] of integer;  // Only integers
    Count: integer;
  end;
```

**Advantages:**
- **Simple** — Easy to understand and implement
- **Fast access** — O(1) random access by index
- **Cache-friendly** — Contiguous memory
- **Efficient** — No pointer overhead
- **Type-safe** — Compile-time type checking

**Disadvantages:**
- **Fixed maximum size** — Limited by array size
- **Homogeneous** — All elements must be same type
- **Inefficient insertion** — O(n) to insert in middle
- **Inefficient deletion** — O(n) to delete from middle

**When to use:**
- Need homogeneous collection (all same type)
- Known maximum size
- Performance-critical code
- Want compile-time type safety

### Basic Operations

**Create empty list:**
```pascal
procedure ListInit(var list: TList);
begin
  list.Count := 0;
end;
```

**Add to end:**
```pascal
procedure ListAdd(var list: TList; value: integer);
begin
  if list.Count < Length(list.Data) then
  begin
    list.Data[list.Count] := value;
    list.Count := list.Count + 1;
  end
  else
    WriteLn('Error: List full');
end;
```

**Get element by index:**
```pascal
function ListGet(const list: TList; index: integer): integer;
begin
  if (index >= 0) and (index < list.Count) then
    ListGet := list.Data[index]
  else
  begin
    WriteLn('Error: Index out of bounds');
    ListGet := 0;
  end;
end;
```

**Remove from end:**
```pascal
function ListRemove(var list: TList): integer;
begin
  if list.Count > 0 then
  begin
    list.Count := list.Count - 1;
    ListRemove := list.Data[list.Count];
  end
  else
  begin
    WriteLn('Error: List empty');
    ListRemove := 0;
  end;
end;
```

---

## Singly Linked Lists

### What is a Singly Linked List?

**Singly linked list** is a chain of nodes, where each node contains:
- **Data** — The value
- **Next** — Pointer to next node

**Structure:**
```pascal
type
  PNode = ^TNode;
  TNode = record
    Data: integer;
    Next: PNode;
  end;
  
  TLinkedList = record
    Head: PNode;  // First node (or nil if empty)
  end;
```

**Visual representation:**
```
Head → [Data: 10] → [Data: 20] → [Data: 30] → nil
```

### Creating Nodes

**Allocate new node:**
```pascal
function CreateNode(value: integer): PNode;
var node: PNode;
begin
  New(node);
  node^.Data := value;
  node^.Next := nil;
  CreateNode := node;
end;
```

### Basic Operations

**Initialize empty list:**
```pascal
procedure ListInit(var list: TLinkedList);
begin
  list.Head := nil;
end;
```

**Add to beginning:**
```pascal
procedure ListPrepend(var list: TLinkedList; value: integer);
var node: PNode;
begin
  node := CreateNode(value);
  node^.Next := list.Head;
  list.Head := node;
end;
```

**Add to end:**
```pascal
procedure ListAppend(var list: TLinkedList; value: integer);
var node, current: PNode;
begin
  node := CreateNode(value);
  
  if list.Head = nil then
    list.Head := node
  else
  begin
    current := list.Head;
    while current^.Next <> nil do
      current := current^.Next;
    current^.Next := node;
  end;
end;
```

**Traverse list:**
```pascal
procedure ListPrint(const list: TLinkedList);
var current: PNode;
begin
  current := list.Head;
  while current <> nil do
  begin
    WriteLn(current^.Data);
    current := current^.Next;
  end;
end;
```

**Find node:**
```pascal
function ListFind(const list: TLinkedList; value: integer): PNode;
var current: PNode;
begin
  current := list.Head;
  while current <> nil do
  begin
    if current^.Data = value then
    begin
      ListFind := current;
      Exit;
    end;
    current := current^.Next;
  end;
  ListFind := nil;  // Not found
end;
```

**Delete node:**
```pascal
procedure ListDelete(var list: TLinkedList; value: integer);
var current, prev: PNode;
begin
  if list.Head = nil then
    Exit;
  
  // Special case: delete head
  if list.Head^.Data = value then
  begin
    current := list.Head;
    list.Head := list.Head^.Next;
    Dispose(current);
    Exit;
  end;
  
  // Find node to delete
  prev := list.Head;
  current := list.Head^.Next;
  while current <> nil do
  begin
    if current^.Data = value then
    begin
      prev^.Next := current^.Next;
      Dispose(current);
      Exit;
    end;
    prev := current;
    current := current^.Next;
  end;
end;
```

**Clear entire list:**
```pascal
procedure ListClear(var list: TLinkedList);
var current, next: PNode;
begin
  current := list.Head;
  while current <> nil do
  begin
    next := current^.Next;
    Dispose(current);
    current := next;
  end;
  list.Head := nil;
end;
```

### Advantages and Disadvantages

**Advantages:**
- **Dynamic size** — Can grow without limit (until memory runs out)
- **Efficient insertion** — O(1) at beginning, O(n) at end
- **Efficient deletion** — O(1) at beginning, O(n) elsewhere
- **No wasted space** — Only allocate what you use

**Disadvantages:**
- **No random access** — Must traverse from head (O(n))
- **Extra memory** — Each node needs pointer (overhead)
- **Cache-unfriendly** — Nodes may be scattered in memory
- **More complex** — Pointer management required

---

## Doubly Linked Lists

### What is a Doubly Linked List?

**Doubly linked list** is like a singly linked list, but each node points to both next and previous:

```pascal
type
  PNode = ^TNode;
  TNode = record
    Data: integer;
    Next: PNode;   // Pointer to next node
    Prev: PNode;   // Pointer to previous node
  end;
  
  TDoublyLinkedList = record
    Head: PNode;   // First node (or nil)
    Tail: PNode;   // Last node (or nil)
  end;
```

**Visual representation:**
```
Head → [Data: 10] ↔ [Data: 20] ↔ [Data: 30] ← Tail
        Prev: nil    Prev: 10     Prev: 20
        Next: 20     Next: 30     Next: nil
```

**Advantages over singly linked:**
- **Bidirectional traversal** — Can go forward or backward
- **Efficient deletion** — O(1) if you have node pointer
- **Easier insertion** — Can insert before or after node

### Creating Nodes

**Allocate new node:**
```pascal
function CreateNode(value: integer): PNode;
var node: PNode;
begin
  New(node);
  node^.Data := value;
  node^.Next := nil;
  node^.Prev := nil;
  CreateNode := node;
end;
```

### Basic Operations

**Initialize empty list:**
```pascal
procedure ListInit(var list: TDoublyLinkedList);
begin
  list.Head := nil;
  list.Tail := nil;
end;
```

**Add to beginning:**
```pascal
procedure ListPrepend(var list: TDoublyLinkedList; value: integer);
var node: PNode;
begin
  node := CreateNode(value);
  
  if list.Head = nil then
  begin
    // Empty list
    list.Head := node;
    list.Tail := node;
  end
  else
  begin
    node^.Next := list.Head;
    list.Head^.Prev := node;
    list.Head := node;
  end;
end;
```

**Add to end:**
```pascal
procedure ListAppend(var list: TDoublyLinkedList; value: integer);
var node: PNode;
begin
  node := CreateNode(value);
  
  if list.Tail = nil then
  begin
    // Empty list
    list.Head := node;
    list.Tail := node;
  end
  else
  begin
    node^.Prev := list.Tail;
    list.Tail^.Next := node;
    list.Tail := node;
  end;
end;
```

**Insert after node:**
```pascal
procedure ListInsertAfter(var list: TDoublyLinkedList; after: PNode; value: integer);
var node: PNode;
begin
  if after = nil then
    Exit;
  
  node := CreateNode(value);
  node^.Prev := after;
  node^.Next := after^.Next;
  
  if after^.Next <> nil then
    after^.Next^.Prev := node
  else
    list.Tail := node;  // Inserting at end
  
  after^.Next := node;
end;
```

**Insert before node:**
```pascal
procedure ListInsertBefore(var list: TDoublyLinkedList; before: PNode; value: integer);
var node: PNode;
begin
  if before = nil then
    Exit;
  
  node := CreateNode(value);
  node^.Next := before;
  node^.Prev := before^.Prev;
  
  if before^.Prev <> nil then
    before^.Prev^.Next := node
  else
    list.Head := node;  // Inserting at beginning
  
  before^.Prev := node;
end;
```

**Delete node:**
```pascal
procedure ListDeleteNode(var list: TDoublyLinkedList; node: PNode);
begin
  if node = nil then
    Exit;
  
  // Update previous node
  if node^.Prev <> nil then
    node^.Prev^.Next := node^.Next
  else
    list.Head := node^.Next;  // Deleting head
  
  // Update next node
  if node^.Next <> nil then
    node^.Next^.Prev := node^.Prev
  else
    list.Tail := node^.Prev;  // Deleting tail
  
  Dispose(node);
end;
```

**Traverse forward:**
```pascal
procedure ListPrintForward(const list: TDoublyLinkedList);
var current: PNode;
begin
  current := list.Head;
  while current <> nil do
  begin
    WriteLn(current^.Data);
    current := current^.Next;
  end;
end;
```

**Traverse backward:**
```pascal
procedure ListPrintBackward(const list: TDoublyLinkedList);
var current: PNode;
begin
  current := list.Tail;
  while current <> nil do
  begin
    WriteLn(current^.Data);
    current := current^.Prev;
  end;
end;
```

---

## Comparison: Arrays vs Lists vs Linked Lists

### SuperPascal List (Heterogeneous)

| Operation | Time Complexity | Notes |
|-----------|----------------|-------|
| Access by index | O(1) | Fast random access |
| Insert at end | O(1) amortized | May need to grow |
| Insert at beginning | O(n) | Must shift all elements |
| Insert in middle | O(n) | Must shift elements |
| Delete from end | O(1) | Just decrement count |
| Delete from beginning | O(n) | Must shift all elements |
| Delete from middle | O(n) | Must shift elements |
| Search | O(n) | Linear search |
| Type checking | O(1) | Runtime type check |

**Best for:**
- Mixed types (heterogeneous)
- Unknown size
- Need to store different types together
- Python-like behavior

### Array-Based List (Homogeneous)

| Operation | Time Complexity | Notes |
|-----------|----------------|-------|
| Access by index | O(1) | Fast random access |
| Insert at end | O(1) | If space available |
| Insert at beginning | O(n) | Must shift all elements |
| Insert in middle | O(n) | Must shift elements |
| Delete from end | O(1) | Just decrement count |
| Delete from beginning | O(n) | Must shift all elements |
| Delete from middle | O(n) | Must shift elements |
| Search | O(n) | Linear search |

**Best for:**
- Random access needed
- Mostly adding/removing at end
- Known maximum size
- Performance-critical code

### Singly Linked List

| Operation | Time Complexity | Notes |
|-----------|----------------|-------|
| Access by index | O(n) | Must traverse from head |
| Insert at beginning | O(1) | Fast |
| Insert at end | O(n) | Must traverse to end |
| Insert after node | O(1) | If you have node pointer |
| Delete from beginning | O(1) | Fast |
| Delete from end | O(n) | Must traverse to end |
| Delete node | O(n) | Must find node first |
| Search | O(n) | Linear search |

**Best for:**
- Frequent insertion/deletion at beginning
- Unknown size
- Don't need random access
- Memory efficiency important

### Doubly Linked List

| Operation | Time Complexity | Notes |
|-----------|----------------|-------|
| Access by index | O(n) | Must traverse |
| Insert at beginning | O(1) | Fast |
| Insert at end | O(1) | Fast (with tail pointer) |
| Insert after/before node | O(1) | If you have node pointer |
| Delete from beginning | O(1) | Fast |
| Delete from end | O(1) | Fast (with tail pointer) |
| Delete node | O(1) | If you have node pointer |
| Search | O(n) | Linear search |
| Traverse backward | O(n) | Can go backward |

**Best for:**
- Need bidirectional traversal
- Frequent insertion/deletion at both ends
- Need to delete arbitrary nodes efficiently
| Unknown size

---

## Common Patterns

### Pattern 1: Stack (LIFO)

**Use array-based list or singly linked list:**
```pascal
// Array-based stack
procedure StackPush(var stack: TList; value: integer);
begin
  ListAdd(stack, value);
end;

function StackPop(var stack: TList): integer;
begin
  StackPop := ListRemove(stack);
end;
```

### Pattern 2: Queue (FIFO)

**Use array-based list with two indices, or doubly linked list:**
```pascal
// Array-based queue
type
  TQueue = record
    Data: array[0..99] of integer;
    Front: integer;  // Index of first element
    Rear: integer;   // Index of last element
    Count: integer;
  end;

procedure QueueEnqueue(var queue: TQueue; value: integer);
begin
  if queue.Count < Length(queue.Data) then
  begin
    queue.Rear := (queue.Rear + 1) mod Length(queue.Data);
    queue.Data[queue.Rear] := value;
    queue.Count := queue.Count + 1;
  end;
end;

function QueueDequeue(var queue: TQueue): integer;
begin
  if queue.Count > 0 then
  begin
    QueueDequeue := queue.Data[queue.Front];
    queue.Front := (queue.Front + 1) mod Length(queue.Data);
    queue.Count := queue.Count - 1;
  end;
end;
```

### Pattern 3: Priority Queue

**Use array-based list with sorting:**
```pascal
procedure PriorityQueueInsert(var queue: TList; value: integer);
var i: integer;
begin
  // Insert in sorted order
  i := queue.Count;
  while (i > 0) and (queue.Data[i - 1] > value) do
  begin
    queue.Data[i] := queue.Data[i - 1];
    i := i - 1;
  end;
  queue.Data[i] := value;
  queue.Count := queue.Count + 1;
end;
```

---

## Best Practices

### 1. Choose the Right Structure

**Use array-based list when:**
- Need random access
- Mostly adding/removing at end
- Known maximum size
- Performance is critical

**Use singly linked list when:**
- Frequent insertion/deletion at beginning
- Unknown size
- Don't need random access
- Memory efficiency important

**Use doubly linked list when:**
- Need bidirectional traversal
- Frequent insertion/deletion at both ends
- Need efficient deletion of arbitrary nodes

### 2. Memory Management

**Always dispose nodes:**
```pascal
// Good: Dispose after use
Dispose(node);

// Bad: Memory leak
// node := nil;  // Doesn't free memory!
```

**Clear lists properly:**
```pascal
// Always clear linked lists
ListClear(list);
```

### 3. Check for Nil

**Always check pointers:**
```pascal
if node <> nil then
  Process(node^.Data);
```

### 4. Handle Empty Lists

**Check for empty:**
```pascal
function ListIsEmpty(const list: TLinkedList): boolean;
begin
  ListIsEmpty := list.Head = nil;
end;
```

---

## Platform Considerations

### Memory

**Linked lists:**
- **Heap allocation** — Nodes allocated on heap
- **Pointer overhead** — Each node needs pointer(s)
- **Memory fragmentation** — Nodes may be scattered
- **On ZealZ80** — Limited heap, use carefully

**Array-based lists:**
- **Stack allocation** — Array on stack
- **No overhead** — Just data
- **Contiguous memory** — Cache-friendly
- **On ZealZ80** — Better for small lists

### Performance

**Array-based:**
- **Fast access** — O(1) random access
- **Cache-friendly** — Contiguous memory
- **Good for small lists** — Up to ~100 elements

**Linked lists:**
- **Flexible** — Can grow without limit
- **Cache-unfriendly** — Nodes scattered
- **Good for large lists** — When size unknown

---

## Summary

**Key Concepts:**
- **SuperPascal lists** — Built-in heterogeneous lists (like Python)
- **Array-based lists** — Fixed-size array with count variable (homogeneous)
- **Singly linked lists** — Nodes with next pointer (homogeneous)
- **Doubly linked lists** — Nodes with next and prev pointers (homogeneous)
- **Dynamic size** — Can grow and shrink
- **Trade-offs** — Speed vs flexibility, homogeneous vs heterogeneous

**SuperPascal Lists (Heterogeneous):**
- Can contain any type or combination of types
- Dynamic size
- Fast random access (O(1))
- Runtime type checking
- Python-like behavior

**Array-Based Lists (Homogeneous):**
- Fast random access (O(1))
- Efficient for adding/removing at end
- Fixed maximum size
- Cache-friendly
- Compile-time type safety

**Singly Linked Lists (Homogeneous):**
- Dynamic size
- Efficient insertion/deletion at beginning (O(1))
- No random access (must traverse)
- Memory overhead (pointers)

**Doubly Linked Lists (Homogeneous):**
- Dynamic size
- Bidirectional traversal
- Efficient insertion/deletion at both ends (O(1))
- More memory overhead (two pointers per node)

**When to Use:**
- **SuperPascal lists** — Need mixed types, Python-like behavior
- **Array-based** — Random access needed, known max size, single type
- **Singly linked** — Frequent operations at beginning, unknown size, single type
- **Doubly linked** — Need bidirectional traversal, frequent operations at both ends, single type

**Next:** Continue with strings and text processing, or explore more advanced data structures.

---

**Previous Section:** [Sets and Unions](./06_SetsAndUnions.md)  
**Next Chapter:** [Chapter 07: Strings and Text Processing](../09_StringsAndTextProcessing/README.md)  
**Language Specification:** See [03_TypeSystem.md](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

