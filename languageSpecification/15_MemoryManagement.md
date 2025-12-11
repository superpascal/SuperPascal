# SuperPascal Language Specification — Memory Management

## Complete Memory Management Strategy

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

---

## 1. Overview

SuperPascal uses **manual memory management** with no garbage collection. The hybrid model (value types + reference types) requires a comprehensive strategy to prevent memory leaks, dangling pointers, and double-free errors.

**Core Principle:** The programmer is responsible for all memory management. The language provides all necessary facilities, but no automatic cleanup.

---

## 2. Memory Management Philosophy

### 2.1 No Garbage Collection

**SuperPascal does not provide garbage collection:**
- No automatic memory reclamation
- No reference counting
- No mark-and-sweep
- No generational GC

**Why:**
- **Performance** — GC adds overhead and unpredictability
- **Control** — Systems programming requires precise memory control
- **Retro platforms** — GC is too expensive for 8-bit/16-bit systems
- **Educational** — Students learn memory management fundamentals

### 2.2 Manual Management Required

**Every allocation must have a corresponding deallocation:**
- `New` → `Dispose`
- `GetMem` → `FreeMem`
- `Create` → `Destroy`
- Resource acquisition → Resource release

**The compiler does not track ownership or lifetime automatically.**

---

## 3. Value Types: Records and Structs

### 3.1 Stack Allocation (Default)

**Records are value types, allocated on stack by default:**

```pascal
type TVec2 = record
  X, Y: integer;
end;

procedure Test;
var
  v: TVec2;  // Stack-allocated
begin
  v.X := 10;
  v.Y := 20;
  // v is automatically cleaned up when procedure exits
end;
```

**Properties:**
- **Allocation**: Automatic (on stack)
- **Lifetime**: Scope-bound (until end of procedure/function)
- **Cleanup**: Automatic (stack unwinding)
- **Copy semantics**: Assignment copies the value
- **Performance**: Fast (no heap allocation)

### 3.2 Heap Allocation (Explicit)

**Records can be heap-allocated using pointers:**

```pascal
type TVec2 = record
  X, Y: integer;
end;

type PVec2 = ^TVec2;

procedure Test;
var
  p: PVec2;
begin
  New(p);        // Allocate on heap
  p^.X := 10;
  p^.Y := 20;
  // ... use p^ ...
  Dispose(p);    // MUST free manually
end;
```

**Properties:**
- **Allocation**: Manual (`New`)
- **Lifetime**: Until `Dispose` is called
- **Cleanup**: Manual (`Dispose`)
- **Copy semantics**: Pointer assignment copies reference, not value
- **Performance**: Slower (heap allocation)

### 3.3 Record Assignment Semantics

**Value types are copied on assignment:**

```pascal
var
  v1, v2: TVec2;
begin
  v1.X := 10;
  v1.Y := 20;
  v2 := v1;      // Copies all fields (value copy)
  v2.X := 30;    // v1.X is still 10 (independent copies)
end;
```

**Pointer assignment (reference copy):**
```pascal
var
  p1, p2: PVec2;
begin
  New(p1);
  p1^.X := 10;
  p2 := p1;      // Copies pointer (reference copy)
  p2^.X := 30;   // p1^.X is now 30 (same object)
  Dispose(p1);   // Free once (p1 and p2 point to same object)
  // p2 is now dangling pointer!
end;
```

---

## 4. Reference Types: Classes

### 4.1 Always Heap-Allocated

**Classes are always allocated on the heap:**

```pascal
type TEntity = class
public
  ID: word;
  constructor Create;
  destructor Destroy;
end;

procedure Test;
var
  entity: TEntity;
begin
  entity := TEntity.Create;  // Allocated on heap
  entity.ID := 42;
  // ... use entity ...
  entity.Destroy;             // MUST free manually
end;
```

**Properties:**
- **Allocation**: Always heap (via `Create`)
- **Lifetime**: Until `Destroy` is called
- **Cleanup**: Manual (`Destroy`)
- **Copy semantics**: Assignment copies reference, not value
- **Performance**: Slower (heap allocation, vtable overhead)

### 4.2 Constructor and Destructor

**Classes use constructors and destructors:**

```pascal
type TResource = class
private
  Handle: word;
public
  constructor Create;
  destructor Destroy;
end;

constructor TResource.Create;
begin
  Handle := AcquireResource;
end;

destructor TResource.Destroy;
begin
  ReleaseResource(Handle);
end;
```

**Usage:**
```pascal
var
  res: TResource;
begin
  res := TResource.Create;  // Calls constructor, allocates
  // ... use res ...
  res.Destroy;              // Calls destructor, deallocates
end;
```

### 4.3 Class Assignment Semantics

**Classes use reference semantics:**

```pascal
var
  obj1, obj2: TEntity;
begin
  obj1 := TEntity.Create;
  obj1.ID := 10;
  obj2 := obj1;      // Copies reference (not value)
  obj2.ID := 20;     // obj1.ID is now 20 (same object)
  obj1.Destroy;      // Free once
  // obj2 is now dangling reference!
end;
```

---

## 5. Memory Management Facilities

### 5.1 New and Dispose

**For records and typed pointers:**

```pascal
// Allocate record on heap
var p: ^TRecord;
New(p);
// ... use p^ ...
Dispose(p);

// Allocate array on heap
type PIntArray = ^array[0..99] of integer;
var arr: PIntArray;
New(arr);
// ... use arr^ ...
Dispose(arr);
```

**Syntax:**
```pascal
procedure New(var P: Pointer);
procedure Dispose(var P: Pointer);
```

**Rules:**
- `New` allocates memory for the type
- `Dispose` frees the memory
- Must call `Dispose` for every `New`
- `Dispose` on `nil` is safe (no-op)
- `Dispose` on already-freed pointer is undefined behavior

### 5.2 GetMem and FreeMem

**For raw memory allocation:**

```pascal
var
  p: pointer;
  size: word;
begin
  size := 100;  // 100 bytes
  GetMem(p, size);
  // ... use p ...
  FreeMem(p, size);
end;
```

**Syntax:**
```pascal
procedure GetMem(var P: pointer; Size: word);
procedure FreeMem(var P: pointer; Size: word);
```

**Rules:**
- `GetMem` allocates raw bytes
- `FreeMem` frees raw bytes
- Must match size in `FreeMem`
- `FreeMem` on `nil` is safe (no-op)
- `FreeMem` on already-freed pointer is undefined behavior

### 5.3 Create and Destroy

**For class instances:**

```pascal
var
  obj: TMyClass;
begin
  obj := TMyClass.Create;  // Allocates and initializes
  // ... use obj ...
  obj.Destroy;             // Cleans up and deallocates
end;
```

**Rules:**
- `Create` allocates memory and calls constructor
- `Destroy` calls destructor and deallocates memory
- Must call `Destroy` for every `Create`
- `Destroy` on `nil` is safe (no-op)
- `Destroy` on already-destroyed object is undefined behavior

### 5.4 Try-Finally for Guaranteed Cleanup

**Use `try/finally` to ensure cleanup even if exceptions occur:**

```pascal
var
  obj: TResource;
begin
  obj := TResource.Create;
  try
    // ... use obj ...
    // Even if exception occurs, finally block executes
  finally
    obj.Destroy;  // Always called
  end;
end;
```

**Pattern for all allocations:**
```pascal
// Record pointer
var p: ^TRecord;
New(p);
try
  // ... use p^ ...
finally
  Dispose(p);
end;

// Raw memory
var p: pointer;
GetMem(p, size);
try
  // ... use p ...
finally
  FreeMem(p, size);
end;

// Class instance
var obj: TMyClass;
obj := TMyClass.Create;
try
  // ... use obj ...
finally
  obj.Destroy;
end;
```

---

## 6. Ownership Patterns

### 6.1 Single Ownership

**One owner is responsible for freeing:**

```pascal
type TManager = class
private
  FResource: TResource;
public
  constructor Create;
  destructor Destroy;
  procedure SetResource(AResource: TResource);
end;

constructor TManager.Create;
begin
  FResource := nil;  // No resource initially
end;

destructor TManager.Destroy;
begin
  if FResource <> nil then
    FResource.Destroy;  // Owner frees resource
end;

procedure TManager.SetResource(AResource: TResource);
begin
  if FResource <> nil then
    FResource.Destroy;  // Free old resource
  FResource := AResource;  // Take ownership
end;
```

### 6.2 Transfer of Ownership

**When transferring ownership, document clearly:**

```pascal
// Function that transfers ownership to caller
function CreateResource: TResource;
begin
  CreateResource := TResource.Create;  // Caller owns it
end;

// Procedure that takes ownership
procedure TakeOwnership(AResource: TResource);
begin
  // AResource is now owned by this procedure
  // Caller must not free it
  // This procedure will free it
end;
```

### 6.3 No Ownership (Borrowed Reference)

**Sometimes objects are borrowed (not owned):**

```pascal
// Procedure that uses but doesn't own
procedure UseResource(AResource: TResource);
begin
  // Uses AResource but doesn't own it
  // Caller still owns it
  // Must not free AResource here
end;
```

**Documentation pattern:**
```pascal
// Takes ownership - caller must not free
procedure TakeOwnership(AResource: TResource);

// Borrows reference - caller still owns
procedure UseResource(AResource: TResource);

// Transfers ownership to caller
function CreateResource: TResource;
```

---

## 7. Common Patterns

### 7.1 RAII-Like Pattern

**Resource Acquisition Is Initialization (using try/finally):**

```pascal
procedure ProcessFile;
var
  file: TFile;
begin
  file := TFile.Open('data.txt');
  try
    // ... process file ...
  finally
    file.Close;  // Always closed
  end;
end;
```

### 7.2 Factory Pattern

**Factory creates and returns objects:**

```pascal
function CreateEntity(ID: word): TEntity;
begin
  CreateEntity := TEntity.Create;
  CreateEntity.ID := ID;
  // Caller owns the returned object
end;

// Usage
var
  entity: TEntity;
begin
  entity := CreateEntity(42);
  try
    // ... use entity ...
  finally
    entity.Destroy;  // Caller must free
  end;
end;
```

### 7.3 Container Pattern

**Container owns and manages objects:**

```pascal
type TEntityList = class
private
  FEntities: array[0..99] of TEntity;
  FCount: word;
public
  constructor Create;
  destructor Destroy;
  procedure Add(Entity: TEntity);  // Takes ownership
  procedure Clear;
end;

constructor TEntityList.Create;
begin
  FCount := 0;
end;

destructor TEntityList.Destroy;
begin
  Clear;  // Free all owned entities
end;

procedure TEntityList.Add(Entity: TEntity);
begin
  if FCount < 100 then
  begin
    FEntities[FCount] := Entity;  // Take ownership
    FCount := FCount + 1;
  end
  else
    Entity.Destroy;  // Can't add, free it
end;

procedure TEntityList.Clear;
var
  i: word;
begin
  for i := 0 to FCount - 1 do
    FEntities[i].Destroy;  // Free all owned entities
  FCount := 0;
end;
```

---

## 8. Memory Safety Rules

### 8.1 Allocation Rules

**Every allocation must be paired with deallocation:**
- `New` → `Dispose`
- `GetMem` → `FreeMem`
- `Create` → `Destroy`

**Use `try/finally` to guarantee cleanup:**
```pascal
New(p);
try
  // ... use p ...
finally
  Dispose(p);  // Always freed
end;
```

### 8.2 Null Safety

**Check for `nil` before use:**

```pascal
var
  obj: TEntity;
begin
  obj := GetEntity;  // May return nil
  if obj <> nil then
  begin
    obj.Update;      // Safe to use
    obj.Destroy;     // Safe to free
  end;
end;
```

**Destructors and Dispose handle `nil` safely:**
```pascal
var
  obj: TEntity;
begin
  obj := nil;
  obj.Destroy;  // Safe: no-op on nil
  Dispose(obj); // Safe: no-op on nil
end;
```

### 8.3 Double-Free Prevention

**Set pointer to `nil` after freeing:**

```pascal
var
  obj: TEntity;
begin
  obj := TEntity.Create;
  try
    // ... use obj ...
  finally
    obj.Destroy;
    obj := nil;  // Prevent double-free
  end;
end;
```

**Or use a procedure:**
```pascal
procedure FreeAndNil(var Obj: TEntity);
begin
  if Obj <> nil then
  begin
    Obj.Destroy;
    Obj := nil;
  end;
end;
```

### 8.4 Dangling Pointer Prevention

**Don't use pointers after freeing:**

```pascal
var
  obj: TEntity;
begin
  obj := TEntity.Create;
  obj.Destroy;
  // obj is now dangling - don't use it!
  // obj.Update;  // ERROR: undefined behavior
  obj := nil;  // Mark as invalid
end;
```

---

## 9. Memory Leak Prevention

### 9.1 Always Free What You Allocate

**Match every allocation with deallocation:**

```pascal
// ❌ BAD: Memory leak
procedure BadExample;
var
  obj: TEntity;
begin
  obj := TEntity.Create;
  // Forgot to call obj.Destroy - memory leak!
end;

// ✅ GOOD: Properly freed
procedure GoodExample;
var
  obj: TEntity;
begin
  obj := TEntity.Create;
  try
    // ... use obj ...
  finally
    obj.Destroy;  // Always freed
  end;
end;
```

### 9.2 Free in Reverse Order of Allocation

**Free in opposite order of creation:**

```pascal
var
  obj1, obj2, obj3: TEntity;
begin
  obj1 := TEntity.Create;
  obj2 := TEntity.Create;
  obj3 := TEntity.Create;
  try
    // ... use objects ...
  finally
    obj3.Destroy;  // Free last first
    obj2.Destroy;
    obj1.Destroy;  // Free first last
  end;
end;
```

### 9.3 Clear Containers

**Free all objects in containers:**

```pascal
type TEntityList = class
private
  FEntities: array[0..99] of TEntity;
  FCount: word;
public
  destructor Destroy;
  procedure Clear;
end;

destructor TEntityList.Destroy;
begin
  Clear;  // Free all owned entities
end;

procedure TEntityList.Clear;
var
  i: word;
begin
  for i := 0 to FCount - 1 do
    FEntities[i].Destroy;
  FCount := 0;
end;
```

---

## 10. Best Practices

### 10.1 Use Try-Finally for All Allocations

**Always use `try/finally` for heap allocations:**

```pascal
// ✅ GOOD
var obj: TEntity;
obj := TEntity.Create;
try
  // ... use obj ...
finally
  obj.Destroy;
end;
```

### 10.2 Prefer Stack Allocation When Possible

**Use stack allocation for records when possible:**

```pascal
// ✅ GOOD: Stack allocation (fast, automatic cleanup)
var
  vec: TVec2;
begin
  vec.X := 10;
  vec.Y := 20;
  // Automatically cleaned up
end;

// ❌ BAD: Unnecessary heap allocation
var
  p: ^TVec2;
begin
  New(p);
  try
    p^.X := 10;
    p^.Y := 20;
  finally
    Dispose(p);
  end;
end;
```

### 10.3 Document Ownership

**Document who owns what:**

```pascal
// Takes ownership - caller must not free
procedure TakeOwnership(AResource: TResource);

// Borrows reference - caller still owns
procedure UseResource(AResource: TResource);

// Transfers ownership to caller
function CreateResource: TResource;
```

### 10.4 Use Containers for Multiple Objects

**Use containers to manage multiple objects:**

```pascal
var
  list: TEntityList;
begin
  list := TEntityList.Create;
  try
    list.Add(TEntity.Create);  // List takes ownership
    list.Add(TEntity.Create);
    // ... use list ...
  finally
    list.Destroy;  // Frees all entities
  end;
end;
```

### 10.5 Set Pointers to Nil After Freeing

**Prevent double-free and dangling pointers:**

```pascal
var
  obj: TEntity;
begin
  obj := TEntity.Create;
  try
    // ... use obj ...
  finally
    obj.Destroy;
    obj := nil;  // Mark as invalid
  end;
end;
```

---

## 11. Lifetime Control and Value Passing

### 11.1 Full Lifetime Control

**Users have complete control over object lifetimes:**

```pascal
// Stack-allocated record - lifetime controlled by scope
procedure Test1;
var
  vec: TVec2;  // Created when procedure starts
begin
  vec.X := 10;
  // vec automatically destroyed when procedure ends
end;

// Heap-allocated record - lifetime controlled manually
procedure Test2;
var
  p: ^TVec2;
begin
  New(p);      // User controls: allocate now
  p^.X := 10;
  Dispose(p);  // User controls: free now (or later)
end;

// Class instance - lifetime controlled manually
procedure Test3;
var
  obj: TEntity;
begin
  obj := TEntity.Create;  // User controls: allocate now
  obj.ID := 42;
  obj.Destroy;            // User controls: free now (or later)
end;
```

**Lifetime control mechanisms:**
- **Stack allocation**: Lifetime = scope (automatic)
- **Heap allocation**: Lifetime = until explicit free (manual)
- **Global variables**: Lifetime = entire program
- **Unit initialization**: Lifetime = program start to end

### 11.2 Passing Values Around

**Records: Pass by value (copied) or by reference:**

```pascal
// Pass by value (default) - copies the record
procedure ProcessPoint(p: TVec2);
begin
  p.X := 100;  // Modifies copy, doesn't affect caller
end;

var
  point: TVec2;
begin
  point.X := 10;
  ProcessPoint(point);  // point.X is still 10 (not 100)
end;
```

**Pass by reference (`var`) - shares the same record:**
```pascal
// Pass by reference - shares the same record
procedure ModifyPoint(var p: TVec2);
begin
  p.X := 100;  // Modifies original, affects caller
end;

var
  point: TVec2;
begin
  point.X := 10;
  ModifyPoint(point);  // point.X is now 100
end;
```

**Pass by const reference (`const`) - efficient for large records:**
```pascal
// Pass by const reference - efficient, read-only
procedure ReadPoint(const p: TVec2);
begin
  WriteLn(p.X, ', ', p.Y);  // Can read but not modify
  // p.X := 100;  // ERROR: const parameter cannot be modified
end;
```

**Classes: Always pass by reference (they're reference types):**
```pascal
// Classes are always passed by reference
procedure ProcessEntity(e: TEntity);
begin
  e.ID := 100;  // Modifies the same object (reference semantics)
end;

var
  entity: TEntity;
begin
  entity := TEntity.Create;
  entity.ID := 10;
  ProcessEntity(entity);  // entity.ID is now 100 (same object)
  entity.Destroy;
end;
```

### 11.3 Returning Values

**Return records by value (copied):**
```pascal
function CreatePoint(x, y: integer): TVec2;
begin
  CreatePoint.X := x;  // Returns a copy
  CreatePoint.Y := y;
end;

var
  p: TVec2;
begin
  p := CreatePoint(10, 20);  // Receives a copy
  // p is independent, can be modified
end;
```

**Return classes by reference (ownership transfer):**
```pascal
// Returns reference - caller takes ownership
function CreateEntity(ID: word): TEntity;
begin
  CreateEntity := TEntity.Create;  // Allocated here
  CreateEntity.ID := ID;
  // Caller owns the returned object
end;

var
  entity: TEntity;
begin
  entity := CreateEntity(42);  // Takes ownership
  try
    // ... use entity ...
  finally
    entity.Destroy;  // Caller must free
  end;
end;
```

**Return pointers (explicit ownership):**
```pascal
// Returns pointer - caller takes ownership
function AllocatePoint(x, y: integer): ^TVec2;
var
  p: ^TVec2;
begin
  New(p);  // Allocated here
  p^.X := x;
  p^.Y := y;
  AllocatePoint := p;  // Caller owns it
end;

var
  p: ^TVec2;
begin
  p := AllocatePoint(10, 20);  // Takes ownership
  try
    // ... use p^ ...
  finally
    Dispose(p);  // Caller must free
  end;
end;
```

### 11.4 Ownership Transfer Patterns

**Transfer ownership via return value:**
```pascal
// Factory function - transfers ownership to caller
function CreateResource: TResource;
begin
  CreateResource := TResource.Create;  // Allocated here
  // Caller owns it, must free it
end;
```

**Transfer ownership via parameter:**
```pascal
// Takes ownership - caller must not free
procedure TakeOwnership(AResource: TResource);
begin
  // AResource is now owned by this procedure
  // This procedure will free it (or store it for later)
  // Caller must not free it
end;

// Usage
var
  res: TResource;
begin
  res := TResource.Create;
  TakeOwnership(res);  // Ownership transferred
  // res is now invalid (don't use or free it)
  res := nil;  // Mark as invalid
end;
```

**Borrow reference (no ownership transfer):**
```pascal
// Borrows reference - caller still owns
procedure UseResource(AResource: TResource);
begin
  // Uses AResource but doesn't own it
  // Caller still owns it, must free it
  AResource.DoSomething;
end;

// Usage
var
  res: TResource;
begin
  res := TResource.Create;
  try
    UseResource(res);  // Borrows reference
    // res still valid, caller still owns
  finally
    res.Destroy;  // Caller frees it
  end;
end;
```

### 11.5 Lifetime Examples

**Example 1: Stack-allocated record (automatic lifetime):**
```pascal
procedure ProcessData;
var
  data: TDataRecord;  // Created when procedure starts
begin
  data.Value := 42;
  ProcessRecord(data);  // Pass by value (copy)
  // data automatically destroyed when procedure ends
end;
```

**Example 2: Heap-allocated record (manual lifetime):**
```pascal
procedure ProcessData;
var
  p: ^TDataRecord;
begin
  New(p);  // User controls: allocate now
  try
    p^.Value := 42;
    ProcessRecordPtr(p);  // Pass pointer (reference)
    // ... use p^ ...
  finally
    Dispose(p);  // User controls: free now
  end;
end;
```

**Example 3: Class instance (manual lifetime):**
```pascal
procedure ProcessEntity;
var
  entity: TEntity;
begin
  entity := TEntity.Create;  // User controls: allocate now
  try
    entity.ID := 42;
    ProcessEntityObj(entity);  // Pass reference (classes always reference)
    // ... use entity ...
  finally
    entity.Destroy;  // User controls: free now
  end;
end;
```

**Example 4: Return value with ownership:**
```pascal
function GetEntity: TEntity;
begin
  GetEntity := TEntity.Create;  // Allocated here
  GetEntity.ID := 42;
  // Caller takes ownership
end;

procedure Main;
var
  entity: TEntity;
begin
  entity := GetEntity;  // Takes ownership
  try
    // ... use entity ...
  finally
    entity.Destroy;  // Caller must free
  end;
end;
```

### 11.6 Lifetime Control Summary

**Users have full control:**
- ✅ **When to allocate**: `New`, `Create`, stack allocation
- ✅ **When to free**: `Dispose`, `Destroy`, scope exit
- ✅ **How to pass**: By value, by reference (`var`), by const reference (`const`)
- ✅ **Ownership transfer**: Via return values, via parameters, via documentation
- ✅ **Lifetime scope**: Stack (automatic), heap (manual), global (program lifetime)

**No automatic lifetime management:**
- ❌ No garbage collection
- ❌ No reference counting
- ❌ No automatic cleanup
- ❌ No ownership tracking by compiler

**The programmer is responsible for:**
- Matching every allocation with deallocation
- Tracking ownership
- Documenting ownership transfer
- Using `try/finally` for guaranteed cleanup

---

## 12. Summary

### 11.1 Memory Management Rules

1. **No garbage collection** — All memory is manually managed
2. **Every allocation must be freed** — Match `New`/`Dispose`, `GetMem`/`FreeMem`, `Create`/`Destroy`
3. **Use `try/finally`** — Guarantee cleanup even with exceptions
4. **Document ownership** — Make it clear who owns what
5. **Prefer stack allocation** — Use heap only when necessary
6. **Set to `nil` after free** — Prevent double-free and dangling pointers

### 11.2 Value Types (Records)

- **Stack-allocated by default** — Automatic cleanup
- **Can be heap-allocated** — Use `New`/`Dispose` with pointers
- **Value semantics** — Assignment copies value

### 11.3 Reference Types (Classes)

- **Always heap-allocated** — Use `Create`/`Destroy`
- **Reference semantics** — Assignment copies reference
- **Must be manually freed** — Call `Destroy` for every `Create`

### 11.4 Facilities Provided

- `New`/`Dispose` — For records and typed pointers
- `GetMem`/`FreeMem` — For raw memory
- `Create`/`Destroy` — For class instances
- `try/finally` — For guaranteed cleanup
- Ownership patterns — For managing object lifetimes

---

**End of Memory Management Specification**

