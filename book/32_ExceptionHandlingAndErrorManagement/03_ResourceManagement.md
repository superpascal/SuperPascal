# Resource Management

**Part of:** [Chapter 32: Exception Handling and Error Management](./README.md)

---

## Introduction

Resources must be properly managed, even when exceptions occur. This section teaches you how to use try/finally for guaranteed cleanup, apply resource acquisition patterns, implement RAII-like patterns, and manage common resource types.

**Key concepts:**
- **try/finally** — Guaranteed cleanup
- **Resource acquisition** — Getting resources safely
- **RAII patterns** — Resource management idioms
- **Common resources** — Files, memory, handles

---

## try/finally for Cleanup

### Guaranteed Cleanup

**finally blocks always execute:**

```pascal
var
  file: TFile;
begin
  file := OpenFile('data.txt');
  try
    ReadData(file);
    ProcessData(file);
  finally
    CloseFile(file);  // Always executes, even if exception
  end;
end;
```

### Why finally is Important

**Without finally, resources may leak:**

```pascal
// ❌ BAD: Resource leak on exception
var
  file: TFile;
begin
  file := OpenFile('data.txt');
  ReadData(file);  // If exception here, file never closed
  ProcessData(file);
  CloseFile(file);
end;

// ✅ GOOD: Always cleaned up
var
  file: TFile;
begin
  file := OpenFile('data.txt');
  try
    ReadData(file);
    ProcessData(file);
  finally
    CloseFile(file);  // Always executes
  end;
end;
```

### Multiple Resources

**Clean up multiple resources:**

```pascal
var
  file1, file2: TFile;
begin
  file1 := OpenFile('file1.txt');
  try
    file2 := OpenFile('file2.txt');
    try
      ProcessFiles(file1, file2);
    finally
      CloseFile(file2);  // Close file2 first
    end;
  finally
    CloseFile(file1);  // Close file1 second
  end;
end;
```

---

## Resource Acquisition Patterns

### Pattern 1: Acquire-Use-Release

**Standard resource pattern:**

```pascal
procedure UseResource;
var
  resource: TResource;
begin
  // 1. Acquire
  resource := AcquireResource;
  try
    // 2. Use
    DoWork(resource);
  finally
    // 3. Release (always)
    ReleaseResource(resource);
  end;
end;
```

### Pattern 2: Checked Acquisition

**Acquire only if successful:**

```pascal
function TryAcquireResource(out resource: TResource): boolean;
begin
  try
    resource := AcquireResource;
    TryAcquireResource := true;
  except
    on E: EOutOfMemory do
      TryAcquireResource := false;
  end;
end;

procedure UseResourceIfAvailable;
var
  resource: TResource;
begin
  if TryAcquireResource(resource) then
  begin
    try
      DoWork(resource);
    finally
      ReleaseResource(resource);
    end;
  end
  else
    WriteLn('Resource not available');
end;
```

### Pattern 3: Nested Resources

**Acquire multiple resources safely:**

```pascal
procedure UseMultipleResources;
var
  resource1: TResource1;
  resource2: TResource2;
begin
  resource1 := AcquireResource1;
  try
    resource2 := AcquireResource2;
    try
      DoWork(resource1, resource2);
    finally
      ReleaseResource2(resource2);
    end;
  finally
    ReleaseResource1(resource1);
  end;
end;
```

---

## RAII-like Patterns

### What is RAII?

**RAII (Resource Acquisition Is Initialization):**
- Acquire resource in constructor
- Release resource in destructor
- Resource lifetime tied to object lifetime
- Automatic cleanup when object destroyed

### SuperPascal RAII Pattern

**Implement RAII with classes:**

```pascal
type
  TFileHandle = class
  private
    FHandle: TFile;
    FFileName: string;
  public
    constructor Create(filename: string);
    destructor Destroy; override;
    property Handle: TFile read FHandle;
  end;

constructor TFileHandle.Create(filename: string);
begin
  inherited Create;
  FFileName := filename;
  FHandle := OpenFile(filename);
  if FHandle = nil then
    raise EIOError.Create('Cannot open file: ' + filename);
end;

destructor TFileHandle.Destroy;
begin
  if FHandle <> nil then
    CloseFile(FHandle);
  inherited Destroy;
end;

// Usage: Automatic cleanup
procedure ProcessFile(filename: string);
var
  file: TFileHandle;
begin
  file := TFileHandle.Create(filename);
  try
    ReadData(file.Handle);
  finally
    file.Free;  // Destructor closes file
  end;
end;
```

### RAII with Records

**RAII pattern with records:**

```pascal
type
  TFileGuard = record
  private
    FFile: TFile;
  public
    procedure Init(filename: string);
    procedure Done;
  end;

procedure TFileGuard.Init(filename: string);
begin
  FFile := OpenFile(filename);
  if FFile = nil then
    raise EIOError.Create('Cannot open file: ' + filename);
end;

procedure TFileGuard.Done;
begin
  if FFile <> nil then
  begin
    CloseFile(FFile);
    FFile := nil;
  end;
end;

// Usage: Manual cleanup required
procedure ProcessFile(filename: string);
var
  guard: TFileGuard;
begin
  guard.Init(filename);
  try
    ReadData(guard.FFile);
  finally
    guard.Done;  // Must call Done
  end;
end;
```

---

## Common Resource Types

### File Resources

**Managing file resources:**

```pascal
procedure ProcessFile(filename: string);
var
  file: TFile;
begin
  file := OpenFile(filename);
  if file = nil then
    raise EFileNotFound.Create('File not found: ' + filename);
  
  try
    while not EOF(file) do
      ProcessLine(ReadLine(file));
  finally
    CloseFile(file);
  end;
end;
```

### Memory Resources

**Managing heap memory:**

```pascal
procedure ProcessData;
var
  buffer: ^TDataBuffer;
begin
  GetMem(buffer, SizeOf(TDataBuffer));
  if buffer = nil then
    raise EOutOfMemory.Create('Out of memory');
  
  try
    Initialize(buffer);
    ProcessBuffer(buffer);
  finally
    FreeMem(buffer, SizeOf(TDataBuffer));
  end;
end;
```

### Object Resources

**Managing object instances:**

```pascal
procedure ProcessWithObject;
var
  obj: TMyObject;
begin
  obj := TMyObject.Create;
  try
    obj.DoWork;
    obj.ProcessData;
  finally
    obj.Free;  // Calls destructor
  end;
end;
```

### Handle Resources

**Managing system handles:**

```pascal
procedure UseHandle;
var
  handle: THandle;
begin
  handle := CreateHandle;
  if handle = INVALID_HANDLE then
    raise ESystemError.Create('Cannot create handle');
  
  try
    UseHandleResource(handle);
  finally
    CloseHandle(handle);
  end;
end;
```

---

## Resource Management Patterns

### Pattern 1: Standard try/finally

**Most common pattern:**

```pascal
resource := Acquire;
try
  Use(resource);
finally
  Release(resource);
end;
```

### Pattern 2: RAII Class

**Automatic cleanup with classes:**

```pascal
guard := TResourceGuard.Create(resource);
try
  Use(guard.Resource);
finally
  guard.Free;  // Destructor releases
end;
```

### Pattern 3: Checked Acquisition

**Acquire with error checking:**

```pascal
if TryAcquire(resource) then
begin
  try
    Use(resource);
  finally
    Release(resource);
  end;
end
else
  HandleAcquisitionFailure;
```

### Pattern 4: Nested Resources

**Multiple resources:**

```pascal
resource1 := Acquire1;
try
  resource2 := Acquire2;
  try
    Use(resource1, resource2);
  finally
    Release2(resource2);
  end;
finally
  Release1(resource1);
end;
```

---

## Common Resource Errors

### Error 1: Resource Leak

**Forgetting to release:**

```pascal
// ❌ BAD: Resource leak
procedure BadCode;
var
  file: TFile;
begin
  file := OpenFile('data.txt');
  ReadData(file);
  // Forgot to close file!
end;

// ✅ GOOD: Always released
procedure GoodCode;
var
  file: TFile;
begin
  file := OpenFile('data.txt');
  try
    ReadData(file);
  finally
    CloseFile(file);
  end;
end;
```

### Error 2: Double Release

**Releasing twice:**

```pascal
// ❌ BAD: Double release
procedure BadCode;
var
  file: TFile;
begin
  file := OpenFile('data.txt');
  try
    ReadData(file);
    CloseFile(file);  // Close here
  finally
    CloseFile(file);  // Close again - error!
  end;
end;

// ✅ GOOD: Release once
procedure GoodCode;
var
  file: TFile;
begin
  file := OpenFile('data.txt');
  try
    ReadData(file);
  finally
    CloseFile(file);  // Close only here
  end;
end;
```

### Error 3: Using After Release

**Using resource after release:**

```pascal
// ❌ BAD: Use after release
procedure BadCode;
var
  file: TFile;
begin
  file := OpenFile('data.txt');
  try
    ReadData(file);
  finally
    CloseFile(file);
  end;
  ReadData(file);  // Error: file already closed!
end;

// ✅ GOOD: Don't use after release
procedure GoodCode;
var
  file: TFile;
begin
  file := OpenFile('data.txt');
  try
    ReadData(file);
    // All use before finally
  finally
    CloseFile(file);
  end;
end;
```

### Error 4: Exception During Acquisition

**Exception before try block:**

```pascal
// ❌ BAD: Exception before try
procedure BadCode;
var
  file: TFile;
begin
  file := OpenFile('data.txt');  // If exception here, no cleanup
  try
    ReadData(file);
  finally
    CloseFile(file);
  end;
end;

// ✅ GOOD: Check before try
procedure GoodCode;
var
  file: TFile;
begin
  file := OpenFile('data.txt');
  if file = nil then
    raise EFileNotFound.Create('File not found');
  
  try
    ReadData(file);
  finally
    CloseFile(file);
  end;
end;
```

---

## Best Practices

### 1. Always Use finally for Cleanup

**Guarantee cleanup:**

```pascal
// ✅ GOOD: Always cleanup
resource := Acquire;
try
  Use(resource);
finally
  Release(resource);  // Always executes
end;

// ❌ BAD: May not cleanup
resource := Acquire;
Use(resource);
Release(resource);  // May not execute if exception
```

### 2. Acquire Before try

**Acquire before entering try block:**

```pascal
// ✅ GOOD: Acquire before try
resource := Acquire;
try
  Use(resource);
finally
  Release(resource);
end;

// ❌ BAD: Acquire in try
try
  resource := Acquire;  // If exception, resource undefined
  Use(resource);
finally
  Release(resource);  // May release invalid resource
end;
```

### 3. Release in Reverse Order

**Release in opposite order of acquisition:**

```pascal
// ✅ GOOD: Reverse order
resource1 := Acquire1;
try
  resource2 := Acquire2;
  try
    Use(resource1, resource2);
  finally
    Release2(resource2);  // Release 2 first
  end;
finally
  Release1(resource1);  // Release 1 second
end;
```

### 4. Use RAII When Possible

**Prefer automatic cleanup:**

```pascal
// ✅ GOOD: RAII pattern
guard := TFileGuard.Create(filename);
try
  Use(guard.File);
finally
  guard.Free;  // Automatic cleanup
end;

// ❌ BAD: Manual management
file := OpenFile(filename);
try
  Use(file);
finally
  CloseFile(file);  // Must remember to close
end;
```

### 5. Check for Nil/Invalid

**Validate resources:**

```pascal
// ✅ GOOD: Check validity
resource := Acquire;
if resource = nil then
  raise EResourceError.Create('Cannot acquire resource');

try
  Use(resource);
finally
  Release(resource);
end;

// ❌ BAD: Assume success
resource := Acquire;  // May be nil
try
  Use(resource);  // May crash
finally
  Release(resource);  // May crash
end;
```

---

## Exercises

### Exercise 1: File Resource Management

Write code that:
1. Opens a file
2. Reads data from file
3. Always closes file (even on exception)
4. Handles file errors properly

### Exercise 2: Memory Resource Management

Write code that:
1. Allocates memory
2. Uses allocated memory
3. Always frees memory (even on exception)
4. Handles allocation failures

### Exercise 3: RAII Pattern

Implement:
1. RAII class for a resource
2. Constructor acquires resource
3. Destructor releases resource
4. Demonstrate automatic cleanup

### Exercise 4: Multiple Resources

Write code that:
1. Acquires multiple resources
2. Uses all resources
3. Releases all resources in correct order
4. Handles exceptions properly

---

**Previous Section:** [Exception Propagation](./02_ExceptionPropagation.md)  
**Next Section:** [Error Handling Patterns](./04_ErrorHandlingPatterns.md)  
**Last Updated:** 2025-01-XX

