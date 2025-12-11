# Exception Basics

**Part of:** [Chapter 32: Exception Handling and Error Management](./README.md)

---

## Introduction

Exceptions allow you to handle errors gracefully. This section teaches you the try/except/finally syntax, exception semantics, basic exception handling patterns, and how exceptions differ from normal return paths.

**Key concepts:**
- **try/except** — Catching exceptions
- **try/finally** — Guaranteed cleanup
- **Exception flow** — How exceptions change program flow
- **Exception types** — Different kinds of exceptions

---

## Understanding Exceptions

### What is an Exception?

**An exception is:**
- An error condition that interrupts normal flow
- A way to handle unexpected situations
- A mechanism for error propagation
- A structured alternative to error codes

**Exceptions are for:**
- Unexpected errors (file not found, out of memory)
- Programming errors (null pointer, out of bounds)
- System errors (hardware failures, resource exhaustion)
- Errors that need propagation up the call stack

**Exceptions are NOT for:**
- Expected conditions (end of file, user input validation)
- Normal control flow (use if/else instead)
- Performance-critical paths (use error codes)

### Exception vs Normal Flow

**Normal flow:**
```pascal
function Divide(a, b: integer): integer;
begin
  if b = 0 then
  begin
    WriteLn('Error: Division by zero');
    Divide := 0;  // Return error value
    Exit;
  end;
  Divide := a div b;
end;
```

**Exception flow:**
```pascal
function Divide(a, b: integer): integer;
begin
  if b = 0 then
    raise EDivideByZero.Create('Division by zero');
  Divide := a div b;
end;
```

---

## try/except Syntax

### Basic try/except

**Catch and handle exceptions:**

```pascal
try
  // Code that might raise exception
  result := Divide(10, 0);
except
  // Handle exception
  on E: EDivideByZero do
    WriteLn('Cannot divide by zero');
  on E: Exception do
    WriteLn('Error: ', E.Message);
end;
```

### Catching Specific Exceptions

**Catch specific exception types:**

```pascal
try
  arr[index] := value;
except
  on E: EOutOfBounds do
    WriteLn('Index out of bounds: ', index);
  on E: ENilPointer do
    WriteLn('Array is nil');
  on E: Exception do
    WriteLn('Unexpected error: ', E.Message);
end;
```

### Multiple Exception Handlers

**Handle different exceptions differently:**

```pascal
try
  ProcessFile(filename);
except
  on E: EFileNotFound do
    WriteLn('File not found: ', filename);
  on E: EIOError do
    WriteLn('I/O error reading file');
  on E: EOutOfMemory do
    WriteLn('Out of memory');
  on E: Exception do
    WriteLn('Unknown error: ', E.Message);
end;
```

---

## try/finally Syntax

### Guaranteed Cleanup

**Ensure cleanup always happens:**

```pascal
var
  file: TFile;
begin
  file := OpenFile('data.txt');
  try
    // Use file
    ReadData(file);
  finally
    // Always close file, even if exception occurs
    CloseFile(file);
  end;
end;
```

### Resource Management Pattern

**Common pattern for resources:**

```pascal
procedure ProcessResource;
var
  resource: TResource;
begin
  resource := AcquireResource;
  try
    UseResource(resource);
  finally
    ReleaseResource(resource);  // Always released
  end;
end;
```

### Multiple Cleanup Operations

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

## try/except/finally Combined

### Both Exception Handling and Cleanup

**Combine except and finally:**

```pascal
var
  file: TFile;
begin
  file := OpenFile('data.txt');
  try
    ReadData(file);
  except
    on E: EIOError do
      WriteLn('Error reading file: ', E.Message);
  finally
    CloseFile(file);  // Always close
  end;
end;
```

### Order of Execution

**Execution order:**

```pascal
try
  // 1. Try block executes
  DoSomething;
except
  // 2. Exception handler (if exception raised)
  on E: Exception do
    HandleException(E);
finally
  // 3. Finally block (always executes)
  Cleanup;
end;
// 4. Continue after try/except/finally
```

---

## Exception Semantics

### Exception Flow Control

**How exceptions change flow:**

```pascal
procedure Level1;
begin
  WriteLn('Level1: Start');
  Level2;
  WriteLn('Level1: End');  // May not execute if exception
end;

procedure Level2;
begin
  WriteLn('Level2: Start');
  Level3;
  WriteLn('Level2: End');  // May not execute if exception
end;

procedure Level3;
begin
  WriteLn('Level3: Start');
  raise Exception.Create('Error in Level3');
  WriteLn('Level3: End');  // Never executes
end;

// If exception raised in Level3:
// - Level3 stops immediately
// - Control jumps to exception handler
// - Level2 and Level1 don't continue normally
```

### Stack Unwinding

**Exceptions unwind the call stack:**

```pascal
procedure Outer;
begin
  try
    Middle;
  except
    on E: Exception do
      WriteLn('Caught in Outer: ', E.Message);
  end;
end;

procedure Middle;
begin
  Inner;  // Exception raised here
end;

procedure Inner;
begin
  raise Exception.Create('Error');
end;

// Execution flow:
// 1. Outer calls Middle
// 2. Middle calls Inner
// 3. Inner raises exception
// 4. Stack unwinds: Inner → Middle → Outer
// 5. Exception caught in Outer
```

---

## Basic Exception Handling Patterns

### Pattern 1: Try and Handle

**Catch and handle locally:**

```pascal
function SafeDivide(a, b: integer): integer;
begin
  try
    SafeDivide := a div b;
  except
    on E: EDivideByZero do
    begin
      WriteLn('Division by zero, returning 0');
      SafeDivide := 0;
    end;
  end;
end;
```

### Pattern 2: Try and Propagate

**Let exception propagate:**

```pascal
function ProcessData(data: array of integer): integer;
begin
  // If exception occurs, let it propagate to caller
  ProcessData := CalculateResult(data);
end;

// Caller handles exception
try
  result := ProcessData(myData);
except
  on E: Exception do
    WriteLn('Error processing data: ', E.Message);
end;
```

### Pattern 3: Try and Convert

**Convert exception to error code:**

```pascal
function TryDivide(a, b: integer; out result: integer): boolean;
begin
  try
    result := a div b;
    TryDivide := true;
  except
    on E: EDivideByZero do
      TryDivide := false;
  end;
end;

// Usage
if TryDivide(10, 2, result) then
  WriteLn('Result: ', result)
else
  WriteLn('Division failed');
```

### Pattern 4: Try and Log

**Log exception and re-raise:**

```pascal
procedure ProcessWithLogging;
begin
  try
    DoRiskyOperation;
  except
    on E: Exception do
    begin
      WriteLn('Error logged: ', E.Message);
      raise;  // Re-raise exception
    end;
  end;
end;
```

---

## Exception vs Normal Return

### When to Use Exceptions

**Use exceptions for:**

1. **Unexpected errors**
   ```pascal
   // File should exist, but doesn't
   file := OpenFile('config.txt');  // Raises EFileNotFound if missing
   ```

2. **Programming errors**
   ```pascal
   // Index should be valid
   value := arr[index];  // Raises EOutOfBounds if invalid
   ```

3. **System errors**
   ```pascal
   // Memory should be available
   ptr := New(TMyRecord);  // Raises EOutOfMemory if fails
   ```

### When NOT to Use Exceptions

**Don't use exceptions for:**

1. **Expected conditions**
   ```pascal
   // End of file is expected
   if not EOF(file) then
     ReadLine(file)
   else
     HandleEndOfFile;  // Normal flow, not exception
   ```

2. **Control flow**
   ```pascal
   // Use if/else for control flow
   if condition then
     DoThis
   else
     DoThat;  // Not an exception
   ```

3. **Performance-critical code**
   ```pascal
   // Use error codes in tight loops
   if TryOperation(result) then
     UseResult(result)
   else
     HandleError;  // Faster than exceptions
   ```

---

## Common Exception Types

### Standard Exceptions

**Built-in exception types:**

```pascal
// Out of bounds
raise EOutOfBounds.Create('Index out of range');

// Null pointer
raise ENilPointer.Create('Pointer is nil');

// Arithmetic overflow
raise EOverflow.Create('Arithmetic overflow');

// Division by zero
raise EDivideByZero.Create('Division by zero');

// Out of memory
raise EOutOfMemory.Create('Out of memory');

// I/O error
raise EIOError.Create('I/O operation failed');
```

### Custom Exceptions

**Define your own exceptions:**

```pascal
type
  EGameError = class(Exception)
  end;
  
  EInvalidMove = class(EGameError)
  end;
  
  ELevelNotFound = class(EGameError)
  end;

// Usage
raise EInvalidMove.Create('Invalid move');
raise ELevelNotFound.Create('Level not found: ' + levelName);
```

---

## Best Practices

### 1. Catch Specific Exceptions

**Be specific about what you catch:**

```pascal
// ✅ GOOD: Catch specific exceptions
try
  ProcessData;
except
  on E: EIOError do
    HandleIOError;
  on E: EOutOfMemory do
    HandleOutOfMemory;
end;

// ❌ BAD: Catch everything
try
  ProcessData;
except
  // Catches everything, may hide bugs
end;
```

### 2. Always Clean Up Resources

**Use finally for cleanup:**

```pascal
// ✅ GOOD: Always cleanup
resource := Acquire;
try
  Use(resource);
finally
  Release(resource);  // Always released
end;

// ❌ BAD: No cleanup on exception
resource := Acquire;
Use(resource);  // If exception, resource leaked
Release(resource);
```

### 3. Don't Swallow Exceptions

**Handle or re-raise:**

```pascal
// ✅ GOOD: Handle or re-raise
try
  DoSomething;
except
  on E: EExpectedError do
    HandleExpected;  // Handle expected errors
  on E: Exception do
    raise;  // Re-raise unexpected errors
end;

// ❌ BAD: Swallow all exceptions
try
  DoSomething;
except
  // Silent failure, hard to debug
end;
```

### 4. Use Exceptions for Unexpected Errors

**Reserve exceptions for unexpected:**

```pascal
// ✅ GOOD: Exception for unexpected
file := OpenFile('required.txt');  // Should exist

// ❌ BAD: Exception for expected
try
  file := OpenFile('optional.txt');
except
  // File might not exist, use error code instead
end;
```

### 5. Document Exception Behavior

**Document what exceptions can be raised:**

```pascal
{*
  Opens a file for reading.
  
  Raises:
    EFileNotFound if file doesn't exist
    EIOError if file cannot be opened
*}
function OpenFile(filename: string): TFile;
```

---

## Exercises

### Exercise 1: Basic Exception Handling

Write code that:
1. Attempts a risky operation
2. Catches specific exceptions
3. Handles each exception type
4. Demonstrates exception flow

### Exercise 2: Resource Cleanup

Write code that:
1. Acquires a resource
2. Uses the resource
3. Always releases the resource (finally)
4. Handles exceptions during use

### Exercise 3: Exception Propagation

Write code that:
1. Calls functions that may raise exceptions
2. Lets exceptions propagate
3. Catches exceptions at appropriate level
4. Demonstrates stack unwinding

### Exercise 4: Exception vs Error Codes

Write code that:
1. Uses exceptions for unexpected errors
2. Uses error codes for expected conditions
3. Demonstrates when to use each
4. Shows the difference

---

**Next Section:** [Exception Propagation](./02_ExceptionPropagation.md)  
**Last Updated:** 2025-01-XX

