# Exception Propagation

**Part of:** [Chapter 32: Exception Handling and Error Management](./README.md)

---

## Introduction

Exceptions propagate automatically through the call stack. This section teaches you how to raise exceptions, understand exception propagation, re-raise exceptions, and understand the exception frame model.

**Key concepts:**
- **raise statement** — Raising exceptions
- **Exception propagation** — How exceptions move up the stack
- **Re-raising** — Passing exceptions to higher levels
- **Exception frames** — Implementation model

---

## Raising Exceptions

### The raise Statement

**Raise an exception:**

```pascal
procedure ValidateIndex(index: integer; maxIndex: integer);
begin
  if (index < 0) or (index > maxIndex) then
    raise EOutOfBounds.Create('Index out of bounds: ' + IntToStr(index));
end;
```

### Raising with Messages

**Provide descriptive error messages:**

```pascal
procedure OpenFile(filename: string);
begin
  if not FileExists(filename) then
    raise EFileNotFound.Create('File not found: ' + filename);
  
  // Open file...
end;
```

### Raising Custom Exceptions

**Raise your own exception types:**

```pascal
type
  EInvalidMove = class(Exception)
  end;

procedure MakeMove(move: TMove);
begin
  if not IsValidMove(move) then
    raise EInvalidMove.Create('Invalid move: ' + MoveToString(move));
  
  // Execute move...
end;
```

### Raising with Context

**Add context to exceptions:**

```pascal
procedure ProcessLevel(levelName: string);
begin
  if not LevelExists(levelName) then
    raise ELevelNotFound.CreateFmt('Level not found: %s', [levelName]);
  
  // Process level...
end;
```

---

## Exception Propagation

### How Exceptions Propagate

**Exceptions automatically propagate up the call stack:**

```pascal
procedure Level1;
begin
  WriteLn('Level1: Start');
  Level2;
  WriteLn('Level1: End');  // May not execute
end;

procedure Level2;
begin
  WriteLn('Level2: Start');
  Level3;
  WriteLn('Level2: End');  // May not execute
end;

procedure Level3;
begin
  WriteLn('Level3: Start');
  raise Exception.Create('Error in Level3');
  WriteLn('Level3: End');  // Never executes
end;

// Execution when exception raised:
// 1. Level3 raises exception
// 2. Level3 execution stops immediately
// 3. Exception propagates to Level2
// 4. Level2 execution stops (no handler)
// 5. Exception propagates to Level1
// 6. Level1 execution stops (no handler)
// 7. Exception propagates to caller
```

### Propagation Path

**Exceptions follow the call stack:**

```pascal
Main
  └─> FunctionA
        └─> FunctionB
              └─> FunctionC (raises exception)
                    └─> Exception propagates: C → B → A → Main
```

### Unhandled Exceptions

**If no handler found:**

```pascal
procedure Unhandled;
begin
  raise Exception.Create('Unhandled exception');
  // If no try/except in call chain, program terminates
end;

begin
  Unhandled;  // Program may terminate
end.
```

---

## Catching and Re-raising

### Re-raising Exceptions

**Catch, handle, then re-raise:**

```pascal
procedure ProcessData;
begin
  try
    LoadData;
    ValidateData;
    SaveData;
  except
    on E: EIOError do
    begin
      WriteLn('I/O error occurred, logging...');
      LogError(E);
      raise;  // Re-raise same exception
    end;
  end;
end;
```

### Transforming Exceptions

**Catch one exception, raise another:**

```pascal
procedure ProcessFile(filename: string);
begin
  try
    ReadFile(filename);
  except
    on E: EFileNotFound do
      raise EConfigurationError.Create('Config file missing: ' + filename);
    on E: EIOError do
      raise EConfigurationError.Create('Cannot read config: ' + E.Message);
  end;
end;
```

### Adding Context

**Add context when re-raising:**

```pascal
procedure ProcessLevel(levelName: string);
begin
  try
    LoadLevel(levelName);
  except
    on E: Exception do
    begin
      // Add context to exception
      raise Exception.CreateFmt('Error loading level %s: %s', 
                                [levelName, E.Message]);
    end;
  end;
end;
```

---

## Exception Frame Model

### Understanding Exception Frames

**SuperPascal uses setjmp/longjmp style exception frames:**

```pascal
// Conceptual model (simplified)
type
  TExceptionFrame = record
    PreviousFrame: ^TExceptionFrame;
    JumpBuffer: TJumpBuffer;  // setjmp buffer
    ExceptionHandler: procedure(E: Exception);
  end;

var
  CurrentFrame: ^TExceptionFrame;
```

### How Frames Work

**Exception frames track exception handlers:**

```pascal
procedure Outer;
begin
  // Frame 1: Outer's try block
  try
    Middle;
  except
    on E: Exception do
      WriteLn('Caught in Outer');
  end;
end;

procedure Middle;
begin
  // Frame 2: Middle's try block
  try
    Inner;
  except
    on E: EIOError do
      WriteLn('Caught in Middle');
    // Other exceptions propagate
  end;
end;

procedure Inner;
begin
  raise EIOError.Create('I/O error');
  // Exception propagates:
  // 1. Inner raises EIOError
  // 2. Check Frame 2 (Middle): handles EIOError
  // 3. Exception caught in Middle
end;
```

### Frame Stack

**Frames form a stack:**

```
CurrentFrame → Frame 3 (Inner's handler)
                ↓
              Frame 2 (Middle's handler)
                ↓
              Frame 1 (Outer's handler)
                ↓
              nil
```

### Exception Search

**Exception searches frames from current to root:**

```pascal
procedure SearchException(E: Exception);
var
  frame: ^TExceptionFrame;
begin
  frame := CurrentFrame;
  while frame <> nil do
  begin
    if frame.ExceptionHandler.CanHandle(E) then
    begin
      // Found handler, jump to it
      longjmp(frame.JumpBuffer, 1);
    end;
    frame := frame.PreviousFrame;
  end;
  
  // No handler found, terminate
  TerminateProgram(E);
end;
```

---

## Propagation Examples

### Example 1: Simple Propagation

**Exception propagates through multiple levels:**

```pascal
procedure DeepCall;
begin
  Level1;
end;

procedure Level1;
begin
  Level2;
end;

procedure Level2;
begin
  Level3;
end;

procedure Level3;
begin
  raise Exception.Create('Error');
end;

// Exception propagates: Level3 → Level2 → Level1 → DeepCall
```

### Example 2: Caught and Re-raised

**Exception caught, handled, then re-raised:**

```pascal
procedure ProcessWithLogging;
begin
  try
    DoRiskyOperation;
  except
    on E: Exception do
    begin
      LogError(E);
      raise;  // Re-raise after logging
    end;
  end;
end;
```

### Example 3: Exception Transformation

**One exception type transformed to another:**

```pascal
procedure LoadConfig;
begin
  try
    ReadConfigFile;
  except
    on E: EFileNotFound do
      raise EConfigError.Create('Config file missing');
    on E: EIOError do
      raise EConfigError.Create('Cannot read config');
  end;
end;
```

### Example 4: Selective Propagation

**Some exceptions caught, others propagate:**

```pascal
procedure ProcessData;
begin
  try
    LoadData;
    ValidateData;
  except
    on E: EValidationError do
    begin
      // Handle validation errors locally
      WriteLn('Validation error: ', E.Message);
      // Don't re-raise
    end;
    // Other exceptions propagate automatically
  end;
end;
```

---

## Exception Propagation Patterns

### Pattern 1: Let It Propagate

**Don't catch, let caller handle:**

```pascal
function CalculateResult(data: array of integer): integer;
begin
  // If exception occurs, let it propagate
  CalculateResult := ProcessData(data);
end;

// Caller handles
try
  result := CalculateResult(myData);
except
  on E: Exception do
    HandleError(E);
end;
```

### Pattern 2: Catch and Transform

**Catch low-level, raise high-level:**

```pascal
procedure HighLevelOperation;
begin
  try
    LowLevelOperation;
  except
    on E: EIOError do
      raise EBusinessLogicError.Create('Operation failed: ' + E.Message);
  end;
end;
```

### Pattern 3: Catch and Log

**Log then re-raise:**

```pascal
procedure OperationWithLogging;
begin
  try
    DoOperation;
  except
    on E: Exception do
    begin
      LogException(E);
      raise;  // Re-raise after logging
    end;
  end;
end;
```

### Pattern 4: Partial Handling

**Handle some, let others propagate:**

```pascal
procedure ProcessWithPartialHandling;
begin
  try
    DoOperation;
  except
    on E: EExpectedError do
      HandleExpected(E);  // Handle expected errors
    // Unexpected errors propagate
  end;
end;
```

---

## Best Practices

### 1. Let Exceptions Propagate When Appropriate

**Don't catch exceptions you can't handle:**

```pascal
// ✅ GOOD: Let it propagate
function GetValue(index: integer): integer;
begin
  // If index invalid, let exception propagate
  GetValue := arr[index];
end;

// ❌ BAD: Catch and ignore
function GetValue(index: integer): integer;
begin
  try
    GetValue := arr[index];
  except
    // Swallowing exception, caller doesn't know about error
    GetValue := 0;
  end;
end;
```

### 2. Add Context When Re-raising

**Provide useful context:**

```pascal
// ✅ GOOD: Add context
procedure ProcessLevel(level: string);
begin
  try
    LoadLevel(level);
  except
    on E: Exception do
      raise Exception.CreateFmt('Error in level %s: %s', [level, E.Message]);
  end;
end;

// ❌ BAD: Lose context
procedure ProcessLevel(level: string);
begin
  try
    LoadLevel(level);
  except
    raise Exception.Create('Error');  // Lost level name
  end;
end;
```

### 3. Transform Exceptions at Boundaries

**Transform at module boundaries:**

```pascal
// ✅ GOOD: Transform at boundary
unit FileIO;

procedure ReadFile(filename: string);
begin
  try
    LowLevelRead(filename);
  except
    on E: ESystemError do
      raise EIOError.Create('File I/O error: ' + E.Message);
  end;
end;

// ❌ BAD: Expose low-level exceptions
procedure ReadFile(filename: string);
begin
  LowLevelRead(filename);  // Caller sees ESystemError
end;
```

### 4. Don't Catch Everything

**Be specific about what you catch:**

```pascal
// ✅ GOOD: Catch specific exceptions
try
  DoOperation;
except
  on E: EIOError do
    HandleIOError;
  on E: EOutOfMemory do
    HandleOutOfMemory;
end;

// ❌ BAD: Catch everything
try
  DoOperation;
except
  // May hide programming errors
end;
```

### 5. Document Exception Behavior

**Document what exceptions can be raised:**

```pascal
{*
  Processes data from file.
  
  Raises:
    EFileNotFound if file doesn't exist
    EIOError if file cannot be read
    EOutOfMemory if insufficient memory
*}
procedure ProcessFile(filename: string);
```

---

## Exercises

### Exercise 1: Exception Propagation

Write code that:
1. Raises exceptions at different levels
2. Demonstrates exception propagation
3. Shows how exceptions move up the stack
4. Handles exceptions at appropriate levels

### Exercise 2: Re-raising Exceptions

Write code that:
1. Catches exceptions
2. Adds logging or context
3. Re-raises exceptions
4. Demonstrates re-raising patterns

### Exercise 3: Exception Transformation

Write code that:
1. Catches low-level exceptions
2. Transforms to high-level exceptions
3. Adds context information
4. Demonstrates transformation patterns

### Exercise 4: Exception Frames

Write code that:
1. Uses nested try/except blocks
2. Demonstrates frame behavior
3. Shows exception search order
4. Handles exceptions at different levels

---

**Previous Section:** [Exception Basics](./01_ExceptionBasics.md)  
**Next Section:** [Resource Management](./03_ResourceManagement.md)  
**Last Updated:** 2025-01-XX

