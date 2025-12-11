# Error Handling Patterns

**Part of:** [Chapter 32: Exception Handling and Error Management](./README.md)

---

## Introduction

Different situations call for different error handling strategies. This section teaches you when to use exceptions vs error codes, how to choose appropriate error handling strategies, and how to implement error recovery patterns.

**Key concepts:**
- **Exceptions vs error codes** — When to use each
- **Error handling strategies** — Different approaches
- **Error recovery** — Recovering from errors
- **Error patterns** — Common error handling patterns

---

## When to Use Exceptions

### Use Exceptions For

**1. Unexpected errors**
```pascal
// File should exist, but doesn't
function LoadConfig: TConfig;
begin
  file := OpenFile('config.txt');  // Raises if not found
  // ...
end;
```

**2. Programming errors**
```pascal
// Index should be valid
function GetValue(index: integer): integer;
begin
  // Raises EOutOfBounds if invalid
  GetValue := arr[index];
end;
```

**3. System errors**
```pascal
// Memory should be available
function AllocateBuffer(size: word): pointer;
begin
  GetMem(result, size);  // Raises EOutOfMemory if fails
end;
```

**4. Errors that need propagation**
```pascal
// Error needs to propagate up call stack
procedure DeepOperation;
begin
  // If error occurs, propagate to caller
  DoComplexOperation;
end;
```

### Exception Characteristics

**Exceptions are good for:**
- **Unexpected** — Shouldn't normally happen
- **Propagation** — Need to propagate up stack
- **Termination** — May need to abort operation
- **Debugging** — Help identify bugs

---

## When to Use Error Codes

### Use Error Codes For

**1. Expected conditions**
```pascal
// End of file is expected
function ReadLine(var file: TFile; out line: string): boolean;
begin
  if EOF(file) then
  begin
    ReadLine := false;  // Expected condition
    Exit;
  end;
  line := ReadLineFromFile(file);
  ReadLine := true;
end;
```

**2. Performance-critical paths**
```pascal
// Tight loop, exceptions too expensive
function TryGetValue(index: integer; out value: integer): boolean;
begin
  if (index < 0) or (index >= Length(arr)) then
  begin
    TryGetValue := false;  // Fast error code
    Exit;
  end;
  value := arr[index];
  TryGetValue := true;
end;
```

**3. Multiple error types**
```pascal
// Need to distinguish error types
type
  TResult = (rSuccess, rNotFound, rIOError, rOutOfMemory);

function OpenFile(filename: string): TResult;
begin
  if not FileExists(filename) then
  begin
    OpenFile := rNotFound;
    Exit;
  end;
  // ...
end;
```

**4. Control flow**
```pascal
// Error is part of normal flow
if TryOperation(result) then
  UseResult(result)
else
  HandleExpectedError;
```

### Error Code Characteristics

**Error codes are good for:**
- **Expected** — Part of normal operation
- **Fast** — No stack unwinding overhead
- **Explicit** — Caller must check return value
- **Control flow** — Part of algorithm logic

---

## Result<T, E> Pattern

### What is Result<T, E>?

**Result type represents success or failure:**

```pascal
type
  TResult<T, E> = record
    IsSuccess: boolean;
    Value: T;
    Error: E;
  end;

function Ok<T>(value: T): TResult<T, E>;
begin
  result.IsSuccess := true;
  result.Value := value;
end;

function Err<T, E>(error: E): TResult<T, E>;
begin
  result.IsSuccess := false;
  result.Error := error;
end;
```

### Using Result Pattern

**Return Result instead of raising:**

```pascal
type
  TFileError = (feNotFound, feIOError, fePermissionDenied);

function OpenFile(filename: string): TResult<TFile, TFileError>;
begin
  if not FileExists(filename) then
  begin
    result := Err<TFile, TFileError>(feNotFound);
    Exit;
  end;
  
  // Open file...
  result := Ok<TFile>(file);
end;

// Usage
var
  fileResult: TResult<TFile, TFileError>;
begin
  fileResult := OpenFile('data.txt');
  if fileResult.IsSuccess then
    UseFile(fileResult.Value)
  else
    HandleError(fileResult.Error);
end;
```

### Result vs Exceptions

**When to use Result:**
- Expected errors (file might not exist)
- Performance-critical code
- Need to distinguish error types
- Part of normal control flow

**When to use Exceptions:**
- Unexpected errors (programming bugs)
- Need automatic propagation
- System errors (out of memory)
- Errors that should abort operation

---

## Error Handling Strategies

### Strategy 1: Fail Fast

**Stop immediately on error:**

```pascal
procedure ProcessData;
begin
  // If any step fails, stop immediately
  LoadData;  // Raises if fails
  ValidateData;  // Raises if fails
  ProcessData;  // Raises if fails
  SaveData;  // Raises if fails
end;

// Caller handles all errors
try
  ProcessData;
except
  on E: Exception do
    WriteLn('Processing failed: ', E.Message);
end;
```

### Strategy 2: Collect and Report

**Collect errors, report at end:**

```pascal
type
  TErrorList = array[0..99] of string;
  
var
  Errors: TErrorList;
  ErrorCount: byte;

procedure CollectError(msg: string);
begin
  if ErrorCount < 100 then
  begin
    Errors[ErrorCount] := msg;
    ErrorCount := ErrorCount + 1;
  end;
end;

procedure ProcessData;
begin
  ErrorCount := 0;
  
  // Collect errors, don't stop
  if not TryLoadData then
    CollectError('Failed to load data');
  
  if not TryValidateData then
    CollectError('Validation failed');
  
  // Report all errors at end
  if ErrorCount > 0 then
    ReportErrors;
end;
```

### Strategy 3: Retry with Backoff

**Retry operation on failure:**

```pascal
function TryOperationWithRetry(maxRetries: byte): boolean;
var
  retries: byte;
begin
  retries := 0;
  while retries < maxRetries do
  begin
    try
      DoOperation;
      TryOperationWithRetry := true;
      Exit;
    except
      on E: EIOError do
      begin
        retries := retries + 1;
        if retries < maxRetries then
          Delay(100 * retries);  // Exponential backoff
      end;
    end;
  end;
  TryOperationWithRetry := false;
end;
```

### Strategy 4: Fallback

**Try alternative on failure:**

```pascal
function LoadConfig: TConfig;
begin
  try
    // Try primary location
    result := LoadFromFile('config.txt');
  except
    on E: EFileNotFound do
    begin
      try
        // Fallback to default location
        result := LoadFromFile('default_config.txt');
      except
        // Fallback to hardcoded defaults
        result := GetDefaultConfig;
      end;
    end;
  end;
end;
```

---

## Error Recovery Patterns

### Pattern 1: Retry

**Retry failed operation:**

```pascal
function RetryOperation(operation: procedure; maxRetries: byte): boolean;
var
  retries: byte;
begin
  retries := 0;
  while retries < maxRetries do
  begin
    try
      operation;
      RetryOperation := true;
      Exit;
    except
      on E: ERetryableError do
      begin
        retries := retries + 1;
        if retries < maxRetries then
          Delay(100);
      end;
    end;
  end;
  RetryOperation := false;
end;
```

### Pattern 2: Fallback

**Use alternative on failure:**

```pascal
function GetDataWithFallback: TData;
begin
  try
    result := LoadFromPrimarySource;
  except
    on E: Exception do
    begin
      try
        result := LoadFromSecondarySource;
      except
        result := GetDefaultData;
      end;
    end;
  end;
end;
```

### Pattern 3: Partial Success

**Continue with partial results:**

```pascal
procedure ProcessWithPartialSuccess;
var
  successCount, failCount: word;
begin
  successCount := 0;
  failCount := 0;
  
  for i := 0 to ItemCount - 1 do
  begin
    try
      ProcessItem(Items[i]);
      successCount := successCount + 1;
    except
      on E: Exception do
      begin
        failCount := failCount + 1;
        LogError('Failed to process item ', i, ': ', E.Message);
        // Continue with next item
      end;
    end;
  end;
  
  WriteLn('Processed: ', successCount, ', Failed: ', failCount);
end;
```

### Pattern 4: Validation Before Action

**Validate before attempting:**

```pascal
function SafeOperation(data: TData): boolean;
begin
  // Validate first
  if not IsValid(data) then
  begin
    SafeOperation := false;
    Exit;
  end;
  
  // Then perform operation
  try
    PerformOperation(data);
    SafeOperation := true;
  except
    on E: Exception do
    begin
      LogError(E);
      SafeOperation := false;
    end;
  end;
end;
```

---

## Choosing the Right Strategy

### Decision Tree

**When to use each approach:**

```
Is error expected?
  ├─ Yes → Use error codes or Result<T, E>
  │
  └─ No → Is it a programming error?
      ├─ Yes → Use exception (fail fast)
      │
      └─ No → Is it recoverable?
          ├─ Yes → Use exception with recovery
          │
          └─ No → Use exception (fail fast)
```

### Examples

**Example 1: Expected Error**
```pascal
// File might not exist - expected
function TryLoadFile(filename: string; out data: TData): boolean;
begin
  if not FileExists(filename) then
  begin
    TryLoadFile := false;  // Error code
    Exit;
  end;
  data := LoadFile(filename);
  TryLoadFile := true;
end;
```

**Example 2: Programming Error**
```pascal
// Index should always be valid - programming error
function GetValue(index: integer): integer;
begin
  // Raises exception if invalid
  GetValue := arr[index];
end;
```

**Example 3: Recoverable Error**
```pascal
// Network error - might recover
function LoadWithRetry: TData;
begin
  try
    result := LoadFromNetwork;
  except
    on E: ENetworkError do
    begin
      // Retry once
      Delay(1000);
      result := LoadFromNetwork;
    end;
  end;
end;
```

---

## Best Practices

### 1. Use Exceptions for Unexpected Errors

**Reserve exceptions for unexpected:**

```pascal
// ✅ GOOD: Exception for unexpected
function LoadRequiredFile: TData;
begin
  file := OpenFile('required.txt');  // Should exist
  // ...
end;

// ❌ BAD: Exception for expected
try
  file := OpenFile('optional.txt');
except
  // File might not exist, use error code
end;
```

### 2. Use Error Codes for Expected Conditions

**Use error codes for expected:**

```pascal
// ✅ GOOD: Error code for expected
function TryReadLine(out line: string): boolean;
begin
  if EOF(file) then
  begin
    TryReadLine := false;  // Expected
    Exit;
  end;
  line := ReadLine;
  TryReadLine := true;
end;

// ❌ BAD: Exception for expected
try
  line := ReadLine;  // EOF is expected
except
  on E: EEndOfFile do
    // Should use error code
end;
```

### 3. Be Consistent

**Use consistent error handling:**

```pascal
// ✅ GOOD: Consistent approach
// All file operations use error codes
function TryOpenFile(filename: string; out file: TFile): boolean;
function TryReadFile(var file: TFile; out data: TData): boolean;
function TryCloseFile(var file: TFile): boolean;

// ❌ BAD: Mixed approaches
function OpenFile(filename: string): TFile;  // Raises
function TryReadFile(var file: TFile): boolean;  // Error code
function CloseFile(var file: TFile);  // No error handling
```

### 4. Document Error Behavior

**Document what errors can occur:**

```pascal
{*
  Opens a file for reading.
  
  Returns:
    true if file opened successfully
    false if file not found or cannot be opened
  
  Errors:
    Does not raise exceptions
*}
function TryOpenFile(filename: string; out file: TFile): boolean;
```

### 5. Choose Based on Context

**Consider context when choosing:**

```pascal
// Performance-critical: Use error codes
function TryFastOperation(out result: integer): boolean;

// Deep call stack: Use exceptions
procedure DeepOperation;  // Exception propagates automatically

// Expected condition: Use error codes
function TryOptionalOperation: boolean;

// Programming error: Use exception
function GetValue(index: integer): integer;  // Should be valid
```

---

## Exercises

### Exercise 1: Exception vs Error Code

Write code that:
1. Uses exceptions for unexpected errors
2. Uses error codes for expected conditions
3. Demonstrates when to use each
4. Shows the difference

### Exercise 2: Result Pattern

Implement:
1. Result<T, E> type
2. Functions returning Result
3. Usage of Result pattern
4. Comparison with exceptions

### Exercise 3: Error Recovery

Write code that:
1. Implements retry pattern
2. Implements fallback pattern
3. Handles partial success
4. Recovers from errors

### Exercise 4: Error Handling Strategy

Choose and implement:
1. Appropriate error handling strategy
2. Document error behavior
3. Handle errors consistently
4. Test error scenarios

---

**Previous Section:** [Resource Management](./03_ResourceManagement.md)  
**Next Section:** [Defensive Programming](./05_DefensiveProgramming.md)  
**Last Updated:** 2025-01-XX

