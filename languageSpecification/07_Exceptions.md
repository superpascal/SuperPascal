# SuperPascal Language Specification — Exception Handling

## Structured Error Handling Model

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

---

## 1. Error Handling Model Overview

SuperPascal provides **two complementary error handling mechanisms**:

### 1.1 Result<T, E> Pattern (Preferred)

**Rust-style Result type** for expected errors:
- **Zero-cost** when no error (tagged union)
- **Explicit error handling** (compiler enforces)
- **Type-safe** error codes via enums
- **Optimal for retro platforms** (no stack unwinding)
- **Preferred pattern** for functions that can fail

### 1.2 Exception Model (For Exceptional Cases)

**setjmp/longjmp-style exception model** for unexpected errors:
- **Low overhead** when no exceptions occur
- **Stack unwinding** when exceptions are raised
- **Structured syntax** (`try/except/finally`)
- **Use for** programming errors, system failures, truly exceptional cases

**Guideline:** Use `Result<T, E>` for expected errors, exceptions for unexpected errors.

---

## 2. Result<T, E> Pattern (SuperPascal Extension)

### 2.1 Result Type

**Result is a built-in generic type:**
```pascal
type Result<T, E> = record
  case IsOk: boolean of
    true:  (Value: T);
    false: (Error: E);
  end;
```

**Usage:**
```pascal
type ParseError = (InvalidFormat, OutOfRange, EmptyString);

function ParseInt(s: string): Result<integer, ParseError>;
begin
  if Length(s) = 0 then
    ParseInt := Result<integer, ParseError>.Error(EmptyString)
  else if not IsValidNumber(s) then
    ParseInt := Result<integer, ParseError>.Error(InvalidFormat)
  else
    ParseInt := Result<integer, ParseError>.Ok(StrToInt(s));
end;
```

### 2.2 Result Construction

**Ok value:**
```pascal
Result<integer, ParseError>.Ok(42)
```

**Error value:**
```pascal
Result<integer, ParseError>.Error(InvalidFormat)
```

### 2.3 Result Pattern Matching

**Match on Result:**
```pascal
var result: Result<integer, ParseError>;
var value: integer;
begin
  result := ParseInt('42');
  
  case result.IsOk of
    true:  value := result.Value;
    false: WriteLn('Error: ', result.Error);
  end;
end.
```

**Or use helper functions:**
```pascal
function Unwrap<T, E>(r: Result<T, E>): T;
begin
  if r.IsOk then
    Unwrap := r.Value
  else
    raise;  // Raise exception if error
end;

function UnwrapOr<T, E>(r: Result<T, E>; default: T): T;
begin
  if r.IsOk then
    UnwrapOr := r.Value
  else
    UnwrapOr := default;
end;
```

### 2.4 Result with Enums

**Using enums for error types:**
```pascal
type
  FileError = (NotFound, PermissionDenied, DiskFull, InvalidPath);
  
function OpenFile(path: string): Result<FileHandle, FileError>;
begin
  if not FileExists(path) then
    OpenFile := Result<FileHandle, FileError>.Error(NotFound)
  else if not HasPermission(path) then
    OpenFile := Result<FileHandle, FileError>.Error(PermissionDenied)
  else
    OpenFile := Result<FileHandle, FileError>.Ok(CreateHandle(path));
end;
```

**Usage:**
```pascal
var result: Result<FileHandle, FileError>;
var handle: FileHandle;
begin
  result := OpenFile('data.txt');
  
  case result.IsOk of
    true:
      begin
        handle := result.Value;
        ProcessFile(handle);
      end;
    false:
      case result.Error of
        NotFound:         WriteLn('File not found');
        PermissionDenied: WriteLn('Permission denied');
        DiskFull:         WriteLn('Disk full');
        InvalidPath:      WriteLn('Invalid path');
      end;
  end;
end.
```

### 2.5 Result Propagation

**Propagate errors with `?` operator (future):**
```pascal
function ReadConfig: Result<Config, ConfigError>;
var fileResult: Result<string, FileError>;
var parseResult: Result<Config, ParseError>;
begin
  fileResult := ReadFile('config.txt');
  if not fileResult.IsOk then
    ReadConfig := Result<Config, ConfigError>.Error(FileError(fileResult.Error))
  else
  begin
    parseResult := ParseConfig(fileResult.Value);
    if not parseResult.IsOk then
      ReadConfig := Result<Config, ConfigError>.Error(ParseError(parseResult.Error))
    else
      ReadConfig := Result<Config, ConfigError>.Ok(parseResult.Value);
  end;
end;
```

### 2.6 Result vs Exceptions

**Use Result for:**
- Expected errors (parsing, validation, I/O)
- Functions that commonly fail
- Performance-critical paths
- Explicit error handling required

**Use Exceptions for:**
- Programming errors (bounds violations, nil dereference)
- System failures (out of memory, hardware errors)
- Truly exceptional cases
- When stack unwinding is acceptable

---

## 3. Exception Syntax

### 2.1 Try-Except

**Syntax:**
```pascal
try
  DangerousOperation;
except
  HandleError;
end;
```

**Semantics:**
1. Execute `try` block
2. If exception raised: jump to `except` block
3. If no exception: skip `except` block
4. Continue after `end`

### 2.2 Try-Finally

**Syntax:**
```pascal
try
  UseResource;
finally
  CleanupResource;
end;
```

**Semantics:**
1. Execute `try` block
2. **Always** execute `finally` block (even if exception)
3. If exception was active: re-raise after `finally`
4. Continue after `end`

### 2.3 Try-Except-Finally

**Syntax:**
```pascal
try
  Operation;
except
  HandleError;
finally
  Cleanup;
end;
```

**Semantics:**
1. Execute `try` block
2. If exception: execute `except` block
3. **Always** execute `finally` block
4. Continue after `end`

### 2.4 Raise

**Syntax:**
```pascal
raise;                    // Re-raise current exception
raise EError.Create;      // Raise new exception (future)
```

**Semantics:**
- Unwind stack to nearest exception frame
- Transfer control to exception handler
- If no frame: abort program

---

## 4. Exception Frame Structure

### 3.1 Frame Layout

```
Offset 0: Previous frame pointer (2 bytes)
Offset 2: Handler address (2 bytes)
Offset 4: Saved SP (2 bytes)
Offset 6: Saved IX (2 bytes)
Offset 8: Flags (1 byte)
          Bit 0: Active (1 = exception active)
          Bit 1-7: Reserved
Offset 9: Reserved (1 byte)
Total:   10 bytes
```

### 3.2 Frame Chain

Frames linked via `IY` register:

```
IY → Current frame (most recent)
     ↓
     Previous frame
     ↓
     Older frame
     ↓
     nil (no more frames)
```

### 3.3 Frame Management

**Pushing frame** (entering `try`):
1. Allocate frame on stack or in reserved area
2. Save current `IY` to frame's "Previous" field
3. Set frame's handler address
4. Save current `SP` and `IX`
5. Set `IY` to point to new frame

**Popping frame** (exiting `try`):
1. Restore `IY` from frame's "Previous" field
2. Deallocate frame

---

## 5. Exception Unwinding

### 4.1 Unwind Process

When `raise` is executed:

1. **Read current frame** from `IY`
2. **Restore stack pointer** from frame's saved `SP`
3. **Restore frame pointer** from frame's saved `IX`
4. **Jump to handler** address from frame
5. **Continue unwinding** if handler re-raises

### 4.2 Unwind Example

```pascal
procedure Level3;
begin
  try
    RaiseError;  // Raises exception
  except
    WriteLn('Caught in Level3');
    raise;       // Re-raises
  end;
end;

procedure Level2;
begin
  try
    Level3;
  except
    WriteLn('Caught in Level2');
  end;
end;
```

**Execution:**
1. `Level3` raises exception
2. Unwind to `Level3`'s handler: "Caught in Level3"
3. Re-raise from `Level3`
4. Unwind to `Level2`'s handler: "Caught in Level2"
5. Exception handled, continue

---

## 6. Exception Types

### 5.1 Basic Exception Model

**Initial implementation:**
- Generic exception (no type information)
- `raise` re-raises current exception
- `raise EError.Create` (future: typed exceptions)

### 5.2 Future: Typed Exceptions

**Planned syntax:**
```pascal
type
  EError = class
    Code: integer;
    Message: string;
  end;
  
  EFileError = class(EError)
    FileName: string;
  end;

try
  OpenFile('test.txt');
except
  on E: EFileError do
    WriteLn('File error: ', E.FileName);
  on E: EError do
    WriteLn('Error: ', E.Message);
end;
```

**Status**: Future feature, not in v1.0.

---

## 7. Runtime Integration

### 6.1 Runtime Exception Support

**Runtime provides:**
- Exception frame management
- Stack unwinding routines
- Exception raising routines
- Error reporting

### 6.2 SysUtils_Lite Integration

```pascal
unit SysUtils_Lite;

type
  EError = class
    Code: integer;
    Message: string;
  end;

procedure RaiseError(Code: integer; const Message: string);
```

**Usage:**
```pascal
uses SysUtils_Lite;

try
  if x < 0 then
    RaiseError(1, 'Negative value');
except
  WriteLn('Error caught');
end;
```

---

## 8. Exception Semantics

### 7.1 Exception Propagation

**Rules:**
- Exceptions propagate up call stack
- Each `try..except` can catch and handle
- Unhandled exceptions abort program
- `finally` always executes (even if exception)

### 7.2 Exception Safety

**Guarantees:**
- `finally` blocks always execute
- Stack is properly unwound
- Frame pointers are restored
- Resources are cleaned up

**No guarantees:**
- Partial object construction (if exception in constructor)
- Atomic operations (no transaction support)

### 7.3 Exception Cost

**No exception:**
- Minimal overhead (frame push/pop)
- Fast execution

**Exception raised:**
- Unwinding cost (proportional to stack depth)
- Handler execution
- Re-raising (if applicable)

---

## 9. Compiler Implementation

### 8.1 Code Generation

**Try block entry:**
```asm
; Push exception frame
call Runtime_PushExceptionFrame
; Frame address in HL
ld (IY), hl      ; Link to chain
```

**Try block exit (normal):**
```asm
call Runtime_PopExceptionFrame
```

**Raise:**
```asm
call Runtime_RaiseException
; Never returns (unwinds stack)
```

**Except handler:**
```asm
HandlerLabel:
  ; Exception handler code
  ; IY points to current frame
  ; SP and IX restored
```

### 8.2 Exception Tables

Compiler generates exception tables:

```
ExceptionTable:
  dw TryStart
  dw TryEnd
  dw HandlerAddress
  dw FrameSize
```

Used by runtime for unwinding.

---

## 10. Educational Considerations

### 9.1 Teaching Progression

**Stage 1: No exceptions**
- Use return codes
- Check results explicitly

**Stage 2: Try-Finally**
- Resource cleanup
- Guaranteed execution

**Stage 3: Try-Except**
- Error handling
- Exception propagation

### 9.2 Error Messages

**Clear error reporting:**
```
Error: Exception raised in UpdatePlayer at line 42
  Cause: Array index out of bounds
  Index: 100, Array size: 10
```

**Educational messages:**
```
Error: The program tried to access array element 100,
  but the array only has 10 elements (0-9).
  This raised an exception in UpdatePlayer.
```

---

## 11. Best Practices

### 10.1 Exception Usage

**Use exceptions for:**
- Unexpected errors (file not found, out of memory)
- Programming errors (bounds violations, nil dereference)
- System errors (hardware failures)

**Don't use exceptions for:**
- Expected conditions (end of file, user cancellation)
- Control flow (use if/else instead)
- Performance-critical paths (if possible)

### 10.2 Resource Management

**Pattern:**
```pascal
AcquireResource;
try
  UseResource;
finally
  ReleaseResource;  // Always executes
end;
```

**Guarantees cleanup** even if exception occurs.

### 10.3 Error Recovery

**Pattern:**
```pascal
try
  DangerousOperation;
except
  LogError;
  UseFallback;  // Recover gracefully
end;
```

**Allows program to continue** after error.

---

## 12. Limitations

### 11.1 Current Limitations

- No typed exceptions (v1.0)
- No exception inheritance
- No exception filters
- No async exception handling

### 11.2 Future Enhancements

- Typed exceptions (planned)
- Exception inheritance
- Better debugging support
- Performance optimizations

---

## 13. Summary

**Error Handling Strategy:**

**Result<T, E> Pattern (Preferred):**
- Zero-cost when no error (tagged union)
- Explicit error handling (compiler enforces)
- Type-safe error codes via enums
- Optimal for retro platforms
- Use for expected errors

**Exception Model:**
- Setjmp/longjmp-style unwinding
- Low overhead when unused
- Structured syntax
- Use for unexpected errors

**Key Features:**
- `Result<T, E>`: Type-safe error handling
- `try..except`: Catch exceptions
- `try..finally`: Guaranteed cleanup
- `raise`: Raise/re-raise exceptions
- Enums: Type-safe error codes

**Status:**
- Result<T, E>: v1.0
- Basic exception model: v1.0
- Typed exceptions: Future
- Result `?` operator: Future

**Guideline:**
- **Use Result<T, E>** for expected errors (parsing, validation, I/O)
- **Use Exceptions** for unexpected errors (programming errors, system failures)

---

**End of Error Handling Specification**

