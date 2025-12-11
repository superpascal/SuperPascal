# Defensive Programming

**Part of:** [Chapter 32: Exception Handling and Error Management](./README.md)

---

## Introduction

Defensive programming prevents errors before they occur. This section teaches you how to validate input, use bounds checking effectively, apply null safety patterns, and use assertions and invariants.

**Key concepts:**
- **Input validation** — Checking inputs before use
- **Bounds checking** — Preventing out-of-bounds access
- **Null safety** — Handling nil pointers safely
- **Assertions** — Checking invariants and assumptions

---

## Input Validation

### Why Validate Input?

**Input validation prevents:**
- Invalid data causing errors
- Security vulnerabilities
- Unexpected behavior
- Program crashes

### Validating Function Parameters

**Check parameters at function entry:**

```pascal
function CalculateAverage(values: array of integer): Q8.8;
begin
  // Validate input
  if Length(values) = 0 then
    raise EInvalidArgument.Create('Array cannot be empty');
  
  // Now safe to use
  var sum := 0;
  var i: word;
  for i := 0 to Length(values) - 1 do
    sum := sum + values[i];
  
  CalculateAverage := Q8_8(sum) / Q8_8(Length(values));
end;
```

### Validating User Input

**Validate user input:**

```pascal
function GetPlayerName: string;
var
  name: string;
begin
  repeat
    Write('Enter player name: ');
    ReadLn(name);
    
    // Validate
    if Length(name) = 0 then
      WriteLn('Name cannot be empty')
    else if Length(name) > 20 then
      WriteLn('Name too long (max 20 characters)')
    else if not IsValidName(name) then
      WriteLn('Name contains invalid characters')
    else
      Break;  // Valid input
  until false;
  
  GetPlayerName := name;
end;
```

### Validating Range Values

**Check value ranges:**

```pascal
procedure SetVolume(volume: byte);
begin
  // Validate range
  if volume > 100 then
    raise EInvalidArgument.Create('Volume must be 0-100');
  
  // Now safe to use
  AudioVolume := volume;
end;

function GetArrayValue(index: integer; maxIndex: integer): integer;
begin
  // Validate bounds
  if (index < 0) or (index > maxIndex) then
    raise EOutOfBounds.CreateFmt('Index %d out of range [0..%d]', 
                                  [index, maxIndex]);
  
  GetArrayValue := arr[index];
end;
```

### Validating State

**Check state before operations:**

```pascal
procedure SaveGame;
begin
  // Validate state
  if not GameInitialized then
    raise EInvalidState.Create('Game not initialized');
  
  if SaveInProgress then
    raise EInvalidState.Create('Save already in progress');
  
  // Now safe to save
  SaveInProgress := true;
  try
    DoSave;
  finally
    SaveInProgress := false;
  end;
end;
```

---

## Bounds Checking

### Array Bounds Checking

**Always check array bounds:**

```pascal
function SafeGetValue(arr: array of integer; index: integer): integer;
begin
  // Check bounds
  if (index < 0) or (index >= Length(arr)) then
    raise EOutOfBounds.CreateFmt('Index %d out of bounds [0..%d]', 
                                  [index, Length(arr) - 1]);
  
  SafeGetValue := arr[index];
end;

procedure SafeSetValue(var arr: array of integer; 
                       index: integer; value: integer);
begin
  // Check bounds
  if (index < 0) or (index >= Length(arr)) then
    raise EOutOfBounds.CreateFmt('Index %d out of bounds [0..%d]', 
                                  [index, Length(arr) - 1]);
  
  arr[index] := value;
end;
```

### Runtime Bounds Checking

**SuperPascal provides runtime bounds checking:**

```pascal
// With {$RANGE_CHECK ON} (default in debug)
var
  arr: array[0..9] of integer;
begin
  arr[10] := 5;  // Raises EOutOfBounds at runtime
end;

// Can disable in release for performance
{$RANGE_CHECK OFF}
arr[10] := 5;  // No check, may cause undefined behavior
{$RANGE_CHECK ON}
```

### Pointer Bounds Checking

**Check pointer bounds:**

```pascal
function SafeDereference(ptr: ^TRecord; size: word): ^TRecord;
begin
  // Check pointer validity
  if ptr = nil then
    raise ENilPointer.Create('Pointer is nil');
  
  // Check bounds (if you have bounds information)
  // This is platform/implementation specific
  
  SafeDereference := ptr;
end;
```

### String Bounds

**Check string bounds:**

```pascal
function SafeSubstring(s: string; start, length: integer): string;
begin
  // Validate bounds
  if (start < 1) or (start > Length(s)) then
    raise EOutOfBounds.Create('Start index out of bounds');
  
  if (start + length - 1) > Length(s) then
    raise EOutOfBounds.Create('Substring extends beyond string');
  
  // Now safe to extract
  SafeSubstring := Copy(s, start, length);
end;
```

---

## Null Safety Patterns

### Checking for Nil

**Always check pointers before use:**

```pascal
procedure ProcessRecord(ptr: ^TRecord);
begin
  // Check for nil
  if ptr = nil then
    raise ENilPointer.Create('Pointer is nil');
  
  // Now safe to use
  ptr^.Field := value;
  ptr^.Process;
end;
```

### Null-Safe Access Pattern

**Pattern for safe access:**

```pascal
function SafeGetField(ptr: ^TRecord): integer;
begin
  if ptr = nil then
  begin
    SafeGetField := 0;  // Default value
    Exit;
  end;
  
  SafeGetField := ptr^.Field;
end;

// Or raise exception
function GetField(ptr: ^TRecord): integer;
begin
  if ptr = nil then
    raise ENilPointer.Create('Cannot access field: pointer is nil');
  
  GetField := ptr^.Field;
end;
```

### Null-Safe Method Calls

**Check before method calls:**

```pascal
procedure CallMethod(obj: TMyObject);
begin
  if obj = nil then
    raise ENilPointer.Create('Object is nil');
  
  obj.DoSomething;  // Safe to call
end;

// Or use optional pattern
procedure OptionalCall(obj: TMyObject);
begin
  if obj <> nil then
    obj.DoSomething;  // Only call if not nil
end;
```

### Initialization Checks

**Check initialization:**

```pascal
var
  Initialized: boolean = false;
  Data: TData;

procedure UseData;
begin
  if not Initialized then
    raise ENotInitialized.Create('Data not initialized');
  
  // Now safe to use
  ProcessData(Data);
end;

procedure InitializeData;
begin
  if Initialized then
    raise EAlreadyInitialized.Create('Data already initialized');
  
  Initialize(Data);
  Initialized := true;
end;
```

---

## Assertions and Invariants

### What are Assertions?

**Assertions check assumptions:**
- Should always be true
- Indicate programming errors if false
- Can be disabled in release builds
- Help catch bugs during development

### Using Assertions

**Check assumptions with assertions:**

```pascal
procedure ProcessData(data: ^TData);
begin
  // Assert: data should not be nil
  Assert(data <> nil, 'Data pointer is nil');
  
  // Assert: data should be valid
  Assert(data^.IsValid, 'Data is invalid');
  
  // Process data...
end;

function Divide(a, b: integer): integer;
begin
  // Assert: b should not be zero
  Assert(b <> 0, 'Division by zero');
  
  Divide := a div b;
end;
```

### Class Invariants

**Check class invariants:**

```pascal
type
  TPlayer = class
  private
    FHealth: word;
    FMaxHealth: word;
    
    procedure CheckInvariant;
  public
    procedure SetHealth(value: word);
    procedure TakeDamage(amount: word);
  end;

procedure TPlayer.CheckInvariant;
begin
  Assert(FHealth <= FMaxHealth, 'Health exceeds max health');
  Assert(FMaxHealth > 0, 'Max health must be positive');
end;

procedure TPlayer.SetHealth(value: word);
begin
  FHealth := value;
  CheckInvariant;  // Verify invariant maintained
end;
```

### Loop Invariants

**Check loop invariants:**

```pascal
function BinarySearch(arr: array of integer; value: integer): integer;
var
  left, right, mid: integer;
begin
  left := 0;
  right := Length(arr) - 1;
  
  while left <= right do
  begin
    // Invariant: value is in [left..right] if it exists
    Assert((left >= 0) and (right < Length(arr)), 
           'Search bounds invalid');
    
    mid := (left + right) div 2;
    
    if arr[mid] = value then
    begin
      BinarySearch := mid;
      Exit;
    end
    else if arr[mid] < value then
      left := mid + 1
    else
      right := mid - 1;
  end;
  
  BinarySearch := -1;  // Not found
end;
```

### Preconditions and Postconditions

**Check preconditions and postconditions:**

```pascal
function SortedArray(arr: array of integer): boolean;
var
  i: integer;
begin
  for i := 0 to Length(arr) - 2 do
  begin
    if arr[i] > arr[i + 1] then
    begin
      SortedArray := false;
      Exit;
    end;
  end;
  SortedArray := true;
end;

function BinarySearch(arr: array of integer; value: integer): integer;
begin
  // Precondition: array must be sorted
  Assert(SortedArray(arr), 'Array must be sorted');
  
  // Search...
  result := DoBinarySearch(arr, value);
  
  // Postcondition: result is valid index or -1
  Assert((result >= -1) and (result < Length(arr)), 
         'Invalid search result');
end;
```

---

## Defensive Programming Patterns

### Pattern 1: Validate Early

**Validate at function entry:**

```pascal
function ProcessData(data: TData): TResult;
begin
  // Validate immediately
  if not IsValid(data) then
    raise EInvalidArgument.Create('Invalid data');
  
  // Rest of function can assume valid data
  // ...
end;
```

### Pattern 2: Fail Fast

**Fail immediately on invalid state:**

```pascal
procedure CriticalOperation;
begin
  // Check prerequisites
  if not IsReady then
    raise EInvalidState.Create('System not ready');
  
  if not HasPermission then
    raise EAccessDenied.Create('Insufficient permissions');
  
  // Proceed with operation
end;
```

### Pattern 3: Defensive Copies

**Make defensive copies:**

```pascal
procedure SetData(const newData: TData);
begin
  // Make copy to prevent external modification
  FData := newData;  // Record copy (by value)
  
  // For reference types, might need deep copy
  // FData := DeepCopy(newData);
end;
```

### Pattern 4: Default Values

**Provide safe defaults:**

```pascal
function SafeGetValue(ptr: ^TRecord): integer;
begin
  if ptr = nil then
    SafeGetValue := 0  // Safe default
  else
    SafeGetValue := ptr^.Value;
end;
```

---

## Best Practices

### 1. Validate All Input

**Check inputs before use:**

```pascal
// ✅ GOOD: Validate input
function Process(value: integer): integer;
begin
  if value < 0 then
    raise EInvalidArgument.Create('Value must be non-negative');
  
  Process := value * 2;
end;

// ❌ BAD: Assume input is valid
function Process(value: integer): integer;
begin
  Process := value * 2;  // May have negative value
end;
```

### 2. Check Bounds Always

**Always check array bounds:**

```pascal
// ✅ GOOD: Check bounds
function GetValue(index: integer): integer;
begin
  if (index < 0) or (index >= Length(arr)) then
    raise EOutOfBounds.Create('Index out of bounds');
  
  GetValue := arr[index];
end;

// ❌ BAD: Assume index is valid
function GetValue(index: integer): integer;
begin
  GetValue := arr[index];  // May be out of bounds
end;
```

### 3. Check for Nil

**Always check pointers:**

```pascal
// ✅ GOOD: Check for nil
procedure Use(ptr: ^TRecord);
begin
  if ptr = nil then
    raise ENilPointer.Create('Pointer is nil');
  
  ptr^.Field := value;
end;

// ❌ BAD: Assume pointer is valid
procedure Use(ptr: ^TRecord);
begin
  ptr^.Field := value;  // May crash if nil
end;
```

### 4. Use Assertions for Invariants

**Assert assumptions:**

```pascal
// ✅ GOOD: Assert invariants
procedure Process(data: TData);
begin
  Assert(IsValid(data), 'Data must be valid');
  // Process...
end;

// ❌ BAD: No checks
procedure Process(data: TData);
begin
  // May process invalid data
end;
```

### 5. Document Assumptions

**Document what you assume:**

```pascal
{*
  Processes data array.
  
  Preconditions:
    - data must not be nil
    - data must be valid (IsValid returns true)
    - index must be in range [0..Length(data)-1]
  
  Postconditions:
    - result is valid processed data
    - original data unchanged
*}
function ProcessData(data: array of integer; index: integer): TProcessedData;
```

---

## Exercises

### Exercise 1: Input Validation

Write code that:
1. Validates function parameters
2. Validates user input
3. Validates state before operations
4. Handles validation errors appropriately

### Exercise 2: Bounds Checking

Write code that:
1. Checks array bounds
2. Checks string bounds
3. Uses runtime bounds checking
4. Handles out-of-bounds errors

### Exercise 3: Null Safety

Write code that:
1. Checks for nil pointers
2. Uses null-safe access patterns
3. Handles uninitialized data
4. Prevents null pointer errors

### Exercise 4: Assertions

Write code that:
1. Uses assertions for invariants
2. Checks preconditions
3. Verifies postconditions
4. Documents assumptions

---

## Security Basics

### Understanding Security in Programming

**Security means:**
- Protecting data from unauthorized access
- Preventing malicious input from causing harm
- Ensuring program behavior is predictable
- Avoiding vulnerabilities that attackers can exploit

**Target Levels:**
- **GCSE:** Basic input validation, safe practices
- **A-Level:** Common vulnerabilities, secure coding patterns
- **University:** Advanced security concepts, threat modeling

### Common Vulnerabilities

#### Buffer Overflow

**What it is:**
- Writing data beyond allocated memory
- Can overwrite other data or code
- Can allow code injection

**How to prevent:**
```pascal
// ❌ BAD: No bounds checking
procedure UnsafeCopy(src: string; var dest: array[0..9] of char);
var
  i: integer;
begin
  for i := 1 to Length(src) do
    dest[i-1] := src[i];  // May overflow if src > 10 chars
end;

// ✅ GOOD: Bounds checking
procedure SafeCopy(src: string; var dest: array[0..9] of char);
var
  i, maxLen: integer;
begin
  maxLen := Min(Length(src), 10);  // Limit to array size
  for i := 1 to maxLen do
    dest[i-1] := src[i];
end;
```

#### Integer Overflow

**What it is:**
- Arithmetic operations exceed type limits
- Can cause unexpected behavior
- Can bypass security checks

**How to prevent:**
```pascal
// ❌ BAD: No overflow check
function UnsafeAdd(a, b: integer): integer;
begin
  UnsafeAdd := a + b;  // May overflow
end;

// ✅ GOOD: Overflow check
function SafeAdd(a, b: integer): integer;
begin
  if (a > 0) and (b > MaxInt - a) then
    raise EOverflow.Create('Addition would overflow');
  if (a < 0) and (b < -MaxInt - a) then
    raise EOverflow.Create('Addition would underflow');
  
  SafeAdd := a + b;
end;
```

#### Format String Vulnerabilities

**What it is:**
- User input used directly in format strings
- Can allow information disclosure
- Can enable code injection

**How to prevent:**
```pascal
// ❌ BAD: User input in format string
procedure UnsafePrint(userInput: string);
begin
  WriteLn(userInput);  // Dangerous if userInput contains format specifiers
end;

// ✅ GOOD: Sanitize or escape
procedure SafePrint(userInput: string);
begin
  // Escape or validate userInput
  WriteLn(SanitizeString(userInput));
end;
```

### Input Sanitization

**Sanitize all user input:**
```pascal
function SanitizeString(input: string): string;
var
  i: integer;
  result: string;
begin
  result := '';
  for i := 1 to Length(input) do
  begin
    // Only allow safe characters
    if IsSafeChar(input[i]) then
      result := result + input[i];
  end;
  SanitizeString := result;
end;

function IsSafeChar(c: char): boolean;
begin
  // Allow alphanumeric and safe punctuation
  IsSafeChar := ((c >= 'A') and (c <= 'Z')) or
                ((c >= 'a') and (c <= 'z')) or
                ((c >= '0') and (c <= '9')) or
                (c in [' ', '-', '_', '.']);
end;
```

### Safe String Handling

**Always validate string operations:**
```pascal
function SafeSubstring(s: string; start, len: integer): string;
begin
  // Validate bounds
  if (start < 1) or (start > Length(s)) then
    raise EOutOfBounds.Create('Start index out of bounds');
  
  if (start + len - 1) > Length(s) then
    raise EOutOfBounds.Create('Substring extends beyond string');
  
  if len < 0 then
    raise EInvalidArgument.Create('Length cannot be negative');
  
  SafeSubstring := Copy(s, start, len);
end;
```

### Secure Memory Operations

**Safe memory access:**
```pascal
procedure SafeMemoryCopy(src, dest: ^byte; size: word);
begin
  // Validate pointers
  if src = nil then
    raise ENilPointer.Create('Source pointer is nil');
  if dest = nil then
    raise ENilPointer.Create('Destination pointer is nil');
  
  // Validate size
  if size = 0 then
    Exit;  // Nothing to copy
  
  // Check for overlap (simplified)
  // In real code, check if ranges overlap
  
  // Perform copy
  Move(src^, dest^, size);
end;
```

### Safe File Operations

**Validate file operations:**
```pascal
procedure SafeReadFile(filename: string; var data: TData);
begin
  // Validate filename
  if Length(filename) = 0 then
    raise EInvalidArgument.Create('Filename cannot be empty');
  
  // Check for path traversal attempts
  if ContainsPathTraversal(filename) then
    raise ESecurityError.Create('Invalid filename');
  
  // Validate file exists
  if not FileExists(filename) then
    raise EFileNotFound.Create('File does not exist');
  
  // Read file safely
  // ...
end;

function ContainsPathTraversal(path: string): boolean;
begin
  // Check for ".." or absolute paths
  ContainsPathTraversal := (Pos('..', path) > 0) or
                           (path[1] = '/') or
                           (Pos(':', path) > 0);
end;
```

### Principle of Least Privilege

**Only request what you need:**
```pascal
// ✅ GOOD: Minimal access
procedure ProcessUserData(userId: integer);
var
  userData: TUserData;
begin
  // Only load what's needed
  userData := LoadUserData(userId);  // Not all user data
  Process(userData);
end;

// ❌ BAD: Excessive access
procedure ProcessUserData(userId: integer);
var
  allUsers: array of TUserData;
begin
  allUsers := LoadAllUsers;  // Loads everything
  Process(allUsers[userId]);
end;
```

### Secure Defaults

**Fail securely:**
```pascal
function AuthenticateUser(username, password: string): boolean;
begin
  // Default to false (deny access)
  AuthenticateUser := false;
  
  // Validate input
  if (Length(username) = 0) or (Length(password) = 0) then
    Exit;  // Fail securely
  
  // Perform authentication
  // If anything fails, result remains false
  if ValidateCredentials(username, password) then
    AuthenticateUser := true;
end;
```

### Security Best Practices

#### 1. Validate All Input

**Never trust input:**
```pascal
// ✅ GOOD: Validate everything
function ProcessInput(input: string): TResult;
begin
  if not IsValidInput(input) then
    raise EInvalidInput.Create('Invalid input');
  
  ProcessInput := DoProcess(input);
end;
```

#### 2. Use Bounds Checking

**Always check bounds:**
```pascal
// ✅ GOOD: Always check
function SafeAccess(arr: array of integer; index: integer): integer;
begin
  if (index < 0) or (index >= Length(arr)) then
    raise EOutOfBounds.Create('Index out of bounds');
  
  SafeAccess := arr[index];
end;
```

#### 3. Avoid Unsafe Operations

**Prefer safe alternatives:**
```pascal
// ❌ BAD: Unsafe pointer arithmetic
var
  ptr: ^integer;
begin
  ptr := @arr[0];
  ptr := ptr + unsafeOffset;  // Dangerous
end;

// ✅ GOOD: Safe array access
begin
  if (index >= 0) and (index < Length(arr)) then
    value := arr[index];
end;
```

#### 4. Handle Errors Securely

**Don't leak information:**
```pascal
// ❌ BAD: Leaks internal details
function Login(username, password: string): boolean;
begin
  if not UserExists(username) then
    raise Exception.Create('User ' + username + ' does not exist');  // Information leak
  
  // ...
end;

// ✅ GOOD: Generic error message
function Login(username, password: string): boolean;
begin
  if not UserExists(username) then
    raise EAuthenticationFailed.Create('Invalid credentials');  // Generic message
  
  // ...
end;
```

#### 5. Keep Security Checks Simple

**Simple checks are harder to bypass:**
```pascal
// ✅ GOOD: Simple, clear check
function IsAuthorized(user: TUser; action: string): boolean;
begin
  IsAuthorized := user.HasPermission(action);
end;

// ❌ BAD: Complex, may have bypasses
function IsAuthorized(user: TUser; action: string): boolean;
begin
  // Complex logic with many conditions
  // May have edge cases that bypass security
end;
```

---

## Security Exercises

### Exercise 1: Input Validation

Write secure input validation:
1. Validate user names (alphanumeric, length limits)
2. Validate numeric input (range checks)
3. Sanitize string input (remove dangerous characters)
4. Validate file paths (prevent path traversal)

### Exercise 2: Bounds Checking

Implement secure array access:
1. Safe array access with bounds checking
2. Safe string operations
3. Safe memory copy operations
4. Handle overflow conditions

### Exercise 3: Secure File Operations

Write secure file handling:
1. Validate filenames
2. Prevent path traversal
3. Check file permissions
4. Handle file errors securely

---

**Previous Section:** [Error Handling Patterns](./04_ErrorHandlingPatterns.md)  
**Next Chapter:** [Chapter 33: Memory Management and Resource Lifecycle](../31_MemoryManagementAndResourceLifecycle/README.md)  
**Last Updated:** 2025-01-XX

