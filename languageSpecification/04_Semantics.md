# SuperPascal Language Specification — Semantics

## Semantic Rules and Language Meaning

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

---

## 1. Semantic Analysis Overview

Semantic analysis determines the **meaning** of a program, beyond syntax:

- Name resolution (what does this identifier refer to?)
- Type checking (are types compatible?)
- Expression evaluation (what is the result type?)
- Control flow analysis (is code reachable?)
- Constant folding (can this be computed at compile time?)

---

## 2. Name Resolution and Scoping

### 2.1 Scope Rules

SuperPascal uses **lexical scoping** (static scoping):

1. **Local scope** (procedure/function body)
2. **Parameter scope** (procedure/function parameters)
3. **Unit-level scope** (unit interface + implementation)
4. **Imported unit scope** (units in `uses` clause)
5. **Global scope** (program-level)

**Resolution order**: Inner to outer (most specific first).

### 2.2 Identifier Resolution

When an identifier is encountered:

1. Search current scope
2. Search enclosing scopes (outward)
3. Search imported units (in `uses` order)
4. If not found: **error**

### 2.3 Shadowing

**Allowed**: Inner scope can shadow outer scope:

```pascal
var x: integer;  // Outer x

procedure Test;
var x: integer;  // Inner x shadows outer x
begin
  x := 5;  // Refers to inner x
end;
```

**Not allowed**: Same scope cannot redeclare:

```pascal
var x: integer;
var x: integer;  // ERROR: x already declared
```

### 2.4 Forward Declarations

Procedures/functions can be forward declared:

```pascal
procedure B; forward;

procedure A;
begin
  B;  // OK: B is forward declared
end;

procedure B;
begin
  // implementation
end;
```

---

## 3. Expression Semantics

### 3.1 Expression Evaluation Order

**Left-to-right evaluation** for:
- Function arguments
- Binary operators
- Array indexing
- Field access

**Example:**
```pascal
f(g(), h());  // g() evaluated before h()
a[i] + b[j];  // a[i] evaluated before b[j]
```

### 3.2 Short-Circuit Evaluation

**Logical operators** short-circuit:

- `A and B`: If `A` is false, `B` is not evaluated
- `A or B`: If `A` is true, `B` is not evaluated

**Example:**
```pascal
if (ptr <> nil) and (ptr^.Value > 0) then
  // Safe: ptr^.Value only evaluated if ptr <> nil
```

### 3.3 Constant Folding

Expressions with compile-time constants are evaluated at compile time:

```pascal
const X = 5 + 3;  // Folds to 8
const Y = X * 2;  // Folds to 16
```

**Foldable operations:**
- Arithmetic on constants
- Logical operations on constants
- Comparisons on constants
- String concatenation of literals

**Not foldable:**
- Operations involving variables
- Function calls (except built-in compile-time functions)

### 3.4 Expression Result Types

**Arithmetic:**
- `integer op integer` → `integer`
- `byte op byte` → `integer` (promoted)
- `word op word` → `word` or `integer` (depending on operation)

**Comparison:**
- Any comparable types → `boolean`

**Logical:**
- `boolean op boolean` → `boolean`

**String:**
- `string + string` → `string`
- `string op string` → `boolean` (comparison)

**Pointer Arithmetic (SuperPascal extension):**
- `^T + integer` → `^T` (pointer to T, scaled by sizeof(T))
- `^T - integer` → `^T` (pointer to T, scaled by sizeof(T))
- `^T - ^T` → `integer` (difference in elements, not bytes)
- Arithmetic is **type-aware**: scaling is automatic based on pointed-to type
- No bounds checking: accessing invalid memory is undefined behavior

**Pointer Arithmetic Examples:**
```pascal
var p, q: ^integer;
var arr: array[0..9] of integer;
var offset: integer;

p := @arr[0];        // p points to arr[0]
p := p + 5;          // p now points to arr[5] (advances by 5 * 2 = 10 bytes)
q := p - 2;          // q points to arr[3] (decrements by 2 * 2 = 4 bytes)
offset := p - @arr[0]; // offset = 5 (element difference, not byte difference)
```

---

## 4. Assignment Semantics

### 4.1 Assignment Rules

Assignment `LHS := RHS` requires:

1. `LHS` is assignable (variable, field, array element, pointer dereference)
2. `RHS` type is compatible with `LHS` type
3. For subranges: value must fit in range (runtime check in debug)

### 4.2 Assignment Types

**Value assignment** (default):
- Copies value from RHS to LHS
- For records: copies all fields
- For arrays: copies all elements
- For strings: copies length + data

**Reference assignment** (classes, pointers):
- Copies reference (address)
- Does not copy object data

### 4.3 Multiple Assignment

Not directly supported (use multiple statements):

```pascal
x := 5;
y := 5;  // Not: x := y := 5;
```

---

## 5. Procedure and Function Call Semantics

### 5.1 Parameter Passing

**By value** (default):
- Parameter value is copied
- Modifications to parameter do not affect caller
- Used for input parameters

**By reference** (`var`):
- Parameter address is passed
- Modifications affect caller's variable
- Used for output/input-output parameters

**By constant reference** (`const`):
- Parameter address is passed (may be optimized)
- Modifications are not allowed
- Used for large input parameters (optimization)

### 5.2 Argument Evaluation

**Left-to-right evaluation**:

```pascal
procedure Test(a, b: integer);
begin
  // a evaluated before b
end;

Test(f(), g());  // f() called before g()
```

### 5.3 Return Values

**Functions:**
- Return value via `Result` variable
- `Result` type must match function return type
- `Result` is implicitly declared

**Procedures:**
- No return value
- Use `var` parameters for output

---

## 6. Method Dispatch

### 6.1 Non-Virtual Methods

**Resolution**: Compile time

```pascal
var obj: TEntity;
obj.NonVirtualMethod;  // Direct call to TEntity.NonVirtualMethod
```

**Rules:**
- Method address known at compile time
- No runtime lookup
- Fastest dispatch

### 6.2 Virtual Methods

**Resolution**: Runtime (via vtable)

```pascal
var obj: TObject;
obj := TEntity.Create;
obj.VirtualMethod;  // Calls TEntity.VirtualMethod via vtable
```

**Rules:**
- Method slot index determined at compile time
- Actual method address from vtable at runtime
- Supports polymorphism

### 6.3 Method Overriding

**Rules:**
- `override` method must match parent's `virtual` method signature
- Overridden method replaces parent's vtable slot
- Cannot override non-virtual method

---

## 7. Control Flow Semantics

### 7.1 If Statement

**Semantics:**
1. Evaluate condition
2. If true: execute `then` branch
3. If false: execute `else` branch (if present)
4. Continue after statement

**Type requirement**: Condition must be `boolean`.

### 7.2 While Statement

**Semantics:**
1. Evaluate condition
2. If true: execute body, then repeat from step 1
3. If false: continue after statement

**Type requirement**: Condition must be `boolean`.

### 7.3 Repeat Statement

**Semantics:**
1. Execute body
2. Evaluate condition
3. If false: repeat from step 1
4. If true: continue after statement

**Note**: Body executes at least once.

### 7.4 For Statement

**Semantics:**
1. Evaluate initial value
2. Evaluate final value
3. Assign initial to loop variable
4. If direction is `to` and variable ≤ final: execute body, increment, repeat
5. If direction is `downto` and variable ≥ final: execute body, decrement, repeat
6. Continue after statement

**Rules:**
- Loop variable is read-only in body
- Initial and final evaluated once
- Loop variable must be ordinal type

### 7.5 Case Statement

**Semantics:**
1. Evaluate case expression
2. Find matching label
3. Execute corresponding statement
4. Continue after `end`

**Rules:**
- Expression must be ordinal type
- Labels must be compile-time constants
- Labels must be unique
- `else` clause executes if no match

---

## 8. Exception Semantics

### 8.1 Try-Except

**Semantics:**
1. Push exception frame
2. Execute `try` block
3. If exception raised: pop frame, execute `except` block
4. If no exception: pop frame, skip `except` block
5. Continue after statement

### 8.2 Try-Finally

**Semantics:**
1. Push exception frame
2. Execute `try` block
3. Always execute `finally` block (even if exception)
4. Pop frame
5. If exception was active: re-raise
6. Continue after statement

### 8.3 Raise

**Semantics:**
1. Unwind stack to nearest exception frame
2. Restore stack pointer and frame pointer
3. Jump to exception handler
4. If no frame: abort program

---

## 9. Memory Model

### 9.1 Storage Classes

**Static** (global variables):
- Allocated at program start
- Lifetime: entire program execution
- Initialized to zero (or constant value)

**Automatic** (local variables):
- Allocated on stack (frame)
- Lifetime: procedure/function execution
- Not initialized (undefined until assigned)

**Heap** (dynamic allocation):
- Allocated via `New` or `GetMem`
- Lifetime: until `Dispose` or `FreeMem`
- Not initialized (undefined until assigned)

### 9.2 Memory Layout

**Stack** (grows downward):
- Function frames
- Local variables
- Parameters
- Return addresses

**Heap** (managed by runtime):
- Class instances
- Dynamically allocated records
- Strings (if allocated)

**Static** (fixed addresses):
- Global variables
- Constants
- Code

### 9.3 Lifetime Rules

**Variables:**
- Scope determines visibility
- Storage class determines lifetime
- Out-of-scope variables are inaccessible

**Objects:**
- Created with `New` or constructor
- Destroyed with `Dispose` or destructor
- References can outlive objects (dangling pointer risk)

---

## 10. Constant Semantics

### 10.1 Constant Evaluation

Constants must be evaluable at compile time:

```pascal
const
  X = 5 + 3;        // OK: compile-time constant
  Y = X * 2;        // OK: folds to 16
  // Z = ReadInt;   // ERROR: not compile-time
```

### 10.2 Constant Types

Constants infer type from value:

```pascal
const
  I = 42;        // integer
  C = 'A';      // char
  S = 'Hello';  // string
  B = true;     // boolean
```

### 10.3 Typed Constants

Constants can have explicit type:

```pascal
const
  X: integer = 42;
  Y: string = 'Hello';
```

---

## 11. Unit Semantics

### 11.1 Unit Initialization

Units can have initialization code:

```pascal
unit MyUnit;
implementation
begin
  // Initialization code
  // Runs at program start
end.
```

**Order:**
1. Initialize units in dependency order
2. Run unit initialization blocks
3. Run program main block

### 11.2 Unit Finalization

Not supported in initial version (future feature).

### 11.3 Interface vs Implementation

**Interface:**
- Exported declarations (visible to other units)
- Types, constants, procedures, functions
- No implementation

**Implementation:**
- Private declarations (not exported)
- Implementations of interface declarations
- Additional private helpers

---

## 12. Error Conditions

### 12.1 Compile-Time Errors

- Syntax errors
- Type errors
- Undefined identifiers
- Duplicate declarations
- Invalid operations

### 12.2 Runtime Errors (Debug Mode)

- Array index out of bounds
- Subrange value out of range
- Nil pointer dereference
- Division by zero
- String length overflow

### 12.3 Undefined Behavior

- Accessing uninitialized variables
- Dereferencing invalid pointers
- Modifying constants (if somehow possible)
- Stack overflow

---

## 13. Semantic Analysis Summary

**Key Phases:**
1. **Name resolution**: Resolve all identifiers
2. **Type checking**: Verify type compatibility
3. **Constant folding**: Evaluate compile-time constants
4. **Control flow**: Verify reachability and returns
5. **Error reporting**: Collect and report semantic errors

**Output:**
- Annotated AST (with types and symbols)
- List of semantic errors
- Symbol table
- Type information for code generation

---

**End of Semantics Specification**

