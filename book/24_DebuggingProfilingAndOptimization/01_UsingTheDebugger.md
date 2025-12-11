# Using the Debugger

**Part of:** [Chapter 24: Debugging, Profiling, and Optimization](./README.md)

---

## Introduction

Debugging is essential for finding and fixing errors. This section teaches you how to use the debugger effectively to locate bugs and understand program behavior.

**Key concepts:**
- **Breakpoints** — Pause execution at specific lines
- **Stepping** — Execute code line by line
- **Variable inspection** — View variable values
- **Call stack** — See function call chain

---

## Understanding Debugging

### What is Debugging?

**Debugging is finding and fixing errors:**

- **Compile-time errors** — Syntax, type errors (caught by compiler)
- **Runtime errors** — Crashes, exceptions (caught at runtime)
- **Logic errors** — Wrong results (program runs but incorrect)

### Systematic Debugging Methodology

**A systematic approach to debugging:**

**Step 1: Reproduce the Error**
- Make the error happen consistently
- Note what you were doing when it occurred
- Identify the exact conditions that trigger it

**Step 2: Understand the Error**
- Read error messages carefully
- Identify error type (compile-time, runtime, logic)
- Note where the error occurs (line number, function)

**Step 3: Isolate the Problem**
- Narrow down to smallest code section
- Remove unrelated code
- Create minimal test case

**Step 4: Locate the Bug**
- Use debugger to step through code
- Set breakpoints at suspected locations
- Inspect variable values
- Trace execution flow

**Step 5: Understand Why**
- Why does the bug occur?
- What assumptions are wrong?
- What's the root cause?

**Step 6: Fix the Bug**
- Make the minimal fix
- Don't introduce new bugs
- Fix the root cause, not symptoms

**Step 7: Verify the Fix**
- Test the fix
- Test edge cases
- Ensure no regressions

**Step 8: Learn and Document**
- Understand what went wrong
- Document the fix
- Prevent similar bugs

### Debugging Checklist

**Before debugging:**
- [ ] Can I reproduce the error?
- [ ] Do I understand what should happen?
- [ ] Do I understand what actually happens?
- [ ] Have I read the error message?

**While debugging:**
- [ ] Have I isolated the problem?
- [ ] Am I using the debugger effectively?
- [ ] Am I checking variable values?
- [ ] Am I tracing execution flow?

**After fixing:**
- [ ] Does the fix work?
- [ ] Have I tested edge cases?
- [ ] Have I checked for regressions?
- [ ] Do I understand why it was broken?

### Common Error Patterns

**1. Off-by-One Errors**
- Array bounds: `for i := 0 to Length(arr) - 1` vs `for i := 0 to Length(arr)`
- Loop conditions: `while i < n` vs `while i <= n`
- Index calculations: `arr[i]` vs `arr[i+1]`

**2. Null/Uninitialized Variables**
- Using variable before initialization
- Forgetting to initialize arrays
- Not checking for nil pointers

**3. Type Mismatches**
- Wrong type in assignment
- Type conversion errors
- Integer vs fixed-point confusion

**4. Logic Errors**
- Wrong condition in if statement
- Incorrect loop bounds
- Wrong operator (and vs or)

**5. Memory Errors**
- Array out of bounds
- Use after free
- Memory leaks

### Debugging Process (Detailed)**

**1. Reproduce** — Make error happen consistently
**2. Locate** — Find where error occurs
**3. Understand** — Figure out why it happens
**4. Fix** — Correct the error
**5. Verify** — Test the fix

### Debug vs Release Mode

**Debug mode (`{$DEBUG}`):**
- Bounds checking: **Always ON**
- Overflow checking: ON by default
- Clear error messages
- Symbol information for debugging
- Slower execution

**Release mode (`{$RELEASE}`):**
- Bounds checking: Can be disabled
- Optimized code generation
- No symbol information
- Faster execution

---

## Setting Breakpoints

### What is a Breakpoint?

**A breakpoint pauses execution at a specific line:**

- **Set breakpoint** — Click line number or use command
- **Program pauses** — Execution stops at breakpoint
- **Inspect state** — View variables, call stack
- **Continue** — Resume execution

### Setting Breakpoints

**In ZealIDE:**

1. **Click line number** — Sets breakpoint (red dot)
2. **Right-click** — Breakpoint options
3. **Conditional breakpoint** — Break only if condition true
4. **Remove breakpoint** — Click again or right-click → Remove

**Example:**
```pascal
program DebugExample;
var
  i: integer;
  sum: integer;
begin
  sum := 0;
  for i := 0 to 9 do  // Set breakpoint here
  begin
    sum := sum + i;
  end;
  WriteLn('Sum: ', sum);
end.
```

**Set breakpoint at `for i := 0 to 9 do`:**
- Program pauses before loop starts
- Can inspect `sum` (should be 0)
- Can step through loop iterations

---

## Stepping Through Code

### Step Commands

**Step Over (F10):**
- Execute current line
- Don't enter function calls
- Move to next line

**Step Into (F11):**
- Execute current line
- Enter function calls
- Step into function body

**Step Out (Shift+F11):**
- Execute rest of current function
- Return to caller
- Continue from call site

**Continue (F5):**
- Resume execution
- Run until next breakpoint
- Or run to end

### Stepping Example

**Program:**
```pascal
procedure AddNumbers(a, b: integer): integer;
begin
  AddNumbers := a + b;  // Step into here
end;

var
  result: integer;
begin
  result := AddNumbers(10, 20);  // Set breakpoint here
  WriteLn('Result: ', result);
end.
```

**Stepping process:**
1. **Set breakpoint** at `result := AddNumbers(10, 20);`
2. **Run** program (pauses at breakpoint)
3. **Step Into** (F11) — Enters `AddNumbers` function
4. **Step Over** (F10) — Executes `AddNumbers := a + b;`
5. **Step Out** (Shift+F11) — Returns to caller
6. **Continue** (F5) — Finishes program

---

## Inspecting Variables

### Variable Watch

**Watch variables during debugging:**

- **Add to watch** — Right-click variable → Add to Watch
- **View value** — See current value
- **Update** — Value updates as program runs
- **Expressions** — Watch expressions (e.g., `i * 2`)

### Variable Window

**View all variables in scope:**

- **Local variables** — Variables in current function
- **Parameters** — Function parameters
- **Global variables** — Module-level variables
- **Registers** — CPU registers (advanced)

**Example:**
```pascal
procedure ProcessArray(arr: array of integer);
var
  i: integer;
  sum: integer;
begin
  sum := 0;
  for i := 0 to High(arr) do  // Breakpoint here
  begin
    sum := sum + arr[i];
  end;
  WriteLn('Sum: ', sum);
end;
```

**At breakpoint, watch window shows:**
- `arr` — Array contents
- `i` — Current index (0)
- `sum` — Current sum (0)

### Evaluating Expressions

**Evaluate expressions in debugger:**

- **Quick watch** — Evaluate expression on the fly
- **Immediate window** — Type expressions to evaluate
- **Hover** — Hover over variable to see value

**Example:**
- Hover over `arr[i]` — See current array element
- Evaluate `sum + arr[i]` — See what will be added
- Evaluate `High(arr)` — See array upper bound

---

## Call Stack

### Understanding Call Stack

**Call stack shows function call chain:**

- **Current function** — Function currently executing
- **Caller** — Function that called current function
- **Caller's caller** — And so on...
- **Stack trace** — Complete call chain

### Viewing Call Stack

**In debugger:**

1. **Call Stack window** — Shows function chain
2. **Click frame** — Jump to that function
3. **View variables** — See variables in that frame
4. **Navigate** — Move up/down call stack

**Example:**
```pascal
procedure Level3;
begin
  WriteLn('Level 3');  // Breakpoint here
end;

procedure Level2;
begin
  Level3;  // Calls Level3
end;

procedure Level1;
begin
  Level2;  // Calls Level2
end;

begin
  Level1;  // Calls Level1
end.
```

**Call stack at breakpoint:**
```
Level3  (current, line 2)
Level2  (called Level3, line 2)
Level1  (called Level2, line 2)
Main    (called Level1, line 7)
```

---

## Practical Debugging Examples

### Array Bounds Error

**Program with error:**
```pascal
program ArrayBounds;
var
  arr: array[0..4] of integer;
  i: integer;
begin
  for i := 0 to 5 do  // ERROR: Array only has 5 elements (0-4)
  begin
    arr[i] := i * 2;
  end;
end.
```

**Debugging steps:**
1. **Set breakpoint** at `for i := 0 to 5 do`
2. **Run** program
3. **Watch** `i` and `arr`
4. **Step through** loop
5. **Notice** when `i = 5`, accessing `arr[5]` is out of bounds
6. **Fix** loop: `for i := 0 to 4 do`

### Null Pointer Error

**Program with error:**
```pascal
program NullPointer;
type
  PEntity = ^TEntity;
  TEntity = record
    X, Y: integer;
  end;

var
  entity: PEntity;
begin
  entity := nil;  // Not initialized
  entity^.X := 100;  // ERROR: Dereferencing nil
end.
```

**Debugging steps:**
1. **Set breakpoint** at `entity := nil;`
2. **Run** program
3. **Watch** `entity` — Shows `nil`
4. **Step** to next line
5. **Notice** `entity` is `nil` before dereference
6. **Fix** — Initialize entity: `New(entity);`

### Logic Error

**Program with logic error:**
```pascal
function CalculateAverage(numbers: array of integer): integer;
var
  i: integer;
  sum: integer;
begin
  sum := 0;
  for i := 0 to High(numbers) do
    sum := sum + numbers[i];
  
  CalculateAverage := sum div High(numbers);  // ERROR: Should be Length
end;
```

**Debugging steps:**
1. **Set breakpoint** at function start
2. **Call** with test data: `[10, 20, 30]`
3. **Watch** `sum` — Should be 60
4. **Watch** `High(numbers)` — Shows 2 (last index)
5. **Calculate** — `60 div 2 = 30` (wrong, should be 20)
6. **Fix** — Use `Length(numbers)` instead

---

## Debugging Best Practices

### 1. Start with Reproducible Cases

**Make errors consistent:**

```pascal
// ✅ GOOD: Reproducible test case
var testData: array[0..4] of integer = (10, 20, 30, 40, 50);
ProcessArray(testData);

// ❌ BAD: Random data
var randomData: array[0..99] of integer;
FillRandom(randomData);  // Hard to debug
```

### 2. Use Breakpoints Strategically

**Set breakpoints at key points:**

```pascal
// ✅ GOOD: Break at function entry and key decisions
procedure ProcessEntity(entity: TEntity);
begin  // Breakpoint here
  if entity.Active then  // Breakpoint here
    UpdateEntity(entity);
end;

// ❌ BAD: Too many breakpoints
// Breakpoint on every line (overwhelming)
```

### 3. Watch Relevant Variables

**Focus on important variables:**

```pascal
// ✅ GOOD: Watch variables that matter
Watch: entity.X, entity.Y, entity.Health

// ❌ BAD: Watch everything
Watch: every single variable (too much information)
```

### 4. Use Conditional Breakpoints

**Break only when condition met:**

```pascal
// ✅ GOOD: Conditional breakpoint
// Break when i > 5
if i > 5 then
  // Breakpoint here

// ❌ BAD: Break every iteration
// Breakpoint in loop (too many pauses)
```

### 5. Document Findings

**Note what you discover:**

```pascal
// ✅ GOOD: Comment findings
// Found: Loop goes to 5, but array ends at 4
// Fix: Change loop to 0 to 4

// ❌ BAD: No notes
// Forget what you found
```

---

## Exercises

### Exercise 1: Basic Debugging

Write a program that:
1. Has a simple bug
2. Use debugger to find it
3. Set breakpoints
4. Step through code
5. Fix the bug

### Exercise 2: Variable Inspection

Write a program that:
1. Uses multiple variables
2. Set breakpoints
3. Watch variables
4. Inspect values
5. Understand program state

### Exercise 3: Call Stack

Write a program that:
1. Has nested function calls
2. Set breakpoint in deep function
3. View call stack
4. Navigate call stack
5. Inspect variables at each level

### Exercise 4: Complex Debugging

Write a program that:
1. Has multiple bugs
2. Use all debugging techniques
3. Find and fix all bugs
4. Verify fixes work
5. Document debugging process

---

**Previous Chapter:** [Chapter 23: Scenes, UI, and Game Architecture](../23_ScenesUIAndGameArchitecture/README.md)  
**Next Section:** [Performance Metrics](./02_PerformanceMetrics.md)  
**Language Specification:** See [Memory and I/O](../../languageSpecification/12_MemoryAndIO.md)  
**Last Updated:** 2025-01-XX

