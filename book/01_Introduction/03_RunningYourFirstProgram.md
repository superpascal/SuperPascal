# Running Your First Program

**Part of:** [Chapter 01: Introduction to Programming on Zeal](./README.md)

---

## Introduction

Now it's time to write and run your first SuperPascal program! This chapter will guide you through:
- Setting up ZealIDE (the development environment)
- Writing your first program
- Building and running it
- Understanding the build process
- Basic debugging

---

## Using ZealIDE

### What is ZealIDE?

**ZealIDE** is the Integrated Development Environment for SuperPascal:
- **Editor** — Write and edit code
- **Compiler** — Build your programs
- **Debugger** — Find and fix errors
- **Project Manager** — Organize your code
- **Offline** — No internet required

### ZealIDE Features

**Code Editor:**
- Syntax highlighting
- Code completion
- Error detection
- Code formatting

**Build System:**
- One-click compilation
- Error reporting
- Object file generation
- Asset bundling

**Debugger:**
- Breakpoints
- Step through code
- Variable inspection
- Call stack viewing

**Project Management:**
- File organization
- Project templates
- Build configurations
- Platform selection

---

## Your First Program: "Hello, World!"

### The Classic First Program

Every programmer's first program displays "Hello, World!":

```pascal
program HelloWorld;
begin
  WriteLn('Hello, World!');
end.
```

### Understanding the Code

**Line 1: `program HelloWorld;`**
- Declares a program named "HelloWorld"
- Program name must be a valid identifier
- Semicolon ends the declaration

**Line 2: `begin`**
- Starts the main program block
- All executable code goes between `begin` and `end`

**Line 3: `WriteLn('Hello, World!');`**
- Calls the `WriteLn` procedure
- `'Hello, World!'` is a string literal
- Outputs text and moves to next line
- Semicolon ends the statement

**Line 4: `end.`**
- Ends the program block
- Period (`.`) ends the program

### Creating the Program

1. **Open ZealIDE**
2. **Create New Project** → "HelloWorld"
3. **Select Platform** → ZealZ80 (or your platform)
4. **Create File** → `HelloWorld.pas`
5. **Type the code** above
6. **Save** the file

---

## Build → Run → Debug

### The Development Cycle

**1. Write Code**
- Type your program in the editor
- Save the file (`.pas` extension)

**2. Build (Compile)**
- Click "Build" or press F7
- Compiler checks for errors
- Generates object file (`.ZOF`)

**3. Run**
- Click "Run" or press F5
- Program executes on target platform
- See output in console/display

**4. Debug (if needed)**
- Set breakpoints
- Step through code
- Inspect variables
- Find and fix errors

### Build Process

**What happens when you build:**

1. **Lexical Analysis** — Breaks code into tokens
2. **Parsing** — Builds syntax tree
3. **Semantic Analysis** — Checks types and rules
4. **Code Generation** — Creates machine code
5. **Linking** — Combines with libraries
6. **Object File** — Creates `.ZOF` file

**If errors occur:**
- Compiler reports errors
- Fix errors in editor
- Rebuild
- Repeat until successful

### Running the Program

**On ZealZ80:**
- Program loads into memory
- Executes on Z80 CPU
- Output appears on screen/console
- Program runs until `end.`

**On Foenix65C816:**
- Program loads into memory
- Executes on WDC65C816S CPU
- Output appears on display
- Program runs until `end.`

**On FoenixA2560M:**
- Program loads into memory
- Executes on MC68LC060 CPU
- Output appears on display
- Program runs until `end.`

---

## Understanding Errors

### Common Errors

**Syntax Errors:**
```pascal
program HelloWorld
begin  // ERROR: Missing semicolon after program name
  WriteLn('Hello, World!')
end.   // ERROR: Missing semicolon after WriteLn
```

**Fix:**
```pascal
program HelloWorld;  // Added semicolon
begin
  WriteLn('Hello, World!');  // Added semicolon
end.
```

**Type Errors:**
```pascal
var x: integer;
begin
  x := 'Hello';  // ERROR: Can't assign string to integer
end.
```

**Fix:**
```pascal
var x: string;  // Changed type to string
begin
  x := 'Hello';  // Now correct
end.
```

**Undefined Errors:**
```pascal
begin
  WriteLn(y);  // ERROR: Variable 'y' not defined
end.
```

**Fix:**
```pascal
var y: string;
begin
  y := 'Hello';
  WriteLn(y);  // Now correct
end.
```

### Reading Error Messages

**Error format:**
```
Error: [file.pas:line:column] Message
```

**Example:**
```
Error: [HelloWorld.pas:3:15] Missing semicolon
```

**Meaning:**
- **File:** `HelloWorld.pas`
- **Line:** 3
- **Column:** 15
- **Message:** Missing semicolon

**How to fix:**
1. Go to the file and line
2. Check the column position
3. Read the error message
4. Fix the problem
5. Rebuild

---

## Expanding Your First Program

### Adding Variables

```pascal
program HelloWorld;
var
  name: string;
begin
  name := 'SuperPascal';
  WriteLn('Hello, ', name, '!');
end.
```

**What's new:**
- **`var` block** — Declares variables
- **`name: string`** — Variable of type string
- **`name := 'SuperPascal'`** — Assignment
- **String concatenation** — Combining strings with `+` or `,`

### Adding Calculations

```pascal
program HelloWorld;
var
  a, b, sum: integer;
begin
  a := 10;
  b := 20;
  sum := a + b;
  WriteLn('Sum of ', a, ' and ', b, ' is ', sum);
end.
```

**Output:**
```
Sum of 10 and 20 is 30
```

### Adding Input (if supported)

```pascal
program HelloWorld;
var
  name: string;
begin
  Write('Enter your name: ');
  ReadLn(name);
  WriteLn('Hello, ', name, '!');
end.
```

**Note:** Input capabilities depend on platform and runtime.

---

## Platform-Specific Notes

### ZealZ80

**Build Output:**
- `.ZOF` file (Zeal Object Format)
- Can be loaded into Zeal computer
- Runs on Z80 CPU

**Running:**
- Load `.ZOF` into Zeal
- Execute from ZealIDE or directly on hardware
- Output to console or screen

### Foenix65C816

**Build Output:**
- Platform-specific object format
- Runs on WDC65C816S CPU
- Uses 16MB address space

**Running:**
- Load into Foenix system
- Execute on target hardware
- Output to display

### FoenixA2560M

**Build Output:**
- Platform-specific object format
- Runs on MC68LC060 CPU
- Uses 32-bit architecture

**Running:**
- Load into Foenix system
- Execute on target hardware
- Output to display

---

## Debugging Basics

### What is Debugging?

**Debugging** is finding and fixing errors in your code.

### Types of Errors

**Compile-time errors:**
- Syntax errors
- Type errors
- Undefined variables
- **Caught by compiler** — Won't run until fixed

**Runtime errors:**
- Division by zero
- Array out of bounds
- Null pointer access
- **Caught at runtime** — Program crashes

**Logic errors:**
- Wrong algorithm
- Incorrect calculations
- **Program runs but produces wrong results**

### Using the Debugger

**Set Breakpoints:**
- Click line number to set breakpoint
- Program pauses at breakpoint
- Inspect variables and state

**Step Through Code:**
- **Step Over** — Execute line, don't enter functions
- **Step Into** — Enter function calls
- **Step Out** — Exit current function
- **Continue** — Run until next breakpoint

**Inspect Variables:**
- View variable values
- Watch expressions
- Check call stack
- See memory contents

### Example Debugging Session

**Program with error:**
```pascal
program DebugExample;
var
  numbers: array[0..4] of integer;
  i, sum: integer;
begin
  sum := 0;
  for i := 0 to 5 do  // ERROR: Array only has 5 elements (0-4)
    sum := sum + numbers[i];
  WriteLn('Sum: ', sum);
end.
```

**Debugging steps:**
1. **Set breakpoint** at `for i := 0 to 5 do`
2. **Run** program
3. **Inspect** `i` and `numbers` array
4. **Notice** loop goes to 5, but array ends at 4
5. **Fix** loop: `for i := 0 to 4 do`
6. **Rebuild** and test

---

## Project Structure

### Basic Project

```
HelloWorld/
├── HelloWorld.pas    # Main program file
├── HelloWorld.ZOF    # Compiled object file (generated)
└── project.sp        # Project configuration (if used)
```

### Multi-File Project

```
MyGame/
├── MyGame.pas        # Main program
├── Graphics.pas      # Graphics unit
├── Audio.pas         # Audio unit
├── Game.pas          # Game logic unit
└── assets/           # Game assets
    ├── sprites/
    ├── tiles/
    └── sounds/
```

---

## Best Practices

### Code Organization

**Use meaningful names:**
```pascal
// Bad
var x, y, z: integer;

// Good
var playerX, playerY, playerScore: integer;
```

**Add comments:**
```pascal
// Calculate player's new position
playerX := playerX + velocityX;
playerY := playerY + velocityY;
```

**Format consistently:**
```pascal
// Consistent indentation
if condition then
begin
  DoSomething;
  DoSomethingElse;
end;
```

### Error Prevention

**Check bounds:**
```pascal
if (index >= 0) and (index < arraySize) then
  value := array[index]
else
  WriteLn('Error: Index out of bounds');
```

**Validate input:**
```pascal
if (value > 0) and (value < 100) then
  ProcessValue(value)
else
  WriteLn('Error: Invalid value');
```

---

## Exercises

### Exercise 1: Modify Hello World

**Task:** Change the program to display your name.

**Starting code:**
```pascal
program HelloWorld;
begin
  WriteLn('Hello, World!');
end.
```

**Goal:** Output "Hello, [Your Name]!"

### Exercise 2: Simple Calculator

**Task:** Write a program that adds two numbers.

**Requirements:**
- Declare two integer variables
- Assign values to them
- Calculate their sum
- Display the result

### Exercise 3: Loop Practice

**Task:** Write a program that counts from 1 to 10.

**Requirements:**
- Use a `for` loop
- Display each number
- Output should be: "1 2 3 4 5 6 7 8 9 10"

---

## Summary

**Key Concepts:**
- **ZealIDE** is the development environment
- **Build** compiles your code
- **Run** executes your program
- **Debug** helps find errors
- **Errors** are part of learning

**Development Cycle:**
1. Write code
2. Build (compile)
3. Run
4. Debug (if needed)
5. Repeat

**Next:** Learn about variables, types, and expressions in detail.

---

**Next Chapter:** [Chapter 02: Variables, Types, and Expressions](../03_VariablesTypesExpressions/README.md)  
**Language Specification:** See [01_LexicalStructure.md](../../languageSpecification/01_LexicalStructure.md)  
**Last Updated:** 2025-01-XX

