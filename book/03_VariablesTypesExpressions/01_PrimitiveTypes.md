# Primitive Types

**Part of:** [Chapter 02: Variables, Types, and Expressions](./README.md)

---

## Introduction

Every piece of data in SuperPascal has a **type**. Types tell the compiler:
- **What kind of data** it is (number, text, true/false)
- **How much memory** it uses
- **What operations** can be performed on it
- **How it's stored** in memory

Understanding types is fundamental to programming. This chapter introduces SuperPascal's **primitive types** — the basic building blocks of all data.

> **Connection to Chapter 1:** Remember that memory is like numbered boxes? Different types need different-sized boxes. An integer might need 2 boxes, while a string might need many boxes. The CPU and ALU work with these values based on their types. *Technical details about how types are represented in memory are covered in Part IV: Computers in Depth.*

### Why Different Types Exist

**Different types exist because:**
- **Different purposes** — Numbers for math, text for messages, booleans for decisions
- **Different sizes** — Some values need more memory than others
- **Different operations** — You can add numbers, but not strings (directly)
- **Efficiency** — Using the right type saves memory and makes code faster

**Example:**
```pascal
var age: integer;        // Whole numbers for age
var name: string;        // Text for names
var isStudent: boolean;  // True/false for yes/no questions
```

Each type is stored differently in memory and can do different things.

---

## What is a Type?

A **type** defines:
- **The set of values** a variable can hold
- **The operations** that can be performed
- **The memory representation** (how it's stored)

**Example:**
```pascal
var x: integer;  // x can hold integer values: ..., -2, -1, 0, 1, 2, ...
```

**Why types matter:**
- **Type safety** — Prevents errors (can't add a string to a number)
- **Memory efficiency** — Compiler knows how much memory to allocate
- **Clarity** — Code clearly shows what data is expected
- **Optimization** — Compiler can optimize based on types

---

## Integer Types

### `integer`

**Purpose:** Whole numbers (positive, negative, or zero)

**Range:** Platform-dependent
- **ZealZ80:** -32768 to 32767 (16-bit signed)
- **Foenix65C816:** -32768 to 32767 (16-bit signed)
- **FoenixA2560M:** -2147483648 to 2147483647 (32-bit signed)

**Memory:** 2 bytes (16-bit) or 4 bytes (32-bit), depending on platform

**Operations:**
- Arithmetic: `+`, `-`, `*`, `div`, `mod`
- Comparison: `=`, `<>`, `<`, `>`, `<=`, `>=`
- Assignment: `:=`

**Example:**
```pascal
var age: integer;
begin
  age := 25;
  age := age + 1;  // age is now 26
  WriteLn('Age: ', age);
end.
```

### `byte`

**Purpose:** Small positive whole numbers (0 to 255)

**Range:** 0 to 255 (8-bit unsigned)

**Memory:** 1 byte

**Use cases:**
- **Memory efficiency** — When you know values fit in 0-255
- **Array indices** — Small array indexing
- **Color values** — RGB components (0-255)
- **Character codes** — ASCII/character values

**Operations:** Same as `integer` (but no negative values)

**Example:**
```pascal
var red, green, blue: byte;
begin
  red := 255;
  green := 128;
  blue := 0;
  // RGB color: (255, 128, 0) - orange
end.
```

### Integer Literals

**Decimal:**
```pascal
var x: integer;
x := 42;      // Decimal
x := -10;     // Negative
x := 0;       // Zero
```

**Hexadecimal:**
```pascal
x := $FF;     // 255 in decimal
x := $A5;     // 165 in decimal
x := $0000;   // 0 in decimal
```

**Binary (if supported):**
```pascal
x := %1010;   // 10 in decimal (if binary literals supported)
```

---

## Boolean Type

### `boolean`

**Purpose:** True or false values

**Values:** `true` or `false`

**Memory:** 1 byte (typically)

**Use cases:**
- **Conditions** — `if` statements, loops
- **Flags** — On/off, enabled/disabled
- **State** — Is something done? Is it valid?

**Operations:**
- **Logical:** `and`, `or`, `not`, `xor`
- **Comparison:** `=`, `<>`
- **Assignment:** `:=`

**Example:**
```pascal
var isReady: boolean;
var hasError: boolean;
begin
  isReady := true;
  hasError := false;
  
  if isReady and not hasError then
    WriteLn('System ready');
end.
```

### Boolean Expressions

**Comparison operators return boolean:**
```pascal
var result: boolean;
begin
  result := 5 > 3;      // true
  result := 10 = 10;     // true
  result := 7 <> 7;     // false
  result := 2 < 1;       // false
end.
```

**Logical operators:**
```pascal
var a, b: boolean;
begin
  a := true;
  b := false;
  
  WriteLn(a and b);   // false (both must be true)
  WriteLn(a or b);    // true (either can be true)
  WriteLn(not a);      // false (opposite)
  WriteLn(a xor b);   // true (exclusive or)
end.
```

---

## Character Type

### `char`

**Purpose:** Single character

**Range:** ASCII characters (0-255, typically printable 32-126)

**Memory:** 1 byte

**Use cases:**
- **Single characters** — Letters, digits, symbols
- **Character codes** — ASCII values
- **String building** — Building strings character by character

**Operations:**
- **Comparison:** `=`, `<>`, `<`, `>`, `<=`, `>=` (by ASCII value)
- **Assignment:** `:=`
- **Conversion:** Can convert to/from `byte` (ASCII code)

**Example:**
```pascal
var letter: char;
var digit: char;
begin
  letter := 'A';
  digit := '5';
  
  WriteLn('Letter: ', letter);
  WriteLn('Digit: ', digit);
  
  // Character comparison
  if letter < 'Z' then
    WriteLn('Letter is before Z');
end.
```

### Character Literals

**Single quotes:**
```pascal
var ch: char;
ch := 'A';    // Letter A
ch := '5';    // Digit 5
ch := ' ';    // Space
ch := '!';    // Exclamation mark
```

**Special characters (if supported):**
```pascal
ch := #10;    // Newline (line feed)
ch := #13;    // Carriage return
ch := #9;     // Tab
ch := #0;     // Null character
```

---

## String Type

### `string`

**Purpose:** Text (sequence of characters)

**Length:** Up to 255 characters (short string)

**Memory:** 1 byte (length) + N bytes (characters)

**Use cases:**
- **Text output** — Messages, labels
- **User input** — Names, descriptions
- **Data representation** — File names, identifiers

**Operations:**
- **Concatenation:** `+` or `,` in WriteLn
- **Comparison:** `=`, `<>`, `<`, `>`, `<=`, `>=` (lexicographic)
- **Length:** `Length(str)` function
- **Indexing:** `str[index]` (1-based)

**Example:**
```pascal
var name: string;
var greeting: string;
begin
  name := 'SuperPascal';
  greeting := 'Hello, ' + name + '!';
  WriteLn(greeting);
  
  // String comparison
  if name = 'SuperPascal' then
    WriteLn('Name matches');
end.
```

### String Literals

**Single quotes:**
```pascal
var message: string;
message := 'Hello, World!';
message := 'It''s a test';  // Escape single quote with ''
```

**String operations:**
```pascal
var first, last, full: string;
begin
  first := 'John';
  last := 'Doe';
  full := first + ' ' + last;  // 'John Doe'
  WriteLn('Full name: ', full);
  WriteLn('Length: ', Length(full));  // 8
end.
```

**Note:** SuperPascal uses a **unified short string type**. All strings are short strings (up to 255 characters), which is efficient for retro platforms.

---

## Fixed-Point Types

### `Q8.8` (Fixed-Point 8.8)

**Purpose:** Fast decimal numbers for games (8-bit integer part, 8-bit fraction part)

**Range:** -128.0 to 127.99609375 (approximately)

**Memory:** 2 bytes (16-bit)

**Precision:** 1/256 (0.00390625) per unit

**Use cases:**
- **Game coordinates** — X, Y positions
- **Velocities** — Movement speeds
- **Scaling** — Sprite scaling factors
- **Fast math** — When floating-point is too slow

**Operations:**
- **Arithmetic:** `+`, `-`, `*`, `/` (fixed-point arithmetic)
- **Comparison:** `=`, `<>`, `<`, `>`, `<=`, `>=`
- **Conversion:** To/from `integer`

**Example:**
```pascal
var x, y: Q8.8;
var speed: Q8.8;
begin
  x := 10.5;      // 10.5 in fixed-point
  y := 20.25;    // 20.25 in fixed-point
  speed := 1.5;  // 1.5 units per frame
  
  x := x + speed;  // Move x by speed
  WriteLn('Position: ', x, ', ', y);
end.
```

### `Q12.12` (Fixed-Point 12.12)

**Purpose:** Higher precision fixed-point numbers (12-bit integer, 12-bit fraction)

**Range:** -2048.0 to 2047.999755859375 (approximately)

**Memory:** 3 bytes (24-bit, typically stored as 32-bit aligned)

**Precision:** 1/4096 (0.000244140625) per unit

**Use cases:**
- **Precise coordinates** — When Q8.8 isn't precise enough
- **Physics calculations** — More accurate math
- **Camera positions** — Smooth camera movement
- **Advanced math** — Trigonometry, interpolation

**Operations:** Same as Q8.8 (but higher precision)

**Example:**
```pascal
var cameraX: Q12.12;
begin
  cameraX := 100.5;        // More precise than Q8.8
  cameraX := cameraX + 0.1; // Smooth movement
end.
```

### Why Fixed-Point?

**Advantages:**
- **Fast** — Integer arithmetic (no floating-point overhead)
- **Deterministic** — Same results on all platforms
- **Hardware-friendly** — Can be FPGA-accelerated (on ZealZ80)
- **Game-optimized** — Perfect for game coordinates and velocities

**When to use:**
- **Q8.8** — Game coordinates, velocities, simple math
- **Q12.12** — Precise calculations, physics, camera
- **integer** — Whole numbers, counts, indices
- **Float32** (if available) — Scientific computing, when precision is critical

---

## Type Summary Table

| Type | Range | Memory | Use Case |
|------|-------|--------|----------|
| `integer` | -32768 to 32767 (16-bit) or -2147483648 to 2147483647 (32-bit) | 2-4 bytes | Whole numbers |
| `byte` | 0 to 255 | 1 byte | Small positive numbers, colors, indices |
| `boolean` | `true`, `false` | 1 byte | Conditions, flags |
| `char` | ASCII (0-255) | 1 byte | Single characters |
| `string` | Up to 255 characters | 1 + N bytes | Text |
| `Q8.8` | -128.0 to ~127.996 | 2 bytes | Game coordinates, velocities |
| `Q12.12` | -2048.0 to ~2047.999 | 3-4 bytes | Precise calculations, physics |

---

## Platform-Specific Considerations

### ZealZ80 (8-bit)

**Integer:** 16-bit signed (-32768 to 32767)
**Byte:** 8-bit unsigned (0 to 255)
**Memory:** Limited (512KB), choose types carefully

**Recommendations:**
- Use `byte` when values fit (saves memory)
- Use `Q8.8` for game math (fast, hardware-accelerated)
- Use `Q12.12` sparingly (takes more memory)

### Foenix65C816 (16-bit)

**Integer:** 16-bit signed (-32768 to 32767)
**Byte:** 8-bit unsigned (0 to 255)
**Memory:** More available (512KB-2MB)

**Recommendations:**
- Standard types work well
- `Q8.8` and `Q12.12` both supported
- More memory allows more flexibility

### FoenixA2560M (32-bit)

**Integer:** 32-bit signed (-2147483648 to 2147483647)
**Byte:** 8-bit unsigned (0 to 255)
**Memory:** Large (1GB DDR3)

**Recommendations:**
- Full 32-bit integer range
- All types supported
- Can use larger data structures

---

## Type Safety

### What is Type Safety?

**Type safety** means the compiler checks that:
- Variables are used with correct types
- Operations are valid for the types
- Conversions are explicit and safe

**Benefits:**
- **Catches errors early** — At compile time, not runtime
- **Prevents bugs** — Can't accidentally mix types
- **Clear code** — Types document what data is expected

### Type Errors

**The compiler will catch:**
```pascal
var x: integer;
var name: string;
begin
  x := 'hello';     // ERROR: Can't assign string to integer
  name := 42;       // ERROR: Can't assign integer to string
  x := name + 5;    // ERROR: Can't add string and integer
end.
```

**Fix by using correct types:**
```pascal
var x: integer;
var name: string;
begin
  x := 42;          // OK: integer to integer
  name := 'hello';  // OK: string to string
  x := x + 5;       // OK: integer arithmetic
end.
```

---

## Summary

**Key Concepts:**
- **Types** define what data can be stored and what operations are allowed
- **Primitive types** are the basic building blocks
- **Type safety** prevents errors at compile time
- **Fixed-point types** provide fast, deterministic math for games

**Primitive Types:**
- `integer` — Whole numbers
- `byte` — Small positive numbers (0-255)
- `boolean` — True/false values
- `char` — Single characters
- `string` — Text (up to 255 characters)
- `Q8.8` — Fast fixed-point (game coordinates)
- `Q12.12` — Precise fixed-point (advanced math)

**Next:** Learn how to declare and use variables.

---

**Next Section:** [Variables and Assignment](./02_VariablesAndAssignment.md)  
**Language Specification:** See [03_TypeSystem.md](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

