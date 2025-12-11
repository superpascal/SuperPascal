# SuperPascal Language Specification — Standard Library

## Standard Library Functions and Constants

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

---

## Overview

The SuperPascal standard library provides essential functions and constants for common operations. This document specifies the standard library API.

**Note:** Platform-specific standard libraries may provide additional functions. See [Platform Specifications](../platforms/README.md) for platform-specific extensions.

---

## String Functions

### Conversion Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `IntToStr` | `IntToStr(x: integer): string` | Convert integer to string |
| `StrToInt` | `StrToInt(s: string): integer` | Convert string to integer |
| `FloatToStr` | `FloatToStr(x: Q8.8): string` | Convert fixed-point to string |
| `StrToFloat` | `StrToFloat(s: string): Q8.8` | Convert string to fixed-point |

### String Manipulation

| Function | Signature | Description |
|----------|-----------|-------------|
| `Length` | `Length(s: string): integer` | Get string length |
| `Copy` | `Copy(s: string; start, count: integer): string` | Extract substring |
| `Pos` | `Pos(substr, s: string): integer` | Find substring position (0 if not found) |
| `Delete` | `Delete(var s: string; start, count: integer)` | Remove characters |
| `Insert` | `Insert(substr: string; var s: string; pos: integer)` | Insert substring |
| `Concat` | `Concat(s1, s2: string): string` | Concatenate strings (same as `+`) |

### Regular Expressions

SuperPascal provides built-in regular expression support for pattern matching and text manipulation.

#### Pattern Matching

| Function | Signature | Description |
|----------|-----------|-------------|
| `Match` | `Match(pattern, text: string): boolean` | Check if text matches pattern |
| `Find` | `Find(pattern, text: string): integer` | Find first match position (0 if not found) |
| `FindAll` | `FindAll(pattern, text: string; var matches: array of string; var count: integer)` | Find all matches |
| `MatchGroups` | `MatchGroups(pattern, text: string; var groups: array of string; var count: integer): boolean` | Match with captured groups |

#### Text Replacement

| Function | Signature | Description |
|----------|-----------|-------------|
| `Replace` | `Replace(pattern, text, replacement: string): string` | Replace all matches |
| `ReplaceFirst` | `ReplaceFirst(pattern, text, replacement: string): string` | Replace first match only |

#### Text Splitting

| Function | Signature | Description |
|----------|-----------|-------------|
| `Split` | `Split(pattern, text: string; var parts: array of string; var count: integer)` | Split text by pattern |

**Regex Syntax:**
- **Character classes:** `[a-z]`, `[0-9]`, `[^abc]`
- **Predefined classes:** `\d` (digit), `\w` (word), `\s` (whitespace)
- **Quantifiers:** `*` (zero or more), `+` (one or more), `?` (optional), `{n}` (exactly n), `{n,m}` (between n and m)
- **Anchors:** `^` (start), `$` (end), `\b` (word boundary)
- **Alternation:** `cat|dog` (cat or dog)
- **Groups:** `(pattern)` (capture group)
- **Escaping:** `\.` (literal dot), `\\` (literal backslash)

**Examples:**
```pascal
// Match digits
Match('[0-9]+', '123');  // true

// Find email pattern
Find('[a-zA-Z0-9._]+@[a-zA-Z0-9.]+', 'user@example.com');  // > 0

// Replace all digits
Replace('[0-9]+', 'Score: 100', 'XXX');  // 'Score: XXX'

// Split by whitespace
Split('\s+', 'hello world', parts, count);  // parts = ['hello', 'world']
```

### Membership Testing (`in` Operator)

SuperPascal's `in` operator supports Python-like membership testing across multiple types:

| Type | Syntax | Description | Performance |
|------|--------|-------------|-------------|
| Set | `value in set` | Check if value is in set | O(1) constant |
| Array | `value in array[:count]` | Check if value is in array | O(n) linear |
| List | `value in list` | Check if value is in list | O(n) linear |
| String | `substring in string` | Check if substring is in string | O(n) linear |
| Tuple | `value in tuple` | Check if value is in tuple | O(n) linear |

**Examples:**
```pascal
// Set membership (fastest)
var s: set of 0..31;
s := [1, 2, 3];
if 2 in s then WriteLn('Found');

// Array membership
var arr: array[0..9] of integer;
var count: integer;
arr[0] := 10; arr[1] := 20; count := 2;
if 20 in arr[:count] then WriteLn('Found');

// List membership
var myList: list;
myList := [10, 'Hello', true];
if 10 in myList then WriteLn('Found');

// String membership (substring search)
var text: string;
text := 'Hello, World!';
if 'World' in text then WriteLn('Found');

// Tuple membership
var point: (integer, integer);
point := (10, 20);
if 10 in point then WriteLn('Found');
```

### Find Functions (Index/Position)

SuperPascal provides `Find` functions that return the index/position of elements (similar to Python's `index()` but returns -1/0 if not found instead of raising):

| Function | Signature | Description | Returns |
|----------|-----------|-------------|---------|
| `Find` (Array) | `Find(const arr: array of T; count: integer; value: T): integer` | Find index in array | -1 if not found |
| `Find` (List) | `Find(const myList: list; value: T): integer` | Find index in list | -1 if not found |
| `Find` (String) | `Find(substr, str: string): integer` | Find substring position | 0 if not found (1-based) |
| `Find` (Tuple) | `Find(const tuple: (T, ...); value: T): integer` | Find index in tuple | -1 if not found |

**Note:** For arrays, `Find` is an alias for `IndexOf`. For strings, `Find` is an alias for `Pos`.

**Examples:**
```pascal
// Array: Find index
var arr: array[0..9] of integer;
var count, index: integer;
arr[0] := 10; arr[1] := 20; arr[2] := 30; count := 3;
index := Find(arr, count, 20);  // index = 1
index := Find(arr, count, 99);  // index = -1 (not found)

// List: Find index
var myList: list;
var index: integer;
myList := [10, 'Hello', true];
index := Find(myList, 10);      // index = 0
index := Find(myList, 'Hello');  // index = 1
index := Find(myList, 99);       // index = -1 (not found)

// String: Find position (1-based)
var text: string;
var pos: integer;
text := 'Hello, World!';
pos := Find('World', text);  // pos = 8
pos := Find('X', text);      // pos = 0 (not found)

// Tuple: Find index
var point: (integer, integer);
var index: integer;
point := (10, 20);
index := Find(point, 10);  // index = 0
index := Find(point, 20);  // index = 1
index := Find(point, 30);  // index = -1 (not found)
```

**Comparison: `in` vs `Find`:**
- `in` operator: Returns boolean (membership test)
- `Find` function: Returns index/position (location)

**When to use:**
- Use `in` when you only need to know if value exists
- Use `Find` when you need the index/position

### Set Operations

SuperPascal supports both Pascal and Python-style set operations:

| Operation | Pascal | Python | Description |
|-----------|--------|--------|-------------|
| Union | `s1 + s2` | `s1 \| s2` | Elements in either set |
| Intersection | `s1 * s2` | `s1 & s2` | Elements in both sets |
| Difference | `s1 - s2` | `s1 - s2` | Elements in s1 but not s2 |
| Symmetric Difference | `s1 ^ s2` | `s1 ^ s2` | Elements in either set, but not both |

**Set Methods (Python-like):**

| Function | Signature | Description |
|----------|-----------|-------------|
| `Add` | `Add(var s: set of T; value: T)` | Add element to set |
| `Remove` | `Remove(var s: set of T; value: T)` | Remove element (raises error if not found) |
| `Discard` | `Discard(var s: set of T; value: T)` | Remove element (no error if not found) |
| `Clear` | `Clear(var s: set of T)` | Remove all elements |
| `Copy` | `Copy(s: set of T): set of T` | Create copy of set |
| `IsSubset` | `IsSubset(s1, s2: set of T): boolean` | Check if s1 ⊆ s2 |
| `IsSuperset` | `IsSuperset(s1, s2: set of T): boolean` | Check if s1 ⊇ s2 |
| `IsDisjoint` | `IsDisjoint(s1, s2: set of T): boolean` | Check if sets have no common elements |

**Examples:**
```pascal
var s1, s2, s3: set of 0..31;
begin
  s1 := [1, 2, 3];
  s2 := [3, 4, 5];
  
  // Union
  s3 := s1 + s2;  // [1, 2, 3, 4, 5] (Pascal)
  s3 := s1 | s2;  // [1, 2, 3, 4, 5] (Python)
  
  // Intersection
  s3 := s1 * s2;  // [3] (Pascal)
  s3 := s1 & s2;  // [3] (Python)
  
  // Symmetric difference
  s3 := s1 ^ s2;  // [1, 2, 4, 5]
  
  // Methods
  Add(s1, 6);           // s1 = [1, 2, 3, 6]
  Remove(s1, 2);       // s1 = [1, 3, 6]
  Discard(s1, 5);      // s1 = [1, 3, 6] (no error)
  
  if IsSubset([1, 3], s1) then
    WriteLn('Subset');
end.
```

---

## Type Conversion Functions

### Numeric Conversions

| Function | Signature | Description |
|----------|-----------|-------------|
| `IntToFloat` | `IntToFloat(x: integer): Q8.8` | Convert integer to fixed-point |
| `FloatToInt` | `FloatToInt(x: Q8.8): integer` | Convert fixed-point to integer (truncates) |
| `Ord` | `Ord(x): integer` | Get ordinal value (enum, char) |
| `Chr` | `Chr(x: integer): char` | Get character from code |

**Note:** `IntToFloat` and `FloatToInt` are aliases for type casts `Q8.8(x)` and `integer(x)` respectively.

---

## Math Functions

### Rounding Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `Round` | `Round(x: Q8.8): integer` | Round to nearest integer |
| `Ceil` | `Ceil(x: Q8.8): integer` | Round up (ceiling) |
| `Floor` | `Floor(x: Q8.8): integer` | Round down (floor) |
| `Trunc` | `Trunc(x: Q8.8): integer` | Truncate (remove fractional part) |

**Behavior:**
- `Round` — Rounds to nearest integer (banker's rounding: 0.5 rounds to even)
- `Ceil` — Always rounds up (toward positive infinity)
- `Floor` — Always rounds down (toward negative infinity)
- `Trunc` — Removes fractional part (toward zero)

### Basic Math Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `Abs` | `Abs(x: integer): integer`<br>`Abs(x: Q8.8): Q8.8` | Absolute value |
| `Sqrt` | `Sqrt(x: Q8.8): Q8.8` | Square root |
| `Pow` | `Pow(base, exponent: Q8.8): Q8.8` | Raise to power (base^exponent) |
| `Exp` | `Exp(x: Q8.8): Q8.8` | Exponential function (e^x) |
| `Log` | `Log(x: Q8.8): Q8.8` | Natural logarithm (ln) |
| `Log10` | `Log10(x: Q8.8): Q8.8` | Base-10 logarithm |

**Precision:**
- Functions use fixed-point arithmetic (Q8.8 or Q12.12)
- Precision depends on fixed-point format
- Platform-specific implementations may use hardware acceleration

### Trigonometric Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `Sin` | `Sin(angle: Q8.8): Q8.8` | Sine (angle in radians) |
| `Cos` | `Cos(angle: Q8.8): Q8.8` | Cosine (angle in radians) |
| `Tan` | `Tan(angle: Q8.8): Q8.8` | Tangent (angle in radians) |
| `ArcSin` | `ArcSin(x: Q8.8): Q8.8` | Arcsine (result in radians) |
| `ArcCos` | `ArcCos(x: Q8.8): Q8.8` | Arccosine (result in radians) |
| `ArcTan` | `ArcTan(x: Q8.8): Q8.8` | Arctangent (result in radians) |
| `ArcTan2` | `ArcTan2(y, x: Q8.8): Q8.8` | Arctangent of y/x (result in radians) |

**Note:** Trigonometric functions use radians. Use `PI` constant for degree conversion.

---

## Math Constants

### Standard Constants

| Constant | Type | Value | Description |
|----------|------|-------|-------------|
| `PI` | `Q8.8` or `Q12.12` | 3.141592653589793 | Pi (π) |
| `E` | `Q8.8` or `Q12.12` | 2.718281828459045 | Euler's number (e) |
| `TAU` | `Q8.8` or `Q12.12` | 6.283185307179586 | Tau (τ = 2π) |

**Usage:**
```pascal
const
  PI = 3.141592653589793;  // Q8.8 or Q12.12 representation
  E = 2.718281828459045;
  TAU = 6.283185307179586;
```

**Note:** Constants are available in both Q8.8 and Q12.12 formats depending on context.

---

## Array Manipulation Functions

### Stack Operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `Push` | `Push(var arr: array of T; var count: integer; value: T)` | Add element to end |
| `Pop` | `Pop(var arr: array of T; var count: integer): T` | Remove and return from end |
| `RPOP` | `RPOP(var arr: array of T; var count: integer): T` | Remove from end (alias for Pop) |
| `LPOP` | `LPOP(var arr: array of T; var count: integer): T` | Remove and return from beginning |

**Performance:**
- `Push`, `Pop`, `RPOP` — O(1) constant time
- `LPOP` — O(n) linear time (shifts all elements)

### Insert and Delete

| Function | Signature | Description |
|----------|-----------|-------------|
| `InsertAt` | `InsertAt(var arr: array of T; var count: integer; index: integer; value: T)` | Insert element at index |
| `DeleteAt` | `DeleteAt(var arr: array of T; var count: integer; index: integer)` | Delete element at index |
| `RemoveValue` | `RemoveValue(var arr: array of T; var count: integer; value: T): boolean` | Remove first occurrence of value |

**Performance:**
- `InsertAt`, `DeleteAt` — O(n) linear time (shifts elements)
- `RemoveValue` — O(n) linear time (searches then deletes)

### Search Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `IndexOf` | `IndexOf(const arr: array of T; count: integer; value: T): integer` | Find index of first occurrence (-1 if not found) |
| `LastIndexOf` | `LastIndexOf(const arr: array of T; count: integer; value: T): integer` | Find index of last occurrence (-1 if not found) |
| `Contains` | `Contains(const arr: array of T; count: integer; value: T): boolean` | Check if array contains value |
| `Count` | `Count(const arr: array of T; count: integer; value: T): integer` | Count occurrences of value |

**Performance:**
- All search functions — O(n) linear time (sequential search)

### Sorting and Reversing

| Function | Signature | Description |
|----------|-----------|-------------|
| `Sort` | `Sort(var arr: array of T; count: integer)` | Sort array in ascending order |
| `SortDescending` | `SortDescending(var arr: array of T; count: integer)` | Sort array in descending order |
| `Reverse` | `Reverse(var arr: array of T; count: integer)` | Reverse array order |

**Performance:**
- `Sort`, `SortDescending` — O(n²) quadratic time (bubble sort implementation)
- `Reverse` — O(n) linear time

### Array Slicing

**SuperPascal supports Python-style array slicing syntax:**

| Syntax | Description | Example |
|--------|-------------|---------|
| `arr[start:end]` | Slice from start to end (end exclusive) | `arr[1:5]` → elements 1-4 |
| `arr[start:end:step]` | Slice with step | `arr[0:10:2]` → every 2nd element |
| `arr[:end]` | From beginning to end | `arr[:5]` → first 5 elements |
| `arr[start:]` | From start to end | `arr[3:]` → from index 3 to end |
| `arr[:]` | Copy entire array | `arr[:]` → all elements |
| `arr[-n:]` | Last n elements | `arr[-3:]` → last 3 elements |
| `arr[:-n]` | All but last n | `arr[:-2]` → all but last 2 |
| `arr[::-1]` | Reverse array | `arr[::-1]` → reversed |

**Slice assignment:**
```pascal
arr[2:5] := [20, 30, 40];  // Replace slice with new values
```

**Performance:**
- Slicing — O(n) linear time (where n is slice length)
- Creates new array — Slicing allocates new array
- Compiler optimizes when possible

**Note:** Slice syntax is a SuperPascal extension. Standard Pascal requires using `Slice()` function.

### Array Concatenation

| Function/Syntax | Signature | Description |
|-----------------|-----------|-------------|
| `Concat` | `Concat(const arr1: array of T; count1: integer; const arr2: array of T; count2: integer; var result: array of T; var resultCount: integer)` | Concatenate two arrays |
| `+` operator | `arr1 + arr2` | Concatenate using `+` operator (SuperPascal extension) |

**Examples:**
```pascal
var arr1, arr2, result: array[0..9] of integer;
var count1, count2, resultCount: integer;
begin
  // Function-based
  Concat(arr1, count1, arr2, count2, result, resultCount);
  
  // Operator-based (SuperPascal extension)
  result := arr1[:count1] + arr2[:count2];
end.
```

**Performance:**
- `Concat` — O(n + m) linear time (where n, m are array sizes)
- Creates new array with combined elements

### Array Growth and Resizing

| Function | Signature | Description |
|----------|-----------|-------------|
| `Grow` | `Grow(const src: array of T; srcCount: integer; var dest: array of T; var destCount: integer)` | Copy to larger array (growth) |
| `Resize` | `Resize(const src: array of T; srcCount: integer; var dest: array of T; var destCount: integer; newSize: integer)` | Resize array (grow or shrink) |

**Memory Considerations:**
- **Fixed-size arrays** — Cannot resize in place
- **Growth requires copying** — All elements copied to new (larger) array
- **Memory allocation** — New array must be allocated (stack or heap)
- **Performance** — O(n) linear time (where n is source array size)

**Example:**
```pascal
var small: array[0..4] of integer;
var large: array[0..9] of integer;
var smallCount, largeCount: integer;
begin
  smallCount := 5;
  // Fill small array...
  
  // Grow to larger array (copies all elements)
  Grow(small, smallCount, large, largeCount);
  // large now contains all elements from small
end.
```

**Growth Strategy:**
- **Double size** — Common pattern (amortized O(1) per insertion)
- **Fixed increment** — Add fixed amount each time
- **Custom strategy** — Based on application needs

### Slicing and Copying (Function-based)

| Function | Signature | Description |
|----------|-----------|-------------|
| `Slice` | `Slice(const src: array of T; srcCount: integer; start, length: integer; var dest: array of T; var destCount: integer)` | Extract subarray (alternative to slice syntax) |
| `CopyArray` | `CopyArray(const src: array of T; srcCount: integer; var dest: array of T; var destCount: integer)` | Copy entire array (alternative to `arr[:]`) |

**Performance:**
- `Slice`, `CopyArray` — O(n) linear time (where n is length)

### Array Length Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `Length` | `Length(const arr: array of T): integer` | Get number of elements in array |
| `High` | `High(const arr: array of T): integer` | Get upper bound (highest valid index) |
| `Low` | `Low(const arr: array of T): integer` | Get lower bound (lowest valid index) |

**Relationship:**
- `Length(arr) = High(arr) - Low(arr) + 1`

**Examples:**
```pascal
var arr: array[0..9] of integer;
var len, upper, lower: integer;
begin
  len := Length(arr);   // 10
  upper := High(arr);   // 9
  lower := Low(arr);    // 0
end.
```

**For multi-dimensional arrays:**
```pascal
var matrix: array[0..3, 0..4] of integer;
var rows, cols: integer;
begin
  rows := Length(matrix);        // First dimension: 4
  cols := Length(matrix[0]);     // Second dimension: 5
end.
```

**Performance:**
- All length functions — O(1) constant time (compile-time or very fast runtime)

**Note:** These functions work with fixed-size arrays. For dynamic arrays (tracked with `count` variable), use the `count` variable directly, not `Length()`.

### Utility Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `Fill` | `Fill(var arr: array of T; count: integer; value: T)` | Set all elements to value |
| `Clear` | `Clear(var count: integer)` | Reset count to 0 (doesn't modify array) |
| `IsEmpty` | `IsEmpty(count: integer): boolean` | Check if array is empty |
| `IsFull` | `IsFull(count: integer; maxSize: integer): boolean` | Check if array is full |

**Performance:**
- `Fill` — O(n) linear time
- `Clear`, `IsEmpty`, `IsFull` — O(1) constant time

**Note:** All array manipulation functions work with a `count` variable that tracks the number of used elements in a fixed-size array. The array size is determined at compile time, but the `count` variable tracks how many elements are actually in use.

**Common Patterns:**
- **Stack (LIFO):** Use `Push` and `Pop`/`RPOP`
- **Queue (FIFO):** Use `Push` and `LPOP`
- **Dynamic array:** Use `count` to track size, `Push`/`Pop`/`InsertAt`/`DeleteAt` to modify

---

## Ordinal Functions

### Ordinal Operations

| Function | Signature | Description |
|----------|-----------|-------------|
| `Ord` | `Ord(x): integer` | Get ordinal value |
| `Pred` | `Pred(x): T` | Predecessor (x - 1) |
| `Succ` | `Succ(x): T` | Successor (x + 1) |
| `Low` | `Low(T): T` | Minimum value of type |
| `High` | `High(T): T` | Maximum value of type |

**Supported types:**
- Enumerations
- Subranges
- Integer types (byte, word, integer)
- Character

---

## String Interpolation

### Syntax

String interpolation allows embedding expressions in string literals:

```pascal
var name: string;
var score: integer;
begin
  name := 'Alice';
  score := 1000;
  
  // String interpolation
  var message: string;
  message := 'Player: {name}, Score: {score}';
  // message is 'Player: Alice, Score: 1000'
end.
```

### Supported Expressions

**String interpolation supports:**
- Variables (integer, byte, word, boolean, char, string, Q8.8, Q12.12)
- Arithmetic expressions
- Function calls
- Conditional expressions

### Escaping

**Literal braces:**
- `{{` → `{` (literal opening brace)
- `}}` → `}` (literal closing brace)

**Example:**
```pascal
WriteLn('Set: {{1, 2, 3}}');
// Output: Set: {1, 2, 3}
```

### Type Conversion

**Automatic conversion:**
- All supported types are automatically converted to string representation
- Integers, fixed-point, booleans, characters, and strings are supported
- Compiler checks type compatibility

---

## Platform Considerations

### Fixed-Point Precision

**Q8.8 vs Q12.12:**
- **Q8.8** — Standard precision (8 bits integer, 8 bits fraction)
- **Q12.12** — High precision (12 bits integer, 12 bits fraction)
- **Math functions** — Available in both formats
- **Constants** — Available in both formats

### Performance

**Function performance:**
- **Fast** — Abs, Round, Ceil, Floor, Trunc (simple operations)
- **Moderate** — Sqrt, Pow (iterative algorithms)
- **Slower** — Exp, Log, trigonometric functions (complex calculations)
- **Platform-specific** — Some functions may use hardware acceleration

### Memory

**Standard library functions:**
- **No heap allocation** — All functions use stack or registers
- **String functions** — Work with shortstring (255 character limit)
- **Efficient** — Optimized for retro platforms

---

## Error Handling

### Range Checking

**Some functions check ranges:**
- `StrToInt` — Returns 0 if string is invalid
- `StrToFloat` — Returns 0.0 if string is invalid
- `Sqrt` — Undefined behavior for negative values (use `Abs` first)
- `Log` — Undefined behavior for non-positive values

### Best Practices

**Always check inputs:**
```pascal
function SafeSqrt(x: Q8.8): Q8.8;
begin
  if x < 0.0 then
  begin
    WriteLn('Error: Negative value');
    SafeSqrt := 0.0;
  end
  else
    SafeSqrt := Sqrt(x);
end;
```

---

## Summary

**Standard Library Categories:**
- **String functions** — Manipulation and conversion
- **Type conversion** — Numeric and ordinal conversions
- **Math functions** — Rounding, basic math, trigonometry
- **Math constants** — Pi, E, Tau
- **String interpolation** — Embed expressions in strings

**Key Features:**
- **Type-safe** — Compiler checks all function calls
- **Efficient** — Optimized for retro platforms
- **Fixed-point** — Full support for Q8.8 and Q12.12
- **Platform-agnostic** — Core functions work on all platforms

---

**Related Documents:**
- [Type System](./03_TypeSystem.md) — Type conversions and casting
- [Lexical Structure](./01_LexicalStructure.md) — String interpolation syntax
- [Platform Specifications](../platforms/README.md) — Platform-specific extensions

**Last Updated:** 2025-01-XX

