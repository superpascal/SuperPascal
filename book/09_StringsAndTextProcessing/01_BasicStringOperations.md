# Basic String Operations

**Part of:** [Chapter 07: Strings and Text Processing](./README.md)

---

## Introduction

**Strings** are sequences of characters used for text. They're essential for:
- **User interaction** — Displaying messages, getting input
- **Game UI** — Menus, scores, status messages
- **Data processing** — Parsing, formatting, searching
- **Debugging** — Outputting information

SuperPascal uses a **unified short string type** — efficient and perfect for retro platforms.

---

## String Basics Review

### String Type

**SuperPascal strings:**
- **Type:** `string`
- **Maximum length:** 255 characters
- **Memory:** 1 byte (length) + N bytes (characters)
- **Indexing:** 1-based (first character is at index 1)

**Declaration:**
```pascal
var name: string;
var message: string;
```

### String Literals

**String literals use single quotes:**
```pascal
var text: string;
begin
  text := 'Hello, World!';
  text := 'SuperPascal';
  text := 'It''s a test';  // Escape single quote with ''
end.
```

---

## String Concatenation

### Concatenation Operator

**Use `+` to combine strings:**
```pascal
var first, last, full: string;
begin
  first := 'John';
  last := 'Doe';
  full := first + ' ' + last;  // 'John Doe'
  WriteLn(full);
end.
```

### Multiple Concatenations

```pascal
var greeting: string;
var name: string;
begin
  name := 'Alice';
  greeting := 'Hello, ' + name + '! Welcome to SuperPascal.';
  WriteLn(greeting);
end.
```

### Concatenation in WriteLn

**WriteLn can concatenate automatically:**
```pascal
var name: string;
var score: integer;
begin
  name := 'Player1';
  score := 100;
  WriteLn('Player: ', name, ', Score: ', score);
  // Output: Player: Player1, Score: 100
end.
```

---

## String Comparison

### Equality

**Compare strings with `=` and `<>`:**
```pascal
var name1, name2: string;
begin
  name1 := 'Alice';
  name2 := 'Bob';
  
  if name1 = name2 then
    WriteLn('Names match')
  else
    WriteLn('Names differ');
end.
```

### Ordering

**Strings can be compared with `<`, `>`, `<=`, `>=`:**
```pascal
var str1, str2: string;
begin
  str1 := 'Apple';
  str2 := 'Banana';
  
  if str1 < str2 then
    WriteLn('Apple comes before Banana');  // Lexicographic order
end.
```

**Comparison rules:**
- **Lexicographic** — Alphabetical order
- **Case-sensitive** — 'A' < 'a' (usually)
- **Character-by-character** — Compares until difference found

---

## String Functions

### Length

**Get string length:**
```pascal
function Length(str: string): integer;
```

**Example:**
```pascal
var text: string;
var len: integer;
begin
  text := 'Hello';
  len := Length(text);  // 5
  WriteLn('Length: ', len);
end.
```

### Copy

**Extract substring:**
```pascal
function Copy(str: string; start, count: integer): string;
```

**Example:**
```pascal
var text, part: string;
begin
  text := 'Hello, World!';
  part := Copy(text, 1, 5);   // 'Hello'
  part := Copy(text, 8, 5);   // 'World'
  WriteLn(part);
end.
```

**Parameters:**
- **str** — Source string
- **start** — Starting position (1-based)
- **count** — Number of characters to copy

### Pos

**Find substring position:**
```pascal
function Pos(substr, str: string): integer;
```

**Example:**
```pascal
var text: string;
var position: integer;
begin
  text := 'Hello, World!';
  position := Pos('World', text);  // 8
  if position > 0 then
    WriteLn('Found at position ', position)
  else
    WriteLn('Not found');
end.
```

**Returns:**
- **Position** (1-based) if found
- **0** if not found

### Python-like `in` Operator

**Check if substring is in string (Python-like):**
```pascal
var text: string;
begin
  text := 'Hello, World!';
  
  // Python-like membership test
  if 'World' in text then
    WriteLn('World is in string');
  
  if 'X' in text then
    WriteLn('X is in string')
  else
    WriteLn('X is not in string');
  
  // Equivalent to Pos() > 0
  if Pos('World', text) > 0 then
    WriteLn('World is in string');
end.
```

**Find position:**
```pascal
var text: string;
var pos: integer;
begin
  text := 'Hello, World!';
  
  // Find position (returns 0 if not found, 1-based)
  pos := Find('World', text);  // pos = 8
  pos := Find('X', text);      // pos = 0 (not found)
  
  if pos > 0 then
    WriteLn('Found at position: ', pos)
  else
    WriteLn('Not found');
  
  // Find is an alias for Pos()
  pos := Pos('World', text);  // Same as Find()
end.
```

**Performance:**
- `in` operator: O(n) linear time (substring search, returns boolean)
- `Find`/`Pos` function: O(n) linear time (substring search, returns position)
- Both use the same underlying algorithm

### Delete

**Remove characters:**
```pascal
procedure Delete(var str: string; start, count: integer);
```

**Example:**
```pascal
var text: string;
begin
  text := 'Hello, World!';
  Delete(text, 6, 2);  // Delete ', ' (position 6, 2 characters)
  WriteLn(text);  // 'HelloWorld!'
end.
```

**Note:** Modifies the string in place.

### Insert

**Insert substring:**
```pascal
procedure Insert(substr: string; var str: string; position: integer);
```

**Example:**
```pascal
var text: string;
begin
  text := 'HelloWorld!';
  Insert(', ', text, 6);  // Insert at position 6
  WriteLn(text);  // 'Hello, World!'
end.
```

**Note:** Modifies the string in place.

---

## Character Access

### Indexing

**Access individual characters (1-based):**
```pascal
var text: string;
var ch: char;
begin
  text := 'Hello';
  ch := text[1];  // 'H' (first character)
  ch := text[5];  // 'o' (fifth character)
  WriteLn(ch);
end.
```

### Modifying Characters

**Change individual characters:**
```pascal
var text: string;
begin
  text := 'Hello';
  text[1] := 'h';  // Change 'H' to 'h'
  WriteLn(text);    // 'hello'
end.
```

### Character-by-Character Processing

**Process each character:**
```pascal
var text: string;
var i: integer;
var ch: char;
begin
  text := 'Hello';
  for i := 1 to Length(text) do
  begin
    ch := text[i];
    WriteLn('Character ', i, ': ', ch);
  end;
end.
```

---

## Common String Patterns

### Pattern 1: Trimming (Manual)

**Remove leading/trailing spaces:**
```pascal
function TrimLeft(const str: string): string;
var i: integer;
begin
  i := 1;
  while (i <= Length(str)) and (str[i] = ' ') do
    i := i + 1;
  TrimLeft := Copy(str, i, Length(str) - i + 1);
end;

function TrimRight(const str: string): string;
var i: integer;
begin
  i := Length(str);
  while (i > 0) and (str[i] = ' ') do
    i := i - 1;
  TrimRight := Copy(str, 1, i);
end;
```

### Pattern 2: Converting Case

**Convert to uppercase (manual):**
```pascal
function ToUpper(const str: string): string;
var i: integer;
var result: string;
var ch: char;
begin
  result := '';
  for i := 1 to Length(str) do
  begin
    ch := str[i];
    if (ch >= 'a') and (ch <= 'z') then
      ch := Chr(Ord(ch) - 32);  // Convert to uppercase
    result := result + ch;
  end;
  ToUpper := result;
end;
```

### Pattern 3: Reversing a String

```pascal
function Reverse(const str: string): string;
var i: integer;
var result: string;
begin
  result := '';
  for i := Length(str) downto 1 do
    result := result + str[i];
  Reverse := result;
end;
```

### Pattern 4: Counting Occurrences

```pascal
function CountChar(const str: string; ch: char): integer;
var i, count: integer;
begin
  count := 0;
  for i := 1 to Length(str) do
    if str[i] = ch then
      count := count + 1;
  CountChar := count;
end;
```

---

## String Examples

### Example 1: Parsing Names

```pascal
var fullName: string;
var firstName, lastName: string;
var spacePos: integer;
begin
  fullName := 'John Doe';
  spacePos := Pos(' ', fullName);
  
  if spacePos > 0 then
  begin
    firstName := Copy(fullName, 1, spacePos - 1);
    lastName := Copy(fullName, spacePos + 1, Length(fullName) - spacePos);
    WriteLn('First: ', firstName);
    WriteLn('Last: ', lastName);
  end;
end.
```

### Example 2: Building Messages

```pascal
var playerName: string;
var score: integer;
var message: string;
begin
  playerName := 'Alice';
  score := 1000;
  
  message := 'Player: ' + playerName + ' | Score: ' + IntToStr(score);
  WriteLn(message);
end.
```

### Example 3: String Validation

```pascal
function IsValidName(const name: string): boolean;
var i: integer;
var ch: char;
begin
  if Length(name) = 0 then
  begin
    IsValidName := false;
    Exit;
  end;
  
  for i := 1 to Length(name) do
  begin
    ch := name[i];
    if not ((ch >= 'A') and (ch <= 'Z')) and
       not ((ch >= 'a') and (ch <= 'z')) and
       not (ch = ' ') then
    begin
      IsValidName := false;
      Exit;
    end;
  end;
  
  IsValidName := true;
end;
```

---

## String and Arrays

### String as Character Array

**Strings are like arrays of characters:**
```pascal
var text: string;
var i: integer;
begin
  text := 'Hello';
  
  // Process like array
  for i := 1 to Length(text) do
    WriteLn('Char[', i, '] = ', text[i]);
end.
```

### Converting Between String and Array

**Manual conversion:**
```pascal
var chars: array[1..10] of char;
var text: string;
var i: integer;
begin
  text := 'Hello';
  
  // String to array
  for i := 1 to Length(text) do
    chars[i] := text[i];
  
  // Array to string (manual)
  text := '';
  for i := 1 to 5 do
    text := text + chars[i];
end.
```

---

## Best Practices

### 1. Check Length Before Access

**Bad:**
```pascal
var text: string;
begin
  text := 'Hi';
  WriteLn(text[10]);  // ERROR: Index out of bounds!
end.
```

**Good:**
```pascal
var text: string;
begin
  text := 'Hi';
  if Length(text) >= 10 then
    WriteLn(text[10])
  else
    WriteLn('String too short');
end.
```

### 2. Use String Functions

**Bad:**
```pascal
// Manual substring (error-prone)
var part: string;
part := text[5] + text[6] + text[7];
```

**Good:**
```pascal
// Use Copy function
var part: string;
part := Copy(text, 5, 3);
```

### 3. Handle Empty Strings

**Always check for empty strings:**
```pascal
if Length(text) > 0 then
  ProcessString(text)
else
  WriteLn('Empty string');
```

### 4. Be Mindful of String Length

**On ZealZ80:**
- **Maximum 255 characters** — Plan accordingly
- **Memory usage** — Each string uses 1 + length bytes
- **Long strings** — Consider splitting or truncating

---

## Platform Considerations

### Memory Usage

**String memory:**
- **1 byte** — Length
- **N bytes** — Characters (up to 255)
- **Total** — 1 + length bytes

**Example:**
```pascal
var text: string;
text := 'Hello';  // Uses 6 bytes: 1 (length) + 5 (characters)
```

### Performance

**String operations:**
- **Concatenation** — Creates new string (may copy)
- **Copy** — Creates new string
- **Delete/Insert** — Modifies in place (efficient)
- **Character access** — Very fast (direct indexing)

**On ZealZ80:**
- **Keep strings short** — 255 char limit
- **Avoid excessive concatenation** — Can be slow
- **Use Delete/Insert** — More efficient than rebuilding

---

## Common Mistakes

### Mistake 1: Index Out of Bounds

**Wrong:**
```pascal
var text: string;
begin
  text := 'Hi';
  WriteLn(text[10]);  // ERROR: Only 2 characters!
end.
```

**Correct:**
```pascal
var text: string;
begin
  text := 'Hi';
  if Length(text) >= 10 then
    WriteLn(text[10])
  else
    WriteLn('Index out of range');
end.
```

### Mistake 2: Forgetting Length Byte

**Remember:** Strings include a length byte, so:
```pascal
var text: string;
text := 'Hello';  // Length is 5, but uses 6 bytes total
```

### Mistake 3: Case Sensitivity

**String comparison is case-sensitive:**
```pascal
var text: string;
begin
  text := 'Hello';
  if text = 'hello' then  // false! (case-sensitive)
    WriteLn('Match');
end.
```

---

## Summary

**Key Concepts:**
- **Strings** are sequences of characters (up to 255)
- **Concatenation** — Use `+` to combine strings
- **Comparison** — `=`, `<>`, `<`, `>`, `<=`, `>=`
- **String functions** — Length, Copy, Pos, Delete, Insert
- **Character access** — Use `[index]` (1-based)

**String Functions:**
- `Length(str)` — Get string length
- `Copy(str, start, count)` — Extract substring
- `Pos(substr, str)` — Find substring position
- `Delete(var str, start, count)` — Remove characters
- `Insert(substr, var str, pos)` — Insert substring

**Best Practices:**
- Check length before access
- Use string functions (don't reinvent)
- Handle empty strings
- Be mindful of 255 character limit

**Next:** Learn about text formatting for output.

---

## Exercises

### GCSE Level Exercises

**Exercise 1: String Basics**
Write a program that:
1. Creates a string variable with your name
2. Displays the string
3. Displays the length of the string
4. Displays the first and last character

**Exercise 2: String Concatenation**
Write a program that:
1. Creates two strings: "Hello" and "World"
2. Combines them to make "Hello World"
3. Displays the result

**Exercise 3: String Comparison**
Write a program that:
1. Asks for two words
2. Compares them
3. Displays if they are the same or different

### A-Level Exercises

**Exercise 1: String Manipulation**
Write a program that:
1. Takes a string input
2. Converts to uppercase
3. Converts to lowercase
4. Reverses the string
5. Displays all versions

**Exercise 2: String Search**
Write a program that:
1. Takes a sentence and a word to find
2. Searches for the word in the sentence
3. Displays the position if found
4. Displays "Not found" if not present

**Exercise 3: String Validation**
Write a program that:
1. Validates a username (alphanumeric, 3-20 chars)
2. Validates an email (contains @ and .)
3. Validates a password (min 8 chars, contains letter and number)
4. Displays validation results

### University Level Exercises

**Exercise 1: String Algorithm Implementation**
Implement efficient string algorithms:
1. KMP pattern matching algorithm
2. Boyer-Moore string search
3. Compare performance with naive search
4. Analyze time complexity

**Exercise 2: String Processing Library**
Design and implement a string utility library:
1. Tokenization (split by delimiter)
2. String formatting (printf-like)
3. String escaping/unescaping
4. Memory-efficient operations
5. Document time/space complexity

**Exercise 3: String Interning System**
Implement a string interning system:
1. Store unique strings only once
2. Return references to interned strings
3. Compare performance vs. regular strings
4. Analyze memory usage
5. Implement garbage collection for unused strings

---

**Next Section:** [String Interpolation](./02_StringInterpolation.md)  
**Language Specification:** See [03_TypeSystem.md](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

