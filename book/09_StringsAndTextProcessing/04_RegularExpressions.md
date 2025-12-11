# Regular Expressions

**Part of:** [Chapter 07: Strings and Text Processing](./README.md)

---

## Introduction

**Regular expressions (regex)** are powerful patterns for matching, searching, and manipulating text. They're essential for:
- **Pattern matching** — Find text that matches a pattern
- **Validation** — Check if input matches expected format
- **Search and replace** — Find and replace text patterns
- **Parsing** — Extract data from structured text
- **Text processing** — Complex string operations

SuperPascal provides built-in regex support for efficient text processing.

---

## What is a Regular Expression?

A **regular expression** is a pattern that describes a set of strings. It uses special characters to match:
- **Literal characters** — Match exact characters
- **Character classes** — Match sets of characters
- **Quantifiers** — Match repeated patterns
- **Anchors** — Match positions in string
- **Groups** — Capture matched text

**Example:**
```pascal
var pattern: string;
var text: string;
begin
  pattern := '[0-9]+';        // Match one or more digits
  text := 'Score: 100';
  
  if Match(pattern, text) then
    WriteLn('Found number!');
end.
```

---

## Basic Regex Functions

### Match

**Check if text matches pattern:**
```pascal
function Match(pattern, text: string): boolean;
```

**Example:**
```pascal
var pattern: string;
var email: string;
begin
  pattern := '^[a-zA-Z0-9._]+@[a-zA-Z0-9.]+$';
  email := 'user@example.com';
  
  if Match(pattern, email) then
    WriteLn('Valid email');
  else
    WriteLn('Invalid email');
end.
```

### Find

**Find first match position:**
```pascal
function Find(pattern, text: string): integer;
```

**Returns:**
- Position of first match (1-based)
- 0 if no match found

**Example:**
```pascal
var pattern: string;
var text: string;
var pos: integer;
begin
  pattern := '[0-9]+';
  text := 'Score: 100 points';
  
  pos := Find(pattern, text);
  if pos > 0 then
    WriteLn('Found number at position: ', pos);
end.
```

### FindAll

**Find all matches:**
```pascal
function FindAll(pattern, text: string; var matches: array of string; var count: integer);
```

**Example:**
```pascal
var pattern: string;
var text: string;
var matches: array[0..9] of string;
var count: integer;
var i: integer;
begin
  pattern := '[0-9]+';
  text := 'Scores: 10, 20, 30';
  
  FindAll(pattern, text, matches, count);
  for i := 0 to count - 1 do
    WriteLn('Match ', i + 1, ': ', matches[i]);
end.
```

### Replace

**Replace all matches:**
```pascal
function Replace(pattern, text, replacement: string): string;
```

**Example:**
```pascal
var pattern: string;
var text: string;
var result: string;
begin
  pattern := '[0-9]+';
  text := 'Score: 100';
  result := Replace(pattern, text, 'XXX');
  // result is 'Score: XXX'
end.
```

### ReplaceFirst

**Replace first match only:**
```pascal
function ReplaceFirst(pattern, text, replacement: string): string;
```

**Example:**
```pascal
var pattern: string;
var text: string;
var result: string;
begin
  pattern := '[0-9]+';
  text := 'Scores: 10, 20, 30';
  result := ReplaceFirst(pattern, text, 'XXX');
  // result is 'Scores: XXX, 20, 30'
end.
```

### Split

**Split text by pattern:**
```pascal
function Split(pattern, text: string; var parts: array of string; var count: integer);
```

**Example:**
```pascal
var pattern: string;
var text: string;
var parts: array[0..9] of string;
var count: integer;
var i: integer;
begin
  pattern := '[,\s]+';  // Match comma or whitespace
  text := 'apple,banana,cherry';
  
  Split(pattern, text, parts, count);
  for i := 0 to count - 1 do
    WriteLn('Part ', i + 1, ': ', parts[i]);
  // Output:
  // Part 1: apple
  // Part 2: banana
  // Part 3: cherry
end.
```

---

## Regex Syntax

### Literal Characters

**Match exact characters:**
```pascal
var pattern: string;
begin
  pattern := 'hello';  // Matches "hello" exactly
end.
```

### Character Classes

**Match sets of characters:**

| Pattern | Matches |
|---------|---------|
| `[abc]` | Any of: a, b, or c |
| `[a-z]` | Any lowercase letter |
| `[A-Z]` | Any uppercase letter |
| `[0-9]` | Any digit |
| `[a-zA-Z]` | Any letter |
| `[a-zA-Z0-9]` | Any alphanumeric |
| `[^abc]` | Any character except a, b, or c |
| `[^0-9]` | Any non-digit |

**Example:**
```pascal
var pattern: string;
begin
  pattern := '[0-9]+';        // One or more digits
  pattern := '[a-zA-Z]+';     // One or more letters
  pattern := '[a-zA-Z0-9]+';  // One or more alphanumeric
end.
```

### Predefined Character Classes

| Pattern | Matches | Equivalent |
|---------|---------|------------|
| `\d` | Digit | `[0-9]` |
| `\D` | Non-digit | `[^0-9]` |
| `\w` | Word character | `[a-zA-Z0-9_]` |
| `\W` | Non-word character | `[^a-zA-Z0-9_]` |
| `\s` | Whitespace | `[ \t\n\r]` |
| `\S` | Non-whitespace | `[^ \t\n\r]` |
| `.` | Any character | (except newline) |

**Example:**
```pascal
var pattern: string;
begin
  pattern := '\d+';      // One or more digits
  pattern := '\w+';      // One or more word characters
  pattern := '\s+';      // One or more whitespace
  pattern := '.';        // Any single character
end.
```

### Quantifiers

**Match repeated patterns:**

| Pattern | Matches |
|---------|---------|
| `*` | Zero or more |
| `+` | One or more |
| `?` | Zero or one (optional) |
| `{n}` | Exactly n times |
| `{n,}` | n or more times |
| `{n,m}` | Between n and m times |

**Example:**
```pascal
var pattern: string;
begin
  pattern := '[0-9]*';      // Zero or more digits
  pattern := '[0-9]+';      // One or more digits
  pattern := '[0-9]?';      // Zero or one digit
  pattern := '[0-9]{3}';    // Exactly 3 digits
  pattern := '[0-9]{2,4}';  // 2 to 4 digits
end.
```

### Anchors

**Match positions in string:**

| Pattern | Matches |
|---------|---------|
| `^` | Start of string |
| `$` | End of string |
| `\b` | Word boundary |
| `\B` | Non-word boundary |

**Example:**
```pascal
var pattern: string;
begin
  pattern := '^hello';    // Starts with "hello"
  pattern := 'world$';    // Ends with "world"
  pattern := '^hello$';   // Exactly "hello"
  pattern := '\bword\b';   // Whole word "word"
end.
```

### Alternation

**Match one of several patterns:**
```pascal
var pattern: string;
begin
  pattern := 'cat|dog|bird';  // Matches "cat" or "dog" or "bird"
end.
```

### Groups

**Capture matched text:**
```pascal
var pattern: string;
var text: string;
var groups: array[0..9] of string;
var count: integer;
begin
  pattern := '([0-9]+)-([0-9]+)';  // Two groups
  text := 'Score: 100-200';
  
  if MatchGroups(pattern, text, groups, count) then
  begin
    WriteLn('First number: ', groups[0]);   // "100"
    WriteLn('Second number: ', groups[1]);  // "200"
  end;
end.
```

### Escaping Special Characters

**Match literal special characters:**
```pascal
var pattern: string;
begin
  pattern := '\\.';      // Match literal dot
  pattern := '\\+';      // Match literal plus
  pattern := '\\*';      // Match literal asterisk
  pattern := '\\?';      // Match literal question mark
  pattern := '\\^';      // Match literal caret
  pattern := '\\$';      // Match literal dollar
  pattern := '\\[';      // Match literal bracket
  pattern := '\\]';      // Match literal bracket
  pattern := '\\(';      // Match literal parenthesis
  pattern := '\\)';      // Match literal parenthesis
  pattern := '\\\\';     // Match literal backslash
end.
```

---

## Common Patterns

### Pattern 1: Email Validation

```pascal
function IsValidEmail(email: string): boolean;
var pattern: string;
begin
  pattern := '^[a-zA-Z0-9._]+@[a-zA-Z0-9.]+$';
  IsValidEmail := Match(pattern, email);
end;
```

### Pattern 2: Extract Numbers

```pascal
procedure ExtractNumbers(text: string; var numbers: array of integer; var count: integer);
var pattern: string;
var matches: array[0..9] of string;
var matchCount: integer;
var i: integer;
begin
  pattern := '[0-9]+';
  FindAll(pattern, text, matches, matchCount);
  
  count := 0;
  for i := 0 to matchCount - 1 do
  begin
    if count < Length(numbers) then
    begin
      numbers[count] := StrToInt(matches[i]);
      count := count + 1;
    end;
  end;
end;
```

### Pattern 3: Remove Whitespace

```pascal
function RemoveWhitespace(text: string): string;
var pattern: string;
begin
  pattern := '\s+';
  RemoveWhitespace := Replace(pattern, text, ' ');
end;
```

### Pattern 4: Validate Phone Number

```pascal
function IsValidPhone(phone: string): boolean;
var pattern: string;
begin
  // Format: (123) 456-7890 or 123-456-7890
  pattern := '^(\([0-9]{3}\)\s?|[0-9]{3}-)[0-9]{3}-[0-9]{4}$';
  IsValidPhone := Match(pattern, phone);
end;
```

### Pattern 5: Extract Words

```pascal
procedure ExtractWords(text: string; var words: array of string; var count: integer);
var pattern: string;
begin
  pattern := '\b[a-zA-Z]+\b';  // Whole words
  FindAll(pattern, text, words, count);
end;
```

### Pattern 6: Format Numbers

```pascal
function FormatNumber(num: string): string;
var pattern: string;
begin
  // Add commas to numbers: 1000 -> 1,000
  pattern := '(\d)(\d{3})$';
  // Note: This is simplified - full implementation would use groups
  FormatNumber := num;  // Simplified
end;
```

---

## Advanced Features

### Case-Insensitive Matching

**Use `(?i)` flag:**
```pascal
var pattern: string;
begin
  pattern := '(?i)hello';  // Case-insensitive
  // Matches: "hello", "Hello", "HELLO", etc.
end.
```

### Greedy vs Non-Greedy

**Greedy (default):**
```pascal
var pattern: string;
var text: string;
begin
  pattern := '<.*>';      // Greedy: matches as much as possible
  text := '<tag>content</tag>';
  // Matches: "<tag>content</tag>"
end.
```

**Non-greedy (lazy):**
```pascal
var pattern: string;
var text: string;
begin
  pattern := '<.*?>';     // Non-greedy: matches as little as possible
  text := '<tag>content</tag>';
  // Matches: "<tag>" and "</tag>"
end.
```

### Lookahead and Lookbehind

**Positive lookahead:**
```pascal
var pattern: string;
begin
  pattern := '\d+(?=px)';  // Digits followed by "px"
  // Matches "100" in "100px" but not "100pt"
end.
```

**Negative lookahead:**
```pascal
var pattern: string;
begin
  pattern := '\d+(?!px)';  // Digits not followed by "px"
  // Matches "100" in "100pt" but not "100px"
end.
```

---

## Performance Considerations

### Compile Patterns

**For repeated use, compile pattern:**
```pascal
type
  TRegexPattern = record
    Pattern: string;
    Compiled: pointer;  // Internal compiled representation
  end;

function CompilePattern(pattern: string): TRegexPattern;
begin
  CompilePattern.Pattern := pattern;
  CompilePattern.Compiled := RegexCompile(pattern);
end;

function MatchCompiled(var compiled: TRegexPattern; text: string): boolean;
begin
  MatchCompiled := RegexMatch(compiled.Compiled, text);
end;
```

### Avoid Complex Patterns

**Simple patterns are faster:**
```pascal
// Fast: Simple character class
pattern := '[0-9]+';

// Slower: Complex nested groups
pattern := '(([a-z]+)([0-9]+))+';
```

---

## Best Practices

### 1. Escape Special Characters

**Always escape special characters when matching literally:**
```pascal
// Good: Escaped
pattern := '\\.txt$';  // Match ".txt" at end

// Bad: Not escaped
pattern := '.txt$';    // Matches any character + "txt"
```

### 2. Use Anchors for Validation

**Use `^` and `$` for full string matching:**
```pascal
// Good: Full string match
pattern := '^[0-9]+$';  // Entire string is digits

// Bad: Partial match
pattern := '[0-9]+';    // May match part of string
```

### 3. Be Specific

**Use specific patterns instead of `.` when possible:**
```pascal
// Good: Specific
pattern := '[a-zA-Z]+';  // Letters only

// Less clear: Too general
pattern := '.+';         // Any characters
```

### 4. Test Patterns

**Test patterns with various inputs:**
```pascal
procedure TestPattern(pattern, text: string);
begin
  if Match(pattern, text) then
    WriteLn('Match: ', text)
  else
    WriteLn('No match: ', text);
end;

begin
  TestPattern('[0-9]+', '123');      // Should match
  TestPattern('[0-9]+', 'abc');      // Should not match
  TestPattern('[0-9]+', '123abc');    // Should match (partial)
  TestPattern('^[0-9]+$', '123abc'); // Should not match (full)
end.
```

### 5. Handle Errors

**Check for invalid patterns:**
```pascal
function SafeMatch(pattern, text: string): boolean;
begin
  try
    SafeMatch := Match(pattern, text);
  except
    WriteLn('Error: Invalid regex pattern');
    SafeMatch := false;
  end;
end;
```

---

## Platform Considerations

### Memory

**Regex patterns:**
- **Compiled patterns** — Stored in memory
- **Match results** — Temporary storage
- **On ZealZ80** — Keep patterns simple, avoid deep nesting

### Performance

**Regex operations:**
- **Simple patterns** — Fast (O(n) where n is text length)
- **Complex patterns** — Slower (backtracking can be expensive)
- **Compiled patterns** — Faster for repeated use
- **On ZealZ80** — Prefer simple patterns for performance

---

## Summary

**Key Concepts:**
- **Regular expressions** — Patterns for matching text
- **Character classes** — Match sets of characters
- **Quantifiers** — Match repeated patterns
- **Anchors** — Match positions
- **Groups** — Capture matched text

**Common Functions:**
- `Match()` — Check if text matches pattern
- `Find()` — Find first match position
- `FindAll()` — Find all matches
- `Replace()` — Replace all matches
- `Split()` — Split text by pattern

**Common Patterns:**
- `[0-9]+` — One or more digits
- `[a-zA-Z]+` — One or more letters
- `^...$` — Full string match
- `\b...\b` — Whole word match

**Best Practices:**
- Escape special characters
- Use anchors for validation
- Be specific with patterns
- Test patterns thoroughly
- Handle errors gracefully

**Next:** Continue with units and modular programming, or explore more advanced text processing.

---

**Previous Section:** [Text Formatting](./03_TextFormatting.md)  
**Next Chapter:** [Chapter 08: Units and Modular Programming](../10_UnitsAndModularProgramming/README.md)  
**Language Specification:** See [13_StandardLibrary.md](../../languageSpecification/13_StandardLibrary.md)  
**Last Updated:** 2025-01-XX

