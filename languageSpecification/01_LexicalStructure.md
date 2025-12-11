# SuperPascal Language Specification — Lexical Structure

## Complete Lexical Analysis Specification

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

---

## 1. Character Set

### 1.1 Supported Characters

SuperPascal uses a subset of ASCII:

- **Letters**: `A-Z`, `a-z` (case-insensitive)
- **Digits**: `0-9`
- **Whitespace**: Space (` `), Tab (`\t`), Carriage Return (`\r`), Line Feed (`\n`)
- **Special Characters**: See Section 1.2

### 1.2 Special Characters

```
! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
```

### 1.3 Character Encoding

- Source files are assumed to be ASCII or UTF-8
- Only ASCII characters (0x00-0x7F) are guaranteed to work
- Extended characters (0x80-0xFF) may be used in string literals and comments
- Character literals use 8-bit values

### 1.4 Case Sensitivity

SuperPascal is **case-insensitive** for:
- Keywords
- Identifiers
- Predefined identifiers (types, procedures, functions)

Case is preserved in:
- String literals
- Character literals
- Comments

---

## 2. Tokens

A SuperPascal program is a sequence of tokens. Tokens are classified as:

1. **Keywords** (reserved words)
2. **Identifiers**
3. **Literals** (integer, character, string, boolean)
4. **Operators**
5. **Delimiters**
6. **Directives** (compiler directives)

---

## 3. Keywords

Keywords are reserved and cannot be used as identifiers.

### 3.1 Core Keywords (Tier 1)

```
and          array        begin       boolean     byte
case         char         const       div         do
downto       else         end         false       for
function     goto         if          integer     mod
not          of           or          procedure   program
record       repeat       set         struct      then        to
true         type         until       var         while
word
```

**Note**: `struct` is a SuperPascal extension keyword for C-style struct syntax (alternative to `record`).

### 3.2 Unit Keywords (Tier 2)

```
implementation   interface       unit        uses
namespace        using            // Future: namespace support
```

### 3.3 Object Pascal Keywords (Tier 3)

```
class           constructor     destructor   override
private         protected       public       virtual
```

### 3.4 Exception Keywords

```
except          finally         raise        try
```

### 3.5 Special Keywords

```
nil             self            inherited
```

**Note**: `self` is implicit in class methods and record methods. `inherited` is used in class methods to call parent methods.

---

## 4. Identifiers

### 4.1 Identifier Syntax

An identifier is a sequence of:
- A letter or underscore (`_`)
- Followed by zero or more letters, digits, or underscores

**EBNF:**
```
identifier ::= letter (letter | digit | "_")*
letter     ::= "A".."Z" | "a".."z"
digit      ::= "0".."9"
```

### 4.2 Identifier Rules

- Must start with a letter or underscore
- Cannot be a keyword
- Case-insensitive (e.g., `MyVar`, `myvar`, `MYVAR` are the same)
- No length limit (practical limit: 255 characters for compatibility)
- Underscores are allowed but not required

### 4.3 Valid Examples

```
MyVariable
_private
x
EntityID
player_score
MAX_SIZE
```

### 4.4 Invalid Examples

```
123abc        // starts with digit
my-var        // contains hyphen
my.var        // contains period
if            // keyword
```

---

## 5. Integer Literals

### 5.1 Decimal Integers

A sequence of one or more digits:

```
0
42
12345
32767
```

### 5.2 Hexadecimal Integers

Hexadecimal integers use the `$` prefix:

```
$0
$FF
$1234
$FFFF
```

**Rules:**
- Must start with `$`
- Followed by one or more hexadecimal digits (`0-9`, `A-F`, `a-f`)
- Case-insensitive
- Maximum value: `$FFFF` (65535) for word, `$7FFF` (32767) for integer

### 5.3 Integer Type Inference

- Literals in range `0..32767` are `integer`
- Literals in range `32768..65535` are `word` (if context allows)
- Hexadecimal literals are `word` if value > `$7FFF`, otherwise `integer`

### 5.4 Overflow

Integer literals must fit in 16 bits:
- Decimal: `0..65535`
- Hexadecimal: `$0..$FFFF`

---

## 6. Character Literals

### 6.1 Character Syntax

A character literal is a single character enclosed in single quotes:

```
'a'
'Z'
'0'
' '
```

### 6.2 Escape Sequences

SuperPascal supports escape sequences:

```
'\''     // single quote
'\\'     // backslash
'\n'     // newline (line feed, 0x0A)
'\r'     // carriage return (0x0D)
'\t'     // tab (0x09)
'\0'     // null (0x00)
```

### 6.3 Numeric Character Codes

Character codes can be specified using `#` followed by a decimal number:

```
#65      // 'A'
#10      // newline
#255     // highest byte value
```

**Rules:**
- `#` followed by 1-3 decimal digits
- Value must be in range `0..255`
- Cannot be used in string literals (use escape sequences instead)

### 6.4 Character Type

Character literals have type `char` (8-bit, 0-255).

---

## 7. String Literals

### 7.1 String Syntax

A string literal is a sequence of characters enclosed in single quotes:

```
'Hello'
'SuperPascal'
''
```

### 7.2 Escape Sequences in Strings

Strings support the same escape sequences as characters:

```
'It''s a test'        // contains single quote
'Line 1\nLine 2'      // contains newline
'Path: C:\\Users'     // contains backslash
```

**Note**: To include a single quote in a string, use two single quotes (`''`).

### 7.3 String Length

- Maximum length: **255 characters** (shortstring model)
- Length is stored as first byte (0-255)
- Empty string `''` has length 0

### 7.4 String Type

String literals have type `string` (shortstring: 1-byte length + data).

### 7.5 String Concatenation

Strings can be concatenated at compile time if both are literals:

```
'Hello' + ' ' + 'World'  // becomes 'Hello World' at compile time
```

### 7.6 String Interpolation

SuperPascal supports **string interpolation** using `{}` to embed expressions:

```
'Player: {name}, Score: {score}'
```

**Syntax:**
- `{expression}` — Embed expression in string
- `{{` — Literal opening brace
- `}}` — Literal closing brace

**Supported expressions:**
- Variables (integer, byte, word, boolean, char, string, Q8.8, Q12.12)
- Arithmetic expressions
- Function calls
- Conditional expressions

**Examples:**
```
'Value: {x}'                    // Variable
'Sum: {a + b}'                  // Expression
'Status: {if x > 0 then "OK" else "Error"}'  // Conditional
'Set: {{1, 2, 3}}'              // Escaped braces
```

**Type conversion:**
- All supported types are automatically converted to string representation
- Integers, fixed-point, booleans, characters, and strings are supported
- Compiler checks type compatibility

---

## 8. Boolean Literals

Boolean literals are:

```
true
false
```

**Type**: `boolean` (1 byte: 0 = false, non-zero = true)

---

## 9. Operators

### 9.1 Arithmetic Operators

```
+       // addition, unary plus
-       // subtraction, unary minus
*       // multiplication
/       // division (integer division, result is integer)
div     // integer division (explicit)
mod     // modulo (remainder)
```

### 9.2 Comparison Operators

```
=       // equality
<>      // inequality
<       // less than
<=      // less than or equal
>       // greater than
>=      // greater than or equal
```

### 9.3 Logical Operators

```
and     // logical AND (short-circuit)
or      // logical OR (short-circuit)
not     // logical NOT (unary)
```

**Note**: `and` and `or` are short-circuiting: right operand is not evaluated if result is determined by left operand.

### 9.4 Bitwise Operators

```
shl     // shift left
shr     // shift right
```

**Note**: Bitwise `and`, `or`, `not` are not separate operators; use logical operators which work on boolean values. For bitwise operations on integers, use library functions.

### 9.5 Set Operators

```
+       // union (also |)
-       // difference
*       // intersection (also &)
^       // symmetric difference (SuperPascal extension)
in      // membership test (sets, arrays, lists, strings, tuples)
```

### 9.6 String Operators

```
+       // concatenation
=       // equality (lexicographic)
<>      // inequality
<       // less than (lexicographic)
<=      // less than or equal
>       // greater than
>=      // greater than or equal
```

### 9.7 Pointer Operators

```
^       // dereference (postfix)
@       // address-of (prefix)
+       // pointer + integer (additive, type-aware scaling)
-       // pointer - integer, pointer - pointer (additive, type-aware scaling)
```

---

## 10. Delimiters

### 10.1 Statement Delimiters

```
;       // statement separator
,       // list separator (parameters, declarations)
:       // type separator (var x: integer)
:=      // assignment operator
.       // record field access, end of program
..      // range operator (subranges, case labels)
```

### 10.2 Grouping Delimiters

```
(       // opening parenthesis
)       // closing parenthesis
[       // opening bracket (array index, set constructor)
]       // closing bracket
{       // opening brace (comments, directives)
}       // closing brace
(*      // opening comment
*)      // closing comment
```

### 10.3 Special Delimiters

```
^       // pointer type, dereference
@       // address-of
..      // range
:=      // assignment
```

---

## 11. Comments

### 11.1 Comment Syntax

SuperPascal supports three comment styles:

#### Style 1: Brace Comments
```
{ This is a comment }
{ Multi-line
  comment }
```

#### Style 2: Parenthesis-Star Comments
```
(* This is a comment *)
(* Multi-line
   comment *)
```

#### Style 3: Line Comments (SuperPascal Extension)
```
// This is a line comment
// Everything after // to end of line is ignored
```

### 11.2 Comment Rules

- Comments are ignored by the compiler
- Comments can appear anywhere whitespace is allowed
- Nested comments: `{ { nested } }` is allowed
- `(* (* nested *) *)` is allowed
- Mixed nesting: `{ (* mixed *) }` is allowed
- Line comments (`//`) cannot be nested (end at newline)

### 11.3 Comment Examples

```pascal
{ Standard comment }
(* Alternative comment *)
// Line comment
x := 5; { inline comment }
```

---

## 12. Whitespace

### 12.1 Whitespace Characters

- Space (` `, 0x20)
- Tab (`\t`, 0x09)
- Carriage Return (`\r`, 0x0D)
- Line Feed (`\n`, 0x0A)
- Form Feed (`\f`, 0x0C) - treated as whitespace

### 12.2 Whitespace Rules

- Whitespace is required to separate tokens
- Whitespace is optional around operators and delimiters
- Multiple consecutive whitespace characters are treated as one
- Line breaks are significant for line comments (`//`)

### 12.3 Examples

```pascal
x:=5;        // OK: no whitespace needed
x := 5;      // OK: whitespace for readability
x: = 5;      // ERROR: := is one token, cannot split
```

---

## 13. Compiler Directives

### 13.1 Directive Syntax

Compiler directives use brace syntax with `$`:

```
{$DIRECTIVE_NAME}
{$DIRECTIVE_NAME argument}
{$DIRECTIVE_NAME(arg1, arg2)}
```

### 13.2 Directive Placement

Directives can appear:
- At the start of a program/unit
- Before declarations
- Before statements (in some cases)
- Inline with code (for some directives)

### 13.3 Standard Directives

```
{$INLINE}           // inline function/procedure
{$UNROLL}           // unroll loop
{$VBLANK_AWARE}     // optimize for vblank timing
{$RANGE_CHECK}      // enable range checking
{$OVERFLOW_CHECK}   // enable overflow checking
{$DEBUG}            // debug mode
{$RELEASE}          // release mode
```

### 13.4 Directive Examples

```pascal
{$INLINE}
procedure FastAdd(a, b: integer): integer;
begin
  Result := a + b;
end;

{$UNROLL}
for i := 0 to 3 do
  array[i] := 0;
```

---

## 14. Token Classification Summary

### 14.1 Token Categories

| Category | Examples | Notes |
|----------|----------|-------|
| Keyword | `begin`, `end`, `if`, `then` | Reserved words |
| Identifier | `MyVar`, `player_score` | User-defined names |
| Integer Literal | `42`, `$FF` | Numeric constants |
| Character Literal | `'A'`, `#65` | Single characters |
| String Literal | `'Hello'` | String constants |
| Boolean Literal | `true`, `false` | Boolean constants |
| Operator | `+`, `-`, `:=`, `=` | Operations |
| Delimiter | `;`, `,`, `(`, `)` | Punctuation |
| Directive | `{$INLINE}` | Compiler hints |

### 14.2 Token Precedence (for Lexer)

When multiple token matches are possible:

1. **Longest match wins**: `:=` matches before `:` and `=`
2. **Keywords before identifiers**: `if` is keyword, not identifier
3. **Directives before comments**: `{$...}` is directive, not comment
4. **Hex before identifiers**: `$FF` is hex literal, not identifier starting with `$`

---

## 15. Lexical Errors

### 15.1 Common Lexical Errors

1. **Invalid character**: Character not in supported set
2. **Unterminated string**: String literal without closing quote
3. **Unterminated comment**: Comment without closing delimiter
4. **Invalid escape sequence**: Unknown escape sequence in string/char
5. **Invalid hex literal**: Hex literal with invalid characters
6. **Number too large**: Integer literal exceeds 16-bit range

### 15.2 Error Recovery

The lexer should:
- Report the error with line/column position
- Attempt to recover by skipping invalid token
- Continue lexing from next valid token
- Provide helpful error messages

### 15.3 Error Message Format

```
Error: <file>:<line>:<column>: <message>
```

Example:
```
Error: program.pas:5:12: Unterminated string literal
```

---

## 16. Implementation Notes

### 16.1 Lexer Requirements

A SuperPascal lexer must:

1. Recognize all token types defined above
2. Handle case-insensitive keywords and identifiers
3. Support all comment styles
4. Process compiler directives
5. Report lexical errors clearly
6. Track source positions (line, column) for error reporting

### 16.2 Token Structure

Each token should include:
- **Kind**: Token type (keyword, identifier, literal, etc.)
- **Lexeme**: The actual text of the token
- **Value**: For literals, the parsed value
- **Position**: Source file, line, column
- **Length**: Token length in characters

### 16.3 Performance Considerations

- Use efficient string matching for keywords
- Consider keyword hash table for O(1) lookup
- Cache frequently used tokens
- Minimize allocations during tokenization

---

## 17. Special Cases and Edge Cases

### 17.1 Ambiguous Cases

1. **`..` vs `.`**: `1..10` is range, `record.field` is field access
   - Resolved by context: after number/identifier = range, after identifier = field access

2. **`:=` vs `:`**: `x := 5` is assignment, `x: integer` is type declaration
   - Resolved by context: in statement = assignment, in declaration = type separator

3. **`-` vs unary minus**: `a - b` vs `-5`
   - Resolved by parser: after expression = binary, at start/after operator = unary

### 17.2 Maximum Limits

- **Identifier length**: 255 characters (practical limit)
- **String length**: 255 characters
- **Integer range**: -32768 to 65535 (depending on type)
- **Nesting depth**: No hard limit, but practical limits apply

### 17.3 Unicode Considerations

- Source files should be ASCII or UTF-8
- Only ASCII characters guaranteed in identifiers
- Extended characters (0x80-0xFF) allowed in string/char literals
- Unicode identifiers not supported

---

**End of Lexical Structure Specification**

