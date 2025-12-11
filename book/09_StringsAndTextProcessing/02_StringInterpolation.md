# String Interpolation

**Part of:** [Chapter 07: Strings and Text Processing](./README.md)

---

## Introduction

**String interpolation** allows you to embed expressions directly in string literals. It's a convenient way to create formatted strings without manual concatenation.

**Benefits:**
- **Cleaner code** — No need for multiple concatenations
- **Readable** — Expressions are visible in the string
- **Less error-prone** — Compiler checks syntax
- **Modern syntax** — Similar to other modern languages

---

## Basic Syntax

### Interpolation Syntax

**String interpolation uses `{}` to embed expressions:**
```pascal
var name: string;
var score: integer;
begin
  name := 'Alice';
  score := 1000;
  
  // String interpolation
  var message: string;
  message := 'Player: {name}, Score: {score}';
  WriteLn(message);
  // Output: Player: Alice, Score: 1000
end.
```

### Expression Evaluation

**Any valid expression can be embedded:**
```pascal
var x, y: integer;
begin
  x := 10;
  y := 20;
  
  // Arithmetic expressions
  WriteLn('Sum: {x + y}');        // Sum: 30
  WriteLn('Product: {x * y}');     // Product: 200
  WriteLn('Average: {(x + y) / 2}'); // Average: 15
end.
```

### Multiple Expressions

**Multiple expressions in one string:**
```pascal
var health, maxHealth: integer;
begin
  health := 75;
  maxHealth := 100;
  
  WriteLn('Health: {health}/{maxHealth} ({health * 100 / maxHealth}%)');
  // Output: Health: 75/100 (75%)
end.
```

---

## Type Conversions

### Automatic Conversion

**String interpolation automatically converts types:**
```pascal
var score: integer;
var ratio: Q8.8;
var isAlive: boolean;
begin
  score := 1000;
  ratio := 0.75;
  isAlive := true;
  
  WriteLn('Score: {score}, Ratio: {ratio}, Alive: {isAlive}');
  // Output: Score: 1000, Ratio: 0.75, Alive: true
end.
```

### Supported Types

**String interpolation supports:**
- **Integers** — `integer`, `byte`, `word`
- **Fixed-point** — `Q8.8`, `Q12.12`
- **Booleans** — `true` → `'true'`, `false` → `'false'`
- **Characters** — Single characters
- **Strings** — Embedded strings
- **Expressions** — Any expression that evaluates to supported type

---

## Advanced Usage

### Nested Expressions

**Expressions can contain function calls:**
```pascal
function GetPlayerName(id: integer): string;
begin
  GetPlayerName := 'Player' + IntToStr(id);
end;

var playerId: integer;
begin
  playerId := 1;
  WriteLn('Welcome, {GetPlayerName(playerId)}!');
  // Output: Welcome, Player1!
end.
```

### Conditional Expressions

**Use conditional expressions:**
```pascal
var health: integer;
begin
  health := 25;
  
  WriteLn('Status: {if health > 50 then "Healthy" else "Low"}');
  // Output: Status: Low
end.
```

### Formatting Numbers

**Format numbers with precision:**
```pascal
var value: Q8.8;
begin
  value := 3.14159;
  
  // Format to 2 decimal places (future: format specifiers)
  WriteLn('Value: {value}');
  // Output: Value: 3.14159
end.
```

---

## Escaping

### Literal Braces

**To include literal `{` or `}`, escape them:**
```pascal
WriteLn('Set: {{1, 2, 3}}');
// Output: Set: {1, 2, 3}
```

**Escape sequences:**
- `{{` → `{` (literal opening brace)
- `}}` → `}` (literal closing brace)

### Example: JSON-like Output

```pascal
var name: string;
var score: integer;
begin
  name := 'Alice';
  score := 1000;
  
  WriteLn('{{"name": "{name}", "score": {score}}}');
  // Output: {"name": "Alice", "score": 1000}
end.
```

---

## Comparison with Concatenation

### Before (Concatenation)

**Old way using concatenation:**
```pascal
var name: string;
var score: integer;
begin
  name := 'Alice';
  score := 1000;
  
  WriteLn('Player: ' + name + ', Score: ' + IntToStr(score));
end.
```

### After (Interpolation)

**New way using interpolation:**
```pascal
var name: string;
var score: integer;
begin
  name := 'Alice';
  score := 1000;
  
  WriteLn('Player: {name}, Score: {score}');
end.
```

**Benefits:**
- **Cleaner** — Less verbose
- **Readable** — Easier to see the final string
- **Type-safe** — Compiler checks expressions
- **Less error-prone** — No manual IntToStr calls

---

## Common Patterns

### Status Messages

```pascal
var health, maxHealth: integer;
var level: integer;
begin
  health := 75;
  maxHealth := 100;
  level := 5;
  
  WriteLn('Level {level}: {health}/{maxHealth} HP');
  // Output: Level 5: 75/100 HP
end.
```

### Debug Output

```pascal
var x, y: integer;
begin
  x := 10;
  y := 20;
  
  WriteLn('Position: ({x}, {y})');
  // Output: Position: (10, 20)
end.
```

### Error Messages

```pascal
var errorCode: integer;
var message: string;
begin
  errorCode := 404;
  message := 'Not Found';
  
  WriteLn('Error {errorCode}: {message}');
  // Output: Error 404: Not Found
end.
```

### Game Messages

```pascal
var playerName: string;
var score: integer;
var level: integer;
begin
  playerName := 'Alice';
  score := 1000;
  level := 3;
  
  WriteLn('{playerName} reached level {level} with {score} points!');
  // Output: Alice reached level 3 with 1000 points!
end.
```

---

## Limitations

### Compile-Time Evaluation

**String interpolation is evaluated at compile time when possible:**
```pascal
const
  NAME = 'SuperPascal';
  VERSION = 1;
  
begin
  WriteLn('{NAME} v{VERSION}');
  // Evaluated at compile time
end.
```

### Runtime Evaluation

**Runtime expressions are evaluated when string is created:**
```pascal
var x: integer;
begin
  x := 10;
  var msg: string;
  msg := 'Value: {x}';  // Evaluated here
  x := 20;              // Changing x doesn't affect msg
  WriteLn(msg);         // Still outputs: Value: 10
end.
```

### String Length

**Interpolated strings still subject to 255 character limit:**
```pascal
// This is fine
var short: string;
short := 'Hello {name}';

// This might exceed limit if name is very long
var long: string;
long := 'Very long message with {veryLongVariableName}';
```

---

## Best Practices

### 1. Use for Readability

**Prefer interpolation for complex strings:**
```pascal
// Good: Clear and readable
WriteLn('Player {name} has {health} HP');

// Less clear: Harder to read
WriteLn('Player ' + name + ' has ' + IntToStr(health) + ' HP');
```

### 2. Keep Expressions Simple

**Keep embedded expressions simple:**
```pascal
// Good: Simple expression
WriteLn('Sum: {a + b}');

// Avoid: Complex logic in interpolation
WriteLn('Result: {if (a > b) and (c < d) then x else y}');
// Better: Calculate first, then interpolate
var result: integer;
if (a > b) and (c < d) then
  result := x
else
  result := y;
WriteLn('Result: {result}');
```

### 3. Escape When Needed

**Always escape literal braces:**
```pascal
// Correct
WriteLn('Set: {{1, 2, 3}}');

// Incorrect (will try to interpolate)
WriteLn('Set: {1, 2, 3}');
```

### 4. Type Safety

**Let compiler check types:**
```pascal
// Compiler ensures types are compatible
var score: integer;
WriteLn('Score: {score}');  // OK

// Compiler error if type can't be converted
var ptr: pointer;
WriteLn('Ptr: {ptr}');  // Error: pointer can't be converted to string
```

---

## Platform Considerations

### Compile-Time Optimization

**Compiler optimizes string interpolation:**
- **Constant expressions** — Evaluated at compile time
- **Simple variables** — Inlined when possible
- **Complex expressions** — Generated as efficient code

### Memory Usage

**Interpolated strings use same memory as concatenation:**
- **No extra overhead** — Same memory model
- **255 character limit** — Still applies
- **Efficient** — Compiler generates optimal code

---

## Summary

**Key Concepts:**
- **String interpolation** — Embed expressions in strings with `{}`
- **Automatic conversion** — Types converted to strings automatically
- **Escaping** — Use `{{` and `}}` for literal braces
- **Type-safe** — Compiler checks expressions

**Syntax:**
- `'Text {expression} more text'` — Basic interpolation
- `'Text {{literal}}'` — Escaped braces
- `'Multiple {expr1} and {expr2}'` — Multiple expressions

**Benefits:**
- Cleaner code
- More readable
- Type-safe
- Less error-prone

**Use When:**
- Creating formatted strings
- Status messages
- Debug output
- User-facing text

**Next:** Learn about advanced text formatting.

---

**Next Section:** [Text Formatting](./03_TextFormatting.md)  
**Language Specification:** See [01_LexicalStructure.md](../../languageSpecification/01_LexicalStructure.md)  
**Last Updated:** 2025-01-XX

