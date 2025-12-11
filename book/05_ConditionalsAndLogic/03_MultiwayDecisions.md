# Multiway Decisions

**Part of:** [Chapter 04: Conditionals and Logic](./README.md)

---

## Introduction

When you have **many possible choices** based on a single value, `if-else if` chains can become long and hard to read. **`case` statements** provide a cleaner way to handle multiple choices.

This chapter teaches you:
- How to use `case` statements
- When to use `case` vs. `if-else if`
- How to handle ranges and multiple values
- How to use the `else` clause in `case`

---

## What is a Case Statement?

A **`case` statement** is like a multi-way switch:
- **Tests one value** against multiple possibilities
- **Executes code** for the matching case
- **Cleaner** than long `if-else if` chains
- **More efficient** (compiler can optimize)

**Analogy:** Think of a menu:
- **Value** — Your choice (1, 2, 3, etc.)
- **Cases** — Menu items
- **Code** — What you get

---

## Basic Case Syntax

### Syntax

**Basic `case` statement:**
```pascal
case expression of
  value1: statement1;
  value2: statement2;
  value3: statement3;
  else statement4;
end;
```

**With blocks:**
```pascal
case expression of
  value1:
  begin
    statement1;
    statement2;
  end;
  value2:
  begin
    statement3;
    statement4;
  end;
  else
  begin
    statement5;
  end;
end;
```

### Simple Example

```pascal
var choice: integer;
begin
  choice := 2;
  case choice of
    1: WriteLn('You chose option 1');
    2: WriteLn('You chose option 2');
    3: WriteLn('You chose option 3');
    else WriteLn('Invalid choice');
  end;
end.
```

**Output:**
```
You chose option 2
```

---

## Case with Different Types

### Integer Cases

```pascal
var score: integer;
begin
  score := 85;
  case score of
    90..100: WriteLn('Grade: A');
    80..89:  WriteLn('Grade: B');
    70..79:  WriteLn('Grade: C');
    60..69:  WriteLn('Grade: D');
    else     WriteLn('Grade: F');
  end;
end.
```

### Character Cases

```pascal
var direction: char;
begin
  direction := 'N';
  case direction of
    'N': WriteLn('Go North');
    'S': WriteLn('Go South');
    'E': WriteLn('Go East');
    'W': WriteLn('Go West');
    else WriteLn('Invalid direction');
  end;
end.
```

### Enumeration Cases

```pascal
type Color = (Red, Green, Blue);
var color: Color;
begin
  color := Green;
  case color of
    Red:   WriteLn('Red color');
    Green: WriteLn('Green color');
    Blue:  WriteLn('Blue color');
  end;
end.
```

---

## Case Ranges

### Range Syntax

**Use `..` for ranges:**
```pascal
var age: integer;
begin
  age := 25;
  case age of
    0..12:   WriteLn('Child');
    13..19:  WriteLn('Teenager');
    20..64:  WriteLn('Adult');
    65..150: WriteLn('Senior');
    else     WriteLn('Invalid age');
  end;
end.
```

### Multiple Values

**List multiple values separated by commas:**
```pascal
var key: char;
begin
  key := 'q';
  case key of
    'q', 'Q': WriteLn('Quit');
    'h', 'H': WriteLn('Help');
    's', 'S': WriteLn('Save');
    else      WriteLn('Unknown key');
  end;
end.
```

### Combining Ranges and Values

```pascal
var score: integer;
begin
  score := 95;
  case score of
    0..59:   WriteLn('Fail');
    60..69:  WriteLn('Pass');
    70..79:  WriteLn('Good');
    80..89:  WriteLn('Very Good');
    90..100: WriteLn('Excellent');
    else     WriteLn('Invalid score');
  end;
end.
```

---

## Case vs. If-Else If

### When to Use Case

**Use `case` when:**
- Testing **one value** against multiple constants
- Values are **discrete** (integers, characters, enums)
- You have **many choices** (3+)
- Code is **cleaner** with `case`

**Example: Menu system**
```pascal
var choice: integer;
begin
  choice := 2;
  case choice of
    1: ShowMainMenu;
    2: StartGame;
    3: LoadGame;
    4: ShowSettings;
    5: Quit;
    else WriteLn('Invalid choice');
  end;
end.
```

### When to Use If-Else If

**Use `if-else if` when:**
- Testing **multiple different values**
- Conditions are **complex** (not just equality)
- Conditions involve **ranges with variables**
- Conditions use **boolean expressions**

**Example: Complex conditions**
```pascal
var score: integer;
var timeRemaining: integer;
begin
  score := 100;
  timeRemaining := 30;
  
  // Can't use case - conditions are complex
  if (score > 100) and (timeRemaining > 0) then
    WriteLn('Excellent!')
  else if (score > 50) and (timeRemaining > 0) then
    WriteLn('Good!')
  else if timeRemaining <= 0 then
    WriteLn('Time up!')
  else
    WriteLn('Keep trying!');
end.
```

---

## Case Examples

### Example 1: Menu System

```pascal
var menuChoice: integer;
begin
  menuChoice := 3;
  case menuChoice of
    1:
    begin
      WriteLn('Starting new game...');
      InitializeGame;
    end;
    2:
    begin
      WriteLn('Loading game...');
      LoadGame;
    end;
    3:
    begin
      WriteLn('Showing settings...');
      ShowSettings;
    end;
    4:
    begin
      WriteLn('Quitting...');
      QuitGame;
    end;
    else
      WriteLn('Invalid menu choice');
  end;
end.
```

### Example 2: Direction Handling

```pascal
var direction: char;
var x, y: integer;
begin
  direction := 'N';
  x := 10;
  y := 20;
  
  case direction of
    'N', 'n': y := y - 1;  // North
    'S', 's': y := y + 1;  // South
    'E', 'e': x := x + 1;  // East
    'W', 'w': x := x - 1;  // West
    else WriteLn('Invalid direction');
  end;
  
  WriteLn('New position: (', x, ', ', y, ')');
end.
```

### Example 3: Status Codes

```pascal
var status: integer;
begin
  status := 200;
  case status of
    200: WriteLn('OK');
    301: WriteLn('Moved Permanently');
    404: WriteLn('Not Found');
    500: WriteLn('Server Error');
    else WriteLn('Unknown status: ', status);
  end;
end.
```

### Example 4: Game States

```pascal
type GameState = (Menu, Playing, Paused, GameOver);
var state: GameState;
begin
  state := Playing;
  case state of
    Menu:
    begin
      ShowMenu;
      HandleMenuInput;
    end;
    Playing:
    begin
      UpdateGame;
      DrawGame;
    end;
    Paused:
    begin
      ShowPauseScreen;
    end;
    GameOver:
    begin
      ShowGameOverScreen;
      HandleGameOverInput;
    end;
  end;
end.
```

---

## The Else Clause

### Default Case

**`else` handles unmatched values:**
```pascal
var value: integer;
begin
  value := 999;
  case value of
    1: WriteLn('One');
    2: WriteLn('Two');
    3: WriteLn('Three');
    else WriteLn('Other value');
  end;
end.
```

### When to Use Else

**Always use `else` when:**
- Value might be **outside expected range**
- You want to **handle errors** gracefully
- You want to **ensure all cases** are covered

**Example: Input validation**
```pascal
var choice: integer;
begin
  choice := 5;  // User input
  case choice of
    1: DoAction1;
    2: DoAction2;
    3: DoAction3;
    else
    begin
      WriteLn('Error: Invalid choice');
      ShowHelp;
    end;
  end;
end.
```

---

## Nested Case Statements

### Case Inside Case

**You can nest `case` statements:**
```pascal
var menu: integer;
var submenu: integer;
begin
  menu := 1;
  submenu := 2;
  
  case menu of
    1:
    begin
      WriteLn('Main Menu');
      case submenu of
        1: WriteLn('  - Option 1.1');
        2: WriteLn('  - Option 1.2');
        3: WriteLn('  - Option 1.3');
      end;
    end;
    2:
    begin
      WriteLn('Settings Menu');
      case submenu of
        1: WriteLn('  - Setting 2.1');
        2: WriteLn('  - Setting 2.2');
      end;
    end;
  end;
end.
```

**Note:** Nested cases can become complex. Consider using procedures instead.

---

## Best Practices

### 1. Always Include Else

**Bad:**
```pascal
case value of
  1: DoSomething;
  2: DoSomethingElse;
  // What if value is 3? Nothing happens!
end;
```

**Good:**
```pascal
case value of
  1: DoSomething;
  2: DoSomethingElse;
  else HandleError;
end;
```

### 2. Use Meaningful Values

**Bad:**
```pascal
case x of
  1: // What does 1 mean?
  2: // What does 2 mean?
end;
```

**Good:**
```pascal
const MENU_START = 1;
const MENU_LOAD = 2;
const MENU_QUIT = 3;

case choice of
  MENU_START: StartGame;
  MENU_LOAD: LoadGame;
  MENU_QUIT: QuitGame;
end;
```

### 3. Keep Cases Simple

**Bad:**
```pascal
case choice of
  1:
  begin
    // 100 lines of code here
  end;
end;
```

**Good:**
```pascal
case choice of
  1: HandleMenuChoice1;
  2: HandleMenuChoice2;
end;

procedure HandleMenuChoice1;
begin
  // 100 lines of code here
end;
```

### 4. Use Ranges When Appropriate

**Bad:**
```pascal
case score of
  0: WriteLn('F');
  1: WriteLn('F');
  2: WriteLn('F');
  // ... 59 more cases!
end;
```

**Good:**
```pascal
case score of
  0..59: WriteLn('F');
  60..69: WriteLn('D');
  70..79: WriteLn('C');
  80..89: WriteLn('B');
  90..100: WriteLn('A');
end;
```

---

## Common Mistakes

### Mistake 1: Missing End

**Wrong:**
```pascal
case value of
  1: DoSomething;
  2: DoSomethingElse;
// Missing 'end;'
```

**Correct:**
```pascal
case value of
  1: DoSomething;
  2: DoSomethingElse;
end;  // Don't forget end!
```

### Mistake 2: Using Variables in Cases

**Wrong:**
```pascal
var min, max: integer;
case value of
  min..max:  // ERROR: Cases must be constants!
end;
```

**Correct:**
```pascal
// Use if-else if for variable ranges
if (value >= min) and (value <= max) then
  // ...
```

### Mistake 3: Forgetting Else

**Wrong:**
```pascal
case choice of
  1: DoSomething;
  2: DoSomethingElse;
  // What if choice is 3? Nothing happens!
end;
```

**Correct:**
```pascal
case choice of
  1: DoSomething;
  2: DoSomethingElse;
  else HandleInvalidChoice;
end;
```

---

## Platform Considerations

### Performance

**Case statements:**
- **Efficient** — Compiler can optimize (jump tables)
- **Fast** — Often faster than if-else if chains
- **Predictable** — Same performance on all platforms

### Code Size

**Case statements:**
- **Can be large** — Many cases = more code
- **On ZealZ80** — Consider code size if many cases
- **Compiler optimization** — May use jump tables (efficient)

---

## Summary

**Key Concepts:**
- **`case` statements** handle multiple choices cleanly
- **Ranges** use `..` syntax: `1..10`
- **Multiple values** use commas: `1, 2, 3`
- **`else` clause** handles unmatched values
- **Use `case`** for discrete values, `if-else if` for complex conditions

**Syntax:**
```pascal
case expression of
  value1, value2: statement1;
  value3..value5: statement2;
  else statement3;
end;
```

**Best Practices:**
- Always include `else` clause
- Use meaningful values (constants)
- Keep cases simple (use procedures)
- Use ranges when appropriate
- Use `case` for discrete values, `if` for complex conditions

**Next:** Learn about loops for repetition.

---

**Next Chapter:** [Chapter 05: Loops and Iteration](../06_LoopsAndIteration/README.md)  
**Language Specification:** See [02_Grammar.md](../../languageSpecification/02_Grammar.md)  
**Last Updated:** 2025-01-XX

