# Script Syntax

**Part of:** [Chapter 21: Scripting and Events](./README.md)

---

## Introduction

Scripting allows you to create sequences, cutscenes, and event-driven behaviors without hardcoding everything. This section teaches you the syntax and structure of SuperPascal scripts.

**Key concepts:**
- **Script structure** — How scripts are organized
- **Commands** — Basic script instructions
- **Parameters** — Command arguments
- **Script execution** — How scripts run

---

## What is a Script?

### Understanding Scripts

**Scripts are sequences of commands:**

- **Command-based** — Each line is a command
- **Sequential** — Commands execute in order
- **Non-blocking** — Scripts run alongside game loop
- **Event-driven** — Scripts triggered by events

**Use cases:**
- Cutscenes
- Dialog sequences
- Event triggers
- Animation sequences
- AI behavior

### Script vs Code

**Scripts vs regular code:**

- **Scripts** — Data-driven, interpreted at runtime
- **Code** — Compiled, executed directly

**Example:**
```pascal
// Regular code (compiled)
procedure MovePlayer(x, y: integer);
begin
  EntitySetPosition(player, x, y);
end;

// Script (interpreted)
MOVE player 100 50
```

---

## Script Structure

### Basic Script Format

**Scripts are arrays of commands:**

```pascal
type
  TScriptCommand = record
    Command: byte;      // Command type
    Param1: integer;   // First parameter
    Param2: integer;   // Second parameter
    Param3: integer;   // Third parameter
  end;
  
  TScript = record
    Commands: ^TScriptCommand;
    Count: word;
    CurrentIndex: word;
    Active: boolean;
  end;
```

### Script Example

**Simple script:**

```pascal
const
  CMD_WAIT = 0;
  CMD_MOVE = 1;
  CMD_ANIM = 2;
  CMD_SFX = 3;

var
  CutsceneScript: array[0..4] of TScriptCommand = (
    (Command: CMD_MOVE; Param1: 0; Param2: 100; Param3: 50),  // Move entity 0 to (100, 50)
    (Command: CMD_WAIT; Param1: 60; Param2: 0; Param3: 0),    // Wait 60 frames
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_WAVE; Param3: 0), // Play wave animation
    (Command: CMD_WAIT; Param1: 30; Param2: 0; Param3: 0),    // Wait 30 frames
    (Command: CMD_SFX; Param1: SFX_HELLO; Param2: 0; Param3: 0)  // Play sound
  );
```

---

## Script Commands

### Command Types

**Common script commands:**

```pascal
const
  CMD_NOP = 0;        // No operation
  CMD_WAIT = 1;       // Wait for time or condition
  CMD_MOVE = 2;       // Move entity
  CMD_ANIM = 3;       // Play animation
  CMD_SFX = 4;        // Play sound effect
  CMD_MUSIC = 5;      // Play music
  CMD_TEXT = 6;       // Display text
  CMD_IF = 7;         // Conditional
  CMD_GOTO = 8;       // Jump to command
  CMD_END = 9;        // End script
```

### Command Parameters

**Parameters depend on command:**

- **WAIT** — `Param1` = frames to wait
- **MOVE** — `Param1` = entity ID, `Param2` = X, `Param3` = Y
- **ANIM** — `Param1` = entity ID, `Param2` = animation ID
- **SFX** — `Param1` = sound effect ID

---

## Script Execution

### Running Scripts

**Execute script step by step:**

```pascal
type
  TScriptRunner = record
    Script: ^TScript;
    CurrentIndex: word;
    WaitCounter: word;
    Active: boolean;
  end;

var
  ScriptRunner: TScriptRunner;

procedure InitScriptRunner(script: ^TScript);
begin
  ScriptRunner.Script := script;
  ScriptRunner.CurrentIndex := 0;
  ScriptRunner.WaitCounter := 0;
  ScriptRunner.Active := true;
end;

procedure UpdateScriptRunner;
var
  cmd: ^TScriptCommand;
begin
  if not ScriptRunner.Active then
    Exit;
  
  cmd := @ScriptRunner.Script^.Commands[ScriptRunner.CurrentIndex];
  
  case cmd^.Command of
    CMD_WAIT:
      begin
        ScriptRunner.WaitCounter := ScriptRunner.WaitCounter + 1;
        if ScriptRunner.WaitCounter >= cmd^.Param1 then
        begin
          ScriptRunner.WaitCounter := 0;
          ScriptRunner.CurrentIndex := ScriptRunner.CurrentIndex + 1;
        end;
      end;
      
    CMD_MOVE:
      begin
        EntitySetPosition(cmd^.Param1, cmd^.Param2, cmd^.Param3);
        ScriptRunner.CurrentIndex := ScriptRunner.CurrentIndex + 1;
      end;
      
    CMD_ANIM:
      begin
        EntitySetAnimation(cmd^.Param1, cmd^.Param2);
        ScriptRunner.CurrentIndex := ScriptRunner.CurrentIndex + 1;
      end;
      
    CMD_END:
      begin
        ScriptRunner.Active := false;
      end;
  end;
  
  // Check if script finished
  if ScriptRunner.CurrentIndex >= ScriptRunner.Script^.Count then
    ScriptRunner.Active := false;
end;
```

### Multiple Scripts

**Run multiple scripts simultaneously:**

```pascal
type
  TScriptManager = record
    Runners: array[0..7] of TScriptRunner;
    Count: byte;
  end;

var
  ScriptManager: TScriptManager;

procedure AddScript(script: ^TScript);
begin
  if ScriptManager.Count < 8 then
  begin
    InitScriptRunner(script);
    ScriptManager.Runners[ScriptManager.Count] := ScriptRunner;
    ScriptManager.Count := ScriptManager.Count + 1;
  end;
end;

procedure UpdateAllScripts;
var
  i: byte;
begin
  i := 0;
  while i < ScriptManager.Count do
  begin
    UpdateScriptRunner;
    
    // Remove finished scripts
    if not ScriptRunner.Active then
    begin
      ScriptManager.Runners[i] := ScriptManager.Runners[ScriptManager.Count - 1];
      ScriptManager.Count := ScriptManager.Count - 1;
    end
    else
      i := i + 1;
  end;
end;
```

---

## Script Syntax Examples

### Simple Cutscene

**Basic cutscene script:**

```pascal
const
  Cutscene1: array[0..5] of TScriptCommand = (
    (Command: CMD_MOVE; Param1: 0; Param2: 100; Param3: 50),
    (Command: CMD_WAIT; Param1: 30; Param2: 0; Param3: 0),
    (Command: CMD_TEXT; Param1: 0; Param2: 0; Param3: 0),  // "Hello!"
    (Command: CMD_WAIT; Param1: 60; Param2: 0; Param3: 0),
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_WAVE; Param3: 0),
    (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0)
  );
```

### Dialog Sequence

**Dialog script:**

```pascal
const
  Dialog1: array[0..7] of TScriptCommand = (
    (Command: CMD_TEXT; Param1: 0; Param2: 0; Param3: 0),  // "Welcome!"
    (Command: CMD_SFX; Param1: SFX_DIALOG; Param2: 0; Param3: 0),
    (Command: CMD_WAIT; Param1: 90; Param2: 0; Param3: 0),
    (Command: CMD_TEXT; Param1: 1; Param2: 0; Param3: 0),  // "Thanks!"
    (Command: CMD_SFX; Param1: SFX_DIALOG; Param2: 0; Param3: 0),
    (Command: CMD_WAIT; Param1: 90; Param2: 0; Param3: 0),
    (Command: CMD_TEXT; Param1: 2; Param2: 0; Param3: 0),  // Clear text
    (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0)
  );
```

### Animation Sequence

**Complex animation:**

```pascal
const
  AnimationSeq: array[0..8] of TScriptCommand = (
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_IDLE; Param3: 0),
    (Command: CMD_WAIT; Param1: 30; Param2: 0; Param3: 0),
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_WALK; Param3: 0),
    (Command: CMD_MOVE; Param1: 0; Param2: 150; Param3: 50),
    (Command: CMD_WAIT; Param1: 60; Param2: 0; Param3: 0),
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_JUMP; Param3: 0),
    (Command: CMD_MOVE; Param1: 0; Param2: 150; Param3: 30),
    (Command: CMD_WAIT; Param1: 30; Param2: 0; Param3: 0),
    (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0)
  );
```

---

## Complete Script System Example

**Putting it all together:**

```pascal
program ScriptDemo;

const
  CMD_WAIT = 1;
  CMD_MOVE = 2;
  CMD_ANIM = 3;
  CMD_SFX = 4;
  CMD_END = 9;

type
  TScriptCommand = record
    Command: byte;
    Param1: integer;
    Param2: integer;
    Param3: integer;
  end;
  
  TScript = record
    Commands: ^TScriptCommand;
    Count: word;
  end;
  
  TScriptRunner = record
    Script: ^TScript;
    CurrentIndex: word;
    WaitCounter: word;
    Active: boolean;
  end;

var
  player: TEntityID;
  TestScript: array[0..4] of TScriptCommand;
  ScriptData: TScript;
  Runner: TScriptRunner;

procedure InitScript;
begin
  TestScript[0] := (Command: CMD_MOVE; Param1: 0; Param2: 100; Param3: 50);
  TestScript[1] := (Command: CMD_WAIT; Param1: 60; Param2: 0; Param3: 0);
  TestScript[2] := (Command: CMD_ANIM; Param1: 0; Param2: 1; Param3: 0);
  TestScript[3] := (Command: CMD_WAIT; Param1: 30; Param2: 0; Param3: 0);
  TestScript[4] := (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0);
  
  ScriptData.Commands := @TestScript[0];
  ScriptData.Count := 5;
end;

procedure StartScript(script: ^TScript);
begin
  Runner.Script := script;
  Runner.CurrentIndex := 0;
  Runner.WaitCounter := 0;
  Runner.Active := true;
end;

procedure UpdateScript;
var
  cmd: ^TScriptCommand;
begin
  if not Runner.Active then
    Exit;
  
  cmd := @Runner.Script^.Commands[Runner.CurrentIndex];
  
  case cmd^.Command of
    CMD_WAIT:
      begin
        Runner.WaitCounter := Runner.WaitCounter + 1;
        if Runner.WaitCounter >= cmd^.Param1 then
        begin
          Runner.WaitCounter := 0;
          Runner.CurrentIndex := Runner.CurrentIndex + 1;
        end;
      end;
      
    CMD_MOVE:
      begin
        EntitySetPosition(cmd^.Param1, cmd^.Param2, cmd^.Param3);
        Runner.CurrentIndex := Runner.CurrentIndex + 1;
      end;
      
    CMD_ANIM:
      begin
        EntitySetAnimation(cmd^.Param1, cmd^.Param2);
        Runner.CurrentIndex := Runner.CurrentIndex + 1;
      end;
      
    CMD_END:
      begin
        Runner.Active := false;
      end;
  end;
  
  if Runner.CurrentIndex >= Runner.Script^.Count then
    Runner.Active := false;
end;

begin
  InitGraphics;
  InitScript;
  player := EntityCreate;
  StartScript(@ScriptData);
  
  while true do
  begin
    UpdateScript;
    UpdateGame;
    RenderGame;
    WaitVBlank;
  end;
end.
```

---

## Best Practices

### 1. Use Constants for Commands

**Define command constants:**

```pascal
// ✅ GOOD: Named constants
const
  CMD_WAIT = 1;
  CMD_MOVE = 2;

// ❌ BAD: Magic numbers
cmd.Command := 1;  // What does 1 mean?
```

### 2. Always End Scripts

**Terminate scripts properly:**

```pascal
// ✅ GOOD: Explicit end
(Command: CMD_END; Param1: 0; Param2: 0; Param3: 0)

// ❌ BAD: Script runs past end
// No CMD_END - may cause errors
```

### 3. Validate Parameters

**Check parameters before use:**

```pascal
// ✅ GOOD: Validate
if cmd^.Param1 < MAX_ENTITIES then
  EntitySetPosition(cmd^.Param1, cmd^.Param2, cmd^.Param3);

// ❌ BAD: No validation
EntitySetPosition(cmd^.Param1, cmd^.Param2, cmd^.Param3);  // May crash
```

### 4. Handle Script Errors

**Gracefully handle script errors:**

```pascal
// ✅ GOOD: Error handling
case cmd^.Command of
  CMD_MOVE: EntitySetPosition(...);
  CMD_ANIM: EntitySetAnimation(...);
else
  WriteLn('Unknown command: ', cmd^.Command);
  Runner.Active := false;  // Stop script
end;

// ❌ BAD: No error handling
case cmd^.Command of
  CMD_MOVE: EntitySetPosition(...);
  // Unknown commands cause undefined behavior
end;
```

### 5. Keep Scripts Simple

**One script per sequence:**

```pascal
// ✅ GOOD: Separate scripts
Cutscene1: array[...] of TScriptCommand = (...);
Cutscene2: array[...] of TScriptCommand = (...);

// ❌ BAD: One huge script
AllCutscenes: array[0..999] of TScriptCommand = (...);  // Hard to manage
```

---

## Exercises

### Exercise 1: Basic Script

Write a program that:
1. Defines a simple script
2. Executes script commands
3. Handles WAIT commands
4. Demonstrates script execution

### Exercise 2: Script Runner

Write a program that:
1. Creates a script runner system
2. Executes multiple scripts
3. Manages script lifecycle
4. Handles script completion

### Exercise 3: Script Commands

Write a program that:
1. Implements multiple command types
2. Handles command parameters
3. Validates commands
4. Demonstrates command execution

### Exercise 4: Script System

Write a program that:
1. Creates complete script system
2. Supports multiple scripts
3. Handles errors gracefully
4. Integrates with game

---

**Previous Chapter:** [Chapter 20: Audio Programming](../20_AudioProgramming/README.md)  
**Next Section:** [WAIT/MOVE/ANIM Commands](./02_WaitMoveAnimCommands.md)  
**Language Specification:** See [Game Engine](../../languageSpecification/09_GameEngine.md)  
**Last Updated:** 2025-01-XX

