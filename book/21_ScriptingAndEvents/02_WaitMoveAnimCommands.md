# WAIT/MOVE/ANIM Commands

**Part of:** [Chapter 21: Scripting and Events](./README.md)

---

## Introduction

WAIT, MOVE, and ANIM are the core scripting commands for creating sequences. This section teaches you how to use these commands effectively.

**Key concepts:**
- **WAIT command** — Pausing script execution
- **MOVE command** — Moving entities
- **ANIM command** — Playing animations
- **Combining commands** — Creating sequences

---

## WAIT Command

### Basic WAIT

**Wait for a number of frames:**

```pascal
const
  CMD_WAIT = 1;

// Wait 60 frames (1 second at 60 FPS)
(Command: CMD_WAIT; Param1: 60; Param2: 0; Param3: 0)
```

**Parameters:**
- `Param1` — Number of frames to wait
- `Param2` — Reserved (0)
- `Param3` — Reserved (0)

**Implementation:**
```pascal
CMD_WAIT:
  begin
    Runner.WaitCounter := Runner.WaitCounter + 1;
    if Runner.WaitCounter >= cmd^.Param1 then
    begin
      Runner.WaitCounter := 0;
      Runner.CurrentIndex := Runner.CurrentIndex + 1;
    end;
  end;
```

### Wait Examples

**Common wait durations:**

```pascal
// Wait 1 second (60 frames at 60 FPS)
(Command: CMD_WAIT; Param1: 60; Param2: 0; Param3: 0)

// Wait 0.5 seconds (30 frames)
(Command: CMD_WAIT; Param1: 30; Param2: 0; Param3: 0)

// Wait 2 seconds (120 frames)
(Command: CMD_WAIT; Param1: 120; Param2: 0; Param3: 0)
```

### Conditional Wait

**Wait for condition (advanced):**

```pascal
// Wait until entity reaches position
CMD_WAIT_UNTIL:
  begin
    var x, y: integer;
    EntityGetPosition(cmd^.Param1, x, y);
    
    if (x = cmd^.Param2) and (y = cmd^.Param3) then
      Runner.CurrentIndex := Runner.CurrentIndex + 1;
    // Otherwise, keep waiting
  end;
```

---

## MOVE Command

### Basic MOVE

**Move entity to position:**

```pascal
const
  CMD_MOVE = 2;

// Move entity 0 to (100, 50)
(Command: CMD_MOVE; Param1: 0; Param2: 100; Param3: 50)
```

**Parameters:**
- `Param1` — Entity ID
- `Param2` — Target X position
- `Param3` — Target Y position

**Implementation:**
```pascal
CMD_MOVE:
  begin
    EntitySetPosition(cmd^.Param1, cmd^.Param2, cmd^.Param3);
    Runner.CurrentIndex := Runner.CurrentIndex + 1;
  end;
```

### Instant vs Smooth Movement

**Instant movement (current):**
```pascal
// Entity immediately appears at position
EntitySetPosition(entity, x, y);
```

**Smooth movement (advanced):**
```pascal
// Move entity smoothly over time
CMD_MOVE_SMOOTH:
  begin
    // Set velocity toward target
    var dx := cmd^.Param2 - GetEntityX(cmd^.Param1);
    var dy := cmd^.Param3 - GetEntityY(cmd^.Param1);
    var speed := cmd^.Param4;  // Speed parameter
    
    EntitySetVelocity(cmd^.Param1, dx * speed, dy * speed);
    
    // Wait until reached (or use WAIT_UNTIL)
    Runner.CurrentIndex := Runner.CurrentIndex + 1;
  end;
```

### Move Examples

**Common movement patterns:**

```pascal
// Move to center
(Command: CMD_MOVE; Param1: 0; Param2: 160; Param3: 120)

// Move to left side
(Command: CMD_MOVE; Param1: 0; Param2: 0; Param3: 120)

// Move to right side
(Command: CMD_MOVE; Param1: 0; Param2: 320; Param3: 120)

// Move off screen
(Command: CMD_MOVE; Param1: 0; Param2: -100; Param3: 120)
```

---

## ANIM Command

### Basic ANIM

**Play animation:**

```pascal
const
  CMD_ANIM = 3;

// Play animation 1 on entity 0
(Command: CMD_ANIM; Param1: 0; Param2: 1; Param3: 0)
```

**Parameters:**
- `Param1` — Entity ID
- `Param2` — Animation ID
- `Param3` — Reserved (0) or loop flag

**Implementation:**
```pascal
CMD_ANIM:
  begin
    EntitySetAnimation(cmd^.Param1, cmd^.Param2);
    Runner.CurrentIndex := Runner.CurrentIndex + 1;
  end;
```

### Animation States

**Common animation IDs:**

```pascal
const
  ANIM_IDLE = 0;
  ANIM_WALK = 1;
  ANIM_RUN = 2;
  ANIM_JUMP = 3;
  ANIM_FALL = 4;
  ANIM_ATTACK = 5;
  ANIM_HURT = 6;
  ANIM_DEATH = 7;
```

### Wait for Animation

**Wait until animation completes:**

```pascal
// Wait for animation to finish
CMD_ANIM_WAIT:
  begin
    // Set animation
    EntitySetAnimation(cmd^.Param1, cmd^.Param2);
    
    // Wait until animation complete
    Runner.WaitCounter := 0;
    Runner.WaitingForAnimation := true;
    Runner.WaitingEntity := cmd^.Param1;
  end;

// In update loop
if Runner.WaitingForAnimation then
begin
  if IsAnimationComplete(Runner.WaitingEntity) then
  begin
    Runner.WaitingForAnimation := false;
    Runner.CurrentIndex := Runner.CurrentIndex + 1;
  end;
end;
```

---

## Combining Commands

### Simple Sequence

**Basic command sequence:**

```pascal
const
  SimpleSequence: array[0..4] of TScriptCommand = (
    (Command: CMD_MOVE; Param1: 0; Param2: 100; Param3: 50),
    (Command: CMD_WAIT; Param1: 30; Param2: 0; Param3: 0),
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_WAVE; Param3: 0),
    (Command: CMD_WAIT; Param1: 60; Param2: 0; Param3: 0),
    (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0)
  );
```

**What happens:**
1. Entity moves to (100, 50)
2. Waits 0.5 seconds
3. Plays wave animation
4. Waits 1 second
5. Script ends

### Walk Sequence

**Entity walks to position:**

```pascal
const
  WalkSequence: array[0..6] of TScriptCommand = (
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_WALK; Param3: 0),  // Start walking
    (Command: CMD_MOVE; Param1: 0; Param2: 200; Param3: 100),      // Move to target
    (Command: CMD_WAIT; Param1: 120; Param2: 0; Param3: 0),        // Wait for movement
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_IDLE; Param3: 0),  // Stop walking
    (Command: CMD_WAIT; Param1: 30; Param2: 0; Param3: 0),          // Brief pause
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_WAVE; Param3: 0),  // Wave
    (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0)
  );
```

### Complex Sequence

**Multiple entities, multiple actions:**

```pascal
const
  ComplexSequence: array[0..11] of TScriptCommand = (
    // Entity 0 walks in
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_WALK; Param3: 0),
    (Command: CMD_MOVE; Param1: 0; Param2: 160; Param3: 100),
    (Command: CMD_WAIT; Param1: 90; Param2: 0; Param3: 0),
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_IDLE; Param3: 0),
    
    // Entity 1 walks in
    (Command: CMD_ANIM; Param1: 1; Param2: ANIM_WALK; Param3: 0),
    (Command: CMD_MOVE; Param1: 1; Param2: 180; Param3: 100),
    (Command: CMD_WAIT; Param1: 90; Param2: 0; Param3: 0),
    (Command: CMD_ANIM; Param1: 1; Param2: ANIM_IDLE; Param3: 0),
    
    // Both wave
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_WAVE; Param3: 0),
    (Command: CMD_ANIM; Param1: 1; Param2: ANIM_WAVE; Param3: 0),
    (Command: CMD_WAIT; Param1: 60; Param2: 0; Param3: 0),
    (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0)
  );
```

---

## Practical Examples

### Cutscene Entry

**Character enters scene:**

```pascal
const
  EntryCutscene: array[0..5] of TScriptCommand = (
    (Command: CMD_MOVE; Param1: 0; Param2: -50; Param3: 100),  // Start off-screen
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_WALK; Param3: 0),
    (Command: CMD_MOVE; Param1: 0; Param2: 160; Param3: 100),   // Walk to center
    (Command: CMD_WAIT; Param1: 120; Param2: 0; Param3: 0),      // Wait for walk
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_IDLE; Param3: 0), // Stop
    (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0)
  );
```

### Dialog Animation

**Animate during dialog:**

```pascal
const
  DialogAnim: array[0..7] of TScriptCommand = (
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_IDLE; Param3: 0),
    (Command: CMD_WAIT; Param1: 30; Param2: 0; Param3: 0),
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_TALK; Param3: 0),  // Talking animation
    (Command: CMD_WAIT; Param1: 180; Param2: 0; Param3: 0),        // Dialog duration
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_IDLE; Param3: 0),  // Back to idle
    (Command: CMD_WAIT; Param1: 30; Param2: 0; Param3: 0),
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_WAVE; Param3: 0),  // Wave goodbye
    (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0)
  );
```

### Exit Sequence

**Character exits scene:**

```pascal
const
  ExitSequence: array[0..4] of TScriptCommand = (
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_WALK; Param3: 0),
    (Command: CMD_MOVE; Param1: 0; Param2: 400; Param3: 100),   // Walk off-screen
    (Command: CMD_WAIT; Param1: 120; Param2: 0; Param3: 0),
    (Command: CMD_MOVE; Param1: 0; Param2: -100; Param3: 100), // Hide entity
    (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0)
  );
```

---

## Complete Example

**Full script system with WAIT/MOVE/ANIM:**

```pascal
program ScriptCommandsDemo;

const
  CMD_WAIT = 1;
  CMD_MOVE = 2;
  CMD_ANIM = 3;
  CMD_END = 9;
  
  ANIM_IDLE = 0;
  ANIM_WALK = 1;
  ANIM_WAVE = 2;

type
  TScriptCommand = record
    Command: byte;
    Param1: integer;
    Param2: integer;
    Param3: integer;
  end;
  
  TScriptRunner = record
    Commands: ^TScriptCommand;
    Count: word;
    CurrentIndex: word;
    WaitCounter: word;
    Active: boolean;
  end;

var
  player: TEntityID;
  Cutscene: array[0..6] of TScriptCommand;
  Runner: TScriptRunner;

procedure InitCutscene;
begin
  Cutscene[0] := (Command: CMD_MOVE; Param1: 0; Param2: -50; Param3: 100);
  Cutscene[1] := (Command: CMD_ANIM; Param1: 0; Param2: ANIM_WALK; Param3: 0);
  Cutscene[2] := (Command: CMD_MOVE; Param1: 0; Param2: 160; Param3: 100);
  Cutscene[3] := (Command: CMD_WAIT; Param1: 120; Param2: 0; Param3: 0);
  Cutscene[4] := (Command: CMD_ANIM; Param1: 0; Param2: ANIM_IDLE; Param3: 0);
  Cutscene[5] := (Command: CMD_WAIT; Param1: 30; Param2: 0; Param3: 0);
  Cutscene[6] := (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0);
end;

procedure StartScript(commands: ^TScriptCommand; count: word);
begin
  Runner.Commands := commands;
  Runner.Count := count;
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
  
  cmd := @Runner.Commands[Runner.CurrentIndex];
  
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
  
  if Runner.CurrentIndex >= Runner.Count then
    Runner.Active := false;
end;

begin
  InitGraphics;
  InitCutscene;
  player := EntityCreate;
  StartScript(@Cutscene[0], 7);
  
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

### 1. Use Appropriate Wait Times

**Match wait times to actions:**

```pascal
// ✅ GOOD: Wait matches action duration
(Command: CMD_MOVE; Param1: 0; Param2: 200; Param3: 100),
(Command: CMD_WAIT; Param1: 120; Param2: 0; Param3: 0),  // Matches walk time

// ❌ BAD: Wait too short or too long
(Command: CMD_MOVE; Param1: 0; Param2: 200; Param3: 100),
(Command: CMD_WAIT; Param1: 10; Param2: 0; Param3: 0),  // Too short
```

### 2. Set Animation Before Movement

**Start animation before moving:**

```pascal
// ✅ GOOD: Animation before movement
(Command: CMD_ANIM; Param1: 0; Param2: ANIM_WALK; Param3: 0),
(Command: CMD_MOVE; Param1: 0; Param2: 200; Param3: 100),

// ❌ BAD: Movement before animation
(Command: CMD_MOVE; Param1: 0; Param2: 200; Param3: 100),
(Command: CMD_ANIM; Param1: 0; Param2: ANIM_WALK; Param3: 0),  // Looks wrong
```

### 3. Wait After Animations

**Give animations time to play:**

```pascal
// ✅ GOOD: Wait after animation
(Command: CMD_ANIM; Param1: 0; Param2: ANIM_WAVE; Param3: 0),
(Command: CMD_WAIT; Param1: 60; Param2: 0; Param3: 0),

// ❌ BAD: No wait
(Command: CMD_ANIM; Param1: 0; Param2: ANIM_WAVE; Param3: 0),
(Command: CMD_END; Param1: 0; Param2: 0; Param3: 0),  // Animation cut short
```

### 4. Validate Entity IDs

**Check entity exists before commands:**

```pascal
// ✅ GOOD: Validate
if EntityValid(cmd^.Param1) then
  EntitySetPosition(cmd^.Param1, cmd^.Param2, cmd^.Param3);

// ❌ BAD: No validation
EntitySetPosition(cmd^.Param1, cmd^.Param2, cmd^.Param3);  // May crash
```

### 5. Use Constants for Animation IDs

**Define animation constants:**

```pascal
// ✅ GOOD: Named constants
const
  ANIM_IDLE = 0;
  ANIM_WALK = 1;
(Command: CMD_ANIM; Param1: 0; Param2: ANIM_WALK; Param3: 0),

// ❌ BAD: Magic numbers
(Command: CMD_ANIM; Param1: 0; Param2: 1; Param3: 0),  // What is 1?
```

---

## Exercises

### Exercise 1: WAIT Command

Write a program that:
1. Implements WAIT command
2. Waits for different durations
3. Demonstrates timing control
4. Shows script pausing

### Exercise 2: MOVE Command

Write a program that:
1. Implements MOVE command
2. Moves entities to positions
3. Creates movement sequences
4. Demonstrates entity positioning

### Exercise 3: ANIM Command

Write a program that:
1. Implements ANIM command
2. Plays different animations
3. Sequences animations
4. Demonstrates animation control

### Exercise 4: Combined Sequence

Write a program that:
1. Combines WAIT, MOVE, ANIM
2. Creates complex sequences
3. Implements cutscene
4. Demonstrates script flow

---

**Previous Section:** [Script Syntax](./01_ScriptSyntax.md)  
**Next Section:** [Trigger Events](./03_TriggerEvents.md)  
**Language Specification:** See [Game Engine](../../languageSpecification/09_GameEngine.md)  
**Last Updated:** 2025-01-XX

