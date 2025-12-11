# Trigger Events

**Part of:** [Chapter 21: Scripting and Events](./README.md)

---

## Introduction

Events connect scripts to game actions. This section teaches you how to trigger scripts based on game events, creating interactive and responsive sequences.

**Key concepts:**
- **Event system** — How events work
- **Event triggers** — What causes events
- **Event handlers** — How to respond to events
- **Scripted sequences** — Connecting scripts to events

---

## Understanding Events

### What is an Event?

**An event is something that happens in the game:**

- **Player actions** — Button presses, movement
- **Game state changes** — Level complete, health low
- **Entity interactions** — Collisions, proximity
- **Time-based** — Timers, delays

**Event flow:**
```
Game Action → Event Triggered → Event Handler → Script Executed
```

### Event Types

**Common event types:**

```pascal
type
  TEventType = (
    EVENT_NONE,
    EVENT_INPUT,        // Input pressed
    EVENT_COLLISION,   // Entity collision
    EVENT_PROXIMITY,   // Entity near
    EVENT_TIMER,        // Timer expired
    EVENT_STATE,        // Game state changed
    EVENT_SCRIPT        // Script finished
  );
```

---

## Event System

### Event Structure

**Event data structure:**

```pascal
type
  TGameEvent = record
    EventType: TEventType;
    Source: TEntityID;      // Entity that triggered event
    Target: TEntityID;      // Entity affected (if any)
    Data: integer;          // Event-specific data
  end;
  
  PGameEvent = ^TGameEvent;
```

### Event Queue

**Store events in a queue:**

```pascal
type
  TEventQueue = record
    Events: array[0..31] of TGameEvent;
    Head: byte;
    Tail: byte;
    Count: byte;
  end;

var
  EventQueue: TEventQueue;

procedure EnqueueEvent(event: TGameEvent);
begin
  if EventQueue.Count < 32 then
  begin
    EventQueue.Events[EventQueue.Tail] := event;
    EventQueue.Tail := (EventQueue.Tail + 1) mod 32;
    EventQueue.Count := EventQueue.Count + 1;
  end;
end;

function DequeueEvent(var event: TGameEvent): boolean;
begin
  if EventQueue.Count > 0 then
  begin
    event := EventQueue.Events[EventQueue.Head];
    EventQueue.Head := (EventQueue.Head + 1) mod 32;
    EventQueue.Count := EventQueue.Count - 1;
    DequeueEvent := true;
  end
  else
    DequeueEvent := false;
end;
```

---

## Event Triggers

### Input Events

**Trigger on button press:**

```pascal
procedure CheckInputEvents;
var
  input: word;
  event: TGameEvent;
begin
  input := ReadInput;
  
  if InputPressed(KEY_A) then
  begin
    event.EventType := EVENT_INPUT;
    event.Source := player;
    event.Target := ENTITY_NULL;
    event.Data := KEY_A;
    EnqueueEvent(event);
  end;
end;
```

### Collision Events

**Trigger on entity collision:**

```pascal
procedure CheckCollisionEvents;
var
  i, j: word;
  event: TGameEvent;
begin
  for i := 0 to EntityCount - 1 do
  begin
    for j := i + 1 to EntityCount - 1 do
    begin
      if CollisionCheck(Entities[i], Entities[j]) then
      begin
        event.EventType := EVENT_COLLISION;
        event.Source := Entities[i];
        event.Target := Entities[j];
        event.Data := 0;
        EnqueueEvent(event);
      end;
    end;
  end;
end;
```

### Proximity Events

**Trigger when entity is near:**

```pascal
procedure CheckProximityEvents;
var
  i, j: word;
  x1, y1, x2, y2: integer;
  distance: integer;
  event: TGameEvent;
begin
  for i := 0 to EntityCount - 1 do
  begin
    EntityGetPosition(Entities[i], x1, y1);
    
    for j := 0 to EntityCount - 1 do
    begin
      if i <> j then
      begin
        EntityGetPosition(Entities[j], x2, y2);
        distance := Round(Sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)));
        
        if distance < 50 then  // Within 50 pixels
        begin
          event.EventType := EVENT_PROXIMITY;
          event.Source := Entities[i];
          event.Target := Entities[j];
          event.Data := distance;
          EnqueueEvent(event);
        end;
      end;
    end;
  end;
end;
```

### Timer Events

**Trigger after time delay:**

```pascal
type
  TTimer = record
    Entity: TEntityID;
    Frames: word;
    Remaining: word;
    EventType: TEventType;
  end;

var
  Timers: array[0..15] of TTimer;
  TimerCount: byte;

procedure AddTimer(entity: TEntityID; frames: word; eventType: TEventType);
begin
  if TimerCount < 16 then
  begin
    Timers[TimerCount].Entity := entity;
    Timers[TimerCount].Frames := frames;
    Timers[TimerCount].Remaining := frames;
    Timers[TimerCount].EventType := eventType;
    TimerCount := TimerCount + 1;
  end;
end;

procedure UpdateTimers;
var
  i: byte;
  event: TGameEvent;
begin
  i := 0;
  while i < TimerCount do
  begin
    Timers[i].Remaining := Timers[i].Remaining - 1;
    
    if Timers[i].Remaining = 0 then
    begin
      event.EventType := Timers[i].EventType;
      event.Source := Timers[i].Entity;
      event.Target := ENTITY_NULL;
      event.Data := 0;
      EnqueueEvent(event);
      
      // Remove timer
      Timers[i] := Timers[TimerCount - 1];
      TimerCount := TimerCount - 1;
    end
    else
      i := i + 1;
  end;
end;
```

---

## Event Handlers

### Event Handler System

**Map events to scripts:**

```pascal
type
  TEventHandler = record
    EventType: TEventType;
    Condition: function(event: TGameEvent): boolean;
    Script: ^TScript;
  end;
  
  TEventHandlers = record
    Handlers: array[0..31] of TEventHandler;
    Count: byte;
  end;

var
  EventHandlers: TEventHandlers;

procedure RegisterHandler(eventType: TEventType; 
                          condition: function(event: TGameEvent): boolean;
                          script: ^TScript);
begin
  if EventHandlers.Count < 32 then
  begin
    EventHandlers.Handlers[EventHandlers.Count].EventType := eventType;
    EventHandlers.Handlers[EventHandlers.Count].Condition := condition;
    EventHandlers.Handlers[EventHandlers.Count].Script := script;
    EventHandlers.Count := EventHandlers.Count + 1;
  end;
end;

procedure ProcessEvents;
var
  event: TGameEvent;
  i: byte;
  handler: ^TEventHandler;
begin
  while DequeueEvent(event) do
  begin
    for i := 0 to EventHandlers.Count - 1 do
    begin
      handler := @EventHandlers.Handlers[i];
      
      if (handler^.EventType = event.EventType) and
         (handler^.Condition(event)) then
      begin
        // Execute script
        StartScript(handler^.Script);
        Break;  // Only handle first matching handler
      end;
    end;
  end;
end;
```

### Simple Condition Functions

**Basic condition functions:**

```pascal
function AlwaysTrue(event: TGameEvent): boolean;
begin
  AlwaysTrue := true;
end;

function EntityIsPlayer(event: TGameEvent): boolean;
begin
  EntityIsPlayer := (event.Source = player);
end;

function TargetIsNPC(event: TGameEvent): boolean;
begin
  EntityIsPlayer := IsNPC(event.Target);
end;
```

---

## Practical Examples

### Button Press Script

**Trigger script on button press:**

```pascal
const
  ButtonPressScript: array[0..3] of TScriptCommand = (
    (Command: CMD_SFX; Param1: SFX_BUTTON; Param2: 0; Param3: 0),
    (Command: CMD_TEXT; Param1: 0; Param2: 0; Param3: 0),  // "Button pressed!"
    (Command: CMD_WAIT; Param1: 60; Param2: 0; Param3: 0),
    (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0)
  );

procedure InitButtonHandler;
begin
  RegisterHandler(EVENT_INPUT, AlwaysTrue, @ButtonPressScript);
end;
```

### Collision Script

**Trigger script on collision:**

```pascal
const
  CollisionScript: array[0..4] of TScriptCommand = (
    (Command: CMD_SFX; Param1: SFX_COLLISION; Param2: 0; Param3: 0),
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_HURT; Param3: 0),
    (Command: CMD_WAIT; Param1: 30; Param2: 0; Param3: 0),
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_IDLE; Param3: 0),
    (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0)
  );

function IsPlayerCollision(event: TGameEvent): boolean;
begin
  IsPlayerCollision := (event.Source = player) or (event.Target = player);
end;

procedure InitCollisionHandler;
begin
  RegisterHandler(EVENT_COLLISION, IsPlayerCollision, @CollisionScript);
end;
```

### Proximity Dialog

**Show dialog when near NPC:**

```pascal
const
  ProximityDialog: array[0..5] of TScriptCommand = (
    (Command: CMD_TEXT; Param1: 0; Param2: 0; Param3: 0),  // "Hello!"
    (Command: CMD_SFX; Param1: SFX_DIALOG; Param2: 0; Param3: 0),
    (Command: CMD_WAIT; Param1: 120; Param2: 0; Param3: 0),
    (Command: CMD_ANIM; Param1: 0; Param2: ANIM_WAVE; Param3: 0),
    (Command: CMD_WAIT; Param1: 60; Param2: 0; Param3: 0),
    (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0)
  );

function IsNearNPC(event: TGameEvent): boolean;
begin
  IsNearNPC := IsNPC(event.Target) and (event.Data < 50);
end;

procedure InitProximityHandler;
begin
  RegisterHandler(EVENT_PROXIMITY, IsNearNPC, @ProximityDialog);
end;
```

### Timer-Based Event

**Trigger script after delay:**

```pascal
const
  TimerScript: array[0..3] of TScriptCommand = (
    (Command: CMD_TEXT; Param1: 1; Param2: 0; Param3: 0),  // "Time's up!"
    (Command: CMD_SFX; Param1: SFX_ALERT; Param2: 0; Param3: 0),
    (Command: CMD_WAIT; Param1: 120; Param2: 0; Param3: 0),
    (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0)
  );

procedure StartTimerEvent;
begin
  AddTimer(player, 300, EVENT_TIMER);  // 5 seconds
  RegisterHandler(EVENT_TIMER, AlwaysTrue, @TimerScript);
end;
```

---

## Complete Event System Example

**Putting it all together:**

```pascal
program EventSystemDemo;

const
  CMD_WAIT = 1;
  CMD_MOVE = 2;
  CMD_ANIM = 3;
  CMD_SFX = 4;
  CMD_END = 9;
  
  EVENT_INPUT = 1;
  EVENT_COLLISION = 2;

type
  TEventType = (EVENT_NONE, EVENT_INPUT, EVENT_COLLISION);
  
  TGameEvent = record
    EventType: TEventType;
    Source: TEntityID;
    Target: TEntityID;
    Data: integer;
  end;
  
  TEventQueue = record
    Events: array[0..15] of TGameEvent;
    Head: byte;
    Tail: byte;
    Count: byte;
  end;
  
  TEventHandler = record
    EventType: TEventType;
    Script: ^TScript;
  end;
  
  TEventHandlers = record
    Handlers: array[0..7] of TEventHandler;
    Count: byte;
  end;

var
  EventQueue: TEventQueue;
  EventHandlers: TEventHandlers;
  player: TEntityID;
  InputScript: array[0..3] of TScriptCommand;

procedure EnqueueEvent(event: TGameEvent);
begin
  if EventQueue.Count < 16 then
  begin
    EventQueue.Events[EventQueue.Tail] := event;
    EventQueue.Tail := (EventQueue.Tail + 1) mod 16;
    EventQueue.Count := EventQueue.Count + 1;
  end;
end;

function DequeueEvent(var event: TGameEvent): boolean;
begin
  if EventQueue.Count > 0 then
  begin
    event := EventQueue.Events[EventQueue.Head];
    EventQueue.Head := (EventQueue.Head + 1) mod 16;
    EventQueue.Count := EventQueue.Count - 1;
    DequeueEvent := true;
  end
  else
    DequeueEvent := false;
end;

procedure RegisterHandler(eventType: TEventType; script: ^TScript);
begin
  if EventHandlers.Count < 8 then
  begin
    EventHandlers.Handlers[EventHandlers.Count].EventType := eventType;
    EventHandlers.Handlers[EventHandlers.Count].Script := script;
    EventHandlers.Count := EventHandlers.Count + 1;
  end;
end;

procedure ProcessEvents;
var
  event: TGameEvent;
  i: byte;
begin
  while DequeueEvent(event) do
  begin
    for i := 0 to EventHandlers.Count - 1 do
    begin
      if EventHandlers.Handlers[i].EventType = event.EventType then
      begin
        StartScript(EventHandlers.Handlers[i].Script);
        Break;
      end;
    end;
  end;
end;

procedure CheckInput;
var
  input: word;
  event: TGameEvent;
begin
  input := ReadInput;
  
  if InputPressed(KEY_A) then
  begin
    event.EventType := EVENT_INPUT;
    event.Source := player;
    event.Target := ENTITY_NULL;
    event.Data := KEY_A;
    EnqueueEvent(event);
  end;
end;

begin
  InitGraphics;
  
  // Setup input script
  InputScript[0] := (Command: CMD_SFX; Param1: SFX_BUTTON; Param2: 0; Param3: 0);
  InputScript[1] := (Command: CMD_ANIM; Param1: 0; Param2: ANIM_JUMP; Param3: 0);
  InputScript[2] := (Command: CMD_WAIT; Param1: 30; Param2: 0; Param3: 0);
  InputScript[3] := (Command: CMD_END; Param1: 0; Param2: 0; Param3: 0);
  
  // Register handler
  RegisterHandler(EVENT_INPUT, @InputScript);
  
  player := EntityCreate;
  
  while true do
  begin
    CheckInput;
    ProcessEvents;
    UpdateScripts;
    UpdateGame;
    RenderGame;
    WaitVBlank;
  end;
end.
```

---

## Best Practices

### 1. Limit Event Queue Size

**Prevent event queue overflow:**

```pascal
// ✅ GOOD: Check queue size
if EventQueue.Count < MAX_EVENTS then
  EnqueueEvent(event);

// ❌ BAD: No check
EnqueueEvent(event);  // May overflow
```

### 2. Process Events Every Frame

**Handle events promptly:**

```pascal
// ✅ GOOD: Process every frame
while true do
begin
  ProcessEvents;
  WaitVBlank;
end;

// ❌ BAD: Skip frames
if frameCount mod 2 = 0 then
  ProcessEvents;  // May miss events
```

### 3. Use Specific Conditions

**Match events precisely:**

```pascal
// ✅ GOOD: Specific condition
function IsPlayerCollision(event: TGameEvent): boolean;
begin
  IsPlayerCollision := (event.Source = player);
end;

// ❌ BAD: Too general
RegisterHandler(EVENT_COLLISION, AlwaysTrue, @Script);  // Triggers too often
```

### 4. Clean Up Finished Scripts

**Remove completed scripts:**

```pascal
// ✅ GOOD: Clean up
if not ScriptRunner.Active then
  RemoveScript(ScriptRunner);

// ❌ BAD: Keep finished scripts
// Scripts accumulate in memory
```

### 5. Validate Event Data

**Check event data before use:**

```pascal
// ✅ GOOD: Validate
if EntityValid(event.Source) then
  ProcessEvent(event);

// ❌ BAD: No validation
ProcessEvent(event);  // May use invalid entity
```

---

## Exercises

### Exercise 1: Event System

Write a program that:
1. Implements event queue
2. Enqueues and dequeues events
3. Processes events
4. Demonstrates event flow

### Exercise 2: Input Events

Write a program that:
1. Triggers events on input
2. Handles input events
3. Executes scripts on input
4. Demonstrates input-driven scripts

### Exercise 3: Collision Events

Write a program that:
1. Detects collisions
2. Triggers collision events
3. Handles collision events
4. Executes scripts on collision

### Exercise 4: Complete Event System

Write a program that:
1. Implements complete event system
2. Supports multiple event types
3. Handles events with scripts
4. Integrates with game

---

**Previous Section:** [WAIT/MOVE/ANIM Commands](./02_WaitMoveAnimCommands.md)  
**Next Chapter:** [Chapter 22: Object-Oriented Programming](../22_ObjectOrientedProgramming/README.md)  
**Language Specification:** See [Game Engine](../../languageSpecification/09_GameEngine.md)  
**Last Updated:** 2025-01-XX

