# Managing Program State

**Part of:** [Chapter 10: Input, State, and Interaction](./README.md)

---

## Introduction

Programs need to track their current state. This section teaches you how to:
- **Understand program state** — What state means in programming
- **Manage state** — Keep track of program state
- **Use state machines** — Organize complex behavior
- **Handle state transitions** — Change between states
- **Apply state patterns** — Common state management patterns

**Key concepts:**
- **State** — Current condition of the program
- **State variables** — Variables that track state
- **State machines** — Formal way to manage states
- **State transitions** — Changing from one state to another

---

## What is Program State?

### Understanding State

**State is the current condition of your program:**

- **What is happening now?** — Current mode, screen, phase
- **What data is active?** — Current values, positions, scores
- **What can happen next?** — Available actions, transitions

**Example:**
```pascal
// Game state
var
  score: word;           // Current score
  lives: byte;           // Remaining lives
  level: byte;           // Current level
  gameMode: TGameMode;   // Current game mode
```

### Why State Matters

**State determines program behavior:**

```pascal
// Different behavior based on state
case gameMode of
  gmMenu:     ShowMenu;
  gmPlaying:  UpdateGame;
  gmPaused:   ShowPauseScreen;
  gmGameOver: ShowGameOverScreen;
end;
```

**State affects:**
- What the program displays
- How input is handled
- What updates occur
- What transitions are possible

---

## Simple State Management

### Using Enumerations

**Enumerations are perfect for state:**

```pascal
type
  TGameState = (gsMenu, gsPlaying, gsPaused, gsGameOver);

var
  gameState: TGameState;

procedure UpdateGame;
begin
  case gameState of
    gsMenu:
      UpdateMenu;
    gsPlaying:
      UpdatePlaying;
    gsPaused:
      UpdatePaused;
    gsGameOver:
      UpdateGameOver;
  end;
end;

procedure RenderGame;
begin
  case gameState of
    gsMenu:
      RenderMenu;
    gsPlaying:
      RenderPlaying;
    gsPaused:
      RenderPaused;
    gsGameOver:
      RenderGameOver;
  end;
end;
```

### State Variables

**Track state with variables:**

```pascal
type
  TPlayerState = (psIdle, psWalking, psJumping, psFalling);
  
var
  playerState: TPlayerState;
  playerX, playerY: integer;
  playerVelocity: integer;
  isGrounded: boolean;
```

### Changing State

**Change state based on conditions:**

```pascal
procedure UpdatePlayer;
begin
  case playerState of
    psIdle:
    begin
      if InputPressed(BTN_A) then
        playerState := psJumping;
      if InputPressed(BTN_LEFT) or InputPressed(BTN_RIGHT) then
        playerState := psWalking;
    end;
    
    psWalking:
    begin
      if not (InputPressed(BTN_LEFT) or InputPressed(BTN_RIGHT)) then
        playerState := psIdle;
      if InputPressed(BTN_A) then
        playerState := psJumping;
    end;
    
    psJumping:
    begin
      playerVelocity := -10;  // Jump up
      playerState := psFalling;
    end;
    
    psFalling:
    begin
      playerVelocity := playerVelocity + 1;  // Gravity
      playerY := playerY + playerVelocity;
      
      if isGrounded then
        playerState := psIdle;
    end;
  end;
end;
```

---

## State Machines

### What is a State Machine?

**A state machine is a formal way to manage states:**

- **States** — Possible conditions
- **Transitions** — Ways to move between states
- **Actions** — What happens in each state
- **Conditions** — When transitions occur

**Visual representation:**
```
[Menu] --(Press Start)--> [Playing] --(Pause)--> [Paused]
  |                           |                        |
  |                           |                        |
  |                    (Game Over)                    |
  |                           |                        |
  |                           v                        |
  |                    [Game Over]                     |
  |                           |                        |
  |                    (Press A)                      |
  |                           |                        |
  +<--------------------------+                        |
                              |                        |
                              +<-----------------------+
```

### Simple State Machine

**Basic state machine implementation:**

```pascal
type
  TGameState = (gsMenu, gsPlaying, gsPaused, gsGameOver);
  
  TStateMachine = record
    CurrentState: TGameState;
    PreviousState: TGameState;
  end;

var
  stateMachine: TStateMachine;

procedure ChangeState(var sm: TStateMachine; newState: TGameState);
begin
  sm.PreviousState := sm.CurrentState;
  sm.CurrentState := newState;
end;

procedure UpdateStateMachine(var sm: TStateMachine);
begin
  case sm.CurrentState of
    gsMenu:
    begin
      UpdateMenu;
      if InputJustPressed(BTN_START) then
        ChangeState(sm, gsPlaying);
    end;
    
    gsPlaying:
    begin
      UpdateGame;
      if InputJustPressed(BTN_START) then
        ChangeState(sm, gsPaused);
      if IsGameOver then
        ChangeState(sm, gsGameOver);
    end;
    
    gsPaused:
    begin
      UpdatePaused;
      if InputJustPressed(BTN_START) then
        ChangeState(sm, gsPlaying);
    end;
    
    gsGameOver:
    begin
      UpdateGameOver;
      if InputJustPressed(BTN_A) then
        ChangeState(sm, gsMenu);
    end;
  end;
end;
```

### State Entry and Exit

**Handle state entry and exit:**

```pascal
procedure EnterState(var sm: TStateMachine; newState: TGameState);
begin
  // Exit current state
  case sm.CurrentState of
    gsMenu: ExitMenuState;
    gsPlaying: ExitPlayingState;
    gsPaused: ExitPausedState;
    gsGameOver: ExitGameOverState;
  end;
  
  // Change state
  sm.PreviousState := sm.CurrentState;
  sm.CurrentState := newState;
  
  // Enter new state
  case newState of
    gsMenu: EnterMenuState;
    gsPlaying: EnterPlayingState;
    gsPaused: EnterPausedState;
    gsGameOver: EnterGameOverState;
  end;
end;
```

---

## Game State Management

### Complete Game State Example

**Full game state management:**

```pascal
type
  TGameState = (gsMenu, gsPlaying, gsPaused, gsGameOver);
  
  TGame = record
    State: TGameState;
    Score: word;
    Lives: byte;
    Level: byte;
    PlayerX, PlayerY: integer;
  end;

var
  game: TGame;

procedure InitGame(var g: TGame);
begin
  g.State := gsMenu;
  g.Score := 0;
  g.Lives := 3;
  g.Level := 1;
  g.PlayerX := 160;
  g.PlayerY := 120;
end;

procedure UpdateGame(var g: TGame);
begin
  case g.State of
    gsMenu:
    begin
      UpdateMenu;
      if InputJustPressed(BTN_START) then
      begin
        g.State := gsPlaying;
        InitLevel(g.Level);
      end;
    end;
    
    gsPlaying:
    begin
      UpdatePlayer(g);
      UpdateEnemies(g);
      UpdateBullets(g);
      CheckCollisions(g);
      
      if InputJustPressed(BTN_START) then
        g.State := gsPaused;
      
      if g.Lives = 0 then
        g.State := gsGameOver;
    end;
    
    gsPaused:
    begin
      UpdatePauseScreen;
      if InputJustPressed(BTN_START) then
        g.State := gsPlaying;
    end;
    
    gsGameOver:
    begin
      UpdateGameOverScreen;
      if InputJustPressed(BTN_A) then
      begin
        InitGame(g);  // Restart game
        g.State := gsMenu;
      end;
    end;
  end;
end;

procedure RenderGame(const g: TGame);
begin
  case g.State of
    gsMenu:
      RenderMenu;
    gsPlaying:
    begin
      RenderBackground;
      RenderPlayer(g.PlayerX, g.PlayerY);
      RenderEnemies;
      RenderHUD(g.Score, g.Lives, g.Level);
    end;
    gsPaused:
    begin
      RenderGame;  // Render game in background
      RenderPauseOverlay;
    end;
    gsGameOver:
      RenderGameOverScreen(g.Score);
  end;
end;
```

### State-Specific Data

**Store data specific to each state:**

```pascal
type
  TMenuState = record
    SelectedItem: byte;
    MenuItems: array[0..2] of string;
  end;
  
  TPlayingState = record
    PlayerX, PlayerY: integer;
    Enemies: array[0..9] of TEnemy;
    Bullets: array[0..19] of TBullet;
  end;
  
  TGameState = (gsMenu, gsPlaying, gsPaused, gsGameOver);
  
  TGame = record
    State: TGameState;
    Menu: TMenuState;
    Playing: TPlayingState;
    Score: word;
    Lives: byte;
  end;
```

---

## State Transition Patterns

### Conditional Transitions

**Transitions based on conditions:**

```pascal
procedure UpdateStateMachine(var sm: TStateMachine);
begin
  case sm.CurrentState of
    gsPlaying:
    begin
      // Multiple transition conditions
      if InputJustPressed(BTN_START) then
        ChangeState(sm, gsPaused)
      else if IsGameOver then
        ChangeState(sm, gsGameOver)
      else if IsLevelComplete then
        ChangeState(sm, gsLevelComplete);
    end;
  end;
end;
```

### Timed Transitions

**Transitions after a delay:**

```pascal
type
  TStateMachine = record
    CurrentState: TGameState;
    StateTimer: word;  // Frames in current state
  end;

procedure UpdateStateMachine(var sm: TStateMachine);
begin
  sm.StateTimer := sm.StateTimer + 1;
  
  case sm.CurrentState of
    gsGameOver:
    begin
      // Auto-return to menu after 5 seconds (300 frames at 60 FPS)
      if sm.StateTimer > 300 then
      begin
        ChangeState(sm, gsMenu);
        sm.StateTimer := 0;
      end;
    end;
  end;
end;

procedure ChangeState(var sm: TStateMachine; newState: TGameState);
begin
  sm.CurrentState := newState;
  sm.StateTimer := 0;  // Reset timer
end;
```

### State Stack

**Push/pop states (for menus, pause screens):**

```pascal
type
  TStateStack = record
    States: array[0..9] of TGameState;
    Top: byte;
  end;

procedure PushState(var stack: TStateStack; state: TGameState);
begin
  if stack.Top < 9 then
  begin
    stack.Top := stack.Top + 1;
    stack.States[stack.Top] := state;
  end;
end;

function PopState(var stack: TStateStack): TGameState;
begin
  if stack.Top > 0 then
  begin
    PopState := stack.States[stack.Top];
    stack.Top := stack.Top - 1;
  end
  else
    PopState := gsMenu;  // Default
end;

// Usage: Pause game
PushState(stateStack, gsPaused);

// Resume game
PopState(stateStack);
```

---

## Common State Patterns

### Menu System

**Menu with sub-menus:**

```pascal
type
  TMenuState = (msMain, msOptions, msControls, msAudio);
  
var
  menuState: TMenuState;
  selectedItem: byte;

procedure UpdateMenu;
begin
  case menuState of
    msMain:
    begin
      // Navigate main menu
      if InputJustPressed(BTN_UP) then
        selectedItem := (selectedItem - 1) mod 3;
      if InputJustPressed(BTN_DOWN) then
        selectedItem := (selectedItem + 1) mod 3;
      
      if InputJustPressed(BTN_A) then
      begin
        case selectedItem of
          0: menuState := msOptions;
          1: menuState := msControls;
          2: menuState := msAudio;
        end;
      end;
    end;
    
    msOptions:
    begin
      // Options menu
      if InputJustPressed(BTN_B) then
        menuState := msMain;  // Back
    end;
    // ... other menus
  end;
end;
```

### Character State Machine

**Player character states:**

```pascal
type
  TPlayerState = (psIdle, psWalking, psRunning, psJumping, psFalling, psAttacking);
  
var
  playerState: TPlayerState;
  stateTimer: word;

procedure UpdatePlayer;
begin
  stateTimer := stateTimer + 1;
  
  case playerState of
    psIdle:
    begin
      if InputPressed(BTN_LEFT) or InputPressed(BTN_RIGHT) then
        playerState := psWalking;
      if InputJustPressed(BTN_A) then
        playerState := psJumping;
      if InputJustPressed(BTN_X) then
        playerState := psAttacking;
    end;
    
    psWalking:
    begin
      if not (InputPressed(BTN_LEFT) or InputPressed(BTN_RIGHT)) then
        playerState := psIdle;
      if InputPressed(BTN_R) then  // Run button
        playerState := psRunning;
      if InputJustPressed(BTN_A) then
        playerState := psJumping;
    end;
    
    psRunning:
    begin
      if not InputPressed(BTN_R) then
        playerState := psWalking;
      if not (InputPressed(BTN_LEFT) or InputPressed(BTN_RIGHT)) then
        playerState := psIdle;
    end;
    
    psJumping:
    begin
      // Jump animation
      if stateTimer > 10 then
        playerState := psFalling;
    end;
    
    psFalling:
    begin
      if IsGrounded then
        playerState := psIdle;
    end;
    
    psAttacking:
    begin
      // Attack animation
      if stateTimer > 20 then
        playerState := psIdle;
    end;
  end;
end;
```

### Game Phase Management

**Game phases (intro, gameplay, outro):**

```pascal
type
  TGamePhase = (gpIntro, gpPlaying, gpLevelComplete, gpGameOver);
  
var
  gamePhase: TGamePhase;
  phaseTimer: word;

procedure UpdateGamePhase;
begin
  phaseTimer := phaseTimer + 1;
  
  case gamePhase of
    gpIntro:
    begin
      ShowIntroAnimation;
      if phaseTimer > 180 then  // 3 seconds
        gamePhase := gpPlaying;
    end;
    
    gpPlaying:
    begin
      UpdateGame;
      if IsLevelComplete then
      begin
        gamePhase := gpLevelComplete;
        phaseTimer := 0;
      end
      else if IsGameOver then
      begin
        gamePhase := gpGameOver;
        phaseTimer := 0;
      end;
    end;
    
    gpLevelComplete:
    begin
      ShowLevelCompleteScreen;
      if phaseTimer > 180 then
        gamePhase := gpPlaying;  // Next level
    end;
    
    gpGameOver:
    begin
      ShowGameOverScreen;
      if InputJustPressed(BTN_A) then
        gamePhase := gpIntro;  // Restart
    end;
  end;
end;
```

---

## Best Practices

### 1. Use Enumerations for States

**Enumerations are type-safe and clear:**

```pascal
// ✅ GOOD
type
  TGameState = (gsMenu, gsPlaying, gsPaused);
var state: TGameState;

// ❌ BAD
var state: byte;  // 0=menu, 1=playing, 2=paused (unclear)
```

### 2. Handle State Entry and Exit

**Initialize and cleanup when changing states:**

```pascal
// ✅ GOOD
procedure EnterState(newState: TGameState);
begin
  ExitCurrentState;
  InitializeNewState(newState);
end;

// ❌ BAD
state := newState;  // No cleanup, may leave resources
```

### 3. Keep State Logic Separate

**Separate update logic for each state:**

```pascal
// ✅ GOOD
case state of
  gsMenu: UpdateMenu;
  gsPlaying: UpdatePlaying;
  gsPaused: UpdatePaused;
end;

// ❌ BAD
if state = gsMenu then UpdateMenu
else if state = gsPlaying then UpdatePlaying
else if state = gsPaused then UpdatePaused;  // Hard to maintain
```

### 4. Use State Machines for Complex Behavior

**State machines make complex behavior manageable:**

```pascal
// ✅ GOOD: Clear state machine
case playerState of
  psIdle: HandleIdle;
  psWalking: HandleWalking;
  psJumping: HandleJumping;
end;

// ❌ BAD: Complex nested conditions
if isGrounded and not moving and not attacking then
  // Idle
else if isGrounded and moving and not running then
  // Walking
else if isGrounded and moving and running then
  // Running
// ... many more conditions
```

### 5. Document State Transitions

**Make state transitions clear:**

```pascal
// ✅ GOOD: Documented transitions
// State: gsPlaying
// Transitions:
//   - BTN_START -> gsPaused
//   - Lives = 0 -> gsGameOver
//   - Level Complete -> gsLevelComplete

// ❌ BAD: Unclear transitions
if someCondition then state := gsPaused;
```

---

## Exercises

### Exercise 1: Simple State Machine

Write a program that:
1. Has three states: Menu, Playing, GameOver
2. Transitions between states based on input
3. Displays current state
4. Handles state entry/exit

### Exercise 2: Player State Machine

Write a program that:
1. Implements a player with states: Idle, Walking, Jumping
2. Transitions based on input
3. Animates player based on state
4. Handles state-specific behavior

### Exercise 3: Menu System

Write a program that:
1. Creates a menu with 3-4 items
2. Navigates menu with arrow keys
3. Selects items with A button
4. Changes to different screens based on selection

### Exercise 4: Game Phases

Write a program that:
1. Has phases: Intro, Playing, Paused, GameOver
2. Transitions between phases
3. Shows different screens for each phase
4. Handles timed transitions (e.g., intro auto-advances)

---

**Previous Section:** [Polling Input](./01_PollingInput.md)  
**Next Chapter:** [Chapter 11: Game Loop and Time-Based Programming](../15_GameLoopAndTimeBasedProgramming/README.md)  
**Language Specification:** See [Type System](../../languageSpecification/03_TypeSystem.md) for enumerations  
**Last Updated:** 2025-01-XX

