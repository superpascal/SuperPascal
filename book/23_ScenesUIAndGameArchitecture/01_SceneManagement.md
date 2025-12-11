# Scene Management

**Part of:** [Chapter 23: Scenes, UI, and Game Architecture](./README.md)

---

## Introduction

Scene management organizes your game into separate, manageable sections. This section teaches you how to create scenes, switch between them, and manage game state.

**Key concepts:**
- **Scenes** — Separate game sections
- **Scene switching** — Changing between scenes
- **Entity management** — Entities per scene
- **Scene lifecycle** — Init, update, render, cleanup

---

## Understanding Scenes

### What is a Scene?

**A scene is a container for game content:**

- **Entities** — Game objects in the scene
- **Systems** — Update and render logic
- **State** — Scene-specific data
- **Isolation** — Separate from other scenes

**Common scene types:**
- Main menu
- Game level
- Pause menu
- Options screen
- Cutscene
- Game over

### Scene Class

**Basic scene structure:**

```pascal
type
  TScene = class
  public
    procedure Init; virtual;
    procedure Update; virtual;
    procedure Render; virtual;
    procedure Cleanup; virtual;
  end;
```

**Lifecycle:**
1. `Init` — Initialize scene
2. `Update` — Update scene each frame
3. `Render` — Render scene each frame
4. `Cleanup` — Clean up when leaving scene

---

## Basic Scene Implementation

### Simple Scene

**Minimal scene implementation:**

```pascal
type
  TMenuScene = class(TScene)
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Cleanup; override;
  end;

implementation

procedure TMenuScene.Init;
begin
  WriteLn('Menu scene initialized');
end;

procedure TMenuScene.Update;
begin
  // Handle input, update menu state
end;

procedure TMenuScene.Render;
begin
  // Render menu
  ClearScreen(0);
  // ... render menu items ...
end;

procedure TMenuScene.Cleanup;
begin
  WriteLn('Menu scene cleaned up');
end;
```

### Scene with Entities

**Scene that manages entities:**

```pascal
type
  TGameScene = class(TScene)
  private
    Entities: array[0..255] of TEntityID;
    EntityCount: word;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Cleanup; override;
    
    function AddEntity: TEntityID;
    procedure RemoveEntity(id: TEntityID);
  end;

implementation

procedure TGameScene.Init;
begin
  EntityCount := 0;
  // Initialize scene entities
end;

procedure TGameScene.Update;
var
  i: word;
begin
  // Update all entities
  for i := 0 to EntityCount - 1 do
  begin
    if EntityValid(Entities[i]) then
      UpdateEntity(Entities[i]);
  end;
end;

procedure TGameScene.Render;
var
  i: word;
begin
  ClearScreen(0);
  
  // Render all entities
  for i := 0 to EntityCount - 1 do
  begin
    if EntityValid(Entities[i]) then
      RenderEntity(Entities[i]);
  end;
end;

procedure TGameScene.Cleanup;
var
  i: word;
begin
  // Destroy all entities
  for i := 0 to EntityCount - 1 do
  begin
    if EntityValid(Entities[i]) then
      EntityDestroy(Entities[i]);
  end;
  EntityCount := 0;
end;

function TGameScene.AddEntity: TEntityID;
begin
  if EntityCount < 256 then
  begin
    Entities[EntityCount] := EntityCreate;
    EntityCount := EntityCount + 1;
    AddEntity := Entities[EntityCount - 1];
  end
  else
    AddEntity := ENTITY_NULL;
end;

procedure TGameScene.RemoveEntity(id: TEntityID);
var
  i: word;
begin
  for i := 0 to EntityCount - 1 do
  begin
    if Entities[i] = id then
    begin
      EntityDestroy(id);
      Entities[i] := Entities[EntityCount - 1];
      EntityCount := EntityCount - 1;
      Break;
    end;
  end;
end;
```

---

## Scene Manager

### Scene Manager Class

**Manages scene switching:**

```pascal
type
  TSceneManager = class
  private
    CurrentScene: TScene;
    NextScene: TScene;
    Transitioning: boolean;
  public
    constructor Create;
    procedure Update;
    procedure Render;
    procedure SwitchScene(newScene: TScene);
    procedure Cleanup;
  end;

implementation

constructor TSceneManager.Create;
begin
  CurrentScene := nil;
  NextScene := nil;
  Transitioning := false;
end;

procedure TSceneManager.Update;
begin
  if Transitioning then
  begin
    // Handle transition
    if TransitionComplete then
    begin
      // Switch scenes
      if CurrentScene <> nil then
        CurrentScene.Cleanup;
      
      CurrentScene := NextScene;
      NextScene := nil;
      Transitioning := false;
      
      if CurrentScene <> nil then
        CurrentScene.Init;
    end;
  end
  else
  begin
    // Update current scene
    if CurrentScene <> nil then
      CurrentScene.Update;
  end;
end;

procedure TSceneManager.Render;
begin
  if CurrentScene <> nil then
    CurrentScene.Render;
end;

procedure TSceneManager.SwitchScene(newScene: TScene);
begin
  NextScene := newScene;
  Transitioning := true;
  StartTransition;
end;

procedure TSceneManager.Cleanup;
begin
  if CurrentScene <> nil then
  begin
    CurrentScene.Cleanup;
    CurrentScene.Destroy;
  end;
end;
```

### Using Scene Manager

**Switch between scenes:**

```pascal
var
  SceneManager: TSceneManager;
  MenuScene: TMenuScene;
  GameScene: TGameScene;

begin
  InitGraphics;
  
  SceneManager := TSceneManager.Create;
  MenuScene := TMenuScene.Create;
  GameScene := TGameScene.Create;
  
  // Start with menu
  SceneManager.SwitchScene(MenuScene);
  
  while true do
  begin
    SceneManager.Update;
    SceneManager.Render;
    WaitVBlank;
  end;
  
  SceneManager.Cleanup;
  MenuScene.Destroy;
  GameScene.Destroy;
end;
```

---

## Scene Types

### Menu Scene

**Main menu scene:**

```pascal
type
  TMainMenuScene = class(TScene)
  private
    SelectedIndex: byte;
    MenuItems: array[0..3] of string;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure HandleInput;
  end;

implementation

procedure TMainMenuScene.Init;
begin
  SelectedIndex := 0;
  MenuItems[0] := 'Start Game';
  MenuItems[1] := 'Options';
  MenuItems[2] := 'Credits';
  MenuItems[3] := 'Quit';
end;

procedure TMainMenuScene.Update;
begin
  HandleInput;
end;

procedure TMainMenuScene.HandleInput;
var
  input: word;
begin
  input := ReadInput;
  
  if InputPressed(KEY_UP) then
  begin
    if SelectedIndex > 0 then
      SelectedIndex := SelectedIndex - 1;
  end
  else if InputPressed(KEY_DOWN) then
  begin
    if SelectedIndex < 3 then
      SelectedIndex := SelectedIndex + 1;
  end
  else if InputPressed(KEY_A) then
  begin
    case SelectedIndex of
      0: SceneManager.SwitchScene(GameScene);
      1: SceneManager.SwitchScene(OptionsScene);
      2: SceneManager.SwitchScene(CreditsScene);
      3: QuitGame;
    end;
  end;
end;

procedure TMainMenuScene.Render;
var
  i: byte;
begin
  ClearScreen(0);
  
  for i := 0 to 3 do
  begin
    if i = SelectedIndex then
      DrawText(100, 80 + i * 20, '> ' + MenuItems[i])
    else
      DrawText(100, 80 + i * 20, '  ' + MenuItems[i]);
  end;
end;
```

### Game Scene

**Gameplay scene:**

```pascal
type
  TGameScene = class(TScene)
  private
    Player: TEntityID;
    Enemies: array[0..31] of TEntityID;
    EnemyCount: byte;
    Score: word;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Cleanup; override;
  end;

implementation

procedure TGameScene.Init;
var
  i: byte;
begin
  // Create player
  Player := EntityCreate;
  EntitySetPosition(Player, 160, 200);
  
  // Create enemies
  EnemyCount := 5;
  for i := 0 to EnemyCount - 1 do
  begin
    Enemies[i] := EntityCreate;
    EntitySetPosition(Enemies[i], 50 + i * 60, 50);
  end;
  
  Score := 0;
end;

procedure TGameScene.Update;
var
  input: word;
begin
  input := ReadInput;
  
  // Handle pause
  if InputPressed(KEY_START) then
    SceneManager.SwitchScene(PauseScene);
  
  // Update player
  UpdatePlayer(Player);
  
  // Update enemies
  UpdateEnemies;
  
  // Check collisions
  CheckCollisions;
end;

procedure TGameScene.Render;
begin
  ClearScreen(0);
  
  // Render game
  RenderEntities;
  RenderUI;
end;

procedure TGameScene.Cleanup;
var
  i: byte;
begin
  EntityDestroy(Player);
  for i := 0 to EnemyCount - 1 do
    EntityDestroy(Enemies[i]);
end;
```

### Pause Scene

**Pause menu overlay:**

```pascal
type
  TPauseScene = class(TScene)
  private
    SelectedIndex: byte;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
  end;

implementation

procedure TPauseScene.Init;
begin
  SelectedIndex := 0;
  PauseMusic(true);
end;

procedure TPauseScene.Update;
var
  input: word;
begin
  input := ReadInput;
  
  if InputPressed(KEY_START) or InputPressed(KEY_B) then
  begin
    PauseMusic(false);
    SceneManager.SwitchScene(GameScene);
  end
  else if InputPressed(KEY_A) then
  begin
    case SelectedIndex of
      0: // Resume
        begin
          PauseMusic(false);
          SceneManager.SwitchScene(GameScene);
        end;
      1: // Main Menu
        SceneManager.SwitchScene(MenuScene);
    end;
  end;
end;

procedure TPauseScene.Render;
begin
  // Render game scene behind (if supported)
  // Or render pause overlay
  DrawText(140, 100, 'PAUSED');
  DrawText(140, 120, 'Resume');
  DrawText(140, 140, 'Main Menu');
end;
```

---

## Complete Example

**Putting it all together:**

```pascal
program SceneManagementDemo;

type
  TScene = class
  public
    procedure Init; virtual;
    procedure Update; virtual;
    procedure Render; virtual;
    procedure Cleanup; virtual;
  end;
  
  TSceneManager = class
  private
    CurrentScene: TScene;
  public
    constructor Create;
    procedure Update;
    procedure Render;
    procedure SwitchScene(newScene: TScene);
    procedure Cleanup;
  end;
  
  TMenuScene = class(TScene)
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
  end;
  
  TGameScene = class(TScene)
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Cleanup; override;
  end;

var
  SceneManager: TSceneManager;
  MenuScene: TMenuScene;
  GameScene: TGameScene;

procedure TScene.Init;
begin
  // Base init (empty)
end;

procedure TScene.Update;
begin
  // Base update (empty)
end;

procedure TScene.Render;
begin
  // Base render (empty)
end;

procedure TScene.Cleanup;
begin
  // Base cleanup (empty)
end;

constructor TSceneManager.Create;
begin
  CurrentScene := nil;
end;

procedure TSceneManager.Update;
begin
  if CurrentScene <> nil then
    CurrentScene.Update;
end;

procedure TSceneManager.Render;
begin
  if CurrentScene <> nil then
    CurrentScene.Render;
end;

procedure TSceneManager.SwitchScene(newScene: TScene);
begin
  if CurrentScene <> nil then
    CurrentScene.Cleanup;
  
  CurrentScene := newScene;
  
  if CurrentScene <> nil then
    CurrentScene.Init;
end;

procedure TSceneManager.Cleanup;
begin
  if CurrentScene <> nil then
  begin
    CurrentScene.Cleanup;
    CurrentScene := nil;
  end;
end;

procedure TMenuScene.Init;
begin
  WriteLn('Menu scene initialized');
end;

procedure TMenuScene.Update;
var
  input: word;
begin
  input := ReadInput;
  if InputPressed(KEY_A) then
    SceneManager.SwitchScene(GameScene);
end;

procedure TMenuScene.Render;
begin
  ClearScreen(0);
  DrawText(140, 100, 'MENU - Press A');
end;

procedure TGameScene.Init;
begin
  WriteLn('Game scene initialized');
end;

procedure TGameScene.Update;
var
  input: word;
begin
  input := ReadInput;
  if InputPressed(KEY_B) then
    SceneManager.SwitchScene(MenuScene);
end;

procedure TGameScene.Render;
begin
  ClearScreen(1);
  DrawText(140, 100, 'GAME - Press B');
end;

procedure TGameScene.Cleanup;
begin
  WriteLn('Game scene cleaned up');
end;

begin
  InitGraphics;
  
  SceneManager := TSceneManager.Create;
  MenuScene := TMenuScene.Create;
  GameScene := TGameScene.Create;
  
  SceneManager.SwitchScene(MenuScene);
  
  while true do
  begin
    SceneManager.Update;
    SceneManager.Render;
    WaitVBlank;
  end;
  
  SceneManager.Cleanup;
  MenuScene.Destroy;
  GameScene.Destroy;
end.
```

---

## Best Practices

### 1. Always Clean Up Scenes

**Free resources in Cleanup:**

```pascal
// ✅ GOOD: Clean up resources
procedure TGameScene.Cleanup;
begin
  EntityDestroy(Player);
  StopMusic;
end;

// ❌ BAD: Leak resources
procedure TGameScene.Cleanup;
begin
  // Entities not destroyed
end;
```

### 2. Initialize in Init

**Set up scene in Init:**

```pascal
// ✅ GOOD: Initialize in Init
procedure TGameScene.Init;
begin
  Player := EntityCreate;
  Score := 0;
end;

// ❌ BAD: Initialize elsewhere
procedure TGameScene.Update;
begin
  if not Initialized then
  begin
    Player := EntityCreate;  // Should be in Init
    Initialized := true;
  end;
end;
```

### 3. Use Virtual Methods

**Allow scene customization:**

```pascal
// ✅ GOOD: Virtual methods
type
  TScene = class
    procedure Update; virtual;
  end;

// ❌ BAD: Non-virtual
type
  TScene = class
    procedure Update;  // Can't override
  end;
```

### 4. Isolate Scene State

**Keep scenes independent:**

```pascal
// ✅ GOOD: Isolated state
type
  TGameScene = class(TScene)
  private
    Score: word;  // Scene-specific
  end;

// ❌ BAD: Shared global state
var
  GlobalScore: word;  // Shared across scenes
```

### 5. Handle Scene Switching

**Smooth scene transitions:**

```pascal
// ✅ GOOD: Handle transition
procedure TSceneManager.SwitchScene(newScene: TScene);
begin
  if CurrentScene <> nil then
    CurrentScene.Cleanup;
  CurrentScene := newScene;
  if CurrentScene <> nil then
    CurrentScene.Init;
end;

// ❌ BAD: Abrupt switch
CurrentScene := newScene;  // No cleanup or init
```

---

## Exercises

### Exercise 1: Basic Scene

Write a program that:
1. Defines a simple scene class
2. Implements Init, Update, Render
3. Creates and uses scene
4. Demonstrates scene lifecycle

### Exercise 2: Scene Manager

Write a program that:
1. Creates scene manager
2. Manages multiple scenes
3. Switches between scenes
4. Handles scene lifecycle

### Exercise 3: Multiple Scenes

Write a program that:
1. Creates menu and game scenes
2. Switches between them
3. Manages scene state
4. Demonstrates scene isolation

### Exercise 4: Complete System

Write a program that:
1. Implements complete scene system
2. Manages multiple scene types
3. Handles transitions
4. Integrates with game loop

---

**Previous Chapter:** [Chapter 22: Object-Oriented Programming](../22_ObjectOrientedProgramming/README.md)  
**Next Section:** [Menus and Buttons](./02_MenusAndButtons.md)  
**Language Specification:** See [Game Engine](../../languageSpecification/09_GameEngine.md)  
**Last Updated:** 2025-01-XX

