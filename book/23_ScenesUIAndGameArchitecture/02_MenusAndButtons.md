# Menus and Buttons

**Part of:** [Chapter 23: Scenes, UI, and Game Architecture](./README.md)

---

## Introduction

Menus and buttons provide user interaction in games. This section teaches you how to create button components, build menu systems, and handle UI navigation.

**Key concepts:**
- **Buttons** — Clickable UI elements
- **Menus** — Collections of buttons
- **Navigation** — Moving between menu items
- **UI rendering** — Drawing buttons and menus

---

## Understanding Buttons

### What is a Button?

**A button is a clickable UI element:**

- **Position** — X, Y coordinates
- **Size** — Width and height
- **Text** — Button label
- **State** — Normal, hover, pressed
- **Action** — What happens when clicked

### Basic Button Class

**Simple button implementation:**

```pascal
type
  TButton = class
  public
    X, Y: integer;
    Width, Height: integer;
    Text: string;
    Pressed: boolean;
    
    constructor Create(x, y, w, h: integer; text: string);
    function IsClicked(mx, my: integer): boolean;
    procedure Render;
  end;

implementation

constructor TButton.Create(x, y, w, h: integer; text: string);
begin
  X := x;
  Y := y;
  Width := w;
  Height := h;
  Text := text;
  Pressed := false;
end;

function TButton.IsClicked(mx, my: integer): boolean;
begin
  IsClicked := (mx >= X) and (mx < X + Width) and
               (my >= Y) and (my < Y + Height);
end;

procedure TButton.Render;
begin
  if Pressed then
    DrawRect(X, Y, Width, Height, 2)  // Pressed color
  else
    DrawRect(X, Y, Width, Height, 1);  // Normal color
  
  DrawText(X + 4, Y + 4, Text);
end;
```

---

## Button States

### Button State Management

**Track button state:**

```pascal
type
  TButtonState = (bsNormal, bsHover, bsPressed);

type
  TButton = class
  public
    X, Y: integer;
    Width, Height: integer;
    Text: string;
    State: TButtonState;
    
    procedure Update(mx, my: integer; clicked: boolean);
    procedure Render;
  end;

implementation

procedure TButton.Update(mx, my: integer; clicked: boolean);
var
  isInside: boolean;
begin
  isInside := (mx >= X) and (mx < X + Width) and
              (my >= Y) and (my < Y + Height);
  
  if clicked and isInside then
    State := bsPressed
  else if isInside then
    State := bsHover
  else
    State := bsNormal;
end;

procedure TButton.Render;
begin
  case State of
    bsNormal: DrawRect(X, Y, Width, Height, 1);
    bsHover: DrawRect(X, Y, Width, Height, 2);
    bsPressed: DrawRect(X, Y, Width, Height, 3);
  end;
  
  DrawText(X + 4, Y + 4, Text);
end;
```

### Button Click Detection

**Detect button clicks:**

```pascal
procedure TButton.HandleInput;
var
  input: word;
  mx, my: integer;
begin
  input := ReadInput;
  
  // Get mouse position (or use touch/pointer)
  GetMousePosition(mx, my);
  
  // Check if clicked
  if InputPressed(KEY_A) or InputPressed(MOUSE_LEFT) then
  begin
    if IsClicked(mx, my) then
    begin
      OnClick;  // Trigger button action
    end;
  end;
end;
```

---

## Menu System

### Basic Menu

**Menu with multiple buttons:**

```pascal
type
  TMenu = class
  private
    Buttons: array[0..7] of TButton;
    ButtonCount: byte;
    SelectedIndex: byte;
  public
    constructor Create;
    procedure AddButton(button: TButton);
    procedure Update;
    procedure Render;
    function GetSelectedAction: string;
  end;

implementation

constructor TMenu.Create;
begin
  ButtonCount := 0;
  SelectedIndex := 0;
end;

procedure TMenu.AddButton(button: TButton);
begin
  if ButtonCount < 8 then
  begin
    Buttons[ButtonCount] := button;
    ButtonCount := ButtonCount + 1;
  end;
end;

procedure TMenu.Update;
var
  input: word;
begin
  input := ReadInput;
  
  // Navigate menu
  if InputPressed(KEY_UP) then
  begin
    if SelectedIndex > 0 then
      SelectedIndex := SelectedIndex - 1;
  end
  else if InputPressed(KEY_DOWN) then
  begin
    if SelectedIndex < ButtonCount - 1 then
      SelectedIndex := SelectedIndex + 1;
  end
  else if InputPressed(KEY_A) then
  begin
    // Activate selected button
    Buttons[SelectedIndex].OnClick;
  end;
end;

procedure TMenu.Render;
var
  i: byte;
begin
  for i := 0 to ButtonCount - 1 do
  begin
    if i = SelectedIndex then
      Buttons[i].State := bsHover
    else
      Buttons[i].State := bsNormal;
    
    Buttons[i].Render;
  end;
end;

function TMenu.GetSelectedAction: string;
begin
  if SelectedIndex < ButtonCount then
    GetSelectedAction := Buttons[SelectedIndex].Text
  else
    GetSelectedAction := '';
end;
```

### Menu with Actions

**Menu that performs actions:**

```pascal
type
  TMenuAction = (maNone, maStartGame, maOptions, maCredits, maQuit);

type
  TMenu = class
  private
    Buttons: array[0..7] of TButton;
    Actions: array[0..7] of TMenuAction;
    ButtonCount: byte;
    SelectedIndex: byte;
  public
    procedure AddButton(button: TButton; action: TMenuAction);
    function GetSelectedAction: TMenuAction;
  end;

implementation

procedure TMenu.AddButton(button: TButton; action: TMenuAction);
begin
  if ButtonCount < 8 then
  begin
    Buttons[ButtonCount] := button;
    Actions[ButtonCount] := action;
    ButtonCount := ButtonCount + 1;
  end;
end;

function TMenu.GetSelectedAction: TMenuAction;
begin
  if SelectedIndex < ButtonCount then
    GetSelectedAction := Actions[SelectedIndex]
  else
    GetSelectedAction := maNone;
end;

// Usage
procedure HandleMenuAction(menu: TMenu);
var
  action: TMenuAction;
begin
  action := menu.GetSelectedAction;
  
  case action of
    maStartGame: SceneManager.SwitchScene(GameScene);
    maOptions: SceneManager.SwitchScene(OptionsScene);
    maCredits: SceneManager.SwitchScene(CreditsScene);
    maQuit: QuitGame;
  end;
end;
```

---

## UI Component System

### UI Component Base Class

**Base class for UI components:**

```pascal
type
  TUIComponent = class
  public
    X, Y: integer;
    Visible: boolean;
    
    procedure Update; virtual;
    procedure Render; virtual;
    function Contains(mx, my: integer): boolean; virtual;
  end;
  
  TButton = class(TUIComponent)
  public
    Width, Height: integer;
    Text: string;
    OnClick: procedure;
    
    function Contains(mx, my: integer): override;
    procedure Render; override;
  end;
  
  TLabel = class(TUIComponent)
  public
    Text: string;
    Color: byte;
    
    procedure Render; override;
  end;
  
  TPanel = class(TUIComponent)
  private
    Components: array[0..15] of TUIComponent;
    Count: byte;
  public
    Width, Height: integer;
    
    procedure Add(component: TUIComponent);
    procedure Update; override;
    procedure Render; override;
  end;
```

### UI Manager

**Manages all UI components:**

```pascal
type
  TUIManager = class
  private
    Components: array[0..63] of TUIComponent;
    Count: word;
  public
    constructor Create;
    procedure Add(component: TUIComponent);
    procedure Update;
    procedure Render;
    procedure Clear;
  end;

implementation

constructor TUIManager.Create;
begin
  Count := 0;
end;

procedure TUIManager.Add(component: TUIComponent);
begin
  if Count < 64 then
  begin
    Components[Count] := component;
    Count := Count + 1;
  end;
end;

procedure TUIManager.Update;
var
  i: word;
begin
  for i := 0 to Count - 1 do
  begin
    if Components[i] <> nil then
      Components[i].Update;
  end;
end;

procedure TUIManager.Render;
var
  i: word;
begin
  for i := 0 to Count - 1 do
  begin
    if (Components[i] <> nil) and Components[i].Visible then
      Components[i].Render;
  end;
end;

procedure TUIManager.Clear;
var
  i: word;
begin
  for i := 0 to Count - 1 do
  begin
    if Components[i] <> nil then
    begin
      Components[i].Destroy;
      Components[i] := nil;
    end;
  end;
  Count := 0;
end;
```

---

## Practical Examples

### Main Menu

**Complete main menu:**

```pascal
type
  TMainMenuScene = class(TScene)
  private
    Menu: TMenu;
    TitleLabel: TLabel;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
    procedure Cleanup; override;
  end;

implementation

procedure TMainMenuScene.Init;
var
  startBtn, optionsBtn, quitBtn: TButton;
begin
  TitleLabel := TLabel.Create(100, 50, 'MY GAME', 3);
  
  startBtn := TButton.Create(100, 100, 120, 30, 'Start Game');
  optionsBtn := TButton.Create(100, 140, 120, 30, 'Options');
  quitBtn := TButton.Create(100, 180, 120, 30, 'Quit');
  
  Menu := TMenu.Create;
  Menu.AddButton(startBtn, maStartGame);
  Menu.AddButton(optionsBtn, maOptions);
  Menu.AddButton(quitBtn, maQuit);
end;

procedure TMainMenuScene.Update;
var
  action: TMenuAction;
begin
  Menu.Update;
  
  if InputPressed(KEY_A) then
  begin
    action := Menu.GetSelectedAction;
    case action of
      maStartGame: SceneManager.SwitchScene(GameScene);
      maOptions: SceneManager.SwitchScene(OptionsScene);
      maQuit: QuitGame;
    end;
  end;
end;

procedure TMainMenuScene.Render;
begin
  ClearScreen(0);
  TitleLabel.Render;
  Menu.Render;
end;

procedure TMainMenuScene.Cleanup;
begin
  Menu.Destroy;
  TitleLabel.Destroy;
end;
```

### Pause Menu

**In-game pause menu:**

```pascal
type
  TPauseMenu = class(TMenu)
  public
    procedure Init;
  end;

implementation

procedure TPauseMenu.Init;
var
  resumeBtn, menuBtn: TButton;
begin
  resumeBtn := TButton.Create(100, 100, 120, 30, 'Resume');
  menuBtn := TButton.Create(100, 140, 120, 30, 'Main Menu');
  
  AddButton(resumeBtn, maResume);
  AddButton(menuBtn, maMainMenu);
end;

// In pause scene
procedure TPauseScene.Update;
var
  action: TMenuAction;
begin
  Menu.Update;
  
  if InputPressed(KEY_START) or InputPressed(KEY_B) then
    SceneManager.SwitchScene(GameScene)
  else if InputPressed(KEY_A) then
  begin
    action := Menu.GetSelectedAction;
    case action of
      maResume: SceneManager.SwitchScene(GameScene);
      maMainMenu: SceneManager.SwitchScene(MenuScene);
    end;
  end;
end;
```

### Options Menu

**Settings menu:**

```pascal
type
  TOptionsMenu = class(TMenu)
  private
    VolumeLabel: TLabel;
    VolumeValue: byte;
  public
    procedure Init; override;
    procedure Update; override;
    procedure Render; override;
  end;

implementation

procedure TOptionsMenu.Init;
var
  volUpBtn, volDownBtn, backBtn: TButton;
begin
  VolumeLabel := TLabel.Create(100, 80, 'Volume: ', 1);
  VolumeValue := GetMasterVolume;
  
  volUpBtn := TButton.Create(100, 120, 60, 30, '+');
  volDownBtn := TButton.Create(170, 120, 60, 30, '-');
  backBtn := TButton.Create(100, 160, 120, 30, 'Back');
  
  AddButton(volUpBtn, maVolumeUp);
  AddButton(volDownBtn, maVolumeDown);
  AddButton(backBtn, maBack);
end;

procedure TOptionsMenu.Update;
var
  action: TMenuAction;
begin
  inherited Update;
  
  if InputPressed(KEY_A) then
  begin
    action := GetSelectedAction;
    case action of
      maVolumeUp:
        begin
          if VolumeValue < 255 then
            VolumeValue := VolumeValue + 16;
          SetMasterVolume(VolumeValue);
        end;
      maVolumeDown:
        begin
          if VolumeValue > 0 then
            VolumeValue := VolumeValue - 16;
          SetMasterVolume(VolumeValue);
        end;
      maBack: SceneManager.SwitchScene(MenuScene);
    end;
  end;
end;

procedure TOptionsMenu.Render;
begin
  inherited Render;
  VolumeLabel.Render;
  DrawText(180, 80, IntToStr(VolumeValue));
end;
```

---

## Complete Example

**Putting it all together:**

```pascal
program MenuSystemDemo;

type
  TButton = class
  public
    X, Y, Width, Height: integer;
    Text: string;
    Pressed: boolean;
    
    constructor Create(x, y, w, h: integer; text: string);
    function IsClicked(mx, my: integer): boolean;
    procedure Render;
  end;
  
  TMenu = class
  private
    Buttons: array[0..7] of TButton;
    Count: byte;
    Selected: byte;
  public
    constructor Create;
    procedure AddButton(button: TButton);
    procedure Update;
    procedure Render;
  end;

var
  Menu: TMenu;
  StartBtn, OptionsBtn, QuitBtn: TButton;

constructor TButton.Create(x, y, w, h: integer; text: string);
begin
  X := x;
  Y := y;
  Width := w;
  Height := h;
  Text := text;
  Pressed := false;
end;

function TButton.IsClicked(mx, my: integer): boolean;
begin
  IsClicked := (mx >= X) and (mx < X + Width) and
               (my >= Y) and (my < Y + Height);
end;

procedure TButton.Render;
begin
  if Pressed then
    DrawRect(X, Y, Width, Height, 2)
  else
    DrawRect(X, Y, Width, Height, 1);
  DrawText(X + 4, Y + 4, Text);
end;

constructor TMenu.Create;
begin
  Count := 0;
  Selected := 0;
end;

procedure TMenu.AddButton(button: TButton);
begin
  if Count < 8 then
  begin
    Buttons[Count] := button;
    Count := Count + 1;
  end;
end;

procedure TMenu.Update;
var
  input: word;
begin
  input := ReadInput;
  
  if InputPressed(KEY_UP) and (Selected > 0) then
    Selected := Selected - 1
  else if InputPressed(KEY_DOWN) and (Selected < Count - 1) then
    Selected := Selected + 1;
end;

procedure TMenu.Render;
var
  i: byte;
begin
  for i := 0 to Count - 1 do
  begin
    Buttons[i].Pressed := (i = Selected);
    Buttons[i].Render;
  end;
end;

begin
  InitGraphics;
  
  Menu := TMenu.Create;
  StartBtn := TButton.Create(100, 100, 120, 30, 'Start');
  OptionsBtn := TButton.Create(100, 140, 120, 30, 'Options');
  QuitBtn := TButton.Create(100, 180, 120, 30, 'Quit');
  
  Menu.AddButton(StartBtn);
  Menu.AddButton(OptionsBtn);
  Menu.AddButton(QuitBtn);
  
  while true do
  begin
    Menu.Update;
    ClearScreen(0);
    Menu.Render;
    WaitVBlank;
  end;
end.
```

---

## Best Practices

### 1. Use Consistent Button Sizes

**Standardize button dimensions:**

```pascal
// ✅ GOOD: Consistent sizes
const
  BUTTON_WIDTH = 120;
  BUTTON_HEIGHT = 30;
  BUTTON_SPACING = 40;

// ❌ BAD: Inconsistent sizes
button1 := TButton.Create(100, 100, 120, 30, 'Start');
button2 := TButton.Create(100, 140, 150, 25, 'Options');  // Different size
```

### 2. Handle Input Properly

**Check input state correctly:**

```pascal
// ✅ GOOD: Check pressed state
if InputPressed(KEY_A) then
  HandleClick;

// ❌ BAD: Check held state
if InputHeld(KEY_A) then
  HandleClick;  // Triggers every frame
```

### 3. Visual Feedback

**Show button state clearly:**

```pascal
// ✅ GOOD: Clear visual feedback
case State of
  bsNormal: DrawRect(X, Y, W, H, 1);
  bsHover: DrawRect(X, Y, W, H, 2);  // Different color
  bsPressed: DrawRect(X, Y, W, H, 3);
end;

// ❌ BAD: No feedback
DrawRect(X, Y, W, H, 1);  // Always same
```

### 4. Organize Menu Layout

**Logical menu organization:**

```pascal
// ✅ GOOD: Organized layout
Y := 100;
for i := 0 to Count - 1 do
begin
  Buttons[i].Y := Y;
  Y := Y + BUTTON_SPACING;
end;

// ❌ BAD: Random positions
Buttons[0].Y := 100;
Buttons[1].Y := 180;
Buttons[2].Y := 140;  // Out of order
```

### 5. Clean Up UI Components

**Free UI resources:**

```pascal
// ✅ GOOD: Clean up
procedure TMenu.Cleanup;
var
  i: byte;
begin
  for i := 0 to Count - 1 do
    Buttons[i].Destroy;
end;

// ❌ BAD: Leak resources
// Buttons not destroyed
```

---

## Exercises

### Exercise 1: Basic Button

Write a program that:
1. Creates a button class
2. Renders button
3. Detects clicks
4. Handles button states

### Exercise 2: Menu System

Write a program that:
1. Creates menu with multiple buttons
2. Handles navigation
3. Selects buttons
4. Performs actions

### Exercise 3: UI Components

Write a program that:
1. Creates UI component system
2. Implements buttons, labels, panels
3. Manages UI hierarchy
4. Renders all components

### Exercise 4: Complete Menu

Write a program that:
1. Implements complete menu system
2. Creates main menu, pause menu
3. Handles all interactions
4. Integrates with scenes

---

**Previous Section:** [Scene Management](./01_SceneManagement.md)  
**Next Section:** [Transitions and Effects](./03_TransitionsAndEffects.md)  
**Language Specification:** See [Game Engine](../../languageSpecification/09_GameEngine.md)  
**Last Updated:** 2025-01-XX

