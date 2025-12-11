# Transitions and Effects

**Part of:** [Chapter 23: Scenes, UI, and Game Architecture](./README.md)

---

## Introduction

Scene transitions create smooth, polished changes between game states. This section teaches you how to implement transitions, fade effects, and visual effects.

**Key concepts:**
- **Scene transitions** — Smooth scene changes
- **Fade effects** — Fade in/out
- **Transition timing** — Controlling transition speed
- **Visual effects** — Additional polish

---

## Understanding Transitions

### What is a Transition?

**A transition is an animation between scenes:**

- **Fade** — Fade out old scene, fade in new scene
- **Slide** — Slide scenes horizontally/vertically
- **Wipe** — Wipe from one side
- **Instant** — Immediate change (no transition)

**Transition phases:**
1. **Fade out** — Old scene disappears
2. **Switch** — Change to new scene
3. **Fade in** — New scene appears

### Basic Transition

**Simple fade transition:**

```pascal
type
  TTransition = class
  private
    FadeValue: byte;
    FadeDirection: integer;  // -1 = fade out, 1 = fade in
    Active: boolean;
  public
    procedure Start;
    procedure Update;
    function IsComplete: boolean;
    procedure ApplyFade;
  end;

implementation

procedure TTransition.Start;
begin
  FadeValue := 0;
  FadeDirection := -1;  // Start fading out
  Active := true;
end;

procedure TTransition.Update;
begin
  if not Active then
    Exit;
  
  FadeValue := FadeValue + FadeDirection * 4;
  
  if FadeValue >= 255 then
  begin
    FadeValue := 255;
    FadeDirection := 1;  // Switch to fade in
  end
  else if FadeValue <= 0 then
  begin
    FadeValue := 0;
    Active := false;  // Transition complete
  end;
end;

function TTransition.IsComplete: boolean;
begin
  IsComplete := not Active;
end;

procedure TTransition.ApplyFade;
begin
  // Apply fade overlay
  DrawRect(0, 0, 320, 240, FadeValue);
end;
```

---

## Fade Transitions

### Fade Out/In

**Complete fade transition:**

```pascal
type
  TFadeTransition = class
  private
    Alpha: byte;
    Phase: (fpFadeOut, fpSwitch, fpFadeIn);
    Speed: byte;
  public
    constructor Create(speed: byte);
    procedure Start;
    procedure Update;
    function IsComplete: boolean;
    procedure Render;
  end;

implementation

constructor TFadeTransition.Create(speed: byte);
begin
  Speed := speed;
  Phase := fpFadeOut;
  Alpha := 0;
end;

procedure TFadeTransition.Start;
begin
  Phase := fpFadeOut;
  Alpha := 0;
end;

procedure TFadeTransition.Update;
begin
  case Phase of
    fpFadeOut:
      begin
        Alpha := Alpha + Speed;
        if Alpha >= 255 then
        begin
          Alpha := 255;
          Phase := fpSwitch;
        end;
      end;
      
    fpSwitch:
      begin
        // Scene switch happens here
        Phase := fpFadeIn;
      end;
      
    fpFadeIn:
      begin
        Alpha := Alpha - Speed;
        if Alpha <= 0 then
        begin
          Alpha := 0;
          // Transition complete
        end;
      end;
  end;
end;

function TFadeTransition.IsComplete: boolean;
begin
  IsComplete := (Phase = fpFadeIn) and (Alpha = 0);
end;

procedure TFadeTransition.Render;
begin
  // Draw fade overlay
  if Alpha > 0 then
    DrawRect(0, 0, 320, 240, Alpha);
end;
```

### Fade with Scene Manager

**Integrate fade with scene switching:**

```pascal
type
  TSceneManager = class
  private
    CurrentScene: TScene;
    NextScene: TScene;
    Transition: TFadeTransition;
    Transitioning: boolean;
  public
    constructor Create;
    procedure Update;
    procedure Render;
    procedure SwitchScene(newScene: TScene);
  end;

implementation

constructor TSceneManager.Create;
begin
  Transition := TFadeTransition.Create(8);  // Speed 8
  Transitioning := false;
end;

procedure TSceneManager.SwitchScene(newScene: TScene);
begin
  NextScene := newScene;
  Transitioning := true;
  Transition.Start;
end;

procedure TSceneManager.Update;
begin
  if Transitioning then
  begin
    Transition.Update;
    
    // Switch scenes when fully faded out
    if Transition.Phase = fpSwitch then
    begin
      if CurrentScene <> nil then
        CurrentScene.Cleanup;
      
      CurrentScene := NextScene;
      NextScene := nil;
      
      if CurrentScene <> nil then
        CurrentScene.Init;
    end;
    
    if Transition.IsComplete then
      Transitioning := false;
  end
  else
  begin
    if CurrentScene <> nil then
      CurrentScene.Update;
  end;
end;

procedure TSceneManager.Render;
begin
  if CurrentScene <> nil then
    CurrentScene.Render;
  
  if Transitioning then
    Transition.Render;
end;
```

---

## Slide Transitions

### Horizontal Slide

**Slide scenes horizontally:**

```pascal
type
  TSlideTransition = class
  private
    Offset: integer;
    Direction: integer;  // -1 = left, 1 = right
    Speed: integer;
    Phase: (spSlideOut, spSwitch, spSlideIn);
  public
    constructor Create(direction: integer; speed: integer);
    procedure Start;
    procedure Update;
    function IsComplete: boolean;
    procedure Render(oldScene, newScene: TScene);
  end;

implementation

constructor TSlideTransition.Create(direction: integer; speed: integer);
begin
  Direction := direction;
  Speed := speed;
  Phase := spSlideOut;
  Offset := 0;
end;

procedure TSlideTransition.Start;
begin
  Phase := spSlideOut;
  Offset := 0;
end;

procedure TSlideTransition.Update;
begin
  case Phase of
    spSlideOut:
      begin
        Offset := Offset + Speed * Direction;
        if Abs(Offset) >= 320 then
        begin
          Offset := 320 * Direction;
          Phase := spSwitch;
        end;
      end;
      
    spSwitch:
      begin
        Offset := -320 * Direction;  // Start new scene off-screen
        Phase := spSlideIn;
      end;
      
    spSlideIn:
      begin
        Offset := Offset + Speed * Direction;
        if Offset = 0 then
        begin
          // Transition complete
        end;
      end;
  end;
end;

function TSlideTransition.IsComplete: boolean;
begin
  IsComplete := (Phase = spSlideIn) and (Offset = 0);
end;

procedure TSlideTransition.Render(oldScene, newScene: TScene);
begin
  case Phase of
    spSlideOut:
      begin
        // Render old scene sliding out
        SetRenderOffset(Offset, 0);
        oldScene.Render;
        SetRenderOffset(0, 0);
      end;
      
    spSlideIn:
      begin
        // Render new scene sliding in
        SetRenderOffset(Offset, 0);
        newScene.Render;
        SetRenderOffset(0, 0);
      end;
  end;
end;
```

### Vertical Slide

**Slide scenes vertically:**

```pascal
type
  TVerticalSlideTransition = class(TSlideTransition)
  public
    procedure Render(oldScene, newScene: TScene); override;
  end;

implementation

procedure TVerticalSlideTransition.Render(oldScene, newScene: TScene);
begin
  case Phase of
    spSlideOut:
      begin
        SetRenderOffset(0, Offset);
        oldScene.Render;
        SetRenderOffset(0, 0);
      end;
      
    spSlideIn:
      begin
        SetRenderOffset(0, Offset);
        newScene.Render;
        SetRenderOffset(0, 0);
      end;
  end;
end;
```

---

## Wipe Transitions

### Wipe Effect

**Wipe from one side:**

```pascal
type
  TWipeTransition = class
  private
    Progress: integer;  // 0 to 320
    Direction: integer;  // -1 = left, 1 = right
    Speed: integer;
  public
    constructor Create(direction: integer; speed: integer);
    procedure Start;
    procedure Update;
    function IsComplete: boolean;
    procedure Render(oldScene, newScene: TScene);
  end;

implementation

constructor TWipeTransition.Create(direction: integer; speed: integer);
begin
  Direction := direction;
  Speed := speed;
  Progress := 0;
end;

procedure TWipeTransition.Start;
begin
  Progress := 0;
end;

procedure TWipeTransition.Update;
begin
  Progress := Progress + Speed;
  if Progress >= 320 then
    Progress := 320;
end;

function TWipeTransition.IsComplete: boolean;
begin
  IsComplete := Progress >= 320;
end;

procedure TWipeTransition.Render(oldScene, newScene: TScene);
var
  wipeX: integer;
begin
  if Direction > 0 then
    wipeX := Progress - 320
  else
    wipeX := 320 - Progress;
  
  // Render old scene
  oldScene.Render;
  
  // Render new scene with mask
  SetClipRect(wipeX, 0, 320, 240);
  newScene.Render;
  ClearClipRect;
end;
```

---

## Transition Timing

### Configurable Speed

**Adjust transition speed:**

```pascal
type
  TTransition = class
  public
    Speed: byte;
    Duration: word;  // Frames
    
    constructor Create(speed: byte);
    constructor CreateDuration(duration: word);
  end;

implementation

constructor TTransition.Create(speed: byte);
begin
  Speed := speed;
  Duration := 0;  // Speed-based
end;

constructor TTransition.CreateDuration(duration: word);
begin
  Duration := duration;
  Speed := 255 div duration;  // Calculate speed
end;
```

### Easing Functions

**Smooth transition curves:**

```pascal
function EaseInOut(t: Q8.8): Q8.8;
begin
  // Smooth ease in/out
  if t < Q8_8(0.5) then
    EaseInOut := 2 * t * t
  else
    EaseInOut := 1 - 2 * (1 - t) * (1 - t);
end;

procedure TFadeTransition.Update;
var
  t: Q8.8;
  eased: Q8.8;
begin
  t := Q8_8(Progress) / Q8_8(Duration);
  eased := EaseInOut(t);
  Alpha := Round(eased * 255);
end;
```

---

## Visual Effects

### Screen Flash

**Flash effect for emphasis:**

```pascal
type
  TFlashEffect = class
  private
    Alpha: byte;
    Active: boolean;
  public
    procedure Flash(intensity: byte);
    procedure Update;
    procedure Render;
  end;

implementation

procedure TFlashEffect.Flash(intensity: byte);
begin
  Alpha := intensity;
  Active := true;
end;

procedure TFlashEffect.Update;
begin
  if Active then
  begin
    Alpha := Alpha - 8;
    if Alpha <= 0 then
    begin
      Alpha := 0;
      Active := false;
    end;
  end;
end;

procedure TFlashEffect.Render;
begin
  if Active and (Alpha > 0) then
    DrawRect(0, 0, 320, 240, Alpha);  // White flash
end;
```

### Screen Shake

**Camera shake effect:**

```pascal
type
  TScreenShake = class
  private
    Intensity: integer;
    Duration: word;
    OffsetX, OffsetY: integer;
  public
    procedure Shake(intensity: integer; duration: word);
    procedure Update;
    function GetOffsetX: integer;
    function GetOffsetY: integer;
  end;

implementation

procedure TScreenShake.Shake(intensity: integer; duration: word);
begin
  Intensity := intensity;
  Duration := duration;
end;

procedure TScreenShake.Update;
begin
  if Duration > 0 then
  begin
    OffsetX := Random(Intensity * 2) - Intensity;
    OffsetY := Random(Intensity * 2) - Intensity;
    Duration := Duration - 1;
  end
  else
  begin
    OffsetX := 0;
    OffsetY := 0;
  end;
end;

function TScreenShake.GetOffsetX: integer;
begin
  GetOffsetX := OffsetX;
end;

function TScreenShake.GetOffsetY: integer;
begin
  GetOffsetY := OffsetY;
end;
```

---

## Complete Example

**Putting it all together:**

```pascal
program TransitionsDemo;

type
  TFadeTransition = class
  private
    Alpha: byte;
    Phase: (fpFadeOut, fpSwitch, fpFadeIn);
  public
    procedure Start;
    procedure Update;
    function IsComplete: boolean;
    procedure Render;
  end;
  
  TSceneManager = class
  private
    CurrentScene: TScene;
    NextScene: TScene;
    Transition: TFadeTransition;
    Transitioning: boolean;
  public
    constructor Create;
    procedure Update;
    procedure Render;
    procedure SwitchScene(newScene: TScene);
  end;

var
  SceneManager: TSceneManager;
  MenuScene, GameScene: TScene;

procedure TFadeTransition.Start;
begin
  Alpha := 0;
  Phase := fpFadeOut;
end;

procedure TFadeTransition.Update;
begin
  case Phase of
    fpFadeOut:
      begin
        Alpha := Alpha + 8;
        if Alpha >= 255 then
        begin
          Alpha := 255;
          Phase := fpSwitch;
        end;
      end;
      
    fpSwitch:
      Phase := fpFadeIn;
      
    fpFadeIn:
      begin
        Alpha := Alpha - 8;
        if Alpha <= 0 then
          Alpha := 0;
      end;
  end;
end;

function TFadeTransition.IsComplete: boolean;
begin
  IsComplete := (Phase = fpFadeIn) and (Alpha = 0);
end;

procedure TFadeTransition.Render;
begin
  if Alpha > 0 then
    DrawRect(0, 0, 320, 240, Alpha);
end;

constructor TSceneManager.Create;
begin
  Transition := TFadeTransition.Create;
  Transitioning := false;
end;

procedure TSceneManager.SwitchScene(newScene: TScene);
begin
  NextScene := newScene;
  Transitioning := true;
  Transition.Start;
end;

procedure TSceneManager.Update;
begin
  if Transitioning then
  begin
    Transition.Update;
    
    if Transition.Phase = fpSwitch then
    begin
      if CurrentScene <> nil then
        CurrentScene.Cleanup;
      CurrentScene := NextScene;
      NextScene := nil;
      if CurrentScene <> nil then
        CurrentScene.Init;
    end;
    
    if Transition.IsComplete then
      Transitioning := false;
  end
  else
  begin
    if CurrentScene <> nil then
      CurrentScene.Update;
  end;
end;

procedure TSceneManager.Render;
begin
  if CurrentScene <> nil then
    CurrentScene.Render;
  
  if Transitioning then
    Transition.Render;
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
end.
```

---

## Best Practices

### 1. Use Appropriate Transition Speed

**Match transition to context:**

```pascal
// ✅ GOOD: Fast for gameplay, slow for menus
GameTransition := TFadeTransition.Create(16);  // Fast
MenuTransition := TFadeTransition.Create(4);   // Slow

// ❌ BAD: Same speed everywhere
Transition := TFadeTransition.Create(8);  // May be too fast/slow
```

### 2. Complete Transitions

**Wait for transition to finish:**

```pascal
// ✅ GOOD: Wait for completion
while Transitioning do
begin
  SceneManager.Update;
  WaitVBlank;
end;

// ❌ BAD: Skip transition
SceneManager.SwitchScene(NewScene);
// Immediately use new scene (jarring)
```

### 3. Clean Up During Transition

**Free resources at right time:**

```pascal
// ✅ GOOD: Clean up during switch phase
if Transition.Phase = fpSwitch then
begin
  OldScene.Cleanup;
  NewScene.Init;
end;

// ❌ BAD: Clean up too early/late
OldScene.Cleanup;  // Before transition starts
```

### 4. Use Effects Sparingly

**Don't overuse effects:**

```pascal
// ✅ GOOD: Effects for emphasis
FlashEffect.Flash(200);  // On important event

// ❌ BAD: Constant effects
FlashEffect.Flash(255);  // Every frame (annoying)
```

### 5. Test Transition Timing

**Ensure transitions feel right:**

```pascal
// ✅ GOOD: Tested timing
Transition := TFadeTransition.Create(8);  // Tested, feels good

// ❌ BAD: Guess timing
Transition := TFadeTransition.Create(42);  // Random value
```

---

## Exercises

### Exercise 1: Basic Fade

Write a program that:
1. Implements fade transition
2. Fades between scenes
3. Controls fade speed
4. Demonstrates smooth transitions

### Exercise 2: Slide Transition

Write a program that:
1. Implements slide transition
2. Slides scenes horizontally/vertically
3. Handles scene switching
4. Demonstrates slide effect

### Exercise 3: Multiple Transitions

Write a program that:
1. Implements multiple transition types
2. Chooses transition based on context
3. Handles all transition phases
4. Demonstrates variety

### Exercise 4: Visual Effects

Write a program that:
1. Implements screen effects
2. Uses flash and shake
3. Integrates with transitions
4. Creates polished feel

---

**Previous Section:** [Menus and Buttons](./02_MenusAndButtons.md)  
**Next Chapter:** [Chapter 24: Debugging, Profiling, and Optimization](../24_DebuggingProfilingAndOptimization/README.md)  
**Language Specification:** See [Game Engine](../../languageSpecification/09_GameEngine.md)  
**Last Updated:** 2025-01-XX

