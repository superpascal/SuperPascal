# Core System Development

**Part of:** [Chapter 31: Capstone Project Guide](./README.md)

---

## Introduction

Once you have a plan, it's time to build. This section teaches you how to set up your project structure, implement core systems incrementally, test as you develop, and integrate systems together.

**Key concepts:**
- **Project structure** — Organizing your code
- **Incremental development** — Building step by step
- **Testing** — Verifying functionality
- **Integration** — Combining systems

---

## Setting Up Project Structure

### Project Organization

**Organize your project clearly:**

```
MyProject/
├── src/
│   ├── main.pas              # Entry point
│   ├── game/
│   │   ├── game.pas          # Main game logic
│   │   ├── state.pas         # Game state management
│   │   └── loop.pas          # Game loop
│   ├── entities/
│   │   ├── player.pas        # Player entity
│   │   ├── enemies.pas       # Enemy entities
│   │   └── projectiles.pas   # Bullets, etc.
│   ├── systems/
│   │   ├── movement.pas      # Movement system
│   │   ├── collision.pas     # Collision system
│   │   └── rendering.pas     # Rendering system
│   ├── utils/
│   │   ├── math.pas          # Math utilities
│   │   └── graphics.pas      # Graphics utilities
│   └── data/
│       └── constants.pas     # Game constants
├── assets/
│   ├── sprites/
│   ├── tiles/
│   └── sounds/
├── levels/
└── README.md
```

### Main Program Structure

**Start with a simple main program:**

```pascal
program MyGame;

uses
  Game,
  Constants;

begin
  InitGame;
  
  while true do
  begin
    UpdateGame;
    RenderGame;
    WaitVBlank;
  end;
end.
```

### Constants File

**Define game constants:**

```pascal
unit Constants;

interface

const
  // Screen dimensions
  SCREEN_WIDTH = 320;
  SCREEN_HEIGHT = 240;
  
  // Player constants
  PLAYER_SPEED = Q8_8(2.0);
  PLAYER_JUMP_FORCE = Q8_8(-5.0);
  GRAVITY = Q8_8(0.3);
  
  // Game constants
  MAX_ENEMIES = 10;
  MAX_PROJECTILES = 20;

implementation

end.
```

---

## Implementing Core Systems

### System 1: Video Initialization

**Start with video setup:**

```pascal
unit Game;

interface

procedure InitGame;
procedure UpdateGame;
procedure RenderGame;

implementation

uses
  Constants;

procedure InitGame;
begin
  // Initialize video system
  ZVB_SetVideoMode(VIDEO_MODE_320x240);
  ZVB_EnableScreen(true);
  
  // Clear screen
  ClearScreen(0);  // Black background
end;

procedure UpdateGame;
begin
  // Will add game logic here
end;

procedure RenderGame;
begin
  // Will add rendering here
end;

end.
```

**Test:** Run program, verify screen initializes correctly.

### System 2: Input Handling

**Add input system:**

```pascal
unit Input;

interface

type
  TInputState = record
    Left, Right, Up, Down: boolean;
    Jump, Shoot: boolean;
  end;

var
  Input: TInputState;

procedure UpdateInput;

implementation

procedure UpdateInput;
var
  inputBits: word;
begin
  inputBits := ReadInput;
  
  Input.Left := (inputBits and INPUT_LEFT) <> 0;
  Input.Right := (inputBits and INPUT_RIGHT) <> 0;
  Input.Up := (inputBits and INPUT_UP) <> 0;
  Input.Down := (inputBits and INPUT_DOWN) <> 0;
  Input.Jump := (inputBits and INPUT_A) <> 0;
  Input.Shoot := (inputBits and INPUT_B) <> 0;
end;

end.
```

**Test:** Print input state, verify buttons work.

### System 3: Player Entity

**Create player entity:**

```pascal
unit Player;

interface

type
  TPlayer = record
    X, Y: Q8.8;
    VelocityX, VelocityY: Q8.8;
    OnGround: boolean;
    Sprite: byte;
  end;

var
  Player: TPlayer;

procedure InitPlayer;
procedure UpdatePlayer;
procedure RenderPlayer;

implementation

uses
  Input,
  Constants,
  Math;

procedure InitPlayer;
begin
  Player.X := Q8_8(160.0);  // Center of screen
  Player.Y := Q8_8(200.0);  // Near bottom
  Player.VelocityX := Q8_8(0.0);
  Player.VelocityY := Q8_8(0.0);
  Player.OnGround := false;
  Player.Sprite := 0;
end;

procedure UpdatePlayer;
begin
  // Horizontal movement
  if Input.Left then
    Player.VelocityX := Player.VelocityX - PLAYER_SPEED
  else if Input.Right then
    Player.VelocityX := Player.VelocityX + PLAYER_SPEED
  else
    Player.VelocityX := Player.VelocityX * Q8_8(0.8);  // Friction
  
  // Jump
  if Input.Jump and Player.OnGround then
  begin
    Player.VelocityY := PLAYER_JUMP_FORCE;
    Player.OnGround := false;
  end;
  
  // Apply gravity
  if not Player.OnGround then
    Player.VelocityY := Player.VelocityY + GRAVITY;
  
  // Update position
  Player.X := Player.X + Player.VelocityX;
  Player.Y := Player.Y + Player.VelocityY;
  
  // Simple ground collision (temporary)
  if Player.Y > Q8_8(200.0) then
  begin
    Player.Y := Q8_8(200.0);
    Player.VelocityY := Q8_8(0.0);
    Player.OnGround := true;
  end;
end;

procedure RenderPlayer;
begin
  ZVB_SpriteSetX(0, Round(Player.X));
  ZVB_SpriteSetY(0, Round(Player.Y));
  ZVB_SpriteSetTile(0, Player.Sprite);
  ZVB_SpriteSetFlags(0, SPRITE_VISIBLE);
end;

end.
```

**Test:** Player appears and moves with input.

### System 4: Collision Detection

**Add collision system:**

```pascal
unit Collision;

interface

type
  TRect = record
    X, Y, Width, Height: Q8.8;
  end;

function RectCollides(a, b: TRect): boolean;
function PointInRect(x, y: Q8.8; r: TRect): boolean;

implementation

function RectCollides(a, b: TRect): boolean;
begin
  RectCollides := (a.X < b.X + b.Width) and
                  (a.X + a.Width > b.X) and
                  (a.Y < b.Y + b.Height) and
                  (a.Y + a.Height > b.Y);
end;

function PointInRect(x, y: Q8.8; r: TRect): boolean;
begin
  PointInRect := (x >= r.X) and (x < r.X + r.Width) and
                 (y >= r.Y) and (y < r.Y + r.Height);
end;

end.
```

**Test:** Test collision with simple rectangles.

### System 5: Enemy System

**Add enemies:**

```pascal
unit Enemies;

interface

type
  TEnemy = record
    X, Y: Q8.8;
    VelocityX: Q8.8;
    Active: boolean;
    Sprite: byte;
  end;

var
  Enemies: array[0..MAX_ENEMIES - 1] of TEnemy;

procedure InitEnemies;
procedure UpdateEnemies;
procedure RenderEnemies;
function SpawnEnemy(x, y: Q8.8): boolean;

implementation

uses
  Constants;

procedure InitEnemies;
var
  i: byte;
begin
  for i := 0 to MAX_ENEMIES - 1 do
    Enemies[i].Active := false;
end;

function SpawnEnemy(x, y: Q8.8): boolean;
var
  i: byte;
begin
  for i := 0 to MAX_ENEMIES - 1 do
  begin
    if not Enemies[i].Active then
    begin
      Enemies[i].X := x;
      Enemies[i].Y := y;
      Enemies[i].VelocityX := Q8_8(-1.0);
      Enemies[i].Active := true;
      Enemies[i].Sprite := 1;
      SpawnEnemy := true;
      Exit;
    end;
  end;
  SpawnEnemy := false;
end;

procedure UpdateEnemies;
var
  i: byte;
begin
  for i := 0 to MAX_ENEMIES - 1 do
  begin
    if Enemies[i].Active then
    begin
      Enemies[i].X := Enemies[i].X + Enemies[i].VelocityX;
      
      // Remove if off screen
      if Enemies[i].X < Q8_8(-16.0) then
        Enemies[i].Active := false;
    end;
  end;
end;

procedure RenderEnemies;
var
  i: byte;
  spriteIndex: byte;
begin
  spriteIndex := 1;
  for i := 0 to MAX_ENEMIES - 1 do
  begin
    if Enemies[i].Active then
    begin
      ZVB_SpriteSetX(spriteIndex, Round(Enemies[i].X));
      ZVB_SpriteSetY(spriteIndex, Round(Enemies[i].Y));
      ZVB_SpriteSetTile(spriteIndex, Enemies[i].Sprite);
      ZVB_SpriteSetFlags(spriteIndex, SPRITE_VISIBLE);
      spriteIndex := spriteIndex + 1;
    end;
  end;
end;

end.
```

**Test:** Enemies spawn and move.

---

## Building Incrementally

### Incremental Development Process

**Build one feature at a time:**

1. **Add feature** — Implement one small piece
2. **Test feature** — Verify it works
3. **Integrate feature** — Connect with existing code
4. **Test integration** — Verify everything works together
5. **Refine feature** — Polish and improve

### Example: Adding Shooting

**Step 1: Projectile system**

```pascal
unit Projectiles;

interface

type
  TProjectile = record
    X, Y: Q8.8;
    VelocityX, VelocityY: Q8.8;
    Active: boolean;
  end;

var
  Projectiles: array[0..MAX_PROJECTILES - 1] of TProjectile;

procedure InitProjectiles;
function FireProjectile(x, y, velX, velY: Q8.8): boolean;
procedure UpdateProjectiles;
procedure RenderProjectiles;

implementation

// ... implementation ...

end.
```

**Step 2: Test in isolation**

```pascal
// Test projectile system
InitProjectiles;
FireProjectile(Q8_8(100.0), Q8_8(100.0), Q8_8(5.0), Q8_8(0.0));
UpdateProjectiles;
RenderProjectiles;
```

**Step 3: Integrate with player**

```pascal
// In Player unit
if Input.Shoot then
begin
  FireProjectile(Player.X, Player.Y, Q8_8(5.0), Q8_8(0.0));
end;
```

**Step 4: Test integration**

```pascal
// Verify player can shoot
// Verify projectiles appear
// Verify projectiles move
```

**Step 5: Refine**

```pascal
// Add cooldown
// Add projectile limits
// Add visual effects
```

---

## Testing as You Develop

### Testing Strategy

**Test each system:**

1. **Unit tests** — Test individual functions
2. **Integration tests** — Test systems together
3. **Manual testing** — Play and verify
4. **Edge case testing** — Test boundaries

### Test Functions

**Create test procedures:**

```pascal
unit Tests;

interface

procedure TestCollision;
procedure TestMovement;
procedure TestShooting;

implementation

procedure TestCollision;
var
  rect1, rect2: TRect;
begin
  rect1.X := Q8_8(10.0);
  rect1.Y := Q8_8(10.0);
  rect1.Width := Q8_8(20.0);
  rect1.Height := Q8_8(20.0);
  
  rect2.X := Q8_8(15.0);
  rect2.Y := Q8_8(15.0);
  rect2.Width := Q8_8(20.0);
  rect2.Height := Q8_8(20.0);
  
  if RectCollides(rect1, rect2) then
    WriteLn('Collision test: PASS')
  else
    WriteLn('Collision test: FAIL');
end;

procedure TestMovement;
begin
  Player.X := Q8_8(100.0);
  Player.Y := Q8_8(100.0);
  Player.VelocityX := Q8_8(2.0);
  
  UpdatePlayer;
  
  if Player.X = Q8_8(102.0) then
    WriteLn('Movement test: PASS')
  else
    WriteLn('Movement test: FAIL');
end;

end.
```

### Debug Mode

**Add debug features:**

```pascal
const
  DEBUG_MODE = true;

procedure RenderDebug;
begin
  if DEBUG_MODE then
  begin
    // Draw collision boxes
    // Show entity positions
    // Display frame rate
    // Show input state
  end;
end;
```

---

## Integrating Systems

### System Integration

**Connect systems together:**

```pascal
unit Game;

interface

procedure InitGame;
procedure UpdateGame;
procedure RenderGame;

implementation

uses
  Input,
  Player,
  Enemies,
  Projectiles,
  Collision,
  Constants;

procedure InitGame;
begin
  InitVideo;
  InitInput;
  InitPlayer;
  InitEnemies;
  InitProjectiles;
end;

procedure UpdateGame;
begin
  UpdateInput;
  UpdatePlayer;
  UpdateEnemies;
  UpdateProjectiles;
  
  // Collision between player and enemies
  CheckPlayerEnemyCollisions;
  
  // Collision between projectiles and enemies
  CheckProjectileEnemyCollisions;
end;

procedure RenderGame;
begin
  ClearScreen(0);
  RenderPlayer;
  RenderEnemies;
  RenderProjectiles;
  
  if DEBUG_MODE then
    RenderDebug;
end;

end.
```

### Data Flow

**Understand how data flows:**

```
Input → Player → Position
                ↓
            Collision → Response
                ↓
            Rendering → Screen
```

**Each system:**
- Reads from other systems
- Processes data
- Writes to other systems
- Maintains its own state

---

## Common Development Patterns

### Pattern 1: Entity-Component-System

**Use ECS for complex games:**

```pascal
// Create entities
var player := EntityCreate;
EntitySetPosition(player, Q8_8(100.0), Q8_8(100.0));
EntitySetVelocity(player, Q8_8(0.0), Q8_8(0.0));
EntitySetSprite(player, 0);

// Systems process entities
MovementSystem;  // Updates all entities with Position + Velocity
CollisionSystem;  // Checks all entities with Collider
RenderingSystem;  // Renders all entities with Sprite
```

### Pattern 2: State Machine

**Use state machines for behavior:**

```pascal
type
  TPlayerState = (psIdle, psWalking, psJumping, psFalling);

var
  PlayerState: TPlayerState;

procedure UpdatePlayerState;
begin
  case PlayerState of
    psIdle:
      if Input.Left or Input.Right then
        PlayerState := psWalking
      else if Input.Jump then
        PlayerState := psJumping;
    
    psWalking:
      if not (Input.Left or Input.Right) then
        PlayerState := psIdle
      else if Input.Jump then
        PlayerState := psJumping
      else if not Player.OnGround then
        PlayerState := psFalling;
    
    // ... other states ...
  end;
end;
```

### Pattern 3: Object Pool

**Reuse objects:**

```pascal
// Instead of creating/destroying
function GetEnemy: ^TEnemy;
var
  i: byte;
begin
  for i := 0 to MAX_ENEMIES - 1 do
  begin
    if not Enemies[i].Active then
    begin
      GetEnemy := @Enemies[i];
      Exit;
    end;
  end;
  GetEnemy := nil;
end;
```

---

## Best Practices

### 1. Build Incrementally

**Add one feature at a time:**

```pascal
// ✅ GOOD: Add feature, test, integrate
InitPlayer;
UpdatePlayer;  // Test
RenderPlayer;  // Test
// Then integrate with game loop

// ❌ BAD: Build everything at once
// Hard to debug when things break
```

### 2. Test Frequently

**Test after each change:**

```pascal
// ✅ GOOD: Test after each feature
AddFeature;
TestFeature;
FixBugs;
// Then move to next feature

// ❌ BAD: Build everything, then test
// Hard to find bugs
```

### 3. Keep It Simple

**Start simple, add complexity:**

```pascal
// ✅ GOOD: Simple first
Player.X := Player.X + Speed;
// Then add complexity
Player.X := Player.X + Player.VelocityX;
ApplyFriction;
ApplyCollision;

// ❌ BAD: Complex from start
// Hard to debug
```

### 4. Organize Code

**Keep code organized:**

```pascal
// ✅ GOOD: Clear organization
unit Player;
// Player-specific code

unit Enemies;
// Enemy-specific code

// ❌ BAD: Everything in one file
// Hard to find and maintain
```

### 5. Document As You Go

**Write comments:**

```pascal
// ✅ GOOD: Document purpose
// Updates player position based on velocity and input
procedure UpdatePlayer;

// ❌ BAD: No documentation
// Future you won't remember
```

---

## Exercises

### Exercise 1: Project Setup

Set up a new project:
1. Create project structure
2. Write main program
3. Initialize video
4. Test basic setup

### Exercise 2: Core System

Implement a core system:
1. Choose a system (player, enemy, etc.)
2. Implement basic functionality
3. Test in isolation
4. Integrate with main program

### Exercise 3: Incremental Feature

Add a feature incrementally:
1. Plan the feature
2. Implement step by step
3. Test each step
4. Integrate when complete

### Exercise 4: System Integration

Integrate multiple systems:
1. Connect two systems
2. Test integration
3. Add third system
4. Verify everything works

---

**Previous Section:** [Proposal and Planning](./01_ProposalAndPlanning.md)  
**Next Section:** [Art, Sound, and Content](./03_ArtSoundAndContent.md)  
**Last Updated:** 2025-01-XX

