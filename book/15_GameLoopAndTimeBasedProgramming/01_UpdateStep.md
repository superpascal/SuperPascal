# Update Step

**Part of:** [Chapter 11: Game Loop and Time-Based Programming](./README.md)

---

## Introduction

The game loop is the heart of every game. This section teaches you the **Update Step** — where you process input, update game state, and prepare for rendering.

> **Connection to Part III:** For a conceptual introduction to game loops and why they're needed, see [Chapter 17: How Programs Run on Zeal](../15_HowProgramsRunOnZeal/03_GameLoopAndTiming.md) in Part III. This section provides the detailed implementation.

**Key concepts:**
- **Update step** — Process game logic
- **Input processing** — Handle user input
- **State updates** — Update game objects
- **Physics simulation** — Move objects, check collisions
- **Game logic** — Rules, scoring, win/lose conditions

---

## The Game Loop Pattern

### Basic Structure

**Every game has a main loop:**

```pascal
program GameDemo;

procedure InitGame;
begin
  // Initialize graphics, load resources, etc.
end;

procedure UpdateGame;
begin
  // Update game state
end;

procedure RenderGame;
begin
  // Draw everything
end;

begin
  InitGame;
  
  // Main game loop
  while true do
  begin
    UpdateGame;   // Update step
    RenderGame;   // Render step
    WaitVBlank;   // Synchronize with video
  end;
end.
```

### Why Separate Update and Render?

**Separating update and render has benefits:**

1. **Clear organization** — Logic separate from display
2. **Easier debugging** — Can disable rendering to test logic
3. **Performance** — Can update multiple times per render if needed
4. **Maintainability** — Easier to understand and modify

**Example:**
```pascal
// ✅ GOOD: Clear separation
procedure UpdateGame;
begin
  ProcessInput;
  UpdatePlayer;
  UpdateEnemies;
  CheckCollisions;
end;

procedure RenderGame;
begin
  ClearScreen;
  DrawBackground;
  DrawPlayer;
  DrawEnemies;
  DrawHUD;
end;

// ❌ BAD: Mixed together
procedure UpdateAndRender;
begin
  ProcessInput;
  DrawPlayer;  // Drawing mixed with logic
  UpdateEnemies;
  DrawEnemies;
end;
```

---

## The Update Step

### What Happens in Update?

**The update step processes game logic:**

1. **Read input** — Get current input state
2. **Process input** — Handle button presses, movement
3. **Update objects** — Move sprites, update animations
4. **Simulate physics** — Apply gravity, velocity, collisions
5. **Update game state** — Score, lives, level, etc.
6. **Check conditions** — Win/lose, level complete, etc.

**Example:**
```pascal
procedure UpdateGame;
begin
  // 1. Read input
  var input := ReadInput;
  
  // 2. Process input
  if (input and BTN_LEFT) <> 0 then
    playerX := playerX - 2;
  if (input and BTN_RIGHT) <> 0 then
    playerX := playerX + 2;
  
  // 3. Update objects
  UpdatePlayer;
  UpdateEnemies;
  UpdateBullets;
  
  // 4. Simulate physics
  ApplyGravity;
  UpdateVelocities;
  CheckCollisions;
  
  // 5. Update game state
  UpdateScore;
  CheckLives;
  
  // 6. Check conditions
  if lives = 0 then
    gameState := gsGameOver;
end;
```

### Update Order Matters

**Update objects in the right order:**

```pascal
procedure UpdateGame;
begin
  // 1. Input first (affects everything)
  ProcessInput;
  
  // 2. Player before enemies (player moves first)
  UpdatePlayer;
  
  // 3. Enemies after player (react to player)
  UpdateEnemies;
  
  // 4. Projectiles after entities (may hit entities)
  UpdateBullets;
  
  // 5. Collisions last (check after all movement)
  CheckCollisions;
  
  // 6. Game state last (check after all updates)
  UpdateGameState;
end;
```

**Why order matters:**
- Player should move before enemies react
- Collisions should be checked after all movement
- Game state should update after all logic

---

## Processing Input

### Reading Input

**Read input at the start of update:**

```pascal
procedure UpdateGame;
var
  input: word;
begin
  // Read input first
  input := ReadInput;
  
  // Process input
  ProcessPlayerInput(input);
  ProcessMenuInput(input);
end;
```

### Input Processing Patterns

**Handle input based on game state:**

```pascal
procedure ProcessInput(input: word);
begin
  case gameState of
    gsMenu:
      ProcessMenuInput(input);
    gsPlaying:
      ProcessGameInput(input);
    gsPaused:
      ProcessPauseInput(input);
    gsGameOver:
      ProcessGameOverInput(input);
  end;
end;

procedure ProcessGameInput(input: word);
begin
  // Movement
  if (input and BTN_LEFT) <> 0 then
    playerVelocityX := -2;
  if (input and BTN_RIGHT) <> 0 then
    playerVelocityX := 2;
  
  // Actions
  if WasJustPressed(input, BTN_A) then
    Jump;
  if WasJustPressed(input, BTN_B) then
    Attack;
  if WasJustPressed(input, BTN_START) then
    gameState := gsPaused;
end;
```

---

## Updating Game Objects

### Player Update

**Update player based on input and physics:**

```pascal
procedure UpdatePlayer;
begin
  // Apply velocity
  playerX := playerX + playerVelocityX;
  playerY := playerY + playerVelocityY;
  
  // Apply gravity
  if not isGrounded then
    playerVelocityY := playerVelocityY + 1;
  
  // Ground collision
  if playerY > GROUND_LEVEL then
  begin
    playerY := GROUND_LEVEL;
    playerVelocityY := 0;
    isGrounded := true;
  end;
  
  // Friction
  if isGrounded then
    playerVelocityX := playerVelocityX * 0.9;  // Slow down
  
  // Update animation
  UpdatePlayerAnimation;
  
  // Keep on screen
  if playerX < 0 then playerX := 0;
  if playerX > 320 then playerX := 320;
end;
```

### Enemy Update

**Update enemies (AI, movement, etc.):**

```pascal
procedure UpdateEnemies;
var
  i: byte;
begin
  for i := 0 to enemyCount - 1 do
  begin
    // AI: Move toward player
    if enemies[i].X < playerX then
      enemies[i].X := enemies[i].X + 1
    else
      enemies[i].X := enemies[i].X - 1;
    
    // Update animation
    UpdateEnemyAnimation(i);
    
    // Check bounds
    if enemies[i].X < 0 then
      enemies[i].X := 0;
    if enemies[i].X > 320 then
      enemies[i].X := 320;
  end;
end;
```

### Projectile Update

**Update bullets, missiles, etc.:**

```pascal
procedure UpdateBullets;
var
  i: byte;
begin
  for i := 0 to bulletCount - 1 do
  begin
    if bullets[i].Active then
    begin
      // Move bullet
      bullets[i].X := bullets[i].X + bullets[i].VelocityX;
      bullets[i].Y := bullets[i].Y + bullets[i].VelocityY;
      
      // Remove if off screen
      if (bullets[i].X < 0) or (bullets[i].X > 320) or
         (bullets[i].Y < 0) or (bullets[i].Y > 240) then
        bullets[i].Active := false;
    end;
  end;
end;
```

---

## Physics Simulation

### Velocity and Acceleration

**Basic physics: velocity and acceleration:**

```pascal
type
  TPhysicsObject = record
    X, Y: integer;
    VelocityX, VelocityY: integer;
    AccelerationX, AccelerationY: integer;
  end;

procedure UpdatePhysics(var obj: TPhysicsObject);
begin
  // Apply acceleration to velocity
  obj.VelocityX := obj.VelocityX + obj.AccelerationX;
  obj.VelocityY := obj.VelocityY + obj.AccelerationY;
  
  // Apply velocity to position
  obj.X := obj.X + obj.VelocityX;
  obj.Y := obj.Y + obj.VelocityY;
  
  // Reset acceleration (will be set by forces next frame)
  obj.AccelerationX := 0;
  obj.AccelerationY := 0;
end;
```

### Gravity

**Apply gravity to objects:**

```pascal
const
  GRAVITY = 1;  // Pixels per frame per frame

procedure ApplyGravity(var obj: TPhysicsObject);
begin
  if not obj.IsGrounded then
    obj.AccelerationY := obj.AccelerationY + GRAVITY;
end;
```

### Friction

**Apply friction to slow objects:**

```pascal
const
  FRICTION = 0.9;  // Friction coefficient (0.0 to 1.0)

procedure ApplyFriction(var obj: TPhysicsObject);
begin
  if obj.IsGrounded then
  begin
    obj.VelocityX := Trunc(obj.VelocityX * FRICTION);
    // Stop if very slow
    if Abs(obj.VelocityX) < 1 then
      obj.VelocityX := 0;
  end;
end;
```

---

## Collision Detection

### Basic Collision Checks

**Check collisions in update step:**

```pascal
procedure CheckCollisions;
var
  i, j: byte;
begin
  // Player vs enemies
  for i := 0 to enemyCount - 1 do
  begin
    if CollisionCheck(playerX, playerY, 
                      enemies[i].X, enemies[i].Y) then
    begin
      // Player hit by enemy
      TakeDamage;
      if lives = 0 then
        gameState := gsGameOver;
    end;
  end;
  
  // Bullets vs enemies
  for i := 0 to bulletCount - 1 do
  begin
    if bullets[i].Active then
    begin
      for j := 0 to enemyCount - 1 do
      begin
        if CollisionCheck(bullets[i].X, bullets[i].Y,
                          enemies[j].X, enemies[j].Y) then
        begin
          // Bullet hit enemy
          bullets[i].Active := false;
          enemies[j].Health := enemies[j].Health - 1;
          if enemies[j].Health = 0 then
          begin
            score := score + 100;
            RemoveEnemy(j);
          end;
        end;
      end;
    end;
  end;
end;
```

### Collision Functions

**Simple AABB (Axis-Aligned Bounding Box) collision:**

```pascal
function CollisionCheck(x1, y1, w1, h1, x2, y2, w2, h2: integer): boolean;
begin
  CollisionCheck := 
    (x1 < x2 + w2) and (x1 + w1 > x2) and
    (y1 < y2 + h2) and (y1 + h1 > y2);
end;

// Usage
if CollisionCheck(playerX, playerY, 16, 16,
                  enemyX, enemyY, 16, 16) then
  // Collision!
```

---

## Game State Updates

### Score and Lives

**Update game state:**

```pascal
procedure UpdateGameState;
begin
  // Update score display
  // (score is updated by other systems)
  
  // Check lives
  if lives = 0 then
    gameState := gsGameOver;
  
  // Check level complete
  if enemiesRemaining = 0 then
  begin
    level := level + 1;
    LoadLevel(level);
  end;
  
  // Update timers
  gameTimer := gameTimer + 1;
  if gameTimer > 3600 then  // 60 seconds
    gameState := gsTimeUp;
end;
```

### Win/Lose Conditions

**Check win and lose conditions:**

```pascal
procedure CheckGameConditions;
begin
  // Lose conditions
  if lives = 0 then
  begin
    gameState := gsGameOver;
    finalScore := score;
  end;
  
  if gameTimer = 0 then
  begin
    gameState := gsTimeUp;
    finalScore := score;
  end;
  
  // Win conditions
  if level > MAX_LEVEL then
  begin
    gameState := gsVictory;
    finalScore := score;
  end;
end;
```

---

## Complete Update Example

**Putting it all together:**

```pascal
procedure UpdateGame;
var
  input: word;
begin
  // 1. Read input
  input := ReadInput;
  
  // 2. Process input
  ProcessInput(input);
  
  // 3. Update player
  UpdatePlayer;
  
  // 4. Update enemies
  UpdateEnemies;
  
  // 5. Update projectiles
  UpdateBullets;
  
  // 6. Apply physics
  ApplyGravity;
  UpdateVelocities;
  
  // 7. Check collisions
  CheckCollisions;
  
  // 8. Update animations
  UpdateAllAnimations;
  
  // 9. Update game state
  UpdateScore;
  CheckLives;
  CheckGameConditions;
  
  // 10. Update timers
  gameTimer := gameTimer + 1;
end;
```

---

## Best Practices

### 1. Update Order Matters

**Update objects in logical order:**

```pascal
// ✅ GOOD: Logical order
ProcessInput;
UpdatePlayer;
UpdateEnemies;
CheckCollisions;

// ❌ BAD: Wrong order
CheckCollisions;  // Before movement?
UpdatePlayer;
```

### 2. Keep Update Deterministic

**Same input should produce same result:**

```pascal
// ✅ GOOD: Deterministic
playerX := playerX + velocityX;  // Always same result

// ❌ BAD: Non-deterministic
playerX := playerX + Random(3);  // Random results
```

### 3. Separate Logic from Rendering

**Don't draw in update step:**

```pascal
// ✅ GOOD: Logic only
procedure UpdateGame;
begin
  playerX := playerX + 1;
end;

// ❌ BAD: Drawing in update
procedure UpdateGame;
begin
  playerX := playerX + 1;
  DrawPlayer(playerX, playerY);  // Should be in render
end;
```

### 4. Handle Edge Cases

**Check bounds, null pointers, etc.:**

```pascal
// ✅ GOOD: Bounds checking
if playerX < 0 then playerX := 0;
if playerX > 320 then playerX := 320;

// ❌ BAD: No bounds checking
playerX := playerX + velocityX;  // Can go off screen
```

### 5. Update All Systems

**Don't forget to update everything:**

```pascal
// ✅ GOOD: Complete update
UpdatePlayer;
UpdateEnemies;
UpdateBullets;
UpdateAnimations;
UpdateTimers;

// ❌ BAD: Missing updates
UpdatePlayer;
// Forgot to update enemies!
```

---

## Exercises

### Exercise 1: Basic Update Loop

Write a program that:
1. Has a game loop with update and render steps
2. Updates a player position based on input
3. Keeps player on screen
4. Updates every frame

### Exercise 2: Physics Update

Write a program that:
1. Has objects with velocity
2. Applies gravity to objects
3. Updates positions based on velocity
4. Handles ground collision

### Exercise 3: Collision Detection

Write a program that:
1. Has multiple objects
2. Detects collisions between objects
3. Responds to collisions (bounce, destroy, etc.)
4. Updates collision state each frame

### Exercise 4: Complete Game Update

**GCSE Level:**
Write a program that:
1. Has player, enemies, and bullets
2. Updates all objects in correct order
3. Checks collisions
4. Updates game state (score, lives)
5. Handles win/lose conditions

---

**Previous Chapter:** [Chapter 10: Input, State, and Interaction](../14_InputStateAndInteraction/README.md)  
**Next Section:** [Render Step](./02_RenderStep.md)  
**Language Specification:** See [Game Engine Concepts](../../languageSpecification/09_GameEngine_Concepts.md)  
**Last Updated:** 2025-01-XX

