# Code Organization and Style

**Part of:** [Chapter 08: Units and Modular Programming](./README.md)

---

## Introduction

Writing code that works is only half the battle. Writing code that is readable, maintainable, and follows good style is equally important. This section teaches you code organization principles, naming conventions, and style guidelines that will make your code professional and easy to work with.

**Target Levels:**
- **GCSE:** Basic naming and structure
- **A-Level:** Style guide and organization principles
- **University:** Professional standards and refactoring

---

## Why Code Style Matters

### Readability

**Code is read more than written:**
- You'll read your own code many times
- Others will read your code
- Future you will thank present you

### Maintainability

**Good style makes code easier to:**
- Understand
- Modify
- Debug
- Extend

### Professionalism

**Good style shows:**
- Attention to detail
- Professional standards
- Care for quality

---

## Naming Conventions

### Variables

**Use descriptive names:**
```pascal
// ✅ GOOD: Clear and descriptive
var
  playerHealth: integer;
  enemyCount: integer;
  currentScore: integer;

// ❌ BAD: Unclear
var
  h: integer;
  c: integer;
  s: integer;
```

**Use camelCase for variables:**
```pascal
// ✅ GOOD: camelCase
var
  playerPosition: TPoint;
  enemyVelocity: TPoint;
  gameState: TGameState;

// ❌ BAD: Inconsistent
var
  player_position: TPoint;
  EnemyVelocity: TPoint;
  gamestate: TGameState;
```

### Functions and Procedures

**Use verb-noun pattern:**
```pascal
// ✅ GOOD: Clear action
procedure UpdatePlayer;
procedure DrawSprite(sprite: TSprite);
function CalculateDistance(p1, p2: TPoint): Q8.8;

// ❌ BAD: Unclear
procedure DoStuff;
procedure Thing(s: TSprite);
function Calc(p1, p2: TPoint): Q8.8;
```

**Use PascalCase for procedures/functions:**
```pascal
// ✅ GOOD: PascalCase
procedure UpdateGame;
function GetPlayerPosition: TPoint;
function IsColliding(entity1, entity2: TEntity): boolean;

// ❌ BAD: Inconsistent
procedure update_game;
function getPlayerPosition: TPoint;
function iscolliding(e1, e2: TEntity): boolean;
```

### Types

**Use descriptive type names:**
```pascal
// ✅ GOOD: Clear type names
type
  TPlayer = record
    Position: TPoint;
    Health: integer;
  end;
  
  TGameState = (gsMenu, gsPlaying, gsPaused);

// ❌ BAD: Unclear
type
  T1 = record
    P: TPoint;
    H: integer;
  end;
  
  TS = (m, p, pa);
```

**Use T prefix for types:**
```pascal
// ✅ GOOD: T prefix
type
  TEntity = record
    X, Y: integer;
  end;
  
  TEntityList = array[0..99] of TEntity;

// ❌ BAD: No prefix
type
  Entity = record
    X, Y: integer;
  end;
```

### Constants

**Use UPPER_CASE with underscores:**
```pascal
// ✅ GOOD: UPPER_CASE
const
  MAX_PLAYERS = 4;
  SCREEN_WIDTH = 320;
  SCREEN_HEIGHT = 240;
  DEFAULT_HEALTH = 100;

// ❌ BAD: Inconsistent
const
  maxPlayers = 4;
  SCREEN_WIDTH = 320;
  screenHeight = 240;
```

---

## Code Structure

### Organization

**Organize code logically:**
```pascal
unit GameUnit;

interface
  // Types
  type
    TEntity = record
      X, Y: integer;
    end;
  
  // Constants
  const
    MAX_ENTITIES = 100;
  
  // Procedures and functions
  procedure UpdateEntity(var entity: TEntity);
  function GetEntityPosition(entity: TEntity): TPoint;

implementation
  // Implementation details
  procedure UpdateEntity(var entity: TEntity);
  begin
    // Implementation
  end;
  
  function GetEntityPosition(entity: TEntity): TPoint;
  begin
    // Implementation
  end;
end.
```

### Grouping

**Group related code together:**
```pascal
// ✅ GOOD: Grouped logically
// Entity management
procedure CreateEntity(var entity: TEntity);
procedure UpdateEntity(var entity: TEntity);
procedure DestroyEntity(var entity: TEntity);

// Rendering
procedure DrawEntity(entity: TEntity);
procedure DrawAllEntities(entities: array of TEntity);

// Collision
function CheckCollision(e1, e2: TEntity): boolean;
function CheckCollisionWithTile(entity: TEntity; tile: integer): boolean;

// ❌ BAD: Scattered
procedure CreateEntity(var entity: TEntity);
procedure DrawEntity(entity: TEntity);
procedure UpdateEntity(var entity: TEntity);
function CheckCollision(e1, e2: TEntity): boolean;
procedure DrawAllEntities(entities: array of TEntity);
```

### Spacing

**Use consistent spacing:**
```pascal
// ✅ GOOD: Consistent spacing
procedure UpdateGame;
var
  i: integer;
begin
  for i := 0 to EntityCount - 1 do
  begin
    UpdateEntity(Entities[i]);
    DrawEntity(Entities[i]);
  end;
end;

// ❌ BAD: Inconsistent spacing
procedure UpdateGame;
var
i:integer;
begin
for i:=0 to EntityCount-1 do
begin
UpdateEntity(Entities[i]);
DrawEntity(Entities[i]);
end;
end;
```

---

## Comments

### When to Comment

**Comment:**
- Why code exists (not what it does)
- Complex algorithms
- Non-obvious decisions
- Public APIs

**Don't comment:**
- Obvious code
- What code does (code should be self-explanatory)
- Outdated information

### Good Comments

```pascal
// ✅ GOOD: Explains why
// Use binary search because array is sorted
// This is O(log n) vs O(n) for linear search
index := BinarySearch(sortedArray, value);

// ✅ GOOD: Explains complex logic
// Calculate distance using Manhattan distance
// (faster than Euclidean for grid-based games)
distance := Abs(p1.X - p2.X) + Abs(p1.Y - p2.Y);

// ❌ BAD: States the obvious
// Increment i by 1
i := i + 1;

// ❌ BAD: Outdated
// TODO: Fix this bug (from 3 years ago)
```

### Comment Style

**Use clear, concise comments:**
```pascal
// ✅ GOOD: Clear and helpful
// Initialize entity at spawn point
// Spawn points are predefined safe locations
entity.Position := SpawnPoints[spawnIndex];

// ❌ BAD: Unclear
// Do thing
entity.Position := SpawnPoints[spawnIndex];
```

---

## Code Length

### Function Length

**Keep functions short:**
```pascal
// ✅ GOOD: Short, focused function
function CalculateDistance(p1, p2: TPoint): Q8.8;
var
  dx, dy: Q8.8;
begin
  dx := Q8.8(p1.X) - Q8.8(p2.X);
  dy := Q8.8(p1.Y) - Q8.8(p2.Y);
  CalculateDistance := Sqrt(dx * dx + dy * dy);
end;

// ❌ BAD: Too long, does too much
function DoEverything: integer;
begin
  // 100 lines of code doing many things
  // Hard to understand, test, and maintain
end;
```

**Rule of thumb:** Functions should fit on one screen (20-30 lines)

### File Length

**Keep files focused:**
- One concept per file
- Related code together
- Split large files into smaller units

---

## Refactoring

### What is Refactoring?

**Refactoring** means improving code structure without changing behavior.

**Goals:**
- Make code clearer
- Reduce duplication
- Improve organization
- Make code easier to maintain

### When to Refactor

**Refactor when:**
- Code works but is hard to understand
- You see duplication
- You need to add features
- Code smells appear

### Refactoring Techniques

**1. Extract Function**
```pascal
// ❌ BAD: Long function
procedure UpdateGame;
begin
  // Update player
  player.X := player.X + player.VelocityX;
  player.Y := player.Y + player.VelocityY;
  
  // Update enemies
  for i := 0 to EnemyCount - 1 do
  begin
    enemies[i].X := enemies[i].X + enemies[i].VelocityX;
    enemies[i].Y := enemies[i].Y + enemies[i].VelocityY;
  end;
end;

// ✅ GOOD: Extracted functions
procedure UpdatePosition(var entity: TEntity);
begin
  entity.X := entity.X + entity.VelocityX;
  entity.Y := entity.Y + entity.VelocityY;
end;

procedure UpdateGame;
begin
  UpdatePosition(player);
  for i := 0 to EnemyCount - 1 do
    UpdatePosition(enemies[i]);
end;
```

**2. Rename for Clarity**
```pascal
// ❌ BAD: Unclear name
function Calc(p1, p2: TPoint): Q8.8;

// ✅ GOOD: Clear name
function CalculateDistance(p1, p2: TPoint): Q8.8;
```

**3. Remove Duplication**
```pascal
// ❌ BAD: Duplicated code
procedure DrawPlayer;
begin
  SpriteSet(player.SpriteIndex, player.X, player.Y);
  SpriteShow(player.SpriteIndex);
end;

procedure DrawEnemy;
begin
  SpriteSet(enemy.SpriteIndex, enemy.X, enemy.Y);
  SpriteShow(enemy.SpriteIndex);
end;

// ✅ GOOD: Shared function
procedure DrawEntity(entity: TEntity);
begin
  SpriteSet(entity.SpriteIndex, entity.X, entity.Y);
  SpriteShow(entity.SpriteIndex);
end;
```

---

## Style Guide Summary

### Naming
- **Variables:** camelCase (`playerHealth`)
- **Functions/Procedures:** PascalCase (`UpdatePlayer`)
- **Types:** PascalCase with T prefix (`TEntity`)
- **Constants:** UPPER_CASE (`MAX_PLAYERS`)

### Structure
- Group related code
- Keep functions short (20-30 lines)
- One concept per file
- Consistent spacing

### Comments
- Explain why, not what
- Comment complex logic
- Keep comments up to date
- Remove outdated comments

### Best Practices
- Use descriptive names
- Keep code simple
- Refactor when needed
- Follow conventions consistently

---

## Exercises

### Exercise 1: Improve Naming

Rewrite code with better names:
1. Variables with single letters
2. Functions with unclear names
3. Types without T prefix

### Exercise 2: Organize Code

Reorganize scattered code:
1. Group related functions
2. Organize by purpose
3. Improve structure

### Exercise 3: Add Comments

Add helpful comments:
1. Explain complex algorithms
2. Document public APIs
3. Note important decisions

### Exercise 4: Refactor

Refactor code:
1. Extract long functions
2. Remove duplication
3. Improve clarity

---

**Previous Section:** [Building Larger Projects](./03_BuildingLargerProjects.md)  
**Next Chapter:** [Chapter 09: Graphics](../11_Graphics/README.md)  
**Last Updated:** 2025-01-XX

