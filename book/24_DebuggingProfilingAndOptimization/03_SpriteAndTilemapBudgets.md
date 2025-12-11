# Sprite and Tilemap Budgets

**Part of:** [Chapter 24: Debugging, Profiling, and Optimization](./README.md)

---

## Introduction

Retro platforms have strict limits on sprites and tilemaps. This section teaches you how to manage these resources efficiently and stay within hardware constraints.

**Key concepts:**
- **Sprite limits** — Maximum sprites per frame
- **Tilemap constraints** — Memory and size limits
- **Resource management** — Efficient resource usage
- **Optimization techniques** — Staying within budgets

---

## Understanding Resource Budgets

### What are Resource Budgets?

**Resource budgets are hardware limits:**

- **Sprite budget** — Maximum sprites visible per frame
- **Tilemap budget** — Maximum tilemap memory
- **Palette budget** — Maximum colors per palette
- **Memory budget** — Total RAM available

**Why budgets matter:**
- **Hardware limits** — Cannot exceed hardware capabilities
- **Performance** — Too many resources = slow rendering
- **Planning** — Know limits before designing

### Platform Limits

**Common platform constraints:**

**ZealZ80 (ZVB):**
- Sprites: 128 sprites (0-127)
- Tilemap: 32x32 tiles (1024 tiles)
- Palettes: 16 palettes, 16 colors each
- Memory: 512KB total

**Foenix F256x:**
- Sprites: Platform-specific (varies)
- Tilemap: Platform-specific
- Memory: 512KB-2MB

**Foenix A2560M:**
- Sprites: Platform-specific
- Tilemap: Platform-specific
- Memory: 1GB DDR3

---

## Sprite Budget Management

### Sprite Limits

**Track sprite usage:**

```pascal
type
  TSpriteBudget = record
    MaxSprites: byte;
    UsedSprites: byte;
    AvailableSprites: byte;
    SpriteUsage: array[0..127] of boolean;  // Which sprites are used
  end;

var
  SpriteBudget: TSpriteBudget;

procedure InitSpriteBudget;
begin
  SpriteBudget.MaxSprites := 128;
  SpriteBudget.UsedSprites := 0;
  SpriteBudget.AvailableSprites := 128;
  FillChar(SpriteBudget.SpriteUsage, SizeOf(SpriteBudget.SpriteUsage), false);
end;

function AllocateSprite: byte;
var
  i: byte;
begin
  for i := 0 to SpriteBudget.MaxSprites - 1 do
  begin
    if not SpriteBudget.SpriteUsage[i] then
    begin
      SpriteBudget.SpriteUsage[i] := true;
      SpriteBudget.UsedSprites := SpriteBudget.UsedSprites + 1;
      SpriteBudget.AvailableSprites := SpriteBudget.AvailableSprites - 1;
      AllocateSprite := i;
      Exit;
    end;
  end;
  
  AllocateSprite := $FF;  // No sprites available
end;

procedure FreeSprite(spriteIndex: byte);
begin
  if (spriteIndex < SpriteBudget.MaxSprites) and 
     SpriteBudget.SpriteUsage[spriteIndex] then
  begin
    SpriteBudget.SpriteUsage[spriteIndex] := false;
    SpriteBudget.UsedSprites := SpriteBudget.UsedSprites - 1;
    SpriteBudget.AvailableSprites := SpriteBudget.AvailableSprites + 1;
  end;
end;
```

### Sprite Pool Management

**Reuse sprites efficiently:**

```pascal
type
  TSpritePool = class
  private
    Sprites: array[0..127] of TEntityID;
    Count: byte;
    MaxCount: byte;
  public
    constructor Create(maxSprites: byte);
    function Allocate: byte;
    procedure Release(spriteIndex: byte);
    function GetAvailable: byte;
  end;

implementation

constructor TSpritePool.Create(maxSprites: byte);
begin
  MaxCount := maxSprites;
  Count := 0;
  FillChar(Sprites, SizeOf(Sprites), 0);
end;

function TSpritePool.Allocate: byte;
begin
  if Count < MaxCount then
  begin
    Allocate := Count;
    Count := Count + 1;
  end
  else
    Allocate := $FF;  // No sprites available
end;

procedure TSpritePool.Release(spriteIndex: byte);
begin
  // Mark sprite as available (simplified)
  if spriteIndex < Count then
  begin
    // Could implement free list here
  end;
end;

function TSpritePool.GetAvailable: byte;
begin
  GetAvailable := MaxCount - Count;
end;
```

### Sprite Optimization

**Reduce sprite usage:**

```pascal
// ✅ GOOD: Reuse sprites
procedure RenderEntities;
var
  i: byte;
  spriteIndex: byte;
begin
  spriteIndex := 0;
  for i := 0 to EntityCount - 1 do
  begin
    if Entities[i].Visible and (spriteIndex < MAX_SPRITES) then
    begin
      SpriteSet(spriteIndex, Entities[i].X, Entities[i].Y, Entities[i].Tile);
      spriteIndex := spriteIndex + 1;
    end;
  end;
  
  // Hide unused sprites
  while spriteIndex < MAX_SPRITES do
  begin
    SpriteShow(spriteIndex, false);
    spriteIndex := spriteIndex + 1;
  end;
end;

// ❌ BAD: Allocate new sprites every frame
procedure RenderEntities;
var
  i: byte;
begin
  for i := 0 to EntityCount - 1 do
  begin
    var sprite := AllocateSprite;  // Allocates every frame!
    SpriteSet(sprite, Entities[i].X, Entities[i].Y, Entities[i].Tile);
  end;
end;
```

---

## Tilemap Budget Management

### Tilemap Memory Limits

**Track tilemap memory:**

```pascal
type
  TTilemapBudget = record
    MaxTiles: word;
    UsedTiles: word;
    AvailableTiles: word;
    TilemapMemory: word;  // Memory used in bytes
    MaxMemory: word;       // Maximum memory
  end;

var
  TilemapBudget: TTilemapBudget;

procedure InitTilemapBudget;
begin
  TilemapBudget.MaxTiles := 1024;  // 32x32
  TilemapBudget.MaxMemory := 2048;  // 2KB (2 bytes per tile)
  TilemapBudget.UsedTiles := 0;
  TilemapBudget.AvailableTiles := 1024;
  TilemapBudget.TilemapMemory := 0;
end;

function AllocateTilemap(width, height: word): boolean;
var
  tileCount: word;
  memoryNeeded: word;
begin
  tileCount := width * height;
  memoryNeeded := tileCount * 2;  // 2 bytes per tile
  
  if (TilemapBudget.UsedTiles + tileCount <= TilemapBudget.MaxTiles) and
     (TilemapBudget.TilemapMemory + memoryNeeded <= TilemapBudget.MaxMemory) then
  begin
    TilemapBudget.UsedTiles := TilemapBudget.UsedTiles + tileCount;
    TilemapBudget.AvailableTiles := TilemapBudget.AvailableTiles - tileCount;
    TilemapBudget.TilemapMemory := TilemapBudget.TilemapMemory + memoryNeeded;
    AllocateTilemap := true;
  end
  else
    AllocateTilemap := false;
end;
```

### Tilemap Optimization

**Optimize tilemap usage:**

```pascal
// ✅ GOOD: Reuse tilemaps
const
  LEVEL_TILEMAP = 0;  // Reuse same tilemap

procedure LoadLevel(levelID: byte);
begin
  // Load level data into existing tilemap
  LoadTilemapData(LEVEL_TILEMAP, levelData);
end;

// ❌ BAD: Create new tilemap for each level
procedure LoadLevel(levelID: byte);
begin
  var tilemap := AllocateTilemap(32, 32);  // Allocates every level!
  LoadTilemapData(tilemap, levelData);
end;
```

### Tilemap Compression

**Reduce tilemap memory:**

```pascal
// ✅ GOOD: Use tilemap compression
procedure CompressTilemap(tilemap: ^TTilemap);
begin
  // Use run-length encoding or similar
  // Store repeated tiles efficiently
end;

// ❌ BAD: Store every tile individually
// Wastes memory on repeated tiles
```

---

## Resource Monitoring

### Display Resource Usage

**Show current resource usage:**

```pascal
procedure RenderResourceUsage;
begin
  DrawText(10, 10, 'Sprites: ' + IntToStr(SpriteBudget.UsedSprites) + 
           '/' + IntToStr(SpriteBudget.MaxSprites));
  DrawText(10, 20, 'Tiles: ' + IntToStr(TilemapBudget.UsedTiles) + 
           '/' + IntToStr(TilemapBudget.MaxTiles));
  DrawText(10, 30, 'Memory: ' + IntToStr(TilemapBudget.TilemapMemory) + 
           '/' + IntToStr(TilemapBudget.MaxMemory));
  
  // Warnings
  if SpriteBudget.UsedSprites > SpriteBudget.MaxSprites * 3 div 4 then
    DrawText(10, 40, 'WARNING: High sprite usage!', 2);
  
  if TilemapBudget.UsedTiles > TilemapBudget.MaxTiles * 3 div 4 then
    DrawText(10, 50, 'WARNING: High tilemap usage!', 2);
end;
```

### Resource Warnings

**Warn when approaching limits:**

```pascal
function CheckSpriteBudget: boolean;
begin
  CheckSpriteBudget := SpriteBudget.AvailableSprites > 10;
  
  if SpriteBudget.AvailableSprites < 10 then
    WriteLn('WARNING: Low sprite budget!');
end;

function CheckTilemapBudget: boolean;
begin
  CheckTilemapBudget := TilemapBudget.AvailableTiles > 100;
  
  if TilemapBudget.AvailableTiles < 100 then
    WriteLn('WARNING: Low tilemap budget!');
end;
```

---

## Optimization Techniques

### Sprite Culling

**Only render visible sprites:**

```pascal
procedure RenderSprites;
var
  i: byte;
  spriteIndex: byte;
  cameraX, cameraY: integer;
begin
  GetCameraPosition(cameraX, cameraY);
  spriteIndex := 0;
  
  for i := 0 to EntityCount - 1 do
  begin
    // Cull sprites outside view
    if IsEntityVisible(Entities[i], cameraX, cameraY) and
       (spriteIndex < MAX_SPRITES) then
    begin
      SpriteSet(spriteIndex, Entities[i].X, Entities[i].Y, Entities[i].Tile);
      spriteIndex := spriteIndex + 1;
    end;
  end;
end;
```

### Sprite Batching

**Group sprite updates:**

```pascal
// ✅ GOOD: Batch sprite updates
procedure BatchUpdateSprites;
var
  i: byte;
begin
  // Update all sprites at once
  for i := 0 to ActiveSpriteCount - 1 do
  begin
    SpriteSet(i, SpriteData[i].X, SpriteData[i].Y, SpriteData[i].Tile);
  end;
end;

// ❌ BAD: Update sprites individually
procedure UpdateSprites;
var
  i: byte;
begin
  for i := 0 to EntityCount - 1 do
  begin
    SpriteSet(Entities[i].SpriteIndex, ...);  // One call per sprite
    WaitVBlank;  // Too slow!
  end;
end;
```

### Tilemap Streaming

**Load tilemaps on demand:**

```pascal
procedure StreamTilemap(levelID: byte);
begin
  // Unload previous tilemap
  UnloadTilemap(CURRENT_TILEMAP);
  
  // Load new tilemap
  LoadTilemap(CURRENT_TILEMAP, levelID);
end;
```

---

## Complete Example

**Putting it all together:**

```pascal
program ResourceBudgetDemo;

type
  TResourceManager = class
  private
    SpriteBudget: TSpriteBudget;
    TilemapBudget: TTilemapBudget;
  public
    constructor Create;
    function AllocateSprite: byte;
    procedure FreeSprite(spriteIndex: byte);
    function AllocateTilemap(w, h: word): boolean;
    procedure RenderUsage;
  end;

var
  ResourceManager: TResourceManager;

constructor TResourceManager.Create;
begin
  InitSpriteBudget;
  InitTilemapBudget;
end;

function TResourceManager.AllocateSprite: byte;
begin
  AllocateSprite := AllocateSprite;
end;

procedure TResourceManager.FreeSprite(spriteIndex: byte);
begin
  FreeSprite(spriteIndex);
end;

function TResourceManager.AllocateTilemap(w, h: word): boolean;
begin
  AllocateTilemap := AllocateTilemap(w, h);
end;

procedure TResourceManager.RenderUsage;
begin
  RenderResourceUsage;
end;

begin
  InitGraphics;
  ResourceManager := TResourceManager.Create;
  
  // Allocate resources
  var sprite1 := ResourceManager.AllocateSprite;
  var sprite2 := ResourceManager.AllocateSprite;
  
  ResourceManager.AllocateTilemap(32, 32);
  
  while true do
  begin
    UpdateGame;
    RenderGame;
    ResourceManager.RenderUsage;  // Show resource usage
    WaitVBlank;
  end;
end.
```

---

## Best Practices

### 1. Plan Resource Usage

**Design within limits:**

```pascal
// ✅ GOOD: Plan ahead
const
  MAX_PLAYER_SPRITES = 1;
  MAX_ENEMY_SPRITES = 20;
  MAX_PARTICLE_SPRITES = 10;
  TOTAL_SPRITES = MAX_PLAYER_SPRITES + MAX_ENEMY_SPRITES + MAX_PARTICLE_SPRITES;
  // TOTAL_SPRITES = 31 < 128 ✓

// ❌ BAD: No planning
// Hope sprites fit
```

### 2. Reuse Resources

**Don't allocate every frame:**

```pascal
// ✅ GOOD: Reuse sprites
var
  PlayerSprite: byte;  // Allocated once
begin
  PlayerSprite := AllocateSprite;
  // Reuse PlayerSprite every frame
end;

// ❌ BAD: Allocate every frame
begin
  var sprite := AllocateSprite;  // Every frame!
end;
```

### 3. Monitor Usage

**Track resource consumption:**

```pascal
// ✅ GOOD: Always monitor
RenderResourceUsage;  // Every frame

// ❌ BAD: Only check when problems
// Miss resource leaks
```

### 4. Set Limits

**Enforce maximum usage:**

```pascal
// ✅ GOOD: Enforce limits
if SpriteBudget.UsedSprites >= MAX_SPRITES then
begin
  WriteLn('ERROR: Sprite budget exceeded!');
  Exit;
end;

// ❌ BAD: No limits
// May exceed hardware limits
```

### 5. Optimize Early

**Optimize during development:**

```pascal
// ✅ GOOD: Optimize as you go
// Check sprite usage regularly

// ❌ BAD: Optimize at end
// May need major redesign
```

---

## Exercises

### Exercise 1: Sprite Budget

Write a program that:
1. Tracks sprite usage
2. Allocates and frees sprites
3. Monitors sprite budget
4. Warns when approaching limits

### Exercise 2: Tilemap Budget

Write a program that:
1. Tracks tilemap memory
2. Allocates tilemaps
3. Monitors tilemap budget
4. Manages tilemap memory

### Exercise 3: Resource Management

Write a program that:
1. Manages both sprites and tilemaps
2. Tracks all resources
3. Optimizes resource usage
4. Stays within budgets

### Exercise 4: Optimization

Write a program that:
1. Has resource problems
2. Implements optimization techniques
3. Reduces resource usage
4. Stays within hardware limits

---

**Previous Section:** [Performance Metrics](./02_PerformanceMetrics.md)  
**Next Chapter:** [Chapter 25: File I/O, Save Systems, and Packaging](../25_FileIOSaveSystemsAndPackaging/README.md)  
**Language Specification:** See [Memory and I/O](../../languageSpecification/12_MemoryAndIO.md)  
**Last Updated:** 2025-01-XX

