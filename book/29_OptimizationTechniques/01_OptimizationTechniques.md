# Optimization Techniques

**Part of:** [Chapter 29: Optimization Techniques](./README.md)

---

## Introduction

Optimization makes programs faster and more efficient. This section teaches you how to optimize algorithms, data structures, and memory usage, how to profile and measure performance, and how to apply platform-specific optimizations.

**Key concepts:**
- **Algorithmic optimization** — Choosing efficient algorithms
- **Data structure optimization** — Selecting appropriate structures
- **Memory optimization** — Reducing memory usage
- **Profiling** — Measuring performance
- **Platform-specific** — Optimizing for target hardware

---

## Algorithmic Optimization

### Choose the Right Algorithm

**Algorithm selection matters:**

```pascal
// ❌ BAD: O(n²) bubble sort
procedure BubbleSort(var arr: array of integer);
var
  i, j, temp: integer;
begin
  for i := 0 to Length(arr) - 2 do
    for j := 0 to Length(arr) - 2 - i do
      if arr[j] > arr[j + 1] then
      begin
        temp := arr[j];
        arr[j] := arr[j + 1];
        arr[j + 1] := temp;
      end;
end;

// ✅ GOOD: O(n log n) quicksort
procedure QuickSort(var arr: array of integer; left, right: integer);
// ... (see Section 01)
end;
```

### Early Exit

**Stop when answer is found:**

```pascal
// ❌ BAD: Always checks all elements
function Contains(arr: array of integer; value: integer): boolean;
var
  i: integer;
begin
  Contains := false;
  for i := 0 to Length(arr) - 1 do
    if arr[i] = value then
      Contains := true;
end;

// ✅ GOOD: Exits early
function Contains(arr: array of integer; value: integer): boolean;
var
  i: integer;
begin
  for i := 0 to Length(arr) - 1 do
    if arr[i] = value then
    begin
      Contains := true;
      Exit;
    end;
  Contains := false;
end;
```

### Caching Results

**Avoid redundant calculations:**

```pascal
// ❌ BAD: Recalculates every time
function Distance(x1, y1, x2, y2: Q8.8): Q8.8;
begin
  Distance := Sqrt(Sqr(x2 - x1) + Sqr(y2 - y1));
end;

// ✅ GOOD: Cache if same inputs
var
  LastX1, LastY1, LastX2, LastY2: Q8.8;
  LastDistance: Q8.8;
  DistanceCached: boolean;

function Distance(x1, y1, x2, y2: Q8.8): Q8.8;
begin
  if DistanceCached and 
     (x1 = LastX1) and (y1 = LastY1) and
     (x2 = LastX2) and (y2 = LastY2) then
  begin
    Distance := LastDistance;
    Exit;
  end;
  
  LastDistance := Sqrt(Sqr(x2 - x1) + Sqr(y2 - y1));
  LastX1 := x1;
  LastY1 := y1;
  LastX2 := x2;
  LastY2 := y2;
  DistanceCached := true;
  Distance := LastDistance;
end;
```

### Loop Optimization

**Optimize loop operations:**

```pascal
// ❌ BAD: Calls Length() every iteration
for i := 0 to Length(arr) - 1 do
  Process(arr[i]);

// ✅ GOOD: Cache length
var
  len: integer;
begin
  len := Length(arr);
  for i := 0 to len - 1 do
    Process(arr[i]);
end;

// ❌ BAD: Nested loops with repeated calculations
for i := 0 to n - 1 do
  for j := 0 to n - 1 do
    result := result + arr[i] * arr[j] * SomeExpensiveFunction(i, j);

// ✅ GOOD: Calculate once
for i := 0 to n - 1 do
begin
  var cached := SomeExpensiveFunction(i, 0);
  for j := 0 to n - 1 do
    result := result + arr[i] * arr[j] * cached;
end;
```

---

## Data Structure Optimization

### Choose Efficient Structures

**Match structure to access patterns:**

```pascal
// ❌ BAD: Array for frequent lookups
function FindPlayer(players: array of TPlayer; id: integer): ^TPlayer;
var
  i: integer;
begin
  for i := 0 to Length(players) - 1 do
    if players[i].ID = id then
    begin
      FindPlayer := @players[i];
      Exit;
    end;
  FindPlayer := nil;
end;

// ✅ GOOD: Hash table for fast lookup
var
  PlayerTable: THashTable;
function FindPlayer(id: integer): ^TPlayer;
var
  index: integer;
begin
  if HashLookup(PlayerTable, id, index) then
    FindPlayer := @Players[index]
  else
    FindPlayer := nil;
end;
```

### Structure-of-Arrays (SoA)

**Better cache performance:**

```pascal
// ❌ BAD: Array-of-Structures (AoS)
type
  TEntity = record
    X, Y: Q8.8;
    VelocityX, VelocityY: Q8.8;
    Sprite: byte;
  end;
  
var Entities: array[0..99] of TEntity;

// ✅ GOOD: Structure-of-Arrays (SoA)
type
  TEntityData = record
    X, Y: array[0..99] of Q8.8;
    VelocityX, VelocityY: array[0..99] of Q8.8;
    Sprite: array[0..99] of byte;
  end;
  
var EntityData: TEntityData;

// SoA is better when processing all entities
procedure UpdateAllEntities;
var
  i: integer;
begin
  for i := 0 to EntityCount - 1 do
  begin
    EntityData.X[i] := EntityData.X[i] + EntityData.VelocityX[i];
    EntityData.Y[i] := EntityData.Y[i] + EntityData.VelocityY[i];
  end;
end;
```

### Pre-allocation

**Avoid repeated allocations:**

```pascal
// ❌ BAD: Allocate every frame
procedure RenderFrame;
var
  sprites: array of TSprite;
begin
  SetLength(sprites, 100);  // Allocates every frame
  // ... render ...
end;

// ✅ GOOD: Pre-allocate once
var
  SpritePool: array[0..99] of TSprite;
  SpritePoolSize: integer = 0;

procedure InitSpritePool;
begin
  SpritePoolSize := 100;
  // Initialize pool
end;

procedure RenderFrame;
begin
  // Use pre-allocated pool
  // ... render ...
end;
```

---

## Memory Optimization

### Reduce Memory Usage

**Use smaller data types:**

```pascal
// ❌ BAD: Using larger types than needed
var
  Health: integer;  // 16-bit, but only needs 0-100
  Score: integer;   // 16-bit, but only needs 0-255

// ✅ GOOD: Use appropriate sizes
var
  Health: byte;     // 8-bit, 0-255
  Score: byte;      // 8-bit, 0-255
```

### Pack Records

**Reduce padding:**

```pascal
// ❌ BAD: Unpacked (may have padding)
type
  TEntity = record
    Active: boolean;  // 1 byte + 7 bytes padding
    X, Y: Q8.8;       // 4 bytes each
    Sprite: byte;     // 1 byte + 3 bytes padding
  end;  // Total: ~16 bytes

// ✅ GOOD: Packed (no padding)
type
  TEntity = packed record
    Active: boolean;
    X, Y: Q8.8;
    Sprite: byte;
  end;  // Total: ~10 bytes
```

### Object Pooling

**Reuse objects:**

```pascal
type
  TProjectilePool = record
    Projectiles: array[0..99] of TProjectile;
    FreeList: array[0..99] of integer;
    FreeCount: integer;
  end;

function AllocateProjectile(var pool: TProjectilePool): ^TProjectile;
begin
  if pool.FreeCount > 0 then
  begin
    pool.FreeCount := pool.FreeCount - 1;
    AllocateProjectile := @pool.Projectiles[pool.FreeList[pool.FreeCount]];
  end
  else
    AllocateProjectile := nil;  // Pool exhausted
end;

procedure FreeProjectile(var pool: TProjectilePool; proj: ^TProjectile);
var
  index: integer;
begin
  index := proj - @pool.Projectiles[0];  // Calculate index
  pool.FreeList[pool.FreeCount] := index;
  pool.FreeCount := pool.FreeCount + 1;
end;
```

### Memory Alignment

**Consider platform alignment:**

```pascal
// On 8-bit platforms, alignment matters less
// But still consider for cache performance

// ✅ GOOD: Group related data
type
  TPosition = record
    X, Y: Q8.8;  // 4 bytes together
  end;
  
  TVelocity = record
    VX, VY: Q8.8;  // 4 bytes together
  end;
```

---

## Profiling and Measurement

### Measure Before Optimizing

**Profile to find bottlenecks:**

```pascal
var
  FrameStartTime: word;
  FrameTime: word;
  UpdateTime: word;
  RenderTime: word;

procedure StartFrame;
begin
  FrameStartTime := GetTimerValue;
end;

procedure StartUpdate;
begin
  UpdateTime := GetTimerValue;
end;

procedure EndUpdate;
begin
  UpdateTime := GetTimerValue - UpdateTime;
end;

procedure StartRender;
begin
  RenderTime := GetTimerValue;
end;

procedure EndRender;
begin
  RenderTime := GetTimerValue - RenderTime;
end;

procedure EndFrame;
begin
  FrameTime := GetTimerValue - FrameStartTime;
  
  if DEBUG_MODE then
  begin
    WriteLn('Frame: ', FrameTime, 'ms');
    WriteLn('Update: ', UpdateTime, 'ms');
    WriteLn('Render: ', RenderTime, 'ms');
  end;
end;
```

### Identify Hotspots

**Find slow code:**

```pascal
procedure ProfileFunction(func: procedure);
var
  start, elapsed: word;
begin
  start := GetTimerValue;
  func;
  elapsed := GetTimerValue - start;
  
  if elapsed > THRESHOLD then
    WriteLn('Slow function: ', elapsed, 'ms');
end;
```

### Benchmark Comparisons

**Compare optimization effectiveness:**

```pascal
procedure BenchmarkSorts;
var
  arr1, arr2: array[0..999] of integer;
  start, elapsed1, elapsed2: word;
  i: integer;
begin
  // Prepare test data
  for i := 0 to 999 do
  begin
    arr1[i] := Random(1000);
    arr2[i] := arr1[i];
  end;
  
  // Benchmark bubble sort
  start := GetTimerValue;
  BubbleSort(arr1);
  elapsed1 := GetTimerValue - start;
  
  // Benchmark quicksort
  start := GetTimerValue;
  QuickSort(arr2, 0, 999);
  elapsed2 := GetTimerValue - start;
  
  WriteLn('Bubble sort: ', elapsed1, 'ms');
  WriteLn('Quick sort: ', elapsed2, 'ms');
  WriteLn('Speedup: ', elapsed1 div elapsed2, 'x');
end;
```

---

## Platform-Specific Optimizations

### Z80 Optimizations

**8-bit platform considerations:**

```pascal
// ✅ GOOD: Use 8-bit operations when possible
var
  counter: byte;  // Faster than integer on Z80
begin
  for counter := 0 to 255 do
    Process(counter);
end;

// ✅ GOOD: Minimize 16-bit operations
// Use byte arithmetic when possible
var
  a, b, result: byte;
begin
  result := a + b;  // 8-bit add (faster)
end;
```

### Memory Banking

**Handle memory constraints:**

```pascal
// On platforms with memory banking
procedure LoadLevelData(level: byte);
begin
  // Switch to level data bank
  MapPage(LEVEL_DATA_BANK);
  
  // Load data
  LoadTiles;
  LoadSprites;
  
  // Switch back
  MapPage(MAIN_BANK);
end;
```

### Cache Considerations

**Optimize for cache:**

```pascal
// ✅ GOOD: Sequential access (cache-friendly)
for i := 0 to Length(arr) - 1 do
  Process(arr[i]);

// ❌ BAD: Random access (cache-unfriendly)
for i := 0 to Length(indices) - 1 do
  Process(arr[indices[i]]);  // May cause cache misses
```

### Fixed-Point Optimization

**Use fixed-point efficiently:**

```pascal
// ✅ GOOD: Pre-calculate constants
const
  GRAVITY = Q8_8(0.3);
  FRICTION = Q8_8(0.9);

// ✅ GOOD: Avoid repeated conversions
var
  velocity: Q8.8;
begin
  velocity := velocity * FRICTION;  // Direct fixed-point math
  // Instead of: velocity := Q8_8(Float(velocity) * 0.9);
end;
```

---

## Best Practices

### 1. Profile First

**Measure before optimizing:**

```pascal
// ✅ GOOD: Profile to find bottlenecks
ProfileGame;
// Optimize slowest parts first

// ❌ BAD: Optimize without profiling
// May optimize wrong parts
```

### 2. Optimize Hot Paths

**Focus on frequently executed code:**

```pascal
// ✅ GOOD: Optimize inner loops
for i := 0 to EntityCount - 1 do
begin
  // This runs every frame - optimize here
  UpdateEntity(Entities[i]);
end;

// ❌ BAD: Optimize rarely called code
procedure InitializeGame;  // Called once - don't over-optimize
```

### 3. Don't Prematurely Optimize

**Keep code readable:**

```pascal
// ✅ GOOD: Clear code first
function CalculateDistance(x1, y1, x2, y2: Q8.8): Q8.8;
begin
  CalculateDistance := Sqrt(Sqr(x2 - x1) + Sqr(y2 - y1));
end;

// Then optimize if needed
// Don't make code unreadable for small gains
```

### 4. Measure Improvements

**Verify optimizations work:**

```pascal
// ✅ GOOD: Benchmark before and after
var before := BenchmarkFunction;
OptimizeFunction;
var after := BenchmarkFunction;
WriteLn('Improvement: ', before div after, 'x');
```

### 5. Consider Trade-offs

**Balance speed, memory, clarity:**

```pascal
// Sometimes clarity > speed
// Sometimes speed > memory
// Choose based on requirements
```

---

## Exercises

### Exercise 1: Algorithm Optimization

Write a program that:
1. Implements two versions of an algorithm
2. Benchmarks both versions
3. Identifies performance differences
4. Explains why one is faster

### Exercise 2: Data Structure Optimization

Write a program that:
1. Uses inefficient data structure
2. Profiles performance
3. Switches to efficient structure
4. Measures improvement

### Exercise 3: Memory Optimization

Write a program that:
1. Uses object pooling
2. Reduces memory allocations
3. Measures memory usage
4. Compares with non-pooled version

### Exercise 4: Profiling

Write a program that:
1. Profiles different operations
2. Identifies bottlenecks
3. Optimizes hot paths
4. Measures improvements

---

**Previous Chapter:** [Chapter 28: Advanced Data Structures](../28_AdvancedDataStructures/README.md)  
**Next Chapter:** [Chapter 30: Advanced Patterns](../30_AdvancedPatterns/README.md)  
**Last Updated:** 2025-01-XX

