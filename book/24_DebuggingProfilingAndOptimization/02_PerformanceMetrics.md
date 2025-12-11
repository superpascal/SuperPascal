# Performance Metrics

**Part of:** [Chapter 24: Debugging, Profiling, and Optimization](./README.md)

---

## Introduction

Performance metrics help you understand how your game runs and identify bottlenecks. This section teaches you how to measure frame time, CPU usage, memory consumption, and optimize accordingly.

**Key concepts:**
- **Frame time** — Time per frame (target: 16.67ms at 60 FPS)
- **CPU usage** — Percentage of CPU used
- **Memory usage** — RAM consumption
- **Profiling** — Finding slow code
- **Bottlenecks** — Performance problems

---

## Understanding Performance

### What is Performance?

**Performance is how fast your game runs:**

- **Frame rate** — Frames per second (target: 60 FPS)
- **Frame time** — Time per frame (target: 16.67ms)
- **CPU usage** — How much CPU is used
- **Memory usage** — How much RAM is used

**Performance goals:**
- **60 FPS** — Smooth gameplay
- **16.67ms per frame** — Time budget per frame
- **< 80% CPU** — Leave headroom for audio, I/O
- **< 80% memory** — Leave room for dynamic allocation

### Why Measure Performance?

**Measurement helps you:**
- **Find bottlenecks** — Identify slow code
- **Optimize effectively** — Focus on real problems
- **Maintain performance** — Catch regressions
- **Plan resources** — Know your limits

---

## Frame Time Measurement

### Measuring Frame Time

**Track time per frame:**

```pascal
type
  TFrameTimer = record
    FrameStart: word;  // Frame start time (in CPU cycles or timer ticks)
    FrameTime: word;   // Frame duration
    FrameCount: word;  // Total frames
    TotalTime: dword;  // Total time elapsed
  end;

var
  FrameTimer: TFrameTimer;

procedure StartFrame;
begin
  FrameTimer.FrameStart := GetTimerValue;  // Get current time
end;

procedure EndFrame;
begin
  FrameTimer.FrameTime := GetTimerValue - FrameTimer.FrameStart;
  FrameTimer.FrameCount := FrameTimer.FrameCount + 1;
  FrameTimer.TotalTime := FrameTimer.TotalTime + FrameTimer.FrameTime;
end;

function GetAverageFrameTime: word;
begin
  if FrameTimer.FrameCount > 0 then
    GetAverageFrameTime := FrameTimer.TotalTime div FrameTimer.FrameCount
  else
    GetAverageFrameTime := 0;
end;

function GetFPS: word;
var
  avgFrameTime: word;
begin
  avgFrameTime := GetAverageFrameTime;
  if avgFrameTime > 0 then
    GetFPS := 60000 div avgFrameTime  // 60000ms / frame time = FPS
  else
    GetFPS := 0;
end;
```

### Displaying Frame Time

**Show frame time on screen:**

```pascal
procedure RenderFrameTime;
var
  fps: word;
  frameTime: word;
begin
  fps := GetFPS;
  frameTime := FrameTimer.FrameTime;
  
  DrawText(10, 10, 'FPS: ' + IntToStr(fps));
  DrawText(10, 20, 'Frame: ' + IntToStr(frameTime) + 'ms');
  
  // Warning if frame time too high
  if frameTime > 20 then
    DrawText(10, 30, 'WARNING: Frame time high!', 2);  // Red color
end;

// In game loop
begin
  while true do
  begin
    StartFrame;
    
    UpdateGame;
    RenderGame;
    RenderFrameTime;  // Show metrics
    
    EndFrame;
    WaitVBlank;
  end;
end.
```

---

## CPU Profiling

### Profiling System Time

**Measure time spent in systems:**

```pascal
type
  TSystemProfile = record
    SystemName: string;
    StartTime: word;
    TotalTime: dword;
    CallCount: word;
    AverageTime: word;
  end;

var
  Profiles: array[0..15] of TSystemProfile;
  ProfileCount: byte;

procedure StartProfile(systemName: string);
var
  i: byte;
begin
  // Find or create profile
  i := 0;
  while (i < ProfileCount) and (Profiles[i].SystemName <> systemName) do
    i := i + 1;
  
  if i >= ProfileCount then
  begin
    if ProfileCount < 16 then
    begin
      Profiles[ProfileCount].SystemName := systemName;
      Profiles[ProfileCount].TotalTime := 0;
      Profiles[ProfileCount].CallCount := 0;
      i := ProfileCount;
      ProfileCount := ProfileCount + 1;
    end
    else
      Exit;  // No room
  end;
  
  Profiles[i].StartTime := GetTimerValue;
end;

procedure EndProfile(systemName: string);
var
  i: byte;
  elapsed: word;
begin
  i := 0;
  while (i < ProfileCount) and (Profiles[i].SystemName <> systemName) do
    i := i + 1;
  
  if i < ProfileCount then
  begin
    elapsed := GetTimerValue - Profiles[i].StartTime;
    Profiles[i].TotalTime := Profiles[i].TotalTime + elapsed;
    Profiles[i].CallCount := Profiles[i].CallCount + 1;
    Profiles[i].AverageTime := Profiles[i].TotalTime div Profiles[i].CallCount;
  end;
end;

procedure RenderProfiles;
var
  i: byte;
  y: integer;
begin
  y := 50;
  for i := 0 to ProfileCount - 1 do
  begin
    DrawText(10, y, Profiles[i].SystemName + ': ' + 
             IntToStr(Profiles[i].AverageTime) + 'ms');
    y := y + 10;
  end;
end;
```

### Using Profiling

**Profile game systems:**

```pascal
procedure GameLoop;
begin
  while true do
  begin
    StartFrame;
    
    StartProfile('Input');
    HandleInput;
    EndProfile('Input');
    
    StartProfile('Update');
    UpdateGame;
    EndProfile('Update');
    
    StartProfile('Physics');
    UpdatePhysics;
    EndProfile('Physics');
    
    StartProfile('Render');
    RenderGame;
    EndProfile('Render');
    
    RenderProfiles;  // Show system times
    
    EndFrame;
    WaitVBlank;
  end;
end;
```

---

## Memory Profiling

### Tracking Memory Usage

**Monitor memory consumption:**

```pascal
type
  TMemoryStats = record
    TotalMemory: word;      // Total available
    UsedMemory: word;       // Currently used
    FreeMemory: word;       // Available
    PeakMemory: word;       // Maximum used
    AllocationCount: word;  // Number of allocations
  end;

var
  MemoryStats: TMemoryStats;

procedure InitMemoryStats;
begin
  MemoryStats.TotalMemory := GetTotalMemory;
  MemoryStats.UsedMemory := GetUsedMemory;
  MemoryStats.FreeMemory := MemoryStats.TotalMemory - MemoryStats.UsedMemory;
  MemoryStats.PeakMemory := MemoryStats.UsedMemory;
  MemoryStats.AllocationCount := 0;
end;

procedure UpdateMemoryStats;
var
  used: word;
begin
  used := GetUsedMemory;
  MemoryStats.UsedMemory := used;
  MemoryStats.FreeMemory := MemoryStats.TotalMemory - used;
  
  if used > MemoryStats.PeakMemory then
    MemoryStats.PeakMemory := used;
end;

procedure RenderMemoryStats;
begin
  DrawText(10, 200, 'Memory: ' + IntToStr(MemoryStats.UsedMemory) + 
           '/' + IntToStr(MemoryStats.TotalMemory));
  DrawText(10, 210, 'Free: ' + IntToStr(MemoryStats.FreeMemory));
  DrawText(10, 220, 'Peak: ' + IntToStr(MemoryStats.PeakMemory));
  
  // Warning if memory low
  if MemoryStats.FreeMemory < MemoryStats.TotalMemory div 4 then
    DrawText(10, 230, 'WARNING: Low memory!', 2);
end;
```

### Memory Leak Detection

**Track allocations and deallocations:**

```pascal
type
  TAllocation = record
    Address: pointer;
    Size: word;
    Allocated: boolean;
  end;

var
  Allocations: array[0..255] of TAllocation;
  AllocationCount: word;

procedure TrackAllocation(addr: pointer; size: word);
begin
  if AllocationCount < 256 then
  begin
    Allocations[AllocationCount].Address := addr;
    Allocations[AllocationCount].Size := size;
    Allocations[AllocationCount].Allocated := true;
    AllocationCount := AllocationCount + 1;
  end;
end;

procedure TrackDeallocation(addr: pointer);
var
  i: word;
begin
  for i := 0 to AllocationCount - 1 do
  begin
    if Allocations[i].Address = addr then
    begin
      Allocations[i].Allocated := false;
      Break;
    end;
  end;
end;

procedure CheckMemoryLeaks;
var
  i: word;
  leakCount: word;
begin
  leakCount := 0;
  for i := 0 to AllocationCount - 1 do
  begin
    if Allocations[i].Allocated then
    begin
      leakCount := leakCount + 1;
      WriteLn('Leak: ', Allocations[i].Address, ' size ', Allocations[i].Size);
    end;
  end;
  
  if leakCount > 0 then
    WriteLn('Total leaks: ', leakCount);
end;
```

---

## Finding Bottlenecks

### Identifying Slow Code

**Common bottlenecks:**

1. **Nested loops** — O(n²) or worse complexity
2. **Unnecessary allocations** — Creating objects every frame
3. **Redundant calculations** — Computing same value repeatedly
4. **Inefficient algorithms** — Wrong data structures
5. **Too many entities** — Processing too many objects

### Profiling Example

**Find slow system:**

```pascal
procedure UpdateGame;
begin
  StartProfile('Update');
  
  StartProfile('Entities');
  UpdateEntities;  // Might be slow
  EndProfile('Entities');
  
  StartProfile('Collisions');
  UpdateCollisions;  // Might be slow
  EndProfile('Collisions');
  
  StartProfile('AI');
  UpdateAI;  // Might be slow
  EndProfile('AI');
  
  EndProfile('Update');
end;
```

**After profiling:**
- If `Entities` takes 10ms → Optimize entity updates
- If `Collisions` takes 8ms → Optimize collision detection
- If `AI` takes 2ms → AI is fine

### Optimization Strategy

**Optimize based on profiling:**

1. **Measure first** — Don't guess what's slow
2. **Focus on biggest** — Optimize largest time consumers
3. **Verify improvement** — Measure after optimization
4. **Iterate** — Repeat until performance acceptable

---

## Performance Targets

### Frame Time Budget

**Allocate time per system:**

```
Total frame time: 16.67ms (60 FPS)

Input:        0.5ms
Update:       2.0ms
Physics:      3.0ms
Collisions:   4.0ms
AI:           2.0ms
Animation:    1.0ms
Render:       3.0ms
Audio:        0.5ms
Other:        0.67ms
```

**If over budget:**
- Optimize systems taking most time
- Reduce entity count
- Simplify algorithms
- Use more efficient data structures

### Memory Budget

**Allocate memory per system:**

```
Total memory: 512KB

Code:         200KB
Data:         100KB
Entities:     50KB
Graphics:     100KB
Audio:        30KB
Stack:        20KB
Heap:         12KB (free)
```

**If over budget:**
- Reduce entity count
- Compress assets
- Reuse memory
- Optimize data structures

---

## Complete Example

**Putting it all together:**

```pascal
program PerformanceDemo;

type
  TPerformanceMonitor = class
  private
    FrameStart: word;
    FrameTime: word;
    FPS: word;
    SystemTimes: array[0..7] of word;
  public
    procedure StartFrame;
    procedure EndFrame;
    procedure StartSystem(systemIndex: byte);
    procedure EndSystem(systemIndex: byte);
    procedure Render;
  end;

var
  Monitor: TPerformanceMonitor;

procedure TPerformanceMonitor.StartFrame;
begin
  FrameStart := GetTimerValue;
end;

procedure TPerformanceMonitor.EndFrame;
var
  elapsed: word;
begin
  elapsed := GetTimerValue - FrameStart;
  FrameTime := elapsed;
  
  if elapsed > 0 then
    FPS := 60000 div elapsed
  else
    FPS := 0;
end;

procedure TPerformanceMonitor.StartSystem(systemIndex: byte);
begin
  // Store start time (simplified)
end;

procedure TPerformanceMonitor.EndSystem(systemIndex: byte);
begin
  // Calculate system time (simplified)
end;

procedure TPerformanceMonitor.Render;
begin
  DrawText(10, 10, 'FPS: ' + IntToStr(FPS));
  DrawText(10, 20, 'Frame: ' + IntToStr(FrameTime) + 'ms');
  
  if FrameTime > 20 then
    DrawText(10, 30, 'WARNING: Slow frame!', 2);
end;

begin
  InitGraphics;
  Monitor := TPerformanceMonitor.Create;
  
  while true do
  begin
    Monitor.StartFrame;
    
    UpdateGame;
    RenderGame;
    Monitor.Render;
    
    Monitor.EndFrame;
    WaitVBlank;
  end;
end.
```

---

## Best Practices

### 1. Measure Before Optimizing

**Don't guess what's slow:**

```pascal
// ✅ GOOD: Profile first
StartProfile('System');
System.Update;
EndProfile('System');
// Then optimize if needed

// ❌ BAD: Optimize blindly
// Optimize without knowing if it's slow
```

### 2. Focus on Bottlenecks

**Optimize biggest time consumers:**

```pascal
// ✅ GOOD: Optimize slow system
// Collisions take 8ms → Optimize collisions

// ❌ BAD: Optimize fast system
// Input takes 0.1ms → Don't optimize (waste of time)
```

### 3. Set Performance Targets

**Know your goals:**

```pascal
// ✅ GOOD: Clear targets
const
  TARGET_FPS = 60;
  TARGET_FRAME_TIME = 16;  // ms

// ❌ BAD: No targets
// Just hope it's fast enough
```

### 4. Monitor Continuously

**Track performance over time:**

```pascal
// ✅ GOOD: Always monitor
RenderFrameTime;  // Every frame

// ❌ BAD: Only check when problems
// Miss performance regressions
```

### 5. Test on Real Hardware

**Emulator may be faster:**

```pascal
// ✅ GOOD: Test on hardware
// Real performance may differ

// ❌ BAD: Only test in emulator
// May have different performance
```

---

## Exercises

### Exercise 1: Frame Time Measurement

Write a program that:
1. Measures frame time
2. Calculates FPS
3. Displays metrics on screen
4. Warns if frame time too high

### Exercise 2: System Profiling

Write a program that:
1. Profiles multiple systems
2. Tracks time per system
3. Displays system times
4. Identifies slow systems

### Exercise 3: Memory Tracking

Write a program that:
1. Tracks memory usage
2. Monitors allocations
3. Detects memory leaks
4. Displays memory stats

### Exercise 4: Performance Optimization

Write a program that:
1. Has performance problems
2. Profiles to find bottlenecks
3. Optimizes slow code
4. Verifies improvements

---

**Previous Section:** [Using the Debugger](./01_UsingTheDebugger.md)  
**Next Section:** [Sprite and Tilemap Budgets](./03_SpriteAndTilemapBudgets.md)  
**Language Specification:** See [Memory and I/O](../../languageSpecification/12_MemoryAndIO.md)  
**Last Updated:** 2025-01-XX

