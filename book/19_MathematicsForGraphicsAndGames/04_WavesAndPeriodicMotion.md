# Waves and Periodic Motion

**Part of:** [Chapter 19: Mathematics for Graphics and Games](./README.md)

---

## Introduction

Waves and periodic motion create smooth, repeating animations and effects. This section teaches you how to use sine and cosine functions to create waves, periodic motion, and smooth animations.

**Key concepts:**
- **Sine waves** — Basic wave pattern
- **Periodic functions** — Repeating patterns
- **Animation curves** — Smooth transitions
- **Wave parameters** — Amplitude, frequency, phase
- **Applications** — Floating, pulsing, rotating

---

## Understanding Waves

### What is a Wave?

**A wave is a repeating pattern:**

```
Sine wave:
     /\
    /  \    /\
   /    \  /  \
  /      \/    \
 /              \
```

**Properties:**
- **Amplitude** — Height of wave (how high/low)
- **Frequency** — How often it repeats (cycles per second)
- **Phase** — Starting position (offset)
- **Period** — Time for one complete cycle

### Sine and Cosine

**Sine and cosine are periodic functions:**

- **Sin(θ)** — Starts at 0, goes up to 1, down to -1, back to 0
- **Cos(θ)** — Starts at 1, goes down to -1, up to 1, back to 1

**Relationship:**
- `Cos(θ) = Sin(θ + π/2)` — Cosine is sine shifted by 90 degrees
- Both repeat every 2π radians (360 degrees)

---

## Basic Sine Wave

### Simple Sine Wave

**Create a basic sine wave:**

```pascal
var
  time: Q8.8;
  value: Q8.8;
begin
  time := 0;
  
  while true do
  begin
    value := Sin(time);
    // value oscillates between -1.0 and 1.0
    
    time := time + Q8_8(0.1);  // Increment time
    if time > PI * 2 then
      time := 0;  // Reset (one complete cycle)
    
    WaitVBlank;
  end;
end;
```

### Amplitude

**Control wave height with amplitude:**

```pascal
const
  AMPLITUDE = 50;  // Wave height

var
  time: Q8.8;
  y: integer;
begin
  time := 0;
  
  while true do
  begin
    // Sine wave with amplitude
    y := 120 + Round(AMPLITUDE * Sin(time));  // Center at 120, ±50
    
    time := time + Q8_8(0.1);
    WaitVBlank;
  end;
end;
```

**Formula:**
```
y = center + amplitude * sin(time)
```

### Frequency

**Control wave speed with frequency:**

```pascal
const
  FREQUENCY = 2.0;  // Cycles per second (2x speed)

var
  time: Q8.8;
  value: Q8.8;
begin
  time := 0;
  
  while true do
  begin
    // Higher frequency = faster oscillation
    value := Sin(time * FREQUENCY);
    
    time := time + Q8_8(0.1);
    WaitVBlank;
  end;
end;
```

**Formula:**
```
y = sin(time * frequency)
```

### Phase

**Shift wave with phase:**

```pascal
const
  PHASE = PI / 2;  // 90 degree shift

var
  time: Q8.8;
  value1, value2: Q8.8;
begin
  time := 0;
  
  while true do
  begin
    value1 := Sin(time);           // Normal sine
    value2 := Sin(time + PHASE);   // Shifted sine (cosine)
    
    time := time + Q8_8(0.1);
    WaitVBlank;
  end;
end;
```

**Formula:**
```
y = sin(time + phase)
```

---

## Complete Wave Formula

### General Wave Equation

**Combine all parameters:**

```pascal
function Wave(
  time: Q8.8;
  amplitude: Q8.8;
  frequency: Q8.8;
  phase: Q8.8;
  center: Q8.8
): Q8.8;
begin
  Wave := center + amplitude * Sin(time * frequency + phase);
end;
```

**Parameters:**
- `time` — Current time (increases each frame)
- `amplitude` — Wave height
- `frequency` — Cycles per unit time
- `phase` — Starting offset
- `center` — Center position

**Usage:**
```pascal
var
  time: Q8.8;
  y: integer;
begin
  time := 0;
  
  while true do
  begin
    y := Round(Wave(time, Q8_8(50.0), Q8_8(1.0), Q8_8(0.0), Q8_8(120.0)));
    // y oscillates between 70 and 170
    
    time := time + Q8_8(0.1);
    WaitVBlank;
  end;
end;
```

---

## Applications

### Floating Animation

**Make entity float up and down:**

```pascal
procedure UpdateFloating(entity: TEntityID);
var
  x, y: integer;
  time: Q8.8;
  floatY: Q8.8;
begin
  EntityGetPosition(entity, x, y);
  
  // Use frame count as time
  time := Q8_8(frameCount) * Q8_8(0.1);
  
  // Calculate floating offset
  floatY := Q8_8(10.0) * Sin(time);  // ±10 pixels
  
  // Update position
  EntitySetPosition(entity, x, Round(Q8_8(y) + floatY));
end;
```

### Pulsing Effect

**Make sprite pulse (scale up and down):**

```pascal
procedure UpdatePulsing(spriteIndex: byte);
var
  time: Q8.8;
  scale: Q8.8;
  baseScale: Q8.8;
begin
  time := Q8_8(frameCount) * Q8_8(0.05);
  baseScale := Q8_8(1.0);
  
  // Pulse between 0.8 and 1.2
  scale := baseScale + Q8_8(0.2) * Sin(time);
  
  // Apply scale to sprite
  SetSpriteScale(spriteIndex, scale);
end;
```

### Rotating Motion

**Use sine and cosine for circular motion:**

```pascal
procedure UpdateCircularMotion(entity: TEntityID);
var
  centerX, centerY: integer;
  radius: Q8.8;
  angle: Q8.8;
  x, y: integer;
begin
  centerX := 160;
  centerY := 120;
  radius := Q8_8(50.0);
  angle := Q8_8(frameCount) * Q8_8(0.1);
  
  // Circular motion using sin and cos
  x := centerX + Round(radius * Cos(angle));
  y := centerY + Round(radius * Sin(angle));
  
  EntitySetPosition(entity, x, y);
end;
```

### Breathing Effect

**Create breathing animation:**

```pascal
procedure UpdateBreathing(entity: TEntityID);
var
  time: Q8.8;
  scale: Q8.8;
begin
  time := Q8_8(frameCount) * Q8_8(0.02);  // Slow oscillation
  
  // Breathe between 0.9 and 1.1
  scale := Q8_8(1.0) + Q8_8(0.1) * Sin(time);
  
  SetEntityScale(entity, scale);
end;
```

---

## Advanced Wave Patterns

### Multiple Waves

**Combine multiple waves:**

```pascal
function CombinedWave(time: Q8.8): Q8.8;
begin
  // Combine two waves
  CombinedWave := 
    Q8_8(20.0) * Sin(time) +
    Q8_8(10.0) * Sin(time * Q8_8(2.0));  // Higher frequency
end;
```

### Damped Oscillation

**Wave that decreases over time:**

```pascal
function DampedWave(time, damping: Q8.8): Q8.8;
begin
  // Wave amplitude decreases over time
  DampedWave := Exp(-damping * time) * Sin(time);
end;
```

### Square Wave

**Create square wave pattern:**

```pascal
function SquareWave(time: Q8.8): Q8.8;
begin
  if Sin(time) >= 0 then
    SquareWave := Q8_8(1.0)
  else
    SquareWave := Q8_8(-1.0);
end;
```

### Triangle Wave

**Create triangle wave:**

```pascal
function TriangleWave(time: Q8.8): Q8.8;
var
  normalized: Q8.8;
begin
  // Normalize to 0-1 range
  normalized := (Sin(time) + Q8_8(1.0)) / Q8_8(2.0);
  
  // Create triangle
  if normalized < Q8_8(0.5) then
    TriangleWave := normalized * Q8_8(4.0) - Q8_8(1.0)  // 0 to 1
  else
    TriangleWave := Q8_8(3.0) - normalized * Q8_8(4.0);  // 1 to -1
end;
```

---

## Animation Curves

### Easing Functions

**Use waves for easing:**

```pascal
function EaseInOutSine(t: Q8.8): Q8.8;
begin
  // Smooth ease in/out using sine
  EaseInOutSine := (Q8_8(1.0) - Cos(t * PI)) / Q8_8(2.0);
end;
```

### Smooth Transitions

**Smooth transition between values:**

```pascal
function SmoothTransition(start, target: Q8.8; t: Q8.8): Q8.8;
begin
  // Use sine for smooth transition
  var easedT := (Q8_8(1.0) - Cos(t * PI)) / Q8_8(2.0);
  SmoothTransition := start + (target - start) * easedT;
end;
```

---

## Complete Wave Example

**Putting it all together:**

```pascal
program WaveDemo;

var
  sprite: TEntityID;
  time: Q8.8;

procedure InitSprite;
begin
  sprite := EntityCreate;
  EntitySetPosition(sprite, 160, 120);
  time := Q8_8(0.0);
end;

procedure UpdateWaveAnimation;
var
  x, y: integer;
  floatY: Q8.8;
  pulse: Q8.8;
begin
  // Floating motion
  floatY := Q8_8(30.0) * Sin(time);
  
  // Pulsing scale
  pulse := Q8_8(1.0) + Q8_8(0.2) * Sin(time * Q8_8(2.0));
  
  // Update position
  EntityGetPosition(sprite, x, y);
  y := Round(Q8_8(120.0) + floatY);
  EntitySetPosition(sprite, x, y);
  
  // Update scale
  SetEntityScale(sprite, pulse);
  
  // Increment time
  time := time + Q8_8(0.1);
  if time > PI * 2 then
    time := time - PI * 2;  // Keep in range
end;

begin
  InitGraphics;
  InitSprite;
  
  while true do
  begin
    UpdateWaveAnimation;
    WaitVBlank;
    RenderGame;
  end;
end.
```

---

## Best Practices

### 1. Keep Time in Range

**Reset time to prevent overflow:**

```pascal
// ✅ GOOD: Reset time
if time > PI * 2 then
  time := time - PI * 2;

// ❌ BAD: Time grows unbounded
time := time + Q8_8(0.1);  // May overflow
```

### 2. Use Appropriate Frequency

**Choose frequency based on desired speed:**

```pascal
// ✅ GOOD: Clear frequency
var frequency := Q8_8(1.0);  // 1 cycle per time unit
value := Sin(time * frequency);

// ❌ BAD: Unclear frequency
value := Sin(time * Q8_8(0.314));  // What does this mean?
```

### 3. Combine Waves Carefully

**Understand how waves combine:**

```pascal
// ✅ GOOD: Document wave combination
// Two waves: slow (1x) and fast (2x)
value := Sin(time) + Sin(time * 2) / 2;

// ❌ BAD: Unclear combination
value := Sin(time) + Sin(time * 2);  // May be too large
```

### 4. Use Constants for Parameters

**Define wave parameters as constants:**

```pascal
// ✅ GOOD: Named constants
const
  FLOAT_AMPLITUDE = 30;
  FLOAT_FREQUENCY = 1.0;
floatY := FLOAT_AMPLITUDE * Sin(time * FLOAT_FREQUENCY);

// ❌ BAD: Magic numbers
floatY := 30 * Sin(time * 1.0);  // What are these values?
```

### 5. Optimize Calculations

**Cache expensive calculations:**

```pascal
// ✅ GOOD: Cache sine value
var sinValue := Sin(time);
value1 := amplitude1 * sinValue;
value2 := amplitude2 * sinValue;

// ❌ BAD: Calculate multiple times
value1 := amplitude1 * Sin(time);
value2 := amplitude2 * Sin(time);  // Calculated twice
```

---

## Exercises

### Exercise 1: Basic Sine Wave

Write a program that:
1. Creates a sine wave animation
2. Displays wave on screen
3. Adjusts amplitude and frequency
4. Shows wave parameters

### Exercise 2: Floating Animation

Write a program that:
1. Creates an entity that floats up and down
2. Uses sine wave for smooth motion
3. Adjustable amplitude and speed
4. Smooth, continuous animation

### Exercise 3: Circular Motion

Write a program that:
1. Creates an entity that moves in a circle
2. Uses sine and cosine for circular path
3. Adjustable radius and speed
4. Smooth circular motion

### Exercise 4: Combined Waves

Write a program that:
1. Combines multiple sine waves
2. Creates complex motion patterns
3. Adjusts wave parameters
4. Demonstrates wave combination

---

**Previous Section:** [Trigonometry](./03_Trigonometry.md)  
**Next Section:** [Polynomials and Algebra](./04_PolynomialsAndAlgebra.md) (Advanced)  
**Language Specification:** See [Standard Library](../../languageSpecification/13_StandardLibrary.md)  
**Last Updated:** 2025-01-XX

