# Physics Simulation

**Part of:** [Algorithms Appendix](../99_Algorithms_Appendix.md)

---

## Overview

Physics simulation algorithms for games. These algorithms are **generic** and work on **all platforms**.

**Source Material:** Mikro Documentation Archive

**See Also:**
- Book Chapter: [Chapter 18: Physics and Movement](../../book/18_PhysicsAndMovement/README.md)
- Game Engine: [Game Engine Concepts](../09_GameEngine_Concepts.md)

---

## Particle Systems

**Source:** `docs/mikro_docs_archive/Coding/3/PARTICLE.TXT`

### Basic Particle

**Algorithm:** Independent entities animated using rules

**Key Characteristics:**
- Position
- Velocity (speed + direction)
- Energy/Lifetime
- Color/Size

```pascal
type
  TParticle = record
    X, Y: Fixed16;           // Position
    VX, VY: Fixed16;         // Velocity
    Energy: integer;          // Lifetime/energy
    Color: byte;
    Size: byte;
    Active: boolean;
  end;

procedure UpdateParticle(var p: TParticle);
begin
  if not p.Active then
    exit;
  
  // Update position
  p.X := p.X + p.VX;
  p.Y := p.Y + p.VY;
  
  // Apply acceleration (e.g., gravity)
  p.VY := p.VY + GRAVITY;  // GRAVITY is Fixed16 constant
  
  // Decrease energy
  p.Energy := p.Energy - 1;
  
  // Check if particle is dead
  if p.Energy <= 0 then
    p.Active := false;
end;
```

### Explosion Particle System

**Algorithm:** Create particles with random directions from explosion point

```pascal
procedure CreateExplosion(
  x, y: integer;
  var particles: array of TParticle;
  count: integer
);
var
  i: integer;
  angle: Fixed16;
  speed: Fixed16;
begin
  for i := 0 to count - 1 do
  begin
    // Random angle (0 to 2π)
    angle := Random(256);  // 0-255 represents 0-2π
    
    // Random speed
    speed := IntToFixed16(Random(50) + 10);  // 10-60 pixels/second
    
    particles[i].X := IntToFixed16(x);
    particles[i].Y := IntToFixed16(y);
    particles[i].VX := Fixed16Mul(speed, CosFixed(angle));
    particles[i].VY := Fixed16Mul(speed, SinFixed(angle));
    particles[i].Energy := Random(60) + 30;  // 30-90 frames
    particles[i].Color := Random(256);
    particles[i].Size := Random(3) + 1;
    particles[i].Active := true;
  end;
end;

procedure UpdateParticleSystem(var particles: array of TParticle);
var
  i: integer;
begin
  for i := 0 to Length(particles) - 1 do
    UpdateParticle(particles[i]);
end;
```

### Ground Impact Explosion

**Algorithm:** Particles distributed in hemisphere (not full sphere)

```pascal
procedure CreateGroundExplosion(
  x, y: integer;
  var particles: array of TParticle;
  count: integer
);
var
  i: integer;
  angle: Fixed16;
  speed: Fixed16;
begin
  for i := 0 to count - 1 do
  begin
    // Hemisphere: angle from -π/2 to π/2 (upward)
    angle := Random(128) - 64;  // -64 to +64 (represents -90° to +90°)
    
    speed := IntToFixed16(Random(50) + 10);
    
    particles[i].X := IntToFixed16(x);
    particles[i].Y := IntToFixed16(y);
    particles[i].VX := Fixed16Mul(speed, CosFixed(angle));
    particles[i].VY := Fixed16Mul(speed, SinFixed(angle));
    particles[i].Energy := Random(60) + 30;
    particles[i].Color := Random(256);
    particles[i].Size := Random(3) + 1;
    particles[i].Active := true;
  end;
end;
```

---

## Gravity

**Source:** `docs/mikro_docs_archive/Coding/2/GRAVITY.TXT`

### Basic Gravity Simulation

**Key Concepts:**
- **Velocity:** Change in position over time
- **Acceleration:** Change in velocity over time
- **Force:** Causes acceleration (F = ma, so a = F/m)

**Gravity Formula:**
```
v = v0 + a*t
s = s0 + v0*t + (1/2)*a*t²
```

**Simple Implementation:**
```pascal
type
  TPhysicsObject = record
    X, Y: Fixed16;      // Position
    VX, VY: Fixed16;    // Velocity
    AX, AY: Fixed16;    // Acceleration
  end;

const
  GRAVITY: Fixed16 = 1966;  // 0.03 in 16.16 format (pixels/frame²)

procedure UpdatePhysics(var obj: TPhysicsObject);
begin
  // Apply gravity (downward acceleration)
  obj.AY := GRAVITY;
  
  // Update velocity
  obj.VX := obj.VX + obj.AX;
  obj.VY := obj.VY + obj.AY;
  
  // Update position
  obj.X := obj.X + obj.VX;
  obj.Y := obj.Y + obj.VY;
  
  // Reset acceleration (will be set by forces next frame)
  obj.AX := 0;
  obj.AY := 0;
end;
```

### Jumping

**Algorithm:** Apply upward velocity when jump key pressed

```pascal
const
  JUMP_VELOCITY: Fixed16 = -26214;  // -0.4 in 16.16 format (upward)

procedure Jump(var obj: TPhysicsObject);
begin
  // Only jump if on ground (check collision)
  if IsOnGround(obj) then
    obj.VY := JUMP_VELOCITY;
end;
```

---

## Velocity and Acceleration

### Velocity Integration

**Algorithm:** Integrate velocity to update position

```pascal
procedure IntegrateVelocity(var obj: TPhysicsObject; deltaTime: Fixed16);
begin
  // Euler integration: position = position + velocity * time
  obj.X := obj.X + Fixed16Mul(obj.VX, deltaTime);
  obj.Y := obj.Y + Fixed16Mul(obj.VY, deltaTime);
end;
```

### Acceleration Integration

**Algorithm:** Integrate acceleration to update velocity

```pascal
procedure IntegrateAcceleration(var obj: TPhysicsObject; deltaTime: Fixed16);
begin
  // Euler integration: velocity = velocity + acceleration * time
  obj.VX := obj.VX + Fixed16Mul(obj.AX, deltaTime);
  obj.VY := obj.VY + Fixed16Mul(obj.AY, deltaTime);
end;
```

### Complete Physics Update

```pascal
procedure UpdatePhysicsComplete(var obj: TPhysicsObject; deltaTime: Fixed16);
begin
  // 1. Apply forces (sets acceleration)
  ApplyForces(obj);
  
  // 2. Integrate acceleration to get velocity
  IntegrateAcceleration(obj, deltaTime);
  
  // 3. Integrate velocity to get position
  IntegrateVelocity(obj, deltaTime);
  
  // 4. Handle collisions (may modify velocity/position)
  HandleCollisions(obj);
end;
```

---

## Friction

### Linear Friction

**Algorithm:** Reduce velocity by friction coefficient

```pascal
const
  FRICTION_COEFFICIENT: Fixed16 = 58982;  // 0.9 in 16.16 format

procedure ApplyFriction(var obj: TPhysicsObject);
begin
  // Apply friction to horizontal velocity
  obj.VX := Fixed16Mul(obj.VX, FRICTION_COEFFICIENT);
  
  // Stop if velocity is very small
  if Abs(obj.VX) < 655 then  // 0.01 in 16.16 format
    obj.VX := 0;
end;
```

### Air Resistance

**Algorithm:** Apply velocity-dependent drag force

```pascal
const
  AIR_RESISTANCE: Fixed16 = 62259;  // 0.95 in 16.16 format

procedure ApplyAirResistance(var obj: TPhysicsObject);
begin
  // Apply to both X and Y velocity
  obj.VX := Fixed16Mul(obj.VX, AIR_RESISTANCE);
  obj.VY := Fixed16Mul(obj.VY, AIR_RESISTANCE);
end;
```

---

## Complete Physics System

**Example:** Complete physics update with all forces

```pascal
procedure UpdatePhysicsSystem(var obj: TPhysicsObject; deltaTime: Fixed16);
begin
  // Reset acceleration
  obj.AX := 0;
  obj.AY := GRAVITY;  // Always apply gravity
  
  // Apply user input (e.g., left/right movement)
  if KeyLeft then
    obj.AX := obj.AX - ACCELERATION;
  if KeyRight then
    obj.AX := obj.AX + ACCELERATION;
  
  // Integrate acceleration to velocity
  IntegrateAcceleration(obj, deltaTime);
  
  // Apply friction (only when on ground)
  if IsOnGround(obj) then
    ApplyFriction(obj);
  
  // Apply air resistance (always)
  ApplyAirResistance(obj);
  
  // Integrate velocity to position
  IntegrateVelocity(obj, deltaTime);
  
  // Handle collisions
  HandleCollisions(obj);
end;
```

---

## Performance Notes

**Particle Systems:**
- **Time:** O(n) where n = number of particles
- **Optimization:** Use object pooling to avoid allocation
- **Rendering:** Batch particle rendering for efficiency

**Gravity:**
- **Time:** O(1) per object
- **Fixed-point:** Use fixed-point for smooth movement
- **Integration:** Euler integration is simple but can accumulate errors

**Friction:**
- **Time:** O(1) per object
- **Optimization:** Use lookup table for friction coefficients
- **Threshold:** Stop objects with very small velocity

**Complete System:**
- **Time:** O(n) where n = number of physics objects
- **Optimization:** Use spatial partitioning for collision detection
- **Fixed timestep:** Use fixed deltaTime for consistent physics

---

**Previous:** [Collision Detection](./05_CollisionDetection.md)  
**Back to:** [Algorithms Appendix](../99_Algorithms_Appendix.md)  
**Last Updated:** 2025-01-XX
