# SuperPascal Physics Library

**Status:** ✅ Complete  
**Priority:** ⭐⭐⭐⭐ HIGH  
**Target:** All platforms (8-bit through 64-bit)

---

## Overview

Physics simulation utilities for games. These algorithms are **generic** and work on **all platforms**. This library provides standalone physics utilities that complement the ECS physics systems in `lib/ecs/physics.pas`.

**Key Features:**
- **Particle Systems:** Explosions, ground impacts, particle management
- **Gravity Simulation:** Basic gravity, jumping, velocity integration
- **Friction & Air Resistance:** Linear friction, air resistance, velocity damping
- **Bounce Physics:** Material-based bouncing with bouncability, solidness, and absorbability
- **Brittleness & Fragmentation:** Objects can break apart into fragments based on brittleness and fragmentability
- **Object-Oriented Design:** Class hierarchy with inheritance and polymorphism
- **Material System:** Reusable material classes (Rock, Glass, Ball, Bubble, etc.)
- **Platform-Agnostic:** Works on all platforms without modification
- **Fixed-Point Arithmetic:** No FPU required (uses Q8.8 fixed-point)

---

## Module Structure

```
lib/physics/
├── mod.pas          # Main entry point
├── types.pas        # Core physics types and constants (records)
├── material.pas     # Material class hierarchy
├── object_base.pas  # Base physics object class
├── object_circle.pas # Circle physics object class
├── object_polygon.pas # Polygon physics object class
├── conversion.pas   # Record/class conversion utilities
├── particle.pas     # Particle system utilities
├── gravity.pas      # Gravity simulation utilities
├── friction.pas     # Friction and air resistance utilities
├── bounce.pas       # Bounce physics utilities (record-based)
├── bounce_oo.pas    # Bounce physics utilities (class-based)
├── BOUNCE_DESIGN.md # Bounce physics design document
├── OO_AUDIT.md      # OO pattern audit and refactoring plan
├── OO_USAGE_EXAMPLES.md # OO API usage examples
└── README.md        # This file
```

---

## Quick Start

### Basic Particle System

```pascal
program ParticleDemo;

uses Physics, Math;

var
  particles: TParticleArray;
  i: integer;
begin
  // Create explosion
  SetLength(particles, 50);
  CreateExplosion(100, 100, particles, 50);
  
  // Update particles
  for i := 0 to 100 do
  begin
    UpdateParticleSystem(particles);
    // Render particles...
  end;
end.
```

### Basic Physics Object

```pascal
program PhysicsDemo;

uses Physics, Math;

var
  obj: TPhysicsObject;
begin
  // Initialize object
  obj.X := IntToFixed16(100);
  obj.Y := IntToFixed16(100);
  obj.VX := 0;
  obj.VY := 0;
  obj.AX := 0;
  obj.AY := 0;
  
  // Update physics
  UpdatePhysics(obj);
  
  // Apply jump
  ApplyJump(obj);
  
  // Update with gravity
  UpdatePhysics(obj);
end.
```

---

## Particle Systems

### Creating Particles

```pascal
// Create explosion (full circle)
CreateExplosion(x, y: integer; var particles: TParticleArray; count: integer);

// Create ground explosion (hemisphere, upward)
CreateGroundExplosion(x, y: integer; var particles: TParticleArray; count: integer);

// Create single particle
CreateParticle(
  x, y: integer;
  vx, vy: Fixed16;
  energy: integer;
  color, size: byte;
  var particle: TParticle
);
```

### Updating Particles

```pascal
// Update single particle
UpdateParticle(var p: TParticle);

// Update all particles
UpdateParticleSystem(var particles: TParticleArray);
```

### Particle Utilities

```pascal
// Get number of active particles
GetActiveParticleCount(const particles: TParticleArray): integer;

// Reset particle (deactivate)
ResetParticle(var particle: TParticle);
```

---

## Gravity Simulation

### Basic Gravity

```pascal
// Update physics object with gravity
UpdatePhysics(var obj: TPhysicsObject);

// Apply gravity to velocity
ApplyGravity(var obj: TPhysicsObject);

// Apply jump velocity (upward)
ApplyJump(var obj: TPhysicsObject);
```

### Velocity Integration

```pascal
// Integrate velocity to update position
IntegrateVelocity(var obj: TPhysicsObject; deltaTime: Fixed16);

// Integrate acceleration to update velocity
IntegrateAcceleration(var obj: TPhysicsObject; deltaTime: Fixed16);

// Complete physics update
UpdatePhysicsComplete(var obj: TPhysicsObject; deltaTime: Fixed16);
```

---

## Friction & Air Resistance

### Friction

```pascal
// Apply linear friction (horizontal only)
ApplyFriction(var obj: TPhysicsObject);

// Apply friction with custom coefficient
ApplyFrictionWithCoefficient(var obj: TPhysicsObject; coefficient: Fixed16);

// Stop object if velocity is below threshold
StopIfSlow(var obj: TPhysicsObject);
```

### Air Resistance

```pascal
// Apply air resistance (both X and Y)
ApplyAirResistance(var obj: TPhysicsObject);
```

### Complete System with Friction

```pascal
// Complete physics update with friction
UpdatePhysicsWithFriction(
  var obj: TPhysicsObject;
  deltaTime: Fixed16;
  onGround: boolean
);
```

---

## Physics Constants

All constants are in Q8.8 fixed-point format:

```pascal
PHYSICS_GRAVITY: Fixed16 = 8;              // 0.03125 pixels/frame²
PHYSICS_JUMP_VELOCITY: Fixed16 = -102;     // -0.3984375 pixels/frame
PHYSICS_FRICTION_COEFFICIENT: Fixed16 = 230;  // 0.8984375
PHYSICS_AIR_RESISTANCE: Fixed16 = 243;     // 0.94921875
PHYSICS_VELOCITY_THRESHOLD: Fixed16 = 3;   // 0.01171875
PHYSICS_DEFAULT_ACCELERATION: Fixed16 = 5;  // 0.01953125
```

---

## Complete Example

```pascal
program CompletePhysicsDemo;

uses Physics, Math;

var
  obj: TPhysicsObject;
  particles: TParticleArray;
  deltaTime: Fixed16;
  onGround: boolean;
begin
  // Initialize physics object
  obj.X := IntToFixed16(100);
  obj.Y := IntToFixed16(100);
  obj.VX := 0;
  obj.VY := 0;
  obj.AX := 0;
  obj.AY := 0;
  
  // Initialize particles
  SetLength(particles, 50);
  
  // Game loop
  deltaTime := FIXED16_ONE;  // 1.0 in Q8.8
  onGround := True;
  
  while True do
  begin
    // Update physics with friction
    UpdatePhysicsWithFriction(obj, deltaTime, onGround);
    
    // Create explosion on impact
    if obj.Y > IntToFixed16(400) then
    begin
      CreateGroundExplosion(
        Fixed16ToInt(obj.X),
        Fixed16ToInt(obj.Y),
        particles,
        50
      );
      obj.Y := IntToFixed16(400);
      obj.VY := 0;
      onGround := True;
    end;
    
    // Update particles
    UpdateParticleSystem(particles);
    
    // Render...
  end;
end.
```

---

## Brittleness & Fragmentation

### Material Properties

Objects can have brittleness and fragmentability properties:

```pascal
type
  TPhysicsObject = record
    // ... position, velocity, acceleration ...
    Bouncability: Fixed16;   // 0-256 (0.0-1.0) - how bouncy
    Solidness: Fixed16;      // 0-256 (0.0-1.0) - how solid/rigid
    Brittleness: Fixed16;    // 0-256 (0.0-1.0) - how brittle (prone to breaking)
    Fragmentability: Fixed16; // 0-256 (0.0-1.0) - how many fragments when breaking
  end;
```

### Fragment Types

```pascal
type
  TFragment = record
    X, Y: Fixed16;      // Position
    VX, VY: Fixed16;    // Velocity
    Size: Fixed16;      // Fragment size
    Angle: integer;     // Fragment angle (degrees)
  end;

  TFragmentArray = array of TFragment;
```

### Collision Scenarios with Brittleness

**1. Two Brittle Objects → Fragment**
```pascal
var
  glass1, glass2: TPhysicsObject;
  fragments1, fragments2: TFragmentArray;
begin
  // Both brittle
  glass1.Brittleness := 230;  // 0.9 (very brittle)
  glass2.Brittleness := 200;  // 0.78 (brittle)
  glass1.Fragmentability := 180;  // 0.7 (moderate fragments)
  glass2.Fragmentability := 150;  // 0.59 (fewer fragments)
  
  // Fragment - both break apart
  BounceObjects(glass1, glass2, normalX, normalY, fragments1, fragments2);
  
  // fragments1 and fragments2 now contain the broken pieces
end.
```

**2. Bouncy + Brittle → Both Bounce Away**
```pascal
var
  ball: TPhysicsObject;
  glass: TPhysicsObject;
  fragments1, fragments2: TFragmentArray;
begin
  // Bouncy ball
  ball.Bouncability := 230;  // 0.9
  ball.Brittleness := 0;     // Not brittle
  
  // Brittle glass
  glass.Brittleness := 230;   // 0.9
  glass.Bouncability := 50;  // 0.2 (low bounce)
  
  // Both bounce away (glass doesn't break)
  BounceObjects(ball, glass, normalX, normalY, fragments1, fragments2);
end.
```

### Fragment Calculation

Fragment count is determined by:
- **Fragmentability**: Base fragment count (2-16 fragments)
- **Collision Velocity**: Higher velocity = more fragments
- **Brittleness**: Higher brittleness = more fragments

Final fragment count ranges from 2-32 fragments.

---

## Integration with ECS

The Physics library works alongside the ECS physics systems:

- **ECS Systems** (`lib/ecs/physics.pas`): ECS-integrated physics systems
- **Physics Library** (`lib/physics/`): Standalone physics utilities

You can use both together:

```pascal
uses ECS, Physics;

// Use ECS for entity-based physics
GravitySystem(world);
FrictionSystem(world);

// Use Physics library for standalone calculations
var
  obj: TPhysicsObject;
begin
  UpdatePhysics(obj);
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
- **Fixed-point:** Uses Q8.8 fixed-point for smooth movement
- **Integration:** Euler integration is simple but can accumulate errors

**Friction:**
- **Time:** O(1) per object
- **Optimization:** Use lookup table for friction coefficients
- **Threshold:** Stops objects with very small velocity

---

## Platform Considerations

### Memory Constraints

**8-bit Platforms (ZealZ80, CommanderX16):**
- Use static arrays for particles where possible
- Limit particle count
- Simple physics calculations

**16-bit+ Platforms:**
- Can use dynamic arrays
- More particles supported
- More complex physics calculations

### Fixed-Point Arithmetic

All physics calculations use Q8.8 fixed-point format:
- **No FPU required** - Works on all platforms
- **Consistent precision** - Same behavior across platforms
- **Fast calculations** - Integer operations only

---

## See Also

- **Algorithms:** [Physics Simulation](../../languageSpecification/algorithms/06_PhysicsSimulation.md)
- **ECS Physics:** [ECS Physics Systems](../ecs/physics.pas)
- **Math Library:** [Math Library](../math/README.md) (for fixed-point operations)

---

**Last Updated:** 2025-01-XX  
**Status:** ✅ Complete - Ready for use

