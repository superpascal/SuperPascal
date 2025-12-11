# ECS Implementation - Iteration 6: Algorithm Integration

**Status:** ⚠️ Pending  
**Part of:** SuperPascal ECS Library Implementation

---

## Goal

Integrate ECS with physics algorithms from the Mikro archive, demonstrating how to refactor standalone algorithms into ECS systems.

---

## Implementation

### Physics Components

```pascal
// Add to ECS unit interface section

// Physics component
type
  TPhysicsBody = record
    Mass: Integer;        // Mass (for future use)
    Friction: Integer;    // Friction coefficient (fixed-point)
    Gravity: Integer;      // Gravity multiplier
    OnGround: Boolean;    // Ground collision flag
  end;
  
  TParticle = record
    X, Y: Fixed16;        // Position
    VX, VY: Fixed16;      // Velocity
    Energy: Integer;       // Lifetime/energy
    Color: Byte;
    Size: Byte;
    Active: Boolean;
  end;

const
  COMPONENT_PHYSICS = 3;
  COMPONENT_PARTICLE = 4;
```

### Update World Structure

```pascal
// Add to TWorld record
type
  TWorld = record
    // ... existing fields ...
    
    // Physics component storage
    PhysicsBodies: array[0..MAX_ENTITIES-1] of TPhysicsBody;
    Particles: array[0..MAX_ENTITIES-1] of TParticle;
  end;
```

### Physics Systems

```pascal
// Gravity System (from GRAVITY.TXT)
procedure GravitySystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
  vel: ^TVelocity;
  physics: ^TPhysicsBody;
begin
  query := QueryCreate(world, [COMPONENT_VELOCITY, COMPONENT_PHYSICS]);
  while QueryNext(query, entity) do
  begin
    vel := ComponentGetVelocity(world, entity);
    physics := ComponentGetPhysics(world, entity);
    
    if (vel <> nil) and (physics <> nil) then
    begin
      // Apply gravity
      vel^.DY := vel^.DY + physics^.Gravity;
    end;
  end;
end;

// Velocity System (updates position based on velocity)
procedure VelocitySystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
  pos: ^TPosition;
  vel: ^TVelocity;
begin
  query := QueryCreate(world, [COMPONENT_POSITION, COMPONENT_VELOCITY]);
  while QueryNext(query, entity) do
  begin
    pos := ComponentGetPosition(world, entity);
    vel := ComponentGetVelocity(world, entity);
    
    if (pos <> nil) and (vel <> nil) then
    begin
      pos^.X := pos^.X + vel^.DX;
      pos^.Y := pos^.Y + vel^.DY;
    end;
  end;
end;

// Friction System
procedure FrictionSystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
  vel: ^TVelocity;
  physics: ^TPhysicsBody;
begin
  query := QueryCreate(world, [COMPONENT_VELOCITY, COMPONENT_PHYSICS]);
  while QueryNext(query, entity) do
  begin
    vel := ComponentGetVelocity(world, entity);
    physics := ComponentGetPhysics(world, entity);
    
    if (vel <> nil) and (physics <> nil) then
    begin
      // Apply friction (fixed-point multiplication)
      vel^.DX := (vel^.DX * physics^.Friction) shr 8;  // Fixed-point multiply
      vel^.DY := (vel^.DY * physics^.Friction) shr 8;
    end;
  end;
end;

// Particle System (from PARTICLE.TXT)
procedure ParticleSystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
  particle: ^TParticle;
begin
  query := QueryCreate(world, [COMPONENT_PARTICLE]);
  while QueryNext(query, entity) do
  begin
    particle := ComponentGetParticle(world, entity);
    
    if (particle <> nil) and particle^.Active then
    begin
      // Update position
      particle^.X := particle^.X + particle^.VX;
      particle^.Y := particle^.Y + particle^.VY;
      
      // Apply gravity (simplified)
      particle^.VY := particle^.VY + IntToFixed16(1);  // Gravity constant
      
      // Decrease energy
      particle^.Energy := particle^.Energy - 1;
      
      // Check if particle is dead
      if particle^.Energy <= 0 then
      begin
        particle^.Active := False;
        // Optionally destroy entity
        // EntityDestroy(world, entity);
      end;
    end;
  end;
end;
```

### Component Accessors for Physics

```pascal
// Add to ECS unit interface section

function ComponentGetPhysics(var world: TWorld; entity: TEntity): ^TPhysicsBody;
function ComponentGetParticle(var world: TWorld; entity: TEntity): ^TParticle;

procedure ComponentSetPhysics(var world: TWorld; entity: TEntity; const physics: TPhysicsBody);
procedure ComponentSetParticle(var world: TWorld; entity: TEntity; const particle: TParticle);
```

### Implementation

```pascal
// Add to ECS unit implementation section

// Get Physics component
function ComponentGetPhysics(var world: TWorld; entity: TEntity): ^TPhysicsBody;
begin
  if not ComponentHas(world, entity, COMPONENT_PHYSICS) then
  begin
    Result := nil;
    Exit;
  end;
  Result := @world.PhysicsBodies[entity];
end;

// Get Particle component
function ComponentGetParticle(var world: TWorld; entity: TEntity): ^TParticle;
begin
  if not ComponentHas(world, entity, COMPONENT_PARTICLE) then
  begin
    Result := nil;
    Exit;
  end;
  Result := @world.Particles[entity];
end;

// Set Physics component
procedure ComponentSetPhysics(var world: TWorld; entity: TEntity; const physics: TPhysicsBody);
var
  p: ^TPhysicsBody;
begin
  p := ComponentGetPhysics(world, entity);
  if p <> nil then
    p^ := physics;
end;

// Set Particle component
procedure ComponentSetParticle(var world: TWorld; entity: TEntity; const particle: TParticle);
var
  p: ^TParticle;
begin
  p := ComponentGetParticle(world, entity);
  if p <> nil then
    p^ := particle;
end;

// Update ComponentAdd to handle physics components
procedure ComponentAdd(var world: TWorld; entity: TEntity; componentID: TComponentID);
begin
  // ... existing code ...
  
  case componentID of
    // ... existing cases ...
    COMPONENT_PHYSICS:
    begin
      world.PhysicsBodies[entity].Mass := 1;
      world.PhysicsBodies[entity].Friction := $E0;  // 0.875 fixed-point
      world.PhysicsBodies[entity].Gravity := 1;
      world.PhysicsBodies[entity].OnGround := False;
    end;
    COMPONENT_PARTICLE:
    begin
      world.Particles[entity].X := Fixed16(0);
      world.Particles[entity].Y := Fixed16(0);
      world.Particles[entity].VX := Fixed16(0);
      world.Particles[entity].VY := Fixed16(0);
      world.Particles[entity].Energy := 0;
      world.Particles[entity].Color := 0;
      world.Particles[entity].Size := 1;
      world.Particles[entity].Active := False;
    end;
  end;
end;
```

---

## Testing

### Test 1: Gravity System

```pascal
program TestECS_Iteration6_Gravity;
uses ECS;

var
  world: TWorld;
  entity: TEntity;
  vel: TVelocity;
  physics: TPhysicsBody;
  velPtr: ^TVelocity;
begin
  WorldInit(world);
  
  // Create entity with Velocity and Physics
  entity := EntityCreate(world);
  ComponentAdd(world, entity, COMPONENT_VELOCITY);
  ComponentAdd(world, entity, COMPONENT_PHYSICS);
  
  // Set initial velocity
  vel.DX := 0;
  vel.DY := 0;
  ComponentSetVelocity(world, entity, vel);
  
  // Set physics (gravity = 1)
  physics.Gravity := 1;
  physics.Friction := $E0;
  ComponentSetPhysics(world, entity, physics);
  
  // Register and run gravity system
  SystemRegister(world, GravitySystem);
  SystemRunAll(world);
  
  // Verify velocity was updated by gravity
  velPtr := ComponentGetVelocity(world, entity);
  Assert(velPtr <> nil, 'Velocity should exist');
  Assert(velPtr^.DY = 1, 'Velocity Y should be 1 (gravity applied)');
  
  WriteLn('Iteration 6: Gravity system test passed');
  
  WorldCleanup(world);
end.
```

### Test 2: Particle System

```pascal
program TestECS_Iteration6_Particle;
uses ECS;

var
  world: TWorld;
  entity: TEntity;
  particle: TParticle;
  particlePtr: ^TParticle;
begin
  WorldInit(world);
  
  // Create particle entity
  entity := EntityCreate(world);
  ComponentAdd(world, entity, COMPONENT_PARTICLE);
  
  // Set initial particle state
  particle.X := IntToFixed16(100);
  particle.Y := IntToFixed16(200);
  particle.VX := IntToFixed16(5);
  particle.VY := IntToFixed16(-3);
  particle.Energy := 10;
  particle.Active := True;
  ComponentSetParticle(world, entity, particle);
  
  // Register and run particle system
  SystemRegister(world, ParticleSystem);
  SystemRunAll(world);
  
  // Verify particle was updated
  particlePtr := ComponentGetParticle(world, entity);
  Assert(particlePtr <> nil, 'Particle should exist');
  Assert(particlePtr^.X = IntToFixed16(105), 'X should be updated');
  Assert(particlePtr^.Y = IntToFixed16(197), 'Y should be updated');
  Assert(particlePtr^.Energy = 9, 'Energy should decrease');
  
  WriteLn('Iteration 6: Particle system test passed');
  
  WorldCleanup(world);
end.
```

### Test 3: Complete Physics Integration

```pascal
program TestECS_Iteration6_Complete;
uses ECS;

var
  world: TWorld;
  entity: TEntity;
  pos: TPosition;
  vel: TVelocity;
  physics: TPhysicsBody;
  posPtr: ^TPosition;
begin
  WorldInit(world);
  
  // Create entity with all physics components
  entity := EntityCreate(world);
  ComponentAdd(world, entity, COMPONENT_POSITION);
  ComponentAdd(world, entity, COMPONENT_VELOCITY);
  ComponentAdd(world, entity, COMPONENT_PHYSICS);
  
  // Set initial state
  pos.X := 100;
  pos.Y := 200;
  ComponentSetPosition(world, entity, pos);
  
  vel.DX := 2;
  vel.DY := 0;
  ComponentSetVelocity(world, entity, vel);
  
  physics.Gravity := 1;
  physics.Friction := $E0;
  ComponentSetPhysics(world, entity, physics);
  
  // Register systems in order
  SystemRegister(world, GravitySystem);
  SystemRegister(world, FrictionSystem);
  SystemRegister(world, VelocitySystem);
  
  // Run all systems
  SystemRunAll(world);
  
  // Verify final state
  posPtr := ComponentGetPosition(world, entity);
  Assert(posPtr <> nil, 'Position should exist');
  // Position should be updated by velocity system
  // Velocity should be updated by gravity and friction systems
  
  WriteLn('Iteration 6: Complete physics integration test passed');
  
  WorldCleanup(world);
end.
```

---

## Status

⚠️ **Pending**

**What's Done:**
- Physics components defined (TPhysicsBody, TParticle)
- Physics systems implemented (GravitySystem, VelocitySystem, FrictionSystem, ParticleSystem)
- Component accessors for physics components
- Integration with existing ECS framework

**What's Next:**
- Continue integrating more algorithms (collision, graphics, etc.)
- Create platform-specific extensions
- Add to book chapters

---

## Summary

This iteration demonstrates how to:
1. **Define new components** for algorithm-specific data
2. **Create systems** that operate on entities with specific components
3. **Integrate algorithms** from the Mikro archive as ECS systems
4. **Compose systems** to create complete game logic

**Key Benefits:**
- Algorithms become reusable systems
- Systems can be combined in different ways
- Code is more modular and maintainable
- Performance is optimized through SoA storage

---

**Previous:** [Iteration 5: System Registration](./05_Iteration5_SystemRegistration.md)  
**Next:** Continue with more algorithm integrations

