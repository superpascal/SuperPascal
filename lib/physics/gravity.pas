unit Physics_Gravity;

interface

// Gravity simulation utilities
// Source: algorithms/06_PhysicsSimulation.md
// Based on: Mikro archive GRAVITY.TXT

uses
  Physics_Types,
  Math_Types,
  Math_Fixed;

// ============================================================================
// Gravity Simulation
// ============================================================================

// Update physics object with gravity
procedure UpdatePhysics(var obj: TPhysicsObject);

// Apply gravity to velocity
procedure ApplyGravity(var obj: TPhysicsObject);

// Apply jump velocity (upward)
procedure ApplyJump(var obj: TPhysicsObject);

// ============================================================================
// Velocity Integration
// ============================================================================

// Integrate velocity to update position
procedure IntegrateVelocity(var obj: TPhysicsObject; deltaTime: Fixed16);

// Integrate acceleration to update velocity
procedure IntegrateAcceleration(var obj: TPhysicsObject; deltaTime: Fixed16);

// ============================================================================
// Complete Physics Update
// ============================================================================

// Complete physics update (acceleration -> velocity -> position)
procedure UpdatePhysicsComplete(var obj: TPhysicsObject; deltaTime: Fixed16);

implementation

// Update physics object with gravity
procedure UpdatePhysics(var obj: TPhysicsObject);
begin
  // Apply gravity (downward acceleration)
  obj.AY := PHYSICS_GRAVITY;
  
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

// Apply gravity to velocity
procedure ApplyGravity(var obj: TPhysicsObject);
begin
  // Apply gravity (downward acceleration)
  obj.VY := obj.VY + PHYSICS_GRAVITY;
end;

// Apply jump velocity (upward)
procedure ApplyJump(var obj: TPhysicsObject);
begin
  // Apply upward velocity
  obj.VY := PHYSICS_JUMP_VELOCITY;
end;

// Integrate velocity to update position
procedure IntegrateVelocity(var obj: TPhysicsObject; deltaTime: Fixed16);
begin
  // Euler integration: position = position + velocity * time
  obj.X := obj.X + Fixed16Mul(obj.VX, deltaTime);
  obj.Y := obj.Y + Fixed16Mul(obj.VY, deltaTime);
end;

// Integrate acceleration to update velocity
procedure IntegrateAcceleration(var obj: TPhysicsObject; deltaTime: Fixed16);
begin
  // Euler integration: velocity = velocity + acceleration * time
  obj.VX := obj.VX + Fixed16Mul(obj.AX, deltaTime);
  obj.VY := obj.VY + Fixed16Mul(obj.AY, deltaTime);
end;

// Complete physics update (acceleration -> velocity -> position)
procedure UpdatePhysicsComplete(var obj: TPhysicsObject; deltaTime: Fixed16);
begin
  // 1. Integrate acceleration to get velocity
  IntegrateAcceleration(obj, deltaTime);
  
  // 2. Integrate velocity to get position
  IntegrateVelocity(obj, deltaTime);
  
  // 3. Reset acceleration (will be set by forces next frame)
  obj.AX := 0;
  obj.AY := 0;
end;

end.

