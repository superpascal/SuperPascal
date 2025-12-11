unit Physics_Friction;

interface

// Friction simulation utilities
// Source: algorithms/06_PhysicsSimulation.md

uses
  Physics_Types,
  Math_Types,
  Math_Fixed;

// ============================================================================
// Friction Simulation
// ============================================================================

// Apply linear friction to physics object (horizontal only)
procedure ApplyFriction(var obj: TPhysicsObject);

// Apply air resistance to physics object (both X and Y)
procedure ApplyAirResistance(var obj: TPhysicsObject);

// Apply friction with custom coefficient
procedure ApplyFrictionWithCoefficient(var obj: TPhysicsObject; coefficient: Fixed16);

// Stop object if velocity is below threshold
procedure StopIfSlow(var obj: TPhysicsObject);

// ============================================================================
// Complete Physics System with Friction
// ============================================================================

// Complete physics update with friction (for ground-based objects)
procedure UpdatePhysicsWithFriction(var obj: TPhysicsObject; deltaTime: Fixed16; onGround: boolean);

implementation

uses
  Physics_Gravity;

// Apply linear friction to physics object (horizontal only)
procedure ApplyFriction(var obj: TPhysicsObject);
begin
  // Apply friction to horizontal velocity
  obj.VX := Fixed16Mul(obj.VX, PHYSICS_FRICTION_COEFFICIENT);
  
  // Stop if velocity is very small
  StopIfSlow(obj);
end;

// Apply air resistance to physics object (both X and Y)
procedure ApplyAirResistance(var obj: TPhysicsObject);
begin
  // Apply to both X and Y velocity
  obj.VX := Fixed16Mul(obj.VX, PHYSICS_AIR_RESISTANCE);
  obj.VY := Fixed16Mul(obj.VY, PHYSICS_AIR_RESISTANCE);
end;

// Apply friction with custom coefficient
procedure ApplyFrictionWithCoefficient(var obj: TPhysicsObject; coefficient: Fixed16);
begin
  // Apply friction to horizontal velocity
  obj.VX := Fixed16Mul(obj.VX, coefficient);
  
  // Stop if velocity is very small
  StopIfSlow(obj);
end;

// Stop object if velocity is below threshold
procedure StopIfSlow(var obj: TPhysicsObject);
begin
  // Stop if velocity is very small
  if Fixed16Abs(obj.VX) < PHYSICS_VELOCITY_THRESHOLD then
    obj.VX := 0;
  if Fixed16Abs(obj.VY) < PHYSICS_VELOCITY_THRESHOLD then
    obj.VY := 0;
end;

// Complete physics update with friction (for ground-based objects)
procedure UpdatePhysicsWithFriction(var obj: TPhysicsObject; deltaTime: Fixed16; onGround: boolean);
begin
  // Reset acceleration
  obj.AX := 0;
  obj.AY := PHYSICS_GRAVITY;  // Always apply gravity
  
  // Integrate acceleration to velocity
  IntegrateAcceleration(obj, deltaTime);
  
  // Apply friction (only when on ground)
  if onGround then
    ApplyFriction(obj);
  
  // Apply air resistance (always)
  ApplyAirResistance(obj);
  
  // Integrate velocity to position
  IntegrateVelocity(obj, deltaTime);
  
  // Reset acceleration (will be set by forces next frame)
  obj.AX := 0;
  obj.AY := 0;
end;

end.

