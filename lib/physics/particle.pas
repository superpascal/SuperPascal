unit Physics_Particle;

interface

// Particle system utilities
// Source: algorithms/06_PhysicsSimulation.md
// Based on: Mikro archive PARTICLE.TXT

uses
  Physics_Types,
  Math_Types,
  Math_Fixed,
  Math_Trig;

// ============================================================================
// Particle Management
// ============================================================================

// Update a single particle
procedure UpdateParticle(var p: TParticle);

// Update all particles in array
procedure UpdateParticleSystem(var particles: TParticleArray);

// ============================================================================
// Particle Creation
// ============================================================================

// Create explosion particles (full circle)
procedure CreateExplosion(
  x, y: integer;
  var particles: TParticleArray;
  count: integer
);

// Create ground explosion particles (hemisphere, upward)
procedure CreateGroundExplosion(
  x, y: integer;
  var particles: TParticleArray;
  count: integer
);

// Create particle at position with velocity
procedure CreateParticle(
  x, y: integer;
  vx, vy: Fixed16;
  energy: integer;
  color, size: byte;
  var particle: TParticle
);

// ============================================================================
// Particle Utilities
// ============================================================================

// Get number of active particles
function GetActiveParticleCount(const particles: TParticleArray): integer;

// Reset particle (deactivate)
procedure ResetParticle(var particle: TParticle);

implementation

// Update a single particle
procedure UpdateParticle(var p: TParticle);
begin
  if not p.Active then
    Exit;
  
  // Update position
  p.X := p.X + p.VX;
  p.Y := p.Y + p.VY;
  
  // Apply gravity (downward acceleration)
  p.VY := p.VY + PHYSICS_GRAVITY;
  
  // Decrease energy
  p.Energy := p.Energy - 1;
  
  // Check if particle is dead
  if p.Energy <= 0 then
    p.Active := False;
end;

// Update all particles in array
procedure UpdateParticleSystem(var particles: TParticleArray);
var
  i: integer;
begin
  for i := 0 to Length(particles) - 1 do
    UpdateParticle(particles[i]);
end;

// Create explosion particles (full circle)
procedure CreateExplosion(
  x, y: integer;
  var particles: TParticleArray;
  count: integer
);
var
  i: integer;
  angle: integer;  // Angle in degrees (-180 to +180)
  speed: Fixed16;
  angleRad: Fixed16;
begin
  // Ensure array is large enough
  if Length(particles) < count then
    SetLength(particles, count);
  
  for i := 0 to count - 1 do
  begin
    // Random angle (0 to 359 degrees, mapped to -180 to +180)
    angle := Random(360) - 180;
    
    // Random speed (10-60 pixels/frame in Q8.8)
    speed := IntToFixed16(Random(50) + 10);
    
    // Convert angle to radians for trig functions
    // Note: SinSigned and CosSigned take degrees directly
    particles[i].X := IntToFixed16(x);
    particles[i].Y := IntToFixed16(y);
    particles[i].VX := Fixed16Mul(speed, CosSigned(angle));
    particles[i].VY := Fixed16Mul(speed, SinSigned(angle));
    particles[i].Energy := Random(60) + 30;  // 30-90 frames
    particles[i].Color := Random(256);
    particles[i].Size := Random(3) + 1;
    particles[i].Active := True;
  end;
end;

// Create ground explosion particles (hemisphere, upward)
procedure CreateGroundExplosion(
  x, y: integer;
  var particles: TParticleArray;
  count: integer
);
var
  i: integer;
  angle: integer;  // Angle in degrees (-90 to +90, upward)
  speed: Fixed16;
begin
  // Ensure array is large enough
  if Length(particles) < count then
    SetLength(particles, count);
  
  for i := 0 to count - 1 do
  begin
    // Hemisphere: angle from -90° to +90° (upward)
    angle := Random(180) - 90;  // -90 to +90 degrees
    
    // Random speed (10-60 pixels/frame in Q8.8)
    speed := IntToFixed16(Random(50) + 10);
    
    particles[i].X := IntToFixed16(x);
    particles[i].Y := IntToFixed16(y);
    particles[i].VX := Fixed16Mul(speed, CosSigned(angle));
    particles[i].VY := Fixed16Mul(speed, SinSigned(angle));
    particles[i].Energy := Random(60) + 30;  // 30-90 frames
    particles[i].Color := Random(256);
    particles[i].Size := Random(3) + 1;
    particles[i].Active := True;
  end;
end;

// Create particle at position with velocity
procedure CreateParticle(
  x, y: integer;
  vx, vy: Fixed16;
  energy: integer;
  color, size: byte;
  var particle: TParticle
);
begin
  particle.X := IntToFixed16(x);
  particle.Y := IntToFixed16(y);
  particle.VX := vx;
  particle.VY := vy;
  particle.Energy := energy;
  particle.Color := color;
  particle.Size := size;
  particle.Active := True;
end;

// Get number of active particles
function GetActiveParticleCount(const particles: TParticleArray): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Length(particles) - 1 do
  begin
    if particles[i].Active then
      Result := Result + 1;
  end;
end;

// Reset particle (deactivate)
procedure ResetParticle(var particle: TParticle);
begin
  particle.Active := False;
  particle.Energy := 0;
  particle.X := 0;
  particle.Y := 0;
  particle.VX := 0;
  particle.VY := 0;
end;

end.

