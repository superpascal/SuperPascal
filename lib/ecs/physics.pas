unit ECS_Physics;

interface

uses
  ECS_Types,
  ECS_World,
  ECS_Query,
  ECS_Component;
  // Note: ECS_Types provides IntToFixed16 (delegates to Math_Fixed)

// Physics systems (from Mikro archive algorithms)
procedure MovementSystem(var world: TWorld);
procedure GravitySystem(var world: TWorld);
procedure VelocitySystem(var world: TWorld);
procedure FrictionSystem(var world: TWorld);
procedure ParticleSystem(var world: TWorld);

implementation

// Movement System (updates position based on velocity)
procedure MovementSystem(var world: TWorld);
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
      pos^.X := pos^.X + vel^.VX;
      pos^.Y := pos^.Y + vel^.VY;
    end;
  end;
end;

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
      vel^.VY := vel^.VY + physics^.Gravity;
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
      pos^.X := pos^.X + vel^.VX;
      pos^.Y := pos^.Y + vel^.VY;
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
      vel^.VX := (vel^.VX * physics^.Friction) shr 8;  // Fixed-point multiply
      vel^.VY := (vel^.VY * physics^.Friction) shr 8;
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

end.

