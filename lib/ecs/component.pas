unit ECS_Component;

interface

uses
  ECS_Types,
  ECS_World;

// Component management
procedure ComponentAdd(var world: TWorld; entity: TEntity; componentID: TComponentID);
procedure ComponentRemove(var world: TWorld; entity: TEntity; componentID: TComponentID);
function ComponentHas(var world: TWorld; entity: TEntity; componentID: TComponentID): Boolean;

// Component data accessors
function ComponentGetPosition(var world: TWorld; entity: TEntity): ^TPosition;
function ComponentGetVelocity(var world: TWorld; entity: TEntity): ^TVelocity;
function ComponentGetAcceleration(var world: TWorld; entity: TEntity): ^TAcceleration;
function ComponentGetMaterial(var world: TWorld; entity: TEntity): ^TMaterialProperties;
function ComponentGetShape(var world: TWorld; entity: TEntity): ^TShape;
function ComponentGetSprite(var world: TWorld; entity: TEntity): ^TSprite;
function ComponentGetPhysics(var world: TWorld; entity: TEntity): ^TPhysicsBody;
function ComponentGetParticle(var world: TWorld; entity: TEntity): ^TParticle;

procedure ComponentSetPosition(var world: TWorld; entity: TEntity; const pos: TPosition);
procedure ComponentSetVelocity(var world: TWorld; entity: TEntity; const vel: TVelocity);
procedure ComponentSetAcceleration(var world: TWorld; entity: TEntity; const accel: TAcceleration);
procedure ComponentSetMaterial(var world: TWorld; entity: TEntity; const material: TMaterialProperties);
procedure ComponentSetShape(var world: TWorld; entity: TEntity; const shape: TShape);
procedure ComponentSetSprite(var world: TWorld; entity: TEntity; const sprite: TSprite);
procedure ComponentSetPhysics(var world: TWorld; entity: TEntity; const physics: TPhysicsBody);
procedure ComponentSetParticle(var world: TWorld; entity: TEntity; const particle: TParticle);

implementation

uses
  ECS_Entity;

// Helper functions for component masks
procedure SetComponentBit(var mask: TComponentMask; componentID: TComponentID);
begin
  mask := mask + [componentID];
end;

procedure ClearComponentBit(var mask: TComponentMask; componentID: TComponentID);
begin
  mask := mask - [componentID];
end;

function HasComponentBit(const mask: TComponentMask; componentID: TComponentID): Boolean;
begin
  Result := componentID in mask;
end;

// Add component to entity
procedure ComponentAdd(var world: TWorld; entity: TEntity; componentID: TComponentID);
begin
  // Validate entity
  if not EntityIsValid(world, entity) then
    Exit;
  
  // Validate component ID
  if componentID >= MAX_COMPONENTS then
    Exit;
  
  // Add component to mask
  SetComponentBit(world.EntityComponents[entity], componentID);
  
  // Initialize component data based on type
  case componentID of
    COMPONENT_POSITION:
    begin
      world.Positions[entity].X := 0;
      world.Positions[entity].Y := 0;
    end;
    COMPONENT_VELOCITY:
    begin
      world.Velocities[entity].VX := 0;
      world.Velocities[entity].VY := 0;
    end;
    COMPONENT_SPRITE:
    begin
      world.Sprites[entity].SpriteID := 0;
      world.Sprites[entity].TileID := 0;
      world.Sprites[entity].Palette := 0;
      world.Sprites[entity].Visible := False;
      world.Sprites[entity].Explodable := False;
      world.Sprites[entity].ExplosionRadius := 0;
      world.Sprites[entity].ExplosionDamage := 0;
    end;
    COMPONENT_PHYSICS:
    begin
      world.PhysicsBodies[entity].Mass := 1;
      world.PhysicsBodies[entity].Friction := $E0;
      world.PhysicsBodies[entity].Gravity := 1;
      world.PhysicsBodies[entity].OnGround := False;
    end;
    COMPONENT_PARTICLE:
    begin
      world.Particles[entity].X := 0;
      world.Particles[entity].Y := 0;
      world.Particles[entity].VX := 0;
      world.Particles[entity].VY := 0;
      world.Particles[entity].Energy := 0;
      world.Particles[entity].Color := 0;
      world.Particles[entity].Size := 1;
      world.Particles[entity].Active := False;
    end;
  end;
end;

// Remove component from entity
procedure ComponentRemove(var world: TWorld; entity: TEntity; componentID: TComponentID);
begin
  // Validate entity
  if not EntityIsValid(world, entity) then
    Exit;
  
  // Validate component ID
  if componentID >= MAX_COMPONENTS then
    Exit;
  
  // Remove component from mask
  ClearComponentBit(world.EntityComponents[entity], componentID);
  
  // Clear component data based on type
  case componentID of
    COMPONENT_POSITION:
    begin
      world.Positions[entity].X := 0;
      world.Positions[entity].Y := 0;
    end;
    COMPONENT_VELOCITY:
    begin
      world.Velocities[entity].VX := 0;
      world.Velocities[entity].VY := 0;
    end;
    COMPONENT_SPRITE:
    begin
      world.Sprites[entity].SpriteID := 0;
      world.Sprites[entity].TileID := 0;
      world.Sprites[entity].Palette := 0;
      world.Sprites[entity].Visible := False;
      world.Sprites[entity].Explodable := False;
      world.Sprites[entity].ExplosionRadius := 0;
      world.Sprites[entity].ExplosionDamage := 0;
    end;
    COMPONENT_PHYSICS:
    begin
      world.PhysicsBodies[entity].Mass := 1;
      world.PhysicsBodies[entity].Friction := $E0;
      world.PhysicsBodies[entity].Gravity := 0;
      world.PhysicsBodies[entity].OnGround := False;
    end;
    COMPONENT_PARTICLE:
    begin
      world.Particles[entity].X := 0;
      world.Particles[entity].Y := 0;
      world.Particles[entity].VX := 0;
      world.Particles[entity].VY := 0;
      world.Particles[entity].Energy := 0;
      world.Particles[entity].Active := False;
    end;
  end;
end;

// Check if entity has component
function ComponentHas(var world: TWorld; entity: TEntity; componentID: TComponentID): Boolean;
begin
  // Validate entity
  if not EntityIsValid(world, entity) then
  begin
    Result := False;
    Exit;
  end;
  
  // Validate component ID
  if componentID >= MAX_COMPONENTS then
  begin
    Result := False;
    Exit;
  end;
  
  // Check component mask
  Result := HasComponentBit(world.EntityComponents[entity], componentID);
end;

// Get component data (Position)
function ComponentGetPosition(var world: TWorld; entity: TEntity): ^TPosition;
begin
  // Validate entity and component
  if not ComponentHas(world, entity, COMPONENT_POSITION) then
  begin
    Result := nil;
    Exit;
  end;
  
  Result := @world.Positions[entity];
end;

// Get component data (Velocity)
function ComponentGetVelocity(var world: TWorld; entity: TEntity): ^TVelocity;
begin
  // Validate entity and component
  if not ComponentHas(world, entity, COMPONENT_VELOCITY) then
  begin
    Result := nil;
    Exit;
  end;
  
  Result := @world.Velocities[entity];
end;

// Get component data (Acceleration)
function ComponentGetAcceleration(var world: TWorld; entity: TEntity): ^TAcceleration;
begin
  // Validate entity and component
  if not ComponentHas(world, entity, COMPONENT_ACCELERATION) then
  begin
    Result := nil;
    Exit;
  end;
  
  Result := @world.Accelerations[entity];
end;

// Get component data (Material)
function ComponentGetMaterial(var world: TWorld; entity: TEntity): ^TMaterialProperties;
begin
  // Validate entity and component
  if not ComponentHas(world, entity, COMPONENT_MATERIAL) then
  begin
    Result := nil;
    Exit;
  end;
  
  Result := @world.Materials[entity];
end;

// Get component data (Shape)
function ComponentGetShape(var world: TWorld; entity: TEntity): ^TShape;
begin
  // Validate entity and component
  if not ComponentHas(world, entity, COMPONENT_SHAPE) then
  begin
    Result := nil;
    Exit;
  end;
  
  Result := @world.Shapes[entity];
end;

// Get component data (Sprite)
function ComponentGetSprite(var world: TWorld; entity: TEntity): ^TSprite;
begin
  // Validate entity and component
  if not ComponentHas(world, entity, COMPONENT_SPRITE) then
  begin
    Result := nil;
    Exit;
  end;
  
  Result := @world.Sprites[entity];
end;

// Get component data (Physics)
function ComponentGetPhysics(var world: TWorld; entity: TEntity): ^TPhysicsBody;
begin
  // Validate entity and component
  if not ComponentHas(world, entity, COMPONENT_PHYSICS) then
  begin
    Result := nil;
    Exit;
  end;
  
  Result := @world.PhysicsBodies[entity];
end;

// Get component data (Particle)
function ComponentGetParticle(var world: TWorld; entity: TEntity): ^TParticle;
begin
  // Validate entity and component
  if not ComponentHas(world, entity, COMPONENT_PARTICLE) then
  begin
    Result := nil;
    Exit;
  end;
  
  Result := @world.Particles[entity];
end;

// Set component data (Position)
procedure ComponentSetPosition(var world: TWorld; entity: TEntity; const pos: TPosition);
var
  p: ^TPosition;
begin
  p := ComponentGetPosition(world, entity);
  if p <> nil then
    p^ := pos;
end;

// Set component data (Velocity)
procedure ComponentSetVelocity(var world: TWorld; entity: TEntity; const vel: TVelocity);
var
  v: ^TVelocity;
begin
  v := ComponentGetVelocity(world, entity);
  if v <> nil then
    v^ := vel;
end;

// Set component data (Acceleration)
procedure ComponentSetAcceleration(var world: TWorld; entity: TEntity; const accel: TAcceleration);
var
  a: ^TAcceleration;
begin
  a := ComponentGetAcceleration(world, entity);
  if a <> nil then
    a^ := accel;
end;

// Set component data (Material)
procedure ComponentSetMaterial(var world: TWorld; entity: TEntity; const material: TMaterialProperties);
var
  m: ^TMaterialProperties;
begin
  m := ComponentGetMaterial(world, entity);
  if m <> nil then
    m^ := material;
end;

// Set component data (Shape)
procedure ComponentSetShape(var world: TWorld; entity: TEntity; const shape: TShape);
var
  s: ^TShape;
begin
  s := ComponentGetShape(world, entity);
  if s <> nil then
    s^ := shape;
end;

// Set component data (Sprite)
procedure ComponentSetSprite(var world: TWorld; entity: TEntity; const sprite: TSprite);
var
  s: ^TSprite;
begin
  s := ComponentGetSprite(world, entity);
  if s <> nil then
    s^ := sprite;
end;

// Set component data (Physics)
procedure ComponentSetPhysics(var world: TWorld; entity: TEntity; const physics: TPhysicsBody);
var
  p: ^TPhysicsBody;
begin
  p := ComponentGetPhysics(world, entity);
  if p <> nil then
    p^ := physics;
end;

// Set component data (Particle)
procedure ComponentSetParticle(var world: TWorld; entity: TEntity; const particle: TParticle);
var
  p: ^TParticle;
begin
  p := ComponentGetParticle(world, entity);
  if p <> nil then
    p^ := particle;
end;

end.

