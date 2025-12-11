unit ECS_Entity;

interface

uses
  ECS_Types,
  ECS_World;

// Entity management
function EntityCreate(var world: TWorld): TEntity;
procedure EntityDestroy(var world: TWorld; entity: TEntity);
function EntityIsValid(var world: TWorld; entity: TEntity): Boolean;

implementation

// Create new entity
function EntityCreate(var world: TWorld): TEntity;
var
  entity: TEntity;
begin
  // Check if we have free entities to reuse
  if world.FreeCount > 0 then
  begin
    // Reuse free entity
    world.FreeCount := world.FreeCount - 1;
    entity := world.FreeEntities[world.FreeCount];
  end
  else
  begin
    // Create new entity
    if world.EntityCount >= MAX_ENTITIES then
    begin
      // No more entities available
      Result := ENTITY_NULL;
      Exit;
    end;
    
    entity := world.EntityCount;
    world.EntityCount := world.EntityCount + 1;
  end;
  
  // Initialize entity
  world.EntityValid[entity] := True;
  world.EntityComponents[entity] := [];
  
  Result := entity;
end;

// Destroy entity
procedure EntityDestroy(var world: TWorld; entity: TEntity);
begin
  // Validate entity
  if not EntityIsValid(world, entity) then
    Exit;
  
  // Clear component data
  world.Positions[entity].X := 0;
  world.Positions[entity].Y := 0;
  world.Velocities[entity].VX := 0;
  world.Velocities[entity].VY := 0;
  world.Sprites[entity].SpriteID := 0;
  world.Sprites[entity].TileID := 0;
  world.Sprites[entity].Palette := 0;
  world.Sprites[entity].Visible := False;
  world.Sprites[entity].Explodable := False;
  world.Sprites[entity].ExplosionRadius := 0;
  world.Sprites[entity].ExplosionDamage := 0;
  world.PhysicsBodies[entity].Mass := 1;
  world.PhysicsBodies[entity].Friction := $E0;
  world.PhysicsBodies[entity].Gravity := 0;
  world.PhysicsBodies[entity].OnGround := False;
  world.Particles[entity].X := 0;
  world.Particles[entity].Y := 0;
  world.Particles[entity].VX := 0;
  world.Particles[entity].VY := 0;
  world.Particles[entity].Energy := 0;
  world.Particles[entity].Active := False;
  
  // Clear component presence
  world.EntityComponents[entity] := [];
  
  // Mark entity as invalid
  world.EntityValid[entity] := False;
  
  // Add to free list for reuse
  if world.FreeCount < MAX_ENTITIES then
  begin
    world.FreeEntities[world.FreeCount] := entity;
    world.FreeCount := world.FreeCount + 1;
  end;
end;

// Check if entity is valid
function EntityIsValid(var world: TWorld; entity: TEntity): Boolean;
begin
  // Check bounds
  if entity >= MAX_ENTITIES then
  begin
    Result := False;
    Exit;
  end;
  
  // Check validity flag
  Result := world.EntityValid[entity];
end;

end.

