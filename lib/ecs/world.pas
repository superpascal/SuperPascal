unit ECS_World;

interface

uses
  ECS_Types;

type
  // World structure
  // Note: Systems array is managed by ECS_System unit but stored in TWorld
  TWorld = record
    // Entity management
    EntityCount: Word;
    FreeEntities: array[0..MAX_ENTITIES-1] of TEntity;
    FreeCount: Word;
    
    // Component storage (Structure-of-Arrays)
    Positions: array[0..MAX_ENTITIES-1] of TPosition;
    Velocities: array[0..MAX_ENTITIES-1] of TVelocity;
    Accelerations: array[0..MAX_ENTITIES-1] of TAcceleration;
    Materials: array[0..MAX_ENTITIES-1] of TMaterialProperties;
    Shapes: array[0..MAX_ENTITIES-1] of TShape;
    Sprites: array[0..MAX_ENTITIES-1] of TSprite;
    PhysicsBodies: array[0..MAX_ENTITIES-1] of TPhysicsBody;
    Particles: array[0..MAX_ENTITIES-1] of TParticle;
    
    // Component presence tracking
    EntityComponents: array[0..MAX_ENTITIES-1] of TComponentMask;
    
    // Entity validity
    EntityValid: array[0..MAX_ENTITIES-1] of Boolean;
    
    // Systems (managed by ECS_System unit)
    // Layout must match TWorldWithSystems in ECS_System
    // Systems: array[0..MAX_SYSTEMS-1] of TSystem;
    // SystemCount: Word;
  end;

// World management functions
procedure WorldInit(var world: TWorld);
procedure WorldCleanup(var world: TWorld);
procedure WorldUpdate(var world: TWorld);

implementation

uses
  ECS_System;

// Initialize world
procedure WorldInit(var world: TWorld);
var
  i: Word;
begin
  // Initialize entity management
  world.EntityCount := 0;
  world.FreeCount := 0;
  
  // Initialize component storage
  for i := 0 to MAX_ENTITIES - 1 do
  begin
    // Initialize component data
    world.Positions[i].X := 0;
    world.Positions[i].Y := 0;
    world.Velocities[i].VX := 0;
    world.Velocities[i].VY := 0;
    world.Accelerations[i].AX := 0;
    world.Accelerations[i].AY := 0;
    world.Materials[i].Bouncability := 0;
    world.Materials[i].Solidness := 0;
    world.Materials[i].Brittleness := 0;
    world.Materials[i].Fragmentability := 0;
    world.Shapes[i].ShapeType := stCircle;
    world.Shapes[i].CircleRadius := 0;
    world.Shapes[i].Polygon.Count := 0;
    world.Shapes[i].PolygonArea := 0;
    world.Sprites[i].SpriteID := 0;
    world.Sprites[i].TileID := 0;
    world.Sprites[i].Palette := 0;
    world.Sprites[i].Visible := False;
    world.Sprites[i].Explodable := False;
    world.Sprites[i].ExplosionRadius := 0;
    world.Sprites[i].ExplosionDamage := 0;
    world.PhysicsBodies[i].Mass := 1;
    world.PhysicsBodies[i].Friction := $E0;
    world.PhysicsBodies[i].Gravity := 0;
    world.PhysicsBodies[i].OnGround := False;
    world.Particles[i].X := 0;
    world.Particles[i].Y := 0;
    world.Particles[i].VX := 0;
    world.Particles[i].VY := 0;
    world.Particles[i].Energy := 0;
    world.Particles[i].Color := 0;
    world.Particles[i].Size := 1;
    world.Particles[i].Active := False;
    
    // Initialize component presence
    world.EntityComponents[i] := [];
    
    // Initialize entity validity
    world.EntityValid[i] := False;
  end;
  
  // Initialize systems (via ECS_System unit)
  ECS_System.WorldInitSystems(world);
end;

// Cleanup world
procedure WorldCleanup(var world: TWorld);
begin
  // Reset all entities
  WorldInit(world);
end;

// Update world (run all systems)
procedure WorldUpdate(var world: TWorld);
begin
  ECS_System.SystemRunAll(world);
end;

end.

