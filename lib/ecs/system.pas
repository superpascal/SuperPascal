unit ECS_System;

interface

uses
  ECS_Types,
  ECS_World;

// System type
type
  TSystem = procedure(var world: TWorld);

// System management
procedure SystemRegister(var world: TWorld; system: TSystem);
procedure SystemRunAll(var world: TWorld);
function SystemGetCount(var world: TWorld): Word;

// Internal: Initialize systems array (called by ECS_World)
procedure WorldInitSystems(var world: TWorld);

implementation

// Extended world structure with systems (must match TWorld layout)
type
  TWorldWithSystems = record
    EntityCount: Word;
    FreeEntities: array[0..MAX_ENTITIES-1] of TEntity;
    FreeCount: Word;
    Positions: array[0..MAX_ENTITIES-1] of TPosition;
    Velocities: array[0..MAX_ENTITIES-1] of TVelocity;
    Accelerations: array[0..MAX_ENTITIES-1] of TAcceleration;
    Materials: array[0..MAX_ENTITIES-1] of TMaterialProperties;
    Shapes: array[0..MAX_ENTITIES-1] of TShape;
    Sprites: array[0..MAX_ENTITIES-1] of TSprite;
    PhysicsBodies: array[0..MAX_ENTITIES-1] of TPhysicsBody;
    Particles: array[0..MAX_ENTITIES-1] of TParticle;
    EntityComponents: array[0..MAX_ENTITIES-1] of TComponentMask;
    EntityValid: array[0..MAX_ENTITIES-1] of Boolean;
    // Systems (added by this unit)
    Systems: array[0..MAX_SYSTEMS-1] of TSystem;
    SystemCount: Word;
  end;

// Initialize systems array (called by ECS_World.WorldInit)
procedure WorldInitSystems(var world: TWorld);
var
  i: Word;
  worldWithSystems: ^TWorldWithSystems;
begin
  worldWithSystems := @world;
  worldWithSystems^.SystemCount := 0;
  for i := 0 to MAX_SYSTEMS - 1 do
    worldWithSystems^.Systems[i] := nil;
end;

// Register system
procedure SystemRegister(var world: TWorld; system: TSystem);
var
  worldWithSystems: ^TWorldWithSystems;
begin
  worldWithSystems := @world;
  
  // Check if we have room for more systems
  if worldWithSystems^.SystemCount >= MAX_SYSTEMS then
    Exit;
  
  // Add system to array
  worldWithSystems^.Systems[worldWithSystems^.SystemCount] := system;
  worldWithSystems^.SystemCount := worldWithSystems^.SystemCount + 1;
end;

// Run all registered systems
procedure SystemRunAll(var world: TWorld);
var
  i: Word;
  worldWithSystems: ^TWorldWithSystems;
begin
  worldWithSystems := @world;
  
  // Execute all systems in registration order
  for i := 0 to worldWithSystems^.SystemCount - 1 do
  begin
    if worldWithSystems^.Systems[i] <> nil then
      worldWithSystems^.Systems[i](world);
  end;
end;

// Get system count
function SystemGetCount(var world: TWorld): Word;
var
  worldWithSystems: ^TWorldWithSystems;
begin
  worldWithSystems := @world;
  Result := worldWithSystems^.SystemCount;
end;

end.

