unit ECS;

interface

type
  // Entity ID
  TEntity = Word;
  
  // Component mask (bitmask for component presence)
  TComponentMask = set of Byte;
  
  // Component type IDs
  TComponentID = Byte;
  
const
  // Entity constants
  ENTITY_NULL = $FFFF;  // Invalid entity ID
  MAX_ENTITIES = 256;   // Maximum entities (configurable per platform)
  
  // Component IDs
  COMPONENT_POSITION = 0;
  COMPONENT_VELOCITY = 1;
  COMPONENT_SPRITE = 2;
  MAX_COMPONENTS = 32;  // Maximum component types
  
  // System constants
  MAX_SYSTEMS = 32;     // Maximum registered systems

// Component type definitions
type
  TPosition = record
    X, Y: Integer;
  end;
  
  TVelocity = record
    DX, DY: Integer;
  end;
  
  TSprite = record
    SpriteID: Byte;
    TileID: Word;
    Palette: Byte;
    Visible: Boolean;
  end;

// World structure
type
  TWorld = record
    // Entity management
    EntityCount: Word;
    FreeEntities: array[0..MAX_ENTITIES-1] of TEntity;
    FreeCount: Word;
    
    // Component storage (Structure-of-Arrays)
    Positions: array[0..MAX_ENTITIES-1] of TPosition;
    Velocities: array[0..MAX_ENTITIES-1] of TVelocity;
    Sprites: array[0..MAX_ENTITIES-1] of TSprite;
    
    // Component presence tracking
    EntityComponents: array[0..MAX_ENTITIES-1] of TComponentMask;
    
    // Entity validity
    EntityValid: array[0..MAX_ENTITIES-1] of Boolean;
  end;

// World management functions
procedure WorldInit(var world: TWorld);
procedure WorldCleanup(var world: TWorld);

// Entity management
function EntityCreate(var world: TWorld): TEntity;
procedure EntityDestroy(var world: TWorld; entity: TEntity);
function EntityIsValid(var world: TWorld; entity: TEntity): Boolean;

implementation

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
    world.Velocities[i].DX := 0;
    world.Velocities[i].DY := 0;
    world.Sprites[i].SpriteID := 0;
    world.Sprites[i].TileID := 0;
    world.Sprites[i].Palette := 0;
    world.Sprites[i].Visible := False;
    
    // Initialize component presence
    world.EntityComponents[i] := [];
    
    // Initialize entity validity
    world.EntityValid[i] := False;
  end;
end;

// Cleanup world
procedure WorldCleanup(var world: TWorld);
begin
  // Reset all entities
  WorldInit(world);
end;

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
  world.Velocities[entity].DX := 0;
  world.Velocities[entity].DY := 0;
  world.Sprites[entity].SpriteID := 0;
  world.Sprites[entity].TileID := 0;
  world.Sprites[entity].Palette := 0;
  world.Sprites[entity].Visible := False;
  
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

