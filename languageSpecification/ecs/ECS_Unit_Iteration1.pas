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
    X, Y: Integer;  // Or Fixed16 for no-FPU platforms
  end;
  
  TVelocity = record
    DX, DY: Integer;  // Or Fixed16
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
    
    // Systems (will be implemented in Iteration 5)
    // Systems: array[0..MAX_SYSTEMS-1] of TSystem;
    // SystemCount: Word;
  end;

// World management functions
procedure WorldInit(var world: TWorld);
procedure WorldCleanup(var world: TWorld);

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

end.

