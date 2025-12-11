unit Physics_Material;

interface

// Material property classes
// Source: OO_AUDIT.md
//
// Material classes define the physical properties of objects:
// - Bouncability: How bouncy an object is (0 = no bounce, 1 = perfect bounce)
// - Solidness: How solid/rigid an object is (0 = soft, 1 = solid)
// - Brittleness: How brittle an object is (0 = not brittle, 1 = very brittle)
// - Fragmentability: How many fragments when breaking (0 = few, 1 = many)

uses
  Physics_Types,
  Math_Types;

// ============================================================================
// Base Material Class
// ============================================================================

type
  TMaterial = class
  protected
    FBouncability: Fixed16;   // 0-256 (0.0-1.0) - how bouncy
    FSolidness: Fixed16;      // 0-256 (0.0-1.0) - how solid/rigid
    FBrittleness: Fixed16;    // 0-256 (0.0-1.0) - how brittle
    FFragmentability: Fixed16; // 0-256 (0.0-1.0) - how many fragments
    
  public
    constructor Create(bouncability, solidness, brittleness, fragmentability: Fixed16);
    
    // Property accessors
    property Bouncability: Fixed16 read FBouncability write FBouncability;
    property Solidness: Fixed16 read FSolidness write FSolidness;
    property Brittleness: Fixed16 read FBrittleness write FBrittleness;
    property Fragmentability: Fixed16 read FFragmentability write FFragmentability;
    
    // Material property queries
    function IsSolid: boolean; virtual;
    function IsBouncy: boolean; virtual;
    function IsBrittle: boolean; virtual;
  end;

// ============================================================================
// Specific Material Types
// ============================================================================

// Rock material - solid, not bouncy, not brittle
type
  TRockMaterial = class(TMaterial)
  public
    constructor Create;
  end;

// Glass material - solid, not bouncy, very brittle
type
  TGlassMaterial = class(TMaterial)
  public
    constructor Create;
  end;

// Ball material - bouncy, not solid, not brittle
type
  TBallMaterial = class(TMaterial)
  public
    constructor Create;
  end;

// Bubble material - bouncy, not solid, not brittle, high fragmentability
type
  TBubbleMaterial = class(TMaterial)
  public
    constructor Create;
  end;

// Wood material - medium solidness, low bouncability, medium brittleness
type
  TWoodMaterial = class(TMaterial)
  public
    constructor Create;
  end;

// Metal material - very solid, low bouncability, low brittleness
type
  TMetalMaterial = class(TMaterial)
  public
    constructor Create;
  end;

// Rubber material - very bouncy, not solid, not brittle
type
  TRubberMaterial = class(TMaterial)
  public
    constructor Create;
  end;

// Sponge material - not solid, not bouncy, high absorbability (handled via surface)
type
  TSpongeMaterial = class(TMaterial)
  public
    constructor Create;
  end;

implementation

// ============================================================================
// Base Material Class Implementation
// ============================================================================

constructor TMaterial.Create(bouncability, solidness, brittleness, fragmentability: Fixed16);
begin
  inherited Create;
  FBouncability := bouncability;
  FSolidness := solidness;
  FBrittleness := brittleness;
  FFragmentability := fragmentability;
end;

function TMaterial.IsSolid: boolean;
begin
  Result := FSolidness >= PHYSICS_SOLIDNESS_THRESHOLD;
end;

function TMaterial.IsBouncy: boolean;
begin
  Result := FBouncability >= PHYSICS_BOUNCABILITY_THRESHOLD;
end;

function TMaterial.IsBrittle: boolean;
begin
  Result := FBrittleness >= PHYSICS_BRITTLENESS_THRESHOLD;
end;

// ============================================================================
// Specific Material Implementations
// ============================================================================

// Rock material - solid, not bouncy, not brittle
constructor TRockMaterial.Create;
begin
  inherited Create(
    0,      // Bouncability: 0.0 (not bouncy)
    256,    // Solidness: 1.0 (fully solid)
    0,      // Brittleness: 0.0 (not brittle)
    0       // Fragmentability: 0.0 (no fragments)
  );
end;

// Glass material - solid, not bouncy, very brittle
constructor TGlassMaterial.Create;
begin
  inherited Create(
    50,     // Bouncability: 0.2 (low bounce)
    256,    // Solidness: 1.0 (fully solid)
    230,    // Brittleness: 0.9 (very brittle)
    200     // Fragmentability: 0.78 (high fragments)
  );
end;

// Ball material - bouncy, not solid, not brittle
constructor TBallMaterial.Create;
begin
  inherited Create(
    230,    // Bouncability: 0.9 (very bouncy)
    50,     // Solidness: 0.2 (not solid)
    0,      // Brittleness: 0.0 (not brittle)
    0       // Fragmentability: 0.0 (no fragments)
  );
end;

// Bubble material - bouncy, not solid, not brittle, high fragmentability
constructor TBubbleMaterial.Create;
begin
  inherited Create(
    200,    // Bouncability: 0.78 (bouncy)
    30,     // Solidness: 0.12 (not solid)
    0,      // Brittleness: 0.0 (not brittle)
    180     // Fragmentability: 0.7 (high fragments)
  );
end;

// Wood material - medium solidness, low bouncability, medium brittleness
constructor TWoodMaterial.Create;
begin
  inherited Create(
    64,     // Bouncability: 0.25 (low bounce)
    180,    // Solidness: 0.7 (medium solid)
    150,    // Brittleness: 0.59 (medium brittle)
    100     // Fragmentability: 0.39 (medium fragments)
  );
end;

// Metal material - very solid, low bouncability, low brittleness
constructor TMetalMaterial.Create;
begin
  inherited Create(
    30,     // Bouncability: 0.12 (low bounce)
    256,    // Solidness: 1.0 (fully solid)
    50,     // Brittleness: 0.2 (low brittleness)
    0       // Fragmentability: 0.0 (no fragments)
  );
end;

// Rubber material - very bouncy, not solid, not brittle
constructor TRubberMaterial.Create;
begin
  inherited Create(
    256,    // Bouncability: 1.0 (perfect bounce)
    80,     // Solidness: 0.31 (not solid)
    0,      // Brittleness: 0.0 (not brittle)
    0       // Fragmentability: 0.0 (no fragments)
  );
end;

// Sponge material - not solid, not bouncy, high absorbability (handled via surface)
constructor TSpongeMaterial.Create;
begin
  inherited Create(
    30,     // Bouncability: 0.12 (low bounce)
    64,     // Solidness: 0.25 (soft)
    0,      // Brittleness: 0.0 (not brittle)
    0       // Fragmentability: 0.0 (no fragments)
  );
end;

end.

