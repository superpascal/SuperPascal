unit Physics_ObjectBase;

interface

// Base class for all physics objects
// Source: OO_AUDIT.md
//
// TPhysicsObjectBase provides the foundation for all physics objects with:
// - Position, velocity, acceleration
// - Material properties (via composition)
// - Virtual methods for polymorphic behavior

uses
  Physics_Types,
  Physics_Material,
  Math_Types,
  Math_Fixed;

// ============================================================================
// Base Physics Object Class
// ============================================================================

type
  TPhysicsObjectBase = class
  protected
    FX, FY: Fixed16;           // Position
    FVX, FVY: Fixed16;         // Velocity
    FAX, FAY: Fixed16;         // Acceleration
    
    FMaterial: TMaterial;       // Material properties (composition)
    
  public
    // Constructors
    constructor Create(x, y: Fixed16; material: TMaterial);
    destructor Destroy; override;
    
    // Property accessors
    property X: Fixed16 read FX write FX;
    property Y: Fixed16 read FY write FY;
    property VX: Fixed16 read FVX write FVX;
    property VY: Fixed16 read FVY write FVY;
    property AX: Fixed16 read FAX write FAX;
    property AY: Fixed16 read FAY write FAY;
    property Material: TMaterial read FMaterial;
    
    // Virtual methods (can be overridden)
    procedure Update(deltaTime: Fixed16); virtual;
    procedure ApplyGravity; virtual;
    procedure ApplyFriction; virtual;
    procedure ApplyAirResistance; virtual;
    function CalculateFragmentCount(collisionVelocity: Fixed16): integer; virtual;
    procedure CreateFragments(fragmentCount: integer; var fragments: TFragmentArray); virtual; abstract;
    function CalculateArea: Fixed16; virtual; abstract;
    
    // Material property queries (delegate to material)
    function IsSolid: boolean; virtual;
    function IsBouncy: boolean; virtual;
    function IsBrittle: boolean; virtual;
    
    // Bounce calculations
    function CalculateEffectiveBounce(const surface: TSurface): Fixed16; virtual;
    function CalculateEffectiveBounceObjects(const other: TPhysicsObjectBase): Fixed16; virtual;
    
    // Conversion to record (for ECS compatibility)
    function ToRecord: TPhysicsObject; virtual; abstract;
    procedure FromRecord(const rec: TPhysicsObject); virtual;
  end;

implementation

uses
  Physics_Bounce;

// ============================================================================
// Base Physics Object Implementation
// ============================================================================

constructor TPhysicsObjectBase.Create(x, y: Fixed16; material: TMaterial);
begin
  inherited Create;
  FX := x;
  FY := y;
  FVX := 0;
  FVY := 0;
  FAX := 0;
  FAY := 0;
  FMaterial := material;
end;

destructor TPhysicsObjectBase.Destroy;
begin
  // Note: Material is owned by caller, don't free it
  inherited Destroy;
end;

procedure TPhysicsObjectBase.Update(deltaTime: Fixed16);
begin
  // Apply gravity
  ApplyGravity;
  
  // Integrate acceleration to velocity
  FVX := FVX + Fixed16Mul(FAX, deltaTime);
  FVY := FVY + Fixed16Mul(FAY, deltaTime);
  
  // Integrate velocity to position
  FX := FX + Fixed16Mul(FVX, deltaTime);
  FY := FY + Fixed16Mul(FVY, deltaTime);
  
  // Reset acceleration (will be set by forces next frame)
  FAX := 0;
  FAY := 0;
end;

procedure TPhysicsObjectBase.ApplyGravity;
begin
  // Apply gravity (downward acceleration)
  FAY := FAY + PHYSICS_GRAVITY;
end;

procedure TPhysicsObjectBase.ApplyFriction;
begin
  // Apply friction to horizontal velocity
  FVX := Fixed16Mul(FVX, PHYSICS_FRICTION_COEFFICIENT);
  
  // Stop if velocity is very small
  if Fixed16Abs(FVX) < PHYSICS_VELOCITY_THRESHOLD then
    FVX := 0;
  if Fixed16Abs(FVY) < PHYSICS_VELOCITY_THRESHOLD then
    FVY := 0;
end;

procedure TPhysicsObjectBase.ApplyAirResistance;
begin
  // Apply to both X and Y velocity
  FVX := Fixed16Mul(FVX, PHYSICS_AIR_RESISTANCE);
  FVY := Fixed16Mul(FVY, PHYSICS_AIR_RESISTANCE);
end;

function TPhysicsObjectBase.CalculateFragmentCount(collisionVelocity: Fixed16): integer;
var
  rec: TPhysicsObject;
begin
  // Delegate to bounce module
  rec := ToRecord;
  Result := Physics_Bounce.CalculateFragmentCount(rec, collisionVelocity);
end;

function TPhysicsObjectBase.IsSolid: boolean;
begin
  Result := (FMaterial <> nil) and FMaterial.IsSolid;
end;

function TPhysicsObjectBase.IsBouncy: boolean;
begin
  Result := (FMaterial <> nil) and FMaterial.IsBouncy;
end;

function TPhysicsObjectBase.IsBrittle: boolean;
begin
  Result := (FMaterial <> nil) and FMaterial.IsBrittle;
end;

function TPhysicsObjectBase.CalculateEffectiveBounce(const surface: TSurface): Fixed16;
var
  rec: TPhysicsObject;
begin
  rec := ToRecord;
  Result := Physics_Bounce.CalculateEffectiveBounce(rec, surface);
end;

function TPhysicsObjectBase.CalculateEffectiveBounceObjects(const other: TPhysicsObjectBase): Fixed16;
var
  rec1, rec2: TPhysicsObject;
begin
  rec1 := ToRecord;
  rec2 := other.ToRecord;
  Result := Physics_Bounce.CalculateEffectiveBounceObjects(rec1, rec2);
end;

procedure TPhysicsObjectBase.FromRecord(const rec: TPhysicsObject);
begin
  FX := rec.X;
  FY := rec.Y;
  FVX := rec.VX;
  FVY := rec.VY;
  FAX := rec.AX;
  FAY := rec.AY;
  
  // Material properties are set via material object
  // This is a limitation - we'd need material lookup or creation
end;

end.

