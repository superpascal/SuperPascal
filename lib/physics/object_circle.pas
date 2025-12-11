unit Physics_ObjectCircle;

interface

// Circle/Bubble physics object class
// Source: OO_AUDIT.md

uses
  Physics_ObjectBase,
  Physics_Types,
  Physics_Material,
  Math_Types,
  Math_Fixed,
  Collision_Types;

// ============================================================================
// Circle Physics Object Class
// ============================================================================

type
  TCirclePhysicsObject = class(TPhysicsObjectBase)
  protected
    FRadius: Fixed16;
    
  public
    constructor Create(x, y, radius: Fixed16; material: TMaterial);
    
    property Radius: Fixed16 read FRadius write FRadius;
    
    // Override virtual methods
    procedure CreateFragments(fragmentCount: integer; var fragments: TFragmentArray); override;
    function CalculateArea: Fixed16; override;
    function ToRecord: TPhysicsObject; override;
    procedure FromRecord(const rec: TPhysicsObject); override;
  end;

implementation

uses
  Physics_Bounce;

// ============================================================================
// Circle Physics Object Implementation
// ============================================================================

constructor TCirclePhysicsObject.Create(x, y, radius: Fixed16; material: TMaterial);
begin
  inherited Create(x, y, material);
  FRadius := radius;
end;

procedure TCirclePhysicsObject.CreateFragments(fragmentCount: integer; var fragments: TFragmentArray);
var
  rec: TPhysicsObject;
begin
  rec := ToRecord;
  Physics_Bounce.FragmentCircle(rec, fragmentCount, fragments);
end;

function TCirclePhysicsObject.CalculateArea: Fixed16;
begin
  // Area = π * r²
  Result := Fixed16Mul(
    FIXED16_PI,
    Fixed16Mul(FRadius, FRadius)
  );
end;

function TCirclePhysicsObject.ToRecord: TPhysicsObject;
begin
  Result.X := FX;
  Result.Y := FY;
  Result.VX := FVX;
  Result.VY := FVY;
  Result.AX := FAX;
  Result.AY := FAY;
  
  // Material properties
  if FMaterial <> nil then
  begin
    Result.Bouncability := FMaterial.Bouncability;
    Result.Solidness := FMaterial.Solidness;
    Result.Brittleness := FMaterial.Brittleness;
    Result.Fragmentability := FMaterial.Fragmentability;
  end
  else
  begin
    Result.Bouncability := 0;
    Result.Solidness := 0;
    Result.Brittleness := 0;
    Result.Fragmentability := 0;
  end;
  
  // Shape properties
  Result.ShapeType := stCircle;
  Result.CircleRadius := FRadius;
  Result.PolygonArea := 0;  // Not used for circles
end;

procedure TCirclePhysicsObject.FromRecord(const rec: TPhysicsObject);
begin
  inherited FromRecord(rec);
  
  if rec.ShapeType = stCircle then
  begin
    FRadius := rec.CircleRadius;
  end;
end;

end.

