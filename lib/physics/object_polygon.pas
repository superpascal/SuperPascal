unit Physics_ObjectPolygon;

interface

// Polygon physics object class
// Source: OO_AUDIT.md

uses
  Physics_ObjectBase,
  Physics_Types,
  Physics_Material,
  Math_Types,
  Math_Fixed,
  Collision_Types;

// ============================================================================
// Polygon Physics Object Class
// ============================================================================

type
  TPolygonPhysicsObject = class(TPhysicsObjectBase)
  protected
    FPolygon: TPolygon2D;
    FPolygonArea: Fixed16;
    
  public
    constructor Create(x, y: Fixed16; const polygon: TPolygon2D; material: TMaterial);
    
    property Polygon: TPolygon2D read FPolygon write FPolygon;
    property PolygonArea: Fixed16 read FPolygonArea write FPolygonArea;
    
    // Override virtual methods
    procedure CreateFragments(fragmentCount: integer; var fragments: TFragmentArray); override;
    function CalculateArea: Fixed16; override;
    function ToRecord: TPhysicsObject; override;
    procedure FromRecord(const rec: TPhysicsObject); override;
    
    // Recalculate polygon area
    procedure RecalculateArea;
  end;

implementation

uses
  Physics_Bounce;

// ============================================================================
// Polygon Physics Object Implementation
// ============================================================================

constructor TPolygonPhysicsObject.Create(x, y: Fixed16; const polygon: TPolygon2D; material: TMaterial);
begin
  inherited Create(x, y, material);
  FPolygon := polygon;
  RecalculateArea;
end;

procedure TPolygonPhysicsObject.CreateFragments(fragmentCount: integer; var fragments: TFragmentArray);
var
  rec: TPhysicsObject;
begin
  rec := ToRecord;
  if not Physics_Bounce.FragmentPolygon(rec, fragmentCount, fragments) then
  begin
    // Error handling - set empty fragments array
    SetLength(fragments, 0);
  end;
end;

function TPolygonPhysicsObject.CalculateArea: Fixed16;
begin
  Result := FPolygonArea;
end;

procedure TPolygonPhysicsObject.RecalculateArea;
begin
  FPolygonArea := Physics_Bounce.CalculatePolygonArea(FPolygon);
end;

function TPolygonPhysicsObject.ToRecord: TPhysicsObject;
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
  Result.ShapeType := stPolygon;
  Result.CircleRadius := 0;  // Not used for polygons
  Result.Polygon := FPolygon;
  Result.PolygonArea := FPolygonArea;
end;

procedure TPolygonPhysicsObject.FromRecord(const rec: TPhysicsObject);
begin
  inherited FromRecord(rec);
  
  if rec.ShapeType = stPolygon then
  begin
    FPolygon := rec.Polygon;
    FPolygonArea := rec.PolygonArea;
  end;
end;

end.

