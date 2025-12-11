unit Physics_Conversion;

interface

// Conversion utilities between records and classes
// Source: OO_AUDIT.md
//
// Provides utilities to convert between record-based (ECS-compatible) and
// class-based (OO) physics objects for backward compatibility.

uses
  Physics_Types,
  Physics_ObjectBase,
  Physics_ObjectCircle,
  Physics_ObjectPolygon,
  Physics_Material,
  Math_Types;

// ============================================================================
// Record to Class Conversion
// ============================================================================

// Create physics object class from record
// Note: Material must be provided separately as records don't store material references
function CreatePhysicsObjectFromRecord(
  const rec: TPhysicsObject;
  material: TMaterial
): TPhysicsObjectBase;

// Create circle physics object from record
function CreateCircleFromRecord(
  const rec: TPhysicsObject;
  material: TMaterial
): TCirclePhysicsObject;

// Create polygon physics object from record
function CreatePolygonFromRecord(
  const rec: TPhysicsObject;
  material: TMaterial
): TPolygonPhysicsObject;

// ============================================================================
// Material Creation from Record Properties
// ============================================================================

// Create material from record's material properties
function CreateMaterialFromRecord(const rec: TPhysicsObject): TMaterial;

implementation

// ============================================================================
// Record to Class Conversion Implementation
// ============================================================================

function CreatePhysicsObjectFromRecord(
  const rec: TPhysicsObject;
  material: TMaterial
): TPhysicsObjectBase;
begin
  case rec.ShapeType of
    stCircle:
      Result := CreateCircleFromRecord(rec, material);
    stPolygon:
      Result := CreatePolygonFromRecord(rec, material);
    else
      Result := nil;  // Unknown shape type
  end;
end;

function CreateCircleFromRecord(
  const rec: TPhysicsObject;
  material: TMaterial
): TCirclePhysicsObject;
begin
  Result := TCirclePhysicsObject.Create(rec.X, rec.Y, rec.CircleRadius, material);
  Result.VX := rec.VX;
  Result.VY := rec.VY;
  Result.AX := rec.AX;
  Result.AY := rec.AY;
end;

function CreatePolygonFromRecord(
  const rec: TPhysicsObject;
  material: TMaterial
): TPolygonPhysicsObject;
begin
  Result := TPolygonPhysicsObject.Create(rec.X, rec.Y, rec.Polygon, material);
  Result.VX := rec.VX;
  Result.VY := rec.VY;
  Result.AX := rec.AX;
  Result.AY := rec.AY;
  Result.PolygonArea := rec.PolygonArea;
end;

function CreateMaterialFromRecord(const rec: TPhysicsObject): TMaterial;
begin
  // Create material with properties from record
  Result := TMaterial.Create(
    rec.Bouncability,
    rec.Solidness,
    rec.Brittleness,
    rec.Fragmentability
  );
end;

end.

