unit Physics_BounceOO;

interface

// OO wrapper for bounce physics
// Source: OO_AUDIT.md
//
// Provides class-based bounce functions that use TPhysicsObjectBase classes
// instead of records, enabling polymorphism.

uses
  Physics_ObjectBase,
  Physics_Types,
  Math_Types,
  Math_Fixed;

// ============================================================================
// Class-Based Bounce Functions
// ============================================================================

// Bounce object off surface (wall/floor/ceiling)
procedure BounceOffSurface(
  obj: TPhysicsObjectBase;
  const surface: TSurface;
  normalX, normalY: Fixed16
);

// Bounce two objects off each other
procedure BounceObjects(
  obj1, obj2: TPhysicsObjectBase;
  normalX, normalY: Fixed16;
  var fragments1, fragments2: TFragmentArray
);

implementation

uses
  Physics_Bounce;

// ============================================================================
// Class-Based Bounce Implementation
// ============================================================================

procedure BounceOffSurface(
  obj: TPhysicsObjectBase;
  const surface: TSurface;
  normalX, normalY: Fixed16
);
var
  rec: TPhysicsObject;
begin
  rec := obj.ToRecord;
  Physics_Bounce.BounceOffSurface(rec, surface, normalX, normalY);
  obj.FromRecord(rec);
end;

procedure BounceObjects(
  obj1, obj2: TPhysicsObjectBase;
  normalX, normalY: Fixed16;
  var fragments1, fragments2: TFragmentArray
);
var
  rec1, rec2: TPhysicsObject;
begin
  rec1 := obj1.ToRecord;
  rec2 := obj2.ToRecord;
  Physics_Bounce.BounceObjects(rec1, rec2, normalX, normalY, fragments1, fragments2);
  obj1.FromRecord(rec1);
  obj2.FromRecord(rec2);
end;

end.

