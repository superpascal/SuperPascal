unit Collision_Types;

interface

// Collision detection types and constants
// Source: algorithms/05_CollisionDetection.md

uses
  Math_Types;

// ============================================================================
// 2D Types
// ============================================================================

// Axis-Aligned Bounding Box (2D)
type
  TAABB = record
    MinX, MinY: integer;  // Minimum corner
    MaxX, MaxY: integer;  // Maximum corner
  end;

// Circle (2D)
type
  TCircle = record
    X, Y: integer;        // Center position
    Radius: integer;      // Radius
  end;

// Point (2D)
type
  TPoint2D = record
    X, Y: integer;
  end;

// Polygon (2D) - Array of points
type
  TPolygon2D = record
    Points: array of TPoint2D;  // Vertices
    Count: integer;              // Number of vertices
  end;

// ============================================================================
// 3D Types
// ============================================================================

// Axis-Aligned Bounding Box (3D)
type
  TAABB3D = record
    MinX, MinY, MinZ: integer;  // Minimum corner
    MaxX, MaxY, MaxZ: integer;  // Maximum corner
  end;

// Sphere (3D)
type
  TSphere = record
    X, Y, Z: integer;     // Center position
    Radius: integer;       // Radius
  end;

// Point (3D)
type
  TPoint3D = record
    X, Y, Z: integer;
  end;

// ============================================================================
// Helper Functions
// ============================================================================

// Create AABB from center and size
function CreateAABB(centerX, centerY, width, height: integer): TAABB;

// Create AABB from two points
function CreateAABBFromPoints(x1, y1, x2, y2: integer): TAABB;

// Create circle from center and radius
function CreateCircle(centerX, centerY, radius: integer): TCircle;

// Expand AABB to include point
procedure ExpandAABB(var box: TAABB; x, y: integer);

// Expand AABB to include another AABB
procedure ExpandAABBToInclude(var box: TAABB; const other: TAABB);

implementation

// Create AABB from center and size
function CreateAABB(centerX, centerY, width, height: integer): TAABB;
var
  halfWidth, halfHeight: integer;
begin
  halfWidth := width div 2;
  halfHeight := height div 2;
  Result.MinX := centerX - halfWidth;
  Result.MinY := centerY - halfHeight;
  Result.MaxX := centerX + halfWidth;
  Result.MaxY := centerY + halfHeight;
end;

// Create AABB from two points
function CreateAABBFromPoints(x1, y1, x2, y2: integer): TAABB;
begin
  if x1 < x2 then
  begin
    Result.MinX := x1;
    Result.MaxX := x2;
  end
  else
  begin
    Result.MinX := x2;
    Result.MaxX := x1;
  end;
  
  if y1 < y2 then
  begin
    Result.MinY := y1;
    Result.MaxY := y2;
  end
  else
  begin
    Result.MinY := y2;
    Result.MaxY := y1;
  end;
end;

// Create circle from center and radius
function CreateCircle(centerX, centerY, radius: integer): TCircle;
begin
  Result.X := centerX;
  Result.Y := centerY;
  Result.Radius := radius;
end;

// Expand AABB to include point
procedure ExpandAABB(var box: TAABB; x, y: integer);
begin
  if x < box.MinX then box.MinX := x;
  if x > box.MaxX then box.MaxX := x;
  if y < box.MinY then box.MinY := y;
  if y > box.MaxY then box.MaxY := y;
end;

// Expand AABB to include another AABB
procedure ExpandAABBToInclude(var box: TAABB; const other: TAABB);
begin
  if other.MinX < box.MinX then box.MinX := other.MinX;
  if other.MaxX > box.MaxX then box.MaxX := other.MaxX;
  if other.MinY < box.MinY then box.MinY := other.MinY;
  if other.MaxY > box.MaxY then box.MaxY := other.MaxY;
end;

end.

