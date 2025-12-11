unit Collision;

interface

// Collision detection library
// Source: algorithms/05_CollisionDetection.md
//
// Provides fast collision detection algorithms for:
// - AABB (Axis-Aligned Bounding Boxes)
// - Circles
// - Polygons
// - Point-in-shape tests
// - Polygon clipping

uses
  Collision_Types,
  Collision_AABB,
  Collision_Circle,
  Collision_Polygon;

// Re-export types
type
  TAABB = Collision_Types.TAABB;
  TCircle = Collision_Types.TCircle;
  TPoint2D = Collision_Types.TPoint2D;
  TPolygon2D = Collision_Types.TPolygon2D;
  TAABB3D = Collision_Types.TAABB3D;
  TSphere = Collision_Types.TSphere;
  TPoint3D = Collision_Types.TPoint3D;

// Re-export helper functions
function CreateAABB(centerX, centerY, width, height: integer): TAABB;
function CreateAABBFromPoints(x1, y1, x2, y2: integer): TAABB;
function CreateCircle(centerX, centerY, radius: integer): TCircle;
procedure ExpandAABB(var box: TAABB; x, y: integer);
procedure ExpandAABBToInclude(var box: TAABB; const other: TAABB);

// Re-export AABB functions
function AABBCollision(const a, b: TAABB): boolean;
function PointInAABB(x, y: integer; const box: TAABB): boolean;
function AABBContains(const outer, inner: TAABB): boolean;
function AABBIntersection(const a, b: TAABB; var intersection: TAABB): boolean;
function AABBUnion(const a, b: TAABB): TAABB;
procedure AABBCenter(const box: TAABB; var centerX, centerY: integer);
procedure AABBSize(const box: TAABB; var width, height: integer);
function AABB3DCollision(const a, b: TAABB3D): boolean;
function PointInAABB3D(x, y, z: integer; const box: TAABB3D): boolean;

// Re-export Circle functions
function CircleCollision(const a, b: TCircle): boolean;
function PointInCircle(x, y: integer; const circle: TCircle): boolean;
function CircleContains(const outer, inner: TCircle): boolean;
function CircleDistanceSquared(const a, b: TCircle): LongInt;
function CircleAABBCollision(const circle: TCircle; const box: TAABB): boolean;
function AABBCircleCollision(const box: TAABB; const circle: TCircle): boolean;

// Re-export Polygon functions
function PointInPolygon(x, y: integer; const polygon: TPolygon2D): boolean;
function ClipPolygonToScreen(
  const input: TPolygon2D;
  var output: TPolygon2D;
  screenLeft, screenTop, screenRight, screenBottom: integer
): integer;
procedure ClipEdgeToScreen(
  x1, y1, x2, y2: integer;
  var clippedX1, clippedY1, clippedX2, clippedY2: integer;
  var visible: boolean;
  screenLeft, screenTop, screenRight, screenBottom: integer
);
function PolygonAABBCollision(const polygon: TPolygon2D; const box: TAABB): boolean;
function AABBPolygonCollision(const box: TAABB; const polygon: TPolygon2D): boolean;
function PolygonCollision(const a, b: TPolygon2D): boolean;
function PolygonGetAABB(const polygon: TPolygon2D): TAABB;

implementation

// Re-export implementations
function CreateAABB(centerX, centerY, width, height: integer): TAABB; begin Result := Collision_Types.CreateAABB(centerX, centerY, width, height); end;
function CreateAABBFromPoints(x1, y1, x2, y2: integer): TAABB; begin Result := Collision_Types.CreateAABBFromPoints(x1, y1, x2, y2); end;
function CreateCircle(centerX, centerY, radius: integer): TCircle; begin Result := Collision_Types.CreateCircle(centerX, centerY, radius); end;
procedure ExpandAABB(var box: TAABB; x, y: integer); begin Collision_Types.ExpandAABB(box, x, y); end;
procedure ExpandAABBToInclude(var box: TAABB; const other: TAABB); begin Collision_Types.ExpandAABBToInclude(box, other); end;
function AABBCollision(const a, b: TAABB): boolean; begin Result := Collision_AABB.AABBCollision(a, b); end;
function PointInAABB(x, y: integer; const box: TAABB): boolean; begin Result := Collision_AABB.PointInAABB(x, y, box); end;
function AABBContains(const outer, inner: TAABB): boolean; begin Result := Collision_AABB.AABBContains(outer, inner); end;
function AABBIntersection(const a, b: TAABB; var intersection: TAABB): boolean; begin Result := Collision_AABB.AABBIntersection(a, b, intersection); end;
function AABBUnion(const a, b: TAABB): TAABB; begin Result := Collision_AABB.AABBUnion(a, b); end;
procedure AABBCenter(const box: TAABB; var centerX, centerY: integer); begin Collision_AABB.AABBCenter(box, centerX, centerY); end;
procedure AABBSize(const box: TAABB; var width, height: integer); begin Collision_AABB.AABBSize(box, width, height); end;
function AABB3DCollision(const a, b: TAABB3D): boolean; begin Result := Collision_AABB.AABB3DCollision(a, b); end;
function PointInAABB3D(x, y, z: integer; const box: TAABB3D): boolean; begin Result := Collision_AABB.PointInAABB3D(x, y, z, box); end;
function CircleCollision(const a, b: TCircle): boolean; begin Result := Collision_Circle.CircleCollision(a, b); end;
function PointInCircle(x, y: integer; const circle: TCircle): boolean; begin Result := Collision_Circle.PointInCircle(x, y, circle); end;
function CircleContains(const outer, inner: TCircle): boolean; begin Result := Collision_Circle.CircleContains(outer, inner); end;
function CircleDistanceSquared(const a, b: TCircle): LongInt; begin Result := Collision_Circle.CircleDistanceSquared(a, b); end;
function CircleAABBCollision(const circle: TCircle; const box: TAABB): boolean; begin Result := Collision_Circle.CircleAABBCollision(circle, box); end;
function AABBCircleCollision(const box: TAABB; const circle: TCircle): boolean; begin Result := Collision_Circle.AABBCircleCollision(box, circle); end;
function PointInPolygon(x, y: integer; const polygon: TPolygon2D): boolean; begin Result := Collision_Polygon.PointInPolygon(x, y, polygon); end;
function ClipPolygonToScreen(const input: TPolygon2D; var output: TPolygon2D; screenLeft, screenTop, screenRight, screenBottom: integer): integer; begin Result := Collision_Polygon.ClipPolygonToScreen(input, output, screenLeft, screenTop, screenRight, screenBottom); end;
procedure ClipEdgeToScreen(x1, y1, x2, y2: integer; var clippedX1, clippedY1, clippedX2, clippedY2: integer; var visible: boolean; screenLeft, screenTop, screenRight, screenBottom: integer); begin Collision_Polygon.ClipEdgeToScreen(x1, y1, x2, y2, clippedX1, clippedY1, clippedX2, clippedY2, visible, screenLeft, screenTop, screenRight, screenBottom); end;
function PolygonAABBCollision(const polygon: TPolygon2D; const box: TAABB): boolean; begin Result := Collision_Polygon.PolygonAABBCollision(polygon, box); end;
function AABBPolygonCollision(const box: TAABB; const polygon: TPolygon2D): boolean; begin Result := Collision_Polygon.AABBPolygonCollision(box, polygon); end;
function PolygonCollision(const a, b: TPolygon2D): boolean; begin Result := Collision_Polygon.PolygonCollision(a, b); end;
function PolygonGetAABB(const polygon: TPolygon2D): TAABB; begin Result := Collision_Polygon.PolygonGetAABB(polygon); end;

end.

