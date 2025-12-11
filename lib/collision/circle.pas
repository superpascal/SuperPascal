unit Collision_Circle;

interface

// Circle collision detection
// Source: algorithms/05_CollisionDetection.md
//
// Fast O(1) collision detection for circular objects
// Uses squared distances to avoid square root calculations

uses
  Collision_Types;

// ============================================================================
// Circle-Circle Collision
// ============================================================================

// Check if two circles overlap
function CircleCollision(const a, b: TCircle): boolean;

// Check if point is inside circle
function PointInCircle(x, y: integer; const circle: TCircle): boolean;

// Check if circle contains another circle
function CircleContains(const outer, inner: TCircle): boolean;

// Get distance between circle centers (squared, for performance)
function CircleDistanceSquared(const a, b: TCircle): LongInt;

// ============================================================================
// Circle-AABB Collision
// ============================================================================

// Check if circle overlaps with AABB
function CircleAABBCollision(const circle: TCircle; const box: TAABB): boolean;

// Check if AABB overlaps with circle (same as above, different parameter order)
function AABBCircleCollision(const box: TAABB; const circle: TCircle): boolean;

// ============================================================================
// Circle-Point Collision
// ============================================================================

// Check if point is inside circle (with squared distance optimization)
function PointInCircleSquared(x, y: integer; const circle: TCircle): boolean;

implementation

// ============================================================================
// Circle-Circle Collision
// ============================================================================

// Check if two circles overlap
// Algorithm: Check if distance between centers < sum of radii
// Optimization: Compare squared distances to avoid square root
function CircleCollision(const a, b: TCircle): boolean;
var
  dx, dy: integer;
  distanceSquared: LongInt;
  radiusSum: integer;
  radiusSumSquared: LongInt;
begin
  // Calculate distance between centers
  dx := a.X - b.X;
  dy := a.Y - b.Y;
  distanceSquared := LongInt(dx) * LongInt(dx) + LongInt(dy) * LongInt(dy);
  
  // Calculate sum of radii (squared)
  radiusSum := a.Radius + b.Radius;
  radiusSumSquared := LongInt(radiusSum) * LongInt(radiusSum);
  
  // Check if distance < sum of radii (using squared values)
  Result := distanceSquared < radiusSumSquared;
end;

// Check if point is inside circle
function PointInCircle(x, y: integer; const circle: TCircle): boolean;
var
  dx, dy: integer;
  distanceSquared: LongInt;
  radiusSquared: LongInt;
begin
  dx := x - circle.X;
  dy := y - circle.Y;
  distanceSquared := LongInt(dx) * LongInt(dx) + LongInt(dy) * LongInt(dy);
  radiusSquared := LongInt(circle.Radius) * LongInt(circle.Radius);
  Result := distanceSquared <= radiusSquared;
end;

// Check if point is inside circle (using squared distance - slightly faster)
function PointInCircleSquared(x, y: integer; const circle: TCircle): boolean;
begin
  Result := PointInCircle(x, y, circle);  // Same implementation
end;

// Check if circle contains another circle
function CircleContains(const outer, inner: TCircle): boolean;
var
  dx, dy: integer;
  distanceSquared: LongInt;
  radiusDiff: integer;
  radiusDiffSquared: LongInt;
begin
  // Calculate distance between centers
  dx := inner.X - outer.X;
  dy := inner.Y - outer.Y;
  distanceSquared := LongInt(dx) * LongInt(dx) + LongInt(dy) * LongInt(dy);
  
  // Calculate difference of radii (squared)
  // inner is contained if: distance + inner.Radius <= outer.Radius
  // Which is: distance <= outer.Radius - inner.Radius
  radiusDiff := outer.Radius - inner.Radius;
  if radiusDiff < 0 then
  begin
    Result := False;  // inner is larger than outer
    Exit;
  end;
  
  radiusDiffSquared := LongInt(radiusDiff) * LongInt(radiusDiff);
  Result := distanceSquared <= radiusDiffSquared;
end;

// Get distance between circle centers (squared, for performance)
function CircleDistanceSquared(const a, b: TCircle): LongInt;
var
  dx, dy: integer;
begin
  dx := a.X - b.X;
  dy := a.Y - b.Y;
  Result := LongInt(dx) * LongInt(dx) + LongInt(dy) * LongInt(dy);
end;

// ============================================================================
// Circle-AABB Collision
// ============================================================================

// Check if circle overlaps with AABB
// Algorithm: Find closest point on AABB to circle center, check if within circle
function CircleAABBCollision(const circle: TCircle; const box: TAABB): boolean;
var
  closestX, closestY: integer;
  dx, dy: integer;
  distanceSquared: LongInt;
  radiusSquared: LongInt;
begin
  // Find closest point on AABB to circle center
  closestX := circle.X;
  if closestX < box.MinX then
    closestX := box.MinX
  else if closestX > box.MaxX then
    closestX := box.MaxX;
  
  closestY := circle.Y;
  if closestY < box.MinY then
    closestY := box.MinY
  else if closestY > box.MaxY then
    closestY := box.MaxY;
  
  // Check if closest point is within circle
  dx := circle.X - closestX;
  dy := circle.Y - closestY;
  distanceSquared := LongInt(dx) * LongInt(dx) + LongInt(dy) * LongInt(dy);
  radiusSquared := LongInt(circle.Radius) * LongInt(circle.Radius);
  
  Result := distanceSquared <= radiusSquared;
end;

// Check if AABB overlaps with circle (same as above, different parameter order)
function AABBCircleCollision(const box: TAABB; const circle: TCircle): boolean;
begin
  Result := CircleAABBCollision(circle, box);
end;

end.

