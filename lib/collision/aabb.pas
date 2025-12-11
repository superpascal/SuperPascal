unit Collision_AABB;

interface

// AABB (Axis-Aligned Bounding Box) collision detection
// Source: algorithms/05_CollisionDetection.md
//
// Very fast O(1) collision detection for axis-aligned boxes
// Good for initial broad-phase collision detection

uses
  Collision_Types;

// ============================================================================
// 2D AABB Collision
// ============================================================================

// Check if two AABBs overlap
function AABBCollision(const a, b: TAABB): boolean;

// Check if point is inside AABB
function PointInAABB(x, y: integer; const box: TAABB): boolean;

// Check if AABB contains another AABB
function AABBContains(const outer, inner: TAABB): boolean;

// Get intersection AABB (overlapping region)
function AABBIntersection(const a, b: TAABB; var intersection: TAABB): boolean;

// Get union AABB (bounding box containing both)
function AABBUnion(const a, b: TAABB): TAABB;

// Get AABB center
procedure AABBCenter(const box: TAABB; var centerX, centerY: integer);

// Get AABB width and height
procedure AABBSize(const box: TAABB; var width, height: integer);

// ============================================================================
// 3D AABB Collision
// ============================================================================

// Check if two 3D AABBs overlap
function AABB3DCollision(const a, b: TAABB3D): boolean;

// Check if point is inside 3D AABB
function PointInAABB3D(x, y, z: integer; const box: TAABB3D): boolean;

implementation

// ============================================================================
// 2D AABB Collision
// ============================================================================

// Check if two AABBs overlap
// Algorithm: Check if boxes overlap on both axes
function AABBCollision(const a, b: TAABB): boolean;
begin
  Result := (a.MinX < b.MaxX) and (a.MaxX > b.MinX) and
            (a.MinY < b.MaxY) and (a.MaxY > b.MinY);
end;

// Check if point is inside AABB
function PointInAABB(x, y: integer; const box: TAABB): boolean;
begin
  Result := (x >= box.MinX) and (x <= box.MaxX) and
            (y >= box.MinY) and (y <= box.MaxY);
end;

// Check if AABB contains another AABB
function AABBContains(const outer, inner: TAABB): boolean;
begin
  Result := (outer.MinX <= inner.MinX) and (outer.MaxX >= inner.MaxX) and
            (outer.MinY <= inner.MinY) and (outer.MaxY >= inner.MaxY);
end;

// Get intersection AABB (overlapping region)
// Returns true if boxes overlap, false otherwise
function AABBIntersection(const a, b: TAABB; var intersection: TAABB): boolean;
begin
  // Check if boxes overlap
  if not AABBCollision(a, b) then
  begin
    Result := False;
    Exit;
  end;
  
  // Calculate intersection
  intersection.MinX := a.MinX;
  if b.MinX > intersection.MinX then intersection.MinX := b.MinX;
  
  intersection.MaxX := a.MaxX;
  if b.MaxX < intersection.MaxX then intersection.MaxX := b.MaxX;
  
  intersection.MinY := a.MinY;
  if b.MinY > intersection.MinY then intersection.MinY := b.MinY;
  
  intersection.MaxY := a.MaxY;
  if b.MaxY < intersection.MaxY then intersection.MaxY := b.MaxY;
  
  Result := True;
end;

// Get union AABB (bounding box containing both)
function AABBUnion(const a, b: TAABB): TAABB;
begin
  Result.MinX := a.MinX;
  if b.MinX < Result.MinX then Result.MinX := b.MinX;
  
  Result.MaxX := a.MaxX;
  if b.MaxX > Result.MaxX then Result.MaxX := b.MaxX;
  
  Result.MinY := a.MinY;
  if b.MinY < Result.MinY then Result.MinY := b.MinY;
  
  Result.MaxY := a.MaxY;
  if b.MaxY > Result.MaxY then Result.MaxY := b.MaxY;
end;

// Get AABB center
procedure AABBCenter(const box: TAABB; var centerX, centerY: integer);
begin
  centerX := (box.MinX + box.MaxX) div 2;
  centerY := (box.MinY + box.MaxY) div 2;
end;

// Get AABB width and height
procedure AABBSize(const box: TAABB; var width, height: integer);
begin
  width := box.MaxX - box.MinX;
  height := box.MaxY - box.MinY;
end;

// ============================================================================
// 3D AABB Collision
// ============================================================================

// Check if two 3D AABBs overlap
function AABB3DCollision(const a, b: TAABB3D): boolean;
begin
  Result := (a.MinX < b.MaxX) and (a.MaxX > b.MinX) and
            (a.MinY < b.MaxY) and (a.MaxY > b.MinY) and
            (a.MinZ < b.MaxZ) and (a.MaxZ > b.MinZ);
end;

// Check if point is inside 3D AABB
function PointInAABB3D(x, y, z: integer; const box: TAABB3D): boolean;
begin
  Result := (x >= box.MinX) and (x <= box.MaxX) and
            (y >= box.MinY) and (y <= box.MaxY) and
            (z >= box.MinZ) and (z <= box.MaxZ);
end;

end.

