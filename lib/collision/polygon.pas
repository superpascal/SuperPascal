unit Collision_Polygon;

interface

// Polygon collision detection and clipping
// Source: algorithms/05_CollisionDetection.md
//
// Includes:
// - Polygon clipping (Sutherland-Hodgman algorithm)
// - Point-in-polygon test
// - Polygon-AABB collision
// - Polygon-polygon collision (basic)

uses
  Collision_Types,
  Collision_AABB;

// ============================================================================
// Point-in-Polygon Tests
// ============================================================================

// Check if point is inside polygon (ray casting algorithm)
function PointInPolygon(x, y: integer; const polygon: TPolygon2D): boolean;

// ============================================================================
// Polygon Clipping
// ============================================================================

// Clip polygon to screen boundaries (Sutherland-Hodgman algorithm)
// Returns number of output vertices (0 if polygon is completely outside)
function ClipPolygonToScreen(
  const input: TPolygon2D;
  var output: TPolygon2D;
  screenLeft, screenTop, screenRight, screenBottom: integer
): integer;

// Clip edge to screen boundaries
procedure ClipEdgeToScreen(
  x1, y1, x2, y2: integer;
  var clippedX1, clippedY1, clippedX2, clippedY2: integer;
  var visible: boolean;
  screenLeft, screenTop, screenRight, screenBottom: integer
);

// ============================================================================
// Polygon-AABB Collision
// ============================================================================

// Check if polygon overlaps with AABB
function PolygonAABBCollision(const polygon: TPolygon2D; const box: TAABB): boolean;

// Check if AABB overlaps with polygon
function AABBPolygonCollision(const box: TAABB; const polygon: TPolygon2D): boolean;

// ============================================================================
// Polygon-Polygon Collision (Basic)
// ============================================================================

// Check if two polygons overlap (basic AABB test first, then point-in-polygon)
function PolygonCollision(const a, b: TPolygon2D): boolean;

// Get AABB bounding box for polygon
function PolygonGetAABB(const polygon: TPolygon2D): TAABB;

implementation

// ============================================================================
// Point-in-Polygon Tests
// ============================================================================

// Check if point is inside polygon (ray casting algorithm)
// Algorithm: Cast ray from point to infinity, count intersections with polygon edges
// Odd number of intersections = inside, even = outside
function PointInPolygon(x, y: integer; const polygon: TPolygon2D): boolean;
var
  i, j: integer;
  xi, yi, xj, yj: integer;
  intersect: boolean;
  count: integer;
begin
  count := 0;
  
  // For each edge of polygon
  for i := 0 to polygon.Count - 1 do
  begin
    j := (i + 1) mod polygon.Count;
    xi := polygon.Points[i].X;
    yi := polygon.Points[i].Y;
    xj := polygon.Points[j].X;
    yj := polygon.Points[j].Y;
    
    // Check if ray from (x,y) to (x,infinity) intersects this edge
    // Ray is horizontal to the right
    if ((yi > y) <> (yj > y)) then  // Edge crosses horizontal line at y
    begin
      // Check if intersection point is to the right of x
      // Intersection: x = xi + (y - yi) * (xj - xi) / (yj - yi)
      // We only care if x_intersection > x
      if (x < (xi + ((y - yi) * (xj - xi)) div (yj - yi))) then
        count := count + 1;
    end;
  end;
  
  // Odd count = inside, even = outside
  Result := (count mod 2) = 1;
end;

// ============================================================================
// Polygon Clipping
// ============================================================================

// Clip edge to screen boundaries
procedure ClipEdgeToScreen(
  x1, y1, x2, y2: integer;
  var clippedX1, clippedY1, clippedX2, clippedY2: integer;
  var visible: boolean;
  screenLeft, screenTop, screenRight, screenBottom: integer
);
begin
  visible := True;
  clippedX1 := x1;
  clippedY1 := y1;
  clippedX2 := x2;
  clippedY2 := y2;
  
  // Clip to top
  if (clippedY1 < screenTop) and (clippedY2 < screenTop) then
  begin
    visible := False;
    Exit;
  end;
  if clippedY1 < screenTop then
  begin
    // Intersect with top edge
    if (clippedY2 - clippedY1) <> 0 then
      clippedX1 := clippedX1 + ((screenTop - clippedY1) * (clippedX2 - clippedX1)) div (clippedY2 - clippedY1);
    clippedY1 := screenTop;
  end;
  if clippedY2 < screenTop then
  begin
    if (clippedY1 - clippedY2) <> 0 then
      clippedX2 := clippedX2 + ((screenTop - clippedY2) * (clippedX1 - clippedX2)) div (clippedY1 - clippedY2);
    clippedY2 := screenTop;
  end;
  
  // Clip to bottom
  if (clippedY1 > screenBottom) and (clippedY2 > screenBottom) then
  begin
    visible := False;
    Exit;
  end;
  if clippedY1 > screenBottom then
  begin
    if (clippedY2 - clippedY1) <> 0 then
      clippedX1 := clippedX1 + ((screenBottom - clippedY1) * (clippedX2 - clippedX1)) div (clippedY2 - clippedY1);
    clippedY1 := screenBottom;
  end;
  if clippedY2 > screenBottom then
  begin
    if (clippedY1 - clippedY2) <> 0 then
      clippedX2 := clippedX2 + ((screenBottom - clippedY2) * (clippedX1 - clippedX2)) div (clippedY1 - clippedY2);
    clippedY2 := screenBottom;
  end;
  
  // Clip to left
  if (clippedX1 < screenLeft) and (clippedX2 < screenLeft) then
  begin
    visible := False;
    Exit;
  end;
  if clippedX1 < screenLeft then
  begin
    if (clippedX2 - clippedX1) <> 0 then
      clippedY1 := clippedY1 + ((screenLeft - clippedX1) * (clippedY2 - clippedY1)) div (clippedX2 - clippedX1);
    clippedX1 := screenLeft;
  end;
  if clippedX2 < screenLeft then
  begin
    if (clippedX1 - clippedX2) <> 0 then
      clippedY2 := clippedY2 + ((screenLeft - clippedX2) * (clippedY1 - clippedY2)) div (clippedX1 - clippedX2);
    clippedX2 := screenLeft;
  end;
  
  // Clip to right
  if (clippedX1 > screenRight) and (clippedX2 > screenRight) then
  begin
    visible := False;
    Exit;
  end;
  if clippedX1 > screenRight then
  begin
    if (clippedX2 - clippedX1) <> 0 then
      clippedY1 := clippedY1 + ((screenRight - clippedX1) * (clippedY2 - clippedY1)) div (clippedX2 - clippedX1);
    clippedX1 := screenRight;
  end;
  if clippedX2 > screenRight then
  begin
    if (clippedX1 - clippedX2) <> 0 then
      clippedY2 := clippedY2 + ((screenRight - clippedX2) * (clippedY1 - clippedY2)) div (clippedX1 - clippedX2);
    clippedX2 := screenRight;
  end;
end;

// Clip polygon to screen boundaries (Sutherland-Hodgman algorithm)
// Clips against each edge: left, right, top, bottom
function ClipPolygonToScreen(
  const input: TPolygon2D;
  var output: TPolygon2D;
  screenLeft, screenTop, screenRight, screenBottom: integer
): integer;
var
  temp: TPolygon2D;
  i, j: integer;
  inputCount, outputCount: integer;
  prevInside, currInside: boolean;
  prevX, prevY, currX, currY: integer;
  intersectX, intersectY: integer;
  dx, dy, denom: integer;
  
  // Helper: Check if point is inside clipping rectangle
  function IsInside(x, y: integer; edge: integer): boolean;
  begin
    case edge of
      0: Result := x >= screenLeft;    // Left edge
      1: Result := x <= screenRight;    // Right edge
      2: Result := y >= screenTop;      // Top edge
      3: Result := y <= screenBottom;   // Bottom edge
    else
      Result := False;
    end;
  end;
  
  // Helper: Calculate intersection with clipping edge
  procedure CalculateIntersection(x1, y1, x2, y2: integer; edge: integer; var ix, iy: integer);
  var
    t: integer;
  begin
    case edge of
      0:  // Left edge
      begin
        if (x2 - x1) <> 0 then
        begin
          t := ((screenLeft - x1) * (y2 - y1)) div (x2 - x1);
          ix := screenLeft;
          iy := y1 + t;
        end
        else
        begin
          ix := screenLeft;
          iy := y1;
        end;
      end;
      1:  // Right edge
      begin
        if (x2 - x1) <> 0 then
        begin
          t := ((screenRight - x1) * (y2 - y1)) div (x2 - x1);
          ix := screenRight;
          iy := y1 + t;
        end
        else
        begin
          ix := screenRight;
          iy := y1;
        end;
      end;
      2:  // Top edge
      begin
        if (y2 - y1) <> 0 then
        begin
          t := ((screenTop - y1) * (x2 - x1)) div (y2 - y1);
          ix := x1 + t;
          iy := screenTop;
        end
        else
        begin
          ix := x1;
          iy := screenTop;
        end;
      end;
      3:  // Bottom edge
      begin
        if (y2 - y1) <> 0 then
        begin
          t := ((screenBottom - y1) * (x2 - x1)) div (y2 - y1);
          ix := x1 + t;
          iy := screenBottom;
        end
        else
        begin
          ix := x1;
          iy := screenBottom;
        end;
      end;
    end;
  end;
  
  // Clip against a single edge
  procedure ClipAgainstEdge(edge: integer; var inputPoly, outputPoly: TPolygon2D);
  var
    i: integer;
    prevInside, currInside: boolean;
    prevX, prevY, currX, currY: integer;
    intersectX, intersectY: integer;
  begin
    if inputPoly.Count = 0 then
    begin
      outputPoly.Count := 0;
      Exit;
    end;
    
    outputPoly.Count := 0;
    prevX := inputPoly.Points[inputPoly.Count - 1].X;
    prevY := inputPoly.Points[inputPoly.Count - 1].Y;
    prevInside := IsInside(prevX, prevY, edge);
    
    for i := 0 to inputPoly.Count - 1 do
    begin
      currX := inputPoly.Points[i].X;
      currY := inputPoly.Points[i].Y;
      currInside := IsInside(currX, currY, edge);
      
      if currInside then
      begin
        if not prevInside then
        begin
          // Previous outside, current inside: add intersection
          CalculateIntersection(prevX, prevY, currX, currY, edge, intersectX, intersectY);
          if outputPoly.Count < Length(outputPoly.Points) then
          begin
            outputPoly.Points[outputPoly.Count].X := intersectX;
            outputPoly.Points[outputPoly.Count].Y := intersectY;
            outputPoly.Count := outputPoly.Count + 1;
          end;
        end;
        // Add current vertex
        if outputPoly.Count < Length(outputPoly.Points) then
        begin
          outputPoly.Points[outputPoly.Count].X := currX;
          outputPoly.Points[outputPoly.Count].Y := currY;
          outputPoly.Count := outputPoly.Count + 1;
        end;
      end
      else
      begin
        if prevInside then
        begin
          // Previous inside, current outside: add intersection
          CalculateIntersection(prevX, prevY, currX, currY, edge, intersectX, intersectY);
          if outputPoly.Count < Length(outputPoly.Points) then
          begin
            outputPoly.Points[outputPoly.Count].X := intersectX;
            outputPoly.Points[outputPoly.Count].Y := intersectY;
            outputPoly.Count := outputPoly.Count + 1;
          end;
        end;
        // Current outside: skip
      end;
      
      prevX := currX;
      prevY := currY;
      prevInside := currInside;
    end;
  end;
  
begin
  // Initialize output polygon (assume same max size as input)
  if Length(output.Points) < input.Count * 2 then
  begin
    // Allocate enough space for clipped polygon (may have more vertices)
    SetLength(output.Points, input.Count * 2);
  end;
  
  // Use temp as working buffer
  SetLength(temp.Points, input.Count * 2);
  
  // Start with input polygon
  temp.Count := input.Count;
  for i := 0 to input.Count - 1 do
  begin
    temp.Points[i] := input.Points[i];
  end;
  
  // Clip against each edge: left, right, top, bottom
  ClipAgainstEdge(0, temp, output);  // Left
  temp := output;
  ClipAgainstEdge(1, temp, output);  // Right
  temp := output;
  ClipAgainstEdge(2, temp, output);  // Top
  temp := output;
  ClipAgainstEdge(3, temp, output);  // Bottom
  
  Result := output.Count;
end;

// ============================================================================
// Polygon-AABB Collision
// ============================================================================

// Get AABB bounding box for polygon
function PolygonGetAABB(const polygon: TPolygon2D): TAABB;
var
  i: integer;
begin
  if polygon.Count = 0 then
  begin
    Result.MinX := 0;
    Result.MinY := 0;
    Result.MaxX := 0;
    Result.MaxY := 0;
    Exit;
  end;
  
  Result.MinX := polygon.Points[0].X;
  Result.MaxX := polygon.Points[0].X;
  Result.MinY := polygon.Points[0].Y;
  Result.MaxY := polygon.Points[0].Y;
  
  for i := 1 to polygon.Count - 1 do
  begin
    if polygon.Points[i].X < Result.MinX then
      Result.MinX := polygon.Points[i].X;
    if polygon.Points[i].X > Result.MaxX then
      Result.MaxX := polygon.Points[i].X;
    if polygon.Points[i].Y < Result.MinY then
      Result.MinY := polygon.Points[i].Y;
    if polygon.Points[i].Y > Result.MaxY then
      Result.MaxY := polygon.Points[i].Y;
  end;
end;

// Check if polygon overlaps with AABB
// Algorithm: Check AABB collision first, then check if any polygon vertex is inside AABB
function PolygonAABBCollision(const polygon: TPolygon2D; const box: TAABB): boolean;
var
  polyAABB: TAABB;
  i: integer;
begin
  // Quick rejection: Check AABB collision first
  polyAABB := PolygonGetAABB(polygon);
  if not AABBCollision(polyAABB, box) then
  begin
    Result := False;
    Exit;
  end;
  
  // Check if any polygon vertex is inside AABB
  for i := 0 to polygon.Count - 1 do
  begin
    if PointInAABB(polygon.Points[i].X, polygon.Points[i].Y, box) then
    begin
      Result := True;
      Exit;
    end;
  end;
  
  // Check if any AABB corner is inside polygon
  if PointInPolygon(box.MinX, box.MinY, polygon) or
     PointInPolygon(box.MaxX, box.MinY, polygon) or
     PointInPolygon(box.MinX, box.MaxY, polygon) or
     PointInPolygon(box.MaxX, box.MaxY, polygon) then
  begin
    Result := True;
    Exit;
  end;
  
  // More complex: Check if any polygon edge intersects AABB
  // For now, return False (conservative)
  // TODO: Implement edge-AABB intersection test
  Result := False;
end;

// Check if AABB overlaps with polygon
function AABBPolygonCollision(const box: TAABB; const polygon: TPolygon2D): boolean;
begin
  Result := PolygonAABBCollision(polygon, box);
end;

// ============================================================================
// Polygon-Polygon Collision (Basic)
// ============================================================================

// Check if two polygons overlap (basic AABB test first, then point-in-polygon)
function PolygonCollision(const a, b: TPolygon2D): boolean;
var
  aabbA, aabbB: TAABB;
  i: integer;
begin
  // Quick rejection: Check AABB collision first
  aabbA := PolygonGetAABB(a);
  aabbB := PolygonGetAABB(b);
  if not AABBCollision(aabbA, aabbB) then
  begin
    Result := False;
    Exit;
  end;
  
  // Check if any vertex of A is inside B
  for i := 0 to a.Count - 1 do
  begin
    if PointInPolygon(a.Points[i].X, a.Points[i].Y, b) then
    begin
      Result := True;
      Exit;
    end;
  end;
  
  // Check if any vertex of B is inside A
  for i := 0 to b.Count - 1 do
  begin
    if PointInPolygon(b.Points[i].X, b.Points[i].Y, a) then
    begin
      Result := True;
      Exit;
    end;
  end;
  
  // More complex: Check if edges intersect
  // For now, return False (conservative)
  // TODO: Implement edge-edge intersection test
  Result := False;
end;

end.

