unit Graphics_Polygon;

interface

// Polygon rendering algorithms
// Source: algorithms/04_GraphicsAlgorithms.md
// Based on: Mikro archive OTMPOLY.TXT

uses
  Graphics_Types,
  Math_Types,
  Math_Fixed;

// Edge structure for scan conversion
type
  TEdge = record
    X: Fixed16;      // Current X position
    XStep: Fixed16;  // X increment per scanline (dx/dy)
    YStart: integer;
    YEnd: integer;
  end;

// Scan convert a single edge for polygon filling
procedure ScanConvertEdge(x1, y1, x2, y2: integer; var edge: TEdge);

// Fill a polygon defined by vertices
// Vertices must be in counter-clockwise or clockwise order
procedure FillPolygon(const vertices: array of TPoint; vertexCount: integer; color: TColor; PlotPixel: TPlotPixelProc);

// Draw polygon outline (wireframe)
procedure DrawPolygonOutline(const vertices: array of TPoint; vertexCount: integer; color: TColor; PlotPixel: TPlotPixelProc);

implementation

uses
  Graphics_Line;  // For DrawLineBresenham

// Scan convert a single edge
procedure ScanConvertEdge(x1, y1, x2, y2: integer; var edge: TEdge);
var
  deltaX, deltaY: integer;
begin
  // Ensure YStart < YEnd
  if y1 < y2 then
  begin
    edge.YStart := y1;
    edge.YEnd := y2;
    deltaX := x2 - x1;
    deltaY := y2 - y1;
  end
  else
  begin
    edge.YStart := y2;
    edge.YEnd := y1;
    deltaX := x1 - x2;
    deltaY := y1 - y2;
  end;
  
  // Calculate X step per scanline: dx/dy
  if deltaY > 0 then
    edge.XStep := Fixed16Div(IntToFixed16(deltaX), IntToFixed16(deltaY))
  else
    edge.XStep := 0;
  
  // Initial X position
  if y1 < y2 then
    edge.X := IntToFixed16(x1)
  else
    edge.X := IntToFixed16(x2);
end;

// Fill polygon using scan conversion
procedure FillPolygon(const vertices: array of TPoint; vertexCount: integer; color: TColor; PlotPixel: TPlotPixelProc);
var
  edges: array[0..255] of TEdge;  // Max 256 edges
  edgeCount: integer;
  y, i: integer;
  leftX, rightX: Fixed16;
  xStart, xEnd: integer;
  yMin, yMax: integer;
  
  // Helper: Draw horizontal line
  procedure DrawHorizontalLine(xStart, xEnd, y: integer; color: TColor);
  var
    x: integer;
  begin
    // Ensure xStart <= xEnd
    if xStart > xEnd then
    begin
      x := xStart;
      xStart := xEnd;
      xEnd := x;
    end;
    
    for x := xStart to xEnd do
      PlotPixel(x, y, color);
  end;
  
begin
  if vertexCount < 3 then
    Exit;  // Need at least 3 vertices for a polygon
  
  // Build edge table
  edgeCount := vertexCount;
  for i := 0 to edgeCount - 1 do
  begin
    ScanConvertEdge(
      vertices[i].X, vertices[i].Y,
      vertices[(i + 1) mod vertexCount].X,
      vertices[(i + 1) mod vertexCount].Y,
      edges[i]
    );
  end;
  
  // Find Y range
  yMin := vertices[0].Y;
  yMax := vertices[0].Y;
  for i := 1 to vertexCount - 1 do
  begin
    if vertices[i].Y < yMin then
      yMin := vertices[i].Y;
    if vertices[i].Y > yMax then
      yMax := vertices[i].Y;
  end;
  
  // Fill scanlines
  for y := yMin to yMax do
  begin
    // Find leftmost and rightmost X for this scanline
    leftX := IntToFixed16($7FFFFFFF);  // Large initial value
    rightX := IntToFixed16(-$7FFFFFFF);  // Small initial value
    
    for i := 0 to edgeCount - 1 do
    begin
      // Check if this edge intersects current scanline
      if (y >= edges[i].YStart) and (y < edges[i].YEnd) then
      begin
        // Update left/right bounds
        if edges[i].X < leftX then
          leftX := edges[i].X;
        if edges[i].X > rightX then
          rightX := edges[i].X;
        
        // Advance edge X position
        edges[i].X := edges[i].X + edges[i].XStep;
      end;
    end;
    
    // Draw horizontal line if we found valid bounds
    if leftX < rightX then
    begin
      xStart := Fixed16ToInt(leftX);
      xEnd := Fixed16ToInt(rightX);
      DrawHorizontalLine(xStart, xEnd, y, color);
    end;
  end;
end;

// Draw polygon outline (wireframe)
procedure DrawPolygonOutline(const vertices: array of TPoint; vertexCount: integer; color: TColor; PlotPixel: TPlotPixelProc);
var
  i: integer;
begin
  if vertexCount < 2 then
    Exit;
  
  // Draw lines between consecutive vertices
  for i := 0 to vertexCount - 1 do
  begin
    DrawLineBresenham(
      vertices[i].X, vertices[i].Y,
      vertices[(i + 1) mod vertexCount].X,
      vertices[(i + 1) mod vertexCount].Y,
      color,
      PlotPixel
    );
  end;
end;

end.

