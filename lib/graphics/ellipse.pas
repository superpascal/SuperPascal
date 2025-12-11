unit Graphics_Ellipse;

interface

// Ellipse drawing algorithms
// Source: Midpoint ellipse algorithm (integer-only, no floating-point)

uses
  Graphics_Types;

// Draw ellipse outline using midpoint algorithm
procedure DrawEllipse(centerX, centerY, radiusX, radiusY: integer; color: TColor; PlotPixel: TPlotPixelProc);

// Fill ellipse (filled ellipse)
procedure FillEllipse(centerX, centerY, radiusX, radiusY: integer; color: TColor; PlotPixel: TPlotPixelProc);

implementation

// Midpoint ellipse algorithm
// Similar to circle algorithm but handles different x and y radii
// Uses integer-only arithmetic (no floating-point)
procedure DrawEllipse(centerX, centerY, radiusX, radiusY: integer; color: TColor; PlotPixel: TPlotPixelProc);
var
  x, y: integer;
  a, b: integer;  // Semi-major and semi-minor axes
  a2, b2: LongInt;  // a^2 and b^2
  d1, d2: LongInt;  // Decision variables
  dx, dy: LongInt;
  
  // Plot 4 symmetric points (ellipse has 4-way symmetry)
  procedure Plot4Points(x, y: integer);
  begin
    PlotPixel(centerX + x, centerY + y, color);
    PlotPixel(centerX - x, centerY + y, color);
    PlotPixel(centerX + x, centerY - y, color);
    PlotPixel(centerX - x, centerY - y, color);
  end;
  
begin
  if (radiusX <= 0) or (radiusY <= 0) then
    Exit;
  
  a := radiusX;
  b := radiusY;
  a2 := LongInt(a) * LongInt(a);
  b2 := LongInt(b) * LongInt(b);
  
  // Region 1: From (a, 0) to where slope = -1
  x := 0;
  y := b;
  
  // Initial decision parameter for region 1
  // d1 = b^2 - a^2 * b + a^2 / 4
  d1 := b2 - (a2 * LongInt(b)) + (a2 div 4);
  dx := 2 * b2 * LongInt(x);
  dy := 2 * a2 * LongInt(y);
  
  // Draw initial point
  Plot4Points(x, y);
  
  // Region 1: While slope > -1 (dx < dy)
  while dx < dy do
  begin
    x := x + 1;
    dx := dx + 2 * b2;
    
    if d1 < 0 then
    begin
      d1 := d1 + dx + b2;
    end
    else
    begin
      y := y - 1;
      dy := dy - 2 * a2;
      d1 := d1 + dx - dy + b2;
    end;
    
    Plot4Points(x, y);
  end;
  
  // Region 2: From where slope = -1 to (0, b)
  // Decision parameter for region 2
  // d2 = b^2 * (x + 0.5)^2 + a^2 * (y - 1)^2 - a^2 * b^2
  // Using integer arithmetic: (x + 0.5)^2 ≈ (2x + 1)^2 / 4
  d2 := (b2 * (2 * LongInt(x) + 1) * (2 * LongInt(x) + 1)) div 4 +
        (a2 * (LongInt(y) - 1) * (LongInt(y) - 1)) -
        (a2 * b2);
  
  while y > 0 do
  begin
    y := y - 1;
    dy := dy - 2 * a2;
    
    if d2 > 0 then
    begin
      d2 := d2 - dy + a2;
    end
    else
    begin
      x := x + 1;
      dx := dx + 2 * b2;
      d2 := d2 + dx - dy + a2;
    end;
    
    Plot4Points(x, y);
  end;
end;

// Fill ellipse by drawing horizontal lines
procedure FillEllipse(centerX, centerY, radiusX, radiusY: integer; color: TColor; PlotPixel: TPlotPixelProc);
var
  x, y: integer;
  a, b: integer;  // Semi-major and semi-minor axes
  a2, b2: LongInt;  // a^2 and b^2
  d1, d2: LongInt;  // Decision variables
  dx, dy: LongInt;
  
  // Draw horizontal line from xStart to xEnd at y
  procedure DrawHorizontalLine(xStart, xEnd, y: integer);
  var
    x: integer;
  begin
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
  if (radiusX <= 0) or (radiusY <= 0) then
    Exit;
  
  a := radiusX;
  b := radiusY;
  a2 := LongInt(a) * LongInt(a);
  b2 := LongInt(b) * LongInt(b);
  
  // Region 1: From (a, 0) to where slope = -1
  x := 0;
  y := b;
  
  // Initial decision parameter for region 1
  d1 := b2 - (a2 * LongInt(b)) + (a2 div 4);
  dx := 2 * b2 * LongInt(x);
  dy := 2 * a2 * LongInt(y);
  
  // Draw initial horizontal lines
  DrawHorizontalLine(centerX - x, centerX + x, centerY + y);
  DrawHorizontalLine(centerX - x, centerX + x, centerY - y);
  
  // Region 1: While slope > -1 (dx < dy)
  while dx < dy do
  begin
    x := x + 1;
    dx := dx + 2 * b2;
    
    if d1 < 0 then
    begin
      d1 := d1 + dx + b2;
    end
    else
    begin
      y := y - 1;
      dy := dy - 2 * a2;
      d1 := d1 + dx - dy + b2;
    end;
    
    // Draw horizontal lines for this scanline
    DrawHorizontalLine(centerX - x, centerX + x, centerY + y);
    DrawHorizontalLine(centerX - x, centerX + x, centerY - y);
  end;
  
  // Region 2: From where slope = -1 to (0, b)
  // Decision parameter for region 2
  d2 := (b2 * (2 * LongInt(x) + 1) * (2 * LongInt(x) + 1)) div 4 +
        (a2 * (LongInt(y) - 1) * (LongInt(y) - 1)) -
        (a2 * b2);
  
  while y > 0 do
  begin
    y := y - 1;
    dy := dy - 2 * a2;
    
    if d2 > 0 then
    begin
      d2 := d2 - dy + a2;
    end
    else
    begin
      x := x + 1;
      dx := dx + 2 * b2;
      d2 := d2 + dx - dy + a2;
    end;
    
    // Draw horizontal lines for this scanline
    DrawHorizontalLine(centerX - x, centerX + x, centerY + y);
    DrawHorizontalLine(centerX - x, centerX + x, centerY - y);
  end;
end;

end.
