unit Graphics_Circle;

interface

// Circle drawing algorithms
// Source: algorithms/04_GraphicsAlgorithms.md

uses
  Graphics_Types;

// Draw circle outline using midpoint algorithm
procedure DrawCircle(centerX, centerY, radius: integer; color: TColor; PlotPixel: TPlotPixelProc);

// Fill circle (filled circle)
procedure FillCircle(centerX, centerY, radius: integer; color: TColor; PlotPixel: TPlotPixelProc);

implementation

// Midpoint circle algorithm (Bresenham's circle)
procedure DrawCircle(centerX, centerY, radius: integer; color: TColor; PlotPixel: TPlotPixelProc);
var
  x, y: integer;
  d: integer;
begin
  if radius <= 0 then
    Exit;
  
  x := 0;
  y := radius;
  d := 3 - 2 * radius;
  
  // Draw initial points (8-way symmetry)
  procedure Plot8Points(x, y: integer);
  begin
    PlotPixel(centerX + x, centerY + y, color);
    PlotPixel(centerX - x, centerY + y, color);
    PlotPixel(centerX + x, centerY - y, color);
    PlotPixel(centerX - x, centerY - y, color);
    PlotPixel(centerX + y, centerY + x, color);
    PlotPixel(centerX - y, centerY + x, color);
    PlotPixel(centerX + y, centerY - x, color);
    PlotPixel(centerX - y, centerY - x, color);
  end;
  
  // Draw initial point
  Plot8Points(x, y);
  
  // Draw remaining points
  while x < y do
  begin
    if d < 0 then
    begin
      d := d + 4 * x + 6;
      x := x + 1;
    end
    else
    begin
      d := d + 4 * (x - y) + 10;
      x := x + 1;
      y := y - 1;
    end;
    
    Plot8Points(x, y);
  end;
end;

// Fill circle by drawing horizontal lines
procedure FillCircle(centerX, centerY, radius: integer; color: TColor; PlotPixel: TPlotPixelProc);
var
  x, y: integer;
  d: integer;
  xStart, xEnd: integer;
  
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
  if radius <= 0 then
    Exit;
  
  x := 0;
  y := radius;
  d := 3 - 2 * radius;
  
  // Draw initial horizontal line
  DrawHorizontalLine(centerX - x, centerX + x, centerY + y);
  DrawHorizontalLine(centerX - x, centerX + x, centerY - y);
  DrawHorizontalLine(centerX - y, centerX + y, centerY + x);
  DrawHorizontalLine(centerX - y, centerX + y, centerY - x);
  
  // Fill remaining scanlines
  while x < y do
  begin
    if d < 0 then
    begin
      d := d + 4 * x + 6;
      x := x + 1;
    end
    else
    begin
      d := d + 4 * (x - y) + 10;
      x := x + 1;
      y := y - 1;
    end;
    
    // Draw horizontal lines for this scanline
    DrawHorizontalLine(centerX - x, centerX + x, centerY + y);
    DrawHorizontalLine(centerX - x, centerX + x, centerY - y);
    DrawHorizontalLine(centerX - y, centerX + y, centerY + x);
    DrawHorizontalLine(centerX - y, centerX + y, centerY - x);
  end;
end;

end.

