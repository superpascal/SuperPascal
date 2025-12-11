unit Graphics_Line;

interface

// Line drawing algorithms
// Source: algorithms/04_GraphicsAlgorithms.md
// Based on: Mikro archive BRESENHA.TXT

uses
  Graphics_Types;

// Bresenham's line algorithm
// PlotPixelProc is a platform-specific function to plot a pixel
procedure DrawLineBresenham(x1, y1, x2, y2: integer; color: TColor; PlotPixel: TPlotPixelProc);

implementation

// Bresenham's line algorithm
procedure DrawLineBresenham(x1, y1, x2, y2: integer; color: TColor; PlotPixel: TPlotPixelProc);
var
  deltaX, deltaY: integer;
  xChange, yChange: integer;
  error: integer;
  x, y, i, length: integer;
begin
  x := x1;
  y := y1;
  
  deltaX := x2 - x1;
  deltaY := y2 - y1;
  
  // Determine direction
  if deltaX < 0 then
  begin
    xChange := -1;
    deltaX := -deltaX;
  end
  else
    xChange := 1;
  
  if deltaY < 0 then
  begin
    yChange := -1;
    deltaY := -deltaY;
  end
  else
    yChange := 1;
  
  error := 0;
  i := 0;
  
  // Draw line
  if deltaX < deltaY then
  begin
    // Steep line (slope > 1)
    length := deltaY + 1;
    while i < length do
    begin
      PlotPixel(x, y, color);
      y := y + yChange;
      error := error + deltaX;
      
      if error > deltaY then
      begin
        x := x + xChange;
        error := error - deltaY;
      end;
      
      i := i + 1;
    end;
  end
  else
  begin
    // Shallow line (slope <= 1)
    length := deltaX + 1;
    while i < length do
    begin
      PlotPixel(x, y, color);
      x := x + xChange;
      error := error + deltaY;
      
      if error > deltaX then
      begin
        y := y + yChange;
        error := error - deltaX;
      end;
      
      i := i + 1;
    end;
  end;
end;

end.

