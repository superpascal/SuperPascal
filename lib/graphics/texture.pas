unit Graphics_Texture;

interface

// Texture mapping algorithms
// Source: algorithms/04_GraphicsAlgorithms.md
// Based on: Mikro archive TEXTURE.TXT, TILETMAP.TXT

uses
  Graphics_Types,
  Math_Types,
  Math_Fixed;

// Edge type for texture mapping
type
  TTextureEdge = record
    X: Fixed16;
    XStep: Fixed16;
    U: Fixed16;  // Texture U coordinate
    UStep: Fixed16;  // U increment per scanline
    V: Fixed16;  // Texture V coordinate
    VStep: Fixed16;  // V increment per scanline
    YStart: integer;
    YEnd: integer;
  end;

// Basic texture mapping (affine mapping)
// Maps texture coordinates to screen coordinates
procedure DrawTexturedPolygon(
  const vertices: array of TPoint;
  const texCoords: array of TPoint;  // Texture coordinates (0..textureWidth-1, 0..textureHeight-1)
  vertexCount: integer;
  const texture: TTexture;
  PlotPixel: TPlotPixelProc
);

// Tiled texture mapping
// Repeats texture across polygon
procedure DrawTiledTexture(
  const vertices: array of TPoint;
  vertexCount: integer;
  const texture: TTexture;
  tileScaleX, tileScaleY: Fixed16;  // How many times to repeat texture
  PlotPixel: TPlotPixelProc
);

implementation

uses
  Graphics_Polygon;  // For FillPolygon

// Basic texture mapping (simplified affine)
procedure DrawTexturedPolygon(
  const vertices: array of TPoint;
  const texCoords: array of TPoint;
  vertexCount: integer;
  const texture: TTexture;
  PlotPixel: TPlotPixelProc
);
var
  edges: array[0..255] of TTextureEdge;
  edgeCount: integer;
  y, i: integer;
  leftX, rightX: Fixed16;
  leftU, rightU, leftV, rightV: Fixed16;
  uStep, vStep: Fixed16;
  xStart, xEnd: integer;
  x: integer;
  u, v: Fixed16;
  texX, texY: integer;
  color: TColor;
  nextIdx: integer;
  deltaX, deltaY: integer;
  deltaU, deltaV: integer;
  yMin, yMax: integer;
  deltaXScan: integer;
begin
  if vertexCount < 3 then
    Exit;
  
  if texture.Data = nil then
    Exit;
  
  // Build edge table with texture coordinates
  edgeCount := vertexCount;
  for i := 0 to edgeCount - 1 do
  begin
    nextIdx := (i + 1) mod vertexCount;
    
    if vertices[i].Y < vertices[nextIdx].Y then
    begin
      edges[i].YStart := vertices[i].Y;
      edges[i].YEnd := vertices[nextIdx].Y;
      deltaX := vertices[nextIdx].X - vertices[i].X;
      deltaY := vertices[nextIdx].Y - vertices[i].Y;
      deltaU := texCoords[nextIdx].X - texCoords[i].X;
      deltaV := texCoords[nextIdx].Y - texCoords[i].Y;
    end
    else
    begin
      edges[i].YStart := vertices[nextIdx].Y;
      edges[i].YEnd := vertices[i].Y;
      deltaX := vertices[i].X - vertices[nextIdx].X;
      deltaY := vertices[i].Y - vertices[nextIdx].Y;
      deltaU := texCoords[i].X - texCoords[nextIdx].X;
      deltaV := texCoords[i].Y - texCoords[nextIdx].Y;
    end;
    
    if deltaY > 0 then
    begin
      edges[i].XStep := Fixed16Div(IntToFixed16(deltaX), IntToFixed16(deltaY));
      edges[i].UStep := Fixed16Div(IntToFixed16(deltaU), IntToFixed16(deltaY));
      edges[i].VStep := Fixed16Div(IntToFixed16(deltaV), IntToFixed16(deltaY));
    end
    else
    begin
      edges[i].XStep := 0;
      edges[i].UStep := 0;
      edges[i].VStep := 0;
    end;
    
    if vertices[i].Y < vertices[nextIdx].Y then
    begin
      edges[i].X := IntToFixed16(vertices[i].X);
      edges[i].U := IntToFixed16(texCoords[i].X);
      edges[i].V := IntToFixed16(texCoords[i].Y);
    end
    else
    begin
      edges[i].X := IntToFixed16(vertices[nextIdx].X);
      edges[i].U := IntToFixed16(texCoords[nextIdx].X);
      edges[i].V := IntToFixed16(texCoords[nextIdx].Y);
    end;
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
  
  // Fill scanlines with texture
  for y := yMin to yMax do
  begin
    leftX := IntToFixed16($7FFFFFFF);
    rightX := IntToFixed16(-$7FFFFFFF);
    leftU := 0;
    rightU := 0;
    leftV := 0;
    rightV := 0;
    
    for i := 0 to edgeCount - 1 do
    begin
      if (y >= edges[i].YStart) and (y < edges[i].YEnd) then
      begin
        if edges[i].X < leftX then
        begin
          leftX := edges[i].X;
          leftU := edges[i].U;
          leftV := edges[i].V;
        end;
        if edges[i].X > rightX then
        begin
          rightX := edges[i].X;
          rightU := edges[i].U;
          rightV := edges[i].V;
        end;
        
        edges[i].X := edges[i].X + edges[i].XStep;
        edges[i].U := edges[i].U + edges[i].UStep;
        edges[i].V := edges[i].V + edges[i].VStep;
      end;
    end;
    
    if leftX < rightX then
    begin
      xStart := Fixed16ToInt(leftX);
      xEnd := Fixed16ToInt(rightX);
      
      deltaXScan := xEnd - xStart;
      if deltaXScan > 0 then
      begin
        uStep := Fixed16Div(rightU - leftU, IntToFixed16(deltaXScan));
        vStep := Fixed16Div(rightV - leftV, IntToFixed16(deltaXScan));
      end
      else
      begin
        uStep := 0;
        vStep := 0;
      end;
      
      u := leftU;
      v := leftV;
      
      for x := xStart to xEnd do
      begin
        // Sample texture
        texX := Fixed16ToInt(u) mod texture.Width;
        texY := Fixed16ToInt(v) mod texture.Height;
        
        // Get color from texture (platform-specific)
        // For now, assume texture.Data is array of Byte (palette indices)
        // This will need platform-specific implementation
        color := TColor(Byte(texture.Data^) + (texY * texture.Width + texX));
        
        PlotPixel(x, y, color);
        
        u := u + uStep;
        v := v + vStep;
      end;
    end;
  end;
end;

// Tiled texture mapping
procedure DrawTiledTexture(
  const vertices: array of TPoint;
  vertexCount: integer;
  const texture: TTexture;
  tileScaleX, tileScaleY: Fixed16;
  PlotPixel: TPlotPixelProc
);
var
  i: integer;
  texCoords: array[0..255] of TPoint;
begin
  if texture.Data = nil then
    Exit;
  
  // Generate texture coordinates based on tile scale
  for i := 0 to vertexCount - 1 do
  begin
    texCoords[i].X := Fixed16ToInt(Fixed16Mul(IntToFixed16(vertices[i].X), tileScaleX)) mod texture.Width;
    texCoords[i].Y := Fixed16ToInt(Fixed16Mul(IntToFixed16(vertices[i].Y), tileScaleY)) mod texture.Height;
  end;
  
  // Use basic texture mapping
  DrawTexturedPolygon(vertices, texCoords, vertexCount, texture, PlotPixel);
end;

end.

