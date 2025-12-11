unit Graphics_Tilemap;

interface

// Tilemap rendering algorithms
// Source: algorithms/04_GraphicsAlgorithms.md
// Based on: Mikro archive TILETMAP.TXT

uses
  Graphics_Types;

// Render tilemap to screen
// cameraX, cameraY: Camera position in tilemap coordinates
// screenWidth, screenHeight: Screen dimensions
procedure RenderTilemap(
  const tilemap: TTilemap;
  cameraX, cameraY: integer;
  screenWidth, screenHeight: integer;
  PlotPixel: TPlotPixelProc
);

// Get tile at tilemap coordinates
function GetTile(const tilemap: TTilemap; tileX, tileY: integer): TTile;

// Set tile at tilemap coordinates
procedure SetTile(var tilemap: TTilemap; tileX, tileY: integer; const tile: TTile);

implementation

// Get tile at tilemap coordinates
function GetTile(const tilemap: TTilemap; tileX, tileY: integer): TTile;
var
  index: integer;
  tiles: ^TTile;
begin
  // Bounds checking
  if (tileX < 0) or (tileX >= tilemap.Width) or
     (tileY < 0) or (tileY >= tilemap.Height) then
  begin
    // Return empty tile
    Result.TileID := 0;
    Result.Palette := 0;
    Result.FlipX := False;
    Result.FlipY := False;
    Exit;
  end;
  
  // Calculate index
  index := tileY * tilemap.Width + tileX;
  
  // Access tile array (tilemap.Tiles is Pointer to array of TTile)
  tiles := tilemap.Tiles;
  Result := tiles[index];
end;

// Set tile at tilemap coordinates
procedure SetTile(var tilemap: TTilemap; tileX, tileY: integer; const tile: TTile);
var
  index: integer;
  tiles: ^TTile;
begin
  // Bounds checking
  if (tileX < 0) or (tileX >= tilemap.Width) or
     (tileY < 0) or (tileY >= tilemap.Height) then
    Exit;
  
  // Calculate index
  index := tileY * tilemap.Width + tileX;
  
  // Access tile array
  tiles := tilemap.Tiles;
  tiles[index] := tile;
end;

// Render tilemap to screen
procedure RenderTilemap(
  const tilemap: TTilemap;
  cameraX, cameraY: integer;
  screenWidth, screenHeight: integer;
  PlotPixel: TPlotPixelProc
);
var
  startTileX, startTileY: integer;
  endTileX, endTileY: integer;
  tileX, tileY: integer;
  screenX, screenY: integer;
  tile: TTile;
  tilePixelX, tilePixelY: integer;
  pixelX, pixelY: integer;
  tileData: Pointer;  // Platform-specific tile data
  color: TColor;
begin
  if tilemap.Tiles = nil then
    Exit;
  
  // Calculate which tiles are visible
  startTileX := cameraX div tilemap.TileWidth;
  startTileY := cameraY div tilemap.TileHeight;
  endTileX := (cameraX + screenWidth) div tilemap.TileWidth + 1;
  endTileY := (cameraY + screenHeight) div tilemap.TileHeight + 1;
  
  // Clamp to tilemap bounds
  if startTileX < 0 then startTileX := 0;
  if startTileY < 0 then startTileY := 0;
  if endTileX > tilemap.Width then endTileX := tilemap.Width;
  if endTileY > tilemap.Height then endTileY := tilemap.Height;
  
  // Render visible tiles
  for tileY := startTileY to endTileY - 1 do
  begin
    for tileX := startTileX to endTileX - 1 do
    begin
      // Get tile
      tile := GetTile(tilemap, tileX, tileY);
      
      // Calculate screen position
      screenX := tileX * tilemap.TileWidth - cameraX;
      screenY := tileY * tilemap.TileHeight - cameraY;
      
      // Render tile pixels
      for tilePixelY := 0 to tilemap.TileHeight - 1 do
      begin
        for tilePixelX := 0 to tilemap.TileWidth - 1 do
        begin
          // Calculate actual pixel coordinates (handle flipping)
          if tile.FlipX then
            pixelX := screenX + (tilemap.TileWidth - 1 - tilePixelX)
          else
            pixelX := screenX + tilePixelX;
          
          if tile.FlipY then
            pixelY := screenY + (tilemap.TileHeight - 1 - tilePixelY)
          else
            pixelY := screenY + tilePixelY;
          
          // Check if pixel is on screen
          if (pixelX >= 0) and (pixelX < screenWidth) and
             (pixelY >= 0) and (pixelY < screenHeight) then
          begin
            // Get pixel color from tile (platform-specific)
            // This is a placeholder - actual implementation depends on tile format
            // For now, use tile ID as color (will need proper tile data access)
            color := TColor(tile.TileID mod 256);
            
            PlotPixel(pixelX, pixelY, color);
          end;
        end;
      end;
    end;
  end;
end;

end.

