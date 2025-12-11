unit Graphics_Types;

interface

// Core graphics types and constants
// Source: algorithms/04_GraphicsAlgorithms.md

type
  // Pixel plotting function type (platform-specific)
  TPlotPixelProc = procedure(x, y: integer; color: byte);
  
  // Point type
  TPoint = record
    X, Y: integer;
  end;
  
  // Rectangle type
  TRect = record
    Left, Top, Right, Bottom: integer;
  end;
  
  // Color type (platform-specific, typically 8-bit palette index)
  TColor = Byte;
  
  // Polygon type
  TPolygon = array[0..255] of TPoint;  // Max 256 vertices
  TPolygonCount = Byte;  // Number of vertices
  
  // Texture type
  TTexture = record
    Data: Pointer;  // Platform-specific texture data
    Width: Word;
    Height: Word;
    Format: Byte;  // Texture format (platform-specific)
  end;
  
  // Tilemap type
  TTile = record
    TileID: Word;   // Tile index
    Palette: Byte;  // Palette index
    FlipX: Boolean;
    FlipY: Boolean;
  end;
  
  TTilemap = record
    Tiles: Pointer;  // Array of TTile
    Width: Word;
    Height: Word;
    TileWidth: Byte;
    TileHeight: Byte;
  end;

// Graphics constants
const
  MAX_POLYGON_VERTICES = 256;
  MAX_TEXTURE_SIZE = 256;  // Max 256x256 texture

implementation

end.

