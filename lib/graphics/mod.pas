unit Graphics;

interface

// Graphics library - Main entry point
// Re-exports all graphics functionality

uses
  Graphics_Types,
  Graphics_Line,
  Graphics_Polygon,
  Graphics_Circle,
  Graphics_Ellipse,
  Graphics_Texture,
  Graphics_Tilemap,
  Math_Types;  // For Fixed16

// Re-export types
type
  TPlotPixelProc = Graphics_Types.TPlotPixelProc;
  TPoint = Graphics_Types.TPoint;
  TRect = Graphics_Types.TRect;
  TColor = Graphics_Types.TColor;
  TPolygon = Graphics_Types.TPolygon;
  TPolygonCount = Graphics_Types.TPolygonCount;
  TTexture = Graphics_Types.TTexture;
  TTile = Graphics_Types.TTile;
  TTilemap = Graphics_Types.TTilemap;
  TEdge = Graphics_Polygon.TEdge;
  Fixed16 = Math_Types.Fixed16;  // Re-export for convenience

// Re-export constants
const
  MAX_POLYGON_VERTICES = Graphics_Types.MAX_POLYGON_VERTICES;
  MAX_TEXTURE_SIZE = Graphics_Types.MAX_TEXTURE_SIZE;

// Re-export line functions
procedure DrawLineBresenham(x1, y1, x2, y2: integer; color: TColor; PlotPixel: TPlotPixelProc);

// Re-export polygon functions
procedure ScanConvertEdge(x1, y1, x2, y2: integer; var edge: TEdge);
procedure FillPolygon(const vertices: array of TPoint; vertexCount: integer; color: TColor; PlotPixel: TPlotPixelProc);
procedure DrawPolygonOutline(const vertices: array of TPoint; vertexCount: integer; color: TColor; PlotPixel: TPlotPixelProc);

// Re-export circle functions
procedure DrawCircle(centerX, centerY, radius: integer; color: TColor; PlotPixel: TPlotPixelProc);
procedure FillCircle(centerX, centerY, radius: integer; color: TColor; PlotPixel: TPlotPixelProc);

// Re-export ellipse functions
procedure DrawEllipse(centerX, centerY, radiusX, radiusY: integer; color: TColor; PlotPixel: TPlotPixelProc);
procedure FillEllipse(centerX, centerY, radiusX, radiusY: integer; color: TColor; PlotPixel: TPlotPixelProc);

// Re-export texture functions
procedure DrawTexturedPolygon(
  const vertices: array of TPoint;
  const texCoords: array of TPoint;
  vertexCount: integer;
  const texture: TTexture;
  PlotPixel: TPlotPixelProc
);
procedure DrawTiledTexture(
  const vertices: array of TPoint;
  vertexCount: integer;
  const texture: TTexture;
  tileScaleX, tileScaleY: Fixed16;
  PlotPixel: TPlotPixelProc
);

// Re-export tilemap functions
function GetTile(const tilemap: TTilemap; tileX, tileY: integer): TTile;
procedure SetTile(var tilemap: TTilemap; tileX, tileY: integer; const tile: TTile);
procedure RenderTilemap(
  const tilemap: TTilemap;
  cameraX, cameraY: integer;
  screenWidth, screenHeight: integer;
  PlotPixel: TPlotPixelProc
);

implementation

end.

