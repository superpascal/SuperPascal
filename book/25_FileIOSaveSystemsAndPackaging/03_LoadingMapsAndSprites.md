# Loading Maps and Sprites

**Part of:** [Chapter 25: File I/O, Save Systems, and Packaging](./README.md)

---

## Introduction

Loading maps and sprites from files allows you to create larger games with more content. This section teaches you how to load tilemaps and sprite data from files and asset bundles.

**Key concepts:**
- **Loading tilemaps** — Reading map data from files
- **Loading sprites** — Reading sprite sheets from files
- **Resource management** — Managing loaded resources
- **Streaming** — Loading resources on demand

---

## Loading Tilemaps

### Tilemap File Format

**Simple tilemap format:**

```pascal
type
  TTilemapHeader = record
    Magic: array[0..3] of char;  // "TMAP"
    Version: byte;
    Width: word;
    Height: word;
    TileCount: word;
  end;
  
  TTilemapFile = record
    Header: TTilemapHeader;
    Tiles: array[0..1023] of word;  // Tile indices
  end;
```

### Loading Tilemap

**Load tilemap from file:**

```pascal
function LoadTilemap(filename: string): TTilemap;
var
  fileHandle: TFileHandle;
  header: TTilemapHeader;
  tiles: ^word;
  tileCount: word;
  i: word;
begin
  fileHandle := FileOpen(filename, fmRead);
  if fileHandle = nil then
  begin
    LoadTilemap.Data := nil;
    Exit;
  end;
  
  // Read header
  FileRead(fileHandle, header, SizeOf(TTilemapHeader));
  
  // Validate
  if (header.Magic[0] <> 'T') or (header.Magic[1] <> 'M') or
     (header.Magic[2] <> 'A') or (header.Magic[3] <> 'P') then
  begin
    FileClose(fileHandle);
    LoadTilemap.Data := nil;
    Exit;
  end;
  
  // Allocate tile data
  tileCount := header.Width * header.Height;
  GetMem(tiles, tileCount * SizeOf(word));
  
  // Read tiles
  FileRead(fileHandle, tiles^, tileCount * SizeOf(word));
  
  FileClose(fileHandle);
  
  // Create tilemap
  LoadTilemap.Width := header.Width;
  LoadTilemap.Height := header.Height;
  LoadTilemap.Data := tiles;
end;
```

### Loading from Bundle

**Load tilemap from asset bundle:**

```pascal
function LoadTilemapFromBundle(bundle: TZPKBundle; filename: string): TTilemap;
var
  data: pointer;
  size: dword;
  header: ^TTilemapHeader;
  tiles: ^word;
begin
  // Extract file from bundle
  if not ExtractFile(bundle, filename, data, size) then
  begin
    LoadTilemapFromBundle.Data := nil;
    Exit;
  end;
  
  // Parse tilemap data
  header := data;
  
  // Validate
  if (header^.Magic[0] <> 'T') or (header^.Magic[1] <> 'M') or
     (header^.Magic[2] <> 'A') or (header^.Magic[3] <> 'P') then
  begin
    FreeMem(data);
    LoadTilemapFromBundle.Data := nil;
    Exit;
  end;
  
  // Allocate tilemap
  GetMem(tiles, header^.Width * header^.Height * SizeOf(word));
  
  // Copy tiles
  Move(PByte(data) + SizeOf(TTilemapHeader), tiles^, 
       header^.Width * header^.Height * SizeOf(word));
  
  // Create tilemap
  LoadTilemapFromBundle.Width := header^.Width;
  LoadTilemapFromBundle.Height := header^.Height;
  LoadTilemapFromBundle.Data := tiles;
  
  // Free bundle data (tiles are copied)
  FreeMem(data);
end;
```

### Applying Tilemap

**Load tilemap to video memory:**

```pascal
procedure ApplyTilemap(tilemap: TTilemap; targetX, targetY: integer);
var
  x, y: integer;
  tileIndex: word;
  tiles: ^word;
begin
  tiles := tilemap.Data;
  
  for y := 0 to tilemap.Height - 1 do
  begin
    for x := 0 to tilemap.Width - 1 do
    begin
      tileIndex := tiles[y * tilemap.Width + x];
      
      // Set tile at position
      SetTile(targetX + x, targetY + y, tileIndex);
    end;
  end;
end;
```

---

## Loading Sprites

### Sprite Sheet Format

**Sprite sheet file format:**

```pascal
type
  TSpriteSheetHeader = record
    Magic: array[0..3] of char;  // "SPRT"
    Version: byte;
    SpriteCount: word;
    TileWidth: byte;
    TileHeight: byte;
    PaletteCount: byte;
  end;
  
  TSpriteSheetFile = record
    Header: TSpriteSheetHeader;
    PaletteData: array[0..255] of word;  // RGB565 colors
    TileData: array[0..16383] of byte;    // Tile pixel data
  end;
```

### Loading Sprite Sheet

**Load sprite sheet from file:**

```pascal
function LoadSpriteSheet(filename: string): TSpriteSheet;
var
  fileHandle: TFileHandle;
  header: TSpriteSheetHeader;
  paletteData: ^word;
  tileData: ^byte;
  paletteSize: word;
  tileSize: dword;
begin
  fileHandle := FileOpen(filename, fmRead);
  if fileHandle = nil then
  begin
    LoadSpriteSheet.Data := nil;
    Exit;
  end;
  
  // Read header
  FileRead(fileHandle, header, SizeOf(TSpriteSheetHeader));
  
  // Validate
  if (header.Magic[0] <> 'S') or (header.Magic[1] <> 'P') or
     (header.Magic[2] <> 'R') or (header.Magic[3] <> 'T') then
  begin
    FileClose(fileHandle);
    LoadSpriteSheet.Data := nil;
    Exit;
  end;
  
  // Allocate palette
  paletteSize := header.PaletteCount * SizeOf(word);
  GetMem(paletteData, paletteSize);
  FileRead(fileHandle, paletteData^, paletteSize);
  
  // Allocate tile data
  tileSize := header.SpriteCount * header.TileWidth * header.TileHeight;
  GetMem(tileData, tileSize);
  FileRead(fileHandle, tileData^, tileSize);
  
  FileClose(fileHandle);
  
  // Create sprite sheet
  LoadSpriteSheet.SpriteCount := header.SpriteCount;
  LoadSpriteSheet.TileWidth := header.TileWidth;
  LoadSpriteSheet.TileHeight := header.TileHeight;
  LoadSpriteSheet.PaletteData := paletteData;
  LoadSpriteSheet.TileData := tileData;
end;
```

### Loading from Bundle

**Load sprite sheet from bundle:**

```pascal
function LoadSpriteSheetFromBundle(bundle: TZPKBundle; filename: string): TSpriteSheet;
var
  data: pointer;
  size: dword;
  header: ^TSpriteSheetHeader;
  paletteData: ^word;
  tileData: ^byte;
begin
  // Extract file from bundle
  if not ExtractFile(bundle, filename, data, size) then
  begin
    LoadSpriteSheetFromBundle.Data := nil;
    Exit;
  end;
  
  // Parse sprite sheet
  header := data;
  
  // Validate
  if (header^.Magic[0] <> 'S') or (header^.Magic[1] <> 'P') or
     (header^.Magic[2] <> 'R') or (header^.Magic[3] <> 'T') then
  begin
    FreeMem(data);
    LoadSpriteSheetFromBundle.Data := nil;
    Exit;
  end;
  
  // Allocate palette
  GetMem(paletteData, header^.PaletteCount * SizeOf(word));
  Move(PByte(data) + SizeOf(TSpriteSheetHeader), paletteData^, 
       header^.PaletteCount * SizeOf(word));
  
  // Allocate tile data
  var tileSize := header^.SpriteCount * header^.TileWidth * header^.TileHeight;
  GetMem(tileData, tileSize);
  Move(PByte(data) + SizeOf(TSpriteSheetHeader) + 
       header^.PaletteCount * SizeOf(word), tileData^, tileSize);
  
  // Create sprite sheet
  LoadSpriteSheetFromBundle.SpriteCount := header^.SpriteCount;
  LoadSpriteSheetFromBundle.TileWidth := header^.TileWidth;
  LoadSpriteSheetFromBundle.TileHeight := header^.TileHeight;
  LoadSpriteSheetFromBundle.PaletteData := paletteData;
  LoadSpriteSheetFromBundle.TileData := tileData;
  
  // Free bundle data
  FreeMem(data);
end;
```

### Applying Sprite Sheet

**Load sprite sheet to video memory:**

```pascal
procedure ApplySpriteSheet(sheet: TSpriteSheet; baseTile: word);
var
  i: word;
  tileData: ^byte;
  tileSize: word;
begin
  tileSize := sheet.TileWidth * sheet.TileHeight;
  tileData := sheet.TileData;
  
  // Load palette
  LoadPalette(sheet.PaletteData, sheet.PaletteCount);
  
  // Load tiles
  for i := 0 to sheet.SpriteCount - 1 do
  begin
    // Copy tile data to video memory
    CopyTileToVRAM(baseTile + i, tileData + i * tileSize, 
                   sheet.TileWidth, sheet.TileHeight);
  end;
end;
```

---

## Resource Management

### Resource Manager

**Manage loaded resources:**

```pascal
type
  TResourceType = (rtTilemap, rtSpriteSheet, rtAudio);
  
  TResource = record
    ResourceType: TResourceType;
    Name: string;
    Data: pointer;
    Size: dword;
    Loaded: boolean;
  end;
  
  TResourceManager = class
  private
    Resources: array[0..63] of TResource;
    ResourceCount: word;
  public
    constructor Create;
    function LoadTilemap(filename: string): word;
    function LoadSpriteSheet(filename: string): word;
    function GetTilemap(resourceID: word): TTilemap;
    function GetSpriteSheet(resourceID: word): TSpriteSheet;
    procedure UnloadResource(resourceID: word);
  end;

implementation

constructor TResourceManager.Create;
begin
  ResourceCount := 0;
end;

function TResourceManager.LoadTilemap(filename: string): word;
var
  tilemap: TTilemap;
begin
  if ResourceCount < 64 then
  begin
    tilemap := LoadTilemap(filename);
    if tilemap.Data <> nil then
    begin
      Resources[ResourceCount].ResourceType := rtTilemap;
      Resources[ResourceCount].Name := filename;
      Resources[ResourceCount].Data := tilemap.Data;
      Resources[ResourceCount].Size := tilemap.Width * tilemap.Height * SizeOf(word);
      Resources[ResourceCount].Loaded := true;
      LoadTilemap := ResourceCount;
      ResourceCount := ResourceCount + 1;
      Exit;
    end;
  end;
  
  LoadTilemap := $FFFF;  // Failed
end;

function TResourceManager.LoadSpriteSheet(filename: string): word;
var
  sheet: TSpriteSheet;
begin
  if ResourceCount < 64 then
  begin
    sheet := LoadSpriteSheet(filename);
    if sheet.Data <> nil then
    begin
      Resources[ResourceCount].ResourceType := rtSpriteSheet;
      Resources[ResourceCount].Name := filename;
      Resources[ResourceCount].Data := sheet.Data;
      Resources[ResourceCount].Loaded := true;
      LoadSpriteSheet := ResourceCount;
      ResourceCount := ResourceCount + 1;
      Exit;
    end;
  end;
  
  LoadSpriteSheet := $FFFF;  // Failed
end;

procedure TResourceManager.UnloadResource(resourceID: word);
begin
  if resourceID < ResourceCount then
  begin
    if Resources[resourceID].Loaded then
    begin
      FreeMem(Resources[resourceID].Data);
      Resources[resourceID].Loaded := false;
    end;
  end;
end;
```

### Streaming Resources

**Load resources on demand:**

```pascal
procedure LoadLevelResources(levelID: byte);
var
  bundle: TZPKBundle;
  tilemap: TTilemap;
  sprites: TSpriteSheet;
begin
  // Load level bundle
  bundle := LoadBundle('level' + IntToStr(levelID) + '.zpk');
  
  // Load tilemap
  tilemap := LoadTilemapFromBundle(bundle, 'level' + IntToStr(levelID) + '.tmx');
  ApplyTilemap(tilemap, 0, 0);
  
  // Load sprites
  sprites := LoadSpriteSheetFromBundle(bundle, 'sprites.bin');
  ApplySpriteSheet(sprites, 0);
  
  // Unload previous level resources
  UnloadLevelResources;
end;
```

---

## Practical Examples

### Loading Level

**Complete level loading:**

```pascal
procedure LoadLevel(levelID: byte);
var
  bundleManager: TBundleManager;
  tilemap: TTilemap;
  sprites: TSpriteSheet;
begin
  bundleManager := TBundleManager.Create;
  
  // Load level bundle
  bundleManager.LoadBundle('level' + IntToStr(levelID) + '.zpk');
  
  // Load tilemap
  var tilemapData: pointer;
  var tilemapSize: dword;
  if bundleManager.ExtractFile(0, 'level.tmx', tilemapData, tilemapSize) then
  begin
    tilemap := ParseTilemap(tilemapData, tilemapSize);
    ApplyTilemap(tilemap, 0, 0);
    FreeMem(tilemapData);
  end;
  
  // Load sprites
  var spriteData: pointer;
  var spriteSize: dword;
  if bundleManager.ExtractFile(0, 'sprites.bin', spriteData, spriteSize) then
  begin
    sprites := ParseSpriteSheet(spriteData, spriteSize);
    ApplySpriteSheet(sprites, 0);
    FreeMem(spriteData);
  end;
end;
```

### Dynamic Resource Loading

**Load resources as needed:**

```pascal
type
  TLevelLoader = class
  private
    CurrentLevel: byte;
    LoadedResources: array[0..15] of word;
    ResourceCount: byte;
  public
    procedure LoadLevel(levelID: byte);
    procedure UnloadLevel;
  end;

implementation

procedure TLevelLoader.LoadLevel(levelID: byte);
begin
  // Unload previous level
  UnloadLevel;
  
  // Load new level resources
  CurrentLevel := levelID;
  ResourceCount := 0;
  
  // Load tilemap
  LoadedResources[ResourceCount] := ResourceManager.LoadTilemap(
    'level' + IntToStr(levelID) + '.tmx');
  ResourceCount := ResourceCount + 1;
  
  // Load sprites
  LoadedResources[ResourceCount] := ResourceManager.LoadSpriteSheet(
    'level' + IntToStr(levelID) + '_sprites.bin');
  ResourceCount := ResourceCount + 1;
end;

procedure TLevelLoader.UnloadLevel;
var
  i: byte;
begin
  // Unload all resources
  for i := 0 to ResourceCount - 1 do
    ResourceManager.UnloadResource(LoadedResources[i]);
  
  ResourceCount := 0;
end;
```

---

## Complete Example

**Putting it all together:**

```pascal
program ResourceLoadingDemo;

type
  TResourceManager = class
  public
    function LoadTilemap(filename: string): TTilemap;
    function LoadSpriteSheet(filename: string): TSpriteSheet;
  end;

var
  ResourceManager: TResourceManager;
  LevelTilemap: TTilemap;
  GameSprites: TSpriteSheet;

function TResourceManager.LoadTilemap(filename: string): TTilemap;
var
  fileHandle: TFileHandle;
  header: TTilemapHeader;
  tiles: ^word;
begin
  fileHandle := FileOpen(filename, fmRead);
  if fileHandle = nil then
  begin
    LoadTilemap.Data := nil;
    Exit;
  end;
  
  FileRead(fileHandle, header, SizeOf(TTilemapHeader));
  
  if (header.Magic[0] = 'T') and (header.Magic[1] = 'M') and
     (header.Magic[2] = 'A') and (header.Magic[3] = 'P') then
  begin
    GetMem(tiles, header.Width * header.Height * SizeOf(word));
    FileRead(fileHandle, tiles^, header.Width * header.Height * SizeOf(word));
    
    LoadTilemap.Width := header.Width;
    LoadTilemap.Height := header.Height;
    LoadTilemap.Data := tiles;
  end;
  
  FileClose(fileHandle);
end;

function TResourceManager.LoadSpriteSheet(filename: string): TSpriteSheet;
var
  fileHandle: TFileHandle;
  header: TSpriteSheetHeader;
  paletteData: ^word;
  tileData: ^byte;
begin
  fileHandle := FileOpen(filename, fmRead);
  if fileHandle = nil then
  begin
    LoadSpriteSheet.Data := nil;
    Exit;
  end;
  
  FileRead(fileHandle, header, SizeOf(TSpriteSheetHeader));
  
  if (header.Magic[0] = 'S') and (header.Magic[1] = 'P') and
     (header.Magic[2] = 'R') and (header.Magic[3] = 'T') then
  begin
    GetMem(paletteData, header.PaletteCount * SizeOf(word));
    FileRead(fileHandle, paletteData^, header.PaletteCount * SizeOf(word));
    
    var tileSize := header.SpriteCount * header.TileWidth * header.TileHeight;
    GetMem(tileData, tileSize);
    FileRead(fileHandle, tileData^, tileSize);
    
    LoadSpriteSheet.SpriteCount := header.SpriteCount;
    LoadSpriteSheet.PaletteData := paletteData;
    LoadSpriteSheet.TileData := tileData;
  end;
  
  FileClose(fileHandle);
end;

begin
  InitGraphics;
  ResourceManager := TResourceManager.Create;
  
  // Load resources
  LevelTilemap := ResourceManager.LoadTilemap('level1.tmx');
  GameSprites := ResourceManager.LoadSpriteSheet('sprites.bin');
  
  // Apply to video memory
  if LevelTilemap.Data <> nil then
    ApplyTilemap(LevelTilemap, 0, 0);
  
  if GameSprites.Data <> nil then
    ApplySpriteSheet(GameSprites, 0);
  
  // Game loop
  while true do
  begin
    UpdateGame;
    RenderGame;
    WaitVBlank;
  end;
end.
```

---

## Best Practices

### 1. Validate File Formats

**Check magic numbers:**

```pascal
// ✅ GOOD: Validate format
if (header.Magic[0] = 'T') and (header.Magic[1] = 'M') then
  LoadTilemap(data)
else
  WriteLn('Invalid tilemap format');

// ❌ BAD: No validation
LoadTilemap(data);  // May crash on invalid data
```

### 2. Free Loaded Resources

**Don't leak memory:**

```pascal
// ✅ GOOD: Free after use
var tilemap := LoadTilemap('level.tmx');
ApplyTilemap(tilemap);
FreeMem(tilemap.Data);

// ❌ BAD: Memory leak
var tilemap := LoadTilemap('level.tmx');
// Never freed!
```

### 3. Handle Loading Errors

**Check if load succeeded:**

```pascal
// ✅ GOOD: Check result
var tilemap := LoadTilemap('level.tmx');
if tilemap.Data <> nil then
  ApplyTilemap(tilemap)
else
  WriteLn('Failed to load tilemap');

// ❌ BAD: No error handling
var tilemap := LoadTilemap('level.tmx');
ApplyTilemap(tilemap);  // May crash if load failed
```

### 4. Use Resource Manager

**Centralize resource management:**

```pascal
// ✅ GOOD: Use manager
ResourceManager.LoadTilemap('level.tmx');
ResourceManager.UnloadResource(id);

// ❌ BAD: Manual management
var tilemap := LoadTilemap('level.tmx');
// Hard to track all resources
```

### 5. Load on Demand

**Don't load everything at once:**

```pascal
// ✅ GOOD: Load when needed
procedure EnterLevel(levelID: byte);
begin
  LoadLevelResources(levelID);
end;

// ❌ BAD: Load all levels
LoadAllLevels;  // Wastes memory
```

---

## Exercises

### Exercise 1: Load Tilemap

Write a program that:
1. Loads tilemap from file
2. Validates tilemap format
3. Applies tilemap to video memory
4. Displays loaded tilemap

### Exercise 2: Load Sprite Sheet

Write a program that:
1. Loads sprite sheet from file
2. Validates sprite format
3. Applies sprites to video memory
4. Uses loaded sprites

### Exercise 3: Resource Manager

Write a program that:
1. Implements resource manager
2. Loads multiple resources
3. Tracks loaded resources
4. Unloads resources properly

### Exercise 4: Complete System

Write a program that:
1. Loads resources from bundle
2. Manages resource lifecycle
3. Loads levels dynamically
4. Integrates with game

---

**Previous Section:** [Asset Bundles](./02_AssetBundles.md)  
**Next Chapter:** [Chapter 26: Hardware Interfacing](../26_HardwareInterfacing/README.md)  
**Language Specification:** See [Memory and I/O](../../languageSpecification/12_MemoryAndIO.md)  
**Last Updated:** 2025-01-XX

