# Asset Bundles (.ZPK)

**Part of:** [Chapter 25: File I/O, Save Systems, and Packaging](./README.md)

---

## Introduction

Asset bundles package game resources into a single file for easy distribution and loading. This section teaches you how to create and load .ZPK asset bundles.

**Key concepts:**
- **Asset bundles** — Packaged game resources
- **.ZPK format** — SuperPascal asset package format
- **Bundle structure** — How bundles are organized
- **Loading assets** — Extracting resources from bundles

---

## Understanding Asset Bundles

### What is an Asset Bundle?

**An asset bundle is a packaged collection of game resources:**

- **Sprites** — Sprite sheets and tiles
- **Tilemaps** — Level data
- **Audio** — Sound effects and music
- **Data** — Configuration files
- **Single file** — All resources in one package

**Benefits:**
- **Easy distribution** — One file instead of many
- **Faster loading** — Single file read
- **Organization** — All assets together
- **Compression** — Can compress assets

### .ZPK Format

**.ZPK (Zeal Package) format structure:**

```
Header (16 bytes):
  - Magic: "ZPK" (3 bytes)
  - Version: 1 byte
  - File count: 2 bytes
  - Reserved: 10 bytes

File entries (per file):
  - Name: 32 bytes (null-terminated)
  - Type: 1 byte (sprite, tilemap, audio, data)
  - Offset: 4 bytes (offset in bundle)
  - Size: 4 bytes (file size)
  - Compressed: 1 byte (0 = no, 1 = yes)
  - Reserved: 22 bytes

File data:
  - Files stored sequentially
  - Each file at specified offset
```

---

## Creating Asset Bundles

### Bundle Structure

**Define bundle format:**

```pascal
type
  TZPKHeader = record
    Magic: array[0..2] of char;  // "ZPK"
    Version: byte;
    FileCount: word;
    Reserved: array[0..9] of byte;
  end;
  
  TZPKFileEntry = record
    Name: array[0..31] of char;  // Null-terminated
    FileType: byte;
    Offset: dword;
    Size: dword;
    Compressed: byte;
    Reserved: array[0..21] of byte;
  end;
  
  TZPKBundle = record
    Header: TZPKHeader;
    Entries: array[0..255] of TZPKFileEntry;
    Data: pointer;  // File data
  end;
```

### Building a Bundle

**Create bundle from files:**

```pascal
type
  TAssetFile = record
    Filename: string;
    FileType: byte;
    Data: pointer;
    Size: dword;
  end;
  
  TAssetBundleBuilder = class
  private
    Files: array[0..255] of TAssetFile;
    FileCount: byte;
    CurrentOffset: dword;
  public
    constructor Create;
    procedure AddFile(filename: string; fileType: byte; data: pointer; size: dword);
    function BuildBundle: TZPKBundle;
  end;

implementation

constructor TAssetBundleBuilder.Create;
begin
  FileCount := 0;
  CurrentOffset := SizeOf(TZPKHeader) + SizeOf(TZPKFileEntry) * 256;  // After header and entries
end;

procedure TAssetBundleBuilder.AddFile(filename: string; fileType: byte; data: pointer; size: dword);
begin
  if FileCount < 256 then
  begin
    Files[FileCount].Filename := filename;
    Files[FileCount].FileType := fileType;
    Files[FileCount].Data := data;
    Files[FileCount].Size := size;
    FileCount := FileCount + 1;
  end;
end;

function TAssetBundleBuilder.BuildBundle: TZPKBundle;
var
  bundle: TZPKBundle;
  i: byte;
  offset: dword;
  totalSize: dword;
begin
  // Initialize header
  bundle.Header.Magic[0] := 'Z';
  bundle.Header.Magic[1] := 'P';
  bundle.Header.Magic[2] := 'K';
  bundle.Header.Version := 1;
  bundle.Header.FileCount := FileCount;
  
  // Calculate total size
  totalSize := SizeOf(TZPKHeader);
  for i := 0 to FileCount - 1 do
    totalSize := totalSize + SizeOf(TZPKFileEntry) + Files[i].Size;
  
  // Allocate bundle data
  GetMem(bundle.Data, totalSize);
  
  // Write header
  Move(bundle.Header, bundle.Data^, SizeOf(TZPKHeader));
  offset := SizeOf(TZPKHeader);
  
  // Write file entries and data
  for i := 0 to FileCount - 1 do
  begin
    // Create entry
    FillChar(bundle.Entries[i], SizeOf(TZPKFileEntry), 0);
    StrPCopy(@bundle.Entries[i].Name[0], Files[i].Filename);
    bundle.Entries[i].FileType := Files[i].FileType;
    bundle.Entries[i].Offset := offset + SizeOf(TZPKFileEntry) * FileCount;
    bundle.Entries[i].Size := Files[i].Size;
    bundle.Entries[i].Compressed := 0;
    
    // Write entry
    Move(bundle.Entries[i], PByte(bundle.Data) + offset, SizeOf(TZPKFileEntry));
    offset := offset + SizeOf(TZPKFileEntry);
    
    // Write file data
    Move(Files[i].Data^, PByte(bundle.Data) + bundle.Entries[i].Offset, Files[i].Size);
  end;
  
  BuildBundle := bundle;
end;
```

---

## Loading Asset Bundles

### Reading Bundle Header

**Read and validate bundle:**

```pascal
function LoadBundle(filename: string): TZPKBundle;
var
  fileHandle: TFileHandle;
  bundle: TZPKBundle;
begin
  fileHandle := FileOpen(filename, fmRead);
  if fileHandle = nil then
  begin
    bundle.Data := nil;
    LoadBundle := bundle;
    Exit;
  end;
  
  // Read header
  FileRead(fileHandle, bundle.Header, SizeOf(TZPKHeader));
  
  // Validate magic
  if (bundle.Header.Magic[0] <> 'Z') or
     (bundle.Header.Magic[1] <> 'P') or
     (bundle.Header.Magic[2] <> 'K') then
  begin
    FileClose(fileHandle);
    bundle.Data := nil;
    LoadBundle := bundle;
    Exit;
  end;
  
  // Read file entries
  FileRead(fileHandle, bundle.Entries, 
           SizeOf(TZPKFileEntry) * bundle.Header.FileCount);
  
  // Read file data (simplified - would need to calculate size)
  // ... read data ...
  
  FileClose(fileHandle);
  LoadBundle := bundle;
end;
```

### Extracting Files

**Extract file from bundle:**

```pascal
function ExtractFile(bundle: TZPKBundle; filename: string; 
                     var data: pointer; var size: dword): boolean;
var
  i: word;
  entry: ^TZPKFileEntry;
begin
  for i := 0 to bundle.Header.FileCount - 1 do
  begin
    entry := @bundle.Entries[i];
    
    if StrComp(@entry^.Name[0], PChar(filename)) = 0 then
    begin
      // Found file
      size := entry^.Size;
      GetMem(data, size);
      
      // Copy file data
      Move(PByte(bundle.Data) + entry^.Offset, data^, size);
      
      ExtractFile := true;
      Exit;
    end;
  end;
  
  ExtractFile := false;  // File not found
end;
```

### Bundle Manager

**Manage loaded bundles:**

```pascal
type
  TBundleManager = class
  private
    Bundles: array[0..15] of TZPKBundle;
    BundleCount: byte;
  public
    constructor Create;
    function LoadBundle(filename: string): boolean;
    function ExtractFile(bundleIndex: byte; filename: string; 
                        var data: pointer; var size: dword): boolean;
    procedure UnloadBundle(bundleIndex: byte);
  end;

implementation

constructor TBundleManager.Create;
begin
  BundleCount := 0;
end;

function TBundleManager.LoadBundle(filename: string): boolean;
begin
  if BundleCount < 16 then
  begin
    Bundles[BundleCount] := LoadBundle(filename);
    if Bundles[BundleCount].Data <> nil then
    begin
      BundleCount := BundleCount + 1;
      LoadBundle := true;
    end
    else
      LoadBundle := false;
  end
  else
    LoadBundle := false;
end;

function TBundleManager.ExtractFile(bundleIndex: byte; filename: string; 
                                    var data: pointer; var size: dword): boolean;
begin
  if bundleIndex < BundleCount then
    ExtractFile := ExtractFile(Bundles[bundleIndex], filename, data, size)
  else
    ExtractFile := false;
end;

procedure TBundleManager.UnloadBundle(bundleIndex: byte);
begin
  if bundleIndex < BundleCount then
  begin
    FreeMem(Bundles[bundleIndex].Data);
    // Remove bundle (shift array)
    // ... implementation ...
  end;
end;
```

---

## Practical Examples

### Creating Game Bundle

**Package game assets:**

```pascal
procedure CreateGameBundle;
var
  builder: TAssetBundleBuilder;
  spriteData: pointer;
  tilemapData: pointer;
  audioData: pointer;
begin
  builder := TAssetBundleBuilder.Create;
  
  // Add sprites
  spriteData := LoadSpriteSheet('sprites.bin');
  builder.AddFile('sprites.bin', FILE_TYPE_SPRITE, spriteData, GetFileSize('sprites.bin'));
  
  // Add tilemaps
  tilemapData := LoadTilemap('level1.tmx');
  builder.AddFile('level1.tmx', FILE_TYPE_TILEMAP, tilemapData, GetFileSize('level1.tmx'));
  
  // Add audio
  audioData := LoadAudio('music.bin');
  builder.AddFile('music.bin', FILE_TYPE_AUDIO, audioData, GetFileSize('music.bin'));
  
  // Build bundle
  var bundle := builder.BuildBundle;
  
  // Save bundle
  SaveBundle('game.zpk', bundle);
end;
```

### Loading Game Bundle

**Load assets from bundle:**

```pascal
procedure LoadGameAssets;
var
  bundleManager: TBundleManager;
  spriteData: pointer;
  tilemapData: pointer;
  spriteSize, tilemapSize: dword;
begin
  bundleManager := TBundleManager.Create;
  
  // Load bundle
  bundleManager.LoadBundle('game.zpk');
  
  // Extract sprites
  if bundleManager.ExtractFile(0, 'sprites.bin', spriteData, spriteSize) then
  begin
    LoadSpriteSheet(spriteData, spriteSize);
    FreeMem(spriteData);
  end;
  
  // Extract tilemap
  if bundleManager.ExtractFile(0, 'level1.tmx', tilemapData, tilemapSize) then
  begin
    LoadTilemap(tilemapData, tilemapSize);
    FreeMem(tilemapData);
  end;
end;
```

---

## Complete Example

**Putting it all together:**

```pascal
program AssetBundleDemo;

const
  FILE_TYPE_SPRITE = 0;
  FILE_TYPE_TILEMAP = 1;
  FILE_TYPE_AUDIO = 2;
  FILE_TYPE_DATA = 3;

type
  TZPKHeader = record
    Magic: array[0..2] of char;
    Version: byte;
    FileCount: word;
  end;
  
  TZPKFileEntry = record
    Name: array[0..31] of char;
    FileType: byte;
    Offset: dword;
    Size: dword;
  end;

var
  BundleManager: TBundleManager;

procedure CreateBundle;
var
  builder: TAssetBundleBuilder;
begin
  builder := TAssetBundleBuilder.Create;
  
  // Add files (simplified)
  // builder.AddFile('sprites.bin', FILE_TYPE_SPRITE, ...);
  
  var bundle := builder.BuildBundle;
  SaveBundle('game.zpk', bundle);
end;

procedure LoadBundle;
begin
  BundleManager := TBundleManager.Create;
  BundleManager.LoadBundle('game.zpk');
end;

begin
  InitGraphics;
  
  // Create bundle (development)
  CreateBundle;
  
  // Load bundle (runtime)
  LoadBundle;
  
  // Extract and use assets
  // ...
end.
```

---

## Best Practices

### 1. Validate Bundle Format

**Check magic and version:**

```pascal
// ✅ GOOD: Validate bundle
if (bundle.Header.Magic[0] = 'Z') and
   (bundle.Header.Magic[1] = 'P') and
   (bundle.Header.Magic[2] = 'K') then
  // Valid bundle

// ❌ BAD: No validation
// May read invalid data
```

### 2. Handle Missing Files

**Check if file exists in bundle:**

```pascal
// ✅ GOOD: Check before extracting
if ExtractFile(bundle, 'sprites.bin', data, size) then
  LoadSprites(data, size)
else
  WriteLn('Sprites not found in bundle');

// ❌ BAD: Assume file exists
ExtractFile(bundle, 'sprites.bin', data, size);  // May fail
```

### 3. Free Extracted Data

**Don't leak memory:**

```pascal
// ✅ GOOD: Free after use
ExtractFile(bundle, 'sprites.bin', data, size);
LoadSprites(data, size);
FreeMem(data);

// ❌ BAD: Memory leak
ExtractFile(bundle, 'sprites.bin', data, size);
// Never freed!
```

### 4. Use Appropriate File Types

**Categorize assets:**

```pascal
// ✅ GOOD: Clear file types
const
  FILE_TYPE_SPRITE = 0;
  FILE_TYPE_TILEMAP = 1;
  FILE_TYPE_AUDIO = 2;

// ❌ BAD: Unclear types
const
  FILE_TYPE_1 = 0;  // What is this?
```

### 5. Organize Bundle Contents

**Logical file organization:**

```pascal
// ✅ GOOD: Organized naming
'sprites_player.bin'
'sprites_enemy.bin'
'level1.tmx'
'level2.tmx'

// ❌ BAD: Random names
'file1.bin'
'file2.bin'
'data.bin'
```

---

## Exercises

### Exercise 1: Basic Bundle

Write a program that:
1. Creates an asset bundle
2. Adds files to bundle
3. Saves bundle to file
4. Loads bundle from file

### Exercise 2: Bundle Extraction

Write a program that:
1. Loads asset bundle
2. Extracts files from bundle
3. Uses extracted assets
4. Frees extracted data

### Exercise 3: Bundle Manager

Write a program that:
1. Implements bundle manager
2. Loads multiple bundles
3. Extracts files from bundles
4. Manages bundle lifecycle

### Exercise 4: Complete System

Write a program that:
1. Creates game asset bundle
2. Loads bundle at runtime
3. Extracts all assets
4. Uses assets in game

---

**Previous Section:** [Save Slots](./01_SaveSlots.md)  
**Next Section:** [Loading Maps and Sprites](./03_LoadingMapsAndSprites.md)  
**Language Specification:** See [Memory and I/O](../../languageSpecification/12_MemoryAndIO.md)  
**Last Updated:** 2025-01-XX

