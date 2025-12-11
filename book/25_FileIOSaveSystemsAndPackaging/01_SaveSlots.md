# Save Slots

**Part of:** [Chapter 25: File I/O, Save Systems, and Packaging](./README.md)

---

## Introduction

Save slots allow players to save their progress and resume later. This section teaches you how to implement a save slot system, serialize game state, and load saved games.

**Key concepts:**
- **Save slots** — Multiple save files
- **Serialization** — Converting data to bytes
- **Save data structure** — Organizing save information
- **Loading** — Restoring game state

---

## Understanding Save Systems

### What is a Save Slot?

**A save slot is a file that stores game state:**

- **Player progress** — Level, score, items
- **Game state** — Entity positions, flags
- **Settings** — Volume, controls
- **Timestamp** — When saved

**Save slot system:**
- **Multiple slots** — Slot 1, 2, 3, etc.
- **Save to file** — Write to storage
- **Load from file** — Read from storage
- **Validate** — Check save data integrity

### Save Data Structure

**Organize save data:**

```pascal
type
  TSaveData = record
    Magic: array[0..3] of char;  // "SAVE" magic number
    Version: byte;                // Save format version
    Checksum: word;               // Data integrity check
    
    // Game state
    Level: byte;
    Score: word;
    Lives: byte;
    PlayerX: integer;
    PlayerY: integer;
    
    // Inventory
    Items: array[0..31] of byte;
    ItemCount: byte;
    
    // Flags
    Flags: array[0..15] of boolean;
    
    // Timestamp
    SaveTime: dword;  // Unix timestamp or frame count
  end;
```

---

## Saving Game State

### Serialization

**Convert game state to bytes:**

```pascal
procedure SerializeSaveData(var data: TSaveData);
begin
  // Set magic number
  data.Magic[0] := 'S';
  data.Magic[1] := 'A';
  data.Magic[2] := 'V';
  data.Magic[3] := 'E';
  
  // Set version
  data.Version := 1;
  
  // Copy game state
  data.Level := CurrentLevel;
  data.Score := PlayerScore;
  data.Lives := PlayerLives;
  data.PlayerX := PlayerX;
  data.PlayerY := PlayerY;
  
  // Copy inventory
  CopyInventory(data.Items, data.ItemCount);
  
  // Copy flags
  CopyFlags(data.Flags);
  
  // Set timestamp
  data.SaveTime := GetCurrentTime;
  
  // Calculate checksum
  data.Checksum := CalculateChecksum(data);
end;

function CalculateChecksum(var data: TSaveData): word;
var
  i: word;
  sum: word;
  bytes: ^byte;
begin
  sum := 0;
  bytes := @data;
  
  // Sum all bytes except checksum field
  for i := 0 to SizeOf(TSaveData) - SizeOf(data.Checksum) - 1 do
    sum := sum + bytes[i];
  
  CalculateChecksum := sum;
end;
```

### Writing Save File

**Write save data to file:**

```pascal
function SaveToSlot(slot: byte): boolean;
var
  data: TSaveData;
  filename: string;
  fileHandle: TFileHandle;
begin
  // Serialize game state
  SerializeSaveData(data);
  
  // Create filename
  filename := 'save' + IntToStr(slot) + '.sav';
  
  // Open file for writing
  fileHandle := FileOpen(filename, fmWrite);
  if fileHandle = nil then
  begin
    SaveToSlot := false;
    Exit;
  end;
  
  // Write save data
  if FileWrite(fileHandle, data, SizeOf(TSaveData)) = SizeOf(TSaveData) then
    SaveToSlot := true
  else
    SaveToSlot := false;
  
  // Close file
  FileClose(fileHandle);
end;
```

---

## Loading Game State

### Reading Save File

**Read save data from file:**

```pascal
function LoadFromSlot(slot: byte): boolean;
var
  data: TSaveData;
  filename: string;
  fileHandle: TFileHandle;
  checksum: word;
begin
  // Create filename
  filename := 'save' + IntToStr(slot) + '.sav';
  
  // Open file for reading
  fileHandle := FileOpen(filename, fmRead);
  if fileHandle = nil then
  begin
    LoadFromSlot := false;
    Exit;
  end;
  
  // Read save data
  if FileRead(fileHandle, data, SizeOf(TSaveData)) <> SizeOf(TSaveData) then
  begin
    FileClose(fileHandle);
    LoadFromSlot := false;
    Exit;
  end;
  
  // Close file
  FileClose(fileHandle);
  
  // Validate magic number
  if (data.Magic[0] <> 'S') or (data.Magic[1] <> 'A') or
     (data.Magic[2] <> 'V') or (data.Magic[3] <> 'E') then
  begin
    LoadFromSlot := false;
    Exit;
  end;
  
  // Validate checksum
  checksum := CalculateChecksum(data);
  if checksum <> data.Checksum then
  begin
    LoadFromSlot := false;  // Corrupted save
    Exit;
  end;
  
  // Restore game state
  RestoreGameState(data);
  LoadFromSlot := true;
end;
```

### Restoring Game State

**Restore game from save data:**

```pascal
procedure RestoreGameState(var data: TSaveData);
begin
  // Restore game state
  CurrentLevel := data.Level;
  PlayerScore := data.Score;
  PlayerLives := data.Lives;
  PlayerX := data.PlayerX;
  PlayerY := data.PlayerY;
  
  // Restore inventory
  RestoreInventory(data.Items, data.ItemCount);
  
  // Restore flags
  RestoreFlags(data.Flags);
  
  // Load level
  LoadLevel(data.Level);
  
  // Position player
  EntitySetPosition(Player, data.PlayerX, data.PlayerY);
end;
```

---

## Save Slot Management

### Save Slot System

**Manage multiple save slots:**

```pascal
type
  TSaveSlot = record
    Exists: boolean;
    Level: byte;
    Score: word;
    Timestamp: dword;
  end;
  
  TSaveSlotManager = class
  private
    Slots: array[1..3] of TSaveSlot;
  public
    constructor Create;
    procedure RefreshSlots;
    function SaveToSlot(slot: byte): boolean;
    function LoadFromSlot(slot: byte): boolean;
    function SlotExists(slot: byte): boolean;
    function GetSlotInfo(slot: byte): TSaveSlot;
  end;

implementation

constructor TSaveSlotManager.Create;
begin
  RefreshSlots;
end;

procedure TSaveSlotManager.RefreshSlots;
var
  i: byte;
  filename: string;
  fileHandle: TFileHandle;
  data: TSaveData;
begin
  for i := 1 to 3 do
  begin
    filename := 'save' + IntToStr(i) + '.sav';
    fileHandle := FileOpen(filename, fmRead);
    
    if fileHandle <> nil then
    begin
      if FileRead(fileHandle, data, SizeOf(TSaveData)) = SizeOf(TSaveData) then
      begin
        Slots[i].Exists := true;
        Slots[i].Level := data.Level;
        Slots[i].Score := data.Score;
        Slots[i].Timestamp := data.SaveTime;
      end
      else
        Slots[i].Exists := false;
      
      FileClose(fileHandle);
    end
    else
      Slots[i].Exists := false;
  end;
end;

function TSaveSlotManager.SaveToSlot(slot: byte): boolean;
begin
  SaveToSlot := SaveToSlot(slot);
  RefreshSlots;  // Update slot info
end;

function TSaveSlotManager.LoadFromSlot(slot: byte): boolean;
begin
  LoadFromSlot := LoadFromSlot(slot);
end;

function TSaveSlotManager.SlotExists(slot: byte): boolean;
begin
  if (slot >= 1) and (slot <= 3) then
    SlotExists := Slots[slot].Exists
  else
    SlotExists := false;
end;

function TSaveSlotManager.GetSlotInfo(slot: byte): TSaveSlot;
begin
  if (slot >= 1) and (slot <= 3) then
    GetSlotInfo := Slots[slot]
  else
    GetSlotInfo.Exists := false;
end;
```

### Save Menu

**Display save slots in menu:**

```pascal
procedure RenderSaveMenu;
var
  i: byte;
  slot: TSaveSlot;
  y: integer;
begin
  y := 80;
  
  for i := 1 to 3 do
  begin
    slot := SaveSlotManager.GetSlotInfo(i);
    
    if slot.Exists then
    begin
      DrawText(100, y, 'Slot ' + IntToStr(i) + ':');
      DrawText(120, y + 10, 'Level ' + IntToStr(slot.Level));
      DrawText(120, y + 20, 'Score: ' + IntToStr(slot.Score));
    end
    else
    begin
      DrawText(100, y, 'Slot ' + IntToStr(i) + ': Empty');
    end;
    
    y := y + 60;
  end;
end;
```

---

## Practical Examples

### Simple Save System

**Basic save/load:**

```pascal
program SaveSystemDemo;

type
  TGameState = record
    Level: byte;
    Score: word;
    X, Y: integer;
  end;

var
  GameState: TGameState;

procedure SaveGame;
var
  fileHandle: TFileHandle;
begin
  fileHandle := FileOpen('save.sav', fmWrite);
  if fileHandle <> nil then
  begin
    FileWrite(fileHandle, GameState, SizeOf(TGameState));
    FileClose(fileHandle);
    WriteLn('Game saved!');
  end;
end;

procedure LoadGame;
var
  fileHandle: TFileHandle;
begin
  fileHandle := FileOpen('save.sav', fmRead);
  if fileHandle <> nil then
  begin
    FileRead(fileHandle, GameState, SizeOf(TGameState));
    FileClose(fileHandle);
    WriteLn('Game loaded!');
  end;
end;

begin
  // Initialize game
  GameState.Level := 1;
  GameState.Score := 0;
  GameState.X := 160;
  GameState.Y := 120;
  
  // Save game
  SaveGame;
  
  // Modify state
  GameState.Score := 1000;
  
  // Load game (restores old state)
  LoadGame;
  WriteLn('Score: ', GameState.Score);  // Back to 0
end.
```

### Save with Validation

**Save with checksum:**

```pascal
type
  TSaveData = record
    Magic: array[0..3] of char;
    Checksum: word;
    Data: TGameState;
  end;

function SaveGame: boolean;
var
  saveData: TSaveData;
  fileHandle: TFileHandle;
  i: word;
  sum: word;
  bytes: ^byte;
begin
  // Set magic
  saveData.Magic[0] := 'S';
  saveData.Magic[1] := 'A';
  saveData.Magic[2] := 'V';
  saveData.Magic[3] := 'E';
  
  // Copy game state
  saveData.Data := GameState;
  
  // Calculate checksum
  sum := 0;
  bytes := @saveData.Data;
  for i := 0 to SizeOf(TGameState) - 1 do
    sum := sum + bytes[i];
  saveData.Checksum := sum;
  
  // Write file
  fileHandle := FileOpen('save.sav', fmWrite);
  if fileHandle = nil then
  begin
    SaveGame := false;
    Exit;
  end;
  
  SaveGame := FileWrite(fileHandle, saveData, SizeOf(TSaveData)) = SizeOf(TSaveData);
  FileClose(fileHandle);
end;
```

---

## Complete Example

**Putting it all together:**

```pascal
program SaveSlotDemo;

type
  TSaveData = record
    Magic: array[0..3] of char;
    Version: byte;
    Checksum: word;
    Level: byte;
    Score: word;
    PlayerX, PlayerY: integer;
  end;
  
  TSaveSlotManager = class
  public
    function SaveToSlot(slot: byte): boolean;
    function LoadFromSlot(slot: byte): boolean;
    function SlotExists(slot: byte): boolean;
  end;

var
  SaveManager: TSaveSlotManager;
  CurrentLevel: byte;
  PlayerScore: word;
  PlayerX, PlayerY: integer;

function TSaveSlotManager.SaveToSlot(slot: byte): boolean;
var
  data: TSaveData;
  filename: string;
  fileHandle: TFileHandle;
  sum: word;
  i: word;
  bytes: ^byte;
begin
  // Prepare save data
  data.Magic[0] := 'S';
  data.Magic[1] := 'A';
  data.Magic[2] := 'V';
  data.Magic[3] := 'E';
  data.Version := 1;
  data.Level := CurrentLevel;
  data.Score := PlayerScore;
  data.PlayerX := PlayerX;
  data.PlayerY := PlayerY;
  
  // Calculate checksum
  sum := 0;
  bytes := @data.Level;
  for i := 0 to SizeOf(data) - SizeOf(data.Magic) - SizeOf(data.Checksum) - 1 do
    sum := sum + bytes[i];
  data.Checksum := sum;
  
  // Write file
  filename := 'save' + IntToStr(slot) + '.sav';
  fileHandle := FileOpen(filename, fmWrite);
  if fileHandle = nil then
  begin
    SaveToSlot := false;
    Exit;
  end;
  
  SaveToSlot := FileWrite(fileHandle, data, SizeOf(TSaveData)) = SizeOf(TSaveData);
  FileClose(fileHandle);
end;

function TSaveSlotManager.LoadFromSlot(slot: byte): boolean;
var
  data: TSaveData;
  filename: string;
  fileHandle: TFileHandle;
  checksum: word;
  i: word;
  bytes: ^byte;
begin
  filename := 'save' + IntToStr(slot) + '.sav';
  fileHandle := FileOpen(filename, fmRead);
  if fileHandle = nil then
  begin
    LoadFromSlot := false;
    Exit;
  end;
  
  if FileRead(fileHandle, data, SizeOf(TSaveData)) <> SizeOf(TSaveData) then
  begin
    FileClose(fileHandle);
    LoadFromSlot := false;
    Exit;
  end;
  
  FileClose(fileHandle);
  
  // Validate
  if (data.Magic[0] <> 'S') or (data.Magic[1] <> 'A') or
     (data.Magic[2] <> 'V') or (data.Magic[3] <> 'E') then
  begin
    LoadFromSlot := false;
    Exit;
  end;
  
  // Verify checksum
  checksum := 0;
  bytes := @data.Level;
  for i := 0 to SizeOf(data) - SizeOf(data.Magic) - SizeOf(data.Checksum) - 1 do
    checksum := checksum + bytes[i];
  
  if checksum <> data.Checksum then
  begin
    LoadFromSlot := false;  // Corrupted
    Exit;
  end;
  
  // Restore state
  CurrentLevel := data.Level;
  PlayerScore := data.Score;
  PlayerX := data.PlayerX;
  PlayerY := data.PlayerY;
  
  LoadFromSlot := true;
end;

function TSaveSlotManager.SlotExists(slot: byte): boolean;
var
  filename: string;
  fileHandle: TFileHandle;
begin
  filename := 'save' + IntToStr(slot) + '.sav';
  fileHandle := FileOpen(filename, fmRead);
  if fileHandle <> nil then
  begin
    FileClose(fileHandle);
    SlotExists := true;
  end
  else
    SlotExists := false;
end;

begin
  InitGraphics;
  SaveManager := TSaveSlotManager.Create;
  
  // Initialize game
  CurrentLevel := 1;
  PlayerScore := 0;
  PlayerX := 160;
  PlayerY := 120;
  
  // Game loop
  while true do
  begin
    HandleInput;
    
    if InputPressed(KEY_S) then
      SaveManager.SaveToSlot(1);
    
    if InputPressed(KEY_L) then
      SaveManager.LoadFromSlot(1);
    
    UpdateGame;
    RenderGame;
    WaitVBlank;
  end;
end.
```

---

## Best Practices

### 1. Always Validate Save Data

**Check integrity:**

```pascal
// ✅ GOOD: Validate save data
if ValidateSaveData(data) then
  RestoreGameState(data)
else
  WriteLn('Corrupted save file!');

// ❌ BAD: No validation
RestoreGameState(data);  // May crash if corrupted
```

### 2. Use Checksums

**Detect corruption:**

```pascal
// ✅ GOOD: Checksum validation
data.Checksum := CalculateChecksum(data);
// ... save ...
if LoadedData.Checksum <> CalculateChecksum(LoadedData) then
  // Corrupted!

// ❌ BAD: No checksum
// Can't detect corruption
```

### 3. Handle Errors Gracefully

**Don't crash on save errors:**

```pascal
// ✅ GOOD: Error handling
if not SaveToSlot(slot) then
  ShowError('Failed to save game');

// ❌ BAD: No error handling
SaveToSlot(slot);  // May fail silently
```

### 4. Version Save Format

**Support multiple versions:**

```pascal
// ✅ GOOD: Version checking
case data.Version of
  1: LoadVersion1(data);
  2: LoadVersion2(data);
else
  WriteLn('Unsupported save version');
end;

// ❌ BAD: No version
// Can't upgrade old saves
```

### 5. Backup Important Data

**Keep backup saves:**

```pascal
// ✅ GOOD: Backup before overwriting
if SlotExists(slot) then
  CopyFile('save' + IntToStr(slot) + '.sav', 
           'save' + IntToStr(slot) + '.bak');

// ❌ BAD: No backup
// Lost if save fails
```

---

## Exercises

### Exercise 1: Basic Save System

Write a program that:
1. Saves game state to file
2. Loads game state from file
3. Validates save data
4. Handles errors

### Exercise 2: Save Slots

Write a program that:
1. Implements multiple save slots
2. Displays slot information
3. Saves to specific slot
4. Loads from specific slot

### Exercise 3: Save Validation

Write a program that:
1. Uses checksums for validation
2. Detects corrupted saves
3. Handles version differences
4. Provides error messages

### Exercise 4: Complete Save System

Write a program that:
1. Implements complete save system
2. Saves all game state
3. Loads and restores everything
4. Integrates with game

---

**Previous Chapter:** [Chapter 24: Debugging, Profiling, and Optimization](../24_DebuggingProfilingAndOptimization/README.md)  
**Next Section:** [Asset Bundles](./02_AssetBundles.md)  
**Language Specification:** See [Memory and I/O](../../languageSpecification/12_MemoryAndIO.md)  
**Last Updated:** 2025-01-XX

