# Streaming and Mixers

**Part of:** [Chapter 20: Audio Programming](./README.md)

---

## Introduction

Audio streaming and mixing enable advanced audio features like long-form content, narration, and sophisticated channel management. This section teaches you how to stream audio and manage multiple audio sources.

**Key concepts:**
- **Audio streaming** — Long-form PCM audio playback
- **Channel mixing** — Combining multiple audio sources
- **Volume control** — Master, music, and SFX volume
- **Audio ducking** — Lowering one source when another plays

---

## Audio Streaming

### What is Audio Streaming?

**Audio streaming plays long-form PCM audio:**

- **PCM samples** — Raw audio data
- **Streaming** — Loads data in chunks (not all at once)
- **Memory efficient** — Doesn't load entire file
- **Long content** — Cutscenes, narration, voice acting

**Use cases:**
- Cutscene audio
- Narration
- Voice acting
- Long music tracks
- Ambient sounds

### Starting a Stream

**Start streaming audio:**

```pascal
procedure StreamMusicStart(id: byte);
```

**Parameters:**
- `id` — Stream ID (predefined stream)

**Example:**
```pascal
// Start streaming narration
StreamMusicStart(STREAM_NARRATION);
```

**How it works:**
- Loads stream data
- Begins playback
- Updates automatically each frame

### Stopping a Stream

**Stop streaming:**

```pascal
procedure StreamMusicStop;
```

**Example:**
```pascal
// Stop current stream
StreamMusicStop;
```

### Updating Streams

**Update stream each frame:**

```pascal
procedure StreamMusicUpdate;
```

**Call every frame:**
```pascal
while true do
begin
  // Update stream (loads next chunk if needed)
  StreamMusicUpdate;
  
  // ... game logic ...
  
  WaitVBlank;
end;
```

**How it works:**
- Checks if more data needed
- Loads next chunk from storage
- Continues playback seamlessly

### Checking Stream Status

**Check if streaming:**

```pascal
function IsStreaming: boolean;
```

**Example:**
```pascal
if IsStreaming then
  WriteLn('Stream is playing');
```

**Use cases:**
- Wait for stream to finish
- Check playback state
- Conditional logic

---

## Practical Streaming Examples

### Cutscene Audio

**Play audio during cutscene:**

```pascal
procedure PlayCutscene(cutsceneID: byte);
begin
  // Start cutscene audio stream
  StreamMusicStart(cutsceneID);
  
  // Play cutscene
  while IsStreaming do
  begin
    StreamMusicUpdate;  // Update stream
    RenderCutscene;
    WaitVBlank;
  end;
  
  // Cutscene finished
  StreamMusicStop;
end;
```

### Narration

**Play narration with subtitles:**

```pascal
procedure PlayNarration(narrationID: byte);
begin
  // Start narration stream
  StreamMusicStart(narrationID);
  
  // Show subtitles
  ShowSubtitles(narrationID);
  
  // Wait for narration to finish
  while IsStreaming do
  begin
    StreamMusicUpdate;
    RenderSubtitles;
    WaitVBlank;
  end;
  
  StreamMusicStop;
  HideSubtitles;
end;
```

### Voice Acting

**Play character voice lines:**

```pascal
procedure PlayVoiceLine(character: byte; line: byte);
var
  streamID: byte;
begin
  // Calculate stream ID
  streamID := character * 10 + line;
  
  // Play voice line
  StreamMusicStart(streamID);
  
  // Wait for completion
  while IsStreaming do
  begin
    StreamMusicUpdate;
    WaitVBlank;
  end;
  
  StreamMusicStop;
end;
```

---

## Volume Control

### Master Volume

**Control overall audio volume:**

```pascal
procedure SetMasterVolume(volume: byte);
```

**Parameters:**
- `volume` — Volume level (0-255, 255 = full volume)

**Example:**
```pascal
SetMasterVolume(255);  // Full volume
SetMasterVolume(128); // 50% volume
SetMasterVolume(0);   // Muted
```

**Use cases:**
- User settings
- Mute functionality
- Volume slider

### Music Volume

**Control music volume separately:**

```pascal
procedure SetMusicVolume(volume: byte);
```

**Example:**
```pascal
SetMusicVolume(200);  // Music at ~78% volume
SetMusicVolume(128); // Music at 50% volume
```

**Use cases:**
- Separate music/SFX controls
- User preferences
- Audio mixing

### SFX Volume

**Control sound effect volume:**

```pascal
procedure SetSFXVolume(volume: byte);
```

**Example:**
```pascal
SetSFXVolume(255);  // SFX at full volume
SetSFXVolume(180); // SFX at ~70% volume
```

**Use cases:**
- Separate music/SFX controls
- User preferences
- Audio mixing

### Channel Volume

**Control individual channel volume:**

```pascal
procedure SetChannelVolume(channel: byte; volume: byte);
```

**Example:**
```pascal
SetChannelVolume(0, 255);  // Channel 0 full volume
SetChannelVolume(1, 128); // Channel 1 at 50%
```

**Use cases:**
- Fine-grained control
- Audio mixing
- Special effects

---

## Audio Mixing

### Understanding Mixing

**Mixing combines multiple audio sources:**

- **Multiple channels** — Different sounds on different channels
- **Volume balancing** — Adjust relative volumes
- **Priority** — Which sounds take precedence
- **Ducking** — Lower one source when another plays

### Volume Balancing

**Balance music and SFX:**

```pascal
procedure SetupAudioBalance;
begin
  // Music at 70% volume
  SetMusicVolume(180);
  
  // SFX at full volume
  SetSFXVolume(255);
  
  // Master at 80% volume
  SetMasterVolume(204);
end;
```

**Result:**
- Music: 180/255 * 204/255 ≈ 56% effective volume
- SFX: 255/255 * 204/255 ≈ 80% effective volume

### Audio Ducking

**Lower music when important SFX plays:**

```pascal
var
  originalMusicVolume: byte;
  duckingActive: boolean;

procedure DuckMusicForSFX;
begin
  if not duckingActive then
  begin
    // Save original volume
    originalMusicVolume := GetMusicVolume;
    
    // Lower music volume
    SetMusicVolume(originalMusicVolume div 2);  // 50% of original
    
    duckingActive := true;
  end;
end;

procedure RestoreMusicVolume;
begin
  if duckingActive then
  begin
    // Restore original volume
    SetMusicVolume(originalMusicVolume);
    duckingActive := false;
  end;
end;

// Usage
procedure PlayImportantSFX(id: byte);
begin
  DuckMusicForSFX;  // Lower music
  PlaySFX(id);       // Play SFX
  
  // Restore after SFX finishes
  while IsSFXPlaying(id) do
    WaitVBlank;
  
  RestoreMusicVolume;  // Restore music
end;
```

### Channel Mixing

**Mix multiple channels:**

```pascal
procedure MixChannels;
var
  i: byte;
  totalVolume: word;
begin
  // Calculate total volume across channels
  totalVolume := 0;
  for i := 0 to AUDIO_CHANNELS - 1 do
    totalVolume := totalVolume + GetChannelVolume(i);
  
  // Prevent clipping (too loud)
  if totalVolume > MAX_SAFE_VOLUME then
  begin
    // Reduce all channels proportionally
    var scale := MAX_SAFE_VOLUME / totalVolume;
    for i := 0 to AUDIO_CHANNELS - 1 do
      SetChannelVolume(i, Round(GetChannelVolume(i) * scale));
  end;
end;
```

---

## Complete Audio System Example

**Putting it all together:**

```pascal
program AudioSystemDemo;

type
  TAudioSystem = record
    MasterVolume: byte;
    MusicVolume: byte;
    SFXVolume: byte;
    Streaming: boolean;
  end;

var
  Audio: TAudioSystem;

procedure InitAudio;
begin
  Audio.MasterVolume := 255;
  Audio.MusicVolume := 200;
  Audio.SFXVolume := 255;
  Audio.Streaming := false;
  
  SetMasterVolume(Audio.MasterVolume);
  SetMusicVolume(Audio.MusicVolume);
  SetSFXVolume(Audio.SFXVolume);
end;

procedure UpdateAudio;
begin
  // Update stream if active
  if Audio.Streaming then
  begin
    StreamMusicUpdate;
    
    // Check if stream finished
    if not IsStreaming then
    begin
      Audio.Streaming := false;
      StreamMusicStop;
    end;
  end;
end;

procedure PlayCutscene(cutsceneID: byte);
begin
  // Stop music
  StopMusic;
  
  // Start stream
  StreamMusicStart(cutsceneID);
  Audio.Streaming := true;
  
  // Play cutscene
  while Audio.Streaming do
  begin
    UpdateAudio;
    RenderCutscene;
    WaitVBlank;
  end;
  
  // Resume music
  PlayMusic(@BackgroundMusic);
end;

procedure HandleAudioInput;
begin
  if InputPressed(KEY_VOLUME_UP) then
  begin
    if Audio.MasterVolume < 255 then
    begin
      Audio.MasterVolume := Audio.MasterVolume + 16;
      SetMasterVolume(Audio.MasterVolume);
    end;
  end
  else if InputPressed(KEY_VOLUME_DOWN) then
  begin
    if Audio.MasterVolume > 0 then
    begin
      Audio.MasterVolume := Audio.MasterVolume - 16;
      SetMasterVolume(Audio.MasterVolume);
    end;
  end
  else if InputPressed(KEY_MUTE) then
  begin
    if Audio.MasterVolume > 0 then
    begin
      Audio.MasterVolume := 0;
      SetMasterVolume(0);
    end
    else
    begin
      Audio.MasterVolume := 255;
      SetMasterVolume(255);
    end;
  end;
end;

begin
  InitGraphics;
  InitAudio;
  PlayMusic(@BackgroundMusic);
  
  while true do
  begin
    HandleInput;
    HandleAudioInput;
    UpdateAudio;
    UpdateGame;
    RenderGame;
    WaitVBlank;
  end;
end.
```

---

## Best Practices

### 1. Update Streams Every Frame

**Always update streams:**

```pascal
// ✅ GOOD: Update every frame
while true do
begin
  StreamMusicUpdate;
  WaitVBlank;
end;

// ❌ BAD: Forget to update
while true do
begin
  // Stream not updated - may stutter
  WaitVBlank;
end;
```

### 2. Check Stream Status

**Verify stream state:**

```pascal
// ✅ GOOD: Check before operations
if IsStreaming then
  StreamMusicStop;

// ❌ BAD: Assume stream state
StreamMusicStop;  // May not be streaming
```

### 3. Balance Volumes

**Set appropriate volume levels:**

```pascal
// ✅ GOOD: Balanced volumes
SetMusicVolume(180);  // Music quieter
SetSFXVolume(255);    // SFX louder

// ❌ BAD: All same volume
SetMusicVolume(255);
SetSFXVolume(255);  // May clash
```

### 4. Use Ducking Appropriately

**Duck for important sounds:**

```pascal
// ✅ GOOD: Duck music for important SFX
DuckMusicForSFX;
PlaySFX(SFX_CRITICAL);
RestoreMusicVolume;

// ❌ BAD: Always duck
DuckMusicForSFX;
PlaySFX(SFX_AMBIENT);  // Unnecessary
```

### 5. Manage Memory

**Stream large files, don't load:**

```pascal
// ✅ GOOD: Stream large audio
StreamMusicStart(LONG_AUDIO);

// ❌ BAD: Load entire file
LoadMusic(LONG_AUDIO);  // May not fit in memory
```

---

## Exercises

### Exercise 1: Basic Streaming

Write a program that:
1. Starts an audio stream
2. Updates stream each frame
3. Checks stream status
4. Stops stream when finished

### Exercise 2: Volume Control

Write a program that:
1. Controls master volume
2. Controls music and SFX volume separately
3. Implements volume slider
4. Saves volume settings

### Exercise 3: Audio Ducking

Write a program that:
1. Implements audio ducking
2. Lowers music when important SFX plays
3. Restores music after SFX
4. Demonstrates smooth transitions

### Exercise 4: Complete Audio System

Write a program that:
1. Implements complete audio system
2. Handles streaming, volume, mixing
3. Integrates with game
4. Manages all audio resources

---

**Previous Section:** [Music Tracks](./02_MusicTracks.md)  
**Next Chapter:** [Chapter 21: Scripting and Events](../21_ScriptingAndEvents/README.md)  
**Language Specification:** See [Audio System](../../languageSpecification/10_AudioSystem.md)  
**Last Updated:** 2025-01-XX

