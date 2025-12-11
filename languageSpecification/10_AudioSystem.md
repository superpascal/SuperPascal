# SuperPascal Language Specification — Audio System

## Complete Audio API and Type System

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

---

## 1. Overview

The SuperPascal Audio System provides comprehensive audio support for the Zeal 8-bit Computer, leveraging the Zeal Video Board (ZVB) audio hardware. It supports:

- Sound effects (SFX) with priority system
- Pattern-based music (tracker-style)
- PCM sample playback
- Audio streaming for long-form content
- Channel mixing and ducking
- Scripting integration

This specification defines the language-level support for audio, including types, intrinsics, and standard library units.

---

## 2. Audio Hardware Model

### 2.1 ZVB Audio Channels

The ZVB provides multiple audio channels:

```pascal
const
  AUDIO_CHANNELS = 8;  // Total audio channels
  AUDIO_MUSIC_CHANNELS = 4;  // Reserved for music
  AUDIO_SFX_CHANNELS = 4;    // Available for sound effects
```

### 2.2 Channel Properties

Each channel supports:
- Frequency (tone generation)
- Volume (0-255)
- Waveform (square, triangle, sawtooth, noise)
- PCM sample playback

---

## 3. Core Audio Types

### 3.1 Audio Channel

```pascal
type
  TAudioChannel = record
    Frequency: word;      // Frequency in Hz (or lookup index)
    Volume: byte;         // Volume (0-255)
    Waveform: byte;       // Waveform type
    Active: boolean;      // Channel active flag
    Priority: byte;       // Priority (higher = overrides lower)
    Owner: TAudioOwner;   // SFX, MUSIC, or IDLE
  end;
  
  TAudioOwner = (aoIdle, aoSFX, aoMusic);
```

### 3.2 Sound Effect

```pascal
type
  TSoundEffect = record
    Data: pointer;        // PCM data or sequence commands
    Length: word;         // Length in samples or commands
    Priority: byte;       // Priority (0-255)
    Loop: boolean;        // Loop flag
  end;
  
  PSoundEffect = ^TSoundEffect;
```

### 3.3 Music Track

```pascal
type
  TMusicTrack = record
    Patterns: ^TPattern;  // Pattern array
    PatternCount: byte;   // Number of patterns
    Order: ^byte;         // Playback order
    OrderLength: byte;    // Order list length
    Tempo: byte;          // Tempo (BPM / 10)
    Speed: byte;          // Rows per step
  end;
  
  PMusicTrack = ^TMusicTrack;
```

### 3.4 Music Pattern

```pascal
type
  TPattern = record
    Rows: array[0..63] of TMusicRow;
  end;
  
  TMusicRow = record
    Note: word;           // Frequency lookup index (0 = no note)
    Volume: byte;         // Volume (0-255)
    Effect: byte;         // Effect command
    EffectParam: byte;    // Effect parameter
  end;
```

### 3.5 Audio Manager

```pascal
type
  TAudioManager = class
  private
    Channels: array[0..AUDIO_CHANNELS-1] of TAudioChannel;
    MusicPlaying: boolean;
    CurrentMusic: PMusicTrack;
    MusicRow: byte;
    MusicStep: byte;
    MusicPattern: byte;
    MusicOrder: byte;
  public
    procedure Init;
    procedure Update;  // Called every frame
    procedure PlaySFX(id: byte);
    procedure StopSFX(id: byte);
    procedure PlayMusic(track: PMusicTrack);
    procedure StopMusic;
    procedure SetMasterVolume(volume: byte);
  end;
```

---

## 4. Audio Intrinsics

### 4.1 Low-Level Hardware Access

```pascal
procedure AudioRegWrite(addr: word; value: byte); intrinsic;
function AudioRegRead(addr: word): byte; intrinsic;
procedure AudioRegWriteWord(addr: word; value: word); intrinsic;
```

### 4.2 Channel Control

```pascal
procedure SetTone(channel: byte; freq: word; vol: byte); intrinsic;
procedure SetWaveform(channel: byte; waveform: byte); intrinsic;
procedure ChannelEnable(channel: byte; enabled: boolean); intrinsic;
```

### 4.3 Sound Effects

```pascal
procedure PlaySFX(id: byte); intrinsic;
procedure StopSFX(id: byte); intrinsic;
procedure StopAllSFX; intrinsic;
function IsSFXPlaying(id: byte): boolean; intrinsic;
```

### 4.4 Music Playback

```pascal
procedure PlayMusic(track: PMusicTrack); intrinsic;
procedure StopMusic; intrinsic;
procedure PauseMusic(pause: boolean); intrinsic;
function IsMusicPlaying: boolean; intrinsic;
```

### 4.5 Music Control

```pascal
procedure SetMusicTempo(tempo: byte); intrinsic;
procedure SetMusicSpeed(speed: byte); intrinsic;
procedure MusicFadeOut(frames: word); intrinsic;
procedure MusicFadeIn(frames: word); intrinsic;
procedure Crossfade(newTrack: PMusicTrack; frames: word); intrinsic;
```

### 4.6 Volume Control

```pascal
procedure SetMasterVolume(volume: byte); intrinsic;
procedure SetMusicVolume(volume: byte); intrinsic;
procedure SetSFXVolume(volume: byte); intrinsic;
procedure SetChannelVolume(channel: byte; volume: byte); intrinsic;
```

### 4.7 Audio Streaming

```pascal
procedure StreamMusicStart(id: byte); intrinsic;
procedure StreamMusicStop; intrinsic;
procedure StreamMusicUpdate; intrinsic;  // Called every frame
function IsStreaming: boolean; intrinsic;
```

### 4.8 Sample Playback

```pascal
procedure PlaySample(channel: byte; data: pointer; size: word); intrinsic;
procedure PlaySampleLoop(channel: byte; data: pointer; size: word); intrinsic;
procedure StopSample(channel: byte); intrinsic;
```

---

## 5. Standard Library Units

### 5.1 ZVB_Audio Unit

```pascal
unit ZVB_Audio;

interface
  // Initialization
  procedure Init;
  procedure Update;  // Call every frame
  
  // Channel control
  procedure SetTone(channel: byte; freq: word; vol: byte);
  procedure SetWaveform(channel: byte; waveform: byte);
  procedure ChannelEnable(channel: byte; enabled: boolean);
  
  // Sound effects
  procedure PlaySFX(id: byte);
  procedure StopSFX(id: byte);
  function IsSFXPlaying(id: byte): boolean;
  
  // Music
  procedure PlayMusic(track: PMusicTrack);
  procedure StopMusic;
  procedure PauseMusic(pause: boolean);
  
  // Volume
  procedure SetMasterVolume(volume: byte);
  procedure SetMusicVolume(volume: byte);
  procedure SetSFXVolume(volume: byte);
  
implementation
end.
```

### 5.2 Engine_Audio Unit

```pascal
unit Engine_Audio;

interface
  // Audio manager
  var Audio: TAudioManager;
  
  // High-level functions
  procedure PlaySound(name: string);
  procedure PlayMusic(name: string);
  procedure StopAllAudio;
  
  // Fade functions
  procedure FadeMusic(frames: word);
  procedure CrossfadeMusic(newTrack: PMusicTrack; frames: word);
  
  // Streaming
  procedure StreamMusic(name: string);
  procedure StopStreaming;
  
implementation
end.
```

### 5.3 Audio Constants

```pascal
unit AudioConstants;

interface
  // Waveform types
  const
    WAVEFORM_SQUARE = 0;
    WAVEFORM_TRIANGLE = 1;
    WAVEFORM_SAWTOOTH = 2;
    WAVEFORM_NOISE = 3;
    WAVEFORM_PCM = 4;
  
  // Effect commands
  const
    EFFECT_NONE = 0;
    EFFECT_VIBRATO = 1;
    EFFECT_SLIDE_UP = 2;
    EFFECT_SLIDE_DOWN = 3;
    EFFECT_VOLUME_SLIDE = 4;
    EFFECT_ARPEGGIO = 5;
  
  // Priority levels
  const
    PRIORITY_LOW = 64;
    PRIORITY_NORMAL = 128;
    PRIORITY_HIGH = 192;
    PRIORITY_CRITICAL = 255;
  
implementation
end.
```

---

## 6. Audio Mixing and Priority

### 6.1 Channel Ownership

Channels are assigned based on priority:

```pascal
type
  TChannelOwner = record
    Owner: TAudioOwner;
    Priority: byte;
    SFXID: byte;  // If owner is SFX
  end;
  
var
  ChannelOwner: array[0..AUDIO_CHANNELS-1] of TChannelOwner;
```

### 6.2 Mixing Rules

1. **SFX Priority**: Higher-priority SFX can preempt lower-priority SFX or music
2. **Music Reclaim**: Music attempts to reclaim channels when SFX ends
3. **Ducking**: High-priority SFX can duck music volume
4. **Channel Limit**: Maximum 4 channels for SFX, 4 for music

### 6.3 Ducking Implementation

```pascal
procedure ApplyDucking;
var i: byte;
begin
  if HighPrioritySFXPlaying then
  begin
    MusicVolume := MusicVolume * DUCK_FACTOR;  // e.g., 0.5
  end
  else
  begin
    MusicVolume := MusicVolume;  // Restore
  end;
end;
```

---

## 7. Music Engine

### 7.1 Music Update Loop

```pascal
procedure TAudioManager.UpdateMusic;
begin
  if not MusicPlaying then Exit;
  
  Inc(MusicStep);
  if MusicStep >= CurrentMusic^.Speed then
  begin
    MusicStep := 0;
    Inc(MusicRow);
    if MusicRow >= 64 then
    begin
      MusicRow := 0;
      Inc(MusicOrder);
      if MusicOrder >= CurrentMusic^.OrderLength then
        MusicOrder := 0;  // Loop
      MusicPattern := CurrentMusic^.Order[MusicOrder];
    end;
    
    ApplyMusicRow(MusicPattern, MusicRow);
  end;
end;
```

### 7.2 Row Application

```pascal
procedure ApplyMusicRow(pattern, row: byte);
var ch: byte;
    musicRow: TMusicRow;
begin
  for ch := 0 to AUDIO_MUSIC_CHANNELS - 1 do
  begin
    musicRow := CurrentMusic^.Patterns[pattern].Rows[row];
    if musicRow.Note <> 0 then
      SetTone(ch, NoteToFrequency(musicRow.Note), musicRow.Volume);
    ApplyEffect(ch, musicRow.Effect, musicRow.EffectParam);
  end;
end;
```

### 7.3 Effect Processing

```pascal
procedure ApplyEffect(channel: byte; effect, param: byte);
var freq: word;
begin
  case effect of
    EFFECT_VIBRATO:
      begin
        freq := Channels[channel].Frequency;
        freq := freq + SinWave(VibratoPhase) * param;
        SetTone(channel, freq, Channels[channel].Volume);
      end;
    EFFECT_SLIDE_UP:
      begin
        Channels[channel].Frequency := Channels[channel].Frequency + param;
        SetTone(channel, Channels[channel].Frequency, Channels[channel].Volume);
      end;
    // ... other effects
  end;
end;
```

---

## 8. Audio Streaming

### 8.1 Stream Format

```pascal
type
  TStreamHeader = record
    SampleRate: word;
    Channels: byte;
    Format: byte;  // 8-bit, 16-bit, etc.
    TotalSize: dword;
    ChunkCount: word;
  end;
  
  TStreamChunk = record
    Offset: dword;
    Size: word;
    Compressed: boolean;
    CRC32: dword;
  end;
```

### 8.2 Streaming Buffer

```pascal
type
  TStreamBuffer = record
    Data: array[0..16383] of byte;  // 16 KB buffer
    Position: word;
    Active: boolean;
  end;
  
var
  StreamBufferA: TStreamBuffer;
  StreamBufferB: TStreamBuffer;
  CurrentBuffer: ^TStreamBuffer;
```

### 8.3 Streaming Update

```pascal
procedure StreamMusicUpdate;
begin
  if not IsStreaming then Exit;
  
  // Check if current buffer is running low
  if CurrentBuffer^.Position > 12288 then  // 75% consumed
  begin
    // Switch to other buffer
    if CurrentBuffer = @StreamBufferA then
      CurrentBuffer := @StreamBufferB
    else
      CurrentBuffer := @StreamBufferA;
    
    // Load next chunk into inactive buffer
    LoadStreamChunk(CurrentBuffer);
  end;
  
  // Feed data to audio hardware
  FeedStreamToAudio(CurrentBuffer);
end;
```

---

## 9. Scripting Integration

### 9.1 Audio Script Commands

```pascal
// Script language commands
PLAY_MUSIC "Theme1"
PLAY_SFX "Explosion"
STOP_MUSIC
FADE_MUSIC 60
SET_VOLUME Music 128
STREAM_MUSIC "Narration"
WAIT_MUSIC_BEAT
WAIT_MUSIC_ROW
WAIT_STREAM_CHUNK
```

### 9.2 Script Event Hooks

```pascal
type
  TAudioEvent = (aeMusicEnd, aeMusicBeat, aeMusicRow, aeStreamChunk, aeStreamEnd);
  
procedure RegisterAudioEvent(event: TAudioEvent; handler: pointer);
```

---

## 10. Performance Guidelines

### 10.1 Audio Budget

- **Music update**: < 5% of frame time
- **SFX mixing**: < 2% of frame time
- **Streaming**: < 3% of frame time (background)
- **Total audio**: < 10% of frame time

### 10.2 Channel Usage

- **Music**: 2-4 channels typical
- **SFX**: 1-2 channels typical
- **Reserve**: 1-2 channels for critical SFX

### 10.3 Memory Usage

- **Music patterns**: Stored in ROM or loaded pages
- **SFX samples**: Small samples in RAM, large in pages
- **Streaming buffers**: 2 × 16 KB = 32 KB

---

## 11. Example Usage

### 11.1 Basic Audio Setup

```pascal
program AudioDemo;
uses ZVB_Audio, Engine_Audio;

begin
  Audio.Init;
  Audio.SetMasterVolume(255);
  Audio.SetMusicVolume(200);
  Audio.SetSFXVolume(255);
  
  // Play music
  Audio.PlayMusic(@ThemeMusic);
  
  // Game loop
  while true do
  begin
    // Update audio
    Audio.Update;
    
    // Play SFX on input
    if InputPressed(KEY_A) then
      Audio.PlaySFX(SFX_JUMP);
    
    WaitVBlank;
  end;
end.
```

### 11.2 Music with Fade

```pascal
procedure SwitchMusic(newTrack: PMusicTrack);
begin
  // Fade out current music
  Audio.MusicFadeOut(60);  // 1 second at 60 FPS
  
  // Wait for fade
  while Audio.IsMusicPlaying do
    WaitVBlank;
  
  // Fade in new music
  Audio.PlayMusic(newTrack);
  Audio.MusicFadeIn(60);
end;
```

### 11.3 Streaming Audio

```pascal
procedure PlayCutsceneAudio;
begin
  // Start streaming
  Audio.StreamMusicStart(STREAM_NARRATION);
  
  // Wait for stream to complete
  while Audio.IsStreaming do
  begin
    Audio.Update;
    WaitVBlank;
  end;
  
  // Resume normal music
  Audio.PlayMusic(@BackgroundMusic);
end;
```

---

## 12. Integration with Game Engine

Audio integrates with the ECS system:

```pascal
procedure AudioSystem;
var i: word;
    id: TEntityID;
begin
  // Update music
  Audio.Update;
  
  // Check for entity audio triggers
  for i := 0 to AudioEntityCount - 1 do
  begin
    id := AudioEntities[i];
    if EntityChangeMask[id] contains [chState] then
    begin
      // Trigger SFX on state change
      case EntityState[id] of
        STATE_JUMP: Audio.PlaySFX(SFX_JUMP);
        STATE_LAND: Audio.PlaySFX(SFX_LAND);
        STATE_HIT: Audio.PlaySFX(SFX_HIT);
      end;
    end;
  end;
end;
```

---

## 13. Summary

The SuperPascal Audio System provides:

- **Hardware abstraction**: Clean API over ZVB audio
- **SFX system**: Priority-based sound effects
- **Music engine**: Tracker-style pattern playback
- **Streaming**: Long-form PCM audio support
- **Mixing**: Channel management and ducking
- **Scripting**: Audio commands in game scripts
- **Performance**: < 10% frame time budget

This enables rich, expressive audio for SuperPascal games while maintaining performance on the Z80 CPU.

---

**End of Audio System Specification**

