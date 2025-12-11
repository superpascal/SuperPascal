# Extended Audio Intrinsics

**Part of: [ZealZ80 Platform Intrinsics](./README.md)

---

### 11.1 Low-Level Audio Hardware

#### AudioRegWrite

**Syntax:**
```pascal
procedure AudioRegWrite(addr: word; value: byte);
```

**Purpose**: Write to ZVB audio register.

**Usage:**
```pascal
AudioRegWrite($8000, $FF);  // Write to audio register
```

#### AudioRegRead

**Syntax:**
```pascal
function AudioRegRead(addr: word): byte;
```

**Purpose**: Read from ZVB audio register.

**Usage:**
```pascal
var status: byte;
status := AudioRegRead($8001);
```

#### SetTone

**Syntax:**
```pascal
procedure SetTone(channel: byte; freq: word; vol: byte);
```

**Purpose**: Set audio channel frequency and volume.

**Parameters:**
- `channel`: Audio channel (0-7)
- `freq`: Frequency in Hz or lookup index
- `vol`: Volume (0-255)

**Usage:**
```pascal
SetTone(0, 440, 128);  // A4 note at 50% volume
```

#### SetWaveform

**Syntax:**
```pascal
procedure SetWaveform(channel: byte; waveform: byte);
```

**Purpose**: Set audio channel waveform type.

**Waveforms**: `WAVEFORM_SQUARE`, `WAVEFORM_TRIANGLE`, `WAVEFORM_SAWTOOTH`, `WAVEFORM_NOISE`, `WAVEFORM_PCM`

**Usage:**
```pascal
SetWaveform(0, WAVEFORM_SQUARE);
```

### 11.2 Sound Effects

#### PlaySFX

**Syntax:**
```pascal
procedure PlaySFX(id: byte);
```

**Purpose**: Play sound effect by ID.

**Usage:**
```pascal
PlaySFX(SFX_JUMP);
```

#### StopSFX

**Syntax:**
```pascal
procedure StopSFX(id: byte);
```

**Purpose**: Stop playing sound effect.

**Usage:**
```pascal
StopSFX(SFX_JUMP);
```

#### IsSFXPlaying

**Syntax:**
```pascal
function IsSFXPlaying(id: byte): boolean;
```

**Purpose**: Check if sound effect is currently playing.

**Usage:**
```pascal
if IsSFXPlaying(SFX_JUMP) then
  // SFX still playing
```

### 11.3 Music Playback

#### PlayMusic

**Syntax:**
```pascal
procedure PlayMusic(track: PMusicTrack);
```

**Purpose**: Start playing music track.

**Usage:**
```pascal
PlayMusic(@ThemeMusic);
```

#### StopMusic

**Syntax:**
```pascal
procedure StopMusic;
```

**Purpose**: Stop current music playback.

**Usage:**
```pascal
StopMusic;
```

#### PauseMusic

**Syntax:**
```pascal
procedure PauseMusic(pause: boolean);
```

**Purpose**: Pause or resume music playback.

**Usage:**
```pascal
PauseMusic(true);   // Pause
PauseMusic(false);  // Resume
```

#### MusicFadeOut

**Syntax:**
```pascal
procedure MusicFadeOut(frames: word);
```

**Purpose**: Fade out music over specified frames.

**Usage:**
```pascal
MusicFadeOut(60);  // Fade over 1 second at 60 FPS
```

#### MusicFadeIn

**Syntax:**
```pascal
procedure MusicFadeIn(frames: word);
```

**Purpose**: Fade in music over specified frames.

**Usage:**
```pascal
MusicFadeIn(60);
```

#### Crossfade

**Syntax:**
```pascal
procedure Crossfade(newTrack: PMusicTrack; frames: word);
```

**Purpose**: Crossfade from current music to new track.

**Usage:**
```pascal
Crossfade(@NewTheme, 120);  // Crossfade over 2 seconds
```

### 11.4 Audio Streaming

#### StreamMusicStart

**Syntax:**
```pascal
procedure StreamMusicStart(id: byte);
```

**Purpose**: Start streaming music from asset bundle.

**Usage:**
```pascal
StreamMusicStart(STREAM_NARRATION);
```

#### StreamMusicStop

**Syntax:**
```pascal
procedure StreamMusicStop;
```

**Purpose**: Stop audio streaming.

**Usage:**
```pascal
StreamMusicStop;
```

#### StreamMusicUpdate

**Syntax:**
```pascal
procedure StreamMusicUpdate;
```

**Purpose**: Update streaming buffers (call every frame).

**Usage:**
```pascal
StreamMusicUpdate;  // In game loop
```

#### IsStreaming

**Syntax:**
```pascal
function IsStreaming: boolean;
```

**Purpose**: Check if audio is currently streaming.

**Usage:**
```pascal
while IsStreaming do
  WaitVBlank;
```

### 11.5 Sample Playback

#### PlaySample

**Syntax:**
```pascal
procedure PlaySample(channel: byte; data: pointer; size: word);
```

**Purpose**: Play PCM sample on audio channel.

**Usage:**
```pascal
PlaySample(0, @SampleData, SizeOf(SampleData));
```

#### PlaySampleLoop

**Syntax:**
```pascal
procedure PlaySampleLoop(channel: byte; data: pointer; size: word);
```

**Purpose**: Play PCM sample with looping.

**Usage:**
```pascal
PlaySampleLoop(0, @AmbientLoop, SizeOf(AmbientLoop));
```

---

**See also:**
- [Audio Intrinsics](./04_AudioIntrinsics.md)
- [Audio System Specification](../10_AudioSystem.md)

