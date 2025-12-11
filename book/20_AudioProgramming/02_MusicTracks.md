# Music Tracks

**Part of:** [Chapter 20: Audio Programming](./README.md)

---

## Introduction

Music creates atmosphere and emotion in games. This section teaches you how to play music tracks, control playback, and create pattern-based music.

**Key concepts:**
- **Music tracks** — Pattern-based music (tracker-style)
- **Patterns** — Musical sequences
- **Tempo and speed** — Playback timing
- **Music control** — Play, stop, pause, fade
- **Looping** — Continuous playback

---

## Understanding Music Tracks

### What is a Music Track?

**A music track is a pattern-based music format:**

- **Patterns** — Musical sequences (like sheet music)
- **Order list** — Sequence of patterns to play
- **Tempo** — Speed of playback (BPM)
- **Speed** — Rows per step (granularity)

**Tracker-style format:**
- Similar to MOD/S3M/XM formats
- Efficient for retro platforms
- Small file size
- Real-time playback

### Music Track Structure

**Music track data structure:**

```pascal
type
  TMusicTrack = record
    Patterns: ^TPattern;      // Array of patterns
    PatternCount: byte;       // Number of patterns
    Order: ^byte;             // Playback order list
    OrderLength: byte;        // Length of order list
    Tempo: byte;              // Tempo (BPM / 10)
    Speed: byte;              // Rows per step
  end;
  
  PMusicTrack = ^TMusicTrack;
```

**Pattern structure:**
```pascal
type
  TPattern = record
    Rows: array[0..63] of TMusicRow;  // 64 rows per pattern
  end;
  
  TMusicRow = record
    Note: word;        // Frequency lookup index (0 = no note)
    Volume: byte;      // Volume (0-255)
    Effect: byte;      // Effect command
    EffectParam: byte; // Effect parameter
  end;
```

---

## Playing Music

### Basic Music Playback

**Play a music track:**

```pascal
var
  ThemeMusic: TMusicTrack;
  
begin
  // Initialize music track (load from data)
  // ... setup ThemeMusic ...
  
  // Play music
  PlayMusic(@ThemeMusic);
end;
```

**How it works:**
- `PlayMusic` starts playback immediately
- Uses channels 0-3 (music channels)
- Plays patterns in order list
- Loops when order list completes

### Stopping Music

**Stop music playback:**

```pascal
StopMusic;
```

**When to stop:**
- Scene transitions
- Game state changes
- User requests

### Pausing Music

**Pause/resume music:**

```pascal
PauseMusic(true);   // Pause
PauseMusic(false);  // Resume
```

**Use cases:**
- Pause menu
- Dialog boxes
- Temporary interruptions

### Checking Music Status

**Check if music is playing:**

```pascal
if IsMusicPlaying then
  WriteLn('Music is playing');
```

**Use cases:**
- Wait for music to finish
- Check playback state
- Conditional logic

---

## Music Control

### Tempo Control

**Set playback tempo:**

```pascal
procedure SetMusicTempo(tempo: byte);
```

**Tempo format:**
- `tempo` — BPM / 10 (e.g., 120 BPM = 12)
- Higher value = faster playback
- Lower value = slower playback

**Example:**
```pascal
SetMusicTempo(12);  // 120 BPM
SetMusicTempo(15);  // 150 BPM (faster)
SetMusicTempo(10);  // 100 BPM (slower)
```

### Speed Control

**Set playback speed:**

```pascal
procedure SetMusicSpeed(speed: byte);
```

**Speed format:**
- `speed` — Rows per step
- Higher value = slower updates
- Lower value = faster updates

**Example:**
```pascal
SetMusicSpeed(6);   // 6 rows per step (normal)
SetMusicSpeed(3);   // 3 rows per step (faster updates)
SetMusicSpeed(12);  // 12 rows per step (slower updates)
```

---

## Music Fading

### Fade Out

**Fade music out over time:**

```pascal
procedure MusicFadeOut(frames: word);
```

**Parameters:**
- `frames` — Number of frames for fade (60 frames ≈ 1 second at 60 FPS)

**Example:**
```pascal
// Fade out over 1 second (60 frames)
MusicFadeOut(60);
```

**Use cases:**
- Smooth transitions
- Scene changes
- Ending music

### Fade In

**Fade music in over time:**

```pascal
procedure MusicFadeIn(frames: word);
```

**Example:**
```pascal
// Start playing and fade in
PlayMusic(@ThemeMusic);
MusicFadeIn(60);  // Fade in over 1 second
```

**Use cases:**
- Smooth music start
- Scene transitions
- Background music

### Crossfade

**Fade between two tracks:**

```pascal
procedure Crossfade(newTrack: PMusicTrack; frames: word);
```

**How it works:**
- Fades out current track
- Fades in new track
- Smooth transition

**Example:**
```pascal
// Crossfade to new music
Crossfade(@BattleMusic, 60);  // 1 second crossfade
```

---

## Creating Music Tracks

### Simple Music Track

**Create a basic music track:**

```pascal
const
  // Pattern data
  Pattern0: TPattern = (
    Rows: (
      // Row 0: C note, full volume
      (Note: 1; Volume: 255; Effect: 0; EffectParam: 0),
      // Row 1: E note
      (Note: 3; Volume: 255; Effect: 0; EffectParam: 0),
      // Row 2: G note
      (Note: 5; Volume: 255; Effect: 0; EffectParam: 0),
      // ... more rows ...
    )
  );

var
  ThemeMusic: TMusicTrack;
  PatternArray: array[0..0] of TPattern;
  OrderList: array[0..0] of byte;

procedure InitMusic;
begin
  // Setup pattern array
  PatternArray[0] := Pattern0;
  
  // Setup order list (play pattern 0)
  OrderList[0] := 0;
  
  // Initialize music track
  ThemeMusic.Patterns := @PatternArray[0];
  ThemeMusic.PatternCount := 1;
  ThemeMusic.Order := @OrderList[0];
  ThemeMusic.OrderLength := 1;
  ThemeMusic.Tempo := 12;  // 120 BPM
  ThemeMusic.Speed := 6;   // 6 rows per step
end;
```

### Note Frequencies

**Common note frequencies:**

```pascal
const
  // Note lookup table (frequency indices)
  NOTE_C4 = 1;   // Middle C
  NOTE_D4 = 2;
  NOTE_E4 = 3;
  NOTE_F4 = 4;
  NOTE_G4 = 5;
  NOTE_A4 = 6;
  NOTE_B4 = 7;
  NOTE_C5 = 8;   // Octave up
```

**Or use frequency directly:**
```pascal
// A4 = 440 Hz
(Note: 440; Volume: 255; Effect: 0; EffectParam: 0)
```

### Pattern Effects

**Common pattern effects:**

```pascal
const
  EFFECT_NONE = 0;
  EFFECT_VOLUME_SLIDE = 1;
  EFFECT_PITCH_SLIDE = 2;
  EFFECT_ARPEGGIO = 3;
  EFFECT_VIBRATO = 4;
```

**Example:**
```pascal
// Volume slide effect
(Note: 1; Volume: 255; Effect: EFFECT_VOLUME_SLIDE; EffectParam: 10)
```

---

## Practical Examples

### Background Music

**Play background music in game:**

```pascal
procedure StartGame;
begin
  // Start background music
  PlayMusic(@BackgroundMusic);
  MusicFadeIn(60);  // Fade in
end;

procedure EndGame;
begin
  // Fade out music
  MusicFadeOut(60);
  
  // Wait for fade
  while IsMusicPlaying do
    WaitVBlank;
end;
```

### Scene Music Transitions

**Change music for different scenes:**

```pascal
procedure ChangeScene(newScene: TScene);
begin
  case newScene of
    SCENE_MENU:
      Crossfade(@MenuMusic, 60);
    SCENE_GAME:
      Crossfade(@GameMusic, 60);
    SCENE_BATTLE:
      Crossfade(@BattleMusic, 30);  // Faster transition
  end;
end;
```

### Pause Menu Music

**Pause music when menu opens:**

```pascal
procedure OpenPauseMenu;
begin
  PauseMusic(true);  // Pause music
  // ... show menu ...
end;

procedure ClosePauseMenu;
begin
  PauseMusic(false);  // Resume music
  // ... hide menu ...
end;
```

### Dynamic Music

**Change music based on game state:**

```pascal
procedure UpdateMusic;
begin
  if playerHealth < 25 then
  begin
    // Low health - play tense music
    if not IsMusicPlaying or (currentMusic <> @TenseMusic) then
      Crossfade(@TenseMusic, 30);
    currentMusic := @TenseMusic;
  end
  else if enemyCount > 5 then
  begin
    // Many enemies - play battle music
    if not IsMusicPlaying or (currentMusic <> @BattleMusic) then
      Crossfade(@BattleMusic, 30);
    currentMusic := @BattleMusic;
  end
  else
  begin
    // Normal - play background music
    if not IsMusicPlaying or (currentMusic <> @BackgroundMusic) then
      Crossfade(@BackgroundMusic, 60);
    currentMusic := @BackgroundMusic;
  end;
end;
```

---

## Complete Music Example

**Putting it all together:**

```pascal
program MusicDemo;

const
  // Simple melody pattern
  MelodyPattern: TPattern = (
    Rows: (
      (Note: 1; Volume: 200; Effect: 0; EffectParam: 0),  // C
      (Note: 0; Volume: 0; Effect: 0; EffectParam: 0),     // Rest
      (Note: 3; Volume: 200; Effect: 0; EffectParam: 0),  // E
      (Note: 0; Volume: 0; Effect: 0; EffectParam: 0),     // Rest
      (Note: 5; Volume: 200; Effect: 0; EffectParam: 0),  // G
      (Note: 0; Volume: 0; Effect: 0; EffectParam: 0),     // Rest
      (Note: 8; Volume: 200; Effect: 0; EffectParam: 0),  // C (octave)
      // ... more rows ...
    )
  );

var
  GameMusic: TMusicTrack;
  PatternArray: array[0..0] of TPattern;
  OrderList: array[0..0] of byte;
  gameState: TGameState;

procedure InitMusic;
begin
  PatternArray[0] := MelodyPattern;
  OrderList[0] := 0;
  
  GameMusic.Patterns := @PatternArray[0];
  GameMusic.PatternCount := 1;
  GameMusic.Order := @OrderList[0];
  GameMusic.OrderLength := 1;
  GameMusic.Tempo := 12;  // 120 BPM
  GameMusic.Speed := 6;
end;

procedure HandleMusic;
begin
  case gameState of
    STATE_MENU:
      if not IsMusicPlaying then
      begin
        PlayMusic(@GameMusic);
        MusicFadeIn(60);
      end;
      
    STATE_PAUSED:
      PauseMusic(true);
      
    STATE_PLAYING:
      PauseMusic(false);
      
    STATE_GAME_OVER:
      MusicFadeOut(60);
  end;
end;

begin
  InitGraphics;
  InitMusic;
  gameState := STATE_MENU;
  
  while true do
  begin
    HandleInput;
    HandleMusic;
    UpdateGame;
    RenderGame;
    WaitVBlank;
  end;
end.
```

---

## Best Practices

### 1. Use Fading for Transitions

**Smooth music transitions:**

```pascal
// ✅ GOOD: Fade between tracks
Crossfade(@NewMusic, 60);

// ❌ BAD: Abrupt change
StopMusic;
PlayMusic(@NewMusic);  // Jarring
```

### 2. Pause Music Appropriately

**Pause when appropriate:**

```pascal
// ✅ GOOD: Pause for menus
procedure OpenMenu;
begin
  PauseMusic(true);
end;

// ❌ BAD: Music continues during menu
procedure OpenMenu;
begin
  // Music still playing
end;
```

### 3. Match Music to Game State

**Change music based on context:**

```pascal
// ✅ GOOD: Dynamic music
if enemyCount > 5 then
  Crossfade(@BattleMusic, 30);

// ❌ BAD: Same music always
// Music never changes
```

### 4. Use Appropriate Tempo

**Match tempo to game feel:**

```pascal
// ✅ GOOD: Fast tempo for action
SetMusicTempo(15);  // 150 BPM

// ❌ BAD: Slow tempo for action
SetMusicTempo(8);   // 80 BPM (too slow)
```

### 5. Loop Music Seamlessly

**Ensure music loops properly:**

```pascal
// ✅ GOOD: Seamless loop
// Order list returns to start
OrderList: array[0..3] of byte = (0, 1, 2, 0);

// ❌ BAD: Abrupt loop
// No smooth transition
```

---

## Exercises

### Exercise 1: Basic Music Playback

Write a program that:
1. Creates a simple music track
2. Plays the music
3. Stops and pauses music
4. Demonstrates basic music control

### Exercise 2: Music Transitions

Write a program that:
1. Plays multiple music tracks
2. Transitions between tracks with fade
3. Uses crossfade for smooth transitions
4. Demonstrates music state management

### Exercise 3: Dynamic Music

Write a program that:
1. Changes music based on game state
2. Uses different tracks for different situations
3. Implements smooth transitions
4. Demonstrates context-aware music

### Exercise 4: Music System

Write a program that:
1. Creates a complete music management system
2. Handles music playback, pausing, fading
3. Integrates with game state
4. Manages multiple music tracks

---

**Previous Section:** [Playing Sounds](./01_PlayingSounds.md)  
**Next Section:** [Streaming and Mixers](./03_StreamingAndMixers.md)  
**Language Specification:** See [Audio System](../../languageSpecification/10_AudioSystem.md)  
**Last Updated:** 2025-01-XX

