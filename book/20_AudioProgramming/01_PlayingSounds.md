# Playing Sounds

**Part of:** [Chapter 20: Audio Programming](./README.md)

---

## Introduction

Sound effects bring games to life. This section teaches you how to play sound effects, manage audio channels, and create an effective SFX system.

**Key concepts:**
- **Sound effects (SFX)** — Short audio clips
- **Audio channels** — Hardware resources for playback
- **Priority system** — Managing channel conflicts
- **SFX management** — Playing, stopping, checking status

---

## Understanding Audio Channels

### What are Audio Channels?

**Audio channels are hardware resources:**

- **8 total channels** — Available on ZVB hardware
- **4 music channels** — Reserved for music playback
- **4 SFX channels** — Available for sound effects
- **One sound per channel** — Each channel plays one sound at a time

**Channel properties:**
- **Frequency** — Pitch/tone of the sound
- **Volume** — Loudness (0-255)
- **Waveform** — Shape of the wave (square, triangle, sawtooth, noise)
- **PCM samples** — Can play recorded audio samples

### Channel Allocation

**Channels are allocated automatically:**

```pascal
const
  AUDIO_CHANNELS = 8;          // Total channels
  AUDIO_MUSIC_CHANNELS = 4;    // Reserved for music
  AUDIO_SFX_CHANNELS = 4;      // Available for SFX
```

**How it works:**
- Music uses channels 0-3
- SFX uses channels 4-7
- If all SFX channels are busy, new SFX may not play (or use priority)

---

## Playing Sound Effects

### Basic SFX Playback

**Play a sound effect by ID:**

```pascal
const
  SFX_JUMP = 0;
  SFX_LAND = 1;
  SFX_COIN = 2;
  SFX_HIT = 3;

begin
  // Play jump sound
  PlaySFX(SFX_JUMP);
end;
```

**How it works:**
- `PlaySFX` finds an available SFX channel
- Loads the sound effect data
- Starts playback immediately
- Returns when sound starts (non-blocking)

### Checking if SFX is Playing

**Check if a sound effect is currently playing:**

```pascal
if IsSFXPlaying(SFX_JUMP) then
  WriteLn('Jump sound is playing');
```

**Use cases:**
- Prevent overlapping sounds
- Wait for sound to finish
- Check if sound can be played

### Stopping Sound Effects

**Stop a specific sound effect:**

```pascal
StopSFX(SFX_JUMP);
```

**Stop all sound effects:**

```pascal
StopAllSFX;
```

**When to stop:**
- When game state changes
- When transitioning scenes
- When sound should be interrupted

---

## SFX Priority System

### Understanding Priority

**Priority determines which sounds play when channels are full:**

- **Higher priority** — Overrides lower priority sounds
- **Lower priority** — May be interrupted by higher priority
- **Same priority** — First come, first served

**Priority levels:**
```pascal
const
  PRIORITY_LOW = 64;      // Background sounds
  PRIORITY_MEDIUM = 128;  // Normal sounds
  PRIORITY_HIGH = 192;    // Important sounds
  PRIORITY_CRITICAL = 255; // Must play (UI, critical events)
```

### Using Priority

**Assign priority to sound effects:**

```pascal
// In your SFX data structure
type
  TSoundEffect = record
    Data: pointer;
    Length: word;
    Priority: byte;  // Priority level
    Loop: boolean;
  end;

// When playing
procedure PlaySFXWithPriority(id: byte; priority: byte);
begin
  // Set priority before playing
  SetSFXPriority(id, priority);
  PlaySFX(id);
end;
```

**Example usage:**
```pascal
// Low priority: ambient sounds
PlaySFXWithPriority(SFX_AMBIENT, PRIORITY_LOW);

// High priority: player actions
PlaySFXWithPriority(SFX_JUMP, PRIORITY_HIGH);

// Critical: UI feedback
PlaySFXWithPriority(SFX_MENU_SELECT, PRIORITY_CRITICAL);
```

---

## Low-Level Audio Control

### Setting Tone

**Directly control channel frequency and volume:**

```pascal
procedure SetTone(channel: byte; freq: word; vol: byte);
```

**Parameters:**
- `channel` — Channel number (0-7)
- `freq` — Frequency in Hz (or lookup index)
- `vol` — Volume (0-255)

**Example:**
```pascal
// Play A4 note (440 Hz) at 50% volume on channel 4
SetTone(4, 440, 128);
```

### Setting Waveform

**Choose the waveform shape:**

```pascal
const
  WAVEFORM_SQUARE = 0;
  WAVEFORM_TRIANGLE = 1;
  WAVEFORM_SAWTOOTH = 2;
  WAVEFORM_NOISE = 3;
  WAVEFORM_PCM = 4;

procedure SetWaveform(channel: byte; waveform: byte);
```

**Example:**
```pascal
SetWaveform(4, WAVEFORM_SQUARE);  // Square wave
SetWaveform(4, WAVEFORM_TRIANGLE); // Triangle wave
SetWaveform(4, WAVEFORM_NOISE);   // Noise (for explosions, etc.)
```

### Enabling/Disabling Channels

**Control channel activity:**

```pascal
procedure ChannelEnable(channel: byte; enabled: boolean);
```

**Example:**
```pascal
ChannelEnable(4, true);   // Enable channel 4
ChannelEnable(4, false);  // Disable channel 4
```

---

## Practical Examples

### Jump Sound

**Play sound when player jumps:**

```pascal
procedure HandleJump;
begin
  if InputPressed(KEY_A) and IsOnGround then
  begin
    // Player jumps
    PlayerJump;
    
    // Play jump sound
    PlaySFX(SFX_JUMP);
  end;
end;
```

### Coin Collection

**Play sound when collecting items:**

```pascal
procedure CollectCoin(coin: TEntityID);
begin
  // Remove coin
  EntityDestroy(coin);
  
  // Update score
  score := score + 10;
  
  // Play collection sound
  PlaySFX(SFX_COIN);
end;
```

### Combat Sounds

**Play different sounds based on action:**

```pascal
procedure HandleCombat;
begin
  if InputPressed(KEY_ATTACK) then
  begin
    if IsEnemyInRange then
    begin
      // Hit enemy
      DamageEnemy;
      PlaySFX(SFX_HIT);  // Hit sound
    end
    else
    begin
      // Miss
      PlaySFX(SFX_SWING);  // Swing sound
    end;
  end;
end;
```

### UI Sounds

**Play sounds for menu interactions:**

```pascal
procedure HandleMenuInput;
begin
  if InputPressed(KEY_UP) or InputPressed(KEY_DOWN) then
  begin
    // Navigate menu
    ChangeMenuSelection;
    PlaySFX(SFX_MENU_MOVE);  // Menu navigation sound
  end
  else if InputPressed(KEY_A) then
  begin
    // Select option
    SelectMenuOption;
    PlaySFX(SFX_MENU_SELECT);  // Selection sound
  end;
end;
```

---

## SFX Management System

### Simple SFX Manager

**Create a simple SFX management system:**

```pascal
type
  TSFXManager = record
    Playing: array[0..3] of byte;  // Currently playing SFX IDs
    Count: byte;                    // Number of active SFX
  end;

var
  SFXManager: TSFXManager;

procedure InitSFXManager;
begin
  SFXManager.Count := 0;
  FillChar(SFXManager.Playing, SizeOf(SFXManager.Playing), 0);
end;

procedure PlaySFXManaged(id: byte);
var
  i: byte;
begin
  // Check if already playing
  for i := 0 to SFXManager.Count - 1 do
  begin
    if SFXManager.Playing[i] = id then
      Exit;  // Already playing, don't play again
  end;
  
  // Play sound
  PlaySFX(id);
  
  // Track it
  if SFXManager.Count < 4 then
  begin
    SFXManager.Playing[SFXManager.Count] := id;
    SFXManager.Count := SFXManager.Count + 1;
  end;
end;

procedure UpdateSFXManager;
var
  i: byte;
begin
  // Remove finished sounds
  i := 0;
  while i < SFXManager.Count do
  begin
    if not IsSFXPlaying(SFXManager.Playing[i]) then
    begin
      // Sound finished, remove from list
      SFXManager.Playing[i] := SFXManager.Playing[SFXManager.Count - 1];
      SFXManager.Count := SFXManager.Count - 1;
    end
    else
      i := i + 1;
  end;
end;
```

### SFX with Cooldown

**Prevent sound spam with cooldown:**

```pascal
type
  TSFXCooldown = record
    LastPlayTime: array[0..255] of word;  // Last play time for each SFX
    CooldownFrames: byte;                 // Cooldown in frames
  end;

var
  SFXCooldown: TSFXCooldown;
  frameCount: word;

procedure PlaySFXWithCooldown(id: byte);
begin
  // Check cooldown
  if frameCount - SFXCooldown.LastPlayTime[id] < SFXCooldown.CooldownFrames then
    Exit;  // Still on cooldown
  
  // Play sound
  PlaySFX(id);
  
  // Update cooldown
  SFXCooldown.LastPlayTime[id] := frameCount;
end;
```

---

## Complete SFX Example

**Putting it all together:**

```pascal
program SFXDemo;

const
  SFX_JUMP = 0;
  SFX_LAND = 1;
  SFX_COIN = 2;
  SFX_HIT = 3;

var
  playerY: integer;
  isOnGround: boolean;
  coins: array[0..9] of TEntityID;
  coinCount: byte;

procedure InitGame;
var
  i: byte;
begin
  playerY := 100;
  isOnGround := true;
  coinCount := 10;
  
  // Create coins
  for i := 0 to coinCount - 1 do
    coins[i] := EntityCreate;
end;

procedure HandleInput;
begin
  if InputPressed(KEY_A) and isOnGround then
  begin
    // Jump
    playerY := playerY - 50;
    isOnGround := false;
    PlaySFX(SFX_JUMP);
  end;
end;

procedure UpdatePhysics;
begin
  if not isOnGround then
  begin
    // Apply gravity
    playerY := playerY + 5;
    
    if playerY >= 100 then
    begin
      // Landed
      playerY := 100;
      isOnGround := true;
      PlaySFX(SFX_LAND);
    end;
  end;
end;

procedure CheckCoinCollection;
var
  i: byte;
  coinX, coinY: integer;
  playerX: integer;
begin
  playerX := 80;
  
  for i := 0 to coinCount - 1 do
  begin
    if EntityValid(coins[i]) then
    begin
      EntityGetPosition(coins[i], coinX, coinY);
      
      // Check collision
      if (Abs(playerX - coinX) < 16) and (Abs(playerY - coinY) < 16) then
      begin
        // Collect coin
        EntityDestroy(coins[i]);
        PlaySFX(SFX_COIN);
      end;
    end;
  end;
end;

begin
  InitGraphics;
  InitGame;
  
  while true do
  begin
    HandleInput;
    UpdatePhysics;
    CheckCoinCollection;
    RenderGame;
    WaitVBlank;
  end;
end.
```

---

## Best Practices

### 1. Use Appropriate Priority

**Assign priority based on importance:**

```pascal
// ✅ GOOD: Critical sounds have high priority
PlaySFXWithPriority(SFX_MENU_SELECT, PRIORITY_CRITICAL);

// ❌ BAD: All sounds same priority
PlaySFX(SFX_AMBIENT);  // May interrupt important sounds
```

### 2. Avoid Sound Spam

**Prevent playing same sound too frequently:**

```pascal
// ✅ GOOD: Cooldown system
PlaySFXWithCooldown(SFX_JUMP, 10);  // 10 frame cooldown

// ❌ BAD: No protection
PlaySFX(SFX_JUMP);  // Can spam
```

### 3. Stop Sounds When Appropriate

**Stop sounds when state changes:**

```pascal
// ✅ GOOD: Stop sounds on state change
procedure ChangeScene(newScene: TScene);
begin
  StopAllSFX;  // Clean slate
  // ... change scene
end;

// ❌ BAD: Sounds continue across scenes
procedure ChangeScene(newScene: TScene);
begin
  // Sounds from previous scene still playing
end;
```

### 4. Use Low-Level Control Sparingly

**Prefer high-level functions:**

```pascal
// ✅ GOOD: Use PlaySFX
PlaySFX(SFX_JUMP);

// ❌ BAD: Manual channel control (unless needed)
SetTone(4, 440, 128);
SetWaveform(4, WAVEFORM_SQUARE);
ChannelEnable(4, true);
```

### 5. Check Channel Availability

**Verify sound can play:**

```pascal
// ✅ GOOD: Check before playing
if not IsSFXPlaying(SFX_JUMP) then
  PlaySFX(SFX_JUMP);

// ❌ BAD: Always play (may fail silently)
PlaySFX(SFX_JUMP);  // May not play if channels full
```

---

## Exercises

### Exercise 1: Basic SFX

Write a program that:
1. Plays different sound effects
2. Checks if sounds are playing
3. Stops sounds appropriately
4. Demonstrates basic SFX usage

### Exercise 2: SFX Manager

Write a program that:
1. Creates an SFX management system
2. Tracks playing sounds
3. Prevents duplicate sounds
4. Manages channel allocation

### Exercise 3: Priority System

Write a program that:
1. Implements priority system
2. Demonstrates priority override
3. Shows channel conflict resolution
4. Tests different priority levels

### Exercise 4: Game Integration

Write a program that:
1. Integrates SFX into a simple game
2. Plays sounds for player actions
3. Plays sounds for game events
4. Manages SFX lifecycle

---

**Previous Chapter:** [Chapter 19: Mathematics for Graphics and Games](../19_MathematicsForGraphicsAndGames/README.md)  
**Next Section:** [Music Tracks](./02_MusicTracks.md)  
**Language Specification:** See [Audio System](../../languageSpecification/10_AudioSystem.md)  
**Last Updated:** 2025-01-XX

