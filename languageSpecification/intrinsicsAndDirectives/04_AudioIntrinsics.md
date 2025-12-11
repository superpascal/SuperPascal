# Audio Intrinsics

**Part of:** [06_IntrinsicsAndDirectives.md](../06_IntrinsicsAndDirectives.md)

---

### 4.1 FadeMusic

**Syntax:**
```pascal
procedure FadeMusic(Channel: byte; TargetVolume: byte; Duration: word);
```

**Purpose**: Fade music channel volume.

**Parameters:**
- `Channel`: Audio channel (0-3)
- `TargetVolume`: Target volume (0-255)
- `Duration`: Fade duration in frames

**Usage:**
```pascal
FadeMusic(0, 128, 60);  // Fade channel 0 to 50% over 1 second
```

### 4.2 Crossfade

**Syntax:**
```pascal
procedure Crossfade(FromChannel, ToChannel: byte; Duration: word);
```

**Purpose**: Crossfade between two audio channels.

**Parameters:**
- `FromChannel`: Source channel
- `ToChannel`: Destination channel
- `Duration`: Crossfade duration in frames

**Usage:**
```pascal
Crossfade(0, 1, 120);  // Crossfade from channel 0 to 1 over 2 seconds
```

---

**See also:**
- [Extended Audio Intrinsics](./11_ExtendedAudioIntrinsics.md) for complete audio functionality

