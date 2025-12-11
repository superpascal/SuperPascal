# Chapter 20: Audio Programming

**Learning Objectives:**
- Understand audio hardware and channels
- Learn to play sound effects (SFX)
- Master music track playback
- Understand audio streaming and mixing
- Integrate audio into games

**SuperPascal Features Covered:**
- Audio intrinsics (`PlaySFX`, `PlayMusic`, `SetTone`, etc.)
- Audio types (`TSoundEffect`, `TMusicTrack`, `TAudioChannel`)
- Volume control and mixing
- Audio streaming
- Channel management

**Prerequisites:**
- Chapter 17 (Game Loop and Time-Based Programming) - Understanding frame updates
- Chapter 16 (Input, State, and Interaction) - Triggering audio on input
- Chapter 21 (Mathematics for Graphics and Games) - Understanding frequencies and waves

**Estimated Time:** 2-3 hours

**Chapter Size:** 3 H2 sections, ~30-35 pages total

**Code Examples:**
- Playing sound effects
- Music track playback
- Volume control
- Audio streaming
- Channel mixing

**Exercises:**
- Implement sound effects system
- Create music playback
- Build audio manager
- Integrate audio into game

---

## Chapter Structure

- **01_PlayingSounds.md** - Sound effects, SFX system, channel management, priority
- **02_MusicTracks.md** - Music tracks, pattern playback, tempo, effects, looping
- **03_StreamingAndMixers.md** - Audio streaming, channel mixing, volume control, ducking

---

## Learning Path

- **Before:** Chapter 21 (Mathematics for Graphics and Games) - Understanding frequencies
- **After:** Chapter 23 (Scripting and Events) - Audio scripting commands

---

## Notes

**Audio System Overview:**
- **8 audio channels** — Total channels available
- **4 music channels** — Reserved for music playback
- **4 SFX channels** — Available for sound effects
- **Priority system** — Higher priority SFX override lower priority
- **Volume control** — Master, music, and SFX volume

**Audio Types:**
- **Sound Effects (SFX)** — Short audio clips, triggered by events
- **Music Tracks** — Pattern-based music (tracker-style)
- **Streaming Audio** — Long-form PCM audio for cutscenes/narration

**Platform Support:**
- **ZealZ80** — ZVB audio hardware
- **Foenix** — Platform-specific audio hardware
- **All platforms** — Unified audio API

**Educational Approach:**
- **Practical focus** — Audio applied to game development
- **Code examples** — Implement audio systems in SuperPascal
- **Integration** — Connect audio to game events
- **Performance** — Efficient audio management

---

**Language Specification:** See [Audio System](../../languageSpecification/10_AudioSystem.md)  
**Last Updated:** 2025-01-XX

