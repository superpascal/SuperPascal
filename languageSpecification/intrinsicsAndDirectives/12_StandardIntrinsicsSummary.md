# Standard Intrinsics Summary

**Part of:** [06_IntrinsicsAndDirectives.md](../06_IntrinsicsAndDirectives.md)

---

| Category | Intrinsic | Purpose |
|----------|-----------|---------|
| Graphics | `WaitVBlank` | Frame synchronization |
| Graphics | `WriteReg` | ZVB register write |
| Graphics | `DMA_Copy` | Bulk memory copy |
| Graphics | `SpriteSet` | Configure sprite |
| Graphics | `SpriteShow` | Show/hide sprite |
| ZVB | `ReadReg` | Read ZVB I/O register |
| ZVB | `ZVB_MapPeripheral` | Map peripheral to I/O space |
| ZVB | `ZVB_SetVideoMode` | Set video mode |
| ZVB | `ZVB_EnableScreen` | Enable/disable screen |
| ZVB | `ZVB_GetVPos` | Get vertical raster position |
| ZVB | `ZVB_GetHPos` | Get horizontal raster position |
| ZVB | `ZVB_SetLayer0Scroll` | Set layer 0 scroll |
| ZVB | `ZVB_SetLayer1Scroll` | Set layer 1 scroll |
| ZVB | `ZVB_IsHBlank` | Check H-blank status |
| ZVB | `ZVB_IsVBlank` | Check V-blank status |
| ZVB | `ZVB_SpriteSetFull` | Set complete sprite attributes |
| ZVB | `ZVB_SpriteSetX` | Set sprite X coordinate |
| ZVB | `ZVB_SpriteSetY` | Set sprite Y coordinate |
| ZVB | `ZVB_SpriteSetTile` | Set sprite tile |
| ZVB | `ZVB_SpriteSetFlags` | Set sprite flags |
| ZVB | `ZVB_GetVideoMemBase` | Get video memory base address |
| ZVB | `ZVB_TextPrintChar` | Print character (text mode) |
| ZVB | `ZVB_TextSetCursor` | Set text cursor position |
| ZVB | `ZVB_TextSetColor` | Set text color |
| ZVB | `ZVB_DMAStart` | Start DMA transfer |
| ZVB | `ZVB_DMA_VirtToPhys` | Convert virtual to physical address |
| ZVB | `ZVB_DMA_SetRead` | Set DMA source address |
| ZVB | `ZVB_DMA_SetReadVirt` | Set DMA source from virtual pointer |
| ZVB | `ZVB_DMA_SetWrite` | Set DMA destination address |
| ZVB | `ZVB_DMA_SetWriteVirt` | Set DMA destination from virtual pointer |
| ZVB | `ZVB_DMA_PrepareDescriptor` | Prepare DMA descriptor |
| ZVB | `ZVB_SPI_Initialize` | Initialize SPI controller |
| ZVB | `ZVB_SPI_Start` | Start SPI transaction |
| ZVB | `ZVB_SPI_IsIdle` | Check SPI idle status |
| ZVB | `ZVB_SPI_CS_Start` | Assert chip select |
| ZVB | `ZVB_SPI_CS_Stop` | Deassert chip select |
| ZVB | `ZVB_SPI_WriteByte` | Write byte to SPI |
| ZVB | `ZVB_SPI_ReadByte` | Read byte from SPI |
| ZVB | `ZVB_CRC_Initialize` | Initialize CRC32 controller |
| ZVB | `ZVB_CRC_Reset` | Reset CRC32 calculation |
| ZVB | `ZVB_CRC_Update` | Update CRC32 with buffer |
| ZVB | `ZVB_CRC_GetResult` | Get CRC32 result |
| ZVB | `ZVB_Sound_Initialize` | Initialize sound controller |
| ZVB | `ZVB_Sound_Reset` | Reset sound voices |
| ZVB | `ZVB_Sound_SetFrequency` | Set voice frequency |
| ZVB | `ZVB_Sound_SetWaveform` | Set voice waveform |
| ZVB | `ZVB_Sound_SetVolume` | Set voice volume |
| ZVB | `ZVB_Sound_SetMasterVolume` | Set master volume |
| ZVB | `ZVB_Sound_SetChannels` | Assign voices to channels |
| ZVB | `ZVB_Sound_SetHold` | Hold/unhold voices |
| ZVB | `ZVB_Sound_PlaySamples` | Play PCM samples |
| ZVB | `ZVB_Sound_IsSampleReady` | Check sample ready status |
| ZVB | `ZVB_Sound_IsSampleFull` | Check sample FIFO full |
| Memory | `MapPage` | MMU page mapping |
| Memory | `Peek` | Read byte from memory (PEEK) |
| Memory | `PeekW` | Read word from memory |
| Memory | `Poke` | Write byte to memory (POKE) |
| Memory | `PokeW` | Write word to memory |
| I/O | `PortIn` | Read byte from I/O port |
| I/O | `PortInW` | Read word from I/O port |
| I/O | `PortOut` | Write byte to I/O port |
| I/O | `PortOutW` | Write word to I/O port |
| Audio | `SetTone` | Set channel frequency/volume |
| Audio | `SetWaveform` | Set channel waveform |
| Audio | `PlaySFX` | Play sound effect |
| Audio | `StopSFX` | Stop sound effect |
| Audio | `PlayMusic` | Play music track |
| Audio | `StopMusic` | Stop music |
| Audio | `MusicFadeOut` | Fade out music |
| Audio | `MusicFadeIn` | Fade in music |
| Audio | `Crossfade` | Crossfade music tracks |
| Audio | `StreamMusicStart` | Start audio streaming |
| Audio | `StreamMusicUpdate` | Update streaming buffers |
| Audio | `PlaySample` | Play PCM sample |
| Input | `ReadInput` | Read input state |
| Video | `ClearScreen` | Clear screen |
| Game Engine | `EntityCreate` | Create entity |
| Game Engine | `EntityDestroy` | Destroy entity |
| Game Engine | `EntitySetPosition` | Set entity position |
| Game Engine | `EntitySetVelocity` | Set entity velocity |
| Game Engine | `EntitySetSprite` | Set entity sprite |
| Game Engine | `CollisionCheck` | Check entity collision |
| Game Engine | `CollisionCheckTile` | Check tile collision |
| Game Engine | `EntitySetAnimation` | Set entity animation |
| Game Engine | `AnimationUpdate` | Update animation |
| Game Engine | `PhysicsApplyGravity` | Apply gravity |
| Game Engine | `PhysicsApplyVelocity` | Apply velocity |

---

**See also:**
- Individual section files for detailed documentation
- [Intrinsics Overview](./01_Overview.md)

