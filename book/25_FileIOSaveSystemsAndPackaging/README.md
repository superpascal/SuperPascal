# Chapter 25: File I/O, Save Systems, and Packaging

**Learning Objectives:**
- Understand file I/O operations
- Learn to implement save slot systems
- Master asset bundle creation and loading
- Load maps and sprites from files
- Package games for distribution

**SuperPascal Features Covered:**
- File I/O operations
- Save slot management
- Asset bundles (.ZPK format)
- Resource loading
- Data serialization

**Prerequisites:**
- Chapter 08 (Arrays and Records) - Understanding data structures
- Chapter 18 (Entity Component System) - Understanding game state
- Chapter 25 (Scenes, UI, and Game Architecture) - Scene management

**Estimated Time:** 2-3 hours

**Chapter Size:** 3 H2 sections, ~30-35 pages total

**Code Examples:**
- Save slot implementation
- Asset bundle creation
- Map and sprite loading
- File I/O operations

**Exercises:**
- Create save system
- Build asset bundles
- Load resources from files
- Package complete game

---

## Chapter Structure

- **01_SaveSlots.md** - Save slot system, serialization, loading, save data structure
- **02_AssetBundles.md** - Asset bundle format (.ZPK), creating bundles, loading bundles
- **03_LoadingMapsAndSprites.md** - Loading tilemaps, loading sprites, resource management

---

## Learning Path

- **Before:** Chapter 26 (Debugging, Profiling, and Optimization) - Optimized game systems
- **After:** Chapter 28 (Hardware Interfacing) - External hardware communication

---

## Notes

**File I/O Overview:**
- **Save slots** — Multiple save game files
- **Asset bundles** — Packaged game resources
- **Resource loading** — Loading assets at runtime
- **Serialization** — Converting data to/from files

**Save System:**
- **Save slots** — Multiple save files (slot 1, 2, 3, etc.)
- **Save data** — Game state, progress, settings
- **Serialization** — Convert game state to bytes
- **Loading** — Restore game state from file

**Asset Bundles:**
- **.ZPK format** — SuperPascal asset package
- **Contains** — Sprites, tilemaps, audio, data
- **Compressed** — Efficient storage
- **Loadable** — Load at runtime

**Resource Loading:**
- **Maps** — Load tilemap data
- **Sprites** — Load sprite sheets
- **Audio** — Load sound effects and music
- **Data** — Load game configuration

**Educational Approach:**
- **Practical focus** — File I/O applied to game development
- **Code examples** — Implement save/load systems in SuperPascal
- **Data structures** — Organize save data effectively
- **Error handling** — Handle file I/O errors gracefully

---

**Language Specification:** See [Memory and I/O](../../languageSpecification/12_MemoryAndIO.md)  
**Last Updated:** 2025-01-XX

