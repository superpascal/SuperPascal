# 📘 **SuperPascal Language & Toolchain**

SuperPascal is a modern, educational Pascal superset compiler and game-development toolchain supporting multiple retro computing platforms, including the **Zeal 8-bit Computer** (ZealZ80 @ 10 MHz), **Foenix F256x systems** (WDC65C816S @ 6.29 MHz, 16-bit microprocessor with 24-bit addressing), and **Foenix A2560M** (MC68LC060 @ 66 MHz, 32-bit system with 1GB DDR3).
This roadmap defines the engineering plan from **initial compiler implementation** to the **v1.0 educational release**.

This roadmap replaces earlier self-hosting ideas.
**The production compiler is written in Rust and cross-compiles to multiple 8-bit platforms.**

---

# 🎯 **Project Principles**

* **Rust as the production compiler language**
* **Cross-compiler targeting multiple 8-bit platforms** (not self-hosted)
* **Safe, simple, readable Pascal dialect**
* **Integrated STEM and game-dev capabilities**
* **Offline-first educational platform**
* **Complete formal specification stack** (grammar, semantics, ABI)

---

# 🚀 **PHASE 0 — Foundations & Infrastructure**

### *Set up the repo and ensure Zeal dev environment integration.*

### Tasks

* [ ] Clone & validate: [https://github.com/zoul0813/zeal-dev-environment](https://github.com/zoul0813/zeal-dev-environment)
* [ ] Create repo structure:

  ```
  /compiler-rs
  /stdlib
  /runtime
  /engine
  /tests
  /docs
  /examples
  ```
* [ ] Establish build scripts:

    * `zc` → Z80 assembly
    * assemble → `.bin`
    * package into ZealOS filesystem / SD image
* [ ] Create CI for:

    * `cargo build`
    * language spec linting
    * emulator smoke test

### Deliverable

**Working Zeal dev pipeline** for assembling and running trivial programs.

---

# 🚀 **PHASE 1 — Minimal Rust Compiler (MVP)**

### *The smallest useful SuperPascal compiler.*

### Language Subset

* integer, boolean
* assignments
* if/else
* while
* begin/end blocks
* simple procedures/functions

### Compiler Tasks

* [ ] Tokenizer
* [ ] Parser (core syntax)
* [ ] Symbol table (simple)
* [ ] Linear IR (IR1)
* [ ] Z80 backend (minimal ABI subset)
* [ ] Tiny runtime: entrypoint, MUL/DIV helpers

### Deliverable

`zc` compiles a simple Pascal program (“Hello Zeal”) and runs on emulator.

---

# 🚀 **PHASE 2 — Full Core Language (Tier 1)**

### *Implement all classic Pascal constructs.*

### Features

* [ ] for/downto loops
* [ ] repeat/until
* [ ] case statements
* [ ] return semantics
* [ ] nested procedure scoping
* [ ] improved expression parser

### Compiler Tasks

* [ ] Full recursive descent parser
* [ ] Type checker for all primitive constructs
* [ ] Proper ABI integration (stack frames, IX, return rules)

### Deliverable

Core Pascal programs compile reliably with correct ABI behavior.

---

# 🚀 **PHASE 3 — Data Structures & Modular Programming (Tier 2)**

### Features

* [ ] Records (structs with methods)
* [ ] Static arrays
* [ ] Sets
* [ ] Pointers (`^T`)
* [ ] Units (`interface/implementation`)
* [ ] Initialization sections

### Compiler Tasks

* [ ] Record layout generation
* [ ] Array indexing semantics
* [ ] Unit file generation: `.ZOF` + `.ZOU`
* [ ] Unified shortstring implementation

### Deliverable

`zc` builds the **SuperPascal Standard Library**.

---

# 🚀 **PHASE 4 — OOP Layer (Hybrid Model)**

### *Lightweight classes for orchestration (not data storage).*

### Features

* [ ] Classes
* [ ] Single inheritance
* [ ] Constructors/destructors
* [ ] Virtual/override methods
* [ ] VTables (compact, ABI-specified)

### Compiler Tasks

* [ ] Implicit `Self` handling
* [ ] VTable generation
* [ ] Slot assignment logic
* [ ] Virtual dispatch lowering

### Deliverable

`zc` builds the **SuperPascal Engine** (scene, scripting, UI classes).

---

# 🚀 **PHASE 5 — Exceptions**

### *Structured error handling for robust programs.*

### Features

* [ ] try/except
* [ ] try/finally
* [ ] raise
* [ ] EError integration

### Runtime Tasks

* [ ] Exception frame chain
* [ ] Unwind mechanism (restore SP/IX)

### Deliverable

Exception tests from compliance suite pass.

---

# 🚀 **PHASE 6 — Full Standard Library & Engine Integration**

### Standard Library Units

* System
* Math / MathFP / MathTrig / MathFloat
* Strings
* SysUtils_Lite

### ZVB Hardware Units

* ZVB_Graphics
* ZVB_Sprites
* ZVB_Audio
* ZVB_Input

### Engine Units

* Engine_ECS
* Engine_Scene
* Engine_Physics
* Script Engine
* UI Widgets

### Deliverable

A runnable library + engine ecosystem used by curriculum and example projects.

---

# 🚀 **PHASE 7 — Optimization & Z80 Performance Tuning**

### Compiler Optimizations

* [ ] Peephole optimizer
* [ ] Constant folding & propagation
* [ ] Strength reduction
* [ ] ECS loop unrolling
* [ ] Induction variable optimizations

### Performance Targets

* 60 FPS with 100–200 ECS entities
* Smooth animation & audio
* Efficient tilemap rendering

### Deliverable

`zc` codegen is fast enough for games and simulations.

---

# 🚀 **PHASE 8 — ZealIDE Integration & Developer Experience**

### IDE Features

* [ ] Compiler invocation
* [ ] Error navigation
* [ ] Syntax highlighting
* [ ] Debug info support
* [ ] Project templates

### Deliverable

Teachers and students use ZealIDE seamlessly with `zc`.

---

# 🚀 **PHASE 9 — v1.0 Educational Toolkit Release**

### Includes

* Rust cross-compiler binaries
* Standard Library
* Game Engine
* Documentation bundle

    * Language Reference
    * Teacher Guide
    * Curriculum
    * Student Book
* Example projects
* Test suite
* SD card image for Zeal

### Deliverable

**SuperPascal v1.0** ready for deployment into schools and university CS foundations.

---

# 🚀 **PHASE 10 — Optional: University Teaching Compiler**

### *(Separate from production compiler)*

A small SuperPascal-written compiler used to teach:

* parsing
* type systems
* codegen fundamentals

Not part of the production toolchain.

---

# ✔️ Long-Term Vision

SuperPascal becomes:

* A global offline-first CS education platform
* A powerful retro game development ecosystem
* A gateway to deeper STEM learning
* A stable, well-documented language for decades

