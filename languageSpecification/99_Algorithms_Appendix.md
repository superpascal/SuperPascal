# SuperPascal Algorithms Appendix

## Overview

This appendix provides a comprehensive reference for algorithms implemented in SuperPascal. These algorithms are **generic** and work on **all platforms** (ZealZ80, CommanderX16, Foenix65C816, FoenixA2560M, Raspberry Pi 5).

**Source Material:** Algorithms are derived from the Mikro Documentation Archive and adapted for SuperPascal.

**Educational Content:** For detailed explanations and examples, see the relevant book chapters.

**File Organization:** Each algorithm category is documented in its own file to keep the documentation manageable. See the table of contents below.

---

## Table of Contents

1. [Fixed-Point Arithmetic](./algorithms/01_FixedPointArithmetic.md)
   - Conversion algorithms
   - Arithmetic operations (add, subtract, multiply, divide)
   - Polynomial evaluation
   - Trigonometry lookup tables
   - Performance characteristics

2. [Mathematical Algorithms](./algorithms/02_MathematicalAlgorithms.md)
   - Matrix math
   - Trigonometry
   - Square root
   - Vector math

3. [Sorting Algorithms](./algorithms/03_SortingAlgorithms.md)
   - Quicksort
   - Shellsort
   - Mergesort
   - Heapsort
   - Algorithm comparison

4. [Graphics Algorithms](./algorithms/04_GraphicsAlgorithms.md)
   - Polygon rendering
   - Texture mapping
   - Tilemaps
   - Line drawing
   - Circle drawing

5. [Collision Detection](./algorithms/05_CollisionDetection.md)
   - AABB (Axis-Aligned Bounding Box)
   - Circle collision
   - Polygon clipping
   - Spatial partitioning

6. [Physics Simulation](./algorithms/06_PhysicsSimulation.md)
   - Particle systems
   - Gravity
   - Velocity and acceleration
   - Friction

7. [Advanced Graphics Algorithms](./algorithms/07_AdvancedGraphics.md)
   - 3D rotation and shading
   - Bump mapping
   - Environment mapping
   - Shadows
   - Transparency
   - Voxel rendering
   - Ray tracing
   - Special effects (fire, water, tunnel)

8. [Game Algorithms](./algorithms/08_GameAlgorithms.md)
   - Camera systems
   - Line of sight
   - BSP trees
   - Pathfinding
   - Game state management
   - Random number generation

9. [Utility Algorithms](./algorithms/09_UtilityAlgorithms.md)
   - Data compression
   - CRC error detection
   - Random number generation
   - Data packing
   - Radix operations

---

## Quick Reference

### Fixed-Point Arithmetic
- **File:** [01_FixedPointArithmetic.md](./algorithms/01_FixedPointArithmetic.md)
- **Source:** `docs/mikro_docs_archive/Coding/2/FIXEDPOI.TXT`
- **Key Topics:** Q8.8, Q12.12, Fixed16, Fixed32 formats, arithmetic operations, lookup tables

### Mathematical Algorithms
- **File:** [02_MathematicalAlgorithms.md](./algorithms/02_MathematicalAlgorithms.md)
- **Key Topics:** Matrix operations, trigonometry, square root, vector math

### Sorting Algorithms
- **File:** [03_SortingAlgorithms.md](./algorithms/03_SortingAlgorithms.md)
- **Source:** `docs/mikro_docs_archive/Coding/1/SORT_ALG.TXT`
- **Key Topics:** Quicksort, Shellsort, Mergesort, Heapsort

### Graphics Algorithms
- **File:** [04_GraphicsAlgorithms.md](./algorithms/04_GraphicsAlgorithms.md)
- **Key Topics:** Polygon rendering, texture mapping, tilemaps, line/circle drawing

### Collision Detection
- **File:** [05_CollisionDetection.md](./algorithms/05_CollisionDetection.md)
- **Key Topics:** AABB, circle collision, polygon clipping, spatial partitioning

### Physics Simulation
- **File:** [06_PhysicsSimulation.md](./algorithms/06_PhysicsSimulation.md)
- **Key Topics:** Particle systems, gravity, velocity, acceleration, friction

### Advanced Graphics Algorithms
- **File:** [07_AdvancedGraphics.md](./algorithms/07_AdvancedGraphics.md)
- **Key Topics:** 3D shading, bump mapping, environment mapping, shadows, special effects

### Game Algorithms
- **File:** [08_GameAlgorithms.md](./algorithms/08_GameAlgorithms.md)
- **Key Topics:** Camera systems, line of sight, BSP trees, pathfinding

### Utility Algorithms
- **File:** [09_UtilityAlgorithms.md](./algorithms/09_UtilityAlgorithms.md)
- **Key Topics:** Compression, CRC, random numbers, data packing

---

## Platform Support

All algorithms in this appendix are **generic** and work on all SuperPascal platforms:

- **ZealZ80** (Zilog Z80 @ 10 MHz, 8-bit)
- **CommanderX16** (WDC 65C02S @ 8 MHz, 8-bit)
- **Foenix65C816** (WDC W65C816S @ 6.29 MHz, 16-bit)
- **FoenixA2560M** (MC68LC060 @ 66 MHz, 32-bit)
- **Raspberry Pi 5** (ARM Cortex-A76 @ 2.4 GHz, 64-bit)

Platform-specific optimizations are documented within each algorithm file where applicable.

---

## Related Documentation

- **Book Chapters:**
  - [Chapter 19: Mathematics for Graphics and Games](../../book/19_MathematicsForGraphicsAndGames/README.md)
  - [Chapter 27: Advanced Algorithms](../../book/27_AdvancedAlgorithms/README.md)
  
- **Language Specification:**
  - [Type System - Fixed-Point Types](./03_TypeSystem.md#fixed-point-types)
  - [Standard Library - Math Functions](./13_StandardLibrary.md#math-functions)

- **Source Material:**
  - Mikro Documentation Archive: `docs/mikro_docs_archive/`

---

**Last Updated:** 2025-01-XX  
**Source:** Mikro Documentation Archive (`docs/mikro_docs_archive/`)

