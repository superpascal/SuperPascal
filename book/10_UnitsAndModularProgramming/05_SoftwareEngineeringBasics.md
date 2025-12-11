# Software Engineering Basics

**Part of:** [Chapter 10: Units and Modular Programming](./README.md)

---

## Introduction

As your projects grow, you'll need software engineering practices to manage complexity, collaborate with others, and maintain code quality. This section introduces essential software engineering concepts and practices.

**Target Levels:**
- **GCSE:** Basic project organization
- **A-Level:** Version control basics, documentation
- **University:** Full software engineering practices

---

## Project Organization

### Directory Structure

**Organize projects logically:**
```
MyGame/
├── src/              # Source code
│   ├── main.pas      # Main program
│   ├── units/        # Unit files
│   │   ├── GameLogic.pas
│   │   ├── Graphics.pas
│   │   └── Audio.pas
│   └── utils/        # Utility units
│       └── MathUtils.pas
├── assets/           # Game assets
│   ├── sprites/
│   ├── sounds/
│   └── maps/
├── docs/             # Documentation
└── build/            # Build output
```

### Naming Conventions

**Files:**
- Unit files: `UnitName.pas`
- Main program: `main.pas` or `ProgramName.pas`
- Use PascalCase for file names

**Directories:**
- Use lowercase with underscores: `game_logic/`
- Or camelCase: `gameLogic/`
- Be consistent

---

## Documentation

### Code Comments

**Document your code:**
```pascal
// ✅ GOOD: Explains why
// Use binary search because array is sorted
// This is O(log n) vs O(n) for linear search
index := BinarySearch(sortedArray, value);

// ❌ BAD: States the obvious
// Increment i
i := i + 1;
```

### Unit Documentation

**Document public APIs:**
```pascal
unit GameLogic;

interface
  // Game entity management
  // Entities are integer IDs that reference components
  
  type
    TEntity = integer;
  
  // Create a new entity
  // Returns entity ID, or -1 if failed
  function CreateEntity: TEntity;
  
  // Destroy an entity
  // Frees all associated components
  procedure DestroyEntity(entity: TEntity);
```

### README Files

**Document your project:**
```markdown
# My Game

A platformer game built with SuperPascal.

## Building

```bash
spc main.pas -o game.zof
```

## Running

```bash
zeal-run game.zof
```

## Structure

- `src/` - Source code
- `assets/` - Game assets
- `docs/` - Documentation
```

---

## Version Control Basics

### What is Version Control?

**Version control tracks changes:**
- See what changed
- Revert to previous versions
- Collaborate with others
- Track history

### Basic Concepts

**Repository:** Collection of files and history  
**Commit:** Snapshot of changes  
**Branch:** Parallel development line  
**Merge:** Combine changes

### Basic Workflow

**1. Initialize repository:**
```bash
git init
```

**2. Add files:**
```bash
git add src/
git add assets/
```

**3. Commit changes:**
```bash
git commit -m "Add game logic and sprites"
```

**4. Check status:**
```bash
git status
```

**5. View history:**
```bash
git log
```

### Good Commit Messages

**✅ Good:**
```
Add player movement system
Fix collision detection bug
Update sprite rendering
```

**❌ Bad:**
```
Changes
Fix
Update
```

---

## Code Review

### What is Code Review?

**Code review is:**
- Reviewing code before merging
- Finding bugs early
- Sharing knowledge
- Ensuring quality

### Review Checklist

**When reviewing code:**
- [ ] Does it solve the problem?
- [ ] Is it readable?
- [ ] Are there bugs?
- [ ] Does it follow style guide?
- [ ] Are there edge cases?
- [ ] Is it tested?

### Giving Feedback

**Be constructive:**
- ✅ "This could be clearer if..."
- ✅ "Consider handling this edge case..."
- ✅ "This might be more efficient if..."

**Avoid:**
- ❌ "This is wrong"
- ❌ "You should know better"
- ❌ Personal attacks

---

## Testing Basics

### Why Test?

**Testing helps:**
- Find bugs early
- Ensure code works
- Prevent regressions
- Document behavior

### Test Types

**Unit tests:** Test individual functions
**Integration tests:** Test components together
**System tests:** Test entire system

### Basic Testing

**Test your functions:**
```pascal
procedure TestCalculateDistance;
var
  p1, p2: TPoint;
  dist: integer;
begin
  p1.X := 0;
  p1.Y := 0;
  p2.X := 3;
  p2.Y := 4;
  
  dist := CalculateDistance(p1, p2);
  
  if dist = 5 then
    WriteLn('Test passed')
  else
    WriteLn('Test failed: expected 5, got ', dist);
end;
```

---

## Best Practices

### 1. Organize Early

**Start with good organization:**
- Create directory structure
- Separate concerns
- Use clear names

### 2. Document as You Go

**Don't wait:**
- Comment complex code
- Document public APIs
- Update README

### 3. Commit Often

**Small, frequent commits:**
- One feature per commit
- Clear commit messages
- Working code only

### 4. Review Code

**Review before merging:**
- Check for bugs
- Ensure quality
- Share knowledge

### 5. Test Regularly

**Test as you develop:**
- Test new features
- Test bug fixes
- Run all tests

---

## Exercises

### Exercise 1: Project Organization

Organize a project:
1. Create directory structure
2. Move files to appropriate locations
3. Update file paths
4. Document structure

### Exercise 2: Documentation

Document code:
1. Add comments to complex functions
2. Document public APIs
3. Create README
4. Write usage examples

### Exercise 3: Version Control

Use version control:
1. Initialize repository
2. Add files
3. Make commits
4. View history

---

**Previous Section:** [Code Organization and Style](./04_CodeOrganizationAndStyle.md)  
**Next Chapter:** [Chapter 11: Graphics](../11_Graphics/README.md)  
**Last Updated:** 2025-01-XX

