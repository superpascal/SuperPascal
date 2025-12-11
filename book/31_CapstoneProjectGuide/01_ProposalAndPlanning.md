# Proposal and Planning

**Part of:** [Chapter 31: Capstone Project Guide](./README.md)

---

## Introduction

A successful project starts with a clear plan. This section teaches you how to define your project scope, create a proposal, plan development phases, and design your project architecture.

**Key concepts:**
- **Project scope** — What your project will do
- **Proposal** — Document describing your project
- **Milestones** — Key development checkpoints
- **Architecture** — System design and structure

---

## Defining Project Scope

### What is Project Scope?

**Project scope defines:**
- **What** your project will do
- **What** it won't do
- **How** complex it will be
- **How** long it will take

**Scope helps you:**
- Stay focused
- Avoid feature creep
- Set realistic goals
- Complete your project

### Scope Questions

**Ask yourself:**

1. **What is the core goal?**
   - What problem does it solve?
   - What will users do with it?

2. **What features are essential?**
   - Minimum viable product (MVP)
   - Core functionality

3. **What features are nice-to-have?**
   - Optional features
   - Can be added later

4. **What is out of scope?**
   - Features you won't include
   - Limits and boundaries

### Example: Platformer Game

**Core features (in scope):**
- Player movement (left, right, jump)
- Basic platform collision
- 3-5 levels
- Win/lose conditions
- Simple graphics

**Nice-to-have (optional):**
- Enemies
- Collectibles
- Multiple player characters
- Sound effects
- Animated sprites

**Out of scope:**
- Online multiplayer
- Level editor
- Save system
- Complex physics

---

## Creating a Project Proposal

### Proposal Structure

**A good proposal includes:**

1. **Title** — Project name
2. **Description** — What the project does
3. **Goals** — What you want to achieve
4. **Features** — List of features
5. **Technical Requirements** — What you'll need
6. **Timeline** — Development schedule
7. **Success Criteria** — How you'll know it's done

### Example Proposal

**Title:** Space Defender

**Description:**
A side-scrolling shooter where the player controls a spaceship and defends against waves of enemies. The player shoots enemies, collects power-ups, and survives as long as possible.

**Goals:**
- Create a complete, playable game
- Demonstrate graphics, input, and game loop
- Learn sprite animation and collision detection
- Practice game design and polish

**Features:**
- Player spaceship with movement and shooting
- Enemy waves that spawn and move
- Collision detection between bullets and enemies
- Score system
- Game over screen
- Simple particle effects

**Technical Requirements:**
- SuperPascal compiler
- ZealIDE
- Sprite editor (or pixel art tool)
- Sound effects (optional)

**Timeline:**
- Week 1: Player movement and shooting
- Week 2: Enemy spawning and movement
- Week 3: Collision detection and scoring
- Week 4: Polish and testing

**Success Criteria:**
- Game is fully playable
- All core features work
- No major bugs
- Code is well-organized
- Project is documented

---

## Planning Development Phases

### Phase-Based Development

**Break development into phases:**

1. **Phase 1: Foundation**
   - Set up project structure
   - Implement basic systems
   - Get something running

2. **Phase 2: Core Features**
   - Implement main functionality
   - Build core gameplay
   - Test basic systems

3. **Phase 3: Content**
   - Add art and sound
   - Create game content
   - Polish visuals

4. **Phase 4: Polish**
   - Fix bugs
   - Optimize performance
   - Refine gameplay

### Example: Platformer Phases

**Phase 1: Foundation (Week 1)**
- Set up project files
- Initialize video system
- Create basic game loop
- Draw a test sprite

**Phase 2: Core Features (Week 2-3)**
- Implement player movement
- Add platform collision
- Create level structure
- Add win/lose conditions

**Phase 3: Content (Week 4)**
- Create sprite graphics
- Design levels
- Add sound effects
- Polish visuals

**Phase 4: Polish (Week 5)**
- Fix collision bugs
- Optimize rendering
- Balance gameplay
- Test thoroughly

---

## Setting Milestones

### What are Milestones?

**Milestones are:**
- Key checkpoints in development
- Measurable goals
- Points to review progress
- Opportunities to adjust plans

**Good milestones:**
- Are specific and measurable
- Are achievable
- Have clear success criteria
- Are spaced appropriately

### Example Milestones

**Milestone 1: Basic Movement (End of Week 1)**
- Player can move left/right
- Player can jump
- Player sprite displays correctly
- Input is responsive

**Milestone 2: Collision (End of Week 2)**
- Player collides with platforms
- Player doesn't fall through floors
- Collision feels correct
- No major bugs

**Milestone 3: Complete Level (End of Week 3)**
- One complete level works
- Player can reach goal
- Win condition triggers
- Game can restart

**Milestone 4: Polish (End of Week 4)**
- All levels work
- Graphics are polished
- Sound effects added
- Ready to present

---

## Designing Project Architecture

### What is Architecture?

**Architecture is:**
- How systems are organized
- How code is structured
- How components interact
- How data flows

**Good architecture:**
- Is organized and clear
- Separates concerns
- Is easy to understand
- Is easy to modify

### Project Structure

**Organize your project:**

```
MyProject/
├── src/
│   ├── main.pas          # Main program
│   ├── game.pas          # Game logic
│   ├── player.pas        # Player entity
│   ├── enemies.pas       # Enemy entities
│   ├── collision.pas     # Collision detection
│   └── graphics.pas      # Graphics utilities
├── assets/
│   ├── sprites/          # Sprite images
│   ├── tiles/            # Tile graphics
│   └── sounds/           # Sound effects
├── levels/               # Level data
└── README.md             # Project documentation
```

### System Design

**Design your systems:**

```pascal
// Main program structure
program MyGame;

uses
  Game,
  Player,
  Enemies,
  Collision,
  Graphics;

var
  GameState: TGameState;

begin
  InitGame(GameState);
  
  while true do
  begin
    UpdateGame(GameState);
    RenderGame(GameState);
    WaitVBlank;
  end;
end.
```

**System responsibilities:**
- **Game** — Overall game state and flow
- **Player** — Player entity and behavior
- **Enemies** — Enemy entities and AI
- **Collision** — Collision detection and response
- **Graphics** — Rendering utilities

---

## Planning Tools

### Project Planning Checklist

**Use this checklist:**

- [ ] Project scope defined
- [ ] Proposal written
- [ ] Development phases planned
- [ ] Milestones set
- [ ] Architecture designed
- [ ] Project structure created
- [ ] Timeline established
- [ ] Success criteria defined

### Progress Tracking

**Track your progress:**

```pascal
type
  TProjectStatus = record
    Phase: byte;
    Milestone: byte;
    CompletedFeatures: array[0..15] of string;
    RemainingFeatures: array[0..15] of string;
  end;
```

**Update regularly:**
- Mark completed features
- Update milestone status
- Adjust timeline if needed
- Document decisions

---

## Common Pitfalls

### Pitfall 1: Scope Too Large

**Problem:** Trying to do too much

**Solution:**
- Start small
- Focus on core features
- Add features incrementally
- Be realistic about time

### Pitfall 2: No Plan

**Problem:** Starting without a plan

**Solution:**
- Write a proposal
- Plan phases
- Set milestones
- Review regularly

### Pitfall 3: Feature Creep

**Problem:** Adding features during development

**Solution:**
- Stick to scope
- Document new ideas for later
- Complete core features first
- Add features in polish phase

### Pitfall 4: Poor Architecture

**Problem:** Code becomes messy

**Solution:**
- Design architecture first
- Organize code clearly
- Separate concerns
- Refactor as needed

---

## Best Practices

### 1. Start Small

**Build incrementally:**

```pascal
// ✅ GOOD: Start with minimal version
procedure InitGame;
begin
  InitVideo;
  InitInput;
  // Start simple
end;

// ❌ BAD: Try to do everything at once
procedure InitGame;
begin
  InitVideo;
  InitInput;
  InitAudio;
  InitPhysics;
  InitNetworking;  // Too much!
end;
```

### 2. Plan Before Coding

**Design first:**

```pascal
// ✅ GOOD: Plan structure
// 1. Define types
// 2. Design procedures
// 3. Implement incrementally

// ❌ BAD: Code without plan
// Just start coding and see what happens
```

### 3. Set Clear Milestones

**Make milestones specific:**

```pascal
// ✅ GOOD: Specific milestone
// "Player can move left/right and jump"

// ❌ BAD: Vague milestone
// "Make player work"
```

### 4. Review Regularly

**Check progress:**

```pascal
// ✅ GOOD: Weekly review
// - What's done?
// - What's next?
// - Any issues?

// ❌ BAD: No reviews
// Just keep coding
```

### 5. Document Decisions

**Write down choices:**

```pascal
// ✅ GOOD: Document why
// "Using fixed-point math for smooth movement"
// "Using ECS for entity management"

// ❌ BAD: No documentation
// Future you won't remember why
```

---

## Exercises

### Exercise 1: Define Scope

Choose a project idea and:
1. Define core features
2. List nice-to-have features
3. Identify out-of-scope features
4. Write a scope statement

### Exercise 2: Write Proposal

Write a complete proposal for your project:
1. Title and description
2. Goals and features
3. Technical requirements
4. Timeline and milestones
5. Success criteria

### Exercise 3: Plan Phases

Break your project into phases:
1. Phase 1: Foundation
2. Phase 2: Core Features
3. Phase 3: Content
4. Phase 4: Polish

### Exercise 4: Design Architecture

Design your project architecture:
1. Project structure
2. System organization
3. Code organization
4. Data flow

---

**Next Section:** [Core System Development](./02_CoreSystemDevelopment.md)  
**Last Updated:** 2025-01-XX

