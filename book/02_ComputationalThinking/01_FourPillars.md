# The Four Pillars of Computational Thinking

**Part of:** [Chapter 02: Computational Thinking](./README.md)

---

## Introduction

Computational thinking is a way of solving problems that uses techniques from computer science. It's not just about programming - it's a mental framework that helps you approach any complex problem systematically.

**The four pillars:**
1. **Decomposition** - Breaking problems into parts
2. **Pattern Recognition** - Finding similarities
3. **Abstraction** - Focusing on essentials
4. **Algorithm Design** - Creating step-by-step solutions

---

## 1. Decomposition

### What is Decomposition?

**Decomposition** means breaking a big problem into smaller, easier-to-solve pieces.

**Think of it like:**
- Building a house → Foundation, walls, roof, plumbing, electrical
- Making a game → Graphics, input, physics, audio, game logic
- Writing a program → Input, processing, output

### Example: Making Breakfast

**Big problem:** Make breakfast

**Decomposed:**
1. Get ingredients (eggs, bread, butter)
2. Heat pan
3. Cook eggs
4. Toast bread
5. Serve on plate

**Each step is simpler than the whole problem!**

### Example: Drawing a Game Character

**Big problem:** Draw a character on screen

**Decomposed:**
1. Load character sprite
2. Set character position
3. Draw sprite at position
4. Update position (if moving)
5. Handle animation frames

### Why Decomposition Matters

**Benefits:**
- ✅ Easier to understand
- ✅ Easier to solve
- ✅ Can work on parts separately
- ✅ Easier to test
- ✅ Easier to fix errors

**In programming:**
- Functions break code into parts
- Modules organize related code
- Classes group related data and behavior

---

## 2. Pattern Recognition

### What is Pattern Recognition?

**Pattern recognition** means finding similarities between problems or solutions.

**Think of it like:**
- Recognizing that "count items" and "sum numbers" both use loops
- Seeing that many games have similar structure (update, render, repeat)
- Noticing that sorting and searching both work with arrays

### Example: Recognizing Loop Patterns

**Pattern:** Many problems need to process each item in a list

**Examples:**
- Count items in array
- Sum numbers in array
- Find maximum value
- Print all items

**All use the same pattern:**
```pascal
for i := 0 to Length(arr) - 1 do
  // Process arr[i]
```

### Example: Game Loop Pattern

**Pattern:** Games repeat: update, render, wait

**Every game uses this:**
```pascal
while not Quit do
begin
  Update;      // Update game state
  Render;      // Draw everything
  WaitVBlank;  // Wait for next frame
end;
```

### Why Pattern Recognition Matters

**Benefits:**
- ✅ Reuse solutions
- ✅ Learn faster
- ✅ Write code faster
- ✅ Recognize common problems
- ✅ Apply known solutions

**In programming:**
- Design patterns (reusable solutions)
- Common algorithms (sorting, searching)
- Code templates (game loops, state machines)

---

## 3. Abstraction

### What is Abstraction?

**Abstraction** means removing unnecessary details to focus on what matters.

**Think of it like:**
- A car: You don't need to know how the engine works to drive
- A function: You don't need to know how it works to use it
- A sprite: You don't need to know pixel details to draw it

### Example: Drawing a Sprite

**Without abstraction (too much detail):**
- Set pixel at (0,0) to color RGB(255,0,0)
- Set pixel at (1,0) to color RGB(255,0,0)
- Set pixel at (2,0) to color RGB(0,255,0)
- ... (thousands of pixels)

**With abstraction (just what matters):**
```pascal
DrawSprite(sprite, x, y);
```

**We hide the details and focus on what we need!**

### Example: Game Entity

**Without abstraction:**
- Entity has position X, Y
- Entity has velocity VX, VY
- Entity has sprite index
- Entity has health points
- ... (many details)

**With abstraction:**
```pascal
type
  TEntity = record
    Position: TPoint;
    Velocity: TPoint;
    Sprite: integer;
    Health: integer;
  end;
```

**We group related details into one concept!**

### Levels of Abstraction

**High-level abstraction:**
```pascal
PlayGame;
```

**Medium-level abstraction:**
```pascal
UpdateGame;
RenderGame;
```

**Low-level abstraction:**
```pascal
x := x + velocityX;
y := y + velocityY;
```

**Each level hides details from the level above!**

### Why Abstraction Matters

**Benefits:**
- ✅ Simpler to understand
- ✅ Focus on what matters
- ✅ Hide complexity
- ✅ Reuse code
- ✅ Change implementation without changing usage

**In programming:**
- Functions abstract operations
- Types abstract data
- Classes abstract objects
- Modules abstract systems

---

## 4. Algorithm Design

### What is Algorithm Design?

**Algorithm design** means creating a step-by-step solution to a problem.

**An algorithm is:**
- A sequence of steps
- Clear and unambiguous
- Solves a specific problem
- Can be executed by a computer

### Example: Finding Maximum Value

**Problem:** Find the largest number in an array

**Algorithm:**
1. Start with first number as maximum
2. For each remaining number:
   - If number > maximum, set maximum = number
3. Return maximum

**In SuperPascal:**
```pascal
function FindMax(arr: array of integer): integer;
var
  i, max: integer;
begin
  max := arr[0];
  for i := 1 to Length(arr) - 1 do
    if arr[i] > max then
      max := arr[i];
  FindMax := max;
end;
```

### Example: Sorting Numbers

**Problem:** Sort numbers from smallest to largest

**Algorithm (Bubble Sort):**
1. For each position in array:
   - Compare with next position
   - If out of order, swap
2. Repeat until no swaps needed

**In SuperPascal:**
```pascal
procedure BubbleSort(var arr: array of integer);
var
  i, j, temp: integer;
begin
  for i := 0 to Length(arr) - 2 do
    for j := 0 to Length(arr) - 2 - i do
      if arr[j] > arr[j + 1] then
      begin
        temp := arr[j];
        arr[j] := arr[j + 1];
        arr[j + 1] := temp;
      end;
end;
```

### Algorithm Characteristics

**Good algorithms:**
- ✅ Clear steps
- ✅ Correct (solves the problem)
- ✅ Efficient (fast enough)
- ✅ Understandable
- ✅ Testable

**Algorithm design process:**
1. Understand the problem
2. Plan the solution
3. Write the algorithm
4. Test the algorithm
5. Refine if needed

### Why Algorithm Design Matters

**Benefits:**
- ✅ Systematic problem-solving
- ✅ Reusable solutions
- ✅ Efficient code
- ✅ Clear thinking
- ✅ Foundation for programming

**In programming:**
- Every program is an algorithm
- Functions implement algorithms
- Good algorithms = good programs

---

## Putting It All Together

### Example: Making a Simple Game

**Problem:** Create a game where a character moves and collects items

**1. Decomposition:**
- Initialize game
- Handle input
- Update character position
- Check collisions
- Draw everything
- Game loop

**2. Pattern Recognition:**
- Game loop pattern (update, render, repeat)
- Collision detection pattern
- Input handling pattern

**3. Abstraction:**
- `TEntity` type for character and items
- `UpdateGame` function
- `RenderGame` function
- `CheckCollision` function

**4. Algorithm Design:**
```
1. Initialize game
2. While game running:
   a. Read input
   b. Update character position
   c. Check for item collection
   d. Draw character
   e. Draw items
   f. Wait for next frame
3. End game
```

---

## Best Practices

### 1. Always Decompose First

**Before coding:**
- Break problem into parts
- List all steps
- Identify what each part does

### 2. Look for Patterns

**When solving problems:**
- Have I seen this before?
- Can I reuse a solution?
- What patterns apply?

### 3. Use Abstraction

**When coding:**
- Hide unnecessary details
- Create clear interfaces
- Group related concepts

### 4. Design Algorithms

**Before implementing:**
- Write algorithm in plain language
- Test algorithm mentally
- Refine before coding

---

## Exercises

### Exercise 1: Decomposition

Break down these problems:
1. Making a sandwich
2. Drawing a house on screen
3. Creating a calculator program

### Exercise 2: Pattern Recognition

Find patterns in:
1. These problems: count items, sum numbers, find maximum
2. These games: platformer, shooter, puzzle
3. These programs: text editor, calculator, game

### Exercise 3: Abstraction

Create abstractions for:
1. A game character (what details matter?)
2. A menu system (what operations are needed?)
3. A save system (what data needs saving?)

### Exercise 4: Algorithm Design

Design algorithms for:
1. Finding the smallest number in an array
2. Checking if a number is prime
3. Drawing a rectangle on screen

---

**Next Section:** [Problem-Solving Methodology](./02_ProblemSolvingMethodology.md)  
**Last Updated:** 2025-01-XX

