# Problem-Solving Methodology

**Part of:** [Chapter 02: Computational Thinking](./README.md)

---

## Introduction

Solving programming problems requires a systematic approach. This section teaches you a step-by-step methodology that will help you tackle any programming problem, from simple exercises to complex projects.

**The Problem-Solving Process:**
1. **Understand** - What is the problem asking?
2. **Plan** - How will we solve it?
3. **Implement** - Write the code
4. **Test** - Does it work?
5. **Refine** - Can we improve it?

---

## Step 1: Understand the Problem

### Read Carefully

**Before coding, make sure you understand:**
- What is the problem asking?
- What are the inputs?
- What should the output be?
- Are there any constraints?
- Are there any edge cases?

### Example: Calculate Average

**Problem:** "Write a function that calculates the average of numbers in an array"

**Understanding:**
- **Input:** Array of numbers
- **Output:** Average (sum divided by count)
- **Constraints:** Array may be empty (edge case!)
- **Edge cases:** Empty array, single number, negative numbers

### Ask Questions

**If unclear, ask:**
- What should happen with empty input?
- What format should the output be?
- Are there any special cases?
- What's the expected behavior?

### Restate the Problem

**In your own words:**
- "I need to sum all numbers and divide by the count"
- "I need to handle the case where the array is empty"
- "I need to return a number (probably a float)"

---

## Step 2: Plan the Solution

### Break It Down

**Decompose the problem:**
1. Sum all numbers
2. Count the numbers
3. Divide sum by count
4. Handle edge cases

### Choose an Approach

**Think about:**
- What data structures do I need?
- What algorithms apply?
- What patterns can I use?
- What functions should I create?

### Example: Calculate Average - Plan

**Approach:**
1. Check if array is empty (return 0 or error?)
2. Sum all numbers (loop through array)
3. Count numbers (array length)
4. Divide sum by count
5. Return result

**Algorithm:**
```
1. If array is empty, return 0
2. Initialize sum = 0
3. For each number in array:
   - Add to sum
4. Calculate average = sum / array length
5. Return average
```

### Write Pseudocode

**Before coding, write pseudocode:**
```
function CalculateAverage(arr):
  if arr is empty:
    return 0
  
  sum = 0
  for each number in arr:
    sum = sum + number
  
  average = sum / length(arr)
  return average
```

---

## Step 3: Implement

### Translate to Code

**Convert pseudocode to SuperPascal:**
```pascal
function CalculateAverage(arr: array of integer): Q8.8;
var
  i, sum: integer;
  avg: Q8.8;
begin
  if Length(arr) = 0 then
  begin
    CalculateAverage := Q8.8(0);
    Exit;
  end;
  
  sum := 0;
  for i := 0 to Length(arr) - 1 do
    sum := sum + arr[i];
  
  avg := Q8.8(sum) / Q8.8(Length(arr));
  CalculateAverage := avg;
end;
```

### Start Simple

**Begin with:**
- Basic functionality first
- Handle edge cases later
- Get it working, then optimize

### Use Good Practices

**While implementing:**
- Clear variable names
- Comments for complex logic
- Consistent formatting
- Small functions

---

## Step 4: Test

### Test Cases

**Create test cases:**
1. Normal case: [10, 20, 30] → 20
2. Empty array: [] → 0
3. Single number: [5] → 5
4. Negative numbers: [-10, 10] → 0
5. Large numbers: [1000, 2000] → 1500

### Test Systematically

**Test:**
- Normal cases
- Edge cases (empty, single item)
- Boundary cases (min/max values)
- Error cases (if applicable)

### Debug if Needed

**If tests fail:**
- Check each step
- Print intermediate values
- Trace through the code
- Fix one issue at a time

---

## Step 5: Refine

### Review Your Code

**Ask yourself:**
- Does it work correctly?
- Is it readable?
- Is it efficient?
- Can it be simplified?

### Improve

**Consider:**
- Better variable names
- Simpler logic
- More efficient algorithm
- Better error handling
- More comments

### Example: Refined CalculateAverage

**Original:**
```pascal
function CalculateAverage(arr: array of integer): Q8.8;
var
  i, sum: integer;
  avg: Q8.8;
begin
  if Length(arr) = 0 then
  begin
    CalculateAverage := Q8.8(0);
    Exit;
  end;
  
  sum := 0;
  for i := 0 to Length(arr) - 1 do
    sum := sum + arr[i];
  
  avg := Q8.8(sum) / Q8.8(Length(arr));
  CalculateAverage := avg;
end;
```

**Refined (simpler):**
```pascal
function CalculateAverage(arr: array of integer): Q8.8;
var
  i, sum: integer;
begin
  if Length(arr) = 0 then
  begin
    CalculateAverage := Q8.8(0);
    Exit;
  end;
  
  sum := 0;
  for i := 0 to Length(arr) - 1 do
    sum := sum + arr[i];
  
  CalculateAverage := Q8.8(sum) / Q8.8(Length(arr));
end;
```

---

## Complete Example: Finding Maximum

### Step 1: Understand

**Problem:** Find the largest number in an array

**Understanding:**
- Input: Array of integers
- Output: Largest integer
- Edge case: Empty array

### Step 2: Plan

**Algorithm:**
1. If array empty, return error or special value
2. Start with first number as maximum
3. For each remaining number:
   - If number > maximum, update maximum
4. Return maximum

### Step 3: Implement

```pascal
function FindMax(arr: array of integer): integer;
var
  i, max: integer;
begin
  if Length(arr) = 0 then
  begin
    FindMax := 0;  // Or raise exception
    Exit;
  end;
  
  max := arr[0];
  for i := 1 to Length(arr) - 1 do
    if arr[i] > max then
      max := arr[i];
  
  FindMax := max;
end;
```

### Step 4: Test

**Test cases:**
- [10, 20, 30] → 30 ✓
- [5] → 5 ✓
- [-10, -5, -1] → -1 ✓
- [] → 0 (edge case)

### Step 5: Refine

**Consider:**
- Error handling for empty array
- What if all numbers are negative?
- Can we make it more efficient? (Already O(n), good!)

---

## Problem-Solving Checklist

### Before Coding

- [ ] I understand what the problem is asking
- [ ] I know what the inputs are
- [ ] I know what the output should be
- [ ] I've identified edge cases
- [ ] I have a plan

### While Coding

- [ ] I'm following my plan
- [ ] I'm testing as I go
- [ ] My code is readable
- [ ] I'm handling edge cases

### After Coding

- [ ] I've tested all cases
- [ ] My code works correctly
- [ ] I've reviewed for improvements
- [ ] I've documented complex logic

---

## Common Mistakes

### Mistake 1: Coding Too Early

**❌ Bad:**
- Start coding immediately
- Figure it out as you go
- Fix errors as they appear

**✅ Good:**
- Understand first
- Plan first
- Then code

### Mistake 2: Not Testing

**❌ Bad:**
- Write code
- Assume it works
- Move on

**✅ Good:**
- Write code
- Test immediately
- Fix issues
- Test again

### Mistake 3: Not Refining

**❌ Bad:**
- Code works
- Done!

**✅ Good:**
- Code works
- Review and improve
- Make it better

---

## Best Practices

### 1. Always Understand First

**Don't start coding until you:**
- Understand the problem completely
- Know what you're building
- Have a clear plan

### 2. Plan Before Coding

**Spend time planning:**
- Break down the problem
- Design the algorithm
- Write pseudocode
- Then code

### 3. Test Early and Often

**Test:**
- After each function
- With different inputs
- Edge cases
- Before moving on

### 4. Refine Continuously

**Always ask:**
- Can this be simpler?
- Can this be clearer?
- Can this be more efficient?

---

## Exercises

### Exercise 1: Apply the Methodology

Use the 5-step process to solve:
1. Find the smallest number in an array
2. Count how many times a value appears
3. Check if an array is sorted

### Exercise 2: Problem Understanding

For each problem, identify:
- Inputs
- Outputs
- Edge cases
- Constraints

### Exercise 3: Planning

For each problem, write:
- Algorithm in plain language
- Pseudocode
- List of functions needed

### Exercise 4: Testing

Create test cases for:
- Finding maximum
- Calculating average
- Checking if sorted

---

**Previous Section:** [The Four Pillars of Computational Thinking](./01_FourPillars.md)  
**Next Section:** [Decomposition and Pattern Recognition](./03_DecompositionAndPatterns.md)  
**Last Updated:** 2025-01-XX

