# Polynomials and Algebra

**Part of:** [Chapter 19: Mathematics for Graphics and Games](./README.md)  
**Level:** Advanced (Optional)

---

## Introduction

**Polynomials** are mathematical expressions that are fundamental to:
- **Curves** — Smooth paths, animations, splines
- **Interpolation** — Smooth transitions between values
- **Game mechanics** — Non-linear functions, easing
- **Graphics** — Bezier curves, splines, paths

This section introduces polynomials and algebraic operations, showing how to implement and use them in SuperPascal.

**Note:** This is an **advanced section** for students interested in deeper mathematics. It's optional but highly valuable for advanced game development.

---

## What is a Polynomial?

A **polynomial** is an expression like:
```
P(x) = a₀ + a₁x + a₂x² + a₃x³ + ... + aₙxⁿ
```

**Components:**
- **Coefficients** — a₀, a₁, a₂, ... (the numbers)
- **Variable** — x (the input)
- **Degree** — n (highest power)
- **Terms** — Each aᵢxⁱ is a term

**Examples:**
- **Linear:** P(x) = 2x + 3 (degree 1)
- **Quadratic:** P(x) = x² + 2x + 1 (degree 2)
- **Cubic:** P(x) = x³ - x (degree 3)

---

## Representing Polynomials

### Array of Coefficients

**Polynomials can be represented as arrays:**
```pascal
type
  TPolynomial = array[0..MAX_DEGREE] of Q8.8;  // Coefficients

var poly: TPolynomial;
begin
  // P(x) = 2x² + 3x + 1
  poly[0] := 1.0;   // Constant term (x⁰)
  poly[1] := 3.0;   // x term
  poly[2] := 2.0;   // x² term
  // poly[3..MAX_DEGREE] := 0.0 (higher terms are zero)
end.
```

**Index represents power:**
- `poly[0]` = coefficient of x⁰ (constant)
- `poly[1]` = coefficient of x¹
- `poly[2]` = coefficient of x²
- etc.

### Finding Degree

**Degree is the highest non-zero coefficient:**
```pascal
function PolynomialDegree(const poly: TPolynomial; maxDegree: integer): integer;
var i: integer;
begin
  for i := maxDegree downto 0 do
    if poly[i] <> 0.0 then
    begin
      PolynomialDegree := i;
      Exit;
    end;
  PolynomialDegree := 0;  // Zero polynomial
end;
```

---

## Evaluating Polynomials

### Direct Evaluation

**Evaluate by calculating each term:**
```pascal
function EvaluatePolynomial(const poly: TPolynomial; 
                            degree: integer; 
                            x: Q8.8): Q8.8;
var i: integer;
var result, term: Q8.8;
var xPower: Q8.8;
begin
  result := 0.0;
  xPower := 1.0;  // x⁰ = 1
  
  for i := 0 to degree do
  begin
    term := poly[i] * xPower;
    result := result + term;
    xPower := xPower * x;  // x¹, x², x³, ...
  end;
  
  EvaluatePolynomial := result;
end;
```

**Example:**
```pascal
var poly: TPolynomial;
var result: Q8.8;
begin
  // P(x) = 2x² + 3x + 1
  poly[0] := 1.0;
  poly[1] := 3.0;
  poly[2] := 2.0;
  
  result := EvaluatePolynomial(poly, 2, 2.0);
  // P(2) = 2(2)² + 3(2) + 1 = 8 + 6 + 1 = 15
  WriteLn('P(2) = ', result);
end.
```

### Horner's Method (Efficient)

**Horner's method is more efficient:**
```pascal
function EvaluatePolynomialHorner(const poly: TPolynomial;
                                  degree: integer;
                                  x: Q8.8): Q8.8;
var i: integer;
var result: Q8.8;
begin
  result := poly[degree];  // Start with highest coefficient
  
  for i := degree - 1 downto 0 do
    result := result * x + poly[i];
  
  EvaluatePolynomialHorner := result;
end;
```

**Why Horner's method:**
- **Fewer multiplications** — More efficient
- **Better numerical stability** — Less rounding error
- **Standard method** — Used in practice

**Example:**
```
P(x) = 2x² + 3x + 1

Horner's form: ((2)x + 3)x + 1

Evaluation:
  result = 2
  result = 2 * 2 + 3 = 7
  result = 7 * 2 + 1 = 15
```

---

## Polynomial Operations

### Addition

**Add corresponding coefficients:**
```pascal
procedure PolynomialAdd(const A, B: TPolynomial;
                        degreeA, degreeB: integer;
                        var Result: TPolynomial;
                        var resultDegree: integer);
var i: integer;
var maxDegree: integer;
begin
  if degreeA > degreeB then
    maxDegree := degreeA
  else
    maxDegree := degreeB;
  
  for i := 0 to maxDegree do
    Result[i] := A[i] + B[i];
  
  resultDegree := PolynomialDegree(Result, maxDegree);
end;
```

**Example:**
```
A(x) = 2x² + 3x + 1
B(x) = x² + 2x + 5
A(x) + B(x) = 3x² + 5x + 6
```

### Subtraction

**Subtract corresponding coefficients:**
```pascal
procedure PolynomialSubtract(const A, B: TPolynomial;
                             degreeA, degreeB: integer;
                             var Result: TPolynomial;
                             var resultDegree: integer);
var i: integer;
var maxDegree: integer;
begin
  if degreeA > degreeB then
    maxDegree := degreeA
  else
    maxDegree := degreeB;
  
  for i := 0 to maxDegree do
    Result[i] := A[i] - B[i];
  
  resultDegree := PolynomialDegree(Result, maxDegree);
end;
```

### Scalar Multiplication

**Multiply all coefficients by a scalar:**
```pascal
procedure PolynomialScale(var poly: TPolynomial;
                         degree: integer;
                         scalar: Q8.8);
var i: integer;
begin
  for i := 0 to degree do
    poly[i] := poly[i] * scalar;
end;
```

### Multiplication

**Multiply two polynomials:**
```pascal
procedure PolynomialMultiply(const A, B: TPolynomial;
                             degreeA, degreeB: integer;
                             var Result: TPolynomial;
                             var resultDegree: integer);
var i, j: integer;
begin
  // Initialize result to zero
  for i := 0 to MAX_DEGREE do
    Result[i] := 0.0;
  
  // Multiply: (A[i] * x^i) * (B[j] * x^j) = A[i] * B[j] * x^(i+j)
  for i := 0 to degreeA do
    for j := 0 to degreeB do
      Result[i + j] := Result[i + j] + A[i] * B[j];
  
  resultDegree := degreeA + degreeB;
end;
```

**Example:**
```
A(x) = x + 1
B(x) = x + 2
A(x) * B(x) = (x + 1)(x + 2) = x² + 3x + 2
```

---

## Finding Roots

### What are Roots?

**Roots** are values of x where P(x) = 0.

**Example:**
```
P(x) = x² - 5x + 6
Roots: x = 2 and x = 3 (because P(2) = 0 and P(3) = 0)
```

### Bisection Method

**Find root by narrowing interval:**
```pascal
function FindRootBisection(const poly: TPolynomial;
                           degree: integer;
                           a, b: Q8.8;
                           tolerance: Q8.8): Q8.8;
var mid: Q8.8;
var fa, fb, fmid: Q8.8;
begin
  // Ensure f(a) and f(b) have opposite signs
  fa := EvaluatePolynomialHorner(poly, degree, a);
  fb := EvaluatePolynomialHorner(poly, degree, b);
  
  while (b - a) > tolerance do
  begin
    mid := (a + b) / 2.0;
    fmid := EvaluatePolynomialHorner(poly, degree, mid);
    
    if (fa * fmid) < 0.0 then
    begin
      b := mid;
      fb := fmid;
    end
    else
    begin
      a := mid;
      fa := fmid;
    end;
  end;
  
  FindRootBisection := (a + b) / 2.0;
end;
```

### Newton's Method (Faster)

**Newton's method uses derivative:**
```pascal
// First, need derivative of polynomial
procedure PolynomialDerivative(const poly: TPolynomial;
                               degree: integer;
                               var derivative: TPolynomial;
                               var derivDegree: integer);
var i: integer;
begin
  for i := 0 to degree - 1 do
    derivative[i] := poly[i + 1] * (i + 1);
  derivDegree := degree - 1;
end;

function FindRootNewton(const poly: TPolynomial;
                        degree: integer;
                        initialGuess: Q8.8;
                        maxIterations: integer;
                        tolerance: Q8.8): Q8.8;
var x, xNew: Q8.8;
var polyDeriv: TPolynomial;
var derivDegree: integer;
var i: integer;
var fx, fpx: Q8.8;
begin
  x := initialGuess;
  PolynomialDerivative(poly, degree, polyDeriv, derivDegree);
  
  for i := 1 to maxIterations do
  begin
    fx := EvaluatePolynomialHorner(poly, degree, x);
    if Abs(fx) < tolerance then
      Exit;  // Found root
    
    fpx := EvaluatePolynomialHorner(polyDeriv, derivDegree, x);
    if fpx = 0.0 then
      Exit;  // Derivative is zero, can't continue
    
    xNew := x - (fx / fpx);  // Newton's method formula
    
    if Abs(xNew - x) < tolerance then
      Exit;  // Converged
    
    x := xNew;
  end;
  
  FindRootNewton := x;
end;
```

---

## Applications in Games

### Bezier Curves

**Bezier curves use polynomials:**
```pascal
// Quadratic Bezier: P(t) = (1-t)²P₀ + 2(1-t)tP₁ + t²P₂
function BezierQuadratic(P0, P1, P2: TPoint; t: Q8.8): TPoint;
var oneMinusT: Q8.8;
var result: TPoint;
begin
  oneMinusT := 1.0 - t;
  
  result.X := oneMinusT * oneMinusT * P0.X +
              2.0 * oneMinusT * t * P1.X +
              t * t * P2.X;
  
  result.Y := oneMinusT * oneMinusT * P0.Y +
              2.0 * oneMinusT * t * P1.Y +
              t * t * P2.Y;
  
  BezierQuadratic := result;
end;
```

### Easing Functions

**Easing uses polynomial curves:**
```pascal
// Ease-in-out cubic: t³(3 - 2t)
function EaseInOutCubic(t: Q8.8): Q8.8;
begin
  if t < 0.5 then
    EaseInOutCubic := 4.0 * t * t * t
  else
  begin
    var f: Q8.8;
    f := 2.0 * t - 2.0;
    EaseInOutCubic := 0.5 * f * f * f + 1.0;
  end;
end;
```

### Interpolation

**Polynomial interpolation for smooth transitions:**
```pascal
// Linear interpolation: lerp(a, b, t) = a + t(b - a)
function Lerp(a, b, t: Q8.8): Q8.8;
begin
  Lerp := a + t * (b - a);
end;

// Smooth interpolation using cubic polynomial
function SmoothStep(t: Q8.8): Q8.8;
begin
  // t²(3 - 2t)
  SmoothStep := t * t * (3.0 - 2.0 * t);
end;
```

---

## Algebraic Operations

### Factoring

**Factor polynomials (simplified):**
```pascal
// For quadratic: ax² + bx + c
// Find factors (x - r₁)(x - r₂) where r₁, r₂ are roots
procedure FactorQuadratic(a, b, c: Q8.8;
                         var root1, root2: Q8.8;
                         var hasRealRoots: boolean);
var discriminant: Q8.8;
begin
  discriminant := b * b - 4.0 * a * c;
  
  if discriminant >= 0.0 then
  begin
    hasRealRoots := true;
    root1 := (-b + Sqrt(discriminant)) / (2.0 * a);
    root2 := (-b - Sqrt(discriminant)) / (2.0 * a);
  end
  else
    hasRealRoots := false;
end;
```

### Simplification

**Simplify polynomials (remove zero coefficients):**
```pascal
procedure SimplifyPolynomial(var poly: TPolynomial;
                             var degree: integer);
var newDegree: integer;
begin
  newDegree := PolynomialDegree(poly, degree);
  degree := newDegree;
  // Zero out higher coefficients (already zero, but explicit)
end;
```

---

## Best Practices

### 1. Use Appropriate Degree

**Don't use higher degree than needed:**
```pascal
// Bad: Using degree 10 for a linear function
var poly: array[0..10] of Q8.8;

// Good: Use appropriate degree
var linear: array[0..1] of Q8.8;  // Degree 1
```

### 2. Check for Zero Polynomial

**Handle zero polynomial:**
```pascal
function IsZeroPolynomial(const poly: TPolynomial; degree: integer): boolean;
var i: integer;
begin
  for i := 0 to degree do
    if poly[i] <> 0.0 then
    begin
      IsZeroPolynomial := false;
      Exit;
    end;
  IsZeroPolynomial := true;
end;
```

### 3. Use Horner's Method

**Always use Horner's method for evaluation:**
- More efficient
- Better numerical stability
- Standard practice

---

## Platform Considerations

### Fixed-Point Precision

**Polynomials work with fixed-point:**
- **Q8.8** — Good for simple polynomials
- **Q12.12** — Better for higher-degree polynomials
- **Precision** — Higher degree = more precision needed

### Performance

**Polynomial evaluation:**
- **Horner's method** — O(n) operations
- **Direct evaluation** — O(n²) operations (less efficient)
- **On ZealZ80** — Keep degree low (≤ 5) for performance

---

## Summary

**Key Concepts:**
- **Polynomials** are expressions: a₀ + a₁x + a₂x² + ...
- **Representation** — Array of coefficients
- **Evaluation** — Horner's method (efficient)
- **Operations** — Addition, subtraction, multiplication
- **Roots** — Values where P(x) = 0
- **Applications** — Bezier curves, easing, interpolation

**Operations:**
- Addition — Add coefficients
- Subtraction — Subtract coefficients
- Scalar multiplication — Multiply all coefficients
- Multiplication — Convolve coefficients
- Derivative — Multiply by power, shift

**Applications:**
- Bezier curves
- Easing functions
- Interpolation
- Smooth animations
- Game mechanics

**Next:** Learn about linear algebra for transformations.

---

**Next Section:** [Linear Algebra](./05_LinearAlgebra.md)  
**Language Specification:** See [03_TypeSystem.md](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

