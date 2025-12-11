# Calculus Basics

**Part of:** [Chapter 19: Mathematics for Graphics and Games](./README.md)  
**Level:** Advanced (Optional)

---

## Introduction

**Calculus** is the mathematics of change. It's essential for:
- **Physics simulation** — Velocity, acceleration, forces
- **Optimization** — Finding best values (minima, maxima)
- **Curves** — Smooth animations, paths, interpolation
- **Rates of change** — How things change over time

This section introduces calculus concepts and shows how to implement them in SuperPascal.

**Note:** This is an **advanced section** for students interested in physics simulation and advanced game mechanics.

---

## What is Calculus?

**Calculus** studies:
- **Derivatives** — Rate of change (slope)
- **Integrals** — Accumulation (area under curve)
- **Limits** — Approaching values
- **Optimization** — Finding maxima and minima

**Applications:**
- **Physics** — Velocity (derivative of position), acceleration (derivative of velocity)
- **Animation** — Smooth curves, easing
- **Optimization** — Best values for game mechanics
- **Game mechanics** — Complex behaviors

---

## Derivatives

### What is a Derivative?

**Derivative** measures how fast a function changes:
```
f'(x) = lim(h→0) [f(x+h) - f(x)] / h
```

**Geometric meaning:** Slope of the tangent line at point x.

**Physical meaning:** Rate of change.

### Numerical Derivative

**Approximate derivative using finite differences:**
```pascal
function Derivative(const f: function(x: Q8.8): Q8.8;
                    x: Q8.8;
                    h: Q8.8): Q8.8;
begin
  // f'(x) ≈ [f(x+h) - f(x)] / h
  Derivative := (f(x + h) - f(x)) / h;
end;
```

**Example:**
```pascal
function Square(x: Q8.8): Q8.8;
begin
  Square := x * x;
end;

var x, deriv: Q8.8;
begin
  x := 2.0;
  deriv := Derivative(Square, x, 0.001);
  // f(x) = x², so f'(x) = 2x
  // f'(2) = 4
  WriteLn('Derivative at x=2: ', deriv);  // Should be approximately 4
end.
```

### Analytical Derivatives

**For simple functions, we can calculate exactly:**

**Polynomial derivative:**
```pascal
// If f(x) = a₀ + a₁x + a₂x² + ...
// Then f'(x) = a₁ + 2a₂x + 3a₃x² + ...

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
```

**Example:**
```
f(x) = 3x² + 2x + 1
f'(x) = 6x + 2
```

### Applications: Velocity and Acceleration

**Position → Velocity → Acceleration:**
```pascal
type
  TMotion = record
    Position: Q8.8;
    Velocity: Q8.8;      // Derivative of position
    Acceleration: Q8.8;  // Derivative of velocity
  end;

procedure UpdateMotion(var motion: TMotion; deltaTime: Q8.8);
begin
  // v = v + a*dt (integrate acceleration)
  motion.Velocity := motion.Velocity + motion.Acceleration * deltaTime;
  
  // p = p + v*dt (integrate velocity)
  motion.Position := motion.Position + motion.Velocity * deltaTime;
end;
```

---

## Integrals

### What is an Integral?

**Integral** measures accumulation (area under curve):
```
∫f(x)dx = F(x) where F'(x) = f(x)
```

**Geometric meaning:** Area under the curve.

**Physical meaning:** Total accumulation.

### Numerical Integration

**Rectangle rule (simplest):**
```pascal
function IntegrateRectangle(const f: function(x: Q8.8): Q8.8;
                            a, b: Q8.8;
                            n: integer): Q8.8;
var dx, x, sum: Q8.8;
var i: integer;
begin
  dx := (b - a) / n;
  sum := 0.0;
  
  for i := 0 to n - 1 do
  begin
    x := a + i * dx;
    sum := sum + f(x) * dx;
  end;
  
  IntegrateRectangle := sum;
end;
```

**Trapezoidal rule (more accurate):**
```pascal
function IntegrateTrapezoidal(const f: function(x: Q8.8): Q8.8;
                              a, b: Q8.8;
                              n: integer): Q8.8;
var dx, x, sum: Q8.8;
var i: integer;
begin
  dx := (b - a) / n;
  sum := (f(a) + f(b)) / 2.0;
  
  for i := 1 to n - 1 do
  begin
    x := a + i * dx;
    sum := sum + f(x);
  end;
  
  IntegrateTrapezoidal := sum * dx;
end;
```

### Applications: Distance from Velocity

**Integrate velocity to get distance:**
```pascal
function DistanceFromVelocity(const velocity: function(t: Q8.8): Q8.8;
                             startTime, endTime: Q8.8;
                             steps: integer): Q8.8;
begin
  DistanceFromVelocity := IntegrateTrapezoidal(velocity, startTime, endTime, steps);
end;
```

---

## Optimization

### Finding Maxima and Minima

**Critical points** occur where derivative is zero:
```pascal
function FindCriticalPoint(const f: function(x: Q8.8): Q8.8;
                          start, end: Q8.8;
                          var criticalX: Q8.8): boolean;
var x, step: Q8.8;
var prevDeriv, currDeriv: Q8.8;
begin
  step := (end - start) / 100.0;
  x := start;
  prevDeriv := Derivative(f, x, 0.001);
  
  while x < end do
  begin
    x := x + step;
    currDeriv := Derivative(f, x, 0.001);
    
    // Sign change indicates critical point
    if (prevDeriv * currDeriv) <= 0.0 then
    begin
      criticalX := x;
      FindCriticalPoint := true;
      Exit;
    end;
    
    prevDeriv := currDeriv;
  end;
  
  FindCriticalPoint := false;
end;
```

### Gradient Descent (Minimization)

**Find minimum by following negative gradient:**
```pascal
function GradientDescent(const f: function(x: Q8.8): Q8.8;
                        initialGuess: Q8.8;
                        learningRate: Q8.8;
                        iterations: integer): Q8.8;
var x: Q8.8;
var i: integer;
begin
  x := initialGuess;
  
  for i := 1 to iterations do
  begin
    var grad: Q8.8;
    grad := Derivative(f, x, 0.001);
    x := x - learningRate * grad;  // Move opposite to gradient
  end;
  
  GradientDescent := x;
end;
```

---

## Applications in Games

### Smooth Animation Curves

**Use derivatives for smooth acceleration:**
```pascal
function SmoothAcceleration(t: Q8.8): Q8.8;
begin
  // f(t) = t²(3 - 2t) - smooth step
  // f'(t) = 6t(1 - t) - smooth acceleration
  SmoothAcceleration := 6.0 * t * (1.0 - t);
end;
```

### Physics: Projectile Motion

**Projectile uses calculus:**
```pascal
type
  TProjectile = record
    Position: TVec2;
    Velocity: TVec2;
    Gravity: Q8.8;
  end;

procedure UpdateProjectile(var proj: TProjectile; deltaTime: Q8.8);
begin
  // Integrate velocity to get position
  proj.Position.X := proj.Position.X + proj.Velocity.X * deltaTime;
  proj.Position.Y := proj.Position.Y + proj.Velocity.Y * deltaTime;
  
  // Integrate gravity (acceleration) to get velocity
  proj.Velocity.Y := proj.Velocity.Y + proj.Gravity * deltaTime;
end;
```

### Optimization: Finding Best Values

**Find optimal game parameter:**
```pascal
// Example: Find optimal jump height
function JumpHeight(initialVelocity: Q8.8): Q8.8;
var gravity: Q8.8;
begin
  gravity := 9.8;  // m/s²
  // h = v²/(2g) - maximum height
  JumpHeight := (initialVelocity * initialVelocity) / (2.0 * gravity);
end;

// Find initial velocity that gives desired height
function FindVelocityForHeight(desiredHeight: Q8.8): Q8.8;
var gravity: Q8.8;
begin
  gravity := 9.8;
  // v = sqrt(2gh)
  FindVelocityForHeight := Sqrt(2.0 * gravity * desiredHeight);
end;
```

### Curve Fitting

**Fit curve to data points:**
```pascal
// Least squares method (simplified)
procedure FitLine(const points: array[0..N-1] of TPoint;
                  pointCount: integer;
                  var slope, intercept: Q8.8);
var sumX, sumY, sumXY, sumXX: Q8.8;
var i: integer;
begin
  sumX := 0.0;
  sumY := 0.0;
  sumXY := 0.0;
  sumXX := 0.0;
  
  for i := 0 to pointCount - 1 do
  begin
    sumX := sumX + points[i].X;
    sumY := sumY + points[i].Y;
    sumXY := sumXY + points[i].X * points[i].Y;
    sumXX := sumXX + points[i].X * points[i].X;
  end;
  
  // slope = (n*Σxy - Σx*Σy) / (n*Σx² - (Σx)²)
  slope := (pointCount * sumXY - sumX * sumY) / 
           (pointCount * sumXX - sumX * sumX);
  
  // intercept = (Σy - slope*Σx) / n
  intercept := (sumY - slope * sumX) / pointCount;
end;
```

---

## Best Practices

### 1. Choose Appropriate Step Size

**For numerical derivatives:**
- **Too small** — Rounding errors
- **Too large** — Poor approximation
- **Good:** h ≈ 0.001 to 0.01

### 2. Use Analytical When Possible

**Analytical derivatives are:**
- **Exact** — No approximation error
- **Faster** — No function calls
- **More stable** — No numerical issues

### 3. Cache Expensive Calculations

**Cache derivatives if used multiple times:**
```pascal
var cachedDeriv: Q8.8;
cachedDeriv := Derivative(f, x, 0.001);
// Use cachedDeriv multiple times
```

---

## Platform Considerations

### Performance

**Calculus operations:**
- **Numerical methods** — Can be slow (many function calls)
- **Analytical methods** — Fast (direct calculation)
- **On ZealZ80** — Prefer analytical when possible

### Precision

**Fixed-point limitations:**
- **Small step sizes** — May lose precision
- **Large values** — May overflow
- **Use Q12.12** — Better precision for calculus

---

## Summary

**Key Concepts:**
- **Derivatives** — Rate of change (slope)
- **Integrals** — Accumulation (area)
- **Numerical methods** — Approximate calculations
- **Analytical methods** — Exact calculations
- **Optimization** — Finding best values

**Derivatives:**
- **Numerical** — Finite differences
- **Analytical** — Direct calculation
- **Applications** — Velocity, acceleration, optimization

**Integrals:**
- **Numerical** — Rectangle, trapezoidal rules
- **Applications** — Distance, area, accumulation

**Optimization:**
- **Critical points** — Where derivative is zero
- **Gradient descent** — Follow negative gradient
- **Applications** — Finding best game parameters

**Applications:**
- Physics simulation
- Smooth animations
- Optimization
- Curve fitting
- Game mechanics

**Next:** Continue with audio programming or other game development topics.

---

**Next Chapter:** [Chapter 20: Audio Programming](../20_AudioProgramming/README.md)  
**Language Specification:** See [03_TypeSystem.md](../../languageSpecification/03_TypeSystem.md)  
**Last Updated:** 2025-01-XX

