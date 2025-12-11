# Physics Library OO Usage Examples

**Status:** 📚 Documentation  
**Library:** Physics Library

---

## Overview

The physics library now supports both **procedural** (record-based) and **object-oriented** (class-based) APIs. This document shows how to use the new OO API.

---

## Material Classes

### Creating Materials

```pascal
var
  rockMaterial: TRockMaterial;
  glassMaterial: TGlassMaterial;
  ballMaterial: TBallMaterial;
  bubbleMaterial: TBubbleMaterial;
begin
  // Create materials
  rockMaterial := TRockMaterial.Create;
  glassMaterial := TGlassMaterial.Create;
  ballMaterial := TBallMaterial.Create;
  bubbleMaterial := TBubbleMaterial.Create;
  
  // Materials are reusable - can be shared by multiple objects
end.
```

### Available Material Types

- `TRockMaterial` - Solid, not bouncy, not brittle
- `TGlassMaterial` - Solid, not bouncy, very brittle
- `TBallMaterial` - Bouncy, not solid, not brittle
- `TBubbleMaterial` - Bouncy, not solid, high fragmentability
- `TWoodMaterial` - Medium solidness, low bouncability, medium brittleness
- `TMetalMaterial` - Very solid, low bouncability, low brittleness
- `TRubberMaterial` - Very bouncy, not solid, not brittle
- `TSpongeMaterial` - Not solid, not bouncy

---

## Physics Object Classes

### Creating Circle Objects (Bubbles)

```pascal
var
  bubble: TCirclePhysicsObject;
  bubbleMaterial: TBubbleMaterial;
begin
  bubbleMaterial := TBubbleMaterial.Create;
  
  // Create bubble at position (100, 100) with radius 50
  bubble := TCirclePhysicsObject.Create(
    IntToFixed16(100),
    IntToFixed16(100),
    IntToFixed16(50),
    bubbleMaterial
  );
  
  // Set initial velocity
  bubble.VX := IntToFixed16(5);
  bubble.VY := IntToFixed16(-10);
  
  // Update physics
  bubble.Update(FIXED16_ONE);
  
  // Clean up
  bubble.Free;
  bubbleMaterial.Free;
end.
```

### Creating Polygon Objects

```pascal
var
  polygon: TPolygonPhysicsObject;
  glassMaterial: TGlassMaterial;
  poly: TPolygon2D;
begin
  glassMaterial := TGlassMaterial.Create;
  
  // Create polygon shape
  SetLength(poly.Points, 4);
  poly.Count := 4;
  poly.Points[0].X := 0;
  poly.Points[0].Y := 0;
  poly.Points[1].X := 100;
  poly.Points[1].Y := 0;
  poly.Points[2].X := 100;
  poly.Points[2].Y := 100;
  poly.Points[3].X := 0;
  poly.Points[3].Y := 100;
  
  // Create polygon physics object
  polygon := TPolygonPhysicsObject.Create(
    IntToFixed16(200),
    IntToFixed16(200),
    poly,
    glassMaterial
  );
  
  // Update physics
  polygon.Update(FIXED16_ONE);
  
  // Clean up
  polygon.Free;
  glassMaterial.Free;
end.
```

---

## Polymorphic Usage

### Using Base Class for Polymorphism

```pascal
var
  objects: array of TPhysicsObjectBase;
  ball: TCirclePhysicsObject;
  block: TPolygonPhysicsObject;
  ballMaterial: TBallMaterial;
  rockMaterial: TRockMaterial;
  i: integer;
begin
  ballMaterial := TBallMaterial.Create;
  rockMaterial := TRockMaterial.Create;
  
  // Create different object types
  ball := TCirclePhysicsObject.Create(100, 100, 25, ballMaterial);
  block := TPolygonPhysicsObject.Create(200, 200, somePolygon, rockMaterial);
  
  // Store in base class array (polymorphism)
  SetLength(objects, 2);
  objects[0] := ball;
  objects[1] := block;
  
  // Update all objects polymorphically
  for i := 0 to Length(objects) - 1 do
  begin
    objects[i].Update(FIXED16_ONE);
    objects[i].ApplyGravity;
  end;
  
  // Clean up
  for i := 0 to Length(objects) - 1 do
    objects[i].Free;
  ballMaterial.Free;
  rockMaterial.Free;
end.
```

---

## Bounce Physics with Classes

### Bouncing Objects Off Surfaces

```pascal
var
  ball: TCirclePhysicsObject;
  ballMaterial: TBallMaterial;
  wall: TSurface;
begin
  ballMaterial := TBallMaterial.Create;
  ball := TCirclePhysicsObject.Create(100, 100, 25, ballMaterial);
  
  // Create solid wall surface
  wall := CreateSolidSurface;
  
  // Bounce off wall (normal pointing away from wall: right = (1, 0))
  BounceOffSurface(ball, wall, FIXED16_ONE, 0);
  
  // Clean up
  ball.Free;
  ballMaterial.Free;
end.
```

### Bouncing Two Objects

```pascal
var
  ball1, ball2: TCirclePhysicsObject;
  ballMaterial: TBallMaterial;
  fragments1, fragments2: TFragmentArray;
begin
  ballMaterial := TBallMaterial.Create;
  
  ball1 := TCirclePhysicsObject.Create(100, 100, 25, ballMaterial);
  ball2 := TCirclePhysicsObject.Create(150, 100, 25, ballMaterial);
  
  ball1.VX := IntToFixed16(10);
  ball2.VX := IntToFixed16(-10);
  
  // Bounce objects off each other
  // Normal from ball1 to ball2: (1, 0) = right
  BounceObjects(ball1, ball2, FIXED16_ONE, 0, fragments1, fragments2);
  
  // Clean up
  ball1.Free;
  ball2.Free;
  ballMaterial.Free;
end.
```

### Fragmentation with Classes

```pascal
var
  glass1, glass2: TPolygonPhysicsObject;
  glassMaterial: TGlassMaterial;
  fragments1, fragments2: TFragmentArray;
  collisionVelocity: Fixed16;
begin
  glassMaterial := TGlassMaterial.Create;
  
  glass1 := TPolygonPhysicsObject.Create(100, 100, polygon1, glassMaterial);
  glass2 := TPolygonPhysicsObject.Create(150, 100, polygon2, glassMaterial);
  
  // Calculate collision velocity
  collisionVelocity := Fixed16Abs(glass1.VX - glass2.VX) + Fixed16Abs(glass1.VY - glass2.VY);
  
  // Calculate fragment count (polymorphic call)
  var fragmentCount1: integer;
  var fragmentCount2: integer;
  fragmentCount1 := glass1.CalculateFragmentCount(collisionVelocity);
  fragmentCount2 := glass2.CalculateFragmentCount(collisionVelocity);
  
  // Create fragments (polymorphic call - each class handles its own shape)
  glass1.CreateFragments(fragmentCount1, fragments1);
  glass2.CreateFragments(fragmentCount2, fragments2);
  
  // fragments1 and fragments2 now contain the broken pieces
  
  // Clean up
  glass1.Free;
  glass2.Free;
  glassMaterial.Free;
end.
```

---

## Conversion Between Records and Classes

### Record to Class

```pascal
var
  rec: TPhysicsObject;
  obj: TPhysicsObjectBase;
  material: TMaterial;
begin
  // Initialize record (from ECS or procedural code)
  rec.X := IntToFixed16(100);
  rec.Y := IntToFixed16(100);
  rec.ShapeType := stCircle;
  rec.CircleRadius := IntToFixed16(50);
  // ... set other properties ...
  
  // Create material from record properties
  material := CreateMaterialFromRecord(rec);
  
  // Convert record to class
  obj := CreatePhysicsObjectFromRecord(rec, material);
  
  // Now use OO API
  obj.Update(FIXED16_ONE);
  
  // Clean up
  obj.Free;
  material.Free;
end.
```

### Class to Record

```pascal
var
  ball: TCirclePhysicsObject;
  ballMaterial: TBallMaterial;
  rec: TPhysicsObject;
begin
  ballMaterial := TBallMaterial.Create;
  ball := TCirclePhysicsObject.Create(100, 100, 25, ballMaterial);
  
  // Convert class to record (for ECS or procedural code)
  rec := ball.ToRecord;
  
  // Now use procedural API
  UpdatePhysics(rec);
  
  // Convert back to class if needed
  ball.FromRecord(rec);
  
  // Clean up
  ball.Free;
  ballMaterial.Free;
end.
```

---

## Material Property Queries

### Checking Material Properties

```pascal
var
  obj: TPhysicsObjectBase;
  material: TMaterial;
begin
  material := TRockMaterial.Create;
  obj := TCirclePhysicsObject.Create(100, 100, 25, material);
  
  // Query material properties (polymorphic)
  if obj.IsSolid then
    WriteLn('Object is solid');
  
  if obj.IsBouncy then
    WriteLn('Object is bouncy');
  
  if obj.IsBrittle then
    WriteLn('Object is brittle');
  
  // Clean up
  obj.Free;
  material.Free;
end.
```

---

## Complete Example: Game Loop with OO Physics

```pascal
program PhysicsGame;

uses Physics, Math;

var
  ball: TCirclePhysicsObject;
  block: TPolygonPhysicsObject;
  ballMaterial: TBallMaterial;
  rockMaterial: TRockMaterial;
  deltaTime: Fixed16;
  i: integer;
begin
  // Create materials
  ballMaterial := TBallMaterial.Create;
  rockMaterial := TRockMaterial.Create;
  
  // Create physics objects
  ball := TCirclePhysicsObject.Create(100, 100, 25, ballMaterial);
  block := TPolygonPhysicsObject.Create(200, 200, somePolygon, rockMaterial);
  
  // Game loop
  deltaTime := FIXED16_ONE;
  for i := 0 to 1000 do
  begin
    // Update physics (polymorphic)
    ball.Update(deltaTime);
    block.Update(deltaTime);
    
    // Apply gravity (polymorphic)
    ball.ApplyGravity;
    block.ApplyGravity;
    
    // Apply friction (polymorphic)
    ball.ApplyFriction;
    block.ApplyFriction;
    
    // Check collision and bounce
    if CheckCollision(ball, block) then
    begin
      var fragments1, fragments2: TFragmentArray;
      BounceObjects(ball, block, normalX, normalY, fragments1, fragments2);
    end;
    
    // Render...
  end;
  
  // Clean up
  ball.Free;
  block.Free;
  ballMaterial.Free;
  rockMaterial.Free;
end.
```

---

## Benefits of OO API

### 1. Polymorphism
- Use `TPhysicsObjectBase` for all objects
- No runtime type checking needed
- Compile-time type safety

### 2. Extensibility
- Easy to add new physics object types
- Easy to add new material types
- Override only what's different

### 3. Code Organization
- Related properties and behavior grouped together
- Clear inheritance hierarchy
- Separation of concerns

### 4. Maintainability
- Changes to base class propagate automatically
- Virtual methods provide clear contract
- Easier to test and debug

---

**Last Updated:** 2025-01-XX  
**Status:** 📚 Documentation - Complete

