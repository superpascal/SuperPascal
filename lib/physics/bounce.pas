unit Physics_Bounce;

interface

// Bounce physics utilities
// Source: BOUNCE_DESIGN.md
//
// Handles collisions between objects with different material properties:
// - Bouncability: How bouncy an object is (0 = no bounce, 1 = perfect bounce)
// - Solidness: How solid/rigid an object is (0 = soft, 1 = solid)
// - Brittleness: How brittle an object is (0 = not brittle, 1 = very brittle)
// - Fragmentability: How many fragments when breaking (0 = few, 1 = many)
// - Absorbability: How much energy a surface absorbs (0 = no absorption, 1 = full)

uses
  Physics_Types,
  Math_Types,
  Math_Fixed,
  Math_Trig,
  Collision_Types;

// ============================================================================
// Surface Creation
// ============================================================================

// Create surface with properties
function CreateSurface(solidness, absorbability: Fixed16): TSurface;

// Create default materials
function CreateSolidSurface: TSurface;      // Wall/floor/ceiling
function CreateSpongeSurface: TSurface;    // Absorbable material
function CreateBouncySurface: TSurface;    // Bouncy surface

// ============================================================================
// Bounce Calculations
// ============================================================================

// Calculate effective bounce coefficient for object hitting surface
function CalculateEffectiveBounce(
  const obj: TPhysicsObject;
  const surface: TSurface
): Fixed16;

// Calculate effective bounce coefficient for two objects colliding
function CalculateEffectiveBounceObjects(
  const obj1, obj2: TPhysicsObject
): Fixed16;

// Check if object is "solid" (above threshold)
function IsSolid(const obj: TPhysicsObject): boolean;

// Check if object is "bouncy" (above threshold)
function IsBouncy(const obj: TPhysicsObject): boolean;

// ============================================================================
// Bounce Functions
// ============================================================================

// Bounce object off surface (wall/floor/ceiling)
// normalX, normalY: Collision normal (direction away from surface)
procedure BounceOffSurface(
  var obj: TPhysicsObject;
  const surface: TSurface;
  normalX, normalY: Fixed16
);

// Fragment types
type
  TFragment = record
    X, Y: Fixed16;      // Position (center for circle, centroid for polygon)
    VX, VY: Fixed16;    // Velocity
    Angle: integer;     // Fragment angle (degrees)
    
    // Shape data
    ShapeType: TShapeType;  // Circle or Polygon
    
    // Circle fragment (if ShapeType = stCircle)
    CircleRadius: Fixed16;   // Radius of circular fragment
    CircleArea: Fixed16;     // Area of circle (π * r²) - for area conservation
    
    // Polygon fragment (if ShapeType = stPolygon)
    Polygon: TPolygon2D;     // Polygon vertices
    PolygonArea: Fixed16;    // Area of polygon - for area conservation
  end;

  TFragmentArray = array of TFragment;

// Calculate fragment count based on Fragmentability and collision velocity
function CalculateFragmentCount(
  const obj: TPhysicsObject;
  collisionVelocity: Fixed16
): integer;

// Create fragments from object (with polygon/circle support)
procedure CreateFragments(
  const obj: TPhysicsObject;
  fragmentCount: integer;
  var fragments: TFragmentArray
);

// Fragment circle into smaller circles (area conservation)
function FragmentCircle(
  const obj: TPhysicsObject;
  fragmentCount: integer;
  var fragments: TFragmentArray
): boolean;

// Fragment polygon into smaller polygons (area conservation)
function FragmentPolygon(
  const obj: TPhysicsObject;
  fragmentCount: integer;
  var fragments: TFragmentArray
): boolean;

// Calculate polygon area (shoelace formula)
function CalculatePolygonArea(const polygon: TPolygon2D): Fixed16;

// Verify area conservation (for testing)
function VerifyAreaConservation(
  const original: TPhysicsObject;
  const fragments: TFragmentArray
): boolean;

// Bounce two objects off each other
// normalX, normalY: Collision normal (direction from obj1 to obj2)
// fragments1, fragments2: Output fragments if objects are brittle
procedure BounceObjects(
  var obj1, obj2: TPhysicsObject;
  normalX, normalY: Fixed16;
  var fragments1, fragments2: TFragmentArray
);

// ============================================================================
// Helper Functions
// ============================================================================

// Reflect velocity along normal
procedure ReflectVelocity(
  var vx, vy: Fixed16;
  normalX, normalY: Fixed16;
  bounceCoefficient: Fixed16
);

implementation

// Create surface with properties
function CreateSurface(solidness, absorbability: Fixed16): TSurface;
begin
  Result.Solidness := solidness;
  Result.Absorbability := absorbability;
end;

// Create default materials
function CreateSolidSurface: TSurface;
begin
  Result.Solidness := PHYSICS_MATERIAL_SOLID_SOLIDNESS;
  Result.Absorbability := PHYSICS_MATERIAL_SOLID_ABSORBABILITY;
end;

function CreateSpongeSurface: TSurface;
begin
  Result.Solidness := PHYSICS_MATERIAL_SPONGE_SOLIDNESS;
  Result.Absorbability := PHYSICS_MATERIAL_SPONGE_ABSORBABILITY;
end;

function CreateBouncySurface: TSurface;
begin
  Result.Solidness := PHYSICS_MATERIAL_BOUNCY_SOLIDNESS;
  Result.Absorbability := PHYSICS_MATERIAL_BOUNCY_ABSORBABILITY;
end;

// Calculate effective bounce coefficient for object hitting surface
function CalculateEffectiveBounce(
  const obj: TPhysicsObject;
  const surface: TSurface
): Fixed16;
var
  baseBounce: Fixed16;
begin
  // Base bounce = object's bouncability
  baseBounce := obj.Bouncability;
  
  // Reduce by surface absorbability
  // effectiveBounce = bouncability × (1 - absorbability)
  Result := Fixed16Mul(baseBounce, Fixed16Sub(FIXED16_ONE, surface.Absorbability));
end;

// Calculate effective bounce coefficient for two objects colliding
function CalculateEffectiveBounceObjects(
  const obj1, obj2: TPhysicsObject
): Fixed16;
begin
  // Use minimum bouncability of the two objects
  if obj1.Bouncability < obj2.Bouncability then
    Result := obj1.Bouncability
  else
    Result := obj2.Bouncability;
end;

// Check if object is "solid" (above threshold)
function IsSolid(const obj: TPhysicsObject): boolean;
begin
  Result := obj.Solidness >= PHYSICS_SOLIDNESS_THRESHOLD;
end;

// Check if object is "bouncy" (above threshold)
function IsBouncy(const obj: TPhysicsObject): boolean;
begin
  Result := obj.Bouncability >= PHYSICS_BOUNCABILITY_THRESHOLD;
end;

// Check if object is "brittle" (above threshold)
function IsBrittle(const obj: TPhysicsObject): boolean;
begin
  Result := obj.Brittleness >= PHYSICS_BRITTLENESS_THRESHOLD;
end;

// Reflect velocity along normal
procedure ReflectVelocity(
  var vx, vy: Fixed16;
  normalX, normalY: Fixed16;
  bounceCoefficient: Fixed16
);
var
  dotProduct: Fixed16;
  twoTimesDot: Fixed16;
  reflectedVX, reflectedVY: Fixed16;
begin
  // Calculate dot product: v · n
  dotProduct := Fixed16Mul(vx, normalX) + Fixed16Mul(vy, normalY);
  
  // Calculate 2(v · n)
  // 2 in Q8.8 = 512 (2 * 256)
  twoTimesDot := Fixed16Mul(dotProduct, IntToFixed16(2));
  
  // Reflect: v' = v - 2(v · n)n
  reflectedVX := Fixed16Sub(vx, Fixed16Mul(twoTimesDot, normalX));
  reflectedVY := Fixed16Sub(vy, Fixed16Mul(twoTimesDot, normalY));
  
  // Apply bounce coefficient
  vx := Fixed16Mul(reflectedVX, bounceCoefficient);
  vy := Fixed16Mul(reflectedVY, bounceCoefficient);
end;

// Calculate fragment count based on Fragmentability and collision velocity
function CalculateFragmentCount(
  const obj: TPhysicsObject;
  collisionVelocity: Fixed16
): integer;
var
  baseFragments: integer;
  velocityFactor: Fixed16;
  fragmentFactor: Fixed16;
  velocityNormalized: Fixed16;
begin
  // Base fragments from Fragmentability (2-16 fragments)
  // Fragmentability 0.0 = 2 fragments, 1.0 = 16 fragments
  baseFragments := PHYSICS_BASE_FRAGMENTS + 
    Fixed16ToInt(Fixed16Mul(obj.Fragmentability, IntToFixed16(14)));
  
  // Normalize velocity to 0-1 range
  if collisionVelocity > PHYSICS_MAX_FRAGMENT_VELOCITY then
    velocityNormalized := FIXED16_ONE
  else
    velocityNormalized := Fixed16Div(collisionVelocity, PHYSICS_MAX_FRAGMENT_VELOCITY);
  
  // Velocity factor (higher velocity = more fragments)
  velocityFactor := velocityNormalized;
  
  // Brittleness factor (higher brittleness = more fragments)
  fragmentFactor := Fixed16Mul(obj.Brittleness, velocityFactor);
  
  // Final fragment count
  Result := baseFragments + Fixed16ToInt(Fixed16Mul(fragmentFactor, IntToFixed16(8)));
  
  // Clamp to reasonable range
  if Result < PHYSICS_MIN_FRAGMENTS then Result := PHYSICS_MIN_FRAGMENTS;
  if Result > PHYSICS_MAX_FRAGMENTS then Result := PHYSICS_MAX_FRAGMENTS;
end;

// Calculate polygon area (shoelace formula)
function CalculatePolygonArea(const polygon: TPolygon2D): Fixed16;
var
  i: integer;
  area: LongInt;
  x1, y1, x2, y2: integer;
begin
  if polygon.Count < 3 then
  begin
    Result := 0;
    Exit;
  end;
  
  area := 0;
  for i := 0 to polygon.Count - 1 do
  begin
    x1 := polygon.Points[i].X;
    y1 := polygon.Points[i].Y;
    if i = polygon.Count - 1 then
    begin
      x2 := polygon.Points[0].X;
      y2 := polygon.Points[0].Y;
    end
    else
    begin
      x2 := polygon.Points[i + 1].X;
      y2 := polygon.Points[i + 1].Y;
    end;
    area := area + (x1 * y2 - x2 * y1);
  end;
  
  // Convert to Fixed16 (Q8.8) and take absolute value
  Result := IntToFixed16(Abs(area div 2));
end;

// Fragment circle into smaller circles (area conservation)
function FragmentCircle(
  const obj: TPhysicsObject;
  fragmentCount: integer;
  var fragments: TFragmentArray
): boolean;
var
  originalArea: Fixed16;
  fragmentArea: Fixed16;
  fragmentRadius: Fixed16;
  angleStep: Fixed16;
  i: integer;
  angle: integer;
  parentSpeed: Fixed16;
  speedPercent: Fixed16;
  speed: Fixed16;
begin
  // Calculate original area: π * r²
  originalArea := Fixed16Mul(
    FIXED16_PI,
    Fixed16Mul(obj.CircleRadius, obj.CircleRadius)
  );
  
  // Area per fragment (equal distribution)
  fragmentArea := Fixed16Div(originalArea, IntToFixed16(fragmentCount));
  
  // Calculate fragment radius: r = sqrt(Area / π)
  // For Q8.8: r² = Area / π, so r = sqrt(Area / π)
  fragmentRadius := Fixed16Sqrt(Fixed16Div(fragmentArea, FIXED16_PI));
  
  // Angle step for positioning fragments around original center
  angleStep := Fixed16Div(FIXED16_TWO_PI, IntToFixed16(fragmentCount));
  
  SetLength(fragments, fragmentCount);
  
  // Calculate parent object speed for velocity inheritance
  parentSpeed := Fixed16Abs(obj.VX) + Fixed16Abs(obj.VY);
  
  for i := 0 to fragmentCount - 1 do
  begin
    // Position fragments in circle around original center
    // Angle for this fragment
    angle := Fixed16ToInt(Fixed16Mul(IntToFixed16(i), angleStep));
    
    // Position fragment at distance from center
    // Place fragments at radius = original_radius + fragment_radius (touching)
    fragments[i].X := obj.X + Fixed16Mul(
      Fixed16Add(obj.CircleRadius, fragmentRadius),
      CosSigned(angle)
    );
    fragments[i].Y := obj.Y + Fixed16Mul(
      Fixed16Add(obj.CircleRadius, fragmentRadius),
      SinSigned(angle)
    );
    
    // Set shape properties
    fragments[i].ShapeType := stCircle;
    fragments[i].CircleRadius := fragmentRadius;
    fragments[i].CircleArea := fragmentArea;
    fragments[i].Angle := angle;
    
    // Velocity based on parent velocity + radial component
    speedPercent := IntToFixed16(Random(50) + 50);  // 50-100% of parent velocity
    speed := Fixed16Mul(parentSpeed, speedPercent);
    speed := Fixed16Div(speed, IntToFixed16(256));  // Convert to Q8.8
    
    fragments[i].VX := obj.VX + Fixed16Mul(speed, CosSigned(angle));
    fragments[i].VY := obj.VY + Fixed16Mul(speed, SinSigned(angle));
  end;
  
  Result := True;
end;

// Fragment polygon into smaller polygons (area conservation)
// Uses simplified grid-based method for retro platforms
// Made public for use by TPolygonPhysicsObject
function FragmentPolygon(
  const obj: TPhysicsObject;
  fragmentCount: integer;
  var fragments: TFragmentArray
): boolean;
var
  originalArea: Fixed16;
  fragmentArea: Fixed16;
  i, j: integer;
  centroidX, centroidY: Fixed16;
  parentSpeed: Fixed16;
  speedPercent: Fixed16;
  speed: Fixed16;
  angle: integer;
begin
  // Use cached area or calculate
  if obj.PolygonArea > 0 then
    originalArea := obj.PolygonArea
  else
    originalArea := CalculatePolygonArea(obj.Polygon);
  
  // Area per fragment (equal distribution)
  fragmentArea := Fixed16Div(originalArea, IntToFixed16(fragmentCount));
  
  SetLength(fragments, fragmentCount);
  
  // Calculate parent object speed for velocity inheritance
  parentSpeed := Fixed16Abs(obj.VX) + Fixed16Abs(obj.VY);
  
  // Simplified approach: Create fragments as sub-polygons
  // For retro platforms, use simple subdivision
  // Each fragment gets a portion of the original polygon
  // This is a placeholder - full implementation would use Voronoi or recursive subdivision
  
  for i := 0 to fragmentCount - 1 do
  begin
    // Create simplified fragment polygon (placeholder)
    // In full implementation, this would be a proper sub-polygon
    SetLength(fragments[i].Polygon.Points, obj.Polygon.Count);
    fragments[i].Polygon.Count := obj.Polygon.Count;
    
    // Copy polygon structure (simplified - full implementation would subdivide)
    // For now, create fragments as scaled-down versions
    centroidX := 0;
    centroidY := 0;
    
    for j := 0 to obj.Polygon.Count - 1 do
    begin
      // Scale polygon points around center
      fragments[i].Polygon.Points[j].X := Fixed16ToInt(
        obj.X + Fixed16Div(
          Fixed16Sub(IntToFixed16(obj.Polygon.Points[j].X), obj.X),
          IntToFixed16(fragmentCount)
        )
      );
      fragments[i].Polygon.Points[j].Y := Fixed16ToInt(
        obj.Y + Fixed16Div(
          Fixed16Sub(IntToFixed16(obj.Polygon.Points[j].Y), obj.Y),
          IntToFixed16(fragmentCount)
        )
      );
      
      centroidX := Fixed16Add(centroidX, IntToFixed16(fragments[i].Polygon.Points[j].X));
      centroidY := Fixed16Add(centroidY, IntToFixed16(fragments[i].Polygon.Points[j].Y));
    end;
    
    // Calculate centroid
    centroidX := Fixed16Div(centroidX, IntToFixed16(obj.Polygon.Count));
    centroidY := Fixed16Div(centroidY, IntToFixed16(obj.Polygon.Count));
    
    fragments[i].X := centroidX;
    fragments[i].Y := centroidY;
    fragments[i].ShapeType := stPolygon;
    fragments[i].PolygonArea := fragmentArea;
    
    // Random angle for velocity direction
    angle := Random(360) - 180;
    
    // Velocity based on parent velocity + random spread
    speedPercent := IntToFixed16(Random(50) + 50);  // 50-100% of parent velocity
    speed := Fixed16Mul(parentSpeed, speedPercent);
    speed := Fixed16Div(speed, IntToFixed16(256));  // Convert to Q8.8
    
    fragments[i].VX := obj.VX + Fixed16Mul(speed, CosSigned(angle));
    fragments[i].VY := obj.VY + Fixed16Mul(speed, SinSigned(angle));
    fragments[i].Angle := angle;
  end;
  
  Result := True;
end;

// Create fragments from object (with polygon/circle support)
procedure CreateFragments(
  const obj: TPhysicsObject;
  fragmentCount: integer;
  var fragments: TFragmentArray
);
begin
  case obj.ShapeType of
    stCircle:
      FragmentCircle(obj, fragmentCount, fragments);
    stPolygon:
      FragmentPolygon(obj, fragmentCount, fragments);
  end;
end;

// Verify area conservation (for testing)
function VerifyAreaConservation(
  const original: TPhysicsObject;
  const fragments: TFragmentArray
): boolean;
var
  originalArea: Fixed16;
  totalFragmentArea: Fixed16;
  i: integer;
  tolerance: Fixed16;
begin
  // Calculate original area
  if original.ShapeType = stCircle then
    originalArea := Fixed16Mul(
      FIXED16_PI,
      Fixed16Mul(original.CircleRadius, original.CircleRadius)
    )
  else
  begin
    if original.PolygonArea > 0 then
      originalArea := original.PolygonArea
    else
      originalArea := CalculatePolygonArea(original.Polygon);
  end;
  
  // Sum fragment areas
  totalFragmentArea := 0;
  for i := 0 to Length(fragments) - 1 do
  begin
    if fragments[i].ShapeType = stCircle then
      totalFragmentArea := Fixed16Add(totalFragmentArea, fragments[i].CircleArea)
    else
      totalFragmentArea := Fixed16Add(totalFragmentArea, fragments[i].PolygonArea);
  end;
  
  // Check within tolerance (1% error allowed)
  tolerance := Fixed16Div(originalArea, IntToFixed16(100));
  Result := Fixed16Abs(Fixed16Sub(totalFragmentArea, originalArea)) <= tolerance;
end;

// Bounce object off surface (wall/floor/ceiling)
procedure BounceOffSurface(
  var obj: TPhysicsObject;
  const surface: TSurface;
  normalX, normalY: Fixed16
);
var
  effectiveBounce: Fixed16;
begin
  // Calculate effective bounce coefficient
  effectiveBounce := CalculateEffectiveBounce(obj, surface);
  
  // Reflect velocity along normal with bounce coefficient
  ReflectVelocity(obj.VX, obj.VY, normalX, normalY, effectiveBounce);
end;

// Bounce two objects off each other
procedure BounceObjects(
  var obj1, obj2: TPhysicsObject;
  normalX, normalY: Fixed16;
  var fragments1, fragments2: TFragmentArray
);
var
  effectiveBounce: Fixed16;
  bothSolid: boolean;
  bothBouncy: boolean;
  bothBrittle: boolean;
  smashFactor: Fixed16;  // Multiplication factor (0.1 = keep 10% of velocity)
  collisionVelocity: Fixed16;
  fragmentCount1, fragmentCount2: integer;
begin
  // Initialize fragments arrays
  SetLength(fragments1, 0);
  SetLength(fragments2, 0);
  
  bothSolid := IsSolid(obj1) and IsSolid(obj2) and 
               (obj1.Brittleness < PHYSICS_BRITTLENESS_THRESHOLD) and
               (obj2.Brittleness < PHYSICS_BRITTLENESS_THRESHOLD);
  bothBouncy := IsBouncy(obj1) and IsBouncy(obj2);
  bothBrittle := IsBrittle(obj1) and IsBrittle(obj2);
  
  // Calculate collision velocity for fragment calculation
  collisionVelocity := Fixed16Abs(obj1.VX - obj2.VX) + Fixed16Abs(obj1.VY - obj2.VY);
  
  if bothBrittle then
  begin
    // Scenario 1b: Two brittle objects → Fragment
    // Both break apart into fragments
    fragmentCount1 := CalculateFragmentCount(obj1, collisionVelocity);
    fragmentCount2 := CalculateFragmentCount(obj2, collisionVelocity);
    
    CreateFragments(obj1, fragmentCount1, fragments1);
    CreateFragments(obj2, fragmentCount2, fragments2);
    
    // Original objects are destroyed (zero velocity)
    obj1.VX := 0;
    obj1.VY := 0;
    obj2.VX := 0;
    obj2.VY := 0;
  end
  else if bothSolid then
  begin
    // Scenario 1: Two solid objects → "Smash"
    // Both lose most of their velocity (inelastic collision)
    // Keep 10% of velocity: multiply by 0.1 in Q8.8 = 25.6 ≈ 26
    smashFactor := 26;  // 0.1015625 in Q8.8 (approximately 10%)
    obj1.VX := Fixed16Mul(obj1.VX, smashFactor);
    obj1.VY := Fixed16Mul(obj1.VY, smashFactor);
    obj2.VX := Fixed16Mul(obj2.VX, smashFactor);
    obj2.VY := Fixed16Mul(obj2.VY, smashFactor);
  end
  else if (IsBouncy(obj1) and IsBrittle(obj2)) or (IsBrittle(obj1) and IsBouncy(obj2)) then
  begin
    // Scenario 2b: Bouncy + Brittle → Both bounce away (brittle doesn't break)
    effectiveBounce := CalculateEffectiveBounceObjects(obj1, obj2);
    
    // Reflect both velocities
    ReflectVelocity(obj1.VX, obj1.VY, normalX, normalY, effectiveBounce);
    ReflectVelocity(obj2.VX, obj2.VY, -normalX, -normalY, effectiveBounce);  // Opposite normal
  end
  else if bothBouncy then
  begin
    // Scenario 3: Two bouncy objects → Both bounce away
    effectiveBounce := CalculateEffectiveBounceObjects(obj1, obj2);
    
    // Reflect both velocities
    ReflectVelocity(obj1.VX, obj1.VY, normalX, normalY, effectiveBounce);
    ReflectVelocity(obj2.VX, obj2.VY, -normalX, -normalY, effectiveBounce);  // Opposite normal
  end
  else
  begin
    // Scenario 2: Mixed (one bouncy, one solid)
    // Bouncy one bounces, solid one loses velocity
    if obj1.Bouncability > obj2.Bouncability then
    begin
      // obj1 is bouncy, obj2 is solid
      effectiveBounce := CalculateEffectiveBounce(obj1, CreateSurface(obj2.Solidness, 0));
      ReflectVelocity(obj1.VX, obj1.VY, normalX, normalY, effectiveBounce);
      
      // Solid one loses velocity
      smashFactor := 26;  // 0.1015625 in Q8.8 (approximately 10%)
      obj2.VX := Fixed16Mul(obj2.VX, smashFactor);
      obj2.VY := Fixed16Mul(obj2.VY, smashFactor);
    end
    else
    begin
      // obj2 is bouncy, obj1 is solid
      effectiveBounce := CalculateEffectiveBounce(obj2, CreateSurface(obj1.Solidness, 0));
      ReflectVelocity(obj2.VX, obj2.VY, -normalX, -normalY, effectiveBounce);
      
      // Solid one loses velocity
      smashFactor := 26;  // 0.1015625 in Q8.8 (approximately 10%)
      obj1.VX := Fixed16Mul(obj1.VX, smashFactor);
      obj1.VY := Fixed16Mul(obj1.VY, smashFactor);
    end;
  end;
end;

end.

