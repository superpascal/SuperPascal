unit Physics_ECS_Integration;

interface

// ECS-Physics Library Integration
// Source: ECS_COMPATIBILITY_AUDIT.md
//
// Provides conversion utilities and integration functions between the Physics
// library and the ECS system.

uses
  Physics_Types,
  ECS_Types,
  Math_Types;

// ============================================================================
// ECS Component Extensions (for compatibility)
// ============================================================================

// Extended components that match Physics library structure
type
  // Acceleration component (missing in ECS)
  TAcceleration = record
    AX, AY: Fixed16;  // Acceleration in X and Y
  end;
  
  // Material properties component (missing in ECS)
  TMaterialProperties = record
    Bouncability: Fixed16;   // 0-256 (0.0-1.0)
    Solidness: Fixed16;      // 0-256 (0.0-1.0)
    Brittleness: Fixed16;    // 0-256 (0.0-1.0)
    Fragmentability: Fixed16; // 0-256 (0.0-1.0)
  end;
  
  // Shape component (missing in ECS)
  TShape = record
    ShapeType: TShapeType;   // Circle or Polygon
    CircleRadius: Fixed16;   // For circles
    Polygon: TPolygon2D;     // For polygons
    PolygonArea: Fixed16;    // Cached area
  end;

// ============================================================================
// Conversion Utilities: ECS → Physics Library
// ============================================================================

// Convert ECS components to TPhysicsObject
// Note: ECS uses separate components, Physics uses unified record
function ECSComponentsToPhysicsObject(
  const pos: TPosition;
  const vel: TVelocity;
  const accel: TAcceleration;
  const material: TMaterialProperties;
  const shape: TShape
): TPhysicsObject;

// Convert ECS components (with optional fields)
function ECSComponentsToPhysicsObjectPartial(
  const pos: TPosition;
  const vel: TVelocity;
  const material: TMaterialProperties;
  const shape: TShape
): TPhysicsObject;

// ============================================================================
// Conversion Utilities: Physics Library → ECS
// ============================================================================

// Convert TPhysicsObject to ECS components
procedure PhysicsObjectToECSComponents(
  const obj: TPhysicsObject;
  var pos: TPosition;
  var vel: TVelocity;
  var accel: TAcceleration;
  var material: TMaterialProperties;
  var shape: TShape
);

// Convert TPhysicsObject to ECS components (partial - only position/velocity)
procedure PhysicsObjectToECSComponentsPartial(
  const obj: TPhysicsObject;
  var pos: TPosition;
  var vel: TVelocity
);

// ============================================================================
// ECS Component Compatibility Helpers
// ============================================================================

// Convert ECS TVelocity (DX, DY) to Physics (VX, VY)
procedure ECSVelocityToPhysics(
  const ecsVel: TVelocity;
  var physicsVel: record VX, VY: Fixed16; end
);

// Convert Physics (VX, VY) to ECS TVelocity (DX, DY)
procedure PhysicsVelocityToECS(
  const physicsVel: record VX, VY: Fixed16; end;
  var ecsVel: TVelocity
);

// Convert ECS TPosition (Integer) to Physics (Fixed16)
procedure ECSPositionToPhysics(
  const ecsPos: TPosition;
  var physicsPos: record X, Y: Fixed16; end
);

// Convert Physics (Fixed16) to ECS TPosition (Integer)
procedure PhysicsPositionToECS(
  const physicsPos: record X, Y: Fixed16; end;
  var ecsPos: TPosition
);

implementation

// ============================================================================
// Conversion Implementation: ECS → Physics Library
// ============================================================================

function ECSComponentsToPhysicsObject(
  const pos: TPosition;
  const vel: TVelocity;
  const accel: TAcceleration;
  const material: TMaterialProperties;
  const shape: TShape
): TPhysicsObject;
begin
  Result.X := pos.X;
  Result.Y := pos.Y;
  
  // Handle velocity name mismatch (DX, DY → VX, VY)
  Result.VX := vel.DX;
  Result.VY := vel.DY;
  
  Result.AX := accel.AX;
  Result.AY := accel.AY;
  
  Result.Bouncability := material.Bouncability;
  Result.Solidness := material.Solidness;
  Result.Brittleness := material.Brittleness;
  Result.Fragmentability := material.Fragmentability;
  
  Result.ShapeType := shape.ShapeType;
  Result.CircleRadius := shape.CircleRadius;
  Result.Polygon := shape.Polygon;
  Result.PolygonArea := shape.PolygonArea;
end;

function ECSComponentsToPhysicsObjectPartial(
  const pos: TPosition;
  const vel: TVelocity;
  const material: TMaterialProperties;
  const shape: TShape
): TPhysicsObject;
var
  accel: TAcceleration;
begin
  // Use zero acceleration if not provided
  accel.AX := 0;
  accel.AY := 0;
  Result := ECSComponentsToPhysicsObject(pos, vel, accel, material, shape);
end;

// ============================================================================
// Conversion Implementation: Physics Library → ECS
// ============================================================================

procedure PhysicsObjectToECSComponents(
  const obj: TPhysicsObject;
  var pos: TPosition;
  var vel: TVelocity;
  var accel: TAcceleration;
  var material: TMaterialProperties;
  var shape: TShape
);
begin
  pos.X := obj.X;
  pos.Y := obj.Y;
  
  // Handle velocity name mismatch (VX, VY → DX, DY)
  vel.DX := obj.VX;
  vel.DY := obj.VY;
  
  accel.AX := obj.AX;
  accel.AY := obj.AY;
  
  material.Bouncability := obj.Bouncability;
  material.Solidness := obj.Solidness;
  material.Brittleness := obj.Brittleness;
  material.Fragmentability := obj.Fragmentability;
  
  shape.ShapeType := obj.ShapeType;
  shape.CircleRadius := obj.CircleRadius;
  shape.Polygon := obj.Polygon;
  shape.PolygonArea := obj.PolygonArea;
end;

procedure PhysicsObjectToECSComponentsPartial(
  const obj: TPhysicsObject;
  var pos: TPosition;
  var vel: TVelocity
);
begin
  pos.X := obj.X;
  pos.Y := obj.Y;
  vel.DX := obj.VX;
  vel.DY := obj.VY;
end;

// ============================================================================
// ECS Component Compatibility Helpers
// ============================================================================

procedure ECSVelocityToPhysics(
  const ecsVel: TVelocity;
  var physicsVel: record VX, VY: Fixed16; end
);
begin
  physicsVel.VX := ecsVel.DX;
  physicsVel.VY := ecsVel.DY;
end;

procedure PhysicsVelocityToECS(
  const physicsVel: record VX, VY: Fixed16; end;
  var ecsVel: TVelocity
);
begin
  ecsVel.DX := physicsVel.VX;
  ecsVel.DY := physicsVel.VY;
end;

procedure ECSPositionToPhysics(
  const ecsPos: TPosition;
  var physicsPos: record X, Y: Fixed16; end
);
begin
  // Handle type conversion (Integer → Fixed16)
  physicsPos.X := ecsPos.X;
  physicsPos.Y := ecsPos.Y;
end;

procedure PhysicsPositionToECS(
  const physicsPos: record X, Y: Fixed16; end;
  var ecsPos: TPosition
);
begin
  // Handle type conversion (Fixed16 → Integer)
  ecsPos.X := physicsPos.X;
  ecsPos.Y := physicsPos.Y;
end;

end.

