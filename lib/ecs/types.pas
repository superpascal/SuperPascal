unit ECS_Types;

interface

type
  // Entity ID
  TEntity = Word;
  
  // Component mask (bitmask for component presence)
  TComponentMask = set of Byte;
  
  // Component type IDs
  TComponentID = Byte;
  
  // Fixed-point type (for no-FPU platforms)
  // Note: Uses SmallInt (Q8.8 format) to match Physics library
  Fixed16 = SmallInt;  // Q8.8 fixed-point format (matches Physics library)

const
  // Entity constants
  ENTITY_NULL = $FFFF;  // Invalid entity ID
  MAX_ENTITIES = 256;   // Maximum entities (configurable per platform)
  
  // Component IDs
  COMPONENT_POSITION = 0;
  COMPONENT_VELOCITY = 1;
  COMPONENT_ACCELERATION = 2;
  COMPONENT_MATERIAL = 3;
  COMPONENT_SHAPE = 4;
  COMPONENT_SPRITE = 5;
  COMPONENT_PHYSICS = 6;
  COMPONENT_PARTICLE = 7;
  MAX_COMPONENTS = 32;  // Maximum component types
  
  // System constants
  MAX_SYSTEMS = 32;     // Maximum registered systems

// Component type definitions
type
  TPosition = record
    X, Y: Fixed16;  // Standardized to Fixed16 (Q8.8) for consistency with Physics library
  end;
  
  TVelocity = record
    VX, VY: Fixed16;  // Renamed from DX,DY to VX,VY for consistency with Physics library
  end;
  
  // Acceleration component (new - required for Physics library compatibility)
  TAcceleration = record
    AX, AY: Fixed16;  // Acceleration in X and Y
  end;
  
  // Material properties component (new - required for Physics library compatibility)
  TMaterialProperties = record
    Bouncability: Fixed16;   // 0-256 (0.0-1.0) - how bouncy
    Solidness: Fixed16;      // 0-256 (0.0-1.0) - how solid/rigid
    Brittleness: Fixed16;    // 0-256 (0.0-1.0) - how brittle (prone to breaking)
    Fragmentability: Fixed16; // 0-256 (0.0-1.0) - how many fragments when breaking
  end;
  
  // Shape component (new - required for Physics library compatibility)
  // Note: Uses TShapeType from Physics_Types for consistency
  TShapeType = (stCircle, stPolygon);  // Circle or Polygon (from Physics_Types)
  
  TShape = record
    ShapeType: TShapeType;   // Circle or Polygon
    CircleRadius: Fixed16;   // For circles
    Polygon: record
      Points: array[0..31] of record
        X, Y: Integer;  // Polygon vertex positions (TPoint2D from Collision_Types)
      end;
      Count: Byte;  // Number of vertices
    end;
    PolygonArea: Fixed16;    // Cached polygon area (for area conservation)
  end;
  
  TSprite = record
    SpriteID: Byte;
    TileID: Word;
    Palette: Byte;
    Visible: Boolean;
    // Physics integration properties
    // Note: For full physics, entities should also have Position, Velocity, Material, Shape components
    Explodable: Boolean;     // Can this sprite explode?
    ExplosionRadius: Fixed16; // Explosion radius if explodable
    ExplosionDamage: Integer; // Damage dealt on explosion
  end;
  
  TPhysicsBody = record
    Mass: Integer;
    Friction: Integer;    // Fixed-point
    Gravity: Integer;
    OnGround: Boolean;
  end;
  
  TParticle = record
    X, Y: Fixed16;
    VX, VY: Fixed16;
    Energy: Integer;
    Color: Byte;
    Size: Byte;
    Active: Boolean;
  end;

// Note: Fixed-point helper functions are provided by Math_Fixed unit
// Users should import Math_Fixed for fixed-point operations
// These are kept here for backward compatibility but delegate to Math_Fixed

implementation

uses
  Math_Fixed,
  Physics_Types;  // For TShapeType

// Fixed-point helper functions (delegate to Math_Fixed)
function IntToFixed16(i: Integer): Fixed16;
begin
  Result := Math_Fixed.IntToFixed16(i);
end;

function Fixed16ToInt(f: Fixed16): Integer;
begin
  Result := Math_Fixed.Fixed16ToInt(f);
end;

function Fixed16Mul(a, b: Fixed16): Fixed16;
begin
  Result := Math_Fixed.Fixed16Mul(a, b);
end;

end.

