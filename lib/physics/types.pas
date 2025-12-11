unit Physics_Types;

interface

// Core physics types and constants
// Source: algorithms/06_PhysicsSimulation.md

uses
  Math_Types,
  Collision_Types;

// ============================================================================
// Particle Types
// ============================================================================

type
  TParticle = record
    X, Y: Fixed16;           // Position
    VX, VY: Fixed16;         // Velocity
    Energy: integer;         // Lifetime/energy (frames)
    Color: byte;             // Particle color
    Size: byte;              // Particle size
    Active: boolean;         // Is particle active?
  end;

  TParticleArray = array of TParticle;

// ============================================================================
// Physics Object Types
// ============================================================================

// Shape types for physics objects
type
  TShapeType = (stCircle, stPolygon);

type
  TPhysicsObject = record
    X, Y: Fixed16;           // Position
    VX, VY: Fixed16;         // Velocity
    AX, AY: Fixed16;         // Acceleration
    Bouncability: Fixed16;   // 0-256 (0.0-1.0) - how bouncy the object is
    Solidness: Fixed16;      // 0-256 (0.0-1.0) - how solid/rigid the object is
    Brittleness: Fixed16;    // 0-256 (0.0-1.0) - how brittle (prone to breaking)
    Fragmentability: Fixed16; // 0-256 (0.0-1.0) - how many fragments when breaking
    
    // Shape properties
    ShapeType: TShapeType;   // Circle or Polygon
    
    // Circle properties (if ShapeType = stCircle)
    CircleRadius: Fixed16;    // Radius of circle
    
    // Polygon properties (if ShapeType = stPolygon)
    Polygon: TPolygon2D;     // Polygon vertices (from Collision_Types)
    PolygonArea: Fixed16;   // Cached polygon area (for area conservation)
  end;

// Surface/Material properties
type
  TSurface = record
    Solidness: Fixed16;      // 0-256 (0.0-1.0) - how solid (wall/floor/ceiling)
    Absorbability: Fixed16;  // 0-256 (0.0-1.0) - how much energy it absorbs
  end;

// ============================================================================
// Physics Constants
// ============================================================================

// Gravity constant (Q8.8 format)
// Value: 0.03 pixels/frame² in Q8.8 = 7.68 (approximately 8)
// For Q8.8: 0.03 * 256 = 7.68 ≈ 8
const
  PHYSICS_GRAVITY: Fixed16 = 8;  // 0.03125 in Q8.8 format

// Jump velocity (Q8.8 format)
// Value: -0.4 pixels/frame in Q8.8 = -102.4 ≈ -102
const
  PHYSICS_JUMP_VELOCITY: Fixed16 = -102;  // -0.3984375 in Q8.8 format

// Friction coefficient (Q8.8 format)
// Value: 0.9 in Q8.8 = 230.4 ≈ 230
const
  PHYSICS_FRICTION_COEFFICIENT: Fixed16 = 230;  // 0.8984375 in Q8.8 format

// Air resistance (Q8.8 format)
// Value: 0.95 in Q8.8 = 243.2 ≈ 243
const
  PHYSICS_AIR_RESISTANCE: Fixed16 = 243;  // 0.94921875 in Q8.8 format

// Velocity threshold for stopping (Q8.8 format)
// Value: 0.01 in Q8.8 = 2.56 ≈ 3
const
  PHYSICS_VELOCITY_THRESHOLD: Fixed16 = 3;  // 0.01171875 in Q8.8 format

// Default acceleration (Q8.8 format)
const
  PHYSICS_DEFAULT_ACCELERATION: Fixed16 = 5;  // 0.01953125 in Q8.8 format

// Bounce physics thresholds (Q8.8 format)
const
  PHYSICS_SOLIDNESS_THRESHOLD: Fixed16 = 200;  // 0.78125 - above this = "solid"
  PHYSICS_BOUNCABILITY_THRESHOLD: Fixed16 = 128;  // 0.5 - above this = "bouncy"
  PHYSICS_BRITTLENESS_THRESHOLD: Fixed16 = 128;  // 0.5 - above this = "brittle"
  
// Fragment limits
const
  PHYSICS_MIN_FRAGMENTS: integer = 2;   // Minimum fragments
  PHYSICS_MAX_FRAGMENTS: integer = 32;  // Maximum fragments
  PHYSICS_BASE_FRAGMENTS: integer = 2;  // Base fragment count
  PHYSICS_MAX_FRAGMENT_VELOCITY: Fixed16 = 25600;  // 100.0 in Q8.8 - max velocity for fragment calculation
  
// Default materials
const
  PHYSICS_MATERIAL_SOLID_SOLIDNESS: Fixed16 = 256;  // 1.0 - fully solid
  PHYSICS_MATERIAL_SOLID_ABSORBABILITY: Fixed16 = 0;  // 0.0 - no absorption
  
  PHYSICS_MATERIAL_SPONGE_SOLIDNESS: Fixed16 = 64;  // 0.25 - soft
  PHYSICS_MATERIAL_SPONGE_ABSORBABILITY: Fixed16 = 200;  // 0.78125 - high absorption
  
  PHYSICS_MATERIAL_BOUNCY_SOLIDNESS: Fixed16 = 128;  // 0.5 - medium
  PHYSICS_MATERIAL_BOUNCY_ABSORBABILITY: Fixed16 = 0;  // 0.0 - no absorption

implementation

end.

