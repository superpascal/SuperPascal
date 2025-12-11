unit Physics;

interface

// SuperPascal Physics Library
// Source: algorithms/06_PhysicsSimulation.md
//
// Physics simulation utilities for games. These algorithms are generic and
// work on all platforms. This library provides standalone physics utilities
// that complement the ECS physics systems in lib/ecs/physics.pas.
//
// Key Features:
// - Particle systems (explosions, ground impacts)
// - Gravity simulation
// - Friction and air resistance
// - Velocity and acceleration integration
// - Platform-agnostic (all platforms)
// - Fixed-point arithmetic (no FPU required)

// Import all physics modules
uses
  Physics_Types,
  Physics_Particle,
  Physics_Gravity,
  Physics_Friction,
  Physics_Bounce,
  Physics_BounceOO,
  Physics_Material,
  Physics_ObjectBase,
  Physics_ObjectCircle,
  Physics_ObjectPolygon,
  Physics_Conversion;

// ============================================================================
// Re-export Types
// ============================================================================

// ============================================================================
// Re-export Record Types (for ECS compatibility)
// ============================================================================

type
  TParticle = Physics_Types.TParticle;
  TParticleArray = Physics_Types.TParticleArray;
  TPhysicsObject = Physics_Types.TPhysicsObject;
  TSurface = Physics_Types.TSurface;
  TFragment = Physics_Bounce.TFragment;
  TFragmentArray = Physics_Bounce.TFragmentArray;

// ============================================================================
// Re-export Class Types (for OO usage)
// ============================================================================

type
  TMaterial = Physics_Material.TMaterial;
  TRockMaterial = Physics_Material.TRockMaterial;
  TGlassMaterial = Physics_Material.TGlassMaterial;
  TBallMaterial = Physics_Material.TBallMaterial;
  TBubbleMaterial = Physics_Material.TBubbleMaterial;
  TWoodMaterial = Physics_Material.TWoodMaterial;
  TMetalMaterial = Physics_Material.TMetalMaterial;
  TRubberMaterial = Physics_Material.TRubberMaterial;
  TSpongeMaterial = Physics_Material.TSpongeMaterial;
  
  TPhysicsObjectBase = Physics_ObjectBase.TPhysicsObjectBase;
  TCirclePhysicsObject = Physics_ObjectCircle.TCirclePhysicsObject;
  TPolygonPhysicsObject = Physics_ObjectPolygon.TPolygonPhysicsObject;

// ============================================================================
// Re-export Constants
// ============================================================================

const
  PHYSICS_GRAVITY = Physics_Types.PHYSICS_GRAVITY;
  PHYSICS_JUMP_VELOCITY = Physics_Types.PHYSICS_JUMP_VELOCITY;
  PHYSICS_FRICTION_COEFFICIENT = Physics_Types.PHYSICS_FRICTION_COEFFICIENT;
  PHYSICS_AIR_RESISTANCE = Physics_Types.PHYSICS_AIR_RESISTANCE;
  PHYSICS_VELOCITY_THRESHOLD = Physics_Types.PHYSICS_VELOCITY_THRESHOLD;
  PHYSICS_DEFAULT_ACCELERATION = Physics_Types.PHYSICS_DEFAULT_ACCELERATION;
  PHYSICS_SOLIDNESS_THRESHOLD = Physics_Types.PHYSICS_SOLIDNESS_THRESHOLD;
  PHYSICS_BOUNCABILITY_THRESHOLD = Physics_Types.PHYSICS_BOUNCABILITY_THRESHOLD;

// ============================================================================
// Re-export Functions
// ============================================================================
// All functions are available through the imported units
// Users can call: UpdateParticle(...), ApplyGravity(...), etc.

implementation

end.

