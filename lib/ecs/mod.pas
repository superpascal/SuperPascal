unit ECS;

interface

// Main ECS module - Rust-style mod.pas pattern
// This is the entry point for the ECS library
// Users import this unit to get all ECS functionality

// Import all sub-modules
uses
  ECS_Types,
  ECS_World,
  ECS_Entity,
  ECS_Component,
  ECS_Query,
  ECS_System,
  ECS_Physics;

// Re-export types for convenience (users can use TEntity directly)
type
  TEntity = ECS_Types.TEntity;
  TComponentMask = ECS_Types.TComponentMask;
  TComponentID = ECS_Types.TComponentID;
  TPosition = ECS_Types.TPosition;
  TVelocity = ECS_Types.TVelocity;
  TAcceleration = ECS_Types.TAcceleration;
  TMaterialProperties = ECS_Types.TMaterialProperties;
  TShape = ECS_Types.TShape;
  TSprite = ECS_Types.TSprite;
  TPhysicsBody = ECS_Types.TPhysicsBody;
  TParticle = ECS_Types.TParticle;
  TWorld = ECS_World.TWorld;
  TQuery = ECS_Query.TQuery;
  TSystem = ECS_System.TSystem;
  Fixed16 = ECS_Types.Fixed16;

// Re-export constants for convenience
const
  ENTITY_NULL = ECS_Types.ENTITY_NULL;
  MAX_ENTITIES = ECS_Types.MAX_ENTITIES;
  COMPONENT_POSITION = ECS_Types.COMPONENT_POSITION;
  COMPONENT_VELOCITY = ECS_Types.COMPONENT_VELOCITY;
  COMPONENT_ACCELERATION = ECS_Types.COMPONENT_ACCELERATION;
  COMPONENT_MATERIAL = ECS_Types.COMPONENT_MATERIAL;
  COMPONENT_SHAPE = ECS_Types.COMPONENT_SHAPE;
  COMPONENT_SPRITE = ECS_Types.COMPONENT_SPRITE;
  COMPONENT_PHYSICS = ECS_Types.COMPONENT_PHYSICS;
  COMPONENT_PARTICLE = ECS_Types.COMPONENT_PARTICLE;
  MAX_COMPONENTS = ECS_Types.MAX_COMPONENTS;
  MAX_SYSTEMS = ECS_Types.MAX_SYSTEMS;

// All functions are available through the imported units:
// - ECS_World: WorldInit, WorldCleanup, WorldUpdate
// - ECS_Entity: EntityCreate, EntityDestroy, EntityIsValid
// - ECS_Component: ComponentAdd, ComponentRemove, ComponentHas, ComponentGet*, ComponentSet*
// - ECS_Query: QueryCreate, QueryNext, QueryReset
// - ECS_System: SystemRegister, SystemRunAll, SystemGetCount
// - ECS_Physics: MovementSystem, GravitySystem, VelocitySystem, FrictionSystem, ParticleSystem
// - ECS_Types: IntToFixed16, Fixed16ToInt, Fixed16Mul

// Users can call functions directly:
//   WorldInit(world);
//   EntityCreate(world);
//   etc.

implementation

end.
