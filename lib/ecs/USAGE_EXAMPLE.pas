program ECSExample;

// Example usage of the ECS library
// This demonstrates the Rust-style mod.pas pattern

uses ECS;  // Import main module (mod.pas), which re-exports everything

var
  world: TWorld;
  player: TEntity;
  enemy: TEntity;
  pos: TPosition;
  vel: TVelocity;
  physics: TPhysicsBody;
begin
  // Initialize world
  WorldInit(world);
  
  // Create player entity
  player := EntityCreate(world);
  
  // Add components
  ComponentAdd(world, player, COMPONENT_POSITION);
  ComponentAdd(world, player, COMPONENT_VELOCITY);
  ComponentAdd(world, player, COMPONENT_PHYSICS);
  
  // Set initial position
  pos.X := 100;
  pos.Y := 200;
  ComponentSetPosition(world, player, pos);
  
  // Set initial velocity
  vel.VX := 2;
  vel.VY := 0;
  ComponentSetVelocity(world, player, vel);
  
  // Set physics properties
  physics.Gravity := 1;
  physics.Friction := $E0;  // 0.875 fixed-point
  ComponentSetPhysics(world, player, physics);
  
  // Create enemy entity
  enemy := EntityCreate(world);
  ComponentAdd(world, enemy, COMPONENT_POSITION);
  ComponentAdd(world, enemy, COMPONENT_VELOCITY);
  
  pos.X := 300;
  pos.Y := 200;
  ComponentSetPosition(world, enemy, pos);
  
  vel.VX := -1;
  vel.VY := 0;
  ComponentSetVelocity(world, enemy, vel);
  
  // Register systems
  SystemRegister(world, GravitySystem);
  SystemRegister(world, FrictionSystem);
  SystemRegister(world, MovementSystem);
  
  // Game loop
  while True do
  begin
    // Update world (runs all registered systems)
    WorldUpdate(world);
    
    // Platform-specific: Wait for VBlank
    // WaitVBlank;
  end;
  
  // Cleanup
  WorldCleanup(world);
end.

