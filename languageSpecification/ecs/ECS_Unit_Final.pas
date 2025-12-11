unit ECS;

interface

type
  TEntity = Word;
  TComponentMask = set of Byte;
  TComponentID = Byte;
  
  // Fixed-point type (for no-FPU platforms)
  Fixed16 = Integer;  // 16.16 fixed-point format
  
const
  ENTITY_NULL = $FFFF;
  MAX_ENTITIES = 256;
  COMPONENT_POSITION = 0;
  COMPONENT_VELOCITY = 1;
  COMPONENT_SPRITE = 2;
  COMPONENT_PHYSICS = 3;
  COMPONENT_PARTICLE = 4;
  MAX_COMPONENTS = 32;
  MAX_SYSTEMS = 32;

type
  TPosition = record
    X, Y: Integer;
  end;
  
  TVelocity = record
    DX, DY: Integer;
  end;
  
  TSprite = record
    SpriteID: Byte;
    TileID: Word;
    Palette: Byte;
    Visible: Boolean;
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

  TSystem = procedure(var world: TWorld);

  TWorld = record
    EntityCount: Word;
    FreeEntities: array[0..MAX_ENTITIES-1] of TEntity;
    FreeCount: Word;
    Positions: array[0..MAX_ENTITIES-1] of TPosition;
    Velocities: array[0..MAX_ENTITIES-1] of TVelocity;
    Sprites: array[0..MAX_ENTITIES-1] of TSprite;
    PhysicsBodies: array[0..MAX_ENTITIES-1] of TPhysicsBody;
    Particles: array[0..MAX_ENTITIES-1] of TParticle;
    EntityComponents: array[0..MAX_ENTITIES-1] of TComponentMask;
    EntityValid: array[0..MAX_ENTITIES-1] of Boolean;
    Systems: array[0..MAX_SYSTEMS-1] of TSystem;
    SystemCount: Word;
  end;

  TQuery = record
    World: ^TWorld;
    Components: TComponentMask;
    CurrentIndex: Word;
  end;

// Fixed-point helpers
function IntToFixed16(i: Integer): Fixed16;
function Fixed16ToInt(f: Fixed16): Integer;
function Fixed16Mul(a, b: Fixed16): Fixed16;

// World management
procedure WorldInit(var world: TWorld);
procedure WorldCleanup(var world: TWorld);
procedure WorldUpdate(var world: TWorld);

// Entity management
function EntityCreate(var world: TWorld): TEntity;
procedure EntityDestroy(var world: TWorld; entity: TEntity);
function EntityIsValid(var world: TWorld; entity: TEntity): Boolean;

// Component management
procedure ComponentAdd(var world: TWorld; entity: TEntity; componentID: TComponentID);
procedure ComponentRemove(var world: TWorld; entity: TEntity; componentID: TComponentID);
function ComponentHas(var world: TWorld; entity: TEntity; componentID: TComponentID): Boolean;
function ComponentGetPosition(var world: TWorld; entity: TEntity): ^TPosition;
function ComponentGetVelocity(var world: TWorld; entity: TEntity): ^TVelocity;
function ComponentGetSprite(var world: TWorld; entity: TEntity): ^TSprite;
function ComponentGetPhysics(var world: TWorld; entity: TEntity): ^TPhysicsBody;
function ComponentGetParticle(var world: TWorld; entity: TEntity): ^TParticle;
procedure ComponentSetPosition(var world: TWorld; entity: TEntity; const pos: TPosition);
procedure ComponentSetVelocity(var world: TWorld; entity: TEntity; const vel: TVelocity);
procedure ComponentSetSprite(var world: TWorld; entity: TEntity; const sprite: TSprite);
procedure ComponentSetPhysics(var world: TWorld; entity: TEntity; const physics: TPhysicsBody);
procedure ComponentSetParticle(var world: TWorld; entity: TEntity; const particle: TParticle);

// Query API
function QueryCreate(var world: TWorld; components: TComponentMask): TQuery;
function QueryNext(var query: TQuery; var entity: TEntity): Boolean;
procedure QueryReset(var query: TQuery);

// System management
procedure SystemRegister(var world: TWorld; system: TSystem);
procedure SystemRunAll(var world: TWorld);
function SystemGetCount(var world: TWorld): Word;

// Example systems (from Iteration 6)
procedure MovementSystem(var world: TWorld);
procedure GravitySystem(var world: TWorld);
procedure VelocitySystem(var world: TWorld);
procedure FrictionSystem(var world: TWorld);
procedure ParticleSystem(var world: TWorld);

implementation

// Fixed-point helpers
function IntToFixed16(i: Integer): Fixed16;
begin
  Result := i shl 16;
end;

function Fixed16ToInt(f: Fixed16): Integer;
begin
  Result := f shr 16;
end;

function Fixed16Mul(a, b: Fixed16): Fixed16;
begin
  Result := (Int64(a) * Int64(b)) shr 16;
end;

procedure SetComponentBit(var mask: TComponentMask; componentID: TComponentID);
begin
  mask := mask + [componentID];
end;

procedure ClearComponentBit(var mask: TComponentMask; componentID: TComponentID);
begin
  mask := mask - [componentID];
end;

function HasComponentBit(const mask: TComponentMask; componentID: TComponentID): Boolean;
begin
  Result := componentID in mask;
end;

procedure WorldInit(var world: TWorld);
var
  i: Word;
begin
  world.EntityCount := 0;
  world.FreeCount := 0;
  world.SystemCount := 0;
  for i := 0 to MAX_ENTITIES - 1 do
  begin
    world.Positions[i].X := 0;
    world.Positions[i].Y := 0;
    world.Velocities[i].DX := 0;
    world.Velocities[i].DY := 0;
    world.Sprites[i].SpriteID := 0;
    world.Sprites[i].TileID := 0;
    world.Sprites[i].Palette := 0;
    world.Sprites[i].Visible := False;
    world.PhysicsBodies[i].Mass := 1;
    world.PhysicsBodies[i].Friction := $E0;
    world.PhysicsBodies[i].Gravity := 0;
    world.PhysicsBodies[i].OnGround := False;
    world.Particles[i].X := 0;
    world.Particles[i].Y := 0;
    world.Particles[i].VX := 0;
    world.Particles[i].VY := 0;
    world.Particles[i].Energy := 0;
    world.Particles[i].Color := 0;
    world.Particles[i].Size := 1;
    world.Particles[i].Active := False;
    world.EntityComponents[i] := [];
    world.EntityValid[i] := False;
  end;
  for i := 0 to MAX_SYSTEMS - 1 do
    world.Systems[i] := nil;
end;

procedure WorldCleanup(var world: TWorld);
begin
  WorldInit(world);
end;

procedure WorldUpdate(var world: TWorld);
begin
  SystemRunAll(world);
end;

function EntityCreate(var world: TWorld): TEntity;
var
  entity: TEntity;
begin
  if world.FreeCount > 0 then
  begin
    world.FreeCount := world.FreeCount - 1;
    entity := world.FreeEntities[world.FreeCount];
  end
  else
  begin
    if world.EntityCount >= MAX_ENTITIES then
    begin
      Result := ENTITY_NULL;
      Exit;
    end;
    entity := world.EntityCount;
    world.EntityCount := world.EntityCount + 1;
  end;
  world.EntityValid[entity] := True;
  world.EntityComponents[entity] := [];
  Result := entity;
end;

procedure EntityDestroy(var world: TWorld; entity: TEntity);
begin
  if not EntityIsValid(world, entity) then
    Exit;
  world.Positions[entity].X := 0;
  world.Positions[entity].Y := 0;
  world.Velocities[entity].DX := 0;
  world.Velocities[entity].DY := 0;
  world.Sprites[entity].SpriteID := 0;
  world.Sprites[entity].TileID := 0;
  world.Sprites[entity].Palette := 0;
  world.Sprites[entity].Visible := False;
  world.PhysicsBodies[entity].Mass := 1;
  world.PhysicsBodies[entity].Friction := $E0;
  world.PhysicsBodies[entity].Gravity := 0;
  world.PhysicsBodies[entity].OnGround := False;
  world.Particles[entity].X := 0;
  world.Particles[entity].Y := 0;
  world.Particles[entity].VX := 0;
  world.Particles[entity].VY := 0;
  world.Particles[entity].Energy := 0;
  world.Particles[entity].Active := False;
  world.EntityComponents[entity] := [];
  world.EntityValid[entity] := False;
  if world.FreeCount < MAX_ENTITIES then
  begin
    world.FreeEntities[world.FreeCount] := entity;
    world.FreeCount := world.FreeCount + 1;
  end;
end;

function EntityIsValid(var world: TWorld; entity: TEntity): Boolean;
begin
  if entity >= MAX_ENTITIES then
  begin
    Result := False;
    Exit;
  end;
  Result := world.EntityValid[entity];
end;

procedure ComponentAdd(var world: TWorld; entity: TEntity; componentID: TComponentID);
begin
  if not EntityIsValid(world, entity) then
    Exit;
  if componentID >= MAX_COMPONENTS then
    Exit;
  SetComponentBit(world.EntityComponents[entity], componentID);
  case componentID of
    COMPONENT_POSITION:
    begin
      world.Positions[entity].X := 0;
      world.Positions[entity].Y := 0;
    end;
    COMPONENT_VELOCITY:
    begin
      world.Velocities[entity].DX := 0;
      world.Velocities[entity].DY := 0;
    end;
    COMPONENT_SPRITE:
    begin
      world.Sprites[entity].SpriteID := 0;
      world.Sprites[entity].TileID := 0;
      world.Sprites[entity].Palette := 0;
      world.Sprites[entity].Visible := False;
    end;
    COMPONENT_PHYSICS:
    begin
      world.PhysicsBodies[entity].Mass := 1;
      world.PhysicsBodies[entity].Friction := $E0;
      world.PhysicsBodies[entity].Gravity := 1;
      world.PhysicsBodies[entity].OnGround := False;
    end;
    COMPONENT_PARTICLE:
    begin
      world.Particles[entity].X := 0;
      world.Particles[entity].Y := 0;
      world.Particles[entity].VX := 0;
      world.Particles[entity].VY := 0;
      world.Particles[entity].Energy := 0;
      world.Particles[entity].Color := 0;
      world.Particles[entity].Size := 1;
      world.Particles[entity].Active := False;
    end;
  end;
end;

procedure ComponentRemove(var world: TWorld; entity: TEntity; componentID: TComponentID);
begin
  if not EntityIsValid(world, entity) then
    Exit;
  if componentID >= MAX_COMPONENTS then
    Exit;
  ClearComponentBit(world.EntityComponents[entity], componentID);
  case componentID of
    COMPONENT_POSITION:
    begin
      world.Positions[entity].X := 0;
      world.Positions[entity].Y := 0;
    end;
    COMPONENT_VELOCITY:
    begin
      world.Velocities[entity].DX := 0;
      world.Velocities[entity].DY := 0;
    end;
    COMPONENT_SPRITE:
    begin
      world.Sprites[entity].SpriteID := 0;
      world.Sprites[entity].TileID := 0;
      world.Sprites[entity].Palette := 0;
      world.Sprites[entity].Visible := False;
    end;
    COMPONENT_PHYSICS:
    begin
      world.PhysicsBodies[entity].Mass := 1;
      world.PhysicsBodies[entity].Friction := $E0;
      world.PhysicsBodies[entity].Gravity := 0;
      world.PhysicsBodies[entity].OnGround := False;
    end;
    COMPONENT_PARTICLE:
    begin
      world.Particles[entity].X := 0;
      world.Particles[entity].Y := 0;
      world.Particles[entity].VX := 0;
      world.Particles[entity].VY := 0;
      world.Particles[entity].Energy := 0;
      world.Particles[entity].Active := False;
    end;
  end;
end;

function ComponentHas(var world: TWorld; entity: TEntity; componentID: TComponentID): Boolean;
begin
  if not EntityIsValid(world, entity) then
  begin
    Result := False;
    Exit;
  end;
  if componentID >= MAX_COMPONENTS then
  begin
    Result := False;
    Exit;
  end;
  Result := HasComponentBit(world.EntityComponents[entity], componentID);
end;

function ComponentGetPosition(var world: TWorld; entity: TEntity): ^TPosition;
begin
  if not ComponentHas(world, entity, COMPONENT_POSITION) then
  begin
    Result := nil;
    Exit;
  end;
  Result := @world.Positions[entity];
end;

function ComponentGetVelocity(var world: TWorld; entity: TEntity): ^TVelocity;
begin
  if not ComponentHas(world, entity, COMPONENT_VELOCITY) then
  begin
    Result := nil;
    Exit;
  end;
  Result := @world.Velocities[entity];
end;

function ComponentGetSprite(var world: TWorld; entity: TEntity): ^TSprite;
begin
  if not ComponentHas(world, entity, COMPONENT_SPRITE) then
  begin
    Result := nil;
    Exit;
  end;
  Result := @world.Sprites[entity];
end;

function ComponentGetPhysics(var world: TWorld; entity: TEntity): ^TPhysicsBody;
begin
  if not ComponentHas(world, entity, COMPONENT_PHYSICS) then
  begin
    Result := nil;
    Exit;
  end;
  Result := @world.PhysicsBodies[entity];
end;

function ComponentGetParticle(var world: TWorld; entity: TEntity): ^TParticle;
begin
  if not ComponentHas(world, entity, COMPONENT_PARTICLE) then
  begin
    Result := nil;
    Exit;
  end;
  Result := @world.Particles[entity];
end;

procedure ComponentSetPosition(var world: TWorld; entity: TEntity; const pos: TPosition);
var
  p: ^TPosition;
begin
  p := ComponentGetPosition(world, entity);
  if p <> nil then
    p^ := pos;
end;

procedure ComponentSetVelocity(var world: TWorld; entity: TEntity; const vel: TVelocity);
var
  v: ^TVelocity;
begin
  v := ComponentGetVelocity(world, entity);
  if v <> nil then
    v^ := vel;
end;

procedure ComponentSetSprite(var world: TWorld; entity: TEntity; const sprite: TSprite);
var
  s: ^TSprite;
begin
  s := ComponentGetSprite(world, entity);
  if s <> nil then
    s^ := sprite;
end;

procedure ComponentSetPhysics(var world: TWorld; entity: TEntity; const physics: TPhysicsBody);
var
  p: ^TPhysicsBody;
begin
  p := ComponentGetPhysics(world, entity);
  if p <> nil then
    p^ := physics;
end;

procedure ComponentSetParticle(var world: TWorld; entity: TEntity; const particle: TParticle);
var
  p: ^TParticle;
begin
  p := ComponentGetParticle(world, entity);
  if p <> nil then
    p^ := particle;
end;

function QueryCreate(var world: TWorld; components: TComponentMask): TQuery;
var
  query: TQuery;
begin
  query.World := @world;
  query.Components := components;
  query.CurrentIndex := 0;
  Result := query;
end;

function QueryNext(var query: TQuery; var entity: TEntity): Boolean;
var
  i: Word;
  world: ^TWorld;
begin
  world := query.World;
  for i := query.CurrentIndex to world^.EntityCount - 1 do
  begin
    if not world^.EntityValid[i] then
      Continue;
    if (query.Components <= world^.EntityComponents[i]) then
    begin
      entity := i;
      query.CurrentIndex := i + 1;
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure QueryReset(var query: TQuery);
begin
  query.CurrentIndex := 0;
end;

procedure SystemRegister(var world: TWorld; system: TSystem);
begin
  if world.SystemCount >= MAX_SYSTEMS then
    Exit;
  world.Systems[world.SystemCount] := system;
  world.SystemCount := world.SystemCount + 1;
end;

procedure SystemRunAll(var world: TWorld);
var
  i: Word;
begin
  for i := 0 to world.SystemCount - 1 do
  begin
    if world.Systems[i] <> nil then
      world.Systems[i](world);
  end;
end;

function SystemGetCount(var world: TWorld): Word;
begin
  Result := world.SystemCount;
end;

// Example systems from Iteration 6
procedure MovementSystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
  pos: ^TPosition;
  vel: ^TVelocity;
begin
  query := QueryCreate(world, [COMPONENT_POSITION, COMPONENT_VELOCITY]);
  while QueryNext(query, entity) do
  begin
    pos := ComponentGetPosition(world, entity);
    vel := ComponentGetVelocity(world, entity);
    if (pos <> nil) and (vel <> nil) then
    begin
      pos^.X := pos^.X + vel^.DX;
      pos^.Y := pos^.Y + vel^.DY;
    end;
  end;
end;

procedure GravitySystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
  vel: ^TVelocity;
  physics: ^TPhysicsBody;
begin
  query := QueryCreate(world, [COMPONENT_VELOCITY, COMPONENT_PHYSICS]);
  while QueryNext(query, entity) do
  begin
    vel := ComponentGetVelocity(world, entity);
    physics := ComponentGetPhysics(world, entity);
    if (vel <> nil) and (physics <> nil) then
    begin
      vel^.DY := vel^.DY + physics^.Gravity;
    end;
  end;
end;

procedure VelocitySystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
  pos: ^TPosition;
  vel: ^TVelocity;
begin
  query := QueryCreate(world, [COMPONENT_POSITION, COMPONENT_VELOCITY]);
  while QueryNext(query, entity) do
  begin
    pos := ComponentGetPosition(world, entity);
    vel := ComponentGetVelocity(world, entity);
    if (pos <> nil) and (vel <> nil) then
    begin
      pos^.X := pos^.X + vel^.DX;
      pos^.Y := pos^.Y + vel^.DY;
    end;
  end;
end;

procedure FrictionSystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
  vel: ^TVelocity;
  physics: ^TPhysicsBody;
begin
  query := QueryCreate(world, [COMPONENT_VELOCITY, COMPONENT_PHYSICS]);
  while QueryNext(query, entity) do
  begin
    vel := ComponentGetVelocity(world, entity);
    physics := ComponentGetPhysics(world, entity);
    if (vel <> nil) and (physics <> nil) then
    begin
      vel^.DX := (vel^.DX * physics^.Friction) shr 8;
      vel^.DY := (vel^.DY * physics^.Friction) shr 8;
    end;
  end;
end;

procedure ParticleSystem(var world: TWorld);
var
  query: TQuery;
  entity: TEntity;
  particle: ^TParticle;
begin
  query := QueryCreate(world, [COMPONENT_PARTICLE]);
  while QueryNext(query, entity) do
  begin
    particle := ComponentGetParticle(world, entity);
    if (particle <> nil) and particle^.Active then
    begin
      particle^.X := particle^.X + particle^.VX;
      particle^.Y := particle^.Y + particle^.VY;
      particle^.VY := particle^.VY + IntToFixed16(1);
      particle^.Energy := particle^.Energy - 1;
      if particle^.Energy <= 0 then
        particle^.Active := False;
    end;
  end;
end;

end.

