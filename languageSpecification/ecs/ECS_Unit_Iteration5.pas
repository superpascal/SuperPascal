unit ECS;

interface

type
  TEntity = Word;
  TComponentMask = set of Byte;
  TComponentID = Byte;
  
const
  ENTITY_NULL = $FFFF;
  MAX_ENTITIES = 256;
  COMPONENT_POSITION = 0;
  COMPONENT_VELOCITY = 1;
  COMPONENT_SPRITE = 2;
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

  // System type
  TSystem = procedure(var world: TWorld);

  TWorld = record
    EntityCount: Word;
    FreeEntities: array[0..MAX_ENTITIES-1] of TEntity;
    FreeCount: Word;
    Positions: array[0..MAX_ENTITIES-1] of TPosition;
    Velocities: array[0..MAX_ENTITIES-1] of TVelocity;
    Sprites: array[0..MAX_ENTITIES-1] of TSprite;
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
procedure ComponentSetPosition(var world: TWorld; entity: TEntity; const pos: TPosition);
procedure ComponentSetVelocity(var world: TWorld; entity: TEntity; const vel: TVelocity);
procedure ComponentSetSprite(var world: TWorld; entity: TEntity; const sprite: TSprite);

// Query API
function QueryCreate(var world: TWorld; components: TComponentMask): TQuery;
function QueryNext(var query: TQuery; var entity: TEntity): Boolean;
procedure QueryReset(var query: TQuery);

// System management
procedure SystemRegister(var world: TWorld; system: TSystem);
procedure SystemRunAll(var world: TWorld);
function SystemGetCount(var world: TWorld): Word;

implementation

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

end.

