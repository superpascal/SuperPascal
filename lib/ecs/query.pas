unit ECS_Query;

interface

uses
  ECS_Types,
  ECS_World;

// Query type
type
  TQuery = record
    World: ^TWorld;
    Components: TComponentMask;
    CurrentIndex: Word;
  end;

// Query API
function QueryCreate(var world: TWorld; components: TComponentMask): TQuery;
function QueryNext(var query: TQuery; var entity: TEntity): Boolean;
procedure QueryReset(var query: TQuery);

implementation

// Create query
function QueryCreate(var world: TWorld; components: TComponentMask): TQuery;
var
  query: TQuery;
begin
  query.World := @world;
  query.Components := components;
  query.CurrentIndex := 0;
  Result := query;
end;

// Get next entity from query
function QueryNext(var query: TQuery; var entity: TEntity): Boolean;
var
  i: Word;
  world: ^TWorld;
begin
  world := query.World;
  
  // Search from current index
  for i := query.CurrentIndex to world^.EntityCount - 1 do
  begin
    // Check if entity is valid
    if not world^.EntityValid[i] then
      Continue;
    
    // Check if entity has all required components
    // (All components in query.Components must be in entity's components)
    if (query.Components <= world^.EntityComponents[i]) then
    begin
      entity := i;
      query.CurrentIndex := i + 1;
      Result := True;
      Exit;
    end;
  end;
  
  // No more entities found
  Result := False;
end;

// Reset query to start
procedure QueryReset(var query: TQuery);
begin
  query.CurrentIndex := 0;
end;

end.

