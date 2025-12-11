unit Math_Vector;

interface

// Vector math operations
// Source: algorithms/02_MathematicalAlgorithms.md

uses
  Math_Types,
  Math_Fixed,
  Math_Sqrt;

// Vector2 operations
function Vector2Add(const a, b: TVector2): TVector2;
function Vector2Sub(const a, b: TVector2): TVector2;
function Vector2Scale(const v: TVector2; s: Fixed16): TVector2;
function Vector2Dot(const a, b: TVector2): Fixed16;
function Vector2Length(const v: TVector2): Fixed16;
function Vector2LengthSquared(const v: TVector2): Fixed16;
function Vector2Normalize(const v: TVector2): TVector2;

// Vector3 operations
function Vector3Add(const a, b: TVector3): TVector3;
function Vector3Sub(const a, b: TVector3): TVector3;
function Vector3Scale(const v: TVector3; s: Fixed16): TVector3;
function Vector3Dot(const a, b: TVector3): Fixed16;
function Vector3Cross(const a, b: TVector3): TVector3;
function Vector3Length(const v: TVector3): Fixed16;
function Vector3LengthSquared(const v: TVector3): Fixed16;
function Vector3Normalize(const v: TVector3): TVector3;

implementation

// Vector2 Addition
function Vector2Add(const a, b: TVector2): TVector2;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
end;

// Vector2 Subtraction
function Vector2Sub(const a, b: TVector2): TVector2;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
end;

// Vector2 Scalar Multiplication
function Vector2Scale(const v: TVector2; s: Fixed16): TVector2;
begin
  Result.X := Fixed16Mul(v.X, s);
  Result.Y := Fixed16Mul(v.Y, s);
end;

// Vector2 Dot Product
function Vector2Dot(const a, b: TVector2): Fixed16;
begin
  Result := Fixed16Mul(a.X, b.X) + Fixed16Mul(a.Y, b.Y);
end;

// Vector2 Length Squared (faster, no sqrt)
function Vector2LengthSquared(const v: TVector2): Fixed16;
begin
  Result := Fixed16Mul(v.X, v.X) + Fixed16Mul(v.Y, v.Y);
end;

// Vector2 Length (magnitude)
function Vector2Length(const v: TVector2): Fixed16;
begin
  Result := FixedSqrt(Vector2LengthSquared(v));
end;

// Vector2 Normalization
function Vector2Normalize(const v: TVector2): TVector2;
var
  len: Fixed16;
begin
  len := Vector2Length(v);
  if len > 0 then
  begin
    Result.X := Fixed16Div(v.X, len);
    Result.Y := Fixed16Div(v.Y, len);
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;

// Vector3 Addition
function Vector3Add(const a, b: TVector3): TVector3;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
  Result.Z := a.Z + b.Z;
end;

// Vector3 Subtraction
function Vector3Sub(const a, b: TVector3): TVector3;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
  Result.Z := a.Z - b.Z;
end;

// Vector3 Scalar Multiplication
function Vector3Scale(const v: TVector3; s: Fixed16): TVector3;
begin
  Result.X := Fixed16Mul(v.X, s);
  Result.Y := Fixed16Mul(v.Y, s);
  Result.Z := Fixed16Mul(v.Z, s);
end;

// Vector3 Dot Product
function Vector3Dot(const a, b: TVector3): Fixed16;
begin
  Result := Fixed16Mul(a.X, b.X) +
            Fixed16Mul(a.Y, b.Y) +
            Fixed16Mul(a.Z, b.Z);
end;

// Vector3 Cross Product
function Vector3Cross(const a, b: TVector3): TVector3;
begin
  Result.X := Fixed16Mul(a.Y, b.Z) - Fixed16Mul(a.Z, b.Y);
  Result.Y := Fixed16Mul(a.Z, b.X) - Fixed16Mul(a.X, b.Z);
  Result.Z := Fixed16Mul(a.X, b.Y) - Fixed16Mul(a.Y, b.X);
end;

// Vector3 Length Squared (faster, no sqrt)
function Vector3LengthSquared(const v: TVector3): Fixed16;
begin
  Result := Fixed16Mul(v.X, v.X) +
            Fixed16Mul(v.Y, v.Y) +
            Fixed16Mul(v.Z, v.Z);
end;

// Vector3 Length (magnitude)
function Vector3Length(const v: TVector3): Fixed16;
begin
  Result := FixedSqrt(Vector3LengthSquared(v));
end;

// Vector3 Normalization
function Vector3Normalize(const v: TVector3): TVector3;
var
  len: Fixed16;
begin
  len := Vector3Length(v);
  if len > 0 then
  begin
    Result.X := Fixed16Div(v.X, len);
    Result.Y := Fixed16Div(v.Y, len);
    Result.Z := Fixed16Div(v.Z, len);
  end
  else
  begin
    Result.X := 0;
    Result.Y := 0;
    Result.Z := 0;
  end;
end;

end.

