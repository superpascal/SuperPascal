unit Math_Matrix;

interface

// Matrix operations for 3D transformations
// Source: algorithms/02_MathematicalAlgorithms.md
// Based on: Mikro archive OTMMATX.TXT

uses
  Math_Types,
  Math_Fixed,
  Math_Trig;

// Matrix operations
function MatrixIdentity: TMatrix4x4;
procedure MatrixMultiply(var a: TMatrix4x4; const b: TMatrix4x4);
function MatrixRotateX(angle: Fixed16): TMatrix4x4;
function MatrixRotateY(angle: Fixed16): TMatrix4x4;
function MatrixRotateZ(angle: Fixed16): TMatrix4x4;
function MatrixTranslate(x, y, z: Fixed16): TMatrix4x4;
function MatrixScale(sx, sy, sz: Fixed16): TMatrix4x4;
function TransformVector(const v: TVector3; const m: TMatrix4x4): TVector3;

implementation

// Identity matrix
function MatrixIdentity: TMatrix4x4;
begin
  // Initialize to identity
  Result[0, 0] := FIXED16_ONE; Result[0, 1] := 0; Result[0, 2] := 0; Result[0, 3] := 0;
  Result[1, 0] := 0; Result[1, 1] := FIXED16_ONE; Result[1, 2] := 0; Result[1, 3] := 0;
  Result[2, 0] := 0; Result[2, 1] := 0; Result[2, 2] := FIXED16_ONE; Result[2, 3] := 0;
  Result[3, 0] := 0; Result[3, 1] := 0; Result[3, 2] := 0; Result[3, 3] := FIXED16_ONE;
end;

// Matrix multiplication
procedure MatrixMultiply(var a: TMatrix4x4; const b: TMatrix4x4);
var
  temp: TMatrix4x4;
  i, j: integer;
begin
  // Transform by columns first, then by rows
  for j := 0 to 3 do
    for i := 0 to 3 do
      temp[i, j] := Fixed16Mul(a[i, 0], b[0, j]) +
                    Fixed16Mul(a[i, 1], b[1, j]) +
                    Fixed16Mul(a[i, 2], b[2, j]) +
                    Fixed16Mul(a[i, 3], b[3, j]);
  
  // Copy result back to matrix a
  a := temp;
end;

// Rotate about X axis
function MatrixRotateX(angle: Fixed16): TMatrix4x4;
var
  cosA, sinA: Fixed16;
  angleInt: integer;
begin
  // Convert fixed-point angle to integer (0-255 range)
  // Note: angle is in fixed-point, need to convert to 0-255 range for lookup table
  angleInt := Fixed16ToInt(angle) and $FF;
  
  cosA := CosFixed(angleInt);
  sinA := SinFixed(angleInt);
  
  Result := MatrixIdentity;
  Result[1, 1] := cosA;
  Result[1, 2] := sinA;
  Result[2, 1] := -sinA;  // Negate sinA
  Result[2, 2] := cosA;
end;

// Rotate about Y axis
function MatrixRotateY(angle: Fixed16): TMatrix4x4;
var
  cosA, sinA: Fixed16;
  angleInt: integer;
begin
  angleInt := Fixed16ToInt(angle) and $FF;
  
  cosA := CosFixed(angleInt);
  sinA := SinFixed(angleInt);
  
  Result := MatrixIdentity;
  Result[0, 0] := cosA;
  Result[0, 2] := -sinA;  // Negate sinA
  Result[2, 0] := sinA;
  Result[2, 2] := cosA;
end;

// Rotate about Z axis
function MatrixRotateZ(angle: Fixed16): TMatrix4x4;
var
  cosA, sinA: Fixed16;
  angleInt: integer;
begin
  angleInt := Fixed16ToInt(angle) and $FF;
  
  cosA := CosFixed(angleInt);
  sinA := SinFixed(angleInt);
  
  Result := MatrixIdentity;
  Result[0, 0] := cosA;
  Result[0, 1] := sinA;
  Result[1, 0] := -sinA;  // Negate sinA
  Result[1, 1] := cosA;
end;

// Translation matrix
function MatrixTranslate(x, y, z: Fixed16): TMatrix4x4;
begin
  Result := MatrixIdentity;
  Result[3, 0] := x;
  Result[3, 1] := y;
  Result[3, 2] := z;
end;

// Scale matrix
function MatrixScale(sx, sy, sz: Fixed16): TMatrix4x4;
begin
  Result := MatrixIdentity;
  Result[0, 0] := sx;
  Result[1, 1] := sy;
  Result[2, 2] := sz;
end;

// Transform vector by matrix
function TransformVector(const v: TVector3; const m: TMatrix4x4): TVector3;
begin
  // Transform using dot product with matrix columns
  Result.X := Fixed16Mul(v.X, m[0, 0]) +
              Fixed16Mul(v.Y, m[1, 0]) +
              Fixed16Mul(v.Z, m[2, 0]) +
              m[3, 0];  // Translation
  
  Result.Y := Fixed16Mul(v.X, m[0, 1]) +
              Fixed16Mul(v.Y, m[1, 1]) +
              Fixed16Mul(v.Z, m[2, 1]) +
              m[3, 1];  // Translation
  
  Result.Z := Fixed16Mul(v.X, m[0, 2]) +
              Fixed16Mul(v.Y, m[1, 2]) +
              Fixed16Mul(v.Z, m[2, 2]) +
              m[3, 2];  // Translation
end;

end.

