unit Math;

interface

// Main Math module - Rust-style mod.pas pattern
// This is the entry point for the Math library
// Source: algorithms/01_FixedPointArithmetic.md, 02_MathematicalAlgorithms.md

// Import all sub-modules
uses
  Math_Types,
  Math_Fixed,
  Math_Matrix,
  Math_Trig,
  Math_Sqrt,
  Math_Vector;

// Re-export types for convenience
type
  Fixed16 = Math_Types.Fixed16;
  Fixed32 = Math_Types.Fixed32;
  TVector2 = Math_Types.TVector2;
  TVector3 = Math_Types.TVector3;
  TMatrix2x2 = Math_Types.TMatrix2x2;
  TMatrix3x3 = Math_Types.TMatrix3x3;
  TMatrix4x4 = Math_Types.TMatrix4x4;

// Re-export constants
const
  FIXED16_ONE = Math_Types.FIXED16_ONE;
  FIXED16_HALF = Math_Types.FIXED16_HALF;
  FIXED16_PI = Math_Types.FIXED16_PI;
  FIXED16_TWO_PI = Math_Types.FIXED16_TWO_PI;
  FIXED16_PI_OVER_2 = Math_Types.FIXED16_PI_OVER_2;
  FIXED16_PI_OVER_3 = Math_Types.FIXED16_PI_OVER_3;
  FIXED16_PI_OVER_4 = Math_Types.FIXED16_PI_OVER_4;
  FIXED16_PI_OVER_6 = Math_Types.FIXED16_PI_OVER_6;
  PI = Math_Types.PI;
  TWO_PI = Math_Types.TWO_PI;
  PI_OVER_2 = Math_Types.PI_OVER_2;
  PI_OVER_3 = Math_Types.PI_OVER_3;
  PI_OVER_4 = Math_Types.PI_OVER_4;
  PI_OVER_6 = Math_Types.PI_OVER_6;
  E = Math_Types.E;
  TAU = Math_Types.TAU;

// Re-export trig functions for convenience
// High-precision signed angle functions (-180 to +180, full 360° circle, ~0.0055° per step, 16-bit)
// Fast path: Automatically uses direct lookup when angle aligns with table entry
// Slow path: Uses linear interpolation for precise results
function SinSigned(angle: SmallInt): Fixed16;
function CosSigned(angle: SmallInt): Fixed16;
function TanSigned(angle: SmallInt): Fixed16;

// Radian-based functions
function RadiansToSignedAngle(radians: Fixed16): SmallInt;
function SignedAngleToRadians(angle: SmallInt): Fixed16;
function SinR(radians: Fixed16): Fixed16;
function CosR(radians: Fixed16): Fixed16;
function TanR(radians: Fixed16): Fixed16;

// Degree-based functions
function DegreesToSignedAngle(degrees: integer): SmallInt;
function SignedAngleToDegrees(angle: SmallInt): integer;
function DegreesToRadians(degrees: integer): Fixed16;
function RadiansToDegrees(radians: Fixed16): integer;
function SinD(degrees: integer): Fixed16;
function CosD(degrees: integer): Fixed16;
function TanD(degrees: integer): Fixed16;

// Inverse functions (return radians)
function ArcSin(x: Fixed16): Fixed16;
function ArcCos(x: Fixed16): Fixed16;
function ArcTan(x: Fixed16): Fixed16;
function ArcTan2(y, x: Fixed16): Fixed16;

// Inverse functions (return degrees)
function ArcSinD(x: Fixed16): integer;
function ArcCosD(x: Fixed16): integer;
function ArcTanD(x: Fixed16): integer;
function ArcTan2D(y, x: Fixed16): integer;

// All functions are available through the imported units:
// - Math_Fixed: IntToFixed16, Fixed16ToInt, Fixed16Add, Fixed16Mul, etc.
// - Math_Matrix: MatrixIdentity, MatrixMultiply, MatrixRotateX/Y/Z, etc.
// - Math_Trig: All trig functions (see above)
// - Math_Sqrt: IntegerSqrt, FixedSqrt, Fixed16Sqrt
// - Math_Vector: Vector2Add, Vector3Dot, Vector3Cross, etc.

// Users can call functions directly:
//   var f: Fixed16;
//   f := IntToFixed16(100);
//   f := Fixed16Mul(f, IntToFixed16(2));

implementation

// Re-export trig functions
function SinSigned(angle: SmallInt): Fixed16; begin Result := Math_Trig.SinSigned(angle); end;
function CosSigned(angle: SmallInt): Fixed16; begin Result := Math_Trig.CosSigned(angle); end;
function TanSigned(angle: SmallInt): Fixed16; begin Result := Math_Trig.TanSigned(angle); end;
function RadiansToSignedAngle(radians: Fixed16): SmallInt; begin Result := Math_Trig.RadiansToSignedAngle(radians); end;
function SignedAngleToRadians(angle: SmallInt): Fixed16; begin Result := Math_Trig.SignedAngleToRadians(angle); end;
function SinR(radians: Fixed16): Fixed16; begin Result := Math_Trig.SinR(radians); end;
function CosR(radians: Fixed16): Fixed16; begin Result := Math_Trig.CosR(radians); end;
function TanR(radians: Fixed16): Fixed16; begin Result := Math_Trig.TanR(radians); end;
function DegreesToSignedAngle(degrees: integer): SmallInt; begin Result := Math_Trig.DegreesToSignedAngle(degrees); end;
function SignedAngleToDegrees(angle: SmallInt): integer; begin Result := Math_Trig.SignedAngleToDegrees(angle); end;
function DegreesToRadians(degrees: integer): Fixed16; begin Result := Math_Trig.DegreesToRadians(degrees); end;
function RadiansToDegrees(radians: Fixed16): integer; begin Result := Math_Trig.RadiansToDegrees(radians); end;
function SinD(degrees: integer): Fixed16; begin Result := Math_Trig.SinD(degrees); end;
function CosD(degrees: integer): Fixed16; begin Result := Math_Trig.CosD(degrees); end;
function TanD(degrees: integer): Fixed16; begin Result := Math_Trig.TanD(degrees); end;
function ArcSin(x: Fixed16): Fixed16; begin Result := Math_Trig.ArcSin(x); end;
function ArcCos(x: Fixed16): Fixed16; begin Result := Math_Trig.ArcCos(x); end;
function ArcTan(x: Fixed16): Fixed16; begin Result := Math_Trig.ArcTan(x); end;
function ArcTan2(y, x: Fixed16): Fixed16; begin Result := Math_Trig.ArcTan2(y, x); end;
function ArcSinD(x: Fixed16): integer; begin Result := Math_Trig.ArcSinD(x); end;
function ArcCosD(x: Fixed16): integer; begin Result := Math_Trig.ArcCosD(x); end;
function ArcTanD(x: Fixed16): integer; begin Result := Math_Trig.ArcTanD(x); end;
function ArcTan2D(y, x: Fixed16): integer; begin Result := Math_Trig.ArcTan2D(y, x); end;

end.

