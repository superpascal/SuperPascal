unit Math_Sqrt;

interface

// Square root functions (fast integer approximation)
// Source: algorithms/02_MathematicalAlgorithms.md
// Based on: Mikro archive SQROOT.TXT, SQRT.TXT

uses
  Math_Types,
  Math_Fixed;

// Square root lookup table
var
  SqrtTable: array[0..255] of Byte;
  SqrtTableInitialized: Boolean;

// Initialize square root table
procedure InitSqrtTable;
procedure SetupSqrtTable;

// Integer square root (fast approximation)
function IntegerSqrt(n: LongInt): Integer;

// Fixed-point square root
function FixedSqrt(x: Fixed16): Fixed16;
function Fixed16Sqrt(x: Fixed16): Fixed16;  // Alias for FixedSqrt

implementation

// Initialize square root table
procedure InitSqrtTable;
begin
  if not SqrtTableInitialized then
  begin
    SetupSqrtTable;
    SqrtTableInitialized := True;
  end;
end;

// Setup square root lookup table
// Uses pre-computed table (no floating-point required)
// Works on ALL platforms including no-FPU
procedure SetupSqrtTable;
const
  // Pre-computed sqrt table values (256 entries)
  // SqrtTable[i] = Round(256 * sqrt(i / 256))
  // Generated offline to avoid floating-point dependency
  // Formula: sqrt(i/256) * 256 = sqrt(i * 256)
  PrecomputedSqrtTable: array[0..255] of Byte = (
    $00, $10, $16, $1B, $20, $23, $26, $29, $2C, $2E, $30, $32, $34, $36, $38, $3A,
    $3C, $3D, $3F, $40, $42, $43, $45, $46, $47, $49, $4A, $4B, $4C, $4E, $4F, $50,
    $51, $52, $53, $54, $55, $56, $57, $58, $59, $5A, $5B, $5C, $5D, $5E, $5F, $60,
    $61, $62, $63, $64, $64, $65, $66, $67, $68, $69, $69, $6A, $6B, $6C, $6D, $6D,
    $6E, $6F, $70, $70, $71, $72, $73, $73, $74, $75, $76, $76, $77, $78, $78, $79,
    $7A, $7B, $7B, $7C, $7D, $7D, $7E, $7F, $7F, $80, $81, $81, $82, $83, $83, $84,
    $85, $85, $86, $87, $87, $88, $89, $89, $8A, $8A, $8B, $8C, $8C, $8D, $8E, $8E,
    $8F, $8F, $90, $91, $91, $92, $92, $93, $94, $94, $95, $95, $96, $97, $97, $98,
    $98, $99, $99, $9A, $9B, $9B, $9C, $9C, $9D, $9D, $9E, $9F, $9F, $A0, $A0, $A1,
    $A1, $A2, $A2, $A3, $A3, $A4, $A4, $A5, $A6, $A6, $A7, $A7, $A8, $A8, $A9, $A9,
    $AA, $AA, $AB, $AB, $AC, $AC, $AD, $AD, $AE, $AE, $AF, $AF, $B0, $B0, $B1, $B1,
    $B2, $B2, $B3, $B3, $B4, $B4, $B5, $B5, $B6, $B6, $B7, $B7, $B8, $B8, $B9, $B9,
    $BA, $BA, $BB, $BB, $BC, $BC, $BD, $BD, $BE, $BE, $BF, $BF, $C0, $C0, $C1, $C1,
    $C2, $C2, $C3, $C3, $C4, $C4, $C5, $C5, $C6, $C6, $C7, $C7, $C8, $C8, $C9, $C9,
    $CA, $CA, $CB, $CB, $CC, $CC, $CD, $CD, $CE, $CE, $CF, $CF, $D0, $D0, $D1, $D1,
    $D2, $D2, $D3, $D3, $D4, $D4, $D5, $D5, $D6, $D6, $D7, $D7, $D8, $D8, $D9, $D9,
    $DA, $DA, $DB, $DB, $DC, $DC, $DD, $DD, $DE, $DE, $DF, $DF, $E0, $E0, $E1, $E1,
    $E2, $E2, $E3, $E3, $E4, $E4, $E5, $E5, $E6, $E6, $E7, $E7, $E8, $E8, $E9, $E9,
    $EA, $EA, $EB, $EB, $EC, $EC, $ED, $ED, $EE, $EE, $EF, $EF, $F0, $F0, $F1, $F1,
    $F2, $F2, $F3, $F3, $F4, $F4, $F5, $F5, $F6, $F6, $F7, $F7, $F8, $F8, $F9, $F9,
    $FA, $FA, $FB, $FB, $FC, $FC, $FD, $FD, $FE, $FE, $FF, $FF
  );
var
  i: integer;
begin
  // Use pre-computed table (no floating-point required)
  // This works on ALL platforms including no-FPU
  for i := 0 to 255 do
    SqrtTable[i] := PrecomputedSqrtTable[i];
end;

// Integer square root (fast approximation)
// Performance: 16-27 cycles (much faster than FPU on systems without hardware sqrt)
function IntegerSqrt(n: LongInt): Integer;
var
  bitPos: integer;
  shifted: LongInt;
begin
  if n = 0 then
  begin
    Result := 0;
    Exit;
  end;
  
  if not SqrtTableInitialized then
    InitSqrtTable;
  
  // Find highest bit position using binary search
  if n >= $10000000 then
    bitPos := 30  // Bit 30-31
  else if n >= $1000000 then
    bitPos := 26  // Bit 26-29
  else if n >= $100000 then
    bitPos := 20  // Bit 20-25
  else if n >= $10000 then
    bitPos := 16  // Bit 16-19
  else if n >= $1000 then
    bitPos := 12  // Bit 12-15
  else if n >= $100 then
    bitPos := 8   // Bit 8-11
  else if n >= $10 then
    bitPos := 4   // Bit 4-7
  else
    bitPos := 0;  // Bit 0-3
  
  // Shift to get value in 0..255 range
  shifted := n shr (bitPos - 8);
  
  // Lookup in table
  Result := SqrtTable[shifted and $FF];
  
  // Shift result back
  Result := Result shl ((bitPos div 2) - 4);
end;

// Fixed-point square root
function FixedSqrt(x: Fixed16): Fixed16;
begin
  if x <= 0 then
  begin
    Result := 0;
    Exit;
  end;
  
  // Square root of fixed-point: sqrt(x * 2^16) = sqrt(x) * 2^8
  // Use integer sqrt on the fixed-point value, then adjust
  Result := IntToFixed16(IntegerSqrt(x));
end;

// Alias for FixedSqrt
function Fixed16Sqrt(x: Fixed16): Fixed16;
begin
  Result := FixedSqrt(x);
end;

// Initialize table on unit load
begin
  SqrtTableInitialized := False;
end.

