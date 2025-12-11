unit Math_Fixed;

interface

// Fixed-point arithmetic operations
// Source: algorithms/01_FixedPointArithmetic.md
// Based on: Mikro archive FIXEDPOI.TXT

uses
  Math_Types;

// Conversion functions
function IntToFixed16(i: Integer): Fixed16;
function Fixed16ToInt(f: Fixed16): Integer;
function IntToFixed32(i: LongInt): Fixed32;
function Fixed32ToInt(f: Fixed32): LongInt;

// Arithmetic operations (Fixed16)
function Fixed16Add(a, b: Fixed16): Fixed16;
function Fixed16Sub(a, b: Fixed16): Fixed16;
function Fixed16Mul(a, b: Fixed16): Fixed16;
function Fixed16Div(a, b: Fixed16): Fixed16;
function Fixed16Neg(a: Fixed16): Fixed16;
function Fixed16Abs(a: Fixed16): Fixed16;

// Comparison operations (Fixed16)
function Fixed16Eq(a, b: Fixed16): Boolean;
function Fixed16Lt(a, b: Fixed16): Boolean;
function Fixed16Gt(a, b: Fixed16): Boolean;
function Fixed16Le(a, b: Fixed16): Boolean;
function Fixed16Ge(a, b: Fixed16): Boolean;

// Advanced operations (Fixed16)
function Fixed16Sqrt(x: Fixed16): Fixed16;
function Fixed16Pow(base, exponent: Fixed16): Fixed16;

// Arithmetic operations (Fixed32)
function Fixed32Add(a, b: Fixed32): Fixed32;
function Fixed32Sub(a, b: Fixed32): Fixed32;
function Fixed32Mul(a, b: Fixed32): Fixed32;
function Fixed32Div(a, b: Fixed32): Fixed32;

implementation

// Conversion: Integer to Fixed16
// Q8.8 format: shift left by 8 bits
function IntToFixed16(i: Integer): Fixed16;
begin
  Result := i shl 8;
end;

// Conversion: Fixed16 to Integer (truncates)
// Q8.8 format: shift right by 8 bits
function Fixed16ToInt(f: Fixed16): Integer;
begin
  Result := f shr 8;
end;

// Conversion: LongInt to Fixed32
function IntToFixed32(i: LongInt): Fixed32;
begin
  Result := i shl 32;
end;

// Conversion: Fixed32 to LongInt (truncates)
function Fixed32ToInt(f: Fixed32): LongInt;
begin
  Result := f shr 32;
end;

// Addition: Fixed16
function Fixed16Add(a, b: Fixed16): Fixed16;
begin
  Result := a + b;
end;

// Subtraction: Fixed16
function Fixed16Sub(a, b: Fixed16): Fixed16;
begin
  Result := a - b;
end;

// Multiplication: Fixed16
// Q8.8 format: (a * b) shr 8
// 16-bit multiply gives 32-bit result, shift right by 8 bits
// NOTE: For 8-bit systems, platform-specific assembly version may be needed for extended precision
function Fixed16Mul(a, b: Fixed16): Fixed16;
begin
  // TODO: Platform-specific assembly version for extended precision on Z80/65C02
  // For now, use basic multiply (works but may lose precision on edge cases)
  Result := (LongInt(a) * LongInt(b)) shr 8;
end;

// Division: Fixed16
// Q8.8 format: (a shl 8) / b
// Scale numerator by 8 bits before division
function Fixed16Div(a, b: Fixed16): Fixed16;
begin
  // Avoid division by zero
  if b = 0 then
  begin
    Result := 0;
    Exit;
  end;
  
  // TODO: Platform-specific extended precision version for 8-bit systems
  // For now, use basic division (works but may lose precision on edge cases)
  Result := (LongInt(a) shl 8) div b;
end;

// Negation: Fixed16
function Fixed16Neg(a: Fixed16): Fixed16;
begin
  Result := -a;
end;

// Absolute value: Fixed16
function Fixed16Abs(a: Fixed16): Fixed16;
begin
  if a < 0 then
    Result := -a
  else
    Result := a;
end;

// Equality: Fixed16
function Fixed16Eq(a, b: Fixed16): Boolean;
begin
  Result := a = b;
end;

// Less than: Fixed16
function Fixed16Lt(a, b: Fixed16): Boolean;
begin
  Result := a < b;
end;

// Greater than: Fixed16
function Fixed16Gt(a, b: Fixed16): Boolean;
begin
  Result := a > b;
end;

// Less than or equal: Fixed16
function Fixed16Le(a, b: Fixed16): Boolean;
begin
  Result := a <= b;
end;

// Greater than or equal: Fixed16
function Fixed16Ge(a, b: Fixed16): Boolean;
begin
  Result := a >= b;
end;

// Square root: Fixed16
// Fast integer approximation algorithm
// Source: algorithms/02_MathematicalAlgorithms.md (SQRT.TXT)
function Fixed16Sqrt(x: Fixed16): Fixed16;
var
  root, bit, temp: Fixed16;
begin
  if x <= 0 then
  begin
    Result := 0;
    Exit;
  end;
  
  root := 0;
  // Q8.8 format: Start with bit at position 14 (8+6, leaving room for integer part)
  bit := FIXED16_ONE shl 14;
  
  while bit > x do
    bit := bit shr 2;
  
  while bit <> 0 do
  begin
    temp := root + bit;
    if x >= temp then
    begin
      x := x - temp;
      root := temp + bit;
    end;
    root := root shr 1;
    bit := bit shr 2;
  end;
  
  Result := root;
end;

// Power: Fixed16
// base^exponent using fixed-point arithmetic
function Fixed16Pow(base, exponent: Fixed16): Fixed16;
var
  result: Fixed16;
  exp: Integer;
begin
  // For integer exponents, use repeated multiplication
  exp := Fixed16ToInt(exponent);
  
  if exp = 0 then
  begin
    Result := FIXED16_ONE;
    Exit;
  end;
  
  if exp < 0 then
  begin
    base := Fixed16Div(FIXED16_ONE, base);
    exp := -exp;
  end;
  
  result := FIXED16_ONE;
  while exp > 0 do
  begin
    if (exp and 1) <> 0 then
      result := Fixed16Mul(result, base);
    base := Fixed16Mul(base, base);
    exp := exp shr 1;
  end;
  
  Result := result;
end;

// Addition: Fixed32
function Fixed32Add(a, b: Fixed32): Fixed32;
begin
  Result := a + b;
end;

// Subtraction: Fixed32
function Fixed32Sub(a, b: Fixed32): Fixed32;
begin
  Result := a - b;
end;

// Multiplication: Fixed32
// NOTE: Requires 64-bit intermediate result
// Only available on platforms with 64-bit support
function Fixed32Mul(a, b: Fixed32): Fixed32;
begin
  {$IFDEF CPU_64BIT}
    // 64-bit platforms: Use native 64-bit multiply
    Result := (Int64(a) * Int64(b)) shr 32;
  {$ELSE}
    // 32-bit and below: Fixed32 not fully supported
    // Return approximate result (may lose precision)
    Result := (a div 65536) * (b div 65536);
  {$ENDIF}
end;

// Division: Fixed32
// NOTE: Requires 64-bit intermediate result
// Only available on platforms with 64-bit support
function Fixed32Div(a, b: Fixed32): Fixed32;
begin
  if b = 0 then
  begin
    Result := 0;
    Exit;
  end;
  
  {$IFDEF CPU_64BIT}
    Result := (Int64(a) shl 32) div b;
  {$ELSE}
    // 32-bit and below: Fixed32 not fully supported
    // Return approximate result (may lose precision)
    Result := (a div 65536) div (b div 65536);
  {$ENDIF}
end;

end.

