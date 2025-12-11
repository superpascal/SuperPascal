unit Math_Trig;

interface

// Trigonometry functions using lookup tables
// Source: algorithms/02_MathematicalAlgorithms.md
// Based on: Mikro archive SIN.TXT, SINCOSC.TXT
//
// High-precision signed angle system (-180 to +180 degrees, full 360° circle)
// SmallInt-based (-32768 to +32767), 65536 discrete angles (~0.0055° per step)
// Note: -180° = +180° (same point), so this covers the full 360° circle
// Uses quarter-circle symmetry: only stores 0° to 90° (90 entries), derives rest using symmetry
// Memory: 180 bytes (90 entries × 2 bytes) - 87.5% reduction from 1440 bytes!
//
// Also provides:
// - Radians (Fixed16) - Standard math interface
// - Degrees (integer) - Convenient for users

uses
  Math_Types,
  Math_Fixed;

// Trigonometry lookup tables
// Using quarter-circle symmetry: only store 0° to 90° (90 entries)
// All other angles derived using symmetry:
//   sin(θ) = sin(180° - θ) for 90° to 180°
//   sin(θ) = -sin(θ - 180°) for 180° to 360°
//   cos(θ) = sin(90° - θ) for all angles
var
  SinTable: array[0..89] of Fixed16;      // Quarter-circle table: 90 entries (180 bytes) - 0° to 90°
  TrigTablesInitialized: Boolean;

// Initialize trigonometry tables
procedure InitTrigTables;
procedure GenerateTrigTables;
procedure GenerateSinTableRecursive(var table: array[0..1023] of LongInt);

// ============================================================================
// High-precision signed angle functions (-180 to +180 degrees, full 360° circle)
// SmallInt-based (-32768 to +32767), 65536 discrete angles (~0.0055° per step)
// Memory: 16-bit signed (SmallInt)
// Uses quarter-circle symmetry with linear interpolation for accuracy
// Fast path: When angle aligns with table entry (fraction=0), uses direct lookup
// ============================================================================

function SinSigned(angle: SmallInt): Fixed16;
function CosSigned(angle: SmallInt): Fixed16;
function TanSigned(angle: SmallInt): Fixed16;

// ============================================================================
// Radian-based functions (Fixed16 radians)
// Standard mathematical interface
// ============================================================================

// Convert radians to high-precision signed angle (-180 to +180)
function RadiansToSignedAngle(radians: Fixed16): SmallInt;

// Convert high-precision signed angle (-180 to +180) to radians
function SignedAngleToRadians(angle: SmallInt): Fixed16;

// Radian-based trig functions (use high-precision signed angles)
function SinR(radians: Fixed16): Fixed16;
function CosR(radians: Fixed16): Fixed16;
function TanR(radians: Fixed16): Fixed16;

// ============================================================================
// Degree-based functions (integer degrees)
// Convenient for users, converts to signed angles internally
// ============================================================================

// Convert degrees to high-precision signed angle (-180 to +180)
function DegreesToSignedAngle(degrees: integer): SmallInt;

// Convert high-precision signed angle (-180 to +180) to degrees
function SignedAngleToDegrees(angle: SmallInt): integer;

// Convert degrees to radians
function DegreesToRadians(degrees: integer): Fixed16;

// Convert radians to degrees
function RadiansToDegrees(radians: Fixed16): integer;

// Degree-based trig functions (use high-precision signed angles)
function SinD(degrees: integer): Fixed16;
function CosD(degrees: integer): Fixed16;
function TanD(degrees: integer): Fixed16;

// ============================================================================
// Inverse trigonometry functions
// ============================================================================

// Inverse functions (return Fixed16 radians)
function ArcSin(x: Fixed16): Fixed16;
function ArcCos(x: Fixed16): Fixed16;
function ArcTan(x: Fixed16): Fixed16;
function ArcTan2(y, x: Fixed16): Fixed16;

// Inverse functions (return degrees)
function ArcSinD(x: Fixed16): integer;
function ArcCosD(x: Fixed16): integer;
function ArcTanD(x: Fixed16): integer;
function ArcTan2D(y, x: Fixed16): integer;

implementation

// Initialize trigonometry tables
// Uses recursive method (no floating-point required) for maximum portability
// This is the DEFAULT initialization method - works on ALL platforms (including no-FPU)
// Stores only 0° to 90° using quarter-circle symmetry
procedure InitTrigTables;
var
  cosTable: array[0..1023] of LongInt;  // Internal: 2^24 scale (higher precision)
  i: integer;
  idx, sinIdx: integer;
  cosVal: LongInt;
begin
  if not TrigTablesInitialized then
  begin
    // Generate 1024-entry cosine table using recursive method (no FPU needed)
    // Table values are at 2^24 scale for precision during generation
    GenerateSinTableRecursive(cosTable);
    
    // Convert cosine table to sine table and downsample to 90 entries (0° to 90°)
    // sin(x) = cos(x - π/2), so for 1024 entries: sin[i] = cos[(i - 256) mod 1024]
    for i := 0 to 89 do
    begin
      // Map 0-89 to 0-1023
      idx := (i * 1024) div 90;
      
      // Convert cos to sin: sin(x) = cos(x - π/2)
      // π/2 in 1024-entry circle = 256
      sinIdx := (idx - 256 + 1024) mod 1024;
      
      // Get cosine value at 2^24 scale
      cosVal := cosTable[sinIdx];
      
      // Scale from 2^24 to sin/cos range (-32767 to +32767)
      // cosVal is at 2^24 scale, we want: (cosVal / 2^24) * 32767
      // = (cosVal * 32767) / 16777216
      SinTable[i] := (cosVal * 32767) div 16777216;
    end;
    
    TrigTablesInitialized := True;
  end;
end;

// Generate sin/cos tables (standard method using floating-point)
// NOTE: This requires FPU - only use on platforms with floating-point support
// For no-FPU platforms, InitTrigTables uses GenerateSinTableRecursive instead
procedure GenerateTrigTables;
var
  i: integer;
  angle: Real;
begin
  // This method uses floating-point for table generation
  // Only available on platforms with FPU (e.g., Raspberry Pi 5)
  // On no-FPU platforms, InitTrigTables uses GenerateSinTableRecursive
  {$IFDEF HAS_FPU}
  for i := 0 to 89 do
  begin
    angle := (i * 2.0 * 3.141592653589793) / 360.0;  // Convert to radians (0° to 90°)
    SinTable[i] := Trunc(Sin(angle) * 32767.0);
  end;
  {$ELSE}
  // Fallback to recursive method on no-FPU platforms
  InitTrigTables;
  {$ENDIF}
end;

// Generate cosine table using recursive formula (no floating-point needed)
// Source: SIN.TXT - recursive generation
// This is the DEFAULT method for all platforms (works on no-FPU platforms)
// Generates cosine values at 2^24 scale (higher precision for calculation)
// Note: Uses LongInt for internal calculation to handle 2^24 scale properly
procedure GenerateSinTableRecursive(var table: array[0..1023] of LongInt);
var
  i: integer;
  cos2PiN: LongInt;  // cos(2π/N) where N = 1024 (at 2^24 scale)
  temp: LongInt;
begin
  // First two values: cos(0) = 1, cos(2π/1024)
  // Using 2^24 scale for internal precision: 1.0 = 16777216
  table[0] := 16777216;  // 2^24 = 1.0 in higher precision
  table[1] := 16776900;  // 2^24 * cos(2π/1024) (pre-computed constant)
  
  cos2PiN := table[1];
  
  // Recursive formula: cos(k) = 2*cos(2π/N)*cos(k-1) - cos(k-2)
  // This generates cosine values at 2^24 scale
  // All arithmetic is done at 2^24 scale
  for i := 2 to 1023 do
  begin
    // Calculate: 2 * cos2PiN * table[i-1] - table[i-2]
    // All values are at 2^24 scale, so multiply gives 2^48 scale
    // We need to divide by 2^24 to get back to 2^24 scale
    temp := (2 * cos2PiN * table[i - 1]) div 16777216;  // Divide by 2^24
    table[i] := temp - table[i - 2];
  end;
end;

// ============================================================================
// Quarter-circle symmetry helper functions
// ============================================================================

// Map angle to 0°-90° range using quarter-circle symmetry
// Returns: (baseAngle: 0-89, sign: -1 or +1, needsComplement: boolean)
// needsComplement: true if we need sin(90° - θ) instead of sin(θ)
procedure MapAngleToQuarterCircle(angleDegrees: integer; var baseAngle: integer; var sign: integer; var needsComplement: boolean);
begin
  // Normalize to 0-359 range
  angleDegrees := angleDegrees mod 360;
  if angleDegrees < 0 then
    angleDegrees := angleDegrees + 360;
  
  // Map to quadrant and apply symmetry
  if angleDegrees <= 90 then
  begin
    // Quadrant I: 0° to 90° - direct lookup
    baseAngle := angleDegrees;
    sign := 1;
    needsComplement := False;
  end
  else if angleDegrees <= 180 then
  begin
    // Quadrant II: 90° to 180° - sin(θ) = sin(180° - θ)
    baseAngle := 180 - angleDegrees;
    sign := 1;
    needsComplement := False;
  end
  else if angleDegrees <= 270 then
  begin
    // Quadrant III: 180° to 270° - sin(θ) = -sin(θ - 180°)
    baseAngle := angleDegrees - 180;
    sign := -1;
    needsComplement := False;
  end
  else
  begin
    // Quadrant IV: 270° to 360° - sin(θ) = -sin(360° - θ)
    baseAngle := 360 - angleDegrees;
    sign := -1;
    needsComplement := False;
  end;
end;

// Get sine value from quarter-circle table using symmetry
function GetSinFromQuarterCircle(angleDegrees: integer): Fixed16;
var
  baseAngle: integer;
  sign: integer;
  needsComplement: boolean;
  tableIndex: integer;
  fraction: integer;
  lowVal, highVal: Fixed16;
  lowAngle, highAngle: integer;
begin
  if not TrigTablesInitialized then
    InitTrigTables;
  
  // Map angle to 0°-90° range
  MapAngleToQuarterCircle(angleDegrees, baseAngle, sign, needsComplement);
  
  // Clamp baseAngle to 0-89
  if baseAngle >= 90 then
    baseAngle := 89;
  
  // Direct lookup (no interpolation for integer degrees)
  // For high precision, we'd need interpolation, but for now use direct lookup
  tableIndex := baseAngle;
  if tableIndex >= 90 then
    tableIndex := 89;
  
  Result := SinTable[tableIndex];
  
  // Apply sign
  if sign < 0 then
    Result := -Result;
end;

// ============================================================================
// High-precision signed angle functions (-180 to +180, full 360° circle)
// Uses quarter-circle symmetry with linear interpolation
// ============================================================================

// High-precision signed sine (angle: -180 to +180 degrees, SmallInt)
// Uses quarter-circle symmetry: only stores 0° to 90°, derives rest using symmetry
// Maps SmallInt (-32768 to +32767) to full 360° circle
// Fast path: When fraction=0 (angle aligns with table entry), uses direct lookup
function SinSigned(angle: SmallInt): Fixed16;
var
  // Convert signed angle to 0-65535 range for interpolation
  // -32768 -> 0, 0 -> 32768, +32767 -> 65535
  unsignedAngle: LongInt;
  angleDegrees: integer;
  baseAngle: integer;
  sign: integer;
  needsComplement: boolean;
  tableIndex: integer;
  fraction: LongInt;
  lowVal, highVal: Fixed16;
  lowAngle, highAngle: integer;
  entryRange: LongInt;  // 65536 / 90 = ~728.18 (for quarter-circle)
begin
  if not TrigTablesInitialized then
    InitTrigTables;
  
  // Convert SmallInt (-32768 to +32767) to 0-65535 range
  // Add 32768 to shift from signed to unsigned
  unsignedAngle := LongInt(angle) + 32768;
  
  // Convert to degrees (0-359)
  // unsignedAngle / 65536 * 360
  angleDegrees := (unsignedAngle * 360) div 65536;
  if angleDegrees >= 360 then
    angleDegrees := 359;
  
  // Map to quarter-circle (0° to 90°) using symmetry
  MapAngleToQuarterCircle(angleDegrees, baseAngle, sign, needsComplement);
  
  // Calculate fraction for interpolation within quarter-circle
  // Each table entry covers 65536/90 angles (~728.18 angles per entry)
  entryRange := 65536 div 90;  // ~728
  fraction := unsignedAngle mod entryRange;
  
  // Get table indices for interpolation
  // Map baseAngle (0-89) to table index, with interpolation
  tableIndex := baseAngle;
  if tableIndex >= 90 then
    tableIndex := 89;
  
  lowAngle := tableIndex;
  highAngle := tableIndex + 1;
  if highAngle >= 90 then
    highAngle := 89;  // Clamp to table bounds
  
  lowVal := SinTable[lowAngle];
  highVal := SinTable[highAngle];
  
  // Interpolate within quarter-circle
  if fraction = 0 then
    Result := lowVal
  else
    Result := lowVal + Fixed16Div(Fixed16Mul(Fixed16Sub(highVal, lowVal), IntToFixed16(fraction)), IntToFixed16(entryRange));
  
  // Apply sign based on quadrant
  if sign < 0 then
    Result := -Result;
end;

// High-precision signed cosine (angle: -180 to +180 degrees, SmallInt)
// Uses identity: cos(θ) = sin(90° - θ) for 0° to 90°
// For other quadrants, uses symmetry: cos(θ) = -cos(θ - 180°) for 180° to 360°
function CosSigned(angle: SmallInt): Fixed16;
var
  unsignedAngle: LongInt;
  angleDegrees: integer;
  baseAngle: integer;
  sign: integer;
  needsComplement: boolean;
  tableIndex: integer;
  fraction: LongInt;
  lowVal, highVal: Fixed16;
  lowAngle, highAngle: integer;
  entryRange: LongInt;
begin
  if not TrigTablesInitialized then
    InitTrigTables;
  
  // Convert SmallInt (-32768 to +32767) to 0-65535 range
  unsignedAngle := LongInt(angle) + 32768;
  
  // Convert to degrees (0-359)
  angleDegrees := (unsignedAngle * 360) div 65536;
  if angleDegrees >= 360 then
    angleDegrees := 359;
  
  // For cosine: cos(θ) = sin(90° - θ) for 0° to 90°
  // For other quadrants, use symmetry
  // cos(θ) = -sin(θ - 90°) for 90° to 180°
  // cos(θ) = -cos(θ - 180°) for 180° to 360°
  
  // Map using cosine symmetry
  if angleDegrees <= 90 then
  begin
    // Quadrant I: cos(θ) = sin(90° - θ)
    baseAngle := 90 - angleDegrees;
    sign := 1;
  end
  else if angleDegrees <= 180 then
  begin
    // Quadrant II: cos(θ) = -sin(θ - 90°)
    baseAngle := angleDegrees - 90;
    sign := -1;
  end
  else if angleDegrees <= 270 then
  begin
    // Quadrant III: cos(θ) = -cos(θ - 180°) = -sin(90° - (θ - 180°))
    baseAngle := 90 - (angleDegrees - 180);
    sign := -1;
  end
  else
  begin
    // Quadrant IV: cos(θ) = cos(360° - θ) = sin(90° - (360° - θ))
    baseAngle := 90 - (360 - angleDegrees);
    sign := 1;
  end;
  
  // Clamp baseAngle to 0-89
  if baseAngle < 0 then
    baseAngle := 0
  else if baseAngle >= 90 then
    baseAngle := 89;
  
  // Calculate fraction for interpolation
  entryRange := 65536 div 90;
  fraction := unsignedAngle mod entryRange;
  
  // Get table indices
  tableIndex := baseAngle;
  lowAngle := tableIndex;
  highAngle := tableIndex + 1;
  if highAngle >= 90 then
    highAngle := 89;
  
  lowVal := SinTable[lowAngle];
  highVal := SinTable[highAngle];
  
  // Interpolate
  if fraction = 0 then
    Result := lowVal
  else
    Result := lowVal + Fixed16Div(Fixed16Mul(Fixed16Sub(highVal, lowVal), IntToFixed16(fraction)), IntToFixed16(entryRange));
  
  // Apply sign
  if sign < 0 then
    Result := -Result;
end;

// High-precision signed tangent (angle: -180 to +180 degrees, SmallInt)
function TanSigned(angle: SmallInt): Fixed16;
var
  sinVal, cosVal: Fixed16;
begin
  sinVal := SinSigned(angle);
  cosVal := CosSigned(angle);
  
  if cosVal = 0 then
  begin
    Result := 0;
    Exit;
  end;
  
  Result := Fixed16Div(sinVal, cosVal);
end;

// ============================================================================
// Radian-based functions
// ============================================================================

// Convert radians to high-precision signed angle (-180 to +180)
function RadiansToSignedAngle(radians: Fixed16): SmallInt;
var
  angle: Fixed16;
  unsignedAngle: LongInt;
begin
  // Convert radians to 0-65535 range first
  // radians / (2π) * 65536
  // = radians * 65536 / (2π)
  // = radians * 65536 / TWO_PI
  angle := Fixed16Div(Fixed16Mul(radians, IntToFixed16(65536)), FIXED16_TWO_PI);
  unsignedAngle := Fixed16ToInt(angle) and $FFFF;
  
  // Convert 0-65535 to -32768 to +32767 (signed)
  // Subtract 32768 to shift range
  Result := SmallInt(unsignedAngle - 32768);
end;

// Convert high-precision signed angle (-180 to +180) to radians
function SignedAngleToRadians(angle: SmallInt): Fixed16;
var
  unsignedAngle: LongInt;
begin
  // Convert -32768 to +32767 to 0-65535
  // Add 32768 to shift range
  unsignedAngle := LongInt(angle) + 32768;
  
  // Convert 0-65535 range to radians
  // unsignedAngle / 65536 * 2π
  // = unsignedAngle * 2π / 65536
  // = unsignedAngle * TWO_PI / 65536
  Result := Fixed16Div(Fixed16Mul(IntToFixed16(unsignedAngle), FIXED16_TWO_PI), IntToFixed16(65536));
end;

// Radian-based sine (uses high-precision signed angles)
function SinR(radians: Fixed16): Fixed16;
var
  angle: SmallInt;
begin
  angle := RadiansToSignedAngle(radians);
  Result := SinSigned(angle);
end;

// Radian-based cosine (uses high-precision signed angles)
function CosR(radians: Fixed16): Fixed16;
var
  angle: SmallInt;
begin
  angle := RadiansToSignedAngle(radians);
  Result := CosSigned(angle);
end;

// Radian-based tangent (uses high-precision signed angles)
function TanR(radians: Fixed16): Fixed16;
var
  angle: SmallInt;
begin
  angle := RadiansToSignedAngle(radians);
  Result := TanSigned(angle);
end;

// ============================================================================
// Degree-based functions
// ============================================================================

// Convert degrees to high-precision signed angle (-180 to +180)
// Implements unit circle mapping: φ = (θ + 180°) mod 360° - 180°
// Upper semi-circle (0° to +180°): positive angles, counter-clockwise
// Lower semi-circle (-180° to 0°): negative angles, clockwise
// Key points: 0° = +x-axis, +90° = +y-axis, +180° = -x-axis (same as -180°), -90° = -y-axis
function DegreesToSignedAngle(degrees: integer): SmallInt;
var
  normalizedDegrees: integer;
begin
  // Normalize to -180..+180 range (covers full 360° circle)
  // Formula: φ = (θ + 180°) mod 360° - 180°
  // Examples: 0°→0°, 90°→+90°, 180°→+180°, 270°→-90°, 300°→-60°, 210°→-150°
  degrees := degrees mod 360;
  if degrees > 180 then
    normalizedDegrees := degrees - 360  // Map 181°-359° to -179° to -1°
  else if degrees < -180 then
    normalizedDegrees := degrees + 360   // Handle negative input angles
  else
    normalizedDegrees := degrees;        // 0° to +180° stay as-is
  
  // Convert to signed angle: normalizedDegrees / 180 * 32768
  // = normalizedDegrees * 32768 / 180
  // Maps -180° to -32768, 0° to 0, +180° to +32767
  Result := SmallInt((normalizedDegrees * 32768) div 180);
end;

// Convert high-precision signed angle (-180 to +180) to degrees
function SignedAngleToDegrees(angle: SmallInt): integer;
begin
  // Convert signed angle to degrees
  // angle / 32768 * 180
  // = angle * 180 / 32768
  // = angle * 45 / 8192 (simplified)
  Result := (LongInt(angle) * 45) div 8192;
end;

// Convert degrees to radians
function DegreesToRadians(degrees: integer): Fixed16;
begin
  // degrees * π / 180
  Result := Fixed16Div(Fixed16Mul(IntToFixed16(degrees), FIXED16_PI), IntToFixed16(180));
end;

// Convert radians to degrees
function RadiansToDegrees(radians: Fixed16): integer;
begin
  // radians * 180 / π
  Result := Fixed16ToInt(Fixed16Div(Fixed16Mul(radians, IntToFixed16(180)), FIXED16_PI));
end;

// Degree-based sine (uses high-precision signed angles)
function SinD(degrees: integer): Fixed16;
var
  angle: SmallInt;
begin
  angle := DegreesToSignedAngle(degrees);
  Result := SinSigned(angle);
end;

// Degree-based cosine (uses high-precision signed angles)
function CosD(degrees: integer): Fixed16;
var
  angle: SmallInt;
begin
  angle := DegreesToSignedAngle(degrees);
  Result := CosSigned(angle);
end;

// Degree-based tangent (uses high-precision signed angles)
function TanD(degrees: integer): Fixed16;
var
  angle: SmallInt;
begin
  angle := DegreesToSignedAngle(degrees);
  Result := TanSigned(angle);
end;

// ============================================================================
// Inverse trigonometry functions (return Fixed16 radians)
// ============================================================================

// Arcsine (inverse sine) - returns radians
// Searches quarter-circle table and converts to high-precision signed angle
function ArcSin(x: Fixed16): Fixed16;
var
  i: integer;
  bestAngle: integer;
  bestDiff, diff: Fixed16;
  signedAngle: SmallInt;
  angleDegrees: integer;
begin
  // Clamp x to valid range [-1, 1]
  if x > FIXED16_ONE then
    x := FIXED16_ONE
  else if x < -FIXED16_ONE then
    x := -FIXED16_ONE;
  
  // Search quarter-circle table (90 entries) to find closest match
  // Need to check both positive and negative values
  bestAngle := 0;
  bestDiff := Fixed16Abs(x - SinTable[0]);
  
  // Check positive values (0° to 90°)
  for i := 1 to 89 do
  begin
    diff := Fixed16Abs(x - SinTable[i]);
    if diff < bestDiff then
    begin
      bestDiff := diff;
      bestAngle := i;  // 0° to 90°
    end;
  end;
  
  // Check negative values (mirror of 90° to 180°)
  for i := 1 to 89 do
  begin
    diff := Fixed16Abs(x - (-SinTable[i]));
    if diff < bestDiff then
    begin
      bestDiff := diff;
      bestAngle := 180 - i;  // 90° to 180° (mirrored)
    end;
  end;
  
  // Convert table index to degrees, then to signed angle
  // bestAngle is in 0-180 range
  if bestAngle <= 180 then
    angleDegrees := bestAngle
  else
    angleDegrees := bestAngle - 360;
  
  // Convert to signed angle
  if angleDegrees <= 180 then
    signedAngle := SmallInt((angleDegrees * 32768) div 180)  // 0° to +180°
  else
    signedAngle := SmallInt(((angleDegrees - 360) * 32768) div 180);  // -179° to -1°
  
  // Convert signed angle to radians
  Result := SignedAngleToRadians(signedAngle);
end;

// Arccosine (inverse cosine) - returns radians
function ArcCos(x: Fixed16): Fixed16;
begin
  // arccos(x) = π/2 - arcsin(x)
  Result := Fixed16Sub(FIXED16_PI_OVER_2, ArcSin(x));
end;

// Arctangent (inverse tangent) - returns radians
function ArcTan(x: Fixed16): Fixed16;
var
  i: integer;
  tanVal: Fixed16;
  bestAngle: integer;
  bestDiff, diff: Fixed16;
  signedAngle: SmallInt;
  angleDegrees: integer;
  sinVal, cosVal: Fixed16;
begin
  // Search quarter-circle table to find closest tan value
  bestAngle := 0;
  sinVal := SinTable[0];
  cosVal := SinTable[89];  // cos(0°) = sin(90°)
  if cosVal <> 0 then
    tanVal := Fixed16Div(sinVal, cosVal)
  else
    tanVal := 0;
  bestDiff := Fixed16Abs(x - tanVal);
  
  // Search through all 360 degrees by checking quarter-circle
  for i := 1 to 89 do
  begin
    sinVal := SinTable[i];
    cosVal := SinTable[89 - i];  // cos(i°) = sin(90° - i°)
    if cosVal <> 0 then
    begin
      tanVal := Fixed16Div(sinVal, cosVal);
      diff := Fixed16Abs(x - tanVal);
      if diff < bestDiff then
      begin
        bestDiff := diff;
        bestAngle := i;  // 0° to 90°
      end;
    end;
    
    // Check mirrored angles (90° to 180°)
    sinVal := SinTable[89 - i];
    cosVal := -SinTable[i];  // cos(90°+i°) = -sin(i°)
    if cosVal <> 0 then
    begin
      tanVal := Fixed16Div(sinVal, cosVal);
      diff := Fixed16Abs(x - tanVal);
      if diff < bestDiff then
      begin
        bestDiff := diff;
        bestAngle := 90 + i;  // 90° to 180°
      end;
    end;
    
    // Check negative angles (180° to 270°)
    sinVal := -SinTable[i];
    cosVal := -SinTable[89 - i];
    if cosVal <> 0 then
    begin
      tanVal := Fixed16Div(sinVal, cosVal);
      diff := Fixed16Abs(x - tanVal);
      if diff < bestDiff then
      begin
        bestDiff := diff;
        bestAngle := 180 + i;  // 180° to 270°
      end;
    end;
    
    // Check negative angles (270° to 360°)
    sinVal := -SinTable[89 - i];
    cosVal := SinTable[i];
    if cosVal <> 0 then
    begin
      tanVal := Fixed16Div(sinVal, cosVal);
      diff := Fixed16Abs(x - tanVal);
      if diff < bestDiff then
      begin
        bestDiff := diff;
        bestAngle := 270 + i;  // 270° to 360°
      end;
    end;
  end;
  
  // Convert to signed angle
  if bestAngle <= 180 then
    angleDegrees := bestAngle
  else
    angleDegrees := bestAngle - 360;
  
  if angleDegrees <= 180 then
    signedAngle := SmallInt((angleDegrees * 32768) div 180)
  else
    signedAngle := SmallInt(((angleDegrees - 360) * 32768) div 180);
  
  // Convert signed angle to radians
  Result := SignedAngleToRadians(signedAngle);
end;

// Arctangent2 (y/x) - handles all quadrants, returns radians
function ArcTan2(y, x: Fixed16): Fixed16;
var
  radians: Fixed16;
begin
  if x = 0 then
  begin
    if y > 0 then
      Result := FIXED16_PI_OVER_2  // 90 degrees = π/2
    else if y < 0 then
      Result := Fixed16Sub(FIXED16_TWO_PI, FIXED16_PI_OVER_2)  // 270 degrees = 3π/2
    else
      Result := 0;  // Undefined
    Exit;
  end;
  
  // Use arctan and adjust quadrant
  radians := ArcTan(Fixed16Div(y, x));
  
  if x < 0 then
    radians := Fixed16Add(radians, FIXED16_PI);  // Add 180 degrees (π)
  
  Result := radians;
end;

// ============================================================================
// Inverse trigonometry functions (return degrees)
// ============================================================================

// Arcsine (inverse sine) - returns degrees
function ArcSinD(x: Fixed16): integer;
begin
  Result := RadiansToDegrees(ArcSin(x));
end;

// Arccosine (inverse cosine) - returns degrees
function ArcCosD(x: Fixed16): integer;
begin
  Result := RadiansToDegrees(ArcCos(x));
end;

// Arctangent (inverse tangent) - returns degrees
function ArcTanD(x: Fixed16): integer;
begin
  Result := RadiansToDegrees(ArcTan(x));
end;

// Arctangent2 (y/x) - returns degrees
function ArcTan2D(y, x: Fixed16): integer;
begin
  Result := RadiansToDegrees(ArcTan2(y, x));
end;

// Initialize tables on unit load
begin
  TrigTablesInitialized := False;
end.
