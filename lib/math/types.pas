unit Math_Types;

interface

// Core mathematical types and constants
// Source: algorithms/01_FixedPointArithmetic.md, 02_MathematicalAlgorithms.md

type
  // Fixed-point types
  // Using Q8.8 format (16-bit) across all platforms for simplicity and consistency
  // Precision: ~0.0039 (1/256), Range: -128.0 to +127.996
  // Pi error: ~0.001 (0.1%) - acceptable for game/graphics applications
  Fixed16 = SmallInt;    // 8.8 fixed-point format (Q8.8) - unified across all platforms
  Fixed32 = LongInt;     // 32.32 fixed-point format (Q32.32) - for high precision when needed
  
  // Vector types
  TVector2 = record
    X, Y: Fixed16;
  end;
  
  TVector3 = record
    X, Y, Z: Fixed16;
  end;
  
  // Matrix types
  TMatrix2x2 = array[0..1, 0..1] of Fixed16;
  TMatrix3x3 = array[0..2, 0..2] of Fixed16;
  TMatrix4x4 = array[0..3, 0..3] of Fixed16;

// Fixed-point constants
// Using Q8.8 format (16-bit) across all platforms for simplicity
// Precision: ~0.0039 (1/256), Range: -128.0 to +127.996
// Pi error: ~0.001 (0.1%) - acceptable for game/graphics applications
const
  FIXED16_ONE = $0100;           // 1.0 in Q8.8 format (256)
  FIXED16_HALF = $0080;          // 0.5 in Q8.8 format (128)
  FIXED16_PI = $0324;            // Pi (3.140625) in Q8.8 format (804) - error: ~0.001
  FIXED16_TWO_PI = $0648;        // 2*Pi (6.28125) in Q8.8 format (1608)
  FIXED16_PI_OVER_2 = $0192;     // Pi/2 (1.5703125) in Q8.8 format (402)
  FIXED16_PI_OVER_3 = $010C;     // Pi/3 (1.046875) in Q8.8 format (268)
  FIXED16_PI_OVER_4 = $00C9;     // Pi/4 (0.78515625) in Q8.8 format (201)
  FIXED16_PI_OVER_6 = $0086;     // Pi/6 (0.5234375) in Q8.8 format (134)

  // Fixed32 constants (32.32 format, 64-bit)
  const
    FIXED32_ONE = $0000000100000000;  // 1.0 in Fixed32 format (if supported)

// Math constants (as Fixed16)
// These are aliases for convenience - use FIXED16_* for explicit fixed-point
const
  PI = FIXED16_PI;              // Pi in Fixed16 format
  TWO_PI = FIXED16_TWO_PI;      // 2*Pi in Fixed16 format
  PI_OVER_2 = FIXED16_PI_OVER_2; // Pi/2 in Fixed16 format
  PI_OVER_3 = FIXED16_PI_OVER_3; // Pi/3 in Fixed16 format
  PI_OVER_4 = FIXED16_PI_OVER_4; // Pi/4 in Fixed16 format
  PI_OVER_6 = FIXED16_PI_OVER_6; // Pi/6 in Fixed16 format
  E = $02B8;                   // Euler's number (2.71875) in Q8.8 format (696) - error: ~0.0005
  TAU = FIXED16_TWO_PI;         // Tau (2*Pi) in Fixed16 format

implementation

end.

