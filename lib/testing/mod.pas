unit Testing;

interface

// SuperPascal Unit Testing Framework
// Source: TESTING_FRAMEWORK_DESIGN.md
//
// A comprehensive unit testing framework for SuperPascal, designed from
// modern testing principles but adapted for retro computing platforms.
//
// Key Features:
// - Platform-agnostic (works on all platforms)
// - Rich assertions (True, False, Equal, NotEqual, etc.)
// - Test fixtures (setup/teardown, BeforeEach/AfterEach)
// - Test suites and organization
// - Clear reporting with summary statistics
// - No external dependencies
// - Memory-efficient for 8-bit platforms

// Import all testing modules
uses
  Testing_Types,
  Testing_Assertions,
  Testing_Runner,
  Testing_Reporting,
  Testing_Fixtures;

// ============================================================================
// Re-export Types
// ============================================================================

type
  TTestStatus = Testing_Types.TTestStatus;
  TTestResult = Testing_Types.TTestResult;
  TTestResults = Testing_Types.TTestResults;
  TTestCase = Testing_Types.TTestCase;
  TTestSuite = Testing_Types.TTestSuite;
  TTestProcedure = Testing_Types.TTestProcedure;
  TTestRunnerOptions = Testing_Types.TTestRunnerOptions;

// ============================================================================
// Re-export Constants
// ============================================================================

const
  tsPassed = Testing_Types.tsPassed;
  tsFailed = Testing_Types.tsFailed;
  tsSkipped = Testing_Types.tsSkipped;
  tsError = Testing_Types.tsError;

// ============================================================================
// Re-export Functions
// ============================================================================
// All functions are available through the imported units
// Users can call: RegisterTest(...), RunAllTests(), etc.

implementation

end.

