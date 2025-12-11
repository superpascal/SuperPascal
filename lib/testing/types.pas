unit Testing_Types;

interface

// Core testing framework types
// Source: TESTING_FRAMEWORK_DESIGN.md

// ============================================================================
// Test Status
// ============================================================================

type
  TTestStatus = (tsPassed, tsFailed, tsSkipped, tsError);

// ============================================================================
// Test Result
// ============================================================================

type
  TTestResult = record
    TestName: ShortString;      // Test name (max 255 chars for compatibility)
    SuiteName: ShortString;     // Suite name
    Status: TTestStatus;         // Test status
    Message: ShortString;         // Failure/error message
    Duration: integer;           // Duration in milliseconds (0 if not available)
  end;

  TTestResults = array of TTestResult;

// ============================================================================
// Test Case
// ============================================================================

type
  TTestProcedure = procedure;

  TTestCase = record
    Name: ShortString;           // Test name
    SuiteName: ShortString;      // Suite name
    TestProc: TTestProcedure;    // Test procedure
    SetupProc: TTestProcedure;   // Optional setup procedure
    TeardownProc: TTestProcedure; // Optional teardown procedure
    Skip: boolean;               // Skip this test
    ExpectedException: ShortString; // Expected exception type (if any)
  end;

// ============================================================================
// Test Suite
// ============================================================================

type
  TTestSuite = record
    Name: ShortString;           // Suite name
    Tests: array of TTestCase;   // Test cases
    Count: integer;              // Number of tests
    SetupClassProc: TTestProcedure;   // setUpClass: runs once before all tests in suite (Python-style)
    TeardownClassProc: TTestProcedure; // tearDownClass: runs once after all tests in suite (Python-style)
    SetupProc: TTestProcedure;   // setUp: runs before each test (Python-style) - DEPRECATED, use BeforeEachProc
    TeardownProc: TTestProcedure; // tearDown: runs after each test (Python-style) - DEPRECATED, use AfterEachProc
    BeforeEachProc: TTestProcedure; // setUp: runs before each test (Python-style)
    AfterEachProc: TTestProcedure;  // tearDown: runs after each test (Python-style)
  end;

// ============================================================================
// Test Registry
// ============================================================================

type
  TTestRegistry = record
    Suites: array of TTestSuite;
    SuiteCount: integer;
    GlobalSetupProc: TTestProcedure;    // Global setup (before all tests)
    GlobalTeardownProc: TTestProcedure; // Global teardown (after all tests)
  end;

// ============================================================================
// Test Runner Options
// ============================================================================

type
  TTestRunnerOptions = record
    StopOnFirstFailure: boolean;
    Verbose: boolean;
    ShowTiming: boolean;
    FilterSuite: ShortString;    // Run only this suite (empty = all)
    FilterTest: ShortString;     // Run only this test (empty = all)
  end;

// ============================================================================
// Global Test Registry
// ============================================================================

var
  TestRegistry: TTestRegistry;
  RunnerOptions: TTestRunnerOptions;

// ============================================================================
// Helper Functions
// ============================================================================

// Create empty test result
function CreateTestResult(
  const suiteName, testName: ShortString;
  status: TTestStatus;
  const message: ShortString;
  duration: integer
): TTestResult;

// Create test case
function CreateTestCase(
  const suiteName, testName: ShortString;
  testProc: TTestProcedure
): TTestCase;

// Create test case with fixtures
function CreateTestCaseWithFixtures(
  const suiteName, testName: ShortString;
  testProc, setupProc, teardownProc: TTestProcedure
): TTestCase;

implementation

// Create empty test result
function CreateTestResult(
  const suiteName, testName: ShortString;
  status: TTestStatus;
  const message: ShortString;
  duration: integer
): TTestResult;
begin
  Result.SuiteName := suiteName;
  Result.TestName := testName;
  Result.Status := status;
  Result.Message := message;
  Result.Duration := duration;
end;

// Create test case
function CreateTestCase(
  const suiteName, testName: ShortString;
  testProc: TTestProcedure
): TTestCase;
begin
  Result.SuiteName := suiteName;
  Result.Name := testName;
  Result.TestProc := testProc;
  Result.SetupProc := nil;
  Result.TeardownProc := nil;
  Result.Skip := False;
  Result.ExpectedException := '';
end;

// Create test case with fixtures
function CreateTestCaseWithFixtures(
  const suiteName, testName: ShortString;
  testProc, setupProc, teardownProc: TTestProcedure
): TTestCase;
begin
  Result.SuiteName := suiteName;
  Result.Name := testName;
  Result.TestProc := testProc;
  Result.SetupProc := setupProc;
  Result.TeardownProc := teardownProc;
  Result.Skip := False;
  Result.ExpectedException := '';
end;

// Initialize global registry
begin
  TestRegistry.SuiteCount := 0;
  TestRegistry.GlobalSetupProc := nil;
  TestRegistry.GlobalTeardownProc := nil;
  SetLength(TestRegistry.Suites, 0);
  
  RunnerOptions.StopOnFirstFailure := False;
  RunnerOptions.Verbose := False;
  RunnerOptions.ShowTiming := False;
  RunnerOptions.FilterSuite := '';
  RunnerOptions.FilterTest := '';
end.

