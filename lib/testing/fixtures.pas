unit Testing_Fixtures;

interface

// Test fixtures and setup/teardown support
// Source: TESTING_FRAMEWORK_DESIGN.md
//
// Note: Most fixture functionality is implemented in Testing_Runner.
// This module provides additional convenience functions and BeforeEach/AfterEach support.

uses
  Testing_Types,
  Testing_Runner;

// ============================================================================
// BeforeEach/AfterEach Support
// ============================================================================
// These run before/after each test in a suite (in addition to test-level setup/teardown)

// Set BeforeEach procedure for a suite
procedure SetBeforeEach(const suiteName: ShortString; proc: TTestProcedure);

// Set AfterEach procedure for a suite
procedure SetAfterEach(const suiteName: ShortString; proc: TTestProcedure);

// ============================================================================
// Convenience Functions
// ============================================================================

// Register test with BeforeEach/AfterEach (convenience wrapper)
procedure RegisterTestWithBeforeAfter(
  const suiteName, testName: ShortString;
  testProc: TTestProcedure;
  beforeEachProc, afterEachProc: TTestProcedure
);

implementation

// Note: BeforeEach/AfterEach are stored per-suite and called by the runner
// For now, we'll use suite-level setup/teardown as a simple implementation
// A full implementation would require storing BeforeEach/AfterEach procedures
// in the TestSuite record and calling them in the runner

// Python-style setUp: runs before each test in suite
procedure SetBeforeEach(const suiteName: ShortString; proc: TTestProcedure);
begin
  SetSuiteBeforeEach(suiteName, proc);
end;

// Python-style tearDown: runs after each test in suite
procedure SetAfterEach(const suiteName: ShortString; proc: TTestProcedure);
begin
  SetSuiteAfterEach(suiteName, proc);
end;

procedure RegisterTestWithBeforeAfter(
  const suiteName, testName: ShortString;
  testProc: TTestProcedure;
  beforeEachProc, afterEachProc: TTestProcedure
);
begin
  // Set BeforeEach/AfterEach for the suite
  if beforeEachProc <> nil then
    SetBeforeEach(suiteName, beforeEachProc);
  if afterEachProc <> nil then
    SetAfterEach(suiteName, afterEachProc);
  
  // Register the test
  RegisterTest(suiteName, testName, testProc);
end;

end.

