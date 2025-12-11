unit Testing_Runner;

interface

// Test runner and execution engine
// Source: TESTING_FRAMEWORK_DESIGN.md

uses
  Testing_Types,
  Testing_Assertions;

// ============================================================================
// Test Registration
// ============================================================================

// Register a single test
procedure RegisterTest(
  const suiteName, testName: ShortString;
  testProc: TTestProcedure
);

// Register test with setup/teardown
procedure RegisterTestWithFixtures(
  const suiteName, testName: ShortString;
  testProc, setupProc, teardownProc: TTestProcedure
);

// Register a test suite
procedure RegisterSuite(const suiteName: ShortString; tests: array of TTestCase);

// ============================================================================
// Test Execution
// ============================================================================

// Run a single test
function RunTest(const suiteName, testName: ShortString): TTestResult;

// Run all tests in a suite
function RunSuite(const suiteName: ShortString): TTestResults;

// Run all registered tests
function RunAllTests: TTestResults;

// Run tests matching pattern (simple substring match)
function RunTestsMatching(const pattern: ShortString): TTestResults;

// ============================================================================
// Test Runner Configuration
// ============================================================================

// Set runner options
procedure SetRunnerOptions(const options: TTestRunnerOptions);

// Get runner options
function GetRunnerOptions: TTestRunnerOptions;

// ============================================================================
// Fixture Management
// ============================================================================

// Global fixtures
procedure SetGlobalSetup(setupProc: TTestProcedure);
procedure SetGlobalTeardown(teardownProc: TTestProcedure);

// Suite-level fixtures (Python-style)
// setUpClass/tearDownClass: run once before/after all tests in suite
procedure SetSuiteSetupClass(const suiteName: ShortString; setupProc: TTestProcedure);
procedure SetSuiteTeardownClass(const suiteName: ShortString; teardownProc: TTestProcedure);

// setUp/tearDown: run before/after each test in suite (Python-style)
procedure SetSuiteSetup(const suiteName: ShortString; setupProc: TTestProcedure); // DEPRECATED: use SetSuiteBeforeEach
procedure SetSuiteTeardown(const suiteName: ShortString; teardownProc: TTestProcedure); // DEPRECATED: use SetSuiteAfterEach
procedure SetSuiteBeforeEach(const suiteName: ShortString; setupProc: TTestProcedure); // Python-style setUp()
procedure SetSuiteAfterEach(const suiteName: ShortString; teardownProc: TTestProcedure); // Python-style tearDown()

// Test-level fixtures (set via RegisterTestWithFixtures)

implementation

// ============================================================================
// Helper Functions
// ============================================================================

// Find suite by name
function FindSuite(const suiteName: ShortString): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to TestRegistry.SuiteCount - 1 do
  begin
    if TestRegistry.Suites[i].Name = suiteName then
    begin
      Result := i;
      Break;
    end;
  end;
end;

// Find test in suite
function FindTest(suiteIndex: integer; const testName: ShortString): integer;
var
  i: integer;
begin
  Result := -1;
  if (suiteIndex >= 0) and (suiteIndex < TestRegistry.SuiteCount) then
  begin
    for i := 0 to TestRegistry.Suites[suiteIndex].Count - 1 do
    begin
      if TestRegistry.Suites[suiteIndex].Tests[i].Name = testName then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

// Execute a single test case (Python-style: setUp -> test -> tearDown)
function ExecuteTestCase(const testCase: TTestCase; suiteIndex: integer): TTestResult;
var
  failureMsg: ShortString;
  startTime, endTime: integer; // Placeholder for timing (0 if not available)
  suite: ^TTestSuite;
begin
  // Initialize result
  Result := CreateTestResult(testCase.SuiteName, testCase.Name, tsPassed, '', 0);
  
  // Skip if marked as skip
  if testCase.Skip then
  begin
    Result.Status := tsSkipped;
    Result.Message := 'Test skipped';
    Exit;
  end;
  
  // Get suite reference
  if (suiteIndex >= 0) and (suiteIndex < TestRegistry.SuiteCount) then
    suite := @TestRegistry.Suites[suiteIndex]
  else
    suite := nil;
  
  // Clear failure state
  ClearTestFailure;
  
  // Run test (Python-style execution order)
  try
    // 1. Run test-level setUp (if provided) - highest priority
    if testCase.SetupProc <> nil then
      testCase.SetupProc
    // 2. Run suite-level setUp (BeforeEach) - Python-style setUp()
    else if (suite <> nil) and (suite^.BeforeEachProc <> nil) then
      suite^.BeforeEachProc
    // 3. Fallback to deprecated suite SetupProc
    else if (suite <> nil) and (suite^.SetupProc <> nil) then
      suite^.SetupProc;
    
    // Record start time (placeholder - actual timing depends on platform)
    startTime := 0;
    
    // 4. Run test procedure
    if testCase.TestProc <> nil then
      testCase.TestProc;
    
    // Record end time (placeholder)
    endTime := 0;
    Result.Duration := endTime - startTime;
    
    // Check for failures
    if GetTestFailure(failureMsg) then
    begin
      Result.Status := tsFailed;
      Result.Message := failureMsg;
    end
    else
    begin
      Result.Status := tsPassed;
    end;
    
    // 5. Run test-level tearDown (if provided) - highest priority
    if testCase.TeardownProc <> nil then
      testCase.TeardownProc
    // 6. Run suite-level tearDown (AfterEach) - Python-style tearDown()
    else if (suite <> nil) and (suite^.AfterEachProc <> nil) then
      suite^.AfterEachProc
    // 7. Fallback to deprecated suite TeardownProc
    else if (suite <> nil) and (suite^.TeardownProc <> nil) then
      suite^.TeardownProc;
      
  except
    // Handle exceptions (if platform supports them)
    Result.Status := tsError;
    Result.Message := 'Exception occurred during test execution';
  end;
end;

// ============================================================================
// Test Registration
// ============================================================================

procedure RegisterTest(
  const suiteName, testName: ShortString;
  testProc: TTestProcedure
);
var
  suiteIndex: integer;
  testCase: TTestCase;
begin
  suiteIndex := FindSuite(suiteName);
  
  // Create suite if it doesn't exist
  if suiteIndex < 0 then
  begin
    suiteIndex := TestRegistry.SuiteCount;
    TestRegistry.SuiteCount := TestRegistry.SuiteCount + 1;
    SetLength(TestRegistry.Suites, TestRegistry.SuiteCount);
    TestRegistry.Suites[suiteIndex].Name := suiteName;
    TestRegistry.Suites[suiteIndex].Count := 0;
    TestRegistry.Suites[suiteIndex].SetupClassProc := nil;
    TestRegistry.Suites[suiteIndex].TeardownClassProc := nil;
    TestRegistry.Suites[suiteIndex].SetupProc := nil;
    TestRegistry.Suites[suiteIndex].TeardownProc := nil;
    TestRegistry.Suites[suiteIndex].BeforeEachProc := nil;
    TestRegistry.Suites[suiteIndex].AfterEachProc := nil;
    SetLength(TestRegistry.Suites[suiteIndex].Tests, 0);
  end;
  
  // Create test case
  testCase := CreateTestCase(suiteName, testName, testProc);
  
  // Add test to suite
  SetLength(TestRegistry.Suites[suiteIndex].Tests, TestRegistry.Suites[suiteIndex].Count + 1);
  TestRegistry.Suites[suiteIndex].Tests[TestRegistry.Suites[suiteIndex].Count] := testCase;
  TestRegistry.Suites[suiteIndex].Count := TestRegistry.Suites[suiteIndex].Count + 1;
end;

procedure RegisterTestWithFixtures(
  const suiteName, testName: ShortString;
  testProc, setupProc, teardownProc: TTestProcedure
);
var
  suiteIndex: integer;
  testCase: TTestCase;
begin
  suiteIndex := FindSuite(suiteName);
  
  // Create suite if it doesn't exist
  if suiteIndex < 0 then
  begin
    suiteIndex := TestRegistry.SuiteCount;
    TestRegistry.SuiteCount := TestRegistry.SuiteCount + 1;
    SetLength(TestRegistry.Suites, TestRegistry.SuiteCount);
    TestRegistry.Suites[suiteIndex].Name := suiteName;
    TestRegistry.Suites[suiteIndex].Count := 0;
    TestRegistry.Suites[suiteIndex].SetupClassProc := nil;
    TestRegistry.Suites[suiteIndex].TeardownClassProc := nil;
    TestRegistry.Suites[suiteIndex].SetupProc := nil;
    TestRegistry.Suites[suiteIndex].TeardownProc := nil;
    TestRegistry.Suites[suiteIndex].BeforeEachProc := nil;
    TestRegistry.Suites[suiteIndex].AfterEachProc := nil;
    SetLength(TestRegistry.Suites[suiteIndex].Tests, 0);
  end;
  
  // Create test case with fixtures
  testCase := CreateTestCaseWithFixtures(suiteName, testName, testProc, setupProc, teardownProc);
  
  // Add test to suite
  SetLength(TestRegistry.Suites[suiteIndex].Tests, TestRegistry.Suites[suiteIndex].Count + 1);
  TestRegistry.Suites[suiteIndex].Tests[TestRegistry.Suites[suiteIndex].Count] := testCase;
  TestRegistry.Suites[suiteIndex].Count := TestRegistry.Suites[suiteIndex].Count + 1;
end;

procedure RegisterSuite(const suiteName: ShortString; tests: array of TTestCase);
var
  suiteIndex, i: integer;
begin
  suiteIndex := FindSuite(suiteName);
  
  // Create suite if it doesn't exist
  if suiteIndex < 0 then
  begin
    suiteIndex := TestRegistry.SuiteCount;
    TestRegistry.SuiteCount := TestRegistry.SuiteCount + 1;
    SetLength(TestRegistry.Suites, TestRegistry.SuiteCount);
    TestRegistry.Suites[suiteIndex].Name := suiteName;
    TestRegistry.Suites[suiteIndex].Count := 0;
    TestRegistry.Suites[suiteIndex].SetupClassProc := nil;
    TestRegistry.Suites[suiteIndex].TeardownClassProc := nil;
    TestRegistry.Suites[suiteIndex].SetupProc := nil;
    TestRegistry.Suites[suiteIndex].TeardownProc := nil;
    TestRegistry.Suites[suiteIndex].BeforeEachProc := nil;
    TestRegistry.Suites[suiteIndex].AfterEachProc := nil;
    SetLength(TestRegistry.Suites[suiteIndex].Tests, 0);
  end;
  
  // Add all tests
  for i := 0 to Length(tests) - 1 do
  begin
    SetLength(TestRegistry.Suites[suiteIndex].Tests, TestRegistry.Suites[suiteIndex].Count + 1);
    TestRegistry.Suites[suiteIndex].Tests[TestRegistry.Suites[suiteIndex].Count] := tests[i];
    TestRegistry.Suites[suiteIndex].Count := TestRegistry.Suites[suiteIndex].Count + 1;
  end;
end;

// ============================================================================
// Test Execution
// ============================================================================

function RunTest(const suiteName, testName: ShortString): TTestResult;
var
  suiteIndex, testIndex: integer;
  testCase: TTestCase;
begin
  suiteIndex := FindSuite(suiteName);
  if suiteIndex < 0 then
  begin
    Result := CreateTestResult(suiteName, testName, tsError, 'Suite not found', 0);
    Exit;
  end;
  
  testIndex := FindTest(suiteIndex, testName);
  if testIndex < 0 then
  begin
    Result := CreateTestResult(suiteName, testName, tsError, 'Test not found', 0);
    Exit;
  end;
  
  testCase := TestRegistry.Suites[suiteIndex].Tests[testIndex];
  
  // Run suite setUpClass if provided (Python-style: runs once before suite)
  if TestRegistry.Suites[suiteIndex].SetupClassProc <> nil then
    TestRegistry.Suites[suiteIndex].SetupClassProc;
  
  // Execute test (includes setUp/tearDown)
  Result := ExecuteTestCase(testCase, suiteIndex);
  
  // Run suite tearDownClass if provided (Python-style: runs once after suite)
  if TestRegistry.Suites[suiteIndex].TeardownClassProc <> nil then
    TestRegistry.Suites[suiteIndex].TeardownClassProc;
end;

function RunSuite(const suiteName: ShortString): TTestResults;
var
  suiteIndex, i: integer;
  results: TTestResults;
  testCase: TTestCase;
begin
  suiteIndex := FindSuite(suiteName);
  if suiteIndex < 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  
  SetLength(results, 0);
  
  // Run global setup if provided (Python-style: setUpModule)
  if TestRegistry.GlobalSetupProc <> nil then
    TestRegistry.GlobalSetupProc;
  
  // Run suite setUpClass if provided (Python-style: runs once before all tests in suite)
  if TestRegistry.Suites[suiteIndex].SetupClassProc <> nil then
    TestRegistry.Suites[suiteIndex].SetupClassProc;
  
  // Run all tests in suite (each test gets setUp/tearDown)
  for i := 0 to TestRegistry.Suites[suiteIndex].Count - 1 do
  begin
    // Apply filters
    if (RunnerOptions.FilterTest <> '') and (TestRegistry.Suites[suiteIndex].Tests[i].Name <> RunnerOptions.FilterTest) then
      Continue;
    
    testCase := TestRegistry.Suites[suiteIndex].Tests[i];
    SetLength(results, Length(results) + 1);
    results[Length(results) - 1] := ExecuteTestCase(testCase, suiteIndex);
    
    // Stop on first failure if configured
    if RunnerOptions.StopOnFirstFailure and (results[Length(results) - 1].Status = tsFailed) then
      Break;
  end;
  
  // Run suite tearDownClass if provided (Python-style: runs once after all tests in suite)
  if TestRegistry.Suites[suiteIndex].TeardownClassProc <> nil then
    TestRegistry.Suites[suiteIndex].TeardownClassProc;
  
  // Run global teardown if provided (Python-style: tearDownModule)
  if TestRegistry.GlobalTeardownProc <> nil then
    TestRegistry.GlobalTeardownProc;
  
  Result := results;
end;

function RunAllTests: TTestResults;
var
  suiteIndex, i, j: integer;
  results: TTestResults;
  testCase: TTestCase;
begin
  SetLength(results, 0);
  
  // Run global setup if provided
  if TestRegistry.GlobalSetupProc <> nil then
    TestRegistry.GlobalSetupProc;
  
  // Run all suites
  for suiteIndex := 0 to TestRegistry.SuiteCount - 1 do
  begin
    // Apply suite filter
    if (RunnerOptions.FilterSuite <> '') and (TestRegistry.Suites[suiteIndex].Name <> RunnerOptions.FilterSuite) then
      Continue;
    
    // Run suite setUpClass if provided (Python-style: runs once before all tests in suite)
    if TestRegistry.Suites[suiteIndex].SetupClassProc <> nil then
      TestRegistry.Suites[suiteIndex].SetupClassProc;
    
    // Run all tests in suite (each test gets setUp/tearDown)
    for j := 0 to TestRegistry.Suites[suiteIndex].Count - 1 do
    begin
      // Apply test filter
      if (RunnerOptions.FilterTest <> '') and (TestRegistry.Suites[suiteIndex].Tests[j].Name <> RunnerOptions.FilterTest) then
        Continue;
      
      testCase := TestRegistry.Suites[suiteIndex].Tests[j];
      SetLength(results, Length(results) + 1);
      results[Length(results) - 1] := ExecuteTestCase(testCase, suiteIndex);
      
      // Stop on first failure if configured
      if RunnerOptions.StopOnFirstFailure and (results[Length(results) - 1].Status = tsFailed) then
      begin
        // Run teardowns before exiting
        if TestRegistry.Suites[suiteIndex].TeardownClassProc <> nil then
          TestRegistry.Suites[suiteIndex].TeardownClassProc;
        if TestRegistry.GlobalTeardownProc <> nil then
          TestRegistry.GlobalTeardownProc;
        Result := results;
        Exit;
      end;
    end;
    
    // Run suite tearDownClass if provided (Python-style: runs once after all tests in suite)
    if TestRegistry.Suites[suiteIndex].TeardownClassProc <> nil then
      TestRegistry.Suites[suiteIndex].TeardownClassProc;
  end;
  
  // Run global teardown if provided
  if TestRegistry.GlobalTeardownProc <> nil then
    TestRegistry.GlobalTeardownProc;
  
  Result := results;
end;

function RunTestsMatching(const pattern: ShortString): TTestResults;
var
  suiteIndex, i, j: integer;
  results: TTestResults;
  testCase: TTestCase;
  fullName: ShortString;
begin
  SetLength(results, 0);
  
  // Run global setup if provided
  if TestRegistry.GlobalSetupProc <> nil then
    TestRegistry.GlobalSetupProc;
  
  // Run all suites and tests, matching pattern
  for suiteIndex := 0 to TestRegistry.SuiteCount - 1 do
  begin
    // Run suite setUpClass if provided (Python-style: runs once before all tests in suite)
    if TestRegistry.Suites[suiteIndex].SetupClassProc <> nil then
      TestRegistry.Suites[suiteIndex].SetupClassProc;
    
    // Run all tests in suite (each test gets setUp/tearDown)
    for j := 0 to TestRegistry.Suites[suiteIndex].Count - 1 do
    begin
      // Simple substring matching
      fullName := TestRegistry.Suites[suiteIndex].Name + '.' + TestRegistry.Suites[suiteIndex].Tests[j].Name;
      // Note: Simple substring check - could be enhanced with proper pattern matching
      if (pattern = '') or (Pos(pattern, fullName) > 0) then
      begin
        testCase := TestRegistry.Suites[suiteIndex].Tests[j];
        SetLength(results, Length(results) + 1);
        results[Length(results) - 1] := ExecuteTestCase(testCase, suiteIndex);
        
        // Stop on first failure if configured
        if RunnerOptions.StopOnFirstFailure and (results[Length(results) - 1].Status = tsFailed) then
        begin
          if TestRegistry.Suites[suiteIndex].TeardownClassProc <> nil then
            TestRegistry.Suites[suiteIndex].TeardownClassProc;
          if TestRegistry.GlobalTeardownProc <> nil then
            TestRegistry.GlobalTeardownProc;
          Result := results;
          Exit;
        end;
      end;
    end;
    
    // Run suite tearDownClass if provided (Python-style: runs once after all tests in suite)
    if TestRegistry.Suites[suiteIndex].TeardownClassProc <> nil then
      TestRegistry.Suites[suiteIndex].TeardownClassProc;
  end;
  
  // Run global teardown if provided
  if TestRegistry.GlobalTeardownProc <> nil then
    TestRegistry.GlobalTeardownProc;
  
  Result := results;
end;

// ============================================================================
// Test Runner Configuration
// ============================================================================

procedure SetRunnerOptions(const options: TTestRunnerOptions);
begin
  RunnerOptions := options;
end;

function GetRunnerOptions: TTestRunnerOptions;
begin
  Result := RunnerOptions;
end;

// ============================================================================
// Fixture Management
// ============================================================================

procedure SetGlobalSetup(setupProc: TTestProcedure);
begin
  TestRegistry.GlobalSetupProc := setupProc;
end;

procedure SetGlobalTeardown(teardownProc: TTestProcedure);
begin
  TestRegistry.GlobalTeardownProc := teardownProc;
end;

// Python-style setUpClass: runs once before all tests in suite
procedure SetSuiteSetupClass(const suiteName: ShortString; setupProc: TTestProcedure);
var
  suiteIndex: integer;
begin
  suiteIndex := FindSuite(suiteName);
  if suiteIndex >= 0 then
    TestRegistry.Suites[suiteIndex].SetupClassProc := setupProc;
end;

// Python-style tearDownClass: runs once after all tests in suite
procedure SetSuiteTeardownClass(const suiteName: ShortString; teardownProc: TTestProcedure);
var
  suiteIndex: integer;
begin
  suiteIndex := FindSuite(suiteName);
  if suiteIndex >= 0 then
    TestRegistry.Suites[suiteIndex].TeardownClassProc := teardownProc;
end;

// Python-style setUp: runs before each test in suite
procedure SetSuiteBeforeEach(const suiteName: ShortString; setupProc: TTestProcedure);
var
  suiteIndex: integer;
begin
  suiteIndex := FindSuite(suiteName);
  if suiteIndex >= 0 then
    TestRegistry.Suites[suiteIndex].BeforeEachProc := setupProc;
end;

// Python-style tearDown: runs after each test in suite
procedure SetSuiteAfterEach(const suiteName: ShortString; teardownProc: TTestProcedure);
var
  suiteIndex: integer;
begin
  suiteIndex := FindSuite(suiteName);
  if suiteIndex >= 0 then
    TestRegistry.Suites[suiteIndex].AfterEachProc := teardownProc;
end;

// DEPRECATED: Use SetSuiteBeforeEach instead
procedure SetSuiteSetup(const suiteName: ShortString; setupProc: TTestProcedure);
begin
  SetSuiteBeforeEach(suiteName, setupProc);
end;

// DEPRECATED: Use SetSuiteAfterEach instead
procedure SetSuiteTeardown(const suiteName: ShortString; teardownProc: TTestProcedure);
begin
  SetSuiteAfterEach(suiteName, teardownProc);
end;

end.

