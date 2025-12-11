unit Testing_Reporting;

interface

// Test result reporting
// Source: TESTING_FRAMEWORK_DESIGN.md

uses
  Testing_Types;

// ============================================================================
// Console Output
// ============================================================================

// Print test results to console
procedure PrintTestResults(const results: TTestResults);

// Print summary statistics
procedure PrintTestSummary(const results: TTestResults);

// Print detailed failure information
procedure PrintFailureDetails(const result: TTestResult);

// Print single test result (with formatting)
procedure PrintTestResult(const result: TTestResult; verbose: boolean);

// ============================================================================
// Statistics
// ============================================================================

// Calculate test statistics
procedure CalculateTestStats(
  const results: TTestResults;
  var total, passed, failed, skipped, errors: integer;
  var totalDuration: integer
);

implementation

// ============================================================================
// Helper Functions
// ============================================================================

// Convert status to string
function StatusToString(status: TTestStatus): ShortString;
begin
  case status of
    tsPassed: Result := 'PASSED';
    tsFailed: Result := 'FAILED';
    tsSkipped: Result := 'SKIPPED';
    tsError: Result := 'ERROR';
  end;
end;

// Format duration string
function FormatDuration(duration: integer): ShortString;
begin
  if duration > 0 then
    Result := IntToStr(duration) + 'ms'
  else
    Result := 'N/A';
end;

// Print separator line
procedure PrintSeparator;
begin
  WriteLn('========================================');
end;

// ============================================================================
// Statistics
// ============================================================================

procedure CalculateTestStats(
  const results: TTestResults;
  var total, passed, failed, skipped, errors: integer;
  var totalDuration: integer
);
var
  i: integer;
begin
  total := Length(results);
  passed := 0;
  failed := 0;
  skipped := 0;
  errors := 0;
  totalDuration := 0;
  
  for i := 0 to Length(results) - 1 do
  begin
    case results[i].Status of
      tsPassed: passed := passed + 1;
      tsFailed: failed := failed + 1;
      tsSkipped: skipped := skipped + 1;
      tsError: errors := errors + 1;
    end;
    totalDuration := totalDuration + results[i].Duration;
  end;
end;

// ============================================================================
// Console Output
// ============================================================================

procedure PrintTestResult(const result: TTestResult; verbose: boolean);
var
  statusStr: ShortString;
  dots: ShortString;
  i: integer;
begin
  statusStr := StatusToString(result.Status);
  
  // Calculate dots for alignment (simple padding)
  dots := '';
  i := Length(result.SuiteName) + Length(result.TestName) + 2;
  while i < 40 do
  begin
    dots := dots + '.';
    i := i + 1;
  end;
  
  // Print test result
  Write('  Test: ', result.SuiteName, '.', result.TestName, ' ', dots, ' ', statusStr);
  
  if verbose and (result.Duration > 0) then
    Write(' (', FormatDuration(result.Duration), ')');
  
  WriteLn;
  
  // Print failure message if failed
  if (result.Status = tsFailed) or (result.Status = tsError) then
  begin
    if result.Message <> '' then
      WriteLn('    -> ', result.Message);
  end;
end;

procedure PrintTestResults(const results: TTestResults);
var
  i: integer;
  currentSuite: ShortString;
begin
  WriteLn('Running tests...');
  PrintSeparator;
  
  currentSuite := '';
  for i := 0 to Length(results) - 1 do
  begin
    // Print suite header if new suite
    if results[i].SuiteName <> currentSuite then
    begin
      if currentSuite <> '' then
        WriteLn;
      WriteLn('Suite: ', results[i].SuiteName);
      currentSuite := results[i].SuiteName;
    end;
    
    // Print test result
    PrintTestResult(results[i], False);
  end;
  
  WriteLn;
  PrintSeparator;
end;

procedure PrintTestSummary(const results: TTestResults);
var
  total, passed, failed, skipped, errors: integer;
  totalDuration: integer;
begin
  CalculateTestStats(results, total, passed, failed, skipped, errors, totalDuration);
  
  WriteLn('Summary:');
  WriteLn('  Total: ', total);
  WriteLn('  Passed: ', passed);
  WriteLn('  Failed: ', failed);
  WriteLn('  Skipped: ', skipped);
  WriteLn('  Errors: ', errors);
  if totalDuration > 0 then
    WriteLn('  Duration: ', FormatDuration(totalDuration));
  PrintSeparator;
end;

procedure PrintFailureDetails(const result: TTestResult);
begin
  WriteLn('========================================');
  WriteLn('Test Failure Details:');
  WriteLn('  Suite: ', result.SuiteName);
  WriteLn('  Test: ', result.TestName);
  WriteLn('  Status: ', StatusToString(result.Status));
  if result.Message <> '' then
    WriteLn('  Message: ', result.Message);
  if result.Duration > 0 then
    WriteLn('  Duration: ', FormatDuration(result.Duration));
  WriteLn('========================================');
end;

end.

