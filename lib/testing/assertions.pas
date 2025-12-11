unit Testing_Assertions;

interface

// Assertion functions for unit testing
// Source: TESTING_FRAMEWORK_DESIGN.md
//
// All assertions raise an exception (or set a flag) on failure
// This allows the test runner to catch failures and report them

uses
  Testing_Types,
  Math_Types,
  Math_Fixed;

// ============================================================================
// Global Test Failure State
// ============================================================================
// Note: Using global state instead of exceptions for retro platform compatibility

var
  TestFailureOccurred: boolean;
  TestFailureMessage: ShortString;

// ============================================================================
// Boolean Assertions
// ============================================================================

// Assert that condition is true
procedure AssertTrue(condition: boolean; const message: ShortString = '');

// Assert that condition is false
procedure AssertFalse(condition: boolean; const message: ShortString = '');

// ============================================================================
// Equality Assertions
// ============================================================================

// Assert that two integers are equal
procedure AssertEqual(expected, actual: integer; const message: ShortString = '');

// Assert that two integers are not equal
procedure AssertNotEqual(expected, actual: integer; const message: ShortString = '');

// Assert that two strings are equal
procedure AssertStringEqual(const expected, actual: ShortString; const message: ShortString = '');

// Assert that two Fixed16 values are equal (with tolerance)
procedure AssertEqual(expected, actual: Fixed16; const message: ShortString = '');
procedure AssertEqual(expected, actual: Fixed16; tolerance: Fixed16; const message: ShortString = '');

// ============================================================================
// Comparison Assertions
// ============================================================================

// Assert that value is greater than threshold
procedure AssertGreaterThan(value, threshold: integer; const message: ShortString = '');

// Assert that value is less than threshold
procedure AssertLessThan(value, threshold: integer; const message: ShortString = '');

// Assert that value is greater than or equal to threshold
procedure AssertGreaterThanOrEqual(value, threshold: integer; const message: ShortString = '');

// Assert that value is less than or equal to threshold
procedure AssertLessThanOrEqual(value, threshold: integer; const message: ShortString = '');

// ============================================================================
// Nil Assertions
// ============================================================================

// Assert that pointer is nil
procedure AssertNil(ptr: Pointer; const message: ShortString = '');

// Assert that pointer is not nil
procedure AssertNotNil(ptr: Pointer; const message: ShortString = '');

// ============================================================================
// String Assertions
// ============================================================================

// Assert that string contains substring
procedure AssertStringContains(const haystack, needle: ShortString; const message: ShortString = '');

// ============================================================================
// Array Assertions
// ============================================================================

// Assert that two integer arrays are equal
procedure AssertArrayEqual(
  const expected, actual: array of integer;
  const message: ShortString = ''
);

// ============================================================================
// Internal Functions (used by runner)
// ============================================================================

// Clear test failure state (called by runner before each test)
procedure ClearTestFailure;

// Get test failure state (called by runner after each test)
function GetTestFailure(var msg: ShortString): boolean;

implementation

// ============================================================================
// Boolean Assertions
// ============================================================================

// Clear test failure state (called by runner before each test)
procedure ClearTestFailure;
begin
  TestFailureOccurred := False;
  TestFailureMessage := '';
end;

// Get test failure state (called by runner after each test)
function GetTestFailure(var msg: ShortString): boolean;
begin
  Result := TestFailureOccurred;
  if Result then
    msg := TestFailureMessage;
end;

// Set test failure (internal helper)
procedure SetTestFailure(const msg: ShortString);
begin
  TestFailureOccurred := True;
  TestFailureMessage := msg;
end;

procedure AssertTrue(condition: boolean; const message: ShortString);
begin
  if not condition then
  begin
    if message <> '' then
      SetTestFailure('AssertTrue failed: ' + message)
    else
      SetTestFailure('AssertTrue failed');
  end;
end;

procedure AssertFalse(condition: boolean; const message: ShortString);
begin
  if condition then
  begin
    if message <> '' then
      SetTestFailure('AssertFalse failed: ' + message)
    else
      SetTestFailure('AssertFalse failed');
  end;
end;

// ============================================================================
// Equality Assertions
// ============================================================================

procedure AssertEqual(expected, actual: integer; const message: ShortString);
var
  msg: ShortString;
begin
  if expected <> actual then
  begin
    if message <> '' then
      msg := message + ' (Expected: ' + IntToStr(expected) + ', Actual: ' + IntToStr(actual) + ')'
    else
      msg := 'AssertEqual failed: Expected ' + IntToStr(expected) + ', got ' + IntToStr(actual);
    SetTestFailure(msg);
  end;
end;

procedure AssertNotEqual(expected, actual: integer; const message: ShortString);
begin
  if expected = actual then
  begin
    if message <> '' then
      SetTestFailure('AssertNotEqual failed: ' + message + ' (Values are equal: ' + IntToStr(expected) + ')')
    else
      SetTestFailure('AssertNotEqual failed: Values are equal (' + IntToStr(expected) + ')');
  end;
end;

procedure AssertStringEqual(const expected, actual: ShortString; const message: ShortString);
var
  msg: ShortString;
begin
  if expected <> actual then
  begin
    if message <> '' then
      msg := message + ' (Expected: "' + expected + '", Actual: "' + actual + '")'
    else
      msg := 'AssertStringEqual failed: Expected "' + expected + '", got "' + actual + '"';
    SetTestFailure(msg);
  end;
end;

procedure AssertEqual(expected, actual: Fixed16; const message: ShortString);
begin
  AssertEqual(expected, actual, 0, message);
end;

procedure AssertEqual(expected, actual: Fixed16; tolerance: Fixed16; const message: ShortString);
var
  diff: Fixed16;
  msg: ShortString;
begin
  diff := Fixed16Abs(Fixed16Sub(expected, actual));
  if diff > tolerance then
  begin
    if message <> '' then
      msg := message + ' (Expected: ' + IntToStr(Fixed16ToInt(expected)) + ', Actual: ' + IntToStr(Fixed16ToInt(actual)) + ')'
    else
      msg := 'AssertEqual failed: Expected ' + IntToStr(Fixed16ToInt(expected)) + ', got ' + IntToStr(Fixed16ToInt(actual));
    SetTestFailure(msg);
  end;
end;

// ============================================================================
// Comparison Assertions
// ============================================================================

procedure AssertGreaterThan(value, threshold: integer; const message: ShortString);
begin
  if value <= threshold then
  begin
    if message <> '' then
      SetTestFailure('AssertGreaterThan failed: ' + message + ' (' + IntToStr(value) + ' <= ' + IntToStr(threshold) + ')')
    else
      SetTestFailure('AssertGreaterThan failed: ' + IntToStr(value) + ' <= ' + IntToStr(threshold));
  end;
end;

procedure AssertLessThan(value, threshold: integer; const message: ShortString);
begin
  if value >= threshold then
  begin
    if message <> '' then
      SetTestFailure('AssertLessThan failed: ' + message + ' (' + IntToStr(value) + ' >= ' + IntToStr(threshold) + ')')
    else
      SetTestFailure('AssertLessThan failed: ' + IntToStr(value) + ' >= ' + IntToStr(threshold));
  end;
end;

procedure AssertGreaterThanOrEqual(value, threshold: integer; const message: ShortString);
begin
  if value < threshold then
  begin
    if message <> '' then
      SetTestFailure('AssertGreaterThanOrEqual failed: ' + message + ' (' + IntToStr(value) + ' < ' + IntToStr(threshold) + ')')
    else
      SetTestFailure('AssertGreaterThanOrEqual failed: ' + IntToStr(value) + ' < ' + IntToStr(threshold));
  end;
end;

procedure AssertLessThanOrEqual(value, threshold: integer; const message: ShortString);
begin
  if value > threshold then
  begin
    if message <> '' then
      SetTestFailure('AssertLessThanOrEqual failed: ' + message + ' (' + IntToStr(value) + ' > ' + IntToStr(threshold) + ')')
    else
      SetTestFailure('AssertLessThanOrEqual failed: ' + IntToStr(value) + ' > ' + IntToStr(threshold));
  end;
end;

// ============================================================================
// Nil Assertions
// ============================================================================

procedure AssertNil(ptr: Pointer; const message: ShortString);
begin
  if ptr <> nil then
  begin
    if message <> '' then
      SetTestFailure('AssertNil failed: ' + message)
    else
      SetTestFailure('AssertNil failed: Pointer is not nil');
  end;
end;

procedure AssertNotNil(ptr: Pointer; const message: ShortString);
begin
  if ptr = nil then
  begin
    if message <> '' then
      SetTestFailure('AssertNotNil failed: ' + message)
    else
      SetTestFailure('AssertNotNil failed: Pointer is nil');
  end;
end;

// ============================================================================
// String Assertions
// ============================================================================

procedure AssertStringContains(const haystack, needle: ShortString; const message: ShortString);
var
  i: integer;
  found: boolean;
begin
  found := False;
  for i := 1 to Length(haystack) - Length(needle) + 1 do
  begin
    if Copy(haystack, i, Length(needle)) = needle then
    begin
      found := True;
      Break;
    end;
  end;
  
  if not found then
  begin
    if message <> '' then
      SetTestFailure('AssertStringContains failed: ' + message + ' (String "' + haystack + '" does not contain "' + needle + '")')
    else
      SetTestFailure('AssertStringContains failed: String "' + haystack + '" does not contain "' + needle + '"');
  end;
end;

// ============================================================================
// Array Assertions
// ============================================================================

procedure AssertArrayEqual(
  const expected, actual: array of integer;
  const message: ShortString
);
var
  i: integer;
  msg: ShortString;
begin
  if Length(expected) <> Length(actual) then
  begin
    if message <> '' then
      msg := message + ' (Array lengths differ: Expected ' + IntToStr(Length(expected)) + ', got ' + IntToStr(Length(actual)) + ')'
    else
      msg := 'AssertArrayEqual failed: Array lengths differ (Expected ' + IntToStr(Length(expected)) + ', got ' + IntToStr(Length(actual)) + ')';
    SetTestFailure(msg);
  end;
  
  for i := 0 to Length(expected) - 1 do
  begin
    if expected[i] <> actual[i] then
    begin
      if message <> '' then
        msg := message + ' (Arrays differ at index ' + IntToStr(i) + ': Expected ' + IntToStr(expected[i]) + ', got ' + IntToStr(actual[i]) + ')'
      else
        msg := 'AssertArrayEqual failed: Arrays differ at index ' + IntToStr(i) + ' (Expected ' + IntToStr(expected[i]) + ', got ' + IntToStr(actual[i]) + ')';
      SetTestFailure(msg);
    end;
  end;
end;

begin
  TestFailureOccurred := False;
  TestFailureMessage := '';
end.

