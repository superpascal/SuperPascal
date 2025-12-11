# SuperPascal Unit Testing Framework

**Status:** ✅ Complete  
**Priority:** ⭐⭐⭐⭐⭐ CRITICAL  
**Target:** All platforms (8-bit through 64-bit)

---

## Overview

A comprehensive unit testing framework for SuperPascal, designed from modern testing principles but adapted for retro computing platforms. While unit testing wasn't common in the retro computing era, we can leverage all modern knowledge to build an excellent testing framework.

**Key Features:**
- **Platform-Agnostic:** Works on all platforms without modification
- **No Dependencies:** Standalone framework (no external dependencies)
- **Simple API:** Easy to use, follows modern testing patterns
- **Rich Assertions:** Comprehensive assertion library
- **Clear Reporting:** Human-readable test results
- **Memory Efficient:** Suitable for memory-constrained platforms

---

## Module Structure

```
lib/testing/
├── mod.pas          # Main entry point
├── types.pas        # Core types (TestCase, TestSuite, TestResult)
├── assertions.pas   # Assert functions
├── runner.pas       # Test runner and execution
├── fixtures.pas     # Setup/teardown support
├── reporting.pas    # Test result reporting
├── TESTING_FRAMEWORK_DESIGN.md  # Design document
└── README.md        # This file
```

---

## Quick Start

### Basic Test

```pascal
program MyTests;

uses Testing;

procedure TestAddition;
begin
  AssertEqual(2 + 2, 4, 'Basic addition');
  AssertEqual(10 + 20, 30, 'Larger numbers');
end;

begin
  RegisterTest('Math', 'Addition', @TestAddition);
  RunAllTests;
end.
```

### Test with Setup/Teardown

```pascal
program MyTests;

uses Testing;

var
  TestData: integer;

procedure SetupMathTest;
begin
  TestData := 10;
end;

procedure TeardownMathTest;
begin
  TestData := 0;
end;

procedure TestWithData;
begin
  AssertEqual(TestData * 2, 20, 'TestData should be 10');
end;

begin
  RegisterTestWithFixtures(
    'Math',
    'TestWithData',
    @TestWithData,
    @SetupMathTest,
    @TeardownMathTest
  );
  RunAllTests;
end.
```

### Test Suite

```pascal
program MyTests;

uses Testing;

procedure TestAdd;
begin
  AssertEqual(1 + 1, 2);
end;

procedure TestSubtract;
begin
  AssertEqual(5 - 3, 2);
end;

procedure TestMultiply;
begin
  AssertEqual(3 * 4, 12);
end;

var
  suite: TTestSuite;
  tests: array[0..2] of TTestCase;
begin
  tests[0] := CreateTestCase('Math', 'Add', @TestAdd);
  tests[1] := CreateTestCase('Math', 'Subtract', @TestSubtract);
  tests[2] := CreateTestCase('Math', 'Multiply', @TestMultiply);
  
  RegisterSuite('Math', tests);
  RunSuite('Math');
end.
```

---

## Assertions

### Boolean Assertions

```pascal
AssertTrue(condition, 'Optional message');
AssertFalse(condition, 'Optional message');
```

### Equality Assertions

```pascal
AssertEqual(expected, actual: integer, 'Optional message');
AssertNotEqual(expected, actual: integer, 'Optional message');
AssertStringEqual(expected, actual: ShortString, 'Optional message');
AssertEqual(expected, actual: Fixed16, 'Optional message');
AssertEqual(expected, actual: Fixed16, tolerance: Fixed16, 'Optional message');
```

### Comparison Assertions

```pascal
AssertGreaterThan(value, threshold: integer, 'Optional message');
AssertLessThan(value, threshold: integer, 'Optional message');
AssertGreaterThanOrEqual(value, threshold: integer, 'Optional message');
AssertLessThanOrEqual(value, threshold: integer, 'Optional message');
```

### Nil Assertions

```pascal
AssertNil(ptr: Pointer, 'Optional message');
AssertNotNil(ptr: Pointer, 'Optional message');
```

### String Assertions

```pascal
AssertStringContains(haystack, needle: ShortString, 'Optional message');
```

### Array Assertions

```pascal
AssertArrayEqual(expected, actual: array of integer, 'Optional message');
```

---

## Test Registration

### Register Single Test

```pascal
RegisterTest('SuiteName', 'TestName', @TestProcedure);
```

### Register Test with Fixtures

```pascal
RegisterTestWithFixtures(
  'SuiteName',
  'TestName',
  @TestProcedure,
  @SetupProcedure,      // Optional
  @TeardownProcedure    // Optional
);
```

### Register Test Suite

```pascal
var
  tests: array of TTestCase;
begin
  // ... populate tests array ...
  RegisterSuite('SuiteName', tests);
end;
```

---

## Test Execution

### Run All Tests

```pascal
var
  results: TTestResults;
begin
  results := RunAllTests;
  PrintTestResults(results);
  PrintTestSummary(results);
end;
```

### Run Specific Suite

```pascal
var
  results: TTestResults;
begin
  results := RunSuite('Math');
  PrintTestResults(results);
end;
```

### Run Single Test

```pascal
var
  result: TTestResult;
begin
  result := RunTest('Math', 'Addition');
  PrintTestResult(result, True);
end;
```

### Run Tests Matching Pattern

```pascal
var
  results: TTestResults;
begin
  results := RunTestsMatching('Math');
  PrintTestResults(results);
end;
```

---

## Test Runner Options

### Configure Runner

```pascal
var
  options: TTestRunnerOptions;
begin
  options.StopOnFirstFailure := False;
  options.Verbose := True;
  options.ShowTiming := True;
  options.FilterSuite := '';  // Empty = all suites
  options.FilterTest := '';   // Empty = all tests
  SetRunnerOptions(options);
end;
```

---

## Fixtures

### Global Fixtures

```pascal
procedure GlobalSetup;
begin
  // Run before all tests
end;

procedure GlobalTeardown;
begin
  // Run after all tests
end;

begin
  SetGlobalSetup(@GlobalSetup);
  SetGlobalTeardown(@GlobalTeardown);
end;
```

### Suite-Level Fixtures

```pascal
procedure SuiteSetup;
begin
  // Run before all tests in suite
end;

procedure SuiteTeardown;
begin
  // Run after all tests in suite
end;

begin
  SetSuiteSetup('Math', @SuiteSetup);
  SetSuiteTeardown('Math', @SuiteTeardown);
end;
```

### Test-Level Fixtures

```pascal
// Use RegisterTestWithFixtures for test-level setup/teardown
RegisterTestWithFixtures(
  'Math',
  'TestName',
  @TestProcedure,
  @SetupProcedure,
  @TeardownProcedure
);
```

---

## Reporting

### Print Test Results

```pascal
var
  results: TTestResults;
begin
  results := RunAllTests;
  PrintTestResults(results);      // Detailed output
  PrintTestSummary(results);      // Summary statistics
end;
```

### Print Failure Details

```pascal
var
  results: TTestResults;
  i: integer;
begin
  results := RunAllTests;
  for i := 0 to Length(results) - 1 do
  begin
    if results[i].Status = tsFailed then
      PrintFailureDetails(results[i]);
  end;
end;
```

### Report Format

```
Running tests...
========================================
Suite: Math
  Test: Math.Addition ................. PASSED
  Test: Math.Subtraction .............. PASSED
  Test: Math.Multiplication ........... FAILED
    -> AssertEqual failed: Expected 6, got 5
Suite: Graphics
  Test: Graphics.LineDrawing .......... PASSED
  Test: Graphics.CircleDrawing ........ PASSED

========================================
Summary:
  Total: 5
  Passed: 4
  Failed: 1
  Skipped: 0
  Errors: 0
  Duration: 42ms
========================================
```

---

## Platform Considerations

### Memory Constraints

**8-bit Platforms (ZealZ80, CommanderX16):**
- Uses `ShortString` for maximum compatibility
- Test results stored in dynamic arrays (may be limited)
- Simple string handling

**16-bit+ Platforms:**
- Full dynamic array support
- More detailed reporting
- Timing information (if available)

### String Handling

- Uses `ShortString` (max 255 characters) for compatibility
- All test names and messages are `ShortString`

### Timing

- Timing is optional (depends on platform capabilities)
- Duration is 0 if timing is not available

---

## Design Decisions

### 1. Error Handling

**Decision:** Use global error flag instead of exceptions
- **Pros:** Compatible with all platforms (including retro)
- **Cons:** Less elegant than exceptions

**Implementation:** Assertions set a global failure flag, runner checks it

### 2. String Types

**Decision:** Use `ShortString` for maximum compatibility
- **Pros:** Works on all platforms
- **Cons:** Limited length (255 characters)

### 3. Test Result Storage

**Decision:** Dynamic arrays for test results
- **Pros:** Flexible, can store all results
- **Cons:** May use more memory on 8-bit platforms

**Alternative:** Static arrays with fixed size (future optimization)

---

## Future Enhancements

1. **Test Discovery:** Automatic test discovery via naming convention
2. **Mocking:** Basic mocking framework for dependencies
3. **Parametrized Tests:** Run same test with different parameters
4. **Test Coverage:** Basic code coverage tracking
5. **Parallel Execution:** Run tests in parallel (for multi-core platforms)
6. **XML/JSON Output:** Export test results in structured formats
7. **Continuous Integration:** CI-friendly output formats

---

## Examples

See `TESTING_FRAMEWORK_DESIGN.md` for more detailed examples and usage patterns.

---

**Last Updated:** 2025-01-XX  
**Status:** ✅ Complete - Ready for use

