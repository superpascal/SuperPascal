# Python-Style Setup/Teardown Examples

**Status:** ✅ Complete  
**Framework:** SuperPascal Unit Testing Framework

---

## Overview

The SuperPascal testing framework now supports Python-style setup/teardown functionality, matching the behavior of Python's `unittest` module:

- **`setUp()`** - Runs before each test method
- **`tearDown()`** - Runs after each test method
- **`setUpClass()`** - Runs once before all tests in a suite
- **`tearDownClass()`** - Runs once after all tests in a suite
- **`setUpModule()`** - Runs once before all tests (global)
- **`tearDownModule()`** - Runs once after all tests (global)

---

## Execution Order

The framework executes fixtures in this order (matching Python unittest):

1. **Global Setup** (`setUpModule`) - Once, before all tests
2. **Suite Setup Class** (`setUpClass`) - Once, before all tests in suite
3. **For each test:**
   - **Test Setup** (`setUp`) - Before each test
   - **Test Execution** - The actual test
   - **Test Teardown** (`tearDown`) - After each test
4. **Suite Teardown Class** (`tearDownClass`) - Once, after all tests in suite
5. **Global Teardown** (`tearDownModule`) - Once, after all tests

---

## Example 1: Basic setUp/tearDown (Per Test)

```pascal
program MathTests;

uses Testing;

var
  TestData: integer;

// Python-style setUp: runs before each test
procedure SetUp;
begin
  TestData := 10;
  WriteLn('setUp: TestData = ', TestData);
end;

// Python-style tearDown: runs after each test
procedure TearDown;
begin
  TestData := 0;
  WriteLn('tearDown: TestData = ', TestData);
end;

procedure TestAddition;
begin
  AssertEqual(TestData + 5, 15, 'TestData should be 10');
end;

procedure TestSubtraction;
begin
  AssertEqual(TestData - 3, 7, 'TestData should be 10');
end;

begin
  // Register suite with setUp/tearDown
  SetSuiteBeforeEach('Math', @SetUp);
  SetSuiteAfterEach('Math', @TearDown);
  
  // Register tests
  RegisterTest('Math', 'Addition', @TestAddition);
  RegisterTest('Math', 'Subtraction', @TestSubtraction);
  
  // Run tests
  RunAllTests;
end.
```

**Output:**
```
Running tests...
========================================
Suite: Math
setUp: TestData = 10
  Test: Math.Addition ................. PASSED
tearDown: TestData = 0
setUp: TestData = 10
  Test: Math.Subtraction .............. PASSED
tearDown: TestData = 0
========================================
```

---

## Example 2: setUpClass/tearDownClass (Per Suite)

```pascal
program DatabaseTests;

uses Testing;

var
  DatabaseConnection: integer; // Simulated database connection
  ConnectionCount: integer;

// Python-style setUpClass: runs once before all tests in suite
procedure SetUpClass;
begin
  DatabaseConnection := 1; // Open database connection
  ConnectionCount := 1;
  WriteLn('setUpClass: Database connection opened');
end;

// Python-style tearDownClass: runs once after all tests in suite
procedure TearDownClass;
begin
  DatabaseConnection := 0; // Close database connection
  WriteLn('tearDownClass: Database connection closed');
end;

// Python-style setUp: runs before each test
procedure SetUp;
begin
  WriteLn('setUp: Using database connection ', DatabaseConnection);
end;

// Python-style tearDown: runs after each test
procedure TearDown;
begin
  WriteLn('tearDown: Test completed');
end;

procedure TestQuery1;
begin
  AssertEqual(DatabaseConnection, 1, 'Database should be connected');
end;

procedure TestQuery2;
begin
  AssertEqual(DatabaseConnection, 1, 'Database should be connected');
end;

begin
  // Register suite with setUpClass/tearDownClass
  SetSuiteSetupClass('Database', @SetUpClass);
  SetSuiteTeardownClass('Database', @TearDownClass);
  
  // Register suite with setUp/tearDown (per test)
  SetSuiteBeforeEach('Database', @SetUp);
  SetSuiteAfterEach('Database', @TearDown);
  
  // Register tests
  RegisterTest('Database', 'Query1', @TestQuery1);
  RegisterTest('Database', 'Query2', @TestQuery2);
  
  // Run tests
  RunAllTests;
end.
```

**Output:**
```
Running tests...
========================================
Suite: Database
setUpClass: Database connection opened
setUp: Using database connection 1
  Test: Database.Query1 ............... PASSED
tearDown: Test completed
setUp: Using database connection 1
  Test: Database.Query2 ............... PASSED
tearDown: Test completed
tearDownClass: Database connection closed
========================================
```

---

## Example 3: Full Python-Style (setUpModule, setUpClass, setUp)

```pascal
program FullExample;

uses Testing;

var
  GlobalCounter: integer;
  SuiteCounter: integer;
  TestCounter: integer;

// Python-style setUpModule: runs once before all tests
procedure SetUpModule;
begin
  GlobalCounter := 100;
  WriteLn('setUpModule: GlobalCounter = ', GlobalCounter);
end;

// Python-style tearDownModule: runs once after all tests
procedure TearDownModule;
begin
  WriteLn('tearDownModule: All tests completed');
end;

// Python-style setUpClass: runs once before all tests in suite
procedure SetUpClass;
begin
  SuiteCounter := 10;
  WriteLn('setUpClass: SuiteCounter = ', SuiteCounter);
end;

// Python-style tearDownClass: runs once after all tests in suite
procedure TearDownClass;
begin
  WriteLn('tearDownClass: Suite completed');
end;

// Python-style setUp: runs before each test
procedure SetUp;
begin
  TestCounter := 1;
  WriteLn('setUp: TestCounter = ', TestCounter);
end;

// Python-style tearDown: runs after each test
procedure TearDown;
begin
  WriteLn('tearDown: Test finished');
end;

procedure Test1;
begin
  AssertEqual(GlobalCounter, 100, 'GlobalCounter should be 100');
  AssertEqual(SuiteCounter, 10, 'SuiteCounter should be 10');
  AssertEqual(TestCounter, 1, 'TestCounter should be 1');
end;

procedure Test2;
begin
  AssertEqual(GlobalCounter, 100, 'GlobalCounter should be 100');
  AssertEqual(SuiteCounter, 10, 'SuiteCounter should be 10');
  AssertEqual(TestCounter, 1, 'TestCounter should be 1');
end;

begin
  // Global fixtures
  SetGlobalSetup(@SetUpModule);
  SetGlobalTeardown(@TearDownModule);
  
  // Suite class fixtures
  SetSuiteSetupClass('Example', @SetUpClass);
  SetSuiteTeardownClass('Example', @TearDownClass);
  
  // Suite per-test fixtures
  SetSuiteBeforeEach('Example', @SetUp);
  SetSuiteAfterEach('Example', @TearDown);
  
  // Register tests
  RegisterTest('Example', 'Test1', @Test1);
  RegisterTest('Example', 'Test2', @Test2);
  
  // Run tests
  RunAllTests;
end.
```

**Output:**
```
Running tests...
========================================
setUpModule: GlobalCounter = 100
Suite: Example
setUpClass: SuiteCounter = 10
setUp: TestCounter = 1
  Test: Example.Test1 ................. PASSED
tearDown: Test finished
setUp: TestCounter = 1
  Test: Example.Test2 ................. PASSED
tearDown: Test finished
tearDownClass: Suite completed
tearDownModule: All tests completed
========================================
```

---

## API Reference

### Python-Style Functions

```pascal
// Per-test fixtures (runs before/after each test)
procedure SetSuiteBeforeEach(const suiteName: ShortString; setupProc: TTestProcedure);
procedure SetSuiteAfterEach(const suiteName: ShortString; teardownProc: TTestProcedure);

// Per-suite fixtures (runs once before/after all tests in suite)
procedure SetSuiteSetupClass(const suiteName: ShortString; setupProc: TTestProcedure);
procedure SetSuiteTeardownClass(const suiteName: ShortString; teardownProc: TTestProcedure);

// Global fixtures (runs once before/after all tests)
procedure SetGlobalSetup(setupProc: TTestProcedure);
procedure SetGlobalTeardown(teardownProc: TTestProcedure);
```

### Convenience Functions (from Testing_Fixtures)

```pascal
// Alias for SetSuiteBeforeEach
procedure SetBeforeEach(const suiteName: ShortString; proc: TTestProcedure);

// Alias for SetSuiteAfterEach
procedure SetAfterEach(const suiteName: ShortString; proc: TTestProcedure);
```

---

## Comparison with Python unittest

| Python unittest | SuperPascal Testing |
|----------------|---------------------|
| `setUp(self)` | `SetSuiteBeforeEach('SuiteName', @SetUp)` |
| `tearDown(self)` | `SetSuiteAfterEach('SuiteName', @TearDown)` |
| `setUpClass(cls)` | `SetSuiteSetupClass('SuiteName', @SetUpClass)` |
| `tearDownClass(cls)` | `SetSuiteTeardownClass('SuiteName', @TearDownClass)` |
| `setUpModule()` | `SetGlobalSetup(@SetUpModule)` |
| `tearDownModule()` | `SetGlobalTeardown(@TearDownModule)` |

---

## Best Practices

1. **Use setUp/tearDown for per-test initialization** (e.g., resetting test data)
2. **Use setUpClass/tearDownClass for expensive operations** (e.g., opening database connections)
3. **Use setUpModule/tearDownModule for global setup** (e.g., loading configuration)
4. **Keep fixtures simple** - Complex logic should be in helper functions
5. **Clean up in tearDown** - Always restore state in tearDown procedures

---

**Last Updated:** 2025-01-XX  
**Status:** ✅ Complete - Python-style setup/teardown fully implemented

