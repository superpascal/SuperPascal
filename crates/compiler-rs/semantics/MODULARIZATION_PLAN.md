# Semantics Module Modularization Plan

## Current State
- **File**: `semantics/src/lib.rs` (1441 lines)
- **Coverage**: 23.67% (very low)
- **Tests**: 9 tests (mostly constant folding)
- **Structure**: Single monolithic file with all semantic analysis logic

## Proposed Module Structure

```
semantics/src/
├── lib.rs              # Main entry point, SemanticAnalyzer struct
├── core.rs             # Core analyzer functionality (analyze_block, add_error, etc.)
├── declarations.rs     # Declaration analysis (const, type, var, proc, func)
├── statements.rs      # Statement analysis (if, while, for, repeat, case, assign, call)
├── expressions.rs      # Expression analysis (binary, unary, literals, identifiers, calls)
├── types.rs            # Type analysis (analyze_type, type checking)
├── constants.rs        # Constant folding/evaluation (evaluate_constant_expression, eval_*)
└── lvalues.rs          # Lvalue analysis (analyze_lvalue for assignments)
```

## Function Mapping

### core.rs
- `SemanticAnalyzer` struct definition
- `new()` constructor
- `analyze()` main entry point
- `analyze_block()` - orchestrates declaration and statement analysis
- `add_error()` - error reporting helper
- `format_type()` - type formatting helper

### declarations.rs
- `analyze_const_decl()`
- `analyze_type_decl()`
- `analyze_var_decl()`
- `analyze_proc_decl()`
- `analyze_func_decl()`
- `analyze_params()`

### statements.rs
- `analyze_statement()` - dispatcher
- `analyze_assignment()`
- `analyze_call_stmt()`
- `analyze_if_stmt()`
- `analyze_while_stmt()`
- `analyze_for_stmt()`
- `analyze_repeat_stmt()`
- `analyze_case_stmt()`

### expressions.rs
- `analyze_expression()` - main dispatcher
- Handles: LiteralExpr, IdentExpr, BinaryExpr, UnaryExpr, CallExpr, IndexExpr, FieldExpr, AddressOfExpr, InheritedExpr

### types.rs
- `analyze_type()` - type expression analysis
- Handles: NamedType, ArrayType, RecordType, and other type nodes

### constants.rs
- `evaluate_constant_expression()` - main constant evaluator
- `eval_add()`, `eval_subtract()`, `eval_multiply()`, `eval_divide()`, `eval_mod()`
- `eval_less()`, `eval_less_equal()`, `eval_greater()`, `eval_greater_equal()`
- `eval_and()`, `eval_or()`
- `eval_unary_minus()`, `eval_not()`

### lvalues.rs
- `analyze_lvalue()` - left-hand side of assignment analysis
- Handles: IdentExpr, IndexExpr, FieldExpr

## Implementation Strategy

1. **Create module stubs** - Create all module files with `pub(crate)` visibility
2. **Extract core** - Move SemanticAnalyzer struct and core functions
3. **Extract declarations** - Move all declaration analysis functions
4. **Extract statements** - Move all statement analysis functions
5. **Extract expressions** - Move expression analysis
6. **Extract types** - Move type analysis
7. **Extract constants** - Move constant evaluation
8. **Extract lvalues** - Move lvalue analysis
9. **Move tests** - Distribute tests to appropriate modules
10. **Verify** - Ensure all tests pass after each extraction

## Benefits

1. **Maintainability** - Smaller, focused modules
2. **Testability** - Easier to test individual components
3. **Coverage** - Can identify gaps per module
4. **Readability** - Clear separation of concerns
5. **Parallel Development** - Multiple developers can work on different modules

## Test Coverage Goals

After modularization, add comprehensive tests for each module:
- **declarations**: Test duplicate declarations, type resolution, parameter analysis
- **statements**: Test type checking for all statement types, error cases
- **expressions**: Test all expression types, type compatibility, error cases
- **types**: Test type resolution, built-in types, array/record types
- **constants**: Test all constant evaluation operations, edge cases
- **lvalues**: Test assignment target validation

Target: Increase coverage from 23.67% to 65%+ (minimum target)

