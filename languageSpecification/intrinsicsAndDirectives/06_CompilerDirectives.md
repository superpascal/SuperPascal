# Compiler Directives

**Part of:** [06_IntrinsicsAndDirectives.md](../06_IntrinsicsAndDirectives.md)

---

### 6.1 Directive Syntax

**Format:**
```
{$DIRECTIVE_NAME}
{$DIRECTIVE_NAME argument}
{$DIRECTIVE_NAME(argument1, argument2)}
```

**Placement:**
- Before declarations
- Before statements (some directives)
- Inline with code (some directives)

### 6.2 Optimization Directives

#### {$INLINE}

**Syntax:**
```pascal
{$INLINE}
procedure FastAdd(a, b: integer): integer;
```

**Purpose**: Inline function/procedure at call sites.

**Scope**: Applies to next procedure/function.

**Usage**: For small, frequently-called routines.

#### {$UNROLL}

**Syntax:**
```pascal
{$UNROLL}
for i := 0 to 3 do
  array[i] := 0;
```

**Purpose**: Unroll loop (expand loop body).

**Scope**: Applies to next loop statement.

**Limitations**: Only for small, constant-bound loops.

#### {$VBLANK_AWARE}

**Syntax:**
```pascal
{$VBLANK_AWARE}
procedure UpdateGame;
```

**Purpose**: Optimize for VBlank timing constraints.

**Scope**: Applies to next procedure/function.

**Effect**: Compiler may reorder operations for VBlank efficiency.

### 6.3 Checking Directives

#### {$RANGE_CHECK}

**Syntax:**
```pascal
{$RANGE_CHECK ON}
{$RANGE_CHECK OFF}
```

**Purpose**: Enable/disable array index and subrange bounds checking.

**Default**: `ON` in debug mode, `OFF` in release mode.

**Important**: In debug mode (`{$DEBUG}`), bounds checking is **always enabled** and cannot be disabled. The `{$RANGE_CHECK OFF}` directive is ignored in debug mode. This ensures students learn with safety checks during development.

**Usage:**
```pascal
{$RANGE_CHECK OFF}  // Only effective in release mode
for i := 0 to High(array) do
  array[i] := 0;  // No bounds check (release mode only)
{$RANGE_CHECK ON}
```

**Educational Value:**
- Teaches students about buffer overflows and bounds validation
- Provides clear error messages during development
- Can be disabled in release mode for performance after thorough testing

#### {$OVERFLOW_CHECK}

**Syntax:**
```pascal
{$OVERFLOW_CHECK ON}
{$OVERFLOW_CHECK OFF}
```

**Purpose**: Enable/disable arithmetic overflow checking.

**Default**: `ON` in debug mode, `OFF` in release mode.

### 6.4 Build Mode Directives

#### {$DEBUG}

**Syntax:**
```pascal
{$DEBUG}
```

**Purpose**: Enable debug mode (checks, symbols, etc.).

**Effect**: Enables all checks, includes debug symbols.

#### {$RELEASE}

**Syntax:**
```pascal
{$RELEASE}
```

**Purpose**: Enable release mode (optimizations, no checks).

**Effect**: Disables checks, enables optimizations.

### 6.5 Conditional Compilation

#### {$IFDEF}

**Syntax:**
```pascal
{$IFDEF DEBUG}
  WriteLn('Debug mode');
{$ENDIF}
```

**Purpose**: Conditionally compile code.

**Directives:**
- `{$IFDEF name}`: If symbol defined
- `{$IFNDEF name}`: If symbol not defined
- `{$ELSE}`: Else branch
- `{$ENDIF}`: End conditional

**Usage:**
```pascal
{$IFDEF DEBUG}
  WriteLn('Debug: ', value);
{$ELSE}
  // Optimized code
{$ENDIF}
```

### 6.6 Include Directives

#### {$INCLUDE}

**Syntax:**
```pascal
{$INCLUDE 'filename.pas'}
```

**Purpose**: Include source file at this point.

**Usage**: For shared code, constants, types.

**Note**: Included file must be valid Pascal fragment.

---

**See also:**
- [Directive Processing](./09_DirectiveProcessing.md)
- [Standard Directives Summary](./14_StandardDirectivesSummary.md)

