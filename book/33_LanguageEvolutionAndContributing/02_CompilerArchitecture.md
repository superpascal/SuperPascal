# Compiler Architecture

**Part of:** [Chapter 33: Language Evolution & Contributing](./README.md)

---

## Introduction

Understanding how the SuperPascal compiler works helps you:
- **Write better code** — Know what the compiler does with your code
- **Debug effectively** — Understand compilation errors
- **Optimize performance** — Know what optimizations are applied
- **Contribute** — Understand where to make changes

The SuperPascal compiler is a **Rust-based cross-compiler** that translates SuperPascal source code into machine code for multiple target platforms.

---

## Compiler Pipeline Overview

The compiler transforms source code through multiple stages:

```
Source Code (.pas)
    ↓
[1] Lexical Analysis → Tokens
    ↓
[2] Syntax Analysis → AST
    ↓
[3] Semantic Analysis → Typed AST
    ↓
[4] IR Generation → Intermediate Representation
    ↓
[5] Code Generation → Machine Code
    ↓
[6] Object File Writing → .ZOF file
```

Each stage has a specific responsibility and produces output for the next stage.

---

## Stage 1: Lexical Analysis (Lexer)

**Purpose:** Convert source text into tokens.

**Input:** Raw source code (string)
**Output:** Sequence of tokens

### What is a Token?

A **token** is the smallest meaningful unit of code:
- **Keywords:** `program`, `begin`, `end`, `if`, `then`
- **Identifiers:** Variable names, function names
- **Literals:** Numbers (`42`), strings (`'hello'`), booleans (`true`)
- **Operators:** `+`, `-`, `*`, `/`, `=`, `<>`
- **Punctuation:** `;`, `,`, `(`, `)`, `.`

### Lexer Responsibilities

1. **Scan source** — Read character by character
2. **Identify tokens** — Recognize keywords, identifiers, literals
3. **Handle whitespace** — Skip spaces, tabs, newlines
4. **Handle comments** — Skip `{ }`, `(* *)`, `//`
5. **Handle directives** — Recognize `{$...}` directives
6. **Report errors** — Invalid characters, unterminated strings

### Example

**Source:**
```pascal
program Hello;
begin
  WriteLn('Hello');
end.
```

**Tokens:**
```
KEYWORD(program) IDENTIFIER(Hello) SEMICOLON
KEYWORD(begin)
  IDENTIFIER(WriteLn) LPAREN STRING('Hello') RPAREN SEMICOLON
KEYWORD(end) PERIOD
```

---

## Stage 2: Syntax Analysis (Parser)

**Purpose:** Convert tokens into an Abstract Syntax Tree (AST).

**Input:** Sequence of tokens
**Output:** AST (tree structure)

### What is an AST?

An **Abstract Syntax Tree** is a tree representation of program structure:
- **Nodes** represent language constructs (statements, expressions)
- **Children** represent sub-constructs
- **Structure** reflects program syntax

### Parser Responsibilities

1. **Parse grammar** — Recognize valid SuperPascal syntax
2. **Build AST** — Create tree nodes
3. **Handle precedence** — Operator precedence and associativity
4. **Handle errors** — Syntax errors, recovery
5. **Support DSLs** — Parse tilemap DSL, intrinsics

### Example

**Tokens:**
```
IDENTIFIER(x) ASSIGN INTEGER(10) PLUS INTEGER(5)
```

**AST:**
```
AssignStmt
  target: Variable(x)
  value: BinaryExpr
    op: Plus
    left: Literal(10)
    right: Literal(5)
```

### Parser Strategy

**Recursive Descent:**
- One function per grammar production
- Top-down parsing
- Easy to understand and maintain

**Pratt Parsing:**
- For expressions
- Handles precedence elegantly
- Efficient

---

## Stage 3: Semantic Analysis

**Purpose:** Analyze meaning, check types, build symbol tables.

**Input:** AST
**Output:** Typed AST with symbol information

### Semantic Responsibilities

1. **Type checking** — Verify types are correct
2. **Symbol resolution** — Resolve variable/function names
3. **Scope analysis** — Handle nested scopes
4. **Type inference** — Infer types where possible
5. **Constant evaluation** — Evaluate constant expressions
6. **Error checking** — Undefined variables, type mismatches

### Type Checking

**Verifies:**
- Variable types match usage
- Function parameters match calls
- Return types match declarations
- Operators work with operand types
- Array indices are integers

**Example:**
```pascal
var x: integer;
begin
  x := 'hello';  // ERROR: Type mismatch
end.
```

### Symbol Tables

**Stores:**
- Variable declarations
- Function/procedure declarations
- Type definitions
- Scope information

**Used for:**
- Resolving names
- Checking for undefined variables
- Verifying declarations

---

## Stage 4: IR Generation

**Purpose:** Convert typed AST into Intermediate Representation (IR).

**Input:** Typed AST
**Output:** IR (platform-independent representation)

### What is IR?

**Intermediate Representation** is a platform-independent code representation:
- **Simpler than AST** — Fewer node types
- **More structured than machine code** — Still high-level
- **Platform-agnostic** — Same IR for all platforms
- **Optimizable** — Easy to apply optimizations

### IR Levels

**IR1 (Linear IR):**
- Simple, linear representation
- Easy to generate
- Good for initial implementation

**IR2 (Future):**
- More sophisticated
- Better optimization opportunities
- SSA (Static Single Assignment) form

### IR Generation

**Process:**
1. Traverse typed AST
2. Generate IR instructions
3. Handle control flow
4. Manage registers/temporaries

**Example:**
```pascal
x := 10 + 5;
```

**IR:**
```
t1 = 10
t2 = 5
t3 = t1 + t2
x = t3
```

---

## Stage 5: Code Generation

**Purpose:** Convert IR into platform-specific machine code.

**Input:** IR
**Output:** Machine code (assembly or binary)

### Code Generation Responsibilities

1. **Instruction selection** — Choose machine instructions
2. **Register allocation** — Assign registers to variables
3. **Instruction scheduling** — Order instructions efficiently
4. **ABI compliance** — Follow calling conventions
5. **Optimization** — Peephole, constant folding

### Platform-Specific Codegen

**ZealZ80 (Z80):**
- 8-bit instructions
- Register pairs (BC, DE, HL)
- Stack-based calling convention
- MMU banking support

**Foenix65C816 (WDC65C816S):**
- 16-bit instructions
- Extended addressing (24-bit)
- Native/emulation modes
- IO page architecture

**FoenixA2560M (MC68LC060):**
- 32-bit instructions
- Modern architecture
- Large address space
- Advanced features

### ABI Compliance

**Application Binary Interface** defines:
- Parameter passing (registers, stack)
- Return value handling
- Stack frame layout
- Register usage conventions
- Name mangling

**Why:** Ensures code from different units works together.

---

## Stage 6: Object File Writing

**Purpose:** Write machine code and metadata to object file.

**Input:** Machine code + metadata
**Output:** `.ZOF` file (Zeal Object Format) or platform-specific format

### Object File Contents

1. **Code section** — Machine code
2. **Data section** — Initialized data
3. **Symbol table** — Exported/imported symbols
4. **Relocation info** — Address fixups
5. **Debug info** — Source line mappings

### Object File Format

**`.ZOF` (Zeal Object Format):**
- Platform-specific format
- Contains code, data, symbols
- Supports linking multiple units
- Includes debug information

---

## Compiler Modules

The compiler is organized into **14 modules**:

### Frontend Modules

1. **`lexer/`** — Lexical analysis
2. **`tokens/`** — Token definitions
3. **`parser/`** — Syntax analysis
4. **`ast/`** — AST node definitions

### Middle-End Modules

5. **`semantics/`** — Semantic analysis
6. **`types/`** — Type system
7. **`symbol_table/`** — Symbol management
8. **`ir/`** — IR generation (IR1, IR2)

### Backend Modules

9. **`codegen/`** — Code generation
   - **`z80/`** — Z80 backend
   - **`wdc65c816/`** — 65C816 backend (future)
   - **`m68k/`** — 68060 backend (future)
10. **`object/`** — Object file writing

### Support Modules

11. **`runtime_specs/`** — Runtime specifications
12. **`errors/`** — Error types
13. **`diagnostics/`** — Error reporting
14. **`driver/`** — Main compiler driver

---

## Optimization

### Optimization Levels

**None (Debug):**
- No optimizations
- Fast compilation
- Easy debugging

**Basic:**
- Constant folding
- Dead code elimination
- Simple peephole

**Full:**
- All optimizations
- Slower compilation
- Better performance

### Optimization Techniques

**Constant Folding:**
```pascal
x := 10 + 5;  // → x := 15;
```

**Dead Code Elimination:**
```pascal
if false then
  DoSomething;  // Removed
```

**Peephole Optimization:**
```assembly
LD A, 0
ADD A, B
; → LD A, B  (simpler)
```

**Loop Unrolling:**
```pascal
for i := 1 to 4 do
  Process(i);
// → Process(1); Process(2); Process(3); Process(4);
```

---

## Error Handling

### Error Types

**Lexical Errors:**
- Invalid characters
- Unterminated strings
- Malformed numbers

**Syntax Errors:**
- Missing semicolons
- Mismatched parentheses
- Invalid grammar

**Semantic Errors:**
- Type mismatches
- Undefined variables
- Invalid operations

**Codegen Errors:**
- Register allocation failures
- ABI violations
- Platform limitations

### Error Reporting

**Format:**
```
Error: [file.pas:line:column] Message
```

**Features:**
- Source location
- Clear messages
- Suggestions
- Context

---

## Compiler Directives

**Directives** are special instructions to the compiler:

**Syntax:** `{$DIRECTIVE}`

**Examples:**
- `{$INLINE}` — Inline function
- `{$UNROLL}` — Unroll loop
- `{$RANGE_CHECK ON}` — Enable bounds checking
- `{$DEBUG ON}` — Debug mode

**Processing:**
- Lexer recognizes directives
- Parser processes them
- Semantics applies them
- Codegen uses them

---

## Summary

**Compiler Pipeline:**
1. **Lexer** — Source → Tokens
2. **Parser** — Tokens → AST
3. **Semantics** — AST → Typed AST
4. **IR Gen** — Typed AST → IR
5. **Codegen** — IR → Machine Code
6. **Object Writer** — Machine Code → .ZOF

**Key Concepts:**
- **Tokens** — Smallest meaningful units
- **AST** — Tree representation
- **IR** — Platform-independent code
- **ABI** — Calling conventions
- **Optimization** — Performance improvements

**Next:** Learn how to extend the language.

---

**Next Section:** [Extending the Language](./03_ExtendingTheLanguage.md)  
**Compiler Documentation:** See [MODULES.md](../../../crates/compiler-rs/MODULES.md)  
**Last Updated:** 2025-01-XX

