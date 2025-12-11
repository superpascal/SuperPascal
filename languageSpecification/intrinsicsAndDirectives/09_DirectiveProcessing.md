# Directive Processing

**Part of:** [06_IntrinsicsAndDirectives.md](../06_IntrinsicsAndDirectives.md)

---

### 9.1 Directive Order

Directives processed in source order:

```pascal
{$DEBUG}
{$RANGE_CHECK ON}
{$INLINE}
procedure Test;
```

### 9.2 Directive Scope

**Global scope:**
- `{$DEBUG}`, `{$RELEASE}`
- `{$IFDEF}`, etc.

**Local scope:**
- `{$INLINE}`: Next routine
- `{$UNROLL}`: Next loop
- `{$RANGE_CHECK}`: Until changed

### 9.3 Directive Errors

**Common errors:**
- Unknown directive name
- Invalid directive argument
- Unmatched `{$IFDEF}` / `{$ENDIF}`
- Directive in invalid location

**Error reporting:**
```
Error: Unknown directive '{$UNKNOWN}'
Error: Unmatched {$IFDEF}
```

---

**See also:**
- [Compiler Directives](./06_CompilerDirectives.md)
- [Standard Directives Summary](./14_StandardDirectivesSummary.md)

