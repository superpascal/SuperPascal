# Standard Directives Summary

**Part of:** [06_IntrinsicsAndDirectives.md](../06_IntrinsicsAndDirectives.md)

---

| Directive | Purpose | Scope |
|-----------|---------|-------|
| `{$INLINE}` | Inline function | Next routine |
| `{$UNROLL}` | Unroll loop | Next loop |
| `{$VBLANK_AWARE}` | VBlank optimization | Next routine |
| `{$RANGE_CHECK}` | Bounds checking | Until changed |
| `{$OVERFLOW_CHECK}` | Overflow checking | Until changed |
| `{$DEBUG}` | Debug mode | Global |
| `{$RELEASE}` | Release mode | Global |
| `{$IFDEF}` | Conditional compile | Block |
| `{$INCLUDE}` | Include file | Point |
| `{$ECS_ARCHETYPE}` | ECS archetype hint | Next routine |
| `{$ECS_INLINE_COMPONENT}` | Inline component access | Next routine |
| `{$PHYSICS_FIXED_TIMESTEP}` | Fixed timestep physics | Next routine |

---

**See also:**
- [Compiler Directives](./06_CompilerDirectives.md) for detailed documentation
- [Directive Processing](./09_DirectiveProcessing.md)
- [Game Engine Directives](./13_GameEngineDirectives.md)

