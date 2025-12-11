# Intrinsic Implementation

**Part of:** [06_IntrinsicsAndDirectives.md](../06_IntrinsicsAndDirectives.md)

---

### 8.1 Compiler Handling

**Intrinsics are:**
1. Recognized during parsing
2. Type-checked like regular procedures
3. Replaced with optimized code sequences
4. Cannot be overloaded or redefined

### 8.2 Code Generation

**Strategy:**
- Simple intrinsics: Inline code sequence
- Complex intrinsics: Call to runtime routine
- Hardware intrinsics: Direct register/port access

**Example** (`WaitVBlank`):
```asm
call Runtime_WaitVBlank
```

**Example** (`WriteReg`):
```asm
ld hl, (Reg)      ; Load register address
ld a, (Value)     ; Load value
out (hl), a       ; Write to port (if memory-mapped)
```

---

**See also:**
- [Platform ABI Specifications](../../platforms/README.md) for code generation details (ABI is platform-specific)

