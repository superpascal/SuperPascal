# Memory Management Intrinsics

**Part of:** [06_IntrinsicsAndDirectives.md](../06_IntrinsicsAndDirectives.md)

---

### 3.1 MapPage

**Syntax:**
```pascal
procedure MapPage(Slot: byte; Page: byte);
```

**Purpose**: Map memory page to MMU slot.

**Parameters:**
- `Slot`: MMU slot (0-15)
- `Page`: Physical page number

**Codegen**: Writes to MMU registers.

**Usage:**
```pascal
MapPage(2, 10);  // Map page 10 to slot 2
```

---

**See also:**
- [Direct Memory Access](./03A_DirectMemoryAccess.md)
- [I/O Port Access](./03B_IOPortAccess.md)
- [Absolute Addressing](./03C_AbsoluteAddressing.md)

