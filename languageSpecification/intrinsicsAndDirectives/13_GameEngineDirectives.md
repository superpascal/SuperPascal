# Game Engine Directives

**Part of:** [06_IntrinsicsAndDirectives.md](../06_IntrinsicsAndDirectives.md)

---

### 13.1 ECS Optimization Directives

#### {$ECS_ARCHETYPE}

**Syntax:**
```pascal
{$ECS_ARCHETYPE Dynamic}
procedure MovementSystem;
```

**Purpose**: Specify archetype for system optimization.

**Effect**: Compiler optimizes system for specified archetype's component layout.

#### {$ECS_INLINE_COMPONENT}

**Syntax:**
```pascal
{$ECS_INLINE_COMPONENT}
function GetPositionX(id: TEntityID): integer;
```

**Purpose**: Inline component access functions.

**Effect**: Component array access inlined at call sites.

### 13.2 Physics Directives

#### {$PHYSICS_FIXED_TIMESTEP}

**Syntax:**
```pascal
{$PHYSICS_FIXED_TIMESTEP}
procedure PhysicsUpdate;
```

**Purpose**: Use fixed timestep for physics calculations.

**Effect**: Physics calculations use constant delta time for determinism.

---

**See also:**
- [Game Engine Intrinsics](./10_GameEngineIntrinsics.md)
- [Game Engine Specification](../09_GameEngine.md)
- [Standard Directives Summary](./14_StandardDirectivesSummary.md)

