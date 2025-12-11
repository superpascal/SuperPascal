# Tilemap DSL

**Part of:** [06_IntrinsicsAndDirectives.md](../06_IntrinsicsAndDirectives.md)

---

### 7.1 Tilemap Constructor

**Syntax:**
```pascal
const
  Level1 = (
    Width: 10;
    Height: 8;
    Data: (
      ROW('##########'),
      ROW('#........#'),
      ROW('#..####..#')
    )
  );
```

**Purpose**: Define tilemaps using text representation.

**Properties:**
- `Width`: Tilemap width (compile-time constant)
- `Height`: Tilemap height (compile-time constant)
- `Data`: Array of `ROW()` strings

### 7.2 ROW Function

**Syntax:**
```pascal
ROW('###..###')
```

**Purpose**: Define one row of tiles.

**Mapping**: Characters map to tile indices via tile legend.

**Rules:**
- All rows must have same length (Width)
- Number of rows must equal Height
- Characters map via tile legend (defined elsewhere)

### 7.3 Tile Legend

Tile legend maps characters to tile indices:

```pascal
const
  TILE_WALL = 1;
  TILE_FLOOR = 0;
  // Legend: '#' = TILE_WALL, '.' = TILE_FLOOR
```

**Default mapping** (if no legend):
- Space (` `) = 0
- Other characters = ASCII value

---

**See also:**
- [Grammar Specification](../02_Grammar.md) for formal syntax

