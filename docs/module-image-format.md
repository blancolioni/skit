# Skit Module Image Format (`.skix`)

Concrete on-disk sketch for the relocatable, name-linked module image decided in
[ADR 0002](adr/0002-external-skit-representation.md). This is a **data snapshot**
of a live heap taken between top-level evaluations (post-GC), not a resumable
execution image. Read ADR 0002 first for the *why*; this file is the *what*.

Status: sketch. Field widths and section kinds are provisional until the writer
and two-pass loader are implemented.

## Conventions

- **Integers** are fixed-width unsigned, stored in the byte order declared by the
  header's `endianness` field. `uN` = N-bit unsigned.
- **Object word** (`obj`) is one raw machine word, `word_size` bits wide (32
  today; ADR 0001 may widen to 64). Bit layout — payload in the low
  `word_size - tag_size` bits, tag in the top `tag_size` bits — is fixed by the
  header's `tag_size` / `tag_layout` fields, mirroring `Skit.Object` in
  [skit.ads](../src/skit.ads). The image never interprets a word beyond its tag;
  relocation is by id class (ADR 0002 §"The id classes").
- **Name reference** (`nameref`) = `u32`, a byte offset into the String Pool
  section. The pool holds each distinct name once as `u16 length` + `length`
  bytes, UTF-8; a `nameref` points at the `length`. Names are the only
  cross-boundary identity (imports, exports, symbol atoms, foreign-object class
  names); no
  build-specific integer index ever crosses the boundary. Keeping names in a
  pool — never inline in the header — leaves the header **fixed-size** so
  `section_count` and the directory sit at bootstrap-constant offsets.
- **Local node id** = a cell's index in the Cells section, `0 .. cell_count-1`.
  An `Application` word's payload is a local node id and is offset by the module's
  assigned base at load (ADR 0002, pass 1).
- All section bodies are byte-length-framed by the section directory, so an
  unknown or unwanted section can be skipped without parsing it.

## File layout

```
+--------------------+
| Header             |  fixed-size: magic + version + geometry
+--------------------+
| Section directory  |  section_count x (kind, offset, length)
+--------------------+
| Section bodies ... |  StringPool, Cells, Exports, ImportReloc, SymbolAtoms,
|                    |  Annotations, ForeignObjects, Fingerprint  (any order)
+--------------------+
| Checksum trailer   |  integrity over everything above
+--------------------+
```

The header is fixed-size and the directory immediately follows it, so both are
found at constant offsets with no prior parsing. A reader bootstraps
header → directory → String Pool, then resolves every `nameref` against the pool
before touching the other sections.

## Header

**Fixed size** (no variable-length fields) so `section_count` — and the
directory that follows — sit at constant offsets. Guards the ADR 0001
representation change so an old reader refuses a wider word rather than
misreading it.

| Field            | Type    | Notes |
|------------------|---------|-------|
| `magic`          | `u8[4]` | `"SKIX"` (0x53 0x4B 0x49 0x58) |
| `format_version` | `u16`   | this container format |
| `vm_version`     | `u16`   | pins the baked combinator payloads (1..9 + `Suspension`) |
| `word_size`      | `u8`    | bits per object word: 32 today |
| `tag_size`       | `u8`    | tag bits per word: 2 today |
| `tag_layout`     | `u8`    | id of the tag enum ordering; `0` = `Integer, Primitive, Application, Float` as in [skit.ads](../src/skit.ads) |
| `endianness`     | `u8`    | `0` = little, `1` = big — applies to every `uN` in the file |
| `flags`          | `u16`   | reserved (0) |
| `module_name`    | `u32`   | `nameref` into the String Pool |
| `section_count`  | `u16`   | entries in the section directory (fixed offset) |

`endianness` is a single byte and `magic` is byte-oriented, so both are read
before any multi-byte field needs a byte order — no chicken-and-egg.

## Section directory

`section_count` entries, each:

| Field    | Type  | Notes |
|----------|-------|-------|
| `kind`   | `u16` | section kind (below) |
| `offset` | `u64` | byte offset from start of file to the section body |
| `length` | `u64` | byte length of the body — the skip unit |

Section kinds:

| Kind | Value | Required | Purpose |
|------|-------|----------|---------|
| StringPool  | 0 | yes | every distinct name, referenced by `nameref` |
| Cells       | 1 | yes | the value graph |
| Exports     | 2 | yes | named roots |
| ImportReloc | 3 | yes* | named-import fixups (*may be empty, never absent) |
| SymbolAtoms | 4 | no  | runtime symbol-object names |
| Annotations | 5 | no  | per-export opaque host data (Leander types) |
| ForeignObjects | 6 | no  | host-owned graph nodes (class + children + bytes) |
| Fingerprint | 7 | yes | stale-link detection |

Unknown `kind` values are skipped by `length` (forward-compat). A `nameref` is a
byte offset relative to the StringPool body, so the pool must be located (via the
directory) and available before any name is resolved.

## StringPool (kind 0)

```
str[]        packed { u16 Length; u8 Bytes[Length] }  -- until the section ends
```

A concatenation of length-prefixed UTF-8 strings filling the section body. A
`nameref` is the byte offset (from the pool body start) of a string's `Length`
field. Writers should intern — store each distinct name once and share its
offset — so repeated names (a symbol atom and the export that names it, an import
resolved by many cells) cost one `u32` per use, not one copy. Offset `0` is a
valid name (the first pooled string); there is no "null name" — absence is
modelled by a section omitting the entry, not by a sentinel offset.

## Cells (kind 1)

```
u32          cell_count
cell[]       cell_count x { obj Left; obj Right }
```

Cell `i` is local node id `i`. An `Application` payload is a local node id
(pass-1 relocation adds the base). A slot that is a **named import** holds the
`Undefined` sentinel word (payload 10) and is listed in ImportReloc; pass 2
overwrites it. `Nil` (payload 0) never appears in a valid cell. Unforced
`Suspension` thunks are legal inert content and are reloaded as-is — never
resumed. A `Primitive` payload in the symbol range (4096+) or the foreign-object
range (`2**20 .. 2**21-1`) is rewritten through the pass-1 local-id remap table
(SymbolAtoms / ForeignObjects respectively).

## Exports (kind 2)

```
u32          export_count
export[]     export_count x { nameref Name; u32 Local_Node_Id }
```

The module's named roots / entry set. Export names are unique within a module.
Duplicate export names *across* loaded modules are a hard link error keyed on
`(source module, export name)`; identical provenance (idempotent reload) is
last-wins (ADR 0002, resolved questions).

## ImportReloc (kind 3)

```
u32          import_count
import[]     import_count x { u32 Cell_Index; u8 Side; nameref Import_Name }
                              -- Side: 0 = Left, 1 = Right
```

One entry per import slot. Holds **both** cross-module imports and
host-resolved primitives — one namespace, differing only in who answers the name
at load (sibling exports, then the host environment). This table is
**authoritative and persists across re-dump**: a slot once an import stays an
import, so the writer re-emits the `Undefined` sentinel for these cells even
after a load-and-eval baked resolved objects into them — no reverse map (ADR
0002, Two-pass load).

Resolution order at load: (1) sibling-module exports, (2) host environment
lookup (`Handle.Lookup (name)`), (3) hard error naming the unresolved symbol.

## SymbolAtoms (kind 4)

```
u32          symbol_count
symbol[]     symbol_count x { u32 Local_Symbol_Id; nameref Name }
```

Present only if the module emits runtime symbol objects. `Local_Symbol_Id` =
`payload - Primitive_Variable_Payload'First` (payload `- 4096`). At load, re-intern
each by name into the merged environment and remap old id → new id. Symbol atoms
and export/binding names share **one** namespace (they intern through the same
table in [skit-handles.adb](../src/skit-handles.adb)); a symbol `foo` and the
binding it names are the same object.

## Annotations (kind 5)

```
u32          annotation_count
annotation[] annotation_count x { nameref Export_Name; u32 Length; u8 Bytes[Length] }
```

Keyed by **export name** (survives relocation/renumbering; node ids do not).
Skit never parses `Bytes` — for Leander it is the inferred type. Any internal
versioning lives inside the bytes and is the host's concern.

## ForeignObjects (kind 6)

```
u32          object_count
object[]     object_count x { nameref     Class_Name;
                              u32          Local_Payload;   -- foreign-object range
                              u32          Child_Count;
                              obj          Children[Child_Count];
                              u64          Length;
                              u8           Bytes[Length] }
```

Host-owned graph nodes (see ADR 0002, *Foreign objects*). Each is framed
`class-name + child vector + length + bytes`:

- **`Local_Payload`** is this object's machine-local payload (in the
  foreign-object range `2**20 .. 2**21-1`). Load deserializes the object, binds
  it into the merged machine — yielding a *new* payload — and records
  `Local_Payload → new payload` in the same local-id remap table symbol atoms
  use. Cells referencing the object are rewritten through that map.
- **`Children`** are the object's `Skit.Object` children, written in
  `Foreign_Object_Interface.Visit` order. They are relocated exactly like cell
  contents (internal refs offset by base, nested foreign payloads remapped) and
  handed back to the factory *positionally* — so Visit order is a hard contract
  (unstable order corrupts the object).
- **`Bytes`** are opaque leaf data (`Serialize`), binary-safe
  `Stream_Element_Array`, not `String`. `Length` is `u64` to reserve for
  large/streamed payloads (a future counting-sub-stream writer back-patches this
  slot — no format bump).

At load, dispatch on `Class_Name`:

- name **registered** → call the factory `Deserialize (Bytes, Children)`;
- name **not registered** → skip by the record's byte span (the sole skip
  trigger); a reachable binding that references a skipped object is demoted to an
  error naming the class;
- name registered but factory **fails** → hard load error (not a skip).

A foreign object holds no cross-boundary integer id: its class is a name, its
children are relocated objects, its identity is re-established by `Bind_Object` at
load. On **dump**, `Serialize` may fail (object holds a live non-serializable
resource) — the referencing binding is demoted to an error, mirroring the
unknown-class case on load.

## Fingerprint (kind 7)

```
u8           algo         -- hash algorithm id
u8           length       -- hash byte length
u8           hash[length]
```

Computed as `H(sorted[(export_name, annotation_bytes)])`: export names **plus**
raw annotation bytes, so a type change on an export (same name) invalidates a
stale link. Sorting removes export-order sensitivity. Hash the **dereferenced
name bytes**, never the `nameref` `u32` — the offset is file-position-dependent
and would differ across builds that pool identical names differently. Annotation
bytes are hashed opaquely — skit stays type-agnostic.

## Checksum trailer

```
u8           algo         -- checksum algorithm id
u8           length       -- checksum byte length
u8           sum[length]  -- integrity over all preceding bytes
```

Integrity only; distinct from the interface fingerprint (which is about semantic
staleness, not corruption).

## Load walk (two-pass)

For each module, in one merged heap:

1. **Pass 1** — copy Cells into the merged heap at an assigned base; offset every
   `Application` payload by the base; re-intern SymbolAtoms and remap; deserialize
   ForeignObjects (by `Class_Name`, relocating their `Children` as cell contents),
   bind them, and remap their payloads; register Exports (`name → base + local
   id`) in the global table.
2. **Pass 2** — for each ImportReloc entry, resolve `Import_Name` (sibling
   exports → host environment → hard error) and write the object into the slot.

Every export is registered in pass 1 before any import resolves in pass 2, so
mutually-referencing modules load without a topological order.

## Reconciliation notes

ADR 0002 names `Skit.Environment` / `Skit.Interfaces` that **do not exist yet**.
Concretely today: the environment is the payload-keyed map in
[skit-machines.ads](../src/skit-machines.ads) fronted by the string-intern layer
in [skit-handles.ads](../src/skit-handles.ads); the heap is
[skit-memory.ads](../src/skit-memory.ads); there is **no foreign-object storage
at all**. The whole foreign-object mechanism — `Foreign_Object_Interface`
(`Class_Name`/`Serialize`/`Visit`/`Free`/`Image`), the class-factory registry,
the registry vector + mark flag, collector integration, `Bind_Object`/`Unpin`,
the `2**20 .. 2**21-1` payload range and `Is_Foreign_Object` predicate — is
greenfield. This sketch's field names should be reconciled with real package
names when the writer/loader are implemented.
