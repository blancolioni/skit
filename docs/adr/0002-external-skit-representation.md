# ADR 0002: External Skit Representation (Module Image Format)

- **Status:** Proposed
- **Date:** 2026-07-04
- **Deciders:** Fraser Wilson

## Context

Skit needs an on-disk representation so that compiled work can be persisted
and reloaded without re-running the compiler pipeline. The immediate goal is
**precompiled modules**: emit a Skit image per module, then load several of
them into one machine and link them together.

A Skit runtime value is a single 32-bit tagged `Object`
(`Integer`/`Primitive`/`Application`/`Float`); the heap is a `Cell_Array` of
`(Left, Right)` object pairs, and an `Application` object's payload is a cell
index. Named roots and host-supplied opaque objects live in
`Skit.Environment` (`Bindings : name -> Object`, `Blobs : name -> Interfaces`).
See [skit.ads](../../src/skit.ads),
[skit-impl-memory.ads](../../src/skit-impl-memory.ads), and
[skit-environment.ads](../../src/skit-environment.ads).

The core content of an image is a dump of the live heap immediately after a
garbage collection (the copying collector has already compacted the live set),
plus the symbol table that names its roots, plus arbitrary per-symbol user data
(Leander stores the inferred type there). Skit is deliberately weakly typed and
must remain **agnostic to that user data** — it frames and routes it, the host
owns its meaning.

### Snapshot, not image-resume

The format is a **data snapshot**, not a resumable execution image. It captures
the reachable value graph and its named roots; it does *not* capture the
reduction/spine stack or any mid-reduction machine state. Snapshots are taken
between top-level evaluations. Unforced thunks (`Suspension` cells) reachable
from a bound value are legal graph content and are reloaded as inert cells; the
loader must never attempt to resume them.

## Decision drivers

- **Relocation is mandatory.** Multiple independently-compiled modules are
  loaded into one heap. Each module's cell addresses are local to that module,
  so they cannot be absolute. The format must be position-independent.
- **Cross-module references cannot be addresses.** A precompiled module may
  reference a symbol defined in a module that is not yet loaded. Such an edge
  must be a *symbolic import* resolved at link time, never a raw node index.
- **Skit stays type-agnostic.** Leander's per-symbol types, and host blob
  contents, are opaque payloads. Skit must be able to frame, skip, and route
  them by name without parsing them.
- **Format must survive representation change.** ADR 0001 may widen `Object`
  from 32 to 64 bits (NaN-boxing) and change float precision. An image with no
  version/word-size header becomes unreadable the day that lands.
- **Naming over numbering.** Wherever an identity crosses a module or machine
  boundary — imports, symbol atoms, primitives, blob types — it is resolved by
  **name**, not by an integer index into some build-specific table. Integer ids
  collide across independent builds and extensions; names decouple. Every such
  boundary reduces to a lookup in a binding map keyed by name.

## Decision

Adopt a **relocatable, name-linked module image** with a two-pass loader.

### The id classes

A cell payload is relocated according to its class, distinguishable from the
object tag and payload range:

| Class | Encoding | Relocation on load |
|-------|----------|--------------------|
| Internal cell ref | `Application` payload = local node id | Add the module's assigned base. Uniform; needs no per-entry table. |
| Named import | Slot holds a sentinel (`Undefined`); listed in the reloc table | Resolve the import name (see resolution order below); write the resolved object into the slot. |
| Symbol atom | `Primitive` payload in the interned-symbol range (4096+) | Re-intern by name into the merged environment; remap old id to new id. |
| VM-fixed combinator | `Primitive` 1..13 (`S`,`K`,`I`,`B`,`C`, …) | Left untouched. Pinned by the header's format/VM version. |
| Integer / Float | `Integer`, `Float` | Left untouched (immediate). |

Internal refs relocate *implicitly* (the loader walks all cells and offsets
every `Application` payload), so only named imports need an explicit relocation
table — this is why the table is kept **separate** from the cells rather than
inlined.

### Primitives are named imports, symbolic from compile time

A `foreign import` primitive is bound at compile time to some concrete object
(today, a `Primitive` opcode payload). That object is **not authoritative**: a
different machine build may assign a different opcode, or the host may return a
different object entirely (an opcode, a blob, even an application graph). So a
primitive reference must never be baked into a dumped cell.

Two candidate mechanisms were considered:

- **A. Reverse map at dump time** — the machine keeps `Object -> name` and the
  dumper de-bakes primitive objects back to names. Rejected: the inverse is not
  unique (aliased names → one opcode object) and needs a canonical-name rule.
- **B. Symbolic from compile time (chosen)** — the compiler never bakes a host
  primitive into a cell. A `foreign import` reference is a named-import slot from
  birth, resolved on *every* load, including the compile-run's own. No reverse
  map, no aliasing question; "the resolved object differs from compile time" is
  the normal path, not a special case.

Primitives therefore collapse into the same reloc table and patch loop as
cross-module imports. They differ only in *who answers the name*: a sibling
module's export, or the standing environment (`Environment.Lookup`).

### Resolution order for a named import

Each import name is resolved, in order:

1. **Sibling-module exports** (link-time, module-to-module).
2. **The loaded environment** — `Skit.Environment.Lookup (name)`. No new
   registry is introduced: the host already populates the environment with
   primitives and top-level bindings via `Environment.Bind` (primitives through
   `Machine.Bind`), and each returns whatever object is correct for *this*
   machine build. Load-time resolution is simply that lookup.
3. Otherwise a **hard error** naming the unresolved symbol.

Both steps reduce to a name lookup against a binding map — sibling exports are a
per-load overlay, the environment is the standing set. VM-fixed combinators
(1..13) are the *only* primitives dumped literally, and only because the header
pins them to a VM version.

### Module container sections

1. **Header** — magic, format version, word size, endianness, tag layout,
   module name, section counts. Guards the ADR 0001 representation change.
2. **Cells** — N x `(Left, Right)` raw object words in local node ids. Import
   slots hold a sentinel until patched.
3. **Exports** — `name -> local node id`. The module's named roots / entry set.
4. **Import relocation table** — `(cell index, Left|Right) -> import name`. The
   separate fixup table. Holds both cross-module imports and host-resolved
   primitives; they share one namespace and differ only in resolution source.
5. **Symbol-atom table** — `local symbol id -> name` (present only if the module
   emits runtime symbol objects).
6. **Annotations** — per export, an opaque `tag + length + bytes` record
   (Leander's inferred type). Skit never parses it.
7. **Blobs** — per host object, `type-tag (string) + length + opaque bytes`
   (see below).
8. **Interface fingerprint** — hash of export names (and, provisionally, their
   annotations) for stale-link detection.
9. **Checksum** — integrity over the image.

### Two-pass load

- **Pass 1 (per module):** copy cells into the merged heap at an assigned base;
  offset every `Application` payload by the base; re-intern symbol atoms and
  remap; register exports (`name -> base + local id`) in the global table.
- **Pass 2 (per module):** for each import-reloc entry, resolve the name by the
  resolution order above (sibling exports, then host resolver) and write the
  object into the cell slot.

Two passes (rather than a topological load order) let modules reference each
other mutually: every export is registered in pass 1 before any import is
resolved in pass 2. An import that resolves to nothing is a hard error naming the
symbol.

### Blobs: host-owned, skit-routed

`Skit.Environment.Blobs` are live host Ada objects
(`Skit.Interfaces.Abstraction'Class`) — a live access value cannot be
serialized. The abstraction gains:

```ada
function Serialize (This : Instance) return Ada.Streams.Stream_Element_Array;
--  dispatching; produces the blob's own opaque bytes
```

Deserialization is a *class* operation (no object exists yet), so it cannot be a
primitive method. Instead the host **registers** a factory with the machine
before loading, keyed by string type-tag:

```ada
--  Factory : Stream_Element_Array -> Interfaces.Reference
Register_Blob_Type (Machine, Tag => "myhost.Vector3", Factory => ...);
```

Skit frames each blob as `tag + length + bytes` and, at load, dispatches on the
tag:

- tag **registered** -> call the factory with the bytes;
- tag **not registered** -> *unknown blob*; skip it (this is the sole skip
  trigger);
- tag registered but factory **fails** (corrupt bytes) -> hard load error, not a
  skip.

`Stream_Element_Array` is chosen over `String`: `Stream_Element` is a byte and
binary-safe, whereas `String` is `Character` and, under `-gnatVa`, arbitrary
bytes in a `String` risk invalid-value checks. Length-framing is owned by skit
(not the blob) so an unknown blob can be skipped by byte count. Any per-blob
format versioning lives *inside* the opaque bytes and is the host's concern.

If a skipped (unknown) blob is referenced by a reachable binding's graph, that
binding is demoted to an error naming the missing blob type; the rest of the
load proceeds.

### One consistent principle

Across the whole format, **skit frames and routes by name; the host owns the
payload.** Cross-module imports resolve by export name; primitives resolve by
name through the host; annotations are opaque length-framed records; blobs
dispatch by string type-tag. No cross-boundary identity is a build-specific
integer index — the sole exception being VM-fixed combinators (1..13), pinned by
the header's version fields.

## Open questions

- **Duplicate export** across two loaded modules: hard error, or last-wins?
- **Interface fingerprint contents**: export names only, or names + annotated
  types? Including types catches more stale links but couples the fingerprint to
  Leander's type encoding.
- **Annotation keying**: key on export *name* or export *node id*? Name survives
  export dedup/renumbering; node id does not — leaning name.
- **Symbol-atom vs export namespace**: confirm runtime symbol atoms and exported
  binding names share, or are kept in, distinct name spaces on re-intern.
- **Opcode range (64..255) in dumps**: fully retired in favour of named
  host-resolved imports (decision B applied uniformly), or kept literal for a
  hot core set pinned by the VM version like the 1..13 combinators? Leaning
  fully named, with only 1..13 baked.
- **Streaming large blobs**: current design materializes a blob's bytes whole.
  If a genuinely large blob appears, revisit with a counting sub-stream and a
  back-patched length slot (incremental + still skippable).

## Consequences

To be recorded once the format is implemented. The public interface in
[skit.ads](../../src/skit.ads) is expected to stay stable; new surface is
additive — a blob `Serialize` method on `Skit.Interfaces`, a blob-type registry
on the machine/environment, and image read/write entry points. Name resolution
reuses the existing `Skit.Environment` `Bind`/`Lookup`; no resolver API is added.
Compiler-side, `foreign import` references must be emitted as named import slots
rather than baked opcode objects (decision B). This ADR depends
on ADR 0001 only through the header's word-size/tag-layout fields, which exist
precisely so the two decisions can move independently.
