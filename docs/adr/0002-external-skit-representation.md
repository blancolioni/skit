# ADR 0002: External Skit Representation (Module Image Format)

- **Status:** Accepted
- **Date:** 2026-07-04 (open questions resolved 2026-07-20)
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

> The concrete byte-level layout is sketched in
> [module-image-format.md](../module-image-format.md).

### The id classes

A cell payload is relocated according to its class, distinguishable from the
object tag and payload range:

| Class | Encoding | Relocation on load |
|-------|----------|--------------------|
| Internal cell ref | `Application` payload = local node id | Add the module's assigned base. Uniform; needs no per-entry table. |
| Named import | Slot holds a sentinel (`Undefined`); listed in the reloc table | Resolve the import name (see resolution order below); write the resolved object into the slot. |
| Symbol atom | `Primitive` payload in the interned-symbol range (4096+) | Re-intern by name into the merged environment; remap old id to new id. |
| Foreign object | `Primitive` payload in the foreign-object range (`2**20 .. 2**21-1`) | Deserialize the blob by class name (factory), bind into this machine, remap old payload to new. Same *mechanism* as symbol atoms (local-id remap), different source (run factory vs re-intern name). |
| VM-fixed combinator | `Primitive` payload 1..9 (`S`,`K`,`I`,`C`,`B`,`S′`,`B*`,`C′`,`Y`), plus `Suspension` (11) | Left untouched. Pinned by the header's format/VM version. |
| Integer / Float | `Integer`, `Float` | Left untouched (immediate). |

Internal refs relocate *implicitly* (the loader walks all cells and offsets
every `Application` payload), so only named imports need an explicit relocation
table — this is why the table is kept **separate** from the cells rather than
inlined.

Of the low, VM-fixed `Primitive` payloads, exactly two are *not* in the
untouched set: `Undefined` (10) is the import sentinel — it is the "named
import" row above, patched not preserved; and `Nil` (0) never appears in a
valid cell (`Machine.Append` forbids `Nil` on either side). Payloads 12..13 are
currently unused. The pinned set the header's VM version guarantees is therefore
`1..9` (the combinators, `Skit.Combinator_Payload`) plus `Suspension` (11); see
[skit.ads](../../src/skit.ads).

Though the table lists several classes, there are only **three relocation
strategies** — the rest are no-ops:

- **implicit offset** — internal `Application` refs, via the whole-heap walk;
- **reloc-table patch** — named imports (and host primitives), by name;
- **local-id remap** — symbol atoms *and* foreign objects: pass 1 builds an
  old-payload → new-payload map, then one rewrite pass applies it. The two
  differ only in how the new object is obtained (re-intern a name / run a
  deserializer factory), so they share the map and the rewrite.

Combinators, integers and floats are left untouched — not relocated at all.

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
(payloads 1..9, plus `Suspension`) are the *only* primitives dumped literally,
and only because the header pins them to a VM version.

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
7. **Foreign objects** — per host object, `class-name + child-object vector +
   length + opaque bytes`. The child vector holds the object's `Skit.Object`
   children (relocated like cells); the bytes are opaque leaf data. See
   *Foreign objects* below.
8. **Interface fingerprint** — hash of export names (and, provisionally, their
   annotations) for stale-link detection.
9. **Checksum** — integrity over the image.

### Two-pass load

- **Pass 1 (per module):** copy cells into the merged heap at an assigned base;
  offset every `Application` payload by the base; re-intern symbol atoms and
  remap; deserialize foreign objects (by class name) and remap their payloads —
  their child vectors are relocated as cell contents in the same pass; register
  exports (`name -> base + local id`) in the global table.
- **Pass 2 (per module):** for each import-reloc entry, resolve the name by the
  resolution order above (sibling exports, then host resolver) and write the
  object into the cell slot.

Two passes (rather than a topological load order) let modules reference each
other mutually: every export is registered in pass 1 before any import is
resolved in pass 2. An import that resolves to nothing is a hard error naming the
symbol.

**Re-dump preserves import provenance via the reloc table, not the graph.**
Decision B keeps first-emit cells clean (import slots hold the `Undefined`
sentinel), but a heap snapshotted *after* a load-and-eval no longer is: pass 2
wrote the resolved objects — including build-specific `Primitive_Function`
opcodes — back into those slots. Re-inspecting the object to recover its name
would be exactly the rejected reverse-map (decision A). Instead the
**import-reloc table is authoritative and persists**: a slot once listed as a
named import stays one across every re-dump. The dumper re-emits the sentinel for
any cell carried in the inherited-and-merged reloc table rather than looking at
what pass 2 left there. Provenance travels in the table; the object graph is
never consulted to un-bake a primitive. This is what makes "fully named, no
reverse map" (see the primitives decision) survive re-dump.

### Foreign objects: host-owned graph nodes

A **foreign object** is a live host Ada value that participates in the Skit heap
as a first-class object: it can hold `Skit.Object` children, is traced by the
collector, and serializes into an image. (This supersedes the earlier passive
"blob" notion — a blob is just a foreign object with no children.) A foreign
object is stored outside the `Cell_Array` in a per-machine registry and is
referenced from the heap by a `Primitive`-tagged object whose payload lies in the
foreign-object range (`2**20 .. 2**21-1`, within the 30-bit payload; disjoint
from symbols `4096 .. 65535`). `Bind_Object` allocates a registry slot and
returns that object; the registry maps `payload -> Foreign_Object_Interface'Class`.

#### The interface

```ada
type Foreign_Object_Interface is limited interface;

function Class_Name (This : Foreign_Object_Interface) return String is abstract;
--  stable type tag; written to the image so the object can be deserialized,
--  and the key the deserializer factory is registered under.

function Serialize (This : Foreign_Object_Interface)
   return Ada.Streams.Stream_Element_Array is abstract;
--  opaque leaf bytes only (may be empty). Object children are NOT encoded here
--  -- they are machine-local references and travel in the child vector.

procedure Visit
  (This    : in out Foreign_Object_Interface;
   Process : not null access procedure (Child : in out Object)) is abstract;
--  call Process on every Object child, in a FIXED, DETERMINISTIC order.
--  One traversal, two consumers:
--    * GC     -- Process forwards the child and rewrites the slot in place
--               (hence Child is in out, and This is in out).
--    * dump   -- Process appends the child to the record's child vector.

procedure Free (This : in out Foreign_Object_Interface) is abstract;
--  no live references remain; release resources. Called exactly once.

function Image (This : Foreign_Object_Interface) return String is abstract;
--  per-instance rendering for Machine.Debug_Image.
```

`Serialize` and `Visit` are two halves of one serialization: `Serialize` emits
opaque bytes, `Visit` enumerates the relocatable `Object` children. **`Visit`'s
order is a contract:** dump writes children in `Visit` order; load hands them
back positionally to the factory; `Deserialize` reconstructs by position. An
unstable order silently corrupts the object.

#### Deserialization is a class operation

No object exists yet at load, so deserialization cannot be a primitive method.
The host **registers a factory** before loading, keyed by class name:

```ada
--  Deserialize : (Stream_Element_Array, Object_Array) -> Reference
Register_Object_Class (Machine, Name => "myhost.Vector3", Deserialize => ...);
```

The factory receives both the opaque bytes and the already-relocated child
objects. Requiring registration at `Bind_Object` time is recommended, so an
object whose class has no factory cannot be created and the missing-factory error
surfaces before dump, not at load.

At load skit dispatches on the class name:

- name **registered** -> call the factory with `(bytes, children)`;
- name **not registered** -> *unknown object*; skip it (the sole skip trigger).
  A reachable binding that references a skipped object is demoted to an error
  naming the missing class; the rest of the load proceeds;
- name registered but factory **fails** (corrupt bytes) -> hard load error.

`Stream_Element_Array` is chosen over `String`: `Stream_Element` is a byte and
binary-safe, whereas `String` is `Character` and, under `-gnatVa`, arbitrary
bytes risk invalid-value checks. Length-framing is owned by skit (not the object)
so an unknown object can be skipped by byte count. Per-object format versioning
lives inside the opaque bytes and is the host's concern.

On **dump**, `Serialize` may fail (the object holds a non-serializable live
resource — an open socket, a GPU handle). This is the mirror of unknown-object-
on-load: the referencing binding is demoted to an error naming the object, and
the rest of the dump proceeds.

#### Garbage collection

Foreign objects live outside `Cell_Array`, so they never move — the collector
tracks *slot liveness* and forwards their *children*. The registry is a vector
with a per-slot mark flag:

- **Marking is inline in the copy/scan closure, not a post-pass.** When the scan
  reaches a foreign payload in a live cell: if the slot is already marked, skip
  it (this breaks cycles, including foreign -> cell -> foreign); otherwise set the
  mark and `Visit` the object with the forwarding `Process`, so its children enter
  the copy closure. A child may be the only reference keeping a cell alive, so
  this must happen *during* transitive closure.
- **Freeing is the post-pass sweep.** After the closure, walk the registry: any
  slot allocated but unmarked this cycle has no live reference — call `Free` and
  clear the slot (reusable). This gives Free-exactly-once for free (cleared slots
  are never revisited) and covers cycles and unreachable islands. An epoch
  counter avoids a separate flag-reset pass.
- **Shutdown** sweeps and `Free`s every remaining live foreign object; GC
  collection alone does not guarantee all are freed.

Generational collection (were it adopted; the ADR 0008 design is rejected) would
break the naive sweep — a minor GC does not scan old space, so a foreign object
referenced only from old space would be wrongly freed, and old->foreign /
foreign->young edges would need remembered-set treatment. If a generational
collector lands, the simplest safe rule is to pin all foreign objects as old.

#### The fresh-bind hazard

Between `Bind_Object` returning an object and that object being stored into a
rooted cell, the object is reachable only from Ada locals — invisible to the
collector. A GC in that window (e.g. triggered by the very `Append` that installs
it) would `Free` it: a use-after-free. The `Install` path sidesteps this by
forbidding GC mid-build ([skit-handles.ads](../../src/skit-handles.ads)), but
that does not generalize to arbitrary host code.

**Mitigation — pin on bind.** `Bind_Object` sets a `pinned` flag on the slot;
pinned slots are unconditional GC roots, marked live regardless of references.
The host calls `Unpin (obj)` once the object is safely stored in a rooted cell.
GC never frees a pinned slot, so bind/`Append` may interleave freely. The failure
mode of forgetting to unpin is a *leak* (still reclaimed by the shutdown sweep),
never a use-after-free.

### One consistent principle

Across the whole format, **skit frames and routes by name; the host owns the
payload.** Cross-module imports resolve by export name; primitives resolve by
name through the host; annotations are opaque length-framed records; foreign
objects dispatch by string class name. No cross-boundary identity is a build-specific
integer index — the sole exception being VM-fixed combinators (payloads 1..9,
plus `Suspension`), pinned by the header's version fields.

## Resolved questions

- **Duplicate export** across two loaded modules — **hard error**, keyed on
  `(source module, export name)`. Distinct sources exporting the same name is a
  link-time collision naming both modules; silent last-wins would make link order
  semantically significant and mask real clashes (cf. Haskell's ambiguous
  import). *Identical* provenance (idempotent reload of the same module, or
  decision B re-resolving a primitive on its own compile-run) is last-wins, not
  an error. An explicit per-import `override` may be added later for intentional
  prelude replacement. Note the current `Machine.Bind` is silent last-wins
  ([skit-machines.adb](../../src/skit-machines.adb)); the linker enforces this
  policy above that primitive, which stays override-by-default for the host.

- **Interface fingerprint contents** — **export names + raw annotation bytes**,
  hashed opaquely. Names-only would accept an unsound link where a producer's
  export changed type but kept its name. Skit stays type-agnostic by hashing the
  *bytes* of the already-opaque annotation section (§6), never parsing them:
  `H(sorted[(name, annotation_bytes)])`. Sorting removes export-order
  sensitivity. Cost: the fingerprint churns on cosmetic annotation-encoding
  changes even when the type is semantically unchanged — acceptable given a
  stable annotation encoding.

- **Annotation keying** — **export name** (confirmed). Node ids are offset in
  pass 1 and renumbered on export dedup; names are the stable cross-boundary
  identity the whole format rests on. Export names are already unique per module.

- **Symbol-atom vs export namespace** — **shared**, and already so by
  construction. In [skit-handles.adb](../../src/skit-handles.adb), `Bind (Name :
  String)` and symbol interning both route through one `To_Symbol_Object` → the
  same `Map`/`Vector`, the same `Primitive_Variable_Payload` range. A runtime
  symbol atom `foo` and the exported binding `foo` are the same object and must
  collapse on re-intern, or a symbol used in code and the root it names would
  split identity. The former overlap risk (compiler `To_Variable_Object` sharing
  the symbol range) is **gone**: `To_Variable_Object`/`Variable_Index` were
  removed from [skit.ads](../../src/skit.ads) — raw lambda variables are never
  written to Skit, and the representation can no longer express them.

- **Opcode range (64..4095) in dumps** — **fully retired to named imports**;
  only the VM-fixed combinators (1..9, plus `Suspension`) are baked, pinned by
  the header. `Primitive_Function` payloads are `64 + slot in this build's Prims
  vector` — a registration-order-dependent, build-specific index, exactly what
  the naming-over-numbering driver forbids. Resolution is load-time only, so
  "keep a hot core literal" buys no runtime speed. For this to survive re-dump,
  see the reloc-table provenance rule under **Two-pass load** — provenance rides
  the persistent reloc table, so no reverse map is ever needed.

- **Streaming large blobs** — **deferred, but the format reserves it now.**
  Materializing whole is fine until a genuinely large blob exists (YAGNI). The
  length frame is sized for it today (64-bit / varint, documented as
  back-patchable) so a future counting-sub-stream writer just back-patches the
  slot — no format-version bump. A narrow 16/32-bit length would force a breaking
  change later, so it is avoided.

## Consequences

To be recorded once the format is implemented. The public interface in
[skit.ads](../../src/skit.ads) is expected to stay stable; new surface is
additive — a `Foreign_Object_Interface` (`Class_Name`/`Serialize`/`Visit`/
`Free`/`Image`), a per-machine object-class factory registry keyed by class
name, `Bind_Object`/`Unpin` and a foreign-object registry with a mark flag,
collector integration (inline marking + Free sweep), and image read/write entry
points. Name resolution reuses the existing name → object binding
(`Bind`/`Lookup`); no resolver API is added.
Compiler-side, `foreign import` references must be emitted as named import slots
rather than baked opcode objects (decision B). This ADR depends
on ADR 0001 only through the header's word-size/tag-layout fields, which exist
precisely so the two decisions can move independently.

**Naming note — this ADR describes intended state, not current code.** The
`Skit.Environment` package with `Bindings`/`Blobs` maps, `skit-impl-memory.ads`,
and `Skit.Interfaces` named in Context/Decision **do not exist yet**. Today the
environment is a payload-keyed `Environment_Maps` inside
[skit-machines.ads](../../src/skit-machines.ads), fronted by a `String → Object`
intern layer (`Map`/`Vector`) in [skit-handles.ads](../../src/skit-handles.ads);
the heap is [skit-memory.ads](../../src/skit-memory.ads). There is **no foreign-
object storage of any kind** — the whole mechanism (`Foreign_Object_Interface`,
factory registry, the registry vector + mark flag, collector integration,
unknown-class skip) is greenfield, as is the `Foreign_Object_Payload` range and
`Is_Foreign_Object` predicate in [skit.ads](../../src/skit.ads). The existing
`User_Data_Interface` ([skit.ads](../../src/skit.ads)) is a *different* thing (a
single per-machine host context for primitive callbacks), not to be confused with
the many-instances `Foreign_Object_Interface`. "Resolution order step 2
(`Environment.Lookup`)" maps concretely onto `Handle.Lookup (Name : String)`.
These names should be reconciled when the format is implemented.
