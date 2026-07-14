# ADR 0001: Skit Object Representation

- **Status:** Accepted (2026-07-14) — option C (NaN-boxed 64-bit) prototyped
  and measured; cost is the predicted bounded ~2× memory with a modest
  (1.1–1.34×) wall-clock hit, no pathology. Realising the float-correctness
  prize additionally requires fixing front-end float lowering (see Consequences).
- **Date:** 2026-07-04
- **Deciders:** Fraser Wilson

## Context

Skit stores every runtime value in a single, uniformly-sized `Object` word.
The current word is a packed 32-bit record: a 30-bit `Payload` plus a 2-bit
`Tag` (`Integer_Object`, `Primitive_Object`, `Application_Object`,
`Float_Object`). See [skit.ads](../../src/skit.ads) and
[skit.adb](../../src/skit.adb).

Consequences of the current layout:

- **Float is 30-bit.** `To_Object (Float)` stores `bits / 4`, discarding the
  two low mantissa bits; `To_Float` restores with `* 4`. A 32-bit IEEE-754
  single (23-bit mantissa) is silently truncated to a 21-bit mantissa. This
  violates the IEEE contract that Haskell `Float`/`Double` are expected to
  honour.
- **Integer is 30-bit** (±2^29).
- **Heap is 30-bit** — `Application_Object` payload indexes the cell array, so
  at most 2^30 cells = ~8 GB heap (cell = two 32-bit objects = 8 bytes).

Leander's purpose is to embed a Haskell subset as an extension language inside
Ada projects. The host workload is therefore unknown and unbounded: a
float-heavy numeric embedding is entirely plausible. A representation that is
correct only for float-cold workloads is not acceptable.

Skit is deliberately weakly typed — it relies on the compiler for typing — so
it needs few tags, and 30 bits of address space is ample. The tag/address
budget is not the constraint; **float precision is.**

## Options considered

### A. Status quo — 30-bit tagged float

Compact (8-byte cells, cache-tight hot path). But silently lossy float is a
semantic bug, not merely an aesthetic one. Rejected as a target.

### B. Heap-box floats

Keep the 32-bit tagged word; `Float_Object` payload becomes a cell address,
with a full 64-bit double stored in that cell.

- Pro: hot combinator/integer/application path stays 32-bit and cache-tight;
  full double precision.
- Con: **alloc-per-flop.** A float-heavy loop allocates a heap cell for every
  intermediate double, producing unbounded GC churn. The failure mode is
  pathological and workload-dependent — exactly what an
  unknown-embed-workload runtime must not ship.

### C. NaN-boxed 64-bit word (proposed)

Widen `Object` to 64 bits. Real doubles are stored raw. Non-float values live
in NaN payload space (exponent all-ones + nonzero mantissa gives ~51 usable
bits: 1 sign + 50 mantissa, less the bits spent on the tag). One canonical NaN
pattern is reserved for genuine float-NaN results so `0.0/0.0` does not collide
with the box space.

- Pro: full IEEE-754 `Double`, zero loss; float store/load become a raw
  reinterpret (cheaper than the current shift); float arithmetic is immediate
  and alloc-free (register-native); ample payload for integers and cell
  addresses; a distinct 32-bit `Float` can be boxed in the payload if desired.
- Con: every `Object` doubles 4→8 bytes, so cells go 8→16 bytes and stacks
  double — roughly 2× heap/stack footprint and 2× copy-GC bandwidth (GC
  *frequency* is unchanged, since cell *count* is unchanged). Tag dispatch
  moves from a bitfield read to a 64-bit mask+compare. sNaN discipline
  required: boxed non-floats may only be moved, never subjected to FP
  arithmetic, or the host FPU may quiet/canonicalize and corrupt the payload
  (SSE2 `MOV` preserves bit patterns; arithmetic does not). Ada rework:
  `Object` becomes a `mod 2**64` word, `Float`→`Long_Float` throughout
  including compiler-side literal codegen, and `-gnatVa` validity checks lose
  meaning on a raw modular type.

## Decision drivers

- **Failure-mode asymmetry.** NaN-box worst case (float-cold) is a bounded,
  predictable 2× memory cost. Heap-box worst case (float-heavy) is an unbounded
  alloc storm. An extension-language runtime cannot ship a pathological failure
  mode it cannot predict.
- **Correctness.** Silent mantissa truncation breaks the Haskell float
  contract; a general-purpose embedding must not lie about numeric results.
- **Precedent.** 64-bit heap words are standard for a Haskell RTS (GHC's heap
  is 8-byte-word based on 64-bit hosts). The current 32-bit cell is the unusual
  choice; 2× memory is normal sizing, not exotic.

## Decision

Option **C (NaN-boxed 64-bit)**. The cost was the only open risk; the prototype
measured it as acceptable (see Consequences).

## Open questions — resolved by the prototype

- **Real 2× memory / cache impact.** Measured (see Consequences): equal-cell-count
  A/B shows 1.11–1.34× wall-clock, worse the more GC-bound the run, consistent
  with a copy-GC-bandwidth cost. No cache cliff or pathological regime.
- **`Float` vs `Double`.** Promote everything to `Double` internally. The
  front-end already carries 64-bit literals (`Const_Float` is `Long_Float`);
  only the old 30-bit word truncated them. One `Float_Object` tag suffices, so
  the prototype keeps a single reserved NaN and no distinct 32-bit float type.
- **Bit-pattern preservation.** Confirmed safe: the machine core never does FP
  on Object words — `Evaluate` pushes floats inert, and arithmetic is isolated
  in primitives via a `To_Float`/`To_Object` round-trip. The hot path only
  *moves* floats (Push/Pop/Copy/GC), and moves preserve bit patterns, so sNaN
  quieting cannot corrupt a boxed payload in the collector.
- **Tag-bit layout.** A word is a box iff sign = 1, exponent = all-ones and the
  quiet bit (mantissa bit 51) is set — a negative quiet NaN, top 13 bits
  `0x1FFF`. That leaves bits 49..48 for a 2-bit tag (0 Integer, 1 Primitive,
  2 Application, 3 Float) and bits 47..0 for a 48-bit payload. The single tag-3
  value is the reserved canonical NaN; `To_Object` folds every float NaN
  (including the x86 `0.0/0.0` result, which would alias the box pattern) into
  it, and `To_Float` turns it back into a real NaN.

## Consequences

Option C was implemented as a prototype on branch
`7-evaluate-nan-boxing-to-store-values` and measured against the committed
32-bit representation.

- **Representation.** `Object` is a 64-bit word (`mod 2**64`). The public
  interface in [skit.ads](../../src/skit.ads) (`To_Object`, `Is_Integer`,
  `To_Integer`, `Is_Application`, `To_Variable_Object`) is unchanged, so
  Leander callers were unaffected; the record's `.Tag`/`.Payload` component
  reads became `Tag (X)`/`Payload (X)` accessors and aggregate construction
  became `Make_Integer`/`Make_Primitive`/`Make_Application` constructors across
  the Skit body and its children.
- **Correctness.** Full self-test passes (138/138). Under GC stress (a tiny
  nursery forcing hundreds of collections, with the ADR 0008 heap-integrity
  checks and from-space poisoning enabled) the collector is clean with 16-byte
  cells and NaN-boxed pointers. Both representations report identical GC counts
  and allocation counts on the same input — behavioural parity.
- **Cost (measured).** `sum [1..4000]`, min of 3 runs, equal cell count:

  | core (cells) | 32-bit baseline | 64-bit NaN-box | ratio | GCs |
  |--------------|-----------------|----------------|-------|-----|
  | 512 K | 1.90 s | 2.54 s | 1.34× | 456 |
  | 2048 K | 1.26 s | 1.40 s | 1.11× | 58 |

  Memory is 2× per cell by construction (8→16 bytes). The wall-clock hit scales
  with collection frequency, confirming the cost is copy-GC bandwidth, not a
  cache wall. This is the predicted bounded cost, far from the ~5× that the
  ADR 0008 generational experiment cost.
- **Float precision — prize not yet realised end-to-end.** The representation
  is lossless for `Double` by construction (raw reinterpret), but it cannot be
  demonstrated through the evaluator yet: the front-end coerces every float
  literal to `Integer` at
  [leander-core-literals.adb:98](../../../src/leander-core-literals.adb)
  (`Number (Integer (Float'Value (Image)))`), so doubles never reach Skit.
  Fixing that lowering is a prerequisite to gaining the correctness benefit and
  is tracked separately.
- **Integers widened 30→32-bit** as a side effect (the box payload is 48-bit;
  values round-trip through Ada's 32-bit `Standard.Integer`).
- **Validity checking.** `-gnatVa` treats a NaN `Long_Float` as invalid data,
  so `pragma Suppress (Validity_Check)` is applied within the `Skit` body only,
  where the float reinterpret paths live — the modular `Object` word has no
  invalid patterns, so validity checking is vacuous there regardless.
- **Instrumentation note.** On Windows `Ada.Calendar` reports GC time as 0 ms
  at this resolution; wall-clock (`time`) was used for the A/B above.
