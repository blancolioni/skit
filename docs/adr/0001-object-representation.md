# ADR 0001: Skit Object Representation

- **Status:** Investigating
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

## Leaning

Option **C (NaN-boxed 64-bit)**, subject to validating the cost.

## Open questions

- Measure the real 2× memory / cache impact on representative workloads before
  committing.
- Represent Haskell `Float` (32-bit) and `Double` (64-bit) as distinct types,
  or promote everything to `Double` internally?
- Confirm bit-pattern preservation across the GNAT `Long_Float` code paths
  actually used (moves vs. any incidental arithmetic).
- Tag-bit layout within the NaN payload; choice of the reserved canonical NaN.

## Consequences

To be recorded once a decision is reached. The public interface in
[skit.ads](../../src/skit.ads) (`To_Object`, `Is_Integer`, `To_Integer`,
`Is_Application`, `To_Variable_Object`) is intended to remain stable so callers
are unaffected regardless of the chosen representation.
