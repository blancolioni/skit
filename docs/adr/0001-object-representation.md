# ADR 0001: Skit Object Representation

- **Status:** Deferred (2026-07-15) — option C (NaN-boxed 64-bit) is the target
  representation and its cost is now measured (bounded, memory-bound, ≈ +10–20%
  wall at the small-heap regime the reduction fixes make optimal). But the
  correctness prize is unrealized until the numeric tower exists, so adopting C
  now would pay ~10% on every (float-cold) workload for a benefit nothing can
  use yet. Stay on 32-bit; revisit at numeric-tower time.
  **Revisit trigger:** numeric-tower implementation. At that point the float
  correctness benefit becomes real; measure the tower's actual float density and
  re-check option B (heap-boxed floats) against it before committing to C — if
  floats are rare, B's alloc-storm failure mode never fires and the 32-bit
  hot path survives. The stable public interface (`To_Object`, `Is_Integer`,
  `To_Integer`, `Is_Application`, `To_Variable_Object`) keeps the switch a
  bounded, mechanical rework, so deferring costs nothing built now.
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

### D. Choose A or C at build time (added 2026-10-10)

Keep both the 32-bit word and the NaN-boxed word, and let the embedding
project choose. An Alire crate-configuration variable
(`Object_Representation`, `Tagged_32` by default or `NaN_Boxed_64`) reaches
`skit.gpr` through the generated config, which picks a source directory holding
that representation; `Object` is completed in `skit.ads`'s private part as a
type derived from it. A project chooses with one line in its own
`alire.toml`.

- Pro: it fits this ADR's dilemma directly. A float-heavy embedding gets IEEE
  doubles, and everything else keeps the cache-tight 32-bit hot path; nobody
  pays for the other's choice.
- Pro: images are already safe across builds: an image records its word size
  and tag layout, and the reader rejects a mismatch.
- Con: two of everything in CI. Build, tests and the SPARK proof run per
  representation, with a recorded proof session each (ADR 0004). The
  collector's proof is shared; root `Skit`'s differs.
- Con, and the lasting one: the same Haskell program can behave differently
  depending on how skit was built. `Int` wraps at 30 bits in one and not the
  other, and `Double` loses precision in one and not the other. leander's
  suites would have to run both ways, with per-variant expectations where
  overflow or precision shows.
- Prerequisite, shared with C: representation details must not leak outside
  the representation's own code. Today they do (combinators handled by payload
  number in `Skit.Machines`, `Skit.Debug` and the image format; maps keyed by
  `Object_Payload`); skit#34 closes those leaks without changing behaviour.

Not before C itself: until the numeric tower gives C a reason to exist, the
second representation would have no users. When C is adopted, D is the
cheaper of the two ways to do it if the float-density measurement (see
Leaning) shows floats matter to some embeddings but not most.

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

## Cost measurement (post-fix A/B)

The 2× cost was measured directly, both representations built with the current
reduction fixes in place — the knot-tying `Y` combinator and the redex-update
change that overwrites a redex root with its result's contents instead of an
`App (I, .)` indirection. Those fixes matter here: they cut the live working set
to a small, roughly-constant size and made a *small* heap the optimal operating
point, which is the regime the representation must be judged in.

Workload: `print (sum [1..10000])` (= 50005000), core-size swept 512 … 32768,
min of the reported run. **Every non-timing metric is byte-identical across the
two formats at every core size** — allocated cells (12,527,114), GC count
(60/26/12/6/3/1/0), copied cells, static/transient split, `Static<-young`
writes. The object format changes cell *size* and per-access cost only; it does
not touch the graph, the work, or the collector's behaviour. So this is a clean
apples-to-apples where representation is the sole variable.

Wall-clock (`real`, seconds) and OS memory time (`sys`, seconds):

| core-size | cells | 32-bit real | NaN real | NaN penalty | 32-bit sys | NaN sys |
|-----------|-------|-------------|----------|-------------|-----------|---------|
| 512   | 262 K  | 0.323 | 0.353 | +9%  | 0.033 | 0.042 |
| 1024  | 524 K  | 0.295 | 0.356 | +21% | 0.050 | 0.052 |
| 2048  | 1.0 M  | 0.304 | 0.322 | +6%  | 0.051 | 0.054 |
| 4096  | 2.1 M  | 0.350 | 0.331 | −5%  | 0.060 | 0.076 |
| 8192  | 4.2 M  | 0.328 | 0.377 | +15% | 0.078 | 0.120 |
| 16384 | 8.4 M  | 0.364 | 0.475 | +30% | 0.107 | 0.201 |
| 32768 | 16.8 M | 0.458 | 0.711 | +55% | 0.193 | 0.423 |

Findings:

- **The penalty is memory-bound, not compute-bound.** `user` CPU is close and
  roughly flat across heap sizes (both ≈ 0.25–0.30 s); the 64-bit mask+compare
  tag dispatch vs. a bitfield read is a small, size-independent tax. Reported
  `Evaluation` time is comparable and noisy (both ≈ 90–130 ms, falling as the
  heap grows and GC fires less).
- **The divergence is entirely `sys` time, and it scales with heap size** —
  ≈ 1× at small heaps, 1.9× at 16384, 2.2× at 32768. This is the 2× cell
  footprint (16-byte NaN cells vs. 8-byte packed) realised as OS paging: twice
  the semispace to fault in and zero. It is the predicted bounded cost, not a
  cache cliff or a compute regression.
- **The fixes put the penalty in its cheapest regime.** Both formats peak at a
  *small* heap (512–2048) and *degrade* past ~8192 on `sys`/cache; a large heap
  now buys nothing because the live set is small. Operated where it should be —
  a small default heap — the NaN penalty is ≈ +6–21% wall. The ugly +55% appears
  only under gratuitous oversizing, which the fixes remove any reason for.

Net: 32-bit is consistently faster, cheaply (footprint), and correct *for
float-cold workloads*. NaN-boxing's justification is float **correctness**, not
speed; this quantifies its price as ≈ +10–20% wall at sensible heap sizes,
rising to 2× `sys` / +55% wall only when the heap is oversized. Keeping the
default heap small (now free) keeps the footprint penalty in its smallest
regime.

## Leaning

Option **C (NaN-boxed 64-bit)** as the target, but **not yet**. The cost — the
only open risk — is now measured (see above): a bounded, memory-bound ≈ 2× worst
case that stays around +10–20% wall in the small-heap regime the fixes make
optimal. Correctness is decisive and makes C the eventual choice; the price is
acceptable and predictable *once there is a benefit to weigh it against*.

Adoption is deferred to numeric-tower time (see Status). Until the tower exists
every workload is float-cold, so C would cost ~10% for nothing realisable. Two
things to settle at the tower, in order: (1) measure the tower's float density —
if floats are rare, reconsider option B (heap-boxed floats), whose only failure
mode is float-heavy loops that a rare-float tower never triggers, keeping the
cache-tight 32-bit hot path; (2) if float usage is real and hot, commit to C.

## Interaction with the SPARK proof (added 2026-10-10)

Since this ADR was deferred, [ADR 0004](0004-adopt-spark-for-the-memory-core.md)
has been implemented: root `Skit` and `Skit.Memory` are proved with GNATprove,
and CI replays the proof on every pull request. That does not change when to
adopt option C. It does change what adopting it involves. ADR 0004 suggested
proving against the final representation so that the proof is written once;
it was proved against the 32-bit word instead, so option C means re-proving
part of it.

- **The collector's proof should carry over.** `Skit.Memory` sees an `Object`
  only through `Is_Application`, `Payload`, `Application` and `Cell_Address`.
  Its invariants (`Valid`, `Collecting`, `Counted` and the counting lemmas)
  mention nothing else, so if option C keeps that interface, as this ADR
  already intends, they stay as they are.
- **Constraint: keep `Cell_Address` a small type of its own**, rather than
  widening it to the NaN payload. `Valid`'s non-wrapping bounds,
  `Count_Forwarded` and `Lemma_Room` rely on addresses fitting in `Natural`. A
  30- or 32-bit cell index fits easily in the payload, so the heap limit need
  not change.
- **Root `Skit` is re-proved, and that is where the proof pays.**
  - Tag predicates and constructors become mask-and-compare expression
    functions on a private `mod 2**64` word. SMT provers handle that kind of
    arithmetic well, so they should re-prove much as they did in ADR 0004's
    stage 3.
  - The central risk of option C, a genuine float colliding with the boxed
    space, becomes provable. Nothing can be proved about the bits of an
    arbitrary `Long_Float` (the conversion is opaque to the prover), but the
    canonicalisation in `To_Object (Long_Float)` can be written on the integer
    word: if the exponent is all ones and the mantissa nonzero, store the
    reserved canonical NaN. A postcondition that the result is a float and no
    other kind of object then holds for all 2^64 patterns.
  - `To_Object (Integer)` stops wrapping once integers have at least 32 bits of
    payload, so its round-trip postcondition becomes unconditional (and Haskell
    `Int` in leander stops wrapping at 30 bits). The precondition that a
    `Long_Float` fit in a `Float` goes away. `To_Float` stays outside SPARK:
    SPARK does not accept a float as the target of an unchecked conversion.
- **The rule that boxed non-floats are only moved, never used in float
  arithmetic** is enforced by typing in the proved units, where `Object` is a
  modular word. In `Skit.Machines`, which is not proved, it stays a coding
  rule.
- **When it lands**, CI's proof replay will fail, because the proof obligations
  change. Re-run the full proof and commit the new session
  ([proof/README.md](../../proof/README.md)); a check that no longer proves is
  a real problem with the new representation. The image format records word
  size and tag layout, so it needs a version bump, and the image reader's
  representation-specific checks (#30) change with it.

## Open questions

- ~~Measure the real 2× memory / cache impact on representative workloads before
  committing.~~ **Resolved** (see Cost measurement): memory-bound, ≈ 2× `sys` at
  large heaps, ≈ +10–20% wall at the small heaps the reduction fixes make
  optimal. No cache cliff.
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
