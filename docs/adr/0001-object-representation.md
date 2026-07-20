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

## Open questions — resolved by the prototype

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

## Update (2026-07-15): recursion fix re-characterises the cost

The A/B cost table above was measured **before** the fixpoint-sharing fix, in the
O(n²) recursion regime — note the collection counts (456 GCs for `sum [1..4000]`
at 512 K cells). Recursion compiled to the pure combinator `Y = S S I (C B
(S I I))`, which rebuilds the `Y f` subgraph on every unfold instead of sharing
it, so any linear traversal re-reduced quadratically and allocated a torrent of
transient cells. `Y` is now a built-in knot-tying combinator (see
`Skit.Machines.Eval_Combinator`): it rewrites the redex root `App (Y, f)` in
place to a self-referential `App (f, self)`, so the recursive value is one shared
node. Traversal is now linear; `sum [1..5000]` fell from 269 GCs / 83 M allocated
cells to **16 GCs / 7.6 M cells**, and the live set is small and roughly
constant.

This does not change the decision (float correctness and the failure-mode
asymmetry that pick C over B are orthogonal to recursion sharing), but it
**inverts the cost characterisation**:

- The measured 1.11–1.34× NaN-box penalty was, by this ADR's own finding, copy-GC
  *bandwidth* — "the wall-clock hit scales with collection frequency." Collection
  frequency on these workloads is now ~10× lower, so that penalty largely
  evaporates on the same inputs. The honest post-fix A/B number is smaller and
  must be re-measured.
- The residual cost shifts to **cache density**, which the table above explicitly
  ruled out ("not a cache wall"). That was true in the GC-bound regime; post-fix
  the hot path is cache-bound. 16-byte cells (C) halve the cells-per-cache-line
  versus the 8-byte baseline, so the cache-pressure point arrives at roughly half
  the cell count. Observed on the current 32-bit build: sys time jumps
  0.05 s → 0.24 s at core-size 32768 (33 M cells @ 8 B) with no compute change;
  under C expect the same wall at ~16384.

Sizing consequence: the default heap was oversized to paper over the quadratic.
Post-fix the needed live set is small, so the default core-size should drop
regardless of representation; under C specifically, ~512 (current units) holds
the same memory and headroom that 1024 gives today.

Re-run the equal-cell-count A/B after the fix before quoting a cost number: the
GC-bandwidth figure in the table is now an overestimate, and the real remaining
question is the cache-density delta at the (smaller) heap sizes linear recursion
actually needs.
