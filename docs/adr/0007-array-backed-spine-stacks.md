# ADR 0007: Array-Backed Spine Stacks

- **Status:** Rejected (2026-07-13) — implemented and measured ~26% slower; see Outcome.
- **Date:** 2026-07-13
- **Deciders:** Fraser Wilson

## Outcome (2026-07-13)

Implemented and benchmarked against the cons-list stacks on the main branch,
workload `print (sum [1..10000])`, `-O3 -gnatp`, with `push`/`pop` inlined and
the stacks as **raw `Object` arrays** (no container overhead; 6 total
allocations across the run, max depths 20 / 34 / 20012):

| stacks | wall-clock (real) |
|--------|-------------------|
| array-backed (this ADR) | 4.53 s, 4.29 s |
| cons-list (main) | 3.43 s, 3.39 s, 3.62 s |

The array version is **~26% slower**. **Rejected; do not merge; keep the
cons-list stacks.**

### Why the profile misled

The profile that motivated this ADR showed the array version eliminating
`skit__memory__append` (16% → 0.05%) and GC (11% → 0.26%). Both real — and both
irrelevant to wall-clock, because:

- **Cons "allocation" was never `malloc`.** `Append` bump-allocates into the
  pre-allocated Core (`Core (Free) := (V, S); Free := Free + 1`) — two writes and
  an increment, about the cost of an array store. The 16% was frequency, not
  per-op expense.
- **Cons stacks are cache-local.** Stack cells live *in the Core, interleaved
  with the graph nodes being reduced* — the same lines the reducer already
  touches. The array stacks are a second, cold region; the `sum` spine alone
  reaches ~20 000 entries (~80 KB) walked constantly, adding cache misses per
  push/pop that outweigh the eliminated allocation.
- **Percentages describe distribution, not the counterfactual.** "27% in
  alloc/GC" said where cycles went, not what replacing that machinery would
  cost. The replacement cost more than it saved.

The lesson: a wall-clock A/B is the only arbiter; profile share does not predict
the cost of removing a component. Confirm with timing before committing to a
structural change like this one.

### What this does not overturn

The GC/frequency-vs-volume finding recorded below stands (allocation is a cheap
in-Core bump; the parked static-region idea has no prize to chase). Unrelated
wins made during this investigation are independent and retained. The remaining
evaluation cost is a tight reduction loop bound by graph access and `Object`
tag decode, which points at object representation
([ADR 0001](0001-object-representation.md)), not the stacks.

## Context

A DWARF-unwound `perf` profile of pure evaluation — `print (sum [1..10000])`
run through `leander --main`, with sampling dominated by the reduction rather
than compilation — puts **`skit__machines__evaluate` at 93.5%** of runtime
(Prelude compilation is 3.6%). The eval time breaks down as a stack-machine hot
loop:

- **spine push/pop ≈ 50%+** — `eval_combinator` 50%, of which push variants sum
  to ~42% (`push` 15.0%, two `push__2` at 13.0% and 14.1%), plus pops/top ~15%;
- **cell allocation (`skit__memory__append`) ≈ 16%**;
- **garbage collection ≈ 11%** — `skit__memory__gc → move`, the copying
  collector.

These three are one phenomenon. The four spine registers — `Stack`, `Control`,
`Dump`, `Secondary_Stack` ([skit-machines.ads](../../src/skit-machines.ads)) —
are **heap cons-lists**. Each register holds either `Nil` or an application
cell `App (top_value, rest)`:

- `Push` ([skit-machines.adb](../../src/skit-machines.adb)) is
  `S := Apply (Value, S)` — it **allocates a scaffolding application cell on
  every push**, and that allocation can trigger a collection.
- `Pop` reads `Left (S)` and advances `S := Right (S)`.
- The collector roots the four registers and walks each spine as ordinary heap
  graph.

The *entries* on these stacks are `Object` pointers into the real program graph.
The cons cells that link them are **pure scaffolding** — they exist only to
represent "a stack." That scaffolding is what the profile is burning on:
allocating a cell per push, and then collecting it.

[ADR 0003](0003-machine-is-a-pure-combinator-evaluator.md) already anticipated
this and deferred it:

> *"An array-backed stack representation (replacing the heap cons-cell stacks)
> would recover evaluation's ~11% GC overhead ... inlining the push/pop hot
> path. This is a separate, minor optimisation and is out of scope for this
> ADR."*

The profile shows it is neither minor nor merely a GC concern: push, allocation,
and GC together are the majority of evaluation, and most of it is spine
scaffolding. This ADR commits to the change.

## Decision drivers

- **The cost is measured and concentrated.** ~50% push + ~16% append + ~11% GC,
  a large share of which is spine cons cells that carry no program meaning.
- **Push should not allocate.** A push is conceptually an index bump; making it a
  heap allocation that can fire the collector is the root inefficiency.
- **GC frequency is inflated by the stacks.** Because every push allocates into
  the GC'd core, the spine drives collections. Remove spine allocation and the
  collector fires only for genuine graph construction.
- **The graph is unaffected.** Only the *linking* cons cells are scaffolding; the
  application nodes the stack points at, and lazy update-in-place on redex roots,
  are real graph and stay in the heap.

## Decision

Replace the four cons-list spine registers with four **growable `Object` arrays
plus top indices**. Stack entries (graph-node pointers) are stored directly in
`array (Top)`; the linking cons cells are eliminated.

- `Push` → `A (Top) := V; Top := Top + 1` — no allocation, never triggers a
  collection.
- `Pop` → `Top := Top - 1; return A (Top)`.
- empty / non-empty → `Top = 0` / `Top > 0`.

`R` (the 15 combinator-argument registers) and `Environment` are unchanged.

## Scope of work

1. **Machine state** ([skit-machines.ads](../../src/skit-machines.ads),
   the `Instance` record): replace `Internal : Internal_Register_Array` with,
   per register, an `Object_Array` (heap, grown by doubling) and a
   `Top : Natural`.
2. **Stack primitives** ([skit-machines.adb](../../src/skit-machines.adb)):
   rewrite `Push`, `Push (reg)`, `Pop`, `Pop (reg)`, the multi-`Pop (Args)`, and
   `Stack_Empty` as index operations. Mechanical.
3. **GC roots**: replace the `for X of This.Internal loop Mark` root pass with,
   per stack, `for I in 1 .. Top loop Mark (A (I))`. Scan **only** live slots
   (≤ `Top`); slots above `Top` are stale and must not be scanned (no need to
   clear them).
4. **Evaluator cons-threading — the bulk and the risk.**
   `Evaluate_Application` and its helpers (`Collect_Result`,
   `Advance_Primitive`, `Eval_Suspension`, `Eval_Primitive`, `Top`, `Pop (Args)`)
   read the registers *as cons cells*. Translate each site:
   - `Is_App (Internal (reg))` / `= Nil` → `Top (reg) > 0` / `= 0`;
   - `Left (Internal (reg))` (peek top) → `A (reg) (Top)`;
   - `Internal (reg) := Right (Internal (reg))` (drop) → `Top := Top - 1`;
   - `Right (This.Pop (reg))` and similar stay — that is `Right` of a *popped
     value* (a graph node), ordinary heap access, unaffected.
5. **Growth policy**: an initial capacity (spines can be deep — a few thousand)
   and double-on-overflow. Growth is a rare O(n) realloc, decoupled from GC.
6. Delete the per-push `Apply (Value, S)` allocation.

## The subtle part

The evaluator relies on a **"pushed ⇒ rooted across any collection"** invariant:
several hand-tuned sites park a value on a stack specifically so a collection
firing during a subsequent `Apply` cannot reclaim it (the parking comments in
`Eval_Primitive` and `Advance_Primitive`). That invariant **still holds** with
arrays — a slot at or below `Top` is a scanned root — but the mechanism moves
from "reachable through the heap spine" to "present in a scanned array slot."
Every parking site must be re-audited to confirm the value lands in a live slot
*before* any subsequent allocation. This is the one place a regression would
hide.

A beneficial consequence: because array push no longer allocates, a collection
can fire **only** during genuine graph-node construction (`Apply` building
application nodes, primitive results). Collection frequency drops, which is the
source of most of the recoverable GC time.

## State ownership

A natural objection: today all reduction state is uniform — `Object`s in the
Core cell heap, one address space, one collector — and array stacks appear to
move state "outside the machine." They do not, for three reasons:

- **The uniformity is already gone.** The machine is `Skit.Machines.Instance`,
  and it already holds non-Core state that the collector root-scans:
  `R : Object_Array (1 .. 15)` (the combinator-argument registers), the
  `Environment` map of bound objects, and the `Prims` vector. "The machine =
  the Core heap" was never true; it is "the Core **plus** registers,
  environment, and prims." The spine arrays are the *same pattern* the `R`
  registers already use — an in-`Instance` `Object_Array`, root-scanned — only
  larger and growable.
- **The boundary is ownership, not Core membership.** The spine arrays are
  fields of `Instance`, owned and root-scanned by the machine. Nothing leaves
  the machine. What leaves is the *value heap*: the Core becomes purely the
  program graph, and traversal scaffolding — which was never program data — no
  longer pollutes it. That is cleaner, not looser.
- **The serialization concern is moot.** The one reason to keep all state in the
  Core is a uniform image dump ([ADR 0002](0002-external-skit-representation.md)).
  But stacks are scaffolding holding pointers *into* the Core graph; at rest
  (between top-level evaluations) a stack holds at most a single root pointer to
  a result whose graph lives in the Core. A dump serialises the Core rooted at
  that pointer regardless of how the stack is represented — and a full state
  dump already cannot be taken from the Core alone, since `R` and `Environment`
  live outside it too.

If a single memory owner is nonetheless wanted, the arrays may be allocated as a
machine-owned slab (managed by `Skit.Memory` / the machine) rather than ad-hoc
`new`; same index-bump performance and GC decoupling, one owner for all machine
memory. Either placement keeps the arrays inside `Machines.Instance`.

## Options considered

### A. Keep heap cons-list stacks

Status quo. Rejected — the profile shows this is the dominant evaluation cost,
not a minor one.

### B. Fixed-size arrays

Simplest, but a fixed cap either wastes memory or overflows on deep spines.
Rejected in favour of doubling growth.

### C. Growable arrays with doubling (proposed)

O(1) amortised push, no per-push allocation, no GC coupling, and no depth cap in
practice. Chosen.

## Verification

The rooting invariants are delicate, so behavioural equivalence must be proven,
not assumed:

- Full self-test and integration suites green.
- A **deliberately tiny `Core_Size`** run to force frequent collections and
  stress the rooting invariant on every parking site.
- A GC-trace and allocation-count diff on `sum [1..N]` before/after, confirming
  spine allocation and collection frequency fall.
- Re-run the DWARF eval profile; confirm push/append/GC collapse.

## Garbage collection: frequency vs volume, and a parked follow-up

Total GC cost has two independent factors: **how often** a collection fires
(frequency, driven by allocation rate) and **how much** each collection copies
(volume, since the copying collector's cost is O(live data), not O(garbage)).
This ADR attacks **frequency**: array pushes stop allocating, so the spine no
longer fills the core and drives collections. That gain applies at every heap
size.

An empirical measurement clarified the volume side and led to parking a second
idea. Instrumenting the collector to classify surviving cells as "also survived
the previous collection" (old) vs "allocated since" (young) gave:

- **~90% old** at small heap sizes, but the ratio is heap-size dependent;
- it falls and plateaus at roughly **40% old / 60% young-live** as the heap
  grows and collections become infrequent.

The heap-size dependence shows the "survived the previous GC" test is a
**frequency artifact**, not a reliable measure of immortality: with a small
nursery little young data accumulates between collections, so survivors look
almost entirely old. The honest steady-state composition of the live set at
collection time is ~40% long-lived (dominated by the installed graph, which is
effectively immortal — see [ADR 0003](0003-machine-is-a-pure-combinator-evaluator.md))
and ~60% a genuine transient working set that reduction produces and that must
be copied. A larger heap *defers* collecting that working set but does not
remove it.

That reopens an optimisation — **segregate the immortal installed graph into a
non-collected region so the collector stops re-copying it** — whose ceiling is
therefore about 40% of per-collection copy volume at realistic heap sizes (more
under memory pressure). It is **parked**, deliberately, for two reasons:

1. It attacks *volume*, the secondary factor; this ADR attacks *frequency*, the
   universal one. Frequency should be reduced first.
2. Its true payoff is only measurable *after* this ADR lands: the prize is
   (remaining GC copy time) × (immortal fraction), and this ADR changes the
   first term.

When picked up, it should be gated on two measurements: re-profiling GC cost
after this ADR, and a **provenance cross-check** — count survivors below the
install high-water mark. If that ≈ the 40% old fraction, the immortal set is the
installed graph and an install-watermark static region (non-moving, scanned as
roots for old→young pointers) captures it cleanly and leak-free; if it is much
less, evaluation itself creates long-lived data and a real generational scheme
(age ≥ N tenuring plus a periodic major collection) would be needed instead.
Either way it is a separate ADR, not this one.

## Consequences

Measured, not predicted (this ADR was implemented and then rejected — see
**Outcome** above):

- Push *did* become an index bump and allocation/GC *were* eliminated from the
  profile (`append` 16% → 0.05%, GC 11% → 0.26%) — yet evaluation ran **~26%
  slower** in wall-clock. The predicted "large gain" was wrong.
- The eliminated cost was cheap and cache-local to begin with (in-Core bump
  allocation, stack cells interleaved with the working graph); the array
  replacement added a cold second memory region and lost more to cache misses
  than it saved.
- The cons-list stacks on main are retained. The array branch is not merged.
- The intended SPARK simplification does not materialise, because the design is
  rejected on performance grounds first.

The durable takeaway for future perf work: **validate structural changes with a
wall-clock A/B before committing.** Profile share identifies where time is
spent; it does not predict the cost of removing a component, and here it pointed
the wrong way.
