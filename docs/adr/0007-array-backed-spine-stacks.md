# ADR 0007: Array-Backed Spine Stacks

- **Status:** Proposed
- **Date:** 2026-07-13
- **Deciders:** Fraser Wilson

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

## Consequences

To be recorded once implemented. Expected:

- Push becomes an index bump; the per-push application-cell allocation and the
  collections it drove are gone.
- The ~50% push + ~16% append + ~11% GC of evaluation shrinks substantially, the
  largest gains on allocation-heavy workloads.
- The collector fires only for real graph construction, not for stack traffic.
- The spine-rooting mechanism moves from heap reachability to array-slot scanning
  — a smaller, more explicit root set, which also simplifies reasoning for a
  future SPARK effort ([ADR 0004](0004-adopt-spark-for-the-memory-core.md)).
