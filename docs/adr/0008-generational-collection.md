# ADR 0008: Generational Collection with a Non-Moving Static Region

- **Status:** Proposed
- **Date:** 2026-07-13
- **Deciders:** Fraser Wilson

## Context

Evaluation on `print (sum [1..10000])` is garbage-collector-bound at small and
medium heap sizes. A core-size sweep (`--core-size`, `--report`) measured, per
collection, how many surviving cells were copied and how they split between
**static** (survived a prior collection — `Address < Static_Top`, maintained in
`After_GC`) and **transient** (allocated this epoch):

| core-size | cells | eval ms | GCs | copied/GC | static | transient |
|-----------|-------|---------|-----|-----------|--------|-----------|
| 1024  | 524K | 12046 | 2464 | 387600 | 387599 | 1 |
| 2048  | 1.0M | 5646  | 444  | 491233 | 490778 | 455 |
| 4096  | 2.1M | 3643  | 177  | 490431 | 489149 | 1282 |
| 8192  | 4.2M | 3396  | 81   | 490970 | 487985 | 2985 |
| 16384 | 8.4M | 3600  | 38   | 485598 | 479165 | 6433 |

Two facts stand out:

1. **~99% of every collection's copy work is re-copying immortal cells.** The
   live set is dominated by the installed graph (Prelude + program, see
   [ADR 0003](0003-machine-is-a-pure-combinator-evaluator.md)); transient live
   is 1–6000 cells. Total copy work (GCs × copied) is 955M → 218M → 87M → 40M →
   18M as the heap grows — the sole reason a bigger heap runs faster is that it
   re-copies the immortal set fewer times.
2. **A bigger heap hits a cache wall.** Eval bottoms out at ~4M cells (3396 ms)
   and *rises* at 8.4M despite fewer collections: the larger semispace is
   colder.

A copying collector's cost is O(live copied), and here ~all of that is immortal
data that never dies. The obvious response — stop copying it — was gated on one
risk: **old→young references.** A non-moving static region must still find any
static cell that points into the collected nursery, or it will reclaim live
young cells. If those references are numerous, a nursery collection must scan
the whole static region every time, recovering little.

That risk was measured directly. A write barrier in `Set_Left`/`Set_Right`
counted stores of a young (this-epoch) application pointer into a static cell —
exactly the remembered set a generational collector would maintain:

| core-size | static←young writes, max/epoch |
|-----------|-------------------------------|
| 1024  | 20 |
| 2048  | 19 |
| 4096  | 15 |
| 8192  | 15 |
| 16384 | 15 |

**~15–20 per epoch, flat across heap sizes** — against ~490K static cells
copied per collection. The remembered set is ~0.003% of the copy volume it would
eliminate. The old→young risk is empirically negligible: reduction overwrites
redex roots that are almost always young (created and updated within one epoch);
updates to a *survived* cell are rare.

## Decision drivers

- **The prize is measured, not estimated.** ~99% of GC copy work is re-copying
  immortals; the mechanism to skip it (a remembered set) costs ~15 scans per
  collection. Both numbers are direct measurements on the target workload.
- **It dissolves the heap-sizing tradeoff.** The cache wall exists only because
  a large heap was needed to amortise re-copying the static set. Remove the
  re-copy and a small, cache-hot nursery collected frequently becomes viable —
  potentially faster than the current 3396 ms floor, which the copying collector
  cannot get under.
- **Half the infrastructure exists.** `Static_Top` (the static boundary),
  the static/transient classification in `Move`, and the write-barrier hook are
  already in the tree; what remains is making the static region non-moving and
  turning the barrier counter into a real remembered set.

## Options considered

### A. Keep the two-space copying collector, tune heap size

Status quo. The sweep shows this tops out at ~3396 ms against a cache wall, with
~99% of collector effort wasted on re-copying immortals. Rejected as the
endpoint.

### B. Non-collected static region, scan it wholesale each GC

Segregate immortals; at each nursery collection scan the entire static region
for old→young pointers instead of copying it. Scanning is cheaper than copying
(read-only, no allocation) but still O(static) ≈ 490K per GC — only ~2×. Given
the remembered-set measurement makes wholesale scanning unnecessary, rejected in
favour of C.

### C. Generational: non-moving static + nursery + write-barrier remembered set (proposed)

Immortals live in a non-moving static region; only the nursery is collected;
a write barrier records the ~15/epoch old→young pointers so a collection visits
only those, not the whole static region. Captures ~99% of the copy saving with
~0.003% overhead. Chosen.

## Decision

Adopt Option **C**. Restructure the collector into a **non-moving static region
plus a copied nursery**, with a **write barrier and remembered set** for
old→young references.

### Heap layout

Replace the two equal semispaces with three regions in the Core:

```
[ Static (non-moving) | Nursery semispace A | Nursery semispace B ]
```

- **Static** `[0 .. Static_Top)` — immortal cells; never moved, never copied.
- **Nursery** — two semispaces above `Static_Top`; the mutator bump-allocates
  into the active one; a collection flips A/B and Cheney-copies live young cells.

### Collection

A nursery collection:

1. Forward the machine roots (registers, environment, stacks — the existing root
   set) that point into the nursery.
2. Forward the **remembered set** — the static cells recorded as holding a young
   pointer this epoch. Scan only those, not all of static.
3. Cheney-scan the nursery to-space, forwarding children.
4. Static cells are neither moved nor scanned except via (2).

Cells that survive are **promoted (tenured) into the static region** by
`Static_Top` advancing over them — the transient-copied count (1–6000/GC) is the
tenuring rate; static grows slowly. This reuses the existing meaning of
`Static_Top` (the survived-a-collection boundary) — it becomes the static/nursery
partition rather than a mere classification watermark.

### Write barrier and remembered set

The instrumentation barrier becomes functional: when `Set_Left`/`Set_Right`
stores a young application pointer into a static cell, record that static cell
in a **remembered set** (a small array/set of `Cell_Address`; the measured size
is ~15–20, so a fixed small buffer with overflow-to-scan fallback suffices). The
set is consumed as roots in step (2) and cleared each collection (in
`Before_GC`). Writes where both ends are static, or the target is young, are not
recorded.

### Identifying static

`Static_Top` already provides it, and the choice of how it advances is the one
open tuning question:

- **Survival (current):** `After_GC` sets `Static_Top := Free`, so every
  survivor tenures after one collection. Simple; matches today's code; risks
  tenuring medium-lived cells into a region that (in this design) is not
  collected — a potential leak for workloads with long-but-not-immortal data.
- **Install provenance:** set `Static_Top` once at end of install and tenure
  more conservatively (age ≥ N) thereafter. Leak-free for the installed graph;
  needs a periodic major collection of the static region if eval creates
  immortal data.

For this workload the two coincide (static ≈ installed graph). Start with the
existing survival rule; add a major (whole-heap) collection as a safety valve so
tenured garbage cannot accumulate unboundedly.

## Verification

- **Correctness under GC stress:** full self-test and integration suites at a
  deliberately tiny nursery to force frequent collections; the remembered set
  and promotion must preserve every reachable cell.
- **Remembered-set sizing holds:** confirm max/epoch stays small on other
  workloads (not just `sum`), especially ones with more sharing/laziness; if it
  grows large, revisit (fall back to Option B's wholesale scan for that regime).
- **Wall-clock A/B** (standing rule since [ADR 0007](0007-array-backed-spine-stacks.md)):
  same-input timing before/after, across core sizes, including a small nursery to
  test whether it beats the current 3396 ms floor.
- **Major-collection safety valve** actually reclaims tenured garbage (a
  workload with long-but-mortal data).

## Consequences

To be recorded once implemented. Expected:

- Per-collection copy work falls ~99% (≈490K → transient + ~15 remembered);
  the ~12% GC cost at realistic heap sizes largely disappears.
- The heap-sizing cache wall is removed: a small, cache-hot nursery with frequent
  cheap collections becomes the preferred configuration, and the non-moving
  static graph stays cache-warm with stable addresses.
- The collector gains a remembered set and a write barrier on the two setters —
  a small, measured cost on the mutator's graph-update path.
- A non-moving static region with stable addresses is also friendlier to a future
  SPARK effort ([ADR 0004](0004-adopt-spark-for-the-memory-core.md)) than a
  fully-copying heap, and it retires the static-region follow-up parked in
  [ADR 0007](0007-array-backed-spine-stacks.md) by implementing it.
