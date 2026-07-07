# ADR 0004: Adopt SPARK for the Memory Core

- **Status:** Proposed
- **Date:** 2026-07-07
- **Deciders:** Fraser Wilson

## Context

Skit is a two-space copying garbage collector wrapped in an evaluator. The
collector is the part most likely to be silently wrong and most expensive to
debug: an off-by-one in a space bound, a cell reclaimed while still reachable,
or an `Object` payload used as a `Core` index without a proof it is in range,
all corrupt the heap in ways that surface far from the fault. This is exactly
the class of defect formal proof eliminates outright.

The machine's central design choice makes proof unusually tractable. Per
[ADR 0001](0001-object-representation.md), every runtime value is a single
packed word — a `Payload` plus a `Tag` — and `Application_Object.Payload`
indexes a flat `Cell_Array`. **There is no pointer graph.** The heap is an
array; references into it are modular integers. A copying collector over an
array of integer-indexed cells is a finite, value-semantics data structure —
the natural target for SPARK, and nothing like the aliased-pointer heaps SPARK
struggles with. See [skit.ads](../../src/skit.ads) and
[skit-impl-memory.ads](../../src/skit-impl-memory.ads).

Around that core, the crate is deliberately built on dispatching interfaces
(`Skit.Interfaces`, `Skit.Memory`, `Skit.Allocator`, `Skit.Machine`,
`Skit.Builder`, `Skit.Evaluator`, `Skit.Stacks`), each exposing a
`type Reference is access all Abstraction'Class`. The front-end IR
([skit-terms.ads](../../src/skit-terms.ads), per
[ADR 0003](0003-machine-is-a-pure-combinator-evaluator.md)) is a genuine
pointer structure on a custom `Root_Storage_Pool` arena. I/O lives in
`Skit.Console`, `Skit.Debug`, `Skit.Logging`, `Skit.Environment`, and
`Skit.Library`.

The question this ADR settles is not *whether Skit can be SPARK* but *which
Skit should be*.

## What blocks SPARK, and where

| Construct | Location | Verdict |
|-----------|----------|---------|
| General access-to-class-wide `access all Abstraction'Class` | every interface unit | Hard — SPARK's ownership model rejects general access; only pool-specific `access T` with move semantics is analysable. Dispatching itself is fine; the class-wide *access* is not. |
| Custom `Root_Storage_Pool`, `System.Address`, `System.Storage_Elements` | [skit-terms.ads](../../src/skit-terms.ads#L64-L99) | Hard — outside SPARK entirely. |
| Standard `Ada.Containers.Doubly_Linked_Lists` | [skit-impl-memory.ads:69](../../src/skit-impl-memory.ads#L69) | Medium — replaceable with `Formal_Doubly_Linked_Lists`, or liftable out of the proven record. |
| `Duration` timing (`GC_Time`) | [skit-impl-memory.ads:90](../../src/skit-impl-memory.ads#L90) | Easy — a wall-clock statistic, not machine state; mark ghost or move to an unproven stats record. |
| `Ada.Text_IO` and descendants | console, debug, logging, environment, library | N/A — no properties worth proving; stays out of scope. |

The blockers cluster entirely in the shell. The core is blocked only by
incidental coupling — a returned access `Reference`, a container field, a
timing field — not by anything intrinsic to the collector.

**Prerequisite:** the first hard blocker — general access-to-class-wide across
the interface tower — is removed by
[ADR 0005](0005-collapse-the-machine-interface-tower.md), which collapses that
tower to a concrete machine for reasons independent of proof. This ADR assumes
0005 has landed; it addresses only the residual coupling inside
`Skit.Impl.Memory`.

## Options considered

### A. Whole-crate SPARK

Annotate everything; rewrite the interface layer away from general access.

- Con: enormous cost for near-zero payoff. The dispatching plumbing has no
  interesting invariants — it forwards calls. Proving it "correct" proves it
  forwards, which the type system already guarantees. The Terms arena would
  have to be rebuilt on SPARK ownership. Weeks of rework to prove uninteresting
  code.
- Rejected.

### B. Partition: SPARK the core, leave the shell (proposed)

`SPARK_Mode => On` on `Skit` and the guts of `Skit.Impl.Memory`;
`SPARK_Mode => Off` on the interfaces, Terms, and I/O. SPARK is designed for
exactly this — mode is per-unit, and a proven unit may be called from
unproven code across a package boundary.

- Pro: proof effort lands only where a defect corrupts the heap. `Skit` is
  already almost pure — expression functions, no pointers, no I/O; it needs
  contracts, not restructuring. `Skit.Impl.Memory` needs the incidental
  couplings lifted out (below) and then carries the invariants that matter.
- Con: the core must be lightly restructured so its proven surface owns no
  access values and no unprovable fields.

### C. Contracts without proof

Write the `Pre`/`Post`/`Predicate` contracts, run them as runtime checks
(`-gnata`), never run `gnatprove`.

- Pro: zero tooling, catches violations in the self-test suite.
- Con: leaves the GC's hardest paths (the ones the tests do not exercise at the
  right heap-fill fraction) unchecked. Runtime contracts find bugs on the
  inputs you happen to run; proof finds them on all inputs. For a collector,
  that difference is the whole point.
- Useful as a stepping stone to B, not as the destination.

## Decision drivers

- **Payoff concentration.** The one component whose failure is catastrophic and
  hard to localise — the copying collector — is also the one whose data model
  (array + integer indices) is most amenable to proof. Effort and value
  coincide on the same unit.
- **The design already earned this.** ADR 0001's no-pointer object word was
  chosen for representation reasons; a side effect is that the heap is
  formally tractable. Partitioning banks a property the architecture already
  paid for.
- **Mode is per-unit.** SPARK does not demand all-or-nothing. The cost of
  scoping proof to the core is a handful of `SPARK_Mode` pragmas plus the
  restructuring below — not a crate rewrite.
- **Contracts document the invariants regardless.** Even the parts left
  unproven benefit from the `Pre`/`Post` written for the core, which state the
  heap invariants explicitly for the first time.

## Decision

Adopt Option **B**. Bring `Skit` and the operative core of
`Skit.Impl.Memory` under `SPARK_Mode => On`; leave the dispatching
interfaces, `Skit.Terms`, and all I/O under `SPARK_Mode => Off`.

### Properties to prove on the core

1. **In-bounds indexing.** Every use of `App.Payload` to index `Core` is
   provably within `Core'Range`. No `Application_Object` can address a cell
   outside the heap.
2. **Space discipline.** The from-space / to-space / `Free` / `Top` / `Scan`
   offsets stay within their invariants across `Allocate`, `Append`,
   `Free_And_Allocate`, and a collection: `Free` never passes `Top`; `Scan`
   never passes `Free`; the two spaces do not overlap.
3. **Collection safety.** A collection preserves every reachable cell and never
   returns a live cell to free space; post-collection, `Allocate` hands back a
   cell within the active space.
4. **Absence of runtime error** on the core paths — the SPARK default: no
   overflow, no range violation, no division error in the collector.

### Core restructuring required

The core is not proof-ready as written; three incidental couplings must be
lifted before `SPARK_Mode => On` will take.

- **Access boundary.** `Reference is access all Instance` and the `Create`
  function that returns it move to a thin `SPARK_Mode => Off` wrapper. The
  proven `Instance` becomes an object the caller holds; the proven operations
  take `Instance` by `in out`, never an access value. Allocation of the
  `Instance` itself is unproven glue; the collector operating on it is proven.
  **This work is largely subsumed by [ADR 0005](0005-collapse-the-machine-interface-tower.md):**
  once the machine interface tower collapses to a concrete type, the
  general access-to-class-wide that constitutes SPARK's hard blocker is already
  gone, and the caller already holds a concrete machine. Do 0005 first; what
  remains here is the narrower `Skit.Impl.Memory.Reference`/`Create` seam.
- **Container field.** `Container_Lists.List` leaves the proven `Instance`
  record — either hoisted into the unproven wrapper, or replaced with
  `Ada.Containers.Formal_Doubly_Linked_Lists` if the root set genuinely needs
  to live inside the proven state. Preference: hoist, since root registration
  is a setup concern, not a collection-hot-path concern.
- **Statistics.** `GC_Time : Duration` and the counters that exist only for
  `Report` move to a separate stats record threaded alongside the proven
  `Instance`, or are marked ghost. The collector's *correctness* does not
  depend on them and they should not constrain its proof.

## Tooling

`gnatprove` is not currently installed; only `alr` is present. Add the
GNATprove toolchain (`alr toolchain --select`, or a `gnatprove` dependency in
[alire.toml](../../alire.toml)) and a proof-run target. Start at
`--level=0`/`--mode=flow` to establish data-flow and initialisation, then raise
to `--level=2` and beyond for the numeric and space-bound proofs.

## Staged migration

Each stage keeps the self-test and integration suites green.

1. **Contracts first (Option C as a stage).** Write the heap invariants as
   `Pre`/`Post`/type predicates on `Skit` and `Skit.Impl.Memory`; run under
   `-gnata`. No mode change yet. Shakes out invariant mistakes cheaply.
2. **Lift the couplings.** Move the access `Reference`/`Create`, the container
   field, and the stats out of the proven surface as described above. Suites
   stay green; no behaviour change.
3. **Turn on flow.** `SPARK_Mode => On` on `Skit`; `gnatprove --mode=flow`
   clean. `Skit` is nearly there already.
4. **Turn on the collector.** `SPARK_Mode => On` on the operative core of
   `Skit.Impl.Memory`; drive proof up the levels until the four properties
   discharge or the residual is a documented, justified assumption.
5. **Record the boundary.** Document which units are proven, the exact
   invariants proven, and every `pragma Assume` / `SPARK_Mode => Off` island
   inside the core, so the proof's trust base is explicit.

## Open questions

- Does the collector as written already maintain a provable space invariant, or
  does discharging property 2 require reformulating the offsets (e.g. an
  explicit `Predicate` on `Instance` tying `Free`/`Top`/`Scan`/space bounds
  together)?
- Reachability (property 3) needs a specification of "reachable" the prover can
  reason about — likely a ghost function over the cell graph. Is that ghost
  model tractable at `--level=2`, or does it demand manual lemmas?
- Hoist the root container vs. adopt a formal container: which keeps the proven
  `Instance` smallest without forcing root registration through unproven glue on
  a hot path?
- Interaction with ADR 0001: if `Object` widens to a 64-bit NaN-boxed word, the
  payload-as-index proof is unchanged in shape but the modular arithmetic bounds
  shift. Sequence the two changes so the proof is written once against the final
  representation, not ported.

## Consequences

To be recorded once implemented. Expected:

- The copying collector carries machine-checked guarantees of in-bounds
  indexing, space discipline, and collection safety — the heap cannot be
  corrupted by a core defect that proof would have caught.
- The heap invariants become explicit contracts rather than tribal knowledge in
  the collector's control flow.
- The proven/unproven boundary is small and documented: core proven, dispatch +
  Terms + I/O unproven by deliberate scope, not by omission.
- The public interface in [skit.ads](../../src/skit.ads) is unaffected;
  callers see contracts, not a changed surface.
