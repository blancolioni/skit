# ADR 0005: Collapse the Machine Interface Tower to a Concrete Type

- **Status:** Proposed
- **Date:** 2026-07-07
- **Deciders:** Fraser Wilson

## Context

The machine is presented through a tower of nine dispatching interfaces, each
declaring `type Abstraction is interface` and
`type Reference is access all Abstraction'Class`:

| Interface | Operations | Concrete implementations |
|-----------|-----------|--------------------------|
| [`Skit.Machine`](../../src/skit-machine.ads) (aggregates the four below) | `Bind`, `Set`/`Get (Temporary)`, `Set (Option)` | 1 |
| [`Skit.Builder`](../../src/skit-builder.ads) | `Build_Apply` | 1 |
| [`Skit.Evaluator`](../../src/skit-evaluator.ads) | `Evaluate` | 1 |
| [`Skit.Memory`](../../src/skit-memory.ads) | `Left`, `Right`, `Set_Left`, `Set_Right`, `Add_Container`, `Report`, `Trace_GC` | 1 |
| [`Skit.Stacks`](../../src/skit-stacks.ads) | `Push`, `Pop`, `Top`, `Apply` | 1 |
| [`Skit.Allocator`](../../src/skit-allocator.ads) | `Allocate` | 1 |
| [`Skit.Containers`](../../src/skit-containers.ads) | `Mark` | 1 |
| [`Skit.Interfaces`](../../src/skit-interfaces.ads) | (empty root marker) | 1 |
| [`Skit.Console`](../../src/skit-console.ads) | `Put`, `New_Line` | **0** |

Every one of these except `Skit.Console` is implemented by a **single** type,
`Skit.Impl.Machines.Instance`
([skit-impl-machines.adb:27](../../src/skit-impl-machines.adb#L27)), which
inherits the whole aggregate:

```
subtype Parent is Skit.Machine.Abstraction;
type Instance is new Parent and Skit.Containers.Abstraction with ...
```

`Skit.Console` is implemented by **no** type and referenced from nowhere — dead
code. `Skit.Interfaces.Abstraction` is an empty root interface whose only
implementor is an unrelated `IO_Instance` in `Skit.Library`.

There is no second machine, no test double routed through the interfaces, and
no configuration that selects an implementation at runtime. The tower is a
facade over one concrete machine.

### The seam is unused exactly where it would matter

The evaluator's hot path does not even go through the `Skit.Memory` interface.
`Skit.Impl.Machines` reaches the concrete collector directly:

```
--  skit-impl-machines.adb:77
function Left (This : Instance; App : Object) return Object
is (Skit.Impl.Memory.Left (This.Core.all, App));
```

`Skit.Impl.Memory.Left`/`Right` carry `Inline_Always`
([skit-impl-memory.ads:33](../../src/skit-impl-memory.ads#L33)) — an
optimisation that cannot fire through a dispatching call. The one abstraction
the tightest loop most needs, it bypasses. The interface buys nothing on the
path where indirection would cost the most.

### The external consumer needs no polymorphism either

Leander is the only out-of-crate consumer. It obtains a classwide reference
from the factory and calls dispatching operations on it:

```
--  leander-handles.adb:129
Machine : constant Skit.Machine.Reference := Skit.Impl.Machine (Size);
--  leander-handles.adb:308
M.Push (Skit.K);
```

A single machine kind is created and used. The `'Class` and the general access
exist to serve the facade, not a caller requirement.

### The genuine polymorphism

Two abstractions are *not* speculative and are out of scope for collapse:

- [`Skit.Primitives`](../../src/skit-primitives.ads) — **six** concrete
  implementations in [skit-library.adb](../../src/skit-library.adb). This is
  the real extension point; each primitive is a distinct type dispatched at
  runtime. Keep.
- [`Skit.Terms.Resolver_Interface`](../../src/skit-terms.ads#L40) — a genuine
  front-end↔machine seam: `Install` takes a resolver so the caller supplies
  symbol resolution ([skit-terms.ads:48](../../src/skit-terms.ads#L48)). One
  implementor today, but it is a real strategy injection, not a facade. Keep,
  possibly downgraded (below).

## Decision drivers

- **No second implementation, present or planned.** An interface with one
  implementor is not an abstraction; it is indirection. Nine of them is
  speculative generality (YAGNI).
- **The hot path already votes against it.** The evaluator bypasses the
  `Memory` interface to reach the concrete collector and recover
  `Inline_Always`. The code already treats the seam as overhead.
- **It is [ADR 0004](0004-adopt-spark-for-the-memory-core.md)'s hardest
  blocker.** ADR 0004 lists general access-to-class-wide
  (`access all Abstraction'Class`) as the one *hard* obstacle to SPARK, and it
  exists solely to carry the facade. Remove the facade, remove the blocker.
- **Dispatch cost.** Entering the evaluator/allocator, and every
  interface-mediated call, pays a dispatch that a concrete type would not.
- **Dead weight.** `Skit.Console` (0 implementors) and
  `Skit.Interfaces.Abstraction` (empty marker) are pure deletions.

## Options considered

### A. Keep the tower

Status quo. Preserves a generality nothing uses, keeps ADR 0004's hard blocker
in place, and keeps a dispatch on the hot path the code already tries to dodge.
Rejected.

### B. Collapse to a concrete machine, keep the two real seams (proposed)

Replace the `Machine`/`Builder`/`Evaluator`/`Memory`/`Stacks`/`Allocator`/
`Containers`/`Interfaces`/`Console` interface tower with a single concrete
`Skit.Machine` type. Fold each interface's operations onto that type as
ordinary primitive operations (public where callers need them, private
otherwise). Keep `Skit.Primitives` and `Skit.Terms.Resolver_Interface`.

- Pro: removes the SPARK hard blocker, restores `Inline_Always`, deletes dead
  code, shrinks the surface, and leaves the two abstractions that earn their
  keep untouched.
- Con: the public factory signature changes from a classwide reference to a
  concrete handle; Leander must follow (small, mechanical — see migration).

### C. Collapse only the dead/empty interfaces

Delete `Console` and `Interfaces`; keep the rest. Cheap, but leaves the SPARK
blocker and the hot-path dispatch — i.e. leaves the actual problem. Half a
measure. Rejected as an endpoint; it is simply the first stage of B.

## Decision

Adopt Option **B**. Collapse the nine-interface tower into one concrete
`Skit.Machine` type; keep `Skit.Primitives` and
`Skit.Terms.Resolver_Interface`.

### Shape after collapse

- `Skit.Machine` becomes a concrete (likely limited, private) type holding the
  machine state — the `Skit.Impl.Memory` core, the spine stacks, the temporary
  registers, and the primitive bindings — with the former interface operations
  (`Build_Apply`, `Evaluate`, `Push`/`Pop`/`Top`/`Apply`, `Left`/`Right`/
  `Set_Left`/`Set_Right`, `Bind`, `Set`/`Get`, `Report`, `Trace_GC`) as its
  primitive subprograms. `Skit.Impl.Machines` folds into it or becomes its
  private body.
- `Skit.Builder`, `Skit.Evaluator`, `Skit.Memory`, `Skit.Stacks`,
  `Skit.Allocator`, `Skit.Containers`, `Skit.Interfaces`, and `Skit.Console`
  are **deleted** as interface packages. `Containers.Mark` becomes an internal
  operation of the concrete machine (root marking is machine-internal). The
  `Console` I/O, if ever needed, returns as a concrete helper — not an
  interface with no implementors.
- Non-dispatching `Left`/`Right`/`Set_Left`/`Set_Right` inline as intended.
- `Skit.Primitives` is unchanged. `Skit.Terms.Resolver_Interface` is retained;
  see open questions on a possible downgrade to access-to-function.

### Public boundary change

The factory `Skit.Impl.Machine (Size)` returns a concrete machine handle
instead of `Skit.Machine.Reference`. `Skit.Environment` holds the concrete type
rather than the classwide reference. Leander's
[leander-handles.adb](../../../src/leander-handles.adb) changes its two
`Skit.Machine.Reference` declarations (lines 129, 302) to the concrete handle
type; the dispatching `M.Push (...)` calls become ordinary primitive calls with
no source change beyond the type name.

## Relationship to ADR 0004

This ADR is a **prerequisite** to ADR 0004, not an alternative. ADR 0004's
"access boundary" restructuring — pushing `Reference is access all Instance`
and `Create` out of the proven surface — is largely subsumed here: once the
machine is a concrete type held by the caller, the general access that blocked
SPARK is gone, and the partition line ADR 0004 draws moves deeper into the
machine with far less glue. Sequence: collapse (this ADR) first, then apply
SPARK (ADR 0004) to the resulting concrete core.

## Staged migration

Each stage keeps the self-test and integration suites green.

1. **Delete the dead.** Remove `Skit.Console` and `Skit.Interfaces` (the latter
   after re-homing `Skit.Library`'s `IO_Instance` on a concrete type). No
   behaviour change.
2. **De-aggregate `Skit.Machine`.** Make `Skit.Machine` a concrete type with the
   current interface operations as primitives, implemented by folding in
   `Skit.Impl.Machines.Instance`. Keep the factory returning the old classwide
   reference temporarily via a wrapper so callers do not yet move.
3. **Delete the constituent interfaces.** Remove `Builder`, `Evaluator`,
   `Memory`, `Stacks`, `Allocator`, `Containers`; move their operations onto the
   concrete machine and `Containers.Mark` inside it.
4. **Switch the boundary.** Change the factory to return the concrete handle;
   update `Skit.Environment` and Leander's `leander-handles.adb`. Remove the
   temporary wrapper.
5. **Confirm inlining.** Verify `Skit.Impl.Memory.Left`/`Right` inline on the
   evaluator path now that no dispatch intervenes.

## Open questions

- Concrete machine as a **limited private value** the caller owns, or as an
  access-to-concrete handle? The latter is a smaller diff for Leander (still a
  pointer, just not classwide); the former is cleaner for ADR 0004's ownership
  story. Lean toward a handle now, value later if SPARK prefers it.
- `Skit.Terms.Resolver_Interface`: keep as a one-method interface, or downgrade
  to `access function (Name : String) return Object`? A single-method interface
  with one implementor is itself borderline; a function access is lighter and
  equally expressive. Low priority, decide during migration.
- Does `Skit.Environment` rely on the `Skit.Memory` interface for anything
  beyond forwarding to the one machine? Confirm before deleting the interface so
  no genuine seam is lost.

## Consequences

To be recorded once implemented. Expected:

- Eight interface packages deleted (`Builder`, `Evaluator`, `Memory`, `Stacks`,
  `Allocator`, `Containers`, `Interfaces`, `Console`); the machine is one
  concrete type. `Skit.Primitives` and the Terms resolver seam remain.
- ADR 0004's hard SPARK blocker (general access-to-class-wide) is removed ahead
  of the proof work.
- The evaluator's hot accessors inline as designed; entering the machine loses a
  dispatch.
- The public factory returns a concrete handle; Leander's handle type changes
  in two declarations, mechanically.
