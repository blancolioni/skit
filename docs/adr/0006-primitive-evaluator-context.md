# ADR 0006: Carry Primitive State in Client-Provided Handler Objects

- **Status:** Proposed
- **Date:** 2026-07-09
- **Deciders:** Fraser Wilson

## Context

[ADR 0005](0005-collapse-the-machine-interface-tower.md) collapsed the machine
interface tower. In the process, primitives changed from client-provided tagged
objects (`Skit.Primitives.Abstraction'Class`, which ADR 0005 had listed as
genuine polymorphism to keep) to a flat function pointer:

```
--  skit.ads:32
type Primitive_Evaluator is access
  function (Arguments : Object_Array) return Object;
```

The evaluator is invoked from the machine's reduction loop with only the
already-reduced argument objects
([skit-machines.adb:205](../../src/skit-machines.adb#L205)):

```
This.Push (Evaluator (Arguments));
```

A function pointer carries **no state**. That is fine for Skit's own arithmetic
primitives — `Evaluate_Add (Arguments) is (... Arguments(1) + Arguments(2))`
([skit-tests.adb:28](../../skit_tests/src/skit-tests.adb#L28)) is pure. It is
**not** fine for a foreign-function bridge.

### The consumer that breaks: Leander's foreign bridge

Leander binds host (Ada) functions as Skit primitives. Each binding carried
per-binding state in a tagged object under the old model
([leander.adb:8-16](../../../src/leander.adb#L8-L16)): the owning handle, the
host callback, and the argument/result marshalling type arrays. Its evaluation
was:

```
for I in 1 .. Argument_Count loop
   Send_Value (Slot(I), Arg_Types(I), <arg>);   -- marshal Skit → host, per Arg_Types
end loop;
Eval (Handle);                                    -- run the host callback
for I in 1 .. Result_Count loop
   <push> Receive_Value (Slot(I));                -- marshal host → Skit, per Res_Types
end loop;
```

`Arg_Types`, `Res_Types`, and `Eval` **differ per bound function**. A bare
`Primitive_Evaluator` cannot recover them. See
[Leander ADR 0002](../../../docs/adr/0002-migrate-to-the-collapsed-skit-facade.md),
blocker 2.

### Two distinct pieces of state, not one

- **Per-machine (client) state** — the owning object (Leander's handle /
  interpreter session). One per machine, shared by all its primitives.
- **Per-binding state** — `Arg_Types`/`Res_Types`/`Eval`. Distinct per bound
  primitive.

A single client-supplied datum set on the machine (the obvious first idea)
answers only the first. At the call site nothing says *which binding fired*, so
the client still cannot select the right marshalling.

### The SPARK constraint this must respect

[ADR 0004](0004-adopt-spark-for-the-memory-core.md) names general
access-to-class-wide on the proven core's surface as the hard SPARK blocker, and
the collector core (`Skit.Memory`) is now free of it. Any solution here must not
reintroduce a stored general access into the proven surface, and ideally must not
add one to `Skit.Machines` either (a SPARK stretch goal). The collector never
touches primitive state, so the only question is *where the access lives*, not
*whether it exists*.

## Decision drivers

- **Solve both axes of state.** Per-machine and per-binding, or the foreign
  bridge cannot marshal.
- **Keep the access off proven surfaces.** A *stored* `access all ...'Class`
  field becomes a member of a type; an *anonymous access parameter* does not.
  The former blocks SPARK for the type that holds it; the latter is idiomatic and
  lives only on the call stack.
- **Stable evaluator signature.** Skit's own pure primitives must not be forced
  to name state they do not use; churn in every evaluator is a cost.
- **The machine already knows the binding.** `Eval_Primitive` holds the firing
  primitive's payload ([skit-machines.adb:169](../../src/skit-machines.adb#L169)),
  and the reduction loop already fetches that primitive's record from the prim
  table (`This.Prims (P)`,
  [skit-machines.adb:351](../../src/skit-machines.adb#L351),
  [457](../../src/skit-machines.adb#L457)). Whatever is stored in that record is
  reached with no extra work.
- **Per-call cost is on the hot path.** Primitives fire on every arithmetic /
  control step; any per-call indirection the design forces (a table lookup, an
  extra dispatch hop) is paid in the tightest loop the machine runs.

## Options considered

### A. Store client state on the machine

Add `access all User_Data_Interface'Class` as a field of
`Skit.Machines.Instance`, passed to every evaluator.

- Con: solves only the per-machine axis — the binding is still unidentified.
- Con: puts a stored general access into `Skit.Machines.Instance`, permanently
  blocking that unit from SPARK and reintroducing exactly the construct ADR 0004
  worked to remove from the crate's data structures. Rejected.

### C. Thread a context parameter; expose the binding identity

Keep the flat `Primitive_Evaluator` function pointer, but pass client state as a
**parameter** into `Machine.Evaluate`, thread it to the evaluator call, and hand
the evaluator the firing primitive's **identity** so the client keys its own
per-binding table. Bundle both into one context record so the signature is
stable:

```
type Primitive_Context is record
   User_Data : access User_Data_Interface'Class;   -- per-machine (client)
   Self      : Object;                              -- per-binding identity
end record;

type Primitive_Evaluator is access
  function (Context : Primitive_Context; Arguments : Object_Array) return Object;
```

- Pro: solves both axes — `User_Data` (which machine) + `Self` (which binding) —
  with the access confined to a parameter (`Skit.Machines.Instance` gains no
  access field, `Skit.Memory` untouched).
- **Con: a per-call lookup on the hot path.** The machine fetches
  `Prims (P).Evaluator` and calls it, but that shared evaluator learns *which
  binding fired* only from `Self`, so it must look the real handler up in the
  client's table on **every** primitive call, then dispatch to it. That is a hop
  and a lookup the machine's own fetch already did — pure duplication, paid in
  the reduction loop. The state exists; C just refuses to store it where the
  machine already looks.
- Con: `User_Data` still has to be threaded as a parameter through
  `Machine.Evaluate` and `Evaluate_Application` purely to reach the evaluator.

### B. Client-provided handler objects (proposed)

Restore a client-provided primitive object — the polymorphism ADR 0005 listed as
genuine and said to keep. The prim table stores a handler as a **by-value
class-wide value** in its existing indefinite container; the machine dispatches a
single primitive operation on it:

```
type Primitive_Handler is interface;
function Evaluate
  (This : Primitive_Handler; Arguments : Object_Array) return Object is abstract;
```

Each handler carries its own state. For Skit's own pure primitives, Skit ships a
trivial concrete `Function_Handler` wrapping a plain
`access function (Arguments : Object_Array) return Object`, so
`Handle.Primitive (2, Evaluate_Add'Access)` keeps working unchanged. A stateful
consumer extends the interface: Leander's handler holds its `Handle`, argument /
result marshalling types, and the host callback, so the end user supplies
something as small as
`type Handler is access procedure (H : Leander.Handles.Handle)` and Leander wraps
it.

- Pro: **no per-call lookup.** The machine already fetches `Prims (P)`; the state
  is *in* that record, and dispatch is one indirect call — the same cost as the
  current bare function pointer. C's shared-evaluator hop and table lookup both
  vanish.
- Pro: **no state threading.** Each handler holds its own machine reference; there
  is no `User_Data` to store on the handle or pass through `Machine.Evaluate`.
  `Machine.Evaluate`'s signature is unchanged.
- Pro: matches ADR 0005's stated intent (keep `Skit.Primitives`), and the
  consumer's binding surface is trivial.
- Con: stores class-wide values in the prim table. Held **by value** in the
  already-`Off` indefinite container, this introduces **no general access** —
  dispatching is within the SPARK subset and the container was never a SPARK
  candidate anyway, so the core-proof boundary is untouched either way.
- Con: Skit's own pure primitives now register a `Function_Handler` wrapper
  rather than a bare pointer — a one-time convenience type, absorbed by the
  `Handle.Primitive` overloads so callers do not see it.

## Decision

Adopt Option **B**.

- Define a `Primitive_Handler` interface with a single dispatching
  `Evaluate (This; Arguments : Object_Array) return Object`.
- The prim table stores handlers **by value as class-wide values** (indefinite
  container); the reduction loop dispatches `Evaluate` on the fetched handler in
  place of the current `Evaluator (Arguments)` call
  ([skit-machines.adb:205](../../src/skit-machines.adb#L205)).
- Skit provides a built-in concrete `Function_Handler` wrapping
  `access function (Arguments : Object_Array) return Object`; the existing
  `Handle.Primitive (Count | Modes, ...)` overloads construct it, so pure
  primitives are unaffected.
- Stateful consumers extend `Primitive_Handler`. No `User_Data` on the machine or
  the handle; no parameter threaded through `Machine.Evaluate`; no `Self` lookup.

### Boundary the decision defends

- `Skit.Memory` (proven core): unchanged — never references primitive state.
- `Skit.Machines`: stores handlers by value; no general access introduced.
  Dispatching is SPARK-legal; the unit stays `Off` only for its standard
  containers, exactly as before. No new SPARK debt.
- `Skit.Handles`: unchanged with respect to primitive state — carries no
  `User_Data`.

## Consequences

To be recorded once implemented. Expected:

- The foreign-function bridge (Leander) carries per-binding and per-machine state
  directly in its handler object — no client-side table, no per-call lookup, no
  identity threading.
- The primitive registration type changes from a function pointer to a
  `Primitive_Handler`; Skit's own pure primitives register through the built-in
  `Function_Handler`, so [skit-tests.adb](../../skit_tests/src/skit-tests.adb)
  changes only in how it constructs primitives, not in the evaluators themselves.
- `Machine.Evaluate` keeps its current signature; the reduction loop's per-call
  cost is unchanged (one indirect dispatch, as today).
- `Skit.Machines` stores class-wide handlers by value; no general access, so
  ADR 0004's core-proof boundary and its Machines stretch goal are both intact.
- Unblocks [Leander ADR 0002](../../../docs/adr/0002-migrate-to-the-collapsed-skit-facade.md)
  blocker 2.
