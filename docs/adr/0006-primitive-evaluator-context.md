# ADR 0006: Thread Primitive-Evaluator State Through a Parameter, Not the Machine

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
  primitive's payload ([skit-machines.adb:169](../../src/skit-machines.adb#L169));
  exposing it is nearly free.

## Options considered

### A. Store client state on the machine

Add `access all User_Data_Interface'Class` as a field of
`Skit.Machines.Instance`, passed to every evaluator.

- Con: solves only the per-machine axis — the binding is still unidentified.
- Con: puts a stored general access into `Skit.Machines.Instance`, permanently
  blocking that unit from SPARK and reintroducing exactly the construct ADR 0004
  worked to remove from the crate's data structures. Rejected.

### B. Revert to client-provided tagged primitives

Restore `Skit.Primitives.Abstraction'Class`; each binding is again a dispatching
object holding its own state.

- Pro: per-binding state is natural; matches what ADR 0005 said to keep.
- Con: undoes ADR 0005's primitive simplification; stores class-wide values (or
  access) in the prim table. Heavier than the problem needs. Held in reserve.

### C. Thread a context parameter; expose the binding identity (proposed)

Pass client state as a **parameter** into `Machine.Evaluate`, threaded to the
evaluator call, and hand the evaluator the firing primitive's **identity** so the
client can key its own per-binding table. Bundle both into one context record so
the evaluator signature is stable:

```
type Primitive_Context is record
   User_Data : access User_Data_Interface'Class;   -- per-machine (client)
   Self      : Object;                              -- per-binding identity
end record;

type Primitive_Evaluator is access
  function (Context : Primitive_Context; Arguments : Object_Array) return Object;
```

Call chain:

```
Handle.Evaluate  (reads User_Data from Handle_Record)         -- stored in the Off boundary unit
  → Machine.Evaluate (This; User_Data : access User_Data_Interface'Class)   -- parameter
    → Evaluate_Application (This; User_Data)                                 -- parameter
      → Call_Primitive → Evaluator ((User_Data, Self), Arguments)           -- skit-machines.adb:205
```

- Pro: solves both axes — `User_Data` (which machine) + `Self` (which binding).
- Pro: the access is a parameter only; `Skit.Machines.Instance` gains no access
  field, `Skit.Memory` is untouched, and the SPARK trajectory of ADR 0004 is
  preserved.
- Pro: one context record keeps the signature stable; pure primitives ignore the
  context.

## Decision

Adopt Option **C**.

- Define `User_Data_Interface` (an empty tagged interface the client extends) and
  a `Primitive_Context` bundling `User_Data : access User_Data_Interface'Class`
  and `Self : Object`.
- `Primitive_Evaluator` becomes
  `access function (Context : Primitive_Context; Arguments : Object_Array) return Object`.
- `User_Data` is **stored on `Skit.Handles.Handle_Record`** — the boundary unit
  that is already `SPARK_Mode => Off` and already holds an access — set at
  `New_Handle` (or a dedicated setter), never on `Skit.Machines.Instance`.
- `Machine.Evaluate` and `Evaluate_Application` take
  `access User_Data_Interface'Class` as an anonymous access **parameter**;
  `Call_Primitive` constructs the `Primitive_Context` from that parameter and the
  firing primitive's payload (`(F, Primitive_Object)`).

### Boundary the decision defends

- `Skit.Memory` (proven core): unchanged — never references primitive state.
- `Skit.Machines`: no stored access field; only an anonymous access parameter
  threaded through the reduction loop. No new stored-access SPARK debt.
- `Skit.Handles`: holds the single `access all User_Data_Interface'Class`,
  consistent with its existing `SPARK_Mode => Off` status.

## Consequences

To be recorded once implemented. Expected:

- The foreign-function bridge (Leander) recovers per-binding state by keying a
  client-side table on `Context.Self`, scoped per machine via `Context.User_Data`
  — no tagged primitives, no stored access on the machine.
- The `Primitive_Evaluator` signature changes once; Skit's own pure primitives
  gain an ignored `Context` parameter (a one-line churn each in
  [skit-tests.adb](../../skit_tests/src/skit-tests.adb)).
- `Skit.Machines` acquires an anonymous access parameter but no access field;
  ADR 0004's core-proof boundary is untouched and its Machines stretch goal is
  not foreclosed.
- Unblocks [Leander ADR 0002](../../../docs/adr/0002-migrate-to-the-collapsed-skit-facade.md)
  blocker 2.
