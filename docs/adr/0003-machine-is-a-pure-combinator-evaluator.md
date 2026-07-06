# ADR 0003: The Machine is a Pure Combinator Evaluator

- **Status:** Proposed
- **Date:** 2026-07-06
- **Deciders:** Fraser Wilson

## Context

Compiling a lambda-calculus term to SKI combinators (bracket abstraction) is
currently performed *inside* the Skit machine. `Skit.Compiler` treats the
machine's heap, spine stacks, and temporary registers as scratch memory: it
pushes a lambda graph, rewrites it in place through `Machine.Push`/`Pop`/
`Apply`/`Left`/`Right`/`Set`/`Get`, and leaves the resulting combinator graph
on the stack. The handoff format between the front-end and this on-machine
compiler is a special `Lambda` object (`Payload_Lambda`), which exists only so a
lambda abstraction can be represented as a machine graph long enough to be
compiled away. It is never present at evaluation time.

Two independent front-ends produce these lambda graphs and drive
`Skit.Compiler`: `Leander.Calculus.Compile` (the Haskell pipeline) and
`Skit.Parser` (skit's own textual lambda syntax). See
[skit-compiler.adb](../../src/skit-compiler.adb),
[skit-parser.adb](../../src/skit-parser.adb),
[leander-calculus.adb](../../../src/leander-calculus.adb), and
[skit.ads](../../src/skit.ads).

### The cost

Running bracket abstraction on the machine makes it by far the dominant runtime
cost. Measured on a representative program, with statistics reset at the
compile/evaluate boundary:

| Phase | Cells allocated | GC | GC time |
|-------|-----------------|----|---------|
| Compile | 157,731,941 | 82 collections | 702 ms |
| Evaluate | 1,549,169 | 1 collection | 11 ms |

Post-compile the live graph is only ~300k cells (active 292k, max 357k), so
**99.8% of compile-phase allocation is transient** — intermediate terms and
spine-stack cons cells churned by rewriting on the machine. The 702 ms is
copying-collector time spent repeatedly scanning a two-million-cell core. The
evaluator itself allocates ~7.7 cells per reduction and spends ~11% of its time
in GC; it is not the problem.

Bracket abstraction is a pure tree rewrite. It never needed the machine — it was
borrowing the machine's heap and stacks as scratch, and paying the copying
collector for the privilege.

### Smell: the `Lambda` symbol

The machine's object set carries a `Lambda` constant purely as the front-end →
compiler handoff. The evaluator's dispatch on `Object.Tag` never handles it; the
only remaining consumer is a special-case branch in `Skit.Debug`. A symbol that
exists only to be compiled away, and never reaches the evaluator, does not belong
in the machine's vocabulary.

## Decision drivers

- **The machine should evaluate combinators, nothing else.** Its contract is:
  push and apply combinators and constants, and reduce. Compilation is a
  separate concern that happens *before* anything is installed for evaluation.
- **Compilation must not pay the copying collector.** A pure tree rewrite should
  run on the Ada heap (arena-freeable after use), not churn a two-space GC'd
  core.
- **Both front-ends need the same service.** Bracket abstraction cannot move into
  Leander specifically; `Skit.Parser` needs it too. It becomes an off-machine
  IR-to-IR rewrite that any front-end can drive.
- **Alignment with ADR 0002.** A machine that *installs* a finished combinator
  graph and a loader that *installs* a dumped one are the same operation. The
  combinator IR (application nodes, combinators, constants, and symbolic
  references) is exactly what a module image carries, and symbolic reference
  resolution at install time is exactly ADR 0002's named-import resolution.

## Decision

Move compilation entirely outside the machine. The machine becomes a pure
combinator evaluator that supports only pushing/applying combinators and
constants, reducing, and the graph accessors evaluation needs (`Left`/`Right`/
`Set_Left`/`Set_Right` for lazy update). Bracket abstraction becomes an
off-machine tree rewrite, and its output is installed into the machine in a
single pass.

### A native combinator IR

Introduce a native Ada term type, `Skit.Terms`, with a bottom-up builder:

```
Apply (Left, Right : Terms)  -- application
Lambda (Var; Body : Terms)   -- abstraction (compiled away; never installed)
Combinator (S | K | I | B | C | S_Prime | B_Star | C_Prime)
Const (integer | float | primitive)
Symbol (Name)                -- unresolved reference, resolved at install
```

The IR lives on the Ada heap and can be discarded (or arena-freed) once
installed. It is the single format the front-ends target and the compiler
rewrites.

### Front-ends build IR, not machine graph

`Skit.Parser` constructs `Skit.Terms` directly instead of pushing `Skit.Lambda`
onto the machine. `Leander.Calculus.Compile` lowers its own `Calculus.Tree` to
`Skit.Terms` (a shape-preserving map: `Apply`/`Lambda`/`Number`/`Symbol`
correspond directly) instead of lowering into the machine heap.

### Bracket abstraction as an IR-to-IR rewrite

`Skit.Compiler` is re-hosted on `Skit.Terms`. The algorithm is unchanged in
structure — the same `Compile`/`Abstract_Variable`/`Optimise` passes with the
S/K/I core and the K/B/C/S'/B*/C' peephole optimisations — but it reads and
writes IR nodes instead of driving `Machine.Push`/`Pop`/`Left`/`Right`/`Set`/
`Get`. Its output is an IR with no `Lambda` nodes.

### A one-pass installer

The machine gains a functional application builder, `Build_Apply (Left, Right)
return Object`, which allocates a single application cell (`Memory.Allocate`)
with no stack traffic and no dispatch churn. The installer is a post-order walk:

```
function Install (T : Skit.Terms) return Object is
   (case Kind (T) is
      when Apply      => Machine.Build_Apply (Install (Left (T)), Install (Right (T))),
      when Combinator => <combinator object>,
      when Const      => <integer / float / primitive object>,
      when Symbol     => Environment.Lookup (Name (T)));
```

Installation allocates exactly the node count of the final graph, once. Symbolic
references resolve through `Skit.Environment.Lookup` — the same resolution ADR
0002 uses for named imports — which is why this decision builds the loader
boundary that ADR depends on.

#### Invariant: no collection during Install

Install holds its partially-built subgraphs in Ada call-stack locals (the
`Object` values returned up the recursion), not in machine roots. A collection
firing mid-Install would not see those intermediates and would reclaim them out
from under the build. Rather than root every pending intermediate, Install
relies on an invariant: **a collection runs once before Install, and Install
allocates monotonically into free space without triggering another.** If the
final graph does not fit in the free space left after that collection, the
program does not fit the core at all — a hard core-exhaustion error, which is the
same outcome any over-large program produces. `Build_Apply` is therefore a raw
bump-allocate with no collection path, and the rooting problem does not arise.

### What the machine sheds

- `Skit.Compiler`'s on-machine implementation — deleted; the logic is re-homed
  off-machine on `Skit.Terms`.
- The `Lambda` constant and `Payload_Lambda` from `Skit`, and the special-case
  `Lambda` branch in `Skit.Debug`.
- The `Temporary` type and `Set`/`Get (Temporary)` from `Skit.Machine`, and the
  `Temps` register array (and its marking) from the machine implementation.
  These are compiler-only scratch; the evaluator never uses them.

The machine's public surface reduces to: build/allocate an application, push /
apply / pop / top, evaluate, the graph accessors for lazy update, bind
primitives, and the combinator and constant objects.

## Staged migration

Each stage keeps the self-test and integration suites green.

1. Add the `Skit.Terms` IR and its builder. No behaviour change.
2. Re-host `Skit.Compiler` as an IR-to-IR rewrite over `Skit.Terms`. Test its
   output against known SK forms.
3. Add `Machine.Build_Apply` and `Install (Skit.Terms)`.
4. Rewire producers: `Skit.Parser` and `Leander.Calculus.Compile` emit
   `Skit.Terms`; the `Skit.Compiler.Compile (Machine)` call sites (in
   `Leander.Handles`, `Skit.Environment`, `Skit.Parser`, and the three test
   drivers) become `Install (Compile (Terms))`.
5. Strip the machine: remove `Lambda`, `Payload_Lambda`, `Temporary`, and the
   dead debug branch.
6. Re-run the compile/evaluate GC probe. Expect the compile-phase 82
   collections / 702 ms to fall to near zero; evaluation unchanged.

## Consequences

To be recorded once implemented. Expected:

- Compilation no longer touches the machine's copying collector; the dominant
  702 ms compile-time GC cost is expected to largely disappear, moving to cheap
  Ada-heap (arena-freeable) allocation.
- The machine's contract narrows to combinator evaluation, and its object
  vocabulary loses a symbol that never reached the evaluator.
- The combinator IR and its symbolic-reference resolution establish the
  install/load boundary ADR 0002 needs; a loaded module image and a
  freshly-compiled term install through the same path.
- Independently, the machine's spine stacks now serve only evaluation. An
  array-backed stack representation (replacing the heap cons-cell stacks) would
  recover evaluation's ~11% GC overhead. This is a separate, minor optimisation
  and is out of scope for this ADR; it is recorded here only to note that the
  motivation for it — inlining the push/pop hot path — is largely superseded by
  removing compilation from the machine.
