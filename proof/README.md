# skit proof

This crate exists to run GNATprove over skit
([ADR 0004](../docs/adr/0004-adopt-spark-for-the-memory-core.md)). It is never
built. It depends on `gnatprove` so that skit itself does not, and crates that
use skit don't download the prover. It pins skit to `..`, so it always proves
the working copy.

## Running it

From this directory:

```sh
# Flow analysis (data flow and initialisation) of root Skit
alr exec -- gnatprove -P ../skit.gpr --mode=flow -u skit.ads

# Full proof of root Skit
alr exec -- gnatprove -P ../skit.gpr --mode=all --level=2 -u skit.ads

# Full proof of the collector (needs level 4; about ten minutes)
alr exec -- gnatprove -P ../skit.gpr --mode=all --level=4 -j0   --counterexamples=off -u skit-memory.ads
```

The first run downloads GNATprove. Results go to
`../obj/development/gnatprove/gnatprove.out`.

## What is proved

| Unit | `SPARK_Mode` | Status |
|------|--------------|--------|
| `Skit` (spec and body) | On | Proved at `--level=2`: 48 checks, 47 proved, 1 justified, none unproved |
| `Skit.Memory` (spec and body) | On | Proved at `--level=4`: every check proved, one documented `pragma Assume` (below) |
| everything else | Off | Out of scope, per ADR 0004 |

Inside `Skit`, these are deliberately outside the proof:

- `To_Float`, which converts an arbitrary word to `Float`. SPARK rejects it
  because not every bit pattern is a valid float, so its body is
  `SPARK_Mode => Off`.
- The abstract operations of `Primitive_Evaluator_Interface` and
  `Foreign_Object_Interface`, which have no body here. The host implements
  them.

In `Skit.Memory`, all four properties of ADR 0004 are proved: in-bounds
indexing, the space invariant (`Valid`), absence of run-time errors, and
collection safety -- a collection that starts from a valid heap and is given
only valid roots ends with every live cell holding only live values
(`Heap_Valid`). One fact is assumed rather than proved:

- **The live set fits in one semispace.** In `Move`,
  `pragma Assume (This.Free < This.Top)` before each `Copy`. It holds because
  each old-heap cell is copied at most once (it is marked as forwarded straight
  after) and from-space has `Space_Size` cells. Proving it needs a ghost count
  of forwarded cells, kept equal to `Free - To_Space`, and induction lemmas
  about that count.

What the proof covers is the collector given its callers' side of the
protocol: that `Skit.Machines` (which is not proved) calls `Before_GC`, `Mark`
on every root, `GC` and `After_GC` in order, and passes only valid roots. The
cheap halves of those obligations are checked at run time under `-gnata`
instead: `Mark` asserts each root is unmoved on entry (a stale root fails
there) and storable on exit, and `After_GC` asserts `Heap_Valid`. The
collection invariant itself (`Collecting`) walks both spaces, so its
contracts are proof-only (a local `Assertion_Policy` ignores them at run
time); GNATprove analyses every assertion whatever the policy.

One check is justified rather than proved: the feasibility of
`Argument_Modes`' class-wide postcondition (an array `1 .. N` exists for every
`Natural N`, but the prover cannot see that through the dispatching call to
`Argument_Count`). The justification is the `pragma Annotate` next to it in
`skit.ads`.
