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
```

The first run downloads GNATprove. Results go to
`../obj/development/gnatprove/gnatprove.out`.

## What is proved

| Unit | `SPARK_Mode` | Status |
|------|--------------|--------|
| `Skit` (spec and body) | On | Proved at `--level=2`: 48 checks, 47 proved, 1 justified, none unproved |
| `Skit.Memory` | Off | Contracts only, checked under `-gnata` (stage 1); proof is stage 4 |
| everything else | Off | Out of scope, per ADR 0004 |

Inside `Skit`, these are deliberately outside the proof:

- `To_Float`, which converts an arbitrary word to `Float`. SPARK rejects it
  because not every bit pattern is a valid float, so its body is
  `SPARK_Mode => Off`.
- The abstract operations of `Primitive_Evaluator_Interface` and
  `Foreign_Object_Interface`, which have no body here. The host implements
  them.

One check is justified rather than proved: the feasibility of
`Argument_Modes`' class-wide postcondition (an array `1 .. N` exists for every
`Natural N`, but the prover cannot see that through the dispatching call to
`Argument_Count`). The justification is the `pragma Annotate` next to it in
`skit.ads`.
