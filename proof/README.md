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

# Full proof of both units from scratch, recording the session
alr exec -- gnatprove -P ../skit.gpr --mode=all --level=4 -j0 --counterexamples=off --checks-as-errors=on -u skit.ads skit-memory.ads

# Replay the recorded session, as CI does
alr exec -- gnatprove -P ../skit.gpr --mode=all --replay -j0 --counterexamples=off --checks-as-errors=on -u skit.ads skit-memory.ads
```

The first run downloads GNATprove. Results go to
`../obj/development/gnatprove/gnatprove.out`. With `--checks-as-errors=on`
an unproved check makes gnatprove exit non-zero. On a 16-core machine the full
proof takes about a minute and a half and a replay about one minute; a 4-core
CI runner takes a few times longer.

## The recorded session, and CI

`proof/sessions` records, for every check, which prover discharged it and with
how many steps (`skit.gpr`'s `Prove` package points GNATprove there). A replay
re-runs exactly that, with the recorded step limit rather than a time limit,
so it is fast and gives the same answer on any machine. A full proof searches
afresh and can time out differently from run to run; that is why CI replays.

`.github/workflows/proof.yml` replays the session on every pull request and
push that touches `skit.ads`/`.adb`, `skit-memory.ads`/`.adb`, `skit.gpr` or
this directory, and runs a full proof every Monday to catch the session
drifting from what GNATprove finds by itself.

When you change the proved units or their contracts:

1. Run the full proof above. It updates `proof/sessions` in place.
2. Replay it, starting from a clean slate, to check it stands on its own:
   delete `../obj/development/gnatprove` and run the replay command.
3. Commit the changed session files with the code.

A replay rewrites the session files too, even when it succeeds (leaving
`.bak` copies, which are ignored). After a successful replay the content is
unchanged, apart from line endings on Windows, which git normalises. After a
failed replay the entries it could not prove are rewritten as failures, so
run the full proof again before committing anything in `proof/sessions`.

## What is proved

| Unit | `SPARK_Mode` | Status |
|------|--------------|--------|
| `Skit` (spec and body) | On | Proved at `--level=2`: 48 checks, 47 proved, 1 justified, none unproved |
| `Skit.Memory` (spec and body) | On | Proved at `--level=4`: every check proved, no `pragma Assume` |
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
(`Heap_Valid`).

That includes the fact that a collection never runs out of room: the live set
fits in one semispace. `Counted` says the number of old-heap cells forwarded
so far, a recursive ghost count, equals the number of cells copied. Four
induction lemmas in `skit-memory.adb` carry it through a collection: nothing
forwarded counts as zero (`Before_GC`), writes outside the old heap leave the
count alone (`Copy`, `Scan_Cell`), forwarding one more cell adds one
(`Forward_Copy`), and while a cell is unforwarded the count is below the old
heap's size. With `Lemma_Room`, that gives `Free < Top` before every copy. The
count and lemmas are ghost code under `Ghost => Ignore`: the count recurses
over the whole old heap, so it is never compiled. `Free < Top` itself is an
ordinary assertion, and is checked at run time.

`Forward_Copy` (copy a cell and forward it) and `Scan_Cell` (store a scanned
cell and advance `Scan`) are separate procedures only so that each proof has a
small context; inside `Move` and `GC` the provers timed out.

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
