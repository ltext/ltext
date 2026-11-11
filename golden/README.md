# Golden tests

Black-box tests of the `ltext` CLI, recorded from the Haskell implementation.
They are the parity spec for the Rust port (see `docs/rust-port.md`).

```bash
golden/run.sh path/to/ltext                   # check stdout + exit code
golden/run.sh --strict-stderr path/to/ltext   # also require identical stderr
golden/run.sh --only type- path/to/ltext      # run a subset
golden/run.sh --update path/to/ltext          # re-record from a reference binary
```

- `fixtures/` holds the input files. Expressions refer to them by name, and every
  case runs with `fixtures/` as its working directory.
- `cases/NAME.args` lists the CLI arguments, one per line. `NAME.stdout`,
  `NAME.stderr` and `NAME.code` hold the recorded results.

stderr isn't compared by default. Some recorded stderr is specific to Haskell
and not worth reproducing: optparse-applicative's usage text, and the GHC
exception dumps (with call-stack backtraces) from uncaught top-level parse
errors.

`difffuzz.py REFERENCE CANDIDATE` runs both binaries on random documents and
expressions, and reports any difference in stdout, exit code or stderr
(ignoring stderr that is specific to Haskell).

Several cases record **current bugs** on purpose, so the port can first match
them and then fix them deliberately. `docs/rust-port.md` lists which ones.
