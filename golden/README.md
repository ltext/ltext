# Golden tests

Black-box tests of the `ltext` CLI: for each case, the exact stdout, stderr and
exit code. The corpus was first recorded from the original Haskell
implementation and used as the parity spec for the Rust port
(`docs/rust-port.md`). Since then it records the intended behaviour, and
changes to it should be deliberate and reviewed.

```bash
golden/run.sh path/to/ltext                   # check stdout + exit code
golden/run.sh --strict-stderr path/to/ltext   # also require identical stderr
golden/run.sh --only type- path/to/ltext      # run a subset
golden/run.sh --update path/to/ltext          # re-record (review the diff!)
```

- `fixtures/` holds the input files. Expressions refer to them by name, and every
  case runs with `fixtures/` as its working directory.
- `cases/NAME.args` lists the CLI arguments, one per line. `NAME.stdout`,
  `NAME.stderr` and `NAME.code` hold the recorded results.

stderr is only compared with `--strict-stderr` (CI uses it).

`difffuzz.py REFERENCE CANDIDATE` runs two builds on random documents and
expressions, and reports any difference in stdout, exit code or stderr. Use it
to check that a refactoring doesn't change behaviour, e.g. against the
previous release.
