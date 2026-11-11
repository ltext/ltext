#!/usr/bin/env bash
# Golden-test runner for the ltext CLI.
#
#   golden/run.sh [--update] [--strict-stderr] [--only PATTERN] BINARY
#
# Each case is golden/cases/NAME.args: the CLI arguments, one per line. The
# binary is run with those arguments from inside golden/fixtures/ and its
# stdout, stderr and exit code are compared with NAME.stdout, NAME.stderr and
# NAME.code.
#
# stdout and the exit code are always compared exactly. stderr (error messages,
# --help/usage text) is only compared with --strict-stderr; otherwise a stderr
# mismatch is reported as a note but doesn't fail the run.
#
# --update rewrites the expectations from BINARY (use the reference Haskell
# build for this).
set -uo pipefail

update=0 strict=0 only=
while [[ $# -gt 0 ]]; do
  case $1 in
    --update) update=1; shift ;;
    --strict-stderr) strict=1; shift ;;
    --only) only=$2; shift 2 ;;
    -*) echo "unknown option: $1" >&2; exit 2 ;;
    *) break ;;
  esac
done
[[ $# -eq 1 ]] || { sed -n '4,17p' "$0" | sed 's/^# \{0,1\}//' >&2; exit 2; }

bin=$(realpath "$1")
here=$(cd "$(dirname "$0")" && pwd)
export LANG=C.UTF-8 LC_ALL=C.UTF-8
tmp=$(mktemp -d)
trap 'rm -rf -- "$tmp"' EXIT

pass=0 fail=0 notes=0
for argsfile in "$here"/cases/*.args; do
  name=$(basename "$argsfile" .args)
  [[ -n $only && $name != *$only* ]] && continue
  mapfile -t args < "$argsfile"
  (cd "$here/fixtures" && "$bin" "${args[@]}" >"$tmp/out" 2>"$tmp/err" </dev/null)
  echo $? >"$tmp/code"

  if [[ $update -eq 1 ]]; then
    cp "$tmp/out" "$here/cases/$name.stdout"
    cp "$tmp/err" "$here/cases/$name.stderr"
    cp "$tmp/code" "$here/cases/$name.code"
    continue
  fi

  ok=1
  for f in out:stdout code:code; do
    if ! cmp -s "$tmp/${f%%:*}" "$here/cases/$name.${f##*:}"; then
      ok=0
      echo "FAIL $name (${f##*:})"
      diff -u --label expected --label actual "$here/cases/$name.${f##*:}" "$tmp/${f%%:*}" | sed 's/^/    /' | head -40
    fi
  done
  if ! cmp -s "$tmp/err" "$here/cases/$name.stderr"; then
    if [[ $strict -eq 1 ]]; then
      ok=0
      echo "FAIL $name (stderr)"
      diff -u --label expected --label actual "$here/cases/$name.stderr" "$tmp/err" | sed 's/^/    /' | head -40
    else
      notes=$((notes + 1))
      echo "note $name: stderr differs"
    fi
  fi
  if [[ $ok -eq 1 ]]; then pass=$((pass + 1)); else fail=$((fail + 1)); fi
done

if [[ $update -eq 1 ]]; then
  echo "updated expectations from $bin"
  exit 0
fi
echo "passed $pass, failed $fail, stderr differences $notes"
[[ $fail -eq 0 ]]
