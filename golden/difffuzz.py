#!/usr/bin/env python3
"""Differential fuzzer: run two ltext binaries on random inputs and compare.

    golden/difffuzz.py REFERENCE CANDIDATE [-n COUNT] [--seed SEED]

Generates random document files (headers, expression lines, raw text, odd
whitespace, unicode) in a temp copy of golden/fixtures/. It then runs both
binaries on random expressions, random lexer garbage and random flags.
stdout and exit codes must always match. stderr must match too, except where
the reference prints a GHC exception dump or a usage error (those messages are
specific to the Haskell implementation).
"""
import argparse
import os
import random
import shutil
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
VARS = ["x", "y", "f", "g", "a", "é", "x->", "-x"]
DELIMS = [("{{", "}}"), ("<<", ">>"), ("$$", "%%"), ("«", "»"), ("[", "]")]
WORDS = ["hello", "world", "{{", "}}", "<<", ">>", "x", "f", "->", "\\", "(", ")", "é", "✓", ""]
SPACES = [" ", "  ", "\t", " ", "　", " ", "\u0085", "\r"]


def rand_space(r):
    return r.choice(SPACES) if r.random() < 0.15 else " "


def rand_expr(r, names, depth=0):
    k = r.random()
    if depth > 3 or k < 0.35:
        return r.choice(names + VARS)
    if k < 0.55:
        params = " ".join(r.choice(VARS) for _ in range(r.randint(1, 2)))
        return f"\\{params} -> {rand_expr(r, names, depth + 1)}"
    if k < 0.85:
        return f"{rand_expr(r, names, depth + 1)} {rand_expr(r, names, depth + 1)}"
    return f"({rand_expr(r, names, depth + 1)})"


def rand_garbage(r):
    toks = ["\\", "->", "(", ")", "x", "f", " ", "-", ">", "é", "foo", "bar", "\t", " "]
    return "".join(r.choice(toks) for _ in range(r.randint(0, 8)))


def rand_doc(r, names):
    lines = []
    if r.random() < 0.8:
        l, rd = r.choice(DELIMS)
        params = [r.choice(VARS[:6]) for _ in range(r.randint(1, 3))]
        lines.append(rand_space(r).join([l] + params + [rd]))
        for _ in range(r.randint(0, 5)):
            if r.random() < 0.4:
                body = rand_expr(r, params + names[:3], 2)
                lines.append(rand_space(r).join([l, body, rd]))
            else:
                lines.append(rand_space(r).join(r.choice(WORDS) for _ in range(r.randint(0, 4))))
    else:
        for _ in range(r.randint(0, 3)):
            lines.append(" ".join(r.choice(WORDS) for _ in range(r.randint(0, 4))))
    text = "\n".join(lines)
    if r.random() < 0.7:
        text += "\n"
    return text


def run(binary, args, cwd):
    p = subprocess.run([binary] + args, cwd=cwd, capture_output=True, timeout=20,
                       env={**os.environ, "LANG": "C.UTF-8", "LC_ALL": "C.UTF-8"})
    return p.returncode, p.stdout, p.stderr


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("reference")
    ap.add_argument("candidate")
    ap.add_argument("-n", type=int, default=1000)
    ap.add_argument("--seed", type=int, default=None)
    a = ap.parse_args()
    seed = a.seed if a.seed is not None else random.randrange(1 << 30)
    r = random.Random(seed)
    ref, cand = os.path.abspath(a.reference), os.path.abspath(a.candidate)

    with tempfile.TemporaryDirectory() as tmp:
        work = os.path.join(tmp, "fx")
        shutil.copytree(os.path.join(HERE, "fixtures"), work)
        names = sorted(os.listdir(work))
        gen = [f"d{i}" for i in range(12)]
        for g in gen:
            with open(os.path.join(work, g), "w", encoding="utf-8") as fh:
                fh.write(rand_doc(r, names))
        names += gen

        failures = 0
        ok = 0
        kinds = {}
        for i in range(a.n):
            args = []
            if r.random() < 0.3:
                args.append("-t")
            if r.random() < 0.4:
                l, rd = r.choice(DELIMS)
                args += ["--left", l, "--right", rd]
            if r.random() < 0.15:
                args += ["-r", r.choice(names)]
            args.append(rand_garbage(r) if r.random() < 0.15 else rand_expr(r, names))
            cr, outr, errr = run(ref, args, work)
            cc, outc, errc = run(cand, args, work)
            ok += cr == 0
            kind = errr.split(b']')[0][1:].decode('utf-8', 'replace') if errr.startswith(b'[') else ('ok' if cr == 0 else 'other')
            kinds[kind] = kinds.get(kind, 0) + 1
            hs_specific = b"Uncaught exception" in errr or b"Usage:" in errr or b"ltext: " in errr
            if (cr, outr) != (cc, outc) or (not hs_specific and errr != errc):
                failures += 1
                print(f"MISMATCH #{i}: {args!r}")
                print(f"  ref  exit={cr} out={outr[:300]!r} err={errr[:300]!r}")
                print(f"  cand exit={cc} out={outc[:300]!r} err={errc[:300]!r}")
                if failures >= 10:
                    break
        print(f"seed={seed} cases={i + 1} mismatches={failures} outcomes={dict(sorted(kinds.items()))}")
        sys.exit(1 if failures else 0)


if __name__ == "__main__":
    main()
