# AUR packages

| Package | Builds from | Notes |
| --- | --- | --- |
| [`ltext`](https://aur.archlinux.org/packages/ltext) | the release source tarball | Runs `cargo test` and the golden corpus in `check()` |
| [`ltext-bin`](https://aur.archlinux.org/packages/ltext-bin) | the static musl release binaries | No Rust toolchain needed; `provides`/`conflicts` `ltext` |

These directories are the source of truth; the AUR repositories are copies.

## First publish

AUR packages are created by pushing to them. You need an
[AUR account](https://aur.archlinux.org/register) with your SSH public key
added under *My Account*.

```bash
for p in ltext ltext-bin; do
  git clone ssh://aur@aur.archlinux.org/$p.git /tmp/aur-$p   # empty repo on first clone
  cp packaging/aur/$p/{PKGBUILD,.SRCINFO} /tmp/aur-$p/
  git -C /tmp/aur-$p add PKGBUILD .SRCINFO
  git -C /tmp/aur-$p commit -m "Initial import: $p 0.2.0"
  git -C /tmp/aur-$p push origin master
done
```

## Updating for a new release

1. In both PKGBUILDs, set `pkgver` and reset `pkgrel=1`.
2. Update the checksums:
   - `ltext`: `sha256sum` of `https://github.com/ltext/ltext/archive/vX.Y.Z.tar.gz`
   - `ltext-bin`: the `*-unknown-linux-musl.tar.gz` lines from the release's `SHA256SUMS`
3. On Arch (or in an `archlinux` container), run `makepkg -f` (it builds and tests) and
   `namcap PKGBUILD *.pkg.tar.zst`. Then regenerate `.SRCINFO` with
   `makepkg --printsrcinfo > .SRCINFO`.
4. Commit here, then copy both files to the AUR clone and push.
