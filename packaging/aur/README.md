# AUR packages

| Package | Builds from | Notes |
| --- | --- | --- |
| [`ltext`](https://aur.archlinux.org/packages/ltext) | the release source tarball | Runs `cargo test` and the golden corpus in `check()` |
| [`ltext-bin`](https://aur.archlinux.org/packages/ltext-bin) | the static musl release binaries | No Rust toolchain needed; `provides`/`conflicts` `ltext` |

These directories are templates for the AUR repositories. Releases update the
AUR automatically: after `release.yml` publishes a tag, it calls
`.github/workflows/aur.yml`. That workflow:

1. sets `pkgver` to the release and `pkgrel=1`
2. fills in the checksums of the release's source tarball and musl binaries
3. builds, tests and installs both packages in an Arch container
4. regenerates `.SRCINFO`
5. pushes to the AUR, using the SSH key in the `AUR_SECRET` repository secret

Nothing is pushed if a build fails. Re-running for an already-published
version is a no-op. To publish (or dry-run) a tag by hand:

```bash
gh workflow run aur.yml -f tag=v0.2.0              # build, test and push
gh workflow run aur.yml -f tag=v0.2.0 -f publish=false  # build and test only
```

So the `pkgver` and checksums here may lag behind the AUR; everything else in
these PKGBUILDs (dependencies, build steps) is the source of truth. To change
the packaging without a new release, edit it here, bump `pkgrel`, and push to
the AUR by hand (or tag a release).
