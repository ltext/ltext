#!/usr/bin/env bash
# Write winget manifests for a release of ltext.
#
#   packaging/winget/generate.sh TAG OUTDIR
#
# Produces OUTDIR/ltext.ltext{,.installer,.locale.en-US}.yaml (schema 1.10.0),
# with installer checksums taken from the release's SHA256SUMS.
set -euo pipefail
tag=$1 out=$2
[[ "$tag" =~ ^v[0-9]+\.[0-9]+\.[0-9]+$ ]] || { echo "unexpected tag: $tag" >&2; exit 1; }
ver=${tag#v}
repo=https://github.com/ltext/ltext
id=ltext.ltext
schema=1.10.0

sums=$(curl -fsSL "$repo/releases/download/$tag/SHA256SUMS")
sum() {
  local s
  s=$(awk -v f="ltext-$ver-$1-pc-windows-msvc.zip" '$2 == f { print toupper($1) }' <<< "$sums")
  [[ -n "$s" ]] || { echo "no $1 Windows zip in $tag's SHA256SUMS" >&2; exit 1; }
  echo "$s"
}
x64=$(sum x86_64) arm64=$(sum aarch64)
date=$(curl -fsSL "https://api.github.com/repos/ltext/ltext/releases/tags/$tag" | jq -r '.published_at[:10]')

mkdir -p "$out"
cat > "$out/$id.yaml" <<YAML
# yaml-language-server: \$schema=https://aka.ms/winget-manifest.version.$schema.schema.json
PackageIdentifier: $id
PackageVersion: $ver
DefaultLocale: en-US
ManifestType: version
ManifestVersion: $schema
YAML

installer() {
  cat <<YAML
- Architecture: $1
  NestedInstallerFiles:
  - RelativeFilePath: ltext-$ver-$2-pc-windows-msvc\\ltext.exe
    PortableCommandAlias: ltext
  InstallerUrl: $repo/releases/download/$tag/ltext-$ver-$2-pc-windows-msvc.zip
  InstallerSha256: $3
YAML
}
cat > "$out/$id.installer.yaml" <<YAML
# yaml-language-server: \$schema=https://aka.ms/winget-manifest.installer.$schema.schema.json
PackageIdentifier: $id
PackageVersion: $ver
InstallerType: zip
NestedInstallerType: portable
Commands:
- ltext
ReleaseDate: $date
Installers:
$(installer x64 x86_64 "$x64")
$(installer arm64 aarch64 "$arm64")
ManifestType: installer
ManifestVersion: $schema
YAML

cat > "$out/$id.locale.en-US.yaml" <<YAML
# yaml-language-server: \$schema=https://aka.ms/winget-manifest.defaultLocale.$schema.schema.json
PackageIdentifier: $id
PackageVersion: $ver
PackageLocale: en-US
Publisher: Athan Clark
PublisherUrl: https://github.com/athanclark
PublisherSupportUrl: $repo/issues
Author: Athan Clark
PackageName: ltext
PackageUrl: $repo
License: BSD-3-Clause
LicenseUrl: $repo/blob/$tag/LICENSE
Copyright: Copyright (c) 2014, Athan Clark
ShortDescription: Text-file templating utility that treats files as higher-order functions
Description: |-
  λtext is a templating tool for any text file. A file whose first line is a header like
  {{ x y }} is a function of x and y, and an expression on the command line applies files
  to other files. A Hindley-Milner type checker catches mistakes before anything is rendered.
Moniker: ltext
Tags:
- cli
- templating
- template
- text
- lambda-calculus
ReleaseNotesUrl: $repo/releases/tag/$tag
ManifestType: defaultLocale
ManifestVersion: $schema
YAML
echo "$out"
