# ltext as a ~1 MB image: just the static (musl) release binary.
#
#   docker run --rm -v "$PWD:/work" ghcr.io/ltext/ltext 'greet name'
#   COPY --from=ghcr.io/ltext/ltext /usr/local/bin/ltext /usr/local/bin/ltext
#
# Built by .github/workflows/release.yml from the release binaries, laid out
# in the build context as <TARGETARCH>/ltext (amd64, arm64).
FROM scratch
ARG TARGETARCH
ARG VERSION
LABEL org.opencontainers.image.title="ltext" \
      org.opencontainers.image.description="Text-file templating utility that treats files as higher-order functions" \
      org.opencontainers.image.source="https://github.com/ltext/ltext" \
      org.opencontainers.image.url="https://github.com/ltext/ltext" \
      org.opencontainers.image.licenses="BSD-3-Clause" \
      org.opencontainers.image.version="${VERSION}"
COPY LICENSE /usr/share/doc/ltext/LICENSE
COPY ${TARGETARCH}/ltext /usr/local/bin/ltext
WORKDIR /work
ENTRYPOINT ["/usr/local/bin/ltext"]
