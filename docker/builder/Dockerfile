FROM alpine:latest

# Instructions lifted from:
# http://markbucciarelli.com/posts/2017-04-05_haskell_on_alpine_linux.html

RUN apk --no-cache add \
  ## core deps
  ghc curl musl-dev zlib-dev \
  ## stack deps
  shadow xz make \
  ## app deps
  postgresql-dev

# ENTRYPOINT [ "/bin/sh" ]
